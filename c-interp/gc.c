#include "scheme.h"
bool
is_immutable (struct lisp_type *it)
{
  return (it == NIL_VALUE
	  || it == TRUE_VALUE
	  || it == FALSE_VALUE
	  || it == QUOTE_VALUE
	  || it == QUASIQUOTE_VALUE
	  || it == UNQUOTE_VALUE
	  || it == UNQUOTE_SPLICE_VALUE
	  || it == EOF_VALUE
	  || it == DOT_VALUE);
}

struct cons_cells
{
  struct lisp_type *cars[NPAIRS];
  struct lisp_type *cdrs[NPAIRS];
  struct lisp_type *to_free[NPAIRS];
  unsigned free_index;
  unsigned next;
};


void
gc_set_copied_flag (struct lisp_type *item)
{
  if (is_immutable (item))
    return;
  else
    item->copied = true;
}


void
copy_procedure_cells (struct lisp_type *proc,
		      struct cons_cells *newcells);

void
copy_cons_cells(struct lisp_type *pair,
		struct cons_cells *newcells)
{
  if (pair->copied)
    return;
  struct lisp_type *carpair = car (pair);
  struct lisp_type *cdrpair = cdr (pair);
  assert (carpair != NULL);
  assert (cdrpair != NULL);
  newcells->cars[newcells->next] = carpair;
  newcells->cdrs[newcells->next] = cdrpair;
  pair->v.pair_index = newcells->next++;
  pair->copied = true;

  if (consp (carpair))
    {
      copy_cons_cells (carpair, newcells);
    }
  else if (scheme_procedurep (carpair)
	   || scheme_macrop (carpair))
    {
      copy_procedure_cells (carpair,
			    newcells);
    }
  else
      gc_set_copied_flag (carpair);


  if (consp (cdrpair))
    {
      copy_cons_cells (cdrpair, newcells);
    }
  else if (scheme_procedurep (cdrpair)
	   || scheme_macrop (cdrpair))
    {
      copy_procedure_cells (cdrpair,
			    newcells);
    }
  else
    gc_set_copied_flag (cdrpair);



}
/*
  Procedure values are special in that they have inside them cons
  cells (the text, environment, and formal args) that may not be
  referenced anywhere else except internal to the structure of the
  procedure, so we need to have a special method for them.
 */
void
copy_procedure_cells (struct lisp_type *proc,
		      struct cons_cells *newcells)
{
  if (proc->copied)
    return;
  assert (scheme_procedurep (proc) || scheme_macrop (proc));
  proc->copied = true;
  if (consp (scheme_proc_body (proc)))
    {
      copy_cons_cells (scheme_proc_body (proc),
		       newcells);
    }
  else
    {
      assert (is_immutable (scheme_proc_body (proc)));
    }

  assert (consp (scheme_proc_environ (proc)));
  copy_cons_cells (scheme_proc_environ (proc),
		   newcells);
  if (consp (scheme_proc_formals (proc)))
    copy_cons_cells (scheme_proc_formals (proc),
		     newcells);
  else if (!nilp (scheme_proc_formals (proc)))
    {
      assert (symbolp (scheme_proc_formals (proc)));
      scheme_proc_formals (proc)->copied = true;
    }
}

void
free_old_cells (struct cons_cells *cells)
{
  for (int i = 0; i < cells->free_index; ++i)
    {
      free_lisp_type (cells->to_free[i]);
    }
}

#define should_delete(x) ((x)->copied == false)
void
find_old_cells (struct lisp_type **cells,
		struct cons_cells *cells_out,
		int ncells)
{
  for (int i = 0; i < ncells;  ++i)
    {
      if (cells[i] != NULL
	  && !is_immutable (cells[i])
	  && should_delete (cells[i]))
	{
	  //bzero (cells[i], sizeof (*cells[i]));
	  bool found = false;
	  for (int j = 0; j < cells_out->free_index; ++j)
	    {
	      /*
		we already have this cell so go to the next loop.
	      */
	      if (cells_out->to_free[j] == cells[i])
		{
		  found = true;
		  break;
		}
	    }
	  if (!found)
	    {
	      cells_out->to_free[cells_out->free_index++] = cells[i];
	    }
	  cells[i] = NULL;
	}

    }
}

static void
copy_root_array (struct lisp_type **roots,
		 struct cons_cells *cells,
		 unsigned nroots)
{
  for (int i = 0; i < nroots; ++i)
    {
      if (consp (roots[i]))
	{
	  copy_cons_cells (roots[i], cells);
	}
      else if ((scheme_procedurep (roots[i]) || scheme_macrop (roots[i])))
	{
	  copy_procedure_cells (roots[i],
				cells);
	}
      else if (!is_immutable (roots[i]))
	{
	  gc_set_copied_flag (roots[i]);
	}
    }
}

void
gc_unset_copy_flag (struct lisp_type *item)
{
  if (item && !is_immutable (item))
    {
      item->copied = false;
    }
  if (item && (scheme_procedurep (item)
	       || scheme_macrop (item)))
    {
      if (!is_immutable (scheme_proc_body (item)))
	scheme_proc_body (item)->copied = false;
      if (!is_immutable (scheme_proc_formals (item)))
	scheme_proc_formals (item)->copied = false;
      if (!is_immutable (scheme_proc_environ (item)))
	scheme_proc_environ (item)->copied = false;
    }
}

void
compact_conses (struct lisp_item_stack *conses_stack)
{
  int old_index = conses_stack->index;
  conses_stack->index = 0;
  for (int i = 0; i < old_index; ++i)
    {
      assert (conses_stack->index <= i);
      if (conses_stack->items[i])
	{
	  struct lisp_type *holder = conses_stack->items[i];
	  conses_stack->items[i] = NULL;
	  gc_unset_copy_flag (holder);
	  conses_stack->items[conses_stack->index++] = holder;
	}
    }
}

static bool
is_stack_not_full (struct lisp_item_stack *stack)
{
  return ((double)stack->index / (double)stack->size) < GC_THRESH;
}

static bool
is_cons_not_full (void)
{
  return (((double)max_cons_idx) / ((double)NPAIRS)) < GC_THRESH;
}

void
gc (bool force)
{
  if (is_cons_not_full () &&
      is_stack_not_full (eval_rval_stack) &&
      is_stack_not_full (conses) &&
      !force)
    {
      return;
    }

  struct cons_cells cells;
  bzero (cells.cars, sizeof (cells.cars));
  bzero (cells.cdrs, sizeof (cells.cdrs));
  cells.next = 0;
  cells.free_index = 0;
  /* copy_root_array (roots, &cells, nroots); */
  copy_root_array (formstack->items, &cells, formstack->index);

  find_old_cells (cars, &cells, NPAIRS);
  find_old_cells (cdrs, &cells, NPAIRS);
  find_old_cells (formstack->items + (formstack->index),
		  &cells,
		  (formstack->size - formstack->index));
  find_old_cells (conses->items,
		  &cells,
		  conses->index);
  find_old_cells (eval_rval_stack->items,
		  &cells,
		  eval_rval_stack->index);
  free_old_cells (&cells);
  compact_conses (conses);
  compact_conses (eval_rval_stack);
  for (int i = 0; i < NPAIRS; ++i)
    {
      gc_unset_copy_flag (cells.cars[i]);
      gc_unset_copy_flag (cells.cdrs[i]);
    }

  /* for (int i = 0; i < nroots; ++i) */
  /*   gc_unset_copy_flag (roots[i]); */
      
  for (int i = 0; i < formstack->size; ++i)
    gc_unset_copy_flag (formstack->items[i]);

  memcpy (cdrs, cells.cdrs, NPAIRS * sizeof (struct lisp_type *));
  memcpy (cars, cells.cars, NPAIRS * sizeof (struct lisp_type *));
  max_cons_idx = cells.next;
}