#include "scheme.h"



struct cons_cells
{
  struct lisp_type *cars[NPAIRS];
  struct lisp_type *cdrs[NPAIRS];
  struct lisp_type *to_free[NPAIRS];
  unsigned free_index;
  unsigned next;
  unsigned type_counters[INVALID]; // As many as the enum.
};

static inline void
__print_type_stats(struct cons_cells *cells)
{
  for (int i = 0; i < INVALID; ++i)
    {
      printf("GC found %d %s.\n",
	     cells->type_counters[i],
	     get_type_string (i));
    }
}

#ifdef GC_RECORD_STATS
#define record_type(cells, x) ((cells->type_counters[get_type_enum(x)]++))
#define print_type_stats(cells) __print_type_stats(cells)
#else
#define record_type(cells, x)
#define print_type_stats(cells)
#endif

void
copy_vector (struct lisp_type *vec,
	     struct cons_cells *cells);
static inline void
copy_cons_cells(struct lisp_type *pair,
		
		struct cons_cells *newcells);
static void
copy_root_array (struct lisp_type **roots,
		 struct cons_cells *cells,
		 unsigned nroots);

static inline void
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

static inline void
copy_cell (struct lisp_type *car_or_cdr,
	   struct cons_cells *newcells)
{
  record_type(newcells, car_or_cdr);
  if (consp (car_or_cdr))
    {
      copy_cons_cells (car_or_cdr, newcells);
    }
  else if (scheme_procedurep (car_or_cdr)
	   || compiled_procedurep (car_or_cdr)
	   || scheme_macrop (car_or_cdr))
    {
      copy_procedure_cells (car_or_cdr,
			    newcells);
    }
  else if (mixed_vectorp (car_or_cdr))
    {
      copy_vector (car_or_cdr, newcells);
    }
  else
    gc_set_copied_flag (car_or_cdr);
}

static inline void
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
  copy_cell (carpair, newcells);
  copy_cell (cdrpair, newcells);
}

void
copy_vector (struct lisp_type *vec,
	     struct cons_cells *cells) {
  assert (mixed_vectorp (vec));
  vec->copied = true;
  unsigned nitems = vec->v.vec.nitems;
  copy_root_array (vector_mixedmem(vec),
		   cells,
		   nitems);
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
  assert (scheme_procedurep (proc)
	  || scheme_macrop (proc)
	  || compiled_procedurep (proc));
  proc->copied = true;
  if (!compiled_procedurep (proc) && consp (scheme_proc_body (proc)))
    {
      copy_cons_cells (scheme_proc_body (proc),
		       newcells);
    }
  else if (!compiled_procedurep (proc))
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
      if ((i < (cells->free_index - 1))
	  && (cells->to_free[i+1] == cells->to_free[i]))
	continue;
      else
	{
	  free_lisp_type (cells->to_free[i]);
	}
    }
}


void quicksort_cells (struct lisp_type **cells, unsigned len)
{
  if (len <= 1)
    {
      return;
    }
  else if (len == 2)
    {
      if (cells[0] < cells[1])
	{
	  struct lisp_type *tmp = cells[0];
	  cells[0] = cells[1];
	  cells[1] = tmp;
	  return;
	}
      else
	return;
    }
  else
    {
      struct lisp_type *tmp = NULL;
      int partition_index = 0;
      for (int i = 1; i < len; ++i)
	{
	  if (((unsigned long)cells[i]) <= ((unsigned long)cells[partition_index]))
	    {
	      tmp = cells[partition_index + 1];
	      cells[partition_index + 1] = cells[partition_index];
	      cells[partition_index] = cells[i];
	      cells[i] = tmp;
	      partition_index++;
	    }
	}
      assert (partition_index < len);
      assert (partition_index - len >= 0);
      quicksort_cells (cells, partition_index);
      quicksort_cells (cells + partition_index + 1, (len - partition_index) -1);
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
	  cells_out->to_free[cells_out->free_index++] = cells[i];
	  cells[i] = NULL;
	}
    }
  quicksort_cells (cells_out->to_free, cells_out->free_index);
}

static inline void
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
      else if ((scheme_procedurep (roots[i])
		|| compiled_procedurep (roots[i])
		|| scheme_macrop (roots[i])))
	{
	  copy_procedure_cells (roots[i],
				cells);
	}
      else if (mixed_vectorp (roots[i]))
	{
	  copy_vector (roots[i], cells);
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
  if (!item) return;
  if (!is_immutable (item))
    {
      item->copied = false;
    }
  if ((scheme_procedurep (item)
       || scheme_macrop (item)))
    {
      if (!is_immutable (scheme_proc_body (item)))
	scheme_proc_body (item)->copied = false;
      if (!is_immutable (scheme_proc_formals (item)))
	scheme_proc_formals (item)->copied = false;
      if (!is_immutable (scheme_proc_environ (item)))
	scheme_proc_environ (item)->copied = false;
    }
  if (mixed_vectorp (item))
    {
      for (int i = 0; i < item->v.vec.nitems; ++i)
	gc_unset_copy_flag (item->v.vec.mixed_mem[i]);
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
  bzero (cells.type_counters, sizeof (cells.type_counters));
  cells.next = 0;
  cells.free_index = 0;
  copy_root_array (formstack->items, &cells, formstack->index);
  find_old_cells (conses->items,
		  &cells,
		  conses->index);
  free_old_cells (&cells);
  compact_conses (conses);
  /* for (int i = 0; i < NPAIRS; ++i) */
  /*   { */
  /*     gc_unset_copy_flag (cells.cars[i]); */
  /*     gc_unset_copy_flag (cells.cdrs[i]); */
  /*   } */

  /* for (int i = 0; i < formstack->index; ++i) */
  /*   gc_unset_copy_flag (formstack->items[i]); */

  memcpy (cdrs, cells.cdrs, NPAIRS * sizeof (struct lisp_type *));
  memcpy (cars, cells.cars, NPAIRS * sizeof (struct lisp_type *));
  max_cons_idx = cells.next;
  print_type_stats(&cells);
}
