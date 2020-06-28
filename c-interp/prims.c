#include "scheme.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

struct lisp_type *
scheme_not (struct lisp_type *argl)
{
  if (!nilp (list_seq_manip (argl, argl, "d", 0, "not takes one argument")))
    {
      scheme_signal_eval_error ("Bad argument list -- NOT takes one argument.");
    }
  else
    {
      struct lisp_type *arg = car (argl);
      if (truep (arg))
	return FALSE_VALUE;
      else
	return TRUE_VALUE;
    }
  return NULL;
}

struct lisp_type *
lisp_int_equal (struct lisp_type *argl)
{
  struct lisp_type *arg1 = list_seq_manip (argl, argl, "a",
					   0, "= takes two arguments");
  struct lisp_type *arg2 = list_seq_manip (argl, argl, "da",
					   0,
					   "= takes two arguments");
  if (numberp (arg1) && numberp (arg2)
      && (number_value (arg1) == number_value (arg2)))
    {
      return TRUE_VALUE;
    }
  else
    {
      return FALSE_VALUE;
    }
}

struct lisp_type *
scheme_car (struct lisp_type *argl)
{
  if (!nilp (list_seq_manip (argl, argl, "d", 0, "car takes one argument")))
    {
      scheme_signal_eval_error ("Too many arguments to car");
      return NULL;
    }
  else
    return car (car (argl));
}

struct lisp_type *
scheme_cdr (struct lisp_type *argl)
{
  if (!nilp (list_seq_manip (argl, argl, "d", 0, "cdr takes one argument")))
    {
      scheme_signal_eval_error ("Too many arguments to car");
      return NULL;
    }
  else
    return cdr (car (argl));
}


struct lisp_type *
list_func (struct lisp_type *argl)
{
  return argl;
}

enum arith_operators
  {
    ARITH_MUL,
    ARITH_ADD,
    ARITH_SUB
  };

static int
__lisp_int_arith(struct lisp_type *argl,
		 enum arith_operators oper,
		 int thedefault)
{
  if (nilp (argl))
    return thedefault;
  else
    {
      assert (consp (argl));
      int n = number_value (car (argl));
      int rest = __lisp_int_arith(cdr (argl),
				  oper,
				  thedefault);
      switch (oper)
	{
	case ARITH_MUL:
	  return n * rest;
	case ARITH_ADD:
	  return n + rest;
	case ARITH_SUB:
	  return n - rest;
	}
    }
}


struct lisp_type *
mul (struct lisp_type *argl)
{
  return make_number (__lisp_int_arith (argl,
					ARITH_MUL,
					1));
}

struct lisp_type *
sub (struct lisp_type *argl)
{
  if (nilp (cdr (argl)))
    return make_number (-1 * number_value (car (argl)));
  else
    return make_number (__lisp_int_arith (argl,
					  ARITH_SUB,
					  0));
}


struct lisp_type *
lisp_assert (struct lisp_type *argl)
{
  
  if (truep (car (argl)))
    return TRUE_VALUE;
  else
    {
      longjmp(*jmpbuffer, ASSERTION_FAILURE);
    }
}

struct lisp_type *
lisp_cons (struct lisp_type *argl)
{
  if (!nilp (list_seq_manip (argl, argl, "dd", 0, "CONS takes two arguments")))
    {
      scheme_signal_eval_error ("CONS takes only two arguments");
      return NULL;
    }
  else
    return make_cons (car (argl),
		      car (cdr (argl)));
}

struct lisp_type *
lisp_car (struct lisp_type *argl)
{
  if (!nilp (list_seq_manip (argl, argl, "d", 0, "CAR takes one argument")))
    {
      scheme_signal_eval_error ("CAR requires only one argument");
      return NULL;
    }
  else
    return car (car (argl));
}

struct lisp_type *
lisp_cdr (struct lisp_type *argl)
{
    if (!nilp (list_seq_manip (argl, argl, "d", 0, "CDR takes one argument")))
    {
      scheme_signal_eval_error ("CDR requires only one argument");
      return NULL;
    }
  else
    return cdr (car (argl));
}

struct lisp_type *
add (struct lisp_type *argl)
{

  return make_number (__lisp_int_arith (argl,
					ARITH_ADD,
					0));
}


struct lisp_type *
less_than (struct lisp_type *argl)
{
  //assert (consp (argl));
  eval_assert (consp (argl), "Argl must not a list! <.");
  if (number_value (car (argl))
      < number_value (car (cdr (argl))))
    {
      return TRUE_VALUE;
    }
  return FALSE_VALUE;
}


struct lisp_type *
lists_append (struct lisp_type *l1,
	      struct lisp_type *l2)
{
  if (nilp (l1))
    return l2;
  else
    return make_cons (car (l1),
		      lists_append (cdr (l1), l2));
}

struct lisp_type *
__lisp_load (char *file, struct lisp_type *environ)
{
  int rval = 0;
  FILE *inp = fopen (file, "r");
  struct port p = STDIN_CPORT_READER;
  p.state = inp;
  struct lisp_type *lisp_rval = NIL_VALUE, *form = NULL;
  while ((form = read1 (&p)))
    {
      if (form == EOF_VALUE)
	break;
      else
	lisp_rval = eval (form, environ);
    }

 exit_eof:
  fclose (inp);
  return lisp_rval;
  
}

struct lisp_type *
lisp_load (struct lisp_type *argl)
{
  if (!nilp (list_seq_manip (argl, argl, "d", 0, "load takes one argument")))
    {
      scheme_signal_eval_error ("Load takes one argument");
      return NULL;
    }
  else
    return __lisp_load (string_c_string (car (argl)), the_global_environment);
}

struct lisp_type *
lisp_symb_equal (struct lisp_type *argl)
{
  struct lisp_type *arg1 = list_seq_manip (argl, argl, "a", 0,
					   "symbol=? requires two arguments");
  struct lisp_type *arg2 = list_seq_manip (argl, argl, "da", 0,
					   "symbol=? requires two arguments");
  if (!nilp (list_seq_manip (argl, argl, "dd", 0, "symbol=? requires two arguments")))
    {
      scheme_signal_eval_error ("Too many arguments for symbol=?");
    }
  else
    {
      struct lisp_type *arg1 = car (argl);
      struct lisp_type *arg2 = car (cdr (argl));
      if (!symbolp (arg1) || !symbolp (arg2))
	return FALSE_VALUE;
      else if (symbol_equalp(arg1, arg2))
	return TRUE_VALUE;
      else
	return FALSE_VALUE;
    }
  return NULL;
}

#define check_argl(seq, msg_few, msg_many)			\
  do {								\
    if (!nilp (list_seq_manip (argl, argl, seq, 0, msg_many)))	\
      {								\
	scheme_signal_eval_error (msg_few);			\
      }								\
  } while (0)
  
struct lisp_type *
scheme_open (struct lisp_type *argl)
{
  check_argl("dd", "open() requires two arguments", "Too few arguments for open()");
  struct lisp_type *fname = car (argl);
  struct lisp_type *mode = car (cdr (argl));
  char fname_cstring[MAX_STRING] = {0};
  char mode_cstring[MAX_STRING] = {0};
  strcpy (fname_cstring, string_c_string (fname));
  strcpy (mode_cstring, string_c_string (mode));
  int flags = 0;
  if ((strchr (mode_cstring,
	       'w') != NULL))
    {
      flags  |= (O_WRONLY | O_CREAT);
      if ((strchr (mode_cstring, 'a')))
	flags |= O_APPEND;
    }
  else
    {
      flags |= O_RDONLY;
    }

  int rval = open (fname_cstring, flags);
  if (rval < 0) perror ("open() error: ");
  return make_number (rval);
}

struct lisp_type *
scheme_close (struct lisp_type *argl)
{
  check_argl ("d", "close() requires one argument.", "Too few arguments for close");
  int rval = close (number_value (car (argl)));
  return make_number (rval);
}

struct lisp_type *
scheme_sys_read (struct lisp_type *argl)
{
  /*
    TODO: add a vector type.
   */
  check_argl ("dd", "read requires two arguments.", "Too few arguments for read");
  int n = number_value (car (cdr (argl)));
  char buf[n];
  int rval = read (number_value (car (argl)),
		   buf,
		   n);
  union scheme_value *mem = calloc (n, sizeof(union scheme_value));
  for (int i = 0; i < n; ++i)
    mem[i].intval = buf[i];
  return make_cons (make_number (rval),
		    make_prealloc_vector (SCHEME_CHAR,
					  n,
					  mem));
}


struct lisp_type *
scheme_eq (struct lisp_type *argl)
{

  if (!nilp (list_seq_manip (argl, argl, "dd", 0, "eq? requires two arguments")))
    {
      scheme_signal_eval_error ("Too few arguments for eq?");
    }
  else
    {
      struct lisp_type *arg1 = car (argl);
      struct lisp_type *arg2 = car (cdr (argl));
      if (nilp (arg1) && nilp (arg2))
	{
	  return TRUE_VALUE;
	}
      else if ((arg1 == TRUE_VALUE
		&& arg2 == TRUE_VALUE)
	       || (arg1 == FALSE_VALUE
		   && arg2 == FALSE_VALUE))
	{
	  return TRUE_VALUE;
	}
      else if (lisp_symb_equal (argl) == TRUE_VALUE)
	return TRUE_VALUE;
      else if (lisp_int_equal (argl) == TRUE_VALUE)
	return TRUE_VALUE;
      else
	return FALSE_VALUE;
    }
  return NULL;
}

struct lisp_type *
scheme_consp (struct lisp_type *argl)
{
  if (!nilp (argl) && consp (car (argl)))
    return TRUE_VALUE;
  else
    return FALSE_VALUE;
}

struct lisp_type *
scheme_make_vector (struct lisp_type *argl)
{
  if (nilp (argl))
    scheme_signal_eval_error ("make-vector takes at least one argument");
  else
    {
      if (!symbolp (car (argl)))
	{
	  scheme_signal_eval_error(
	      "first argument to make-vector must be a type symbol");
	  return NULL;
	}
      enum lisp_types t = sym_to_type (car (argl));
      return make_vector (t, cdr (argl));
    }
  return NULL;
}

struct lisp_type *
scheme_vector_ref (struct lisp_type *argl)
{
  check_argl("dd", "vector-ref requires two arguments",
	     "Too few arguments for vector-ref");
  int n = number_value (car (cdr (argl)));
  struct lisp_type *v = car (argl);
  struct lisp_type *clone = calloc (1, sizeof *clone);
  clone->type = v->v.vec.type;
  clone->v = v->v.vec.mem[n];
  PUSH_STACK (conses, clone);
  return clone;
}

struct lisp_reader_state
{
  struct lisp_type *read_proc;
  struct lisp_type *write_proc;
  struct lisp_type *unget_proc;
};

static void
__lisp_reader_ungetc (int c, void *argstate)
{
  struct lisp_reader_state *state = argstate;
  struct lisp_type *args = make_cons (make_number (c), NIL_VALUE);
  struct lisp_type *rval = eval_inner_apply (state->unget_proc,
					     NIL_VALUE,
					     the_global_environment,
					     args);
  PUSH_STACK (eval_rval_stack, rval);
}

static int
__lisp_reader_getc (void *argstate)
{
  struct lisp_reader_state *state = argstate;
  struct lisp_type *rval = eval_inner_apply (state->read_proc,
					     NIL_VALUE,
					     the_global_environment,
					     NIL_VALUE);
  if (!(numberp (rval) || charp (rval)))
    {
      scheme_signal_eval_error ("Lisp byte reader returned non-int");
      return -1;
    }
  else
    return number_value (rval);
}

void
init_cport_reader_from_scheme_reader (struct lisp_type *scheme_reader,
				      struct lisp_reader_state *rstate,
				      struct port *p)
{
  struct lisp_type *read_proc = car (scheme_reader);
  struct lisp_type *unget_proc = car (cdr (scheme_reader));
  rstate->read_proc = read_proc;
  rstate->unget_proc = unget_proc;
  p->state = rstate;
  p->getbyte = __lisp_reader_getc;
  p->ungetbyte = __lisp_reader_ungetc;
  p->writebyte = NULL;
}

struct lisp_type *
scheme_read (struct lisp_type *argl)
{
  if (nilp (argl))
    return read1 (&STDIN_CPORT_READER);
  else if (nilp (cdr (argl)))
    {
      PUSH_STACK (formstack, argl);
      struct port p = {0};
      struct lisp_reader_state rstate = {0};
      init_cport_reader_from_scheme_reader (car (argl),
					    &rstate,
					    &p);
      struct lisp_type *rval = read1 (&p);
      POP_STACK (formstack);
      return rval;
    }
  else
    scheme_signal_eval_error ("Support for non-stdin ports not implemented");
  return NULL;
}

struct lisp_type *
scheme_write (struct lisp_type *argl)
{
  if (nilp (argl))
    {
     write1 (argl);
     return TRUE_VALUE;
    }
  else
    scheme_signal_eval_error("Support for non-stdout ports not implemented");
  return NULL;
}

struct lisp_type *
scheme_vector_set (struct lisp_type *argl)
{
  check_argl ("ddd", "vector-set! requires three arguments",
	      "too few arguments for vector-set!");
  struct lisp_type *v = car (argl);
  int n = number_value (car (cdr (argl)));
  struct lisp_type *toset = car (cdr (cdr (argl)));
  v->v.vec.mem[n] = toset->v;
  return toset;
}

int
__list_length (unsigned cur, struct lisp_type *argl)
{
  if (nilp (argl)) return cur;
  else
    return __list_length (cur + 1, cdr (argl));
}