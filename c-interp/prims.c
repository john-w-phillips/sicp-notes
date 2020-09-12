#include "scheme.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>

struct lisp_type *
scheme_not (struct lisp_type *argl, struct lisp_type *env)
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
lisp_int_equal (struct lisp_type *argl, struct lisp_type *env)
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
  else if (charp (arg1) && charp (arg2)
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
scheme_car (struct lisp_type *argl, struct lisp_type *env)
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
scheme_cdr (struct lisp_type *argl, struct lisp_type *env)
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
list_func (struct lisp_type *argl, struct lisp_type *env)
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
mul (struct lisp_type *argl, struct lisp_type *env)
{
  return make_number (__lisp_int_arith (argl,
					ARITH_MUL,
					1));
}

struct lisp_type *
sub (struct lisp_type *argl, struct lisp_type *env)
{
  if (nilp (cdr (argl)))
    return make_number (-1 * number_value (car (argl)));
  else
    return make_number (__lisp_int_arith (argl,
					  ARITH_SUB,
					  0));
}


struct lisp_type *
lisp_assert (struct lisp_type *argl, struct lisp_type *env)
{
  
  if (truep (car (argl)))
    return TRUE_VALUE;
  else
    {
      scheme_signal_eval_error ("Value is not true");
      return NIL_VALUE;
    }
}

struct lisp_type *
lisp_cons (struct lisp_type *argl, struct lisp_type *env)
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
lisp_car (struct lisp_type *argl, struct lisp_type *env)
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
lisp_cdr (struct lisp_type *argl, struct lisp_type *env)
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
add (struct lisp_type *argl, struct lisp_type *env)
{

  return make_number (__lisp_int_arith (argl,
					ARITH_ADD,
					0));
}


struct lisp_type *
less_than (struct lisp_type *argl, struct lisp_type *env)
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
lisp_load (struct lisp_type *argl, struct lisp_type *env)
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
lisp_symb_equal (struct lisp_type *argl, struct lisp_type *env)
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
scheme_open (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl("dd", "open() requires two arguments", "Too few arguments for open()");
  struct lisp_type *fname = car (argl);
  struct lisp_type *mode = car (cdr (argl));
  char fname_cstring[MAX_STRING] = {0};
  char mode_cstring[MAX_STRING] = {0};
  strcpy (fname_cstring, string_c_string (fname));
  strcpy (mode_cstring, string_c_string (mode));
  int flags = 0;
  mode_t fmode = 0;
  if ((strchr (mode_cstring,
	       'w') != NULL))
    {
      flags  |= (O_WRONLY | O_CREAT);
      fmode |= (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP);
      if ((strchr (mode_cstring, 'a')) != NULL)
	flags |= O_APPEND;
    }
  else
    {
      flags |= O_RDONLY;
    }

  int rval = open (fname_cstring, flags, fmode);
  if (rval < 0) perror ("open() error: ");
  return make_number (rval);
}

struct lisp_type *
scheme_close (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("d", "close() requires one argument.", "Too few arguments for close");
  int rval = close (number_value (car (argl)));
  return make_number (rval);
}

struct lisp_type *
scheme_sys_fork (struct lisp_type *argl, struct lisp_type *env)
{
  pid_t pid = fork ();
  return make_number (pid);
}

#define MAX_ARGS 1024
struct lisp_type *
scheme_sys_exec(struct lisp_type *argl, struct lisp_type *env)
{
  if (nilp (argl) || !stringp (car (argl)))
    {
      scheme_signal_eval_error("exec requires at least one string argument.");
    }
  char **arguments = NULL;
  struct lisp_type *node_iter = argl;
  // Count items in list.
  int count = 0, i = 0;
  for (count = 0; !nilp (node_iter); ++count,node_iter=cdr (node_iter));
  arguments = malloc (sizeof (char *) * count);

  for (node_iter=argl; !nilp (node_iter); ++i,node_iter = cdr (node_iter))
    {
      char *argstr = strdup (string_c_string (car (node_iter)));
      arguments[i] = argstr;
    }
  arguments[i] = NULL;
  int rcode = execvp (arguments[0], arguments);
  // Should this instead just be an exit? This enters the debugger.
  // Probably just fprintf to stderr and die.
  scheme_signal_eval_error("Error doing an exec!");
  return NIL_VALUE;
}

struct lisp_type *
scheme_sys_waitpid (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("d", "waitpid requires one integer argument.",
	      "Too few arguments to waitpid, requires one argument.");
  pid_t pid = number_value (car (argl));
  int wstatus = 0;
  if (waitpid (pid, &wstatus, 0) != pid)
    fprintf (stderr, "sys-waitpid: WARNING: waitpid did not return %d, the child pid\n",
	     pid);
  if (!WIFEXITED (wstatus))
    {
      fprintf (stderr, "sys-waitpid: WARNING: child did not exit, returning -1.\n");
      return make_number (-1);
    }
  else
    {
      return make_number (WEXITSTATUS (wstatus));
    }
}

struct lisp_type *
scheme_sys_read (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("dd", "read requires two arguments.", "Too few arguments for read");
  int n = number_value (car (cdr (argl)));
  char buf[n];
  int rval = read (number_value (car (argl)),
		   buf,
		   n);
  if (rval < 0)
    return make_cons (make_number (rval), NIL_VALUE);
  else
    {
      union scheme_value *mem = calloc (rval, sizeof(union scheme_value));
      for (int i = 0; i < rval; ++i)
	mem[i].intval = buf[i];
      return make_cons (make_number (rval),
			make_prealloc_vector (SCHEME_CHAR,
					      rval,
					      mem));
    }
}


struct lisp_type *
scheme_sys_write (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("ddd", "sys-write requires three arguments.", "Too few arguments for sys-write");
  int fd = number_value (car (argl));
  struct lisp_type *vec = car (cdr (argl));
  int n = number_value (car (cdr (cdr (argl))));

  char buf[n];

  for (int i = 0; i < n; ++i)
    buf[i] = vec->v.vec.mem[i].intval;
  int rval = write (fd, buf, n);
  return make_number (rval);
}

struct lisp_type *
scheme_eq (struct lisp_type *argl, struct lisp_type *env)
{

  if (!nilp (list_seq_manip (argl, argl, "dd", 0, "eq? requires two arguments")))
    {
      scheme_signal_eval_error ("Too few arguments for eq?");
    }
  else
    {
      struct lisp_type *arg1 = car (argl);
      struct lisp_type *arg2 = car (cdr (argl));
      if (arg1 == arg2)
	return TRUE_VALUE;
      else if (nilp (arg1) && nilp (arg2))
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
      else if (lisp_symb_equal (argl, env) == TRUE_VALUE)
	return TRUE_VALUE;
      else if (lisp_int_equal (argl, env) == TRUE_VALUE)
	return TRUE_VALUE;
      else
	return FALSE_VALUE;
    }
  return NULL;
}

struct lisp_type *
scheme_consp (struct lisp_type *argl, struct lisp_type *env)
{
  if (!nilp (argl) && consp (car (argl)))
    return TRUE_VALUE;
  else
    return FALSE_VALUE;
}

struct lisp_type *
scheme_make_vector (struct lisp_type *argl, struct lisp_type *env)
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
      if (t == SCHEME_VECTOR_MIXED)
	return make_mixed_vector (cdr (argl));
      else
	return make_vector (t, cdr (argl));
    }
  return NULL;
}

struct lisp_type *
scheme_vector_ref (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl("dd", "vector-ref requires two arguments",
	     "Too few arguments for vector-ref");
  int n = number_value (car (cdr (argl)));
  struct lisp_type *v = car (argl);
  if (!vectorp (v))
    scheme_signal_eval_error ("Not a vector!");
  if (n > vector_len (v))
    {
      scheme_signal_eval_error ("Out of bounds vector ref!");
    }
  if (mixed_vectorp (v))
    {
      return vector_mixedmem(v)[n];
    }
  else
    {
      struct lisp_type *clone = calloc (1, sizeof *clone);
      clone->type_flags = v->v.vec.type_flags;
      clone->v = v->v.vec.mem[n];
      PUSH_STACK (conses, clone);
      return clone;
    }
  return NULL;
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
					     the_global_environment,
					     args);
  PUSH_STACK (eval_rval_stack, rval);
}

struct lisp_type *
scheme_error (struct lisp_type *argl, struct lisp_type *env)
{
  if (nilp (argl))
    {
      scheme_signal_eval_error ("");
    }
  else if (stringp (car (argl)))
    {
      scheme_signal_eval_error (string_c_string(car (argl)));
    }
  return NULL;
}

static int
__lisp_reader_getc (void *argstate)
{
  struct lisp_reader_state *state = argstate;
  struct lisp_type *rval = eval_inner_apply (state->read_proc,
					     the_global_environment,
					     NIL_VALUE);
  if (!(numberp (rval) || charp (rval) || eofp (rval)))
    {
      scheme_signal_eval_error ("Lisp byte reader returned non-int");
      return -1;
    }
  else if (eofp (rval))
    return EOF;
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
scheme_read (struct lisp_type *argl, struct lisp_type *env)
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
scheme_write (struct lisp_type *argl, struct lisp_type *env)
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
scheme_string_equalp (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("dd", "string=? requires two arguments", "too few arguments to string=?!");
  struct lisp_type *v1 = car (argl);
  struct lisp_type *v2 = car (cdr (argl));
  unsigned n1 = v1->v.vec.nitems;
  unsigned n2 = v2->v.vec.nitems;
  if (!(stringp (v1) && stringp (v2)))
    scheme_signal_eval_error("V1 and V2 are not strings! Passed to string=?.");

  if (n1 != n2)
    return FALSE_VALUE;
  else
    {
      for (int i = 0; i < n1; ++i) {
	if (v1->v.vec.mem[i].intval != v2->v.vec.mem[i].intval)
	  return FALSE_VALUE;
      }
    }
  return TRUE_VALUE;
}

struct lisp_type *
scheme_vector_set (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("ddd", "vector-set! requires three arguments",
	      "too few arguments for vector-set!");
  struct lisp_type *v = car (argl);
  eval_assert (vectorp (v),
	       "First argument to vector-set! must be a vector.");
  eval_assert (numberp (car (cdr (argl))),
	       "Second argument to vector-set! must be a number.");
  int n = number_value (car (cdr (argl)));
  struct lisp_type *toset = car (cdr (cdr (argl)));
  if (mixed_vectorp (v))
    return mixed_vector_set (v, n, toset);
  else
    return univector_set (v, n, toset);
}


int
__list_length (unsigned cur, struct lisp_type *argl)
{
  if (nilp (argl)) return cur;
  else
    return __list_length (cur + 1, cdr (argl));
}

struct lisp_type *
scheme_symbol_to_string (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("d", "symbol->string takes one argument.",
	      "Too many arguments for symbol->string, it takes one.");
  struct lisp_type *sym = car (argl);
  eval_assert (symbolp (sym), "symbol->string takes only symbols.");
  char *c_str = symbol_string_value (sym);
  unsigned len = strlen (c_str);
  union scheme_value *chars = calloc (len, sizeof(union scheme_value));
  for (int i = 0; i < len; ++i)
    chars[i].intval = c_str[i];
  return make_prealloc_vector (SCHEME_CHAR,
			       len,
			       chars);
}

struct lisp_type *
scheme_string_to_symbol (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("d",
	      "string->symbol takes one argument.",
	      "Too many arguments for string->symbol, it takes one.");
  struct lisp_type *str = car (argl);
  eval_assert (stringp (str), "string->symbol only takes strings.");
  unsigned len = vector_len (str);
  char *c_str = calloc (1, len + 1);
  for (int i = 0; i < len; ++i)
    {
      int cval  = vector_unimem(str)[i].intval;
      if (isspecial (cval))
	goto error;
      else
	c_str[i] = cval;
    }
  return make_symbol (c_str, true);
 error:
  free (c_str);
  scheme_signal_eval_error ("string->symbol: Bad symbol chars in string!");
  return NIL_VALUE;
}


struct lisp_type *
scheme_vector_len (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("d", "vector-len takes only one argument",
	      "Too many arguments for vector-len, it takes one.");
  struct lisp_type *v = car (argl);
  eval_assert (vectorp (v), "vector-len must have a vector as its only argument.");
  return make_number (vector_len (v));
}

struct lisp_type *
scheme_vector_concat (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("dd", "vector-concat takes two arguments",
	      "Too many arguments for vector-concat, it takes two.");
  struct lisp_type *v1 = car (argl);
  struct lisp_type *v2 = car (cdr (argl));

  if (mixed_vectorp (v1) && mixed_vectorp (v2))
    {
      return mixed_vector_concat (v1, v2);
    }
  else if (vectorp (v1) && vectorp (v2)
	   && v1->v.vec.type_flags == v2->v.vec.type_flags)
    {
      return vector_concat (v1, v2);
    }
  else
    scheme_signal_eval_error ("vector-concat: Vector types must be the same.");
  return NULL;
}

struct lisp_type *
scheme_vectorp (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("d", "vector? takes only one argument.",
	      "Too many arguments to vector?, it must have one.");
  if (vectorp (car (argl)))
      return TRUE_VALUE;
  else
    return FALSE_VALUE;
}

struct lisp_type *
scheme_apply (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("dd", "apply takes two arguments.",
	      "Too many arguments to apply, it takes two.");
  struct lisp_type *func = car (argl);
  struct lisp_type *func_args = car (cdr (argl));

  return eval_apply (func, func_args, env);
}

/*
  Like concat except the first argument is mutated,
  there is no new vector.
 */
struct lisp_type *
scheme_vector_extend (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("dd", "vector-extend! takes two arguments.",
	      "vector-extend! takes only two arguments.");
  struct lisp_type *v1 = car (argl);
  struct lisp_type *v2 = car (cdr (argl));
  eval_assert ((vectorp (v1) && vectorp (v2)), "vector-extend! takes only two vectors.");

  if (mixed_vectorp (v1))
    return mixed_vector_extend(v1, v2);
  else
    return univector_extend (v1, v2);	      
}

struct lisp_type *
scheme_stringp (struct lisp_type *argl, struct lisp_type *env)
{
  if (stringp (car (argl))) return TRUE_VALUE;
  return FALSE_VALUE;
}

struct lisp_type *
scheme_number_to_string (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("d", "number->string takes only one argument.",
	      "number->string takes one argument.");
  int n = number_value (car (argl));
  char sbuf[MAX_STRING];
  int nchars = snprintf (sbuf, MAX_STRING, "%d", n);
  if (nchars <= 0)
    scheme_signal_eval_error ("Not a string!");
  union scheme_value *mem = calloc (nchars, sizeof(union scheme_value));
  for (int i = 0; i < nchars; ++i)
    {
      mem[i].intval = sbuf[i];
    }
  return make_prealloc_vector (SCHEME_CHAR,
			       nchars,
			       mem);
}

struct lisp_type *
scheme_numberp (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("d", "number? takes only one argument.", "number? takes only one argument.");
  if (numberp (car (argl)))
      return TRUE_VALUE;
  return FALSE_VALUE;
}

struct lisp_type *
scheme_symbolp (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("d", "symbol? takes only one argument.", "symbol? takes only one argument.");
  if (symbolp (car (argl)))
      return TRUE_VALUE;
  return FALSE_VALUE;
}

struct lisp_type *
scheme_load_compiled_module (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("d", "load-compiled takes only one argument.",
	      "load-compiled takes one argument.");
  char strbuf[MAX_STRING] = {0};
  char *c_string = string_c_string (car (argl));
  strncpy (strbuf, c_string, MAX_STRING);  
  return load_compiled_module (c_string);
}

struct lisp_type *
scheme_vector_trunc (struct lisp_type *argl, struct lisp_type *env)
{
  check_argl ("dd", "vector-truncate! takes two arguments",
	      "vector-truncate! takes two arguments.");
  int nitems_new = number_value (car (cdr (argl)));
  return vector_trunc (car (argl), nitems_new); 
}

struct lisp_type *
scheme_vector_push_back (struct lisp_type *argl,
			 struct lisp_type *env)
{
  check_argl ("dd", "vector-push-back! takes two arguments.",
	      "vector-push-back! takes two arguments.");

  struct lisp_type *vector = car (argl);
  struct lisp_type *element = car (cdr (argl));

  eval_assert (vectorp (vector),
	       "First argument to vector-push-back! must be a vector.");
  if (mixed_vectorp (vector))
      mixed_vector_push (vector, element);
  else
      univector_push (vector, element);
}
