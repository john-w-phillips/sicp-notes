#include "scheme.h"

jmp_buf *jmpbuffer= NULL;
jmp_buf *debugbuf = NULL;
struct lisp_type *the_global_environment = NULL;
struct lisp_type *form_global = NULL;
struct lisp_type *environ_global = NULL;

struct lisp_type *cars[NPAIRS];
struct lisp_type *cdrs[NPAIRS];

/*
  The (stupid) way we allocate types means that the heads of conses
  lists are not on the actual cars/cdrs arrays but must be kept track
  of separately so we don't leak memory.
*/
DECLARE_LISP_STACK (formstack, NPAIRS);

DECLARE_LISP_STACK (eval_rval_stack, NPAIRS * 5);

DECLARE_LISP_STACK (conses, NPAIRS);

DECLARE_LISP_STACK (backtrace, NPAIRS);

unsigned max_cons_idx = 0;

struct port STDIN_CPORT_READER = {
  .getbyte = (int (*)(void *))getc,
  .ungetbyte = (void (*)(int, void *))ungetc,
  .writebyte = (void (*)(void *, int))putc
};


#define self_evaluatingp(x) (numberp (x) \
			     || stringp (x) \
			     || booleanp (x) \
			     || nilp (x) \
			     || charp (x))

#define applicationp(x) (consp (x))
#define sequencep(x) (consp (x) && \
		      symbol_string_value_equals (car (x), "begin"))

#define quotedp(x) (consp (x) &&		    \
		    (symbol_string_value_equals (car (x), "quote") ||	\
		     symbol_string_value_equals (car (x), "quasiquote")))


#define assignmentp(x) (consp (x) && \
			symbol_string_value_equals (car (x), "set!"))


#define definitionp(x) (consp (x) &&					\
			symbolp (car (x)) &&				\
			symbol_string_value_equals (car (x), "define"))



#define lambdap(x) \
  (consp (x) && symbol_string_value_equals (car (x), "lambda"))
#define macrop(x) \
  (consp (x) && symbol_string_value_equals (car (x), "macro"))

#define conditionalp(x) (consp (x) &&					\
			 symbolp (car (x)) &&				\
			 symbol_string_value_equals (car (x), "if"))


struct lisp_type *
make_env_frame (void)
{
  return make_cons (make_symbol ("env-frame", false),
		    (struct lisp_type *)&NIL);
}

struct lisp_type *
last_pair (struct lisp_type *list)
{
  if (nilp (cdr (list)))
    {
      return list;
    }
  else
    return last_pair (cdr (list));
}

struct lisp_type *
set_cdr (struct lisp_type *pair1,
	 struct lisp_type *val)
{
  assert (consp (pair1));
  cdrs[pair1->v.pair_index] = val;
  return val;
}

struct lisp_type *
env_frame_insert_var (struct lisp_type *frame,
		      struct lisp_type *name_symbol,
		      struct lisp_type *value)
{
  struct lisp_type *the_last_pair = last_pair (frame);
  struct lisp_type *new_end
    = make_cons (make_cons (name_symbol, value),
		 NIL_VALUE);
  set_cdr(the_last_pair, new_end);
  return frame;
}

struct lisp_type *
assq (struct lisp_type *symbol,
      struct lisp_type *assoclist)
{
  if (nilp (assoclist))
    return NIL_VALUE;
  else
    {
      struct lisp_type *carlist = car (assoclist);
      assert (consp (carlist));
      struct lisp_type *key = car (carlist);
      if (!symbolp (key))
	return assq (symbol, cdr (assoclist));
      else if (symbol_equalp (symbol,
			      key))
	return carlist;
      else
	return assq (symbol, cdr (assoclist));
    }
}

struct lisp_type *
env_frame_find_var (struct lisp_type *frame,
		    struct lisp_type *name_symbol)
{
  assert (symbol_string_value_equals (car (frame), "env-frame"));
  struct lisp_type *valuecel = assq (name_symbol, cdr (frame));
  if (nilp (valuecel))
    return NULL;
  else
    return valuecel;
}



struct lisp_type *
env_frame_set_var_value (struct lisp_type *frame,
			 struct lisp_type *name_symbol,
			 struct lisp_type *val)
{
  assert (symbol_string_value_equals (car (frame), "env-frame"));
  struct lisp_type *valuecel = assq (name_symbol, cdr (frame));
  if (!nilp (valuecel))
    set_cdr (valuecel, val);
  else
    env_frame_insert_var (frame, name_symbol, val);
  return val;
}


struct lisp_type *
env_find_var (struct lisp_type *environ,
	      struct lisp_type *name_symbol)
{
  struct lisp_type *result = NULL;
  if (nilp (environ))
    return NULL;
  else if ( (result = env_frame_find_var (environ_first_frame (environ),
					  name_symbol)))
    return result;
  else if ( (result = env_find_var (enclosing_environ (environ),
				    name_symbol)))
    return result;
  else
    return NULL;
}


struct lisp_type *
env_set_var (struct lisp_type *environ,
	     struct lisp_type *name_symbol,
	     struct lisp_type *value)
{
  return env_frame_set_var_value (environ_first_frame (environ),
				  name_symbol,
				  value);
}

struct lisp_type *
eval_var (struct lisp_type *form,
	  struct lisp_type *environ)
{
  assert (symbolp (form));
  struct lisp_type *r = env_find_var (environ, form);
  if (r)
    return cdr (r);
  else
    {
      scheme_signal_eval_error ("Undefined variable %s",
				symbol_string_value (form));
      return NULL;
    }
}

struct lisp_type *eval_arglist (struct lisp_type *form,
				struct lisp_type *environ)
{
  /* assert (form); */
  /* assert (consp (form) || nilp (form)); */
  eval_assert (form, "Evaluator error -- form was NULL, zero pointer.."); 
  eval_assert (consp (form) || nilp (form),
	       "Evaluator error -- arglist not a list or nil.");
  if (nilp (form))
    return NIL_VALUE;
  else
    {
      struct lisp_type *carvalue = eval (car (form), environ);
      /* push_form (carvalue); */
      PUSH_STACK (formstack, carvalue);
      struct lisp_type *rest_value = eval_arglist (cdr (form),
						   environ);
      /* pop_form (); */
      POP_STACK (formstack);
      return make_cons (carvalue,
			rest_value);		   
    }
}

struct lisp_type *
environment_extend (struct lisp_type *environ,
		    struct lisp_type *symbol_list,
		    struct lisp_type *value_list)
{
  struct lisp_type *frame = make_env_frame();
  struct lisp_type *it_s = symbol_list;
  struct lisp_type *it_v = value_list;
  while (it_s != NIL_VALUE && it_v != NIL_VALUE)
    {
      env_frame_set_var_value(frame,
			      car (it_s),
			      car (it_v));
      it_s = cdr (it_s);
      it_v = cdr (it_v);
    }
  if (it_s != it_v)
    {
      fprintf (stderr, "Bad value/variable lists:\n");
      write1 (symbol_list);
      write1 (value_list);
      exit (1);
    }
  return make_cons (frame, environ);
}

struct lisp_type *eval_sequence (struct lisp_type *forms,
				 struct lisp_type *environ)
{
  assert (forms);
  assert (environ);
  eval_assert (consp (forms), "Forms must be a list.");
  if (nilp (cdr (forms)))
      return eval (car (forms), environ);
  else
    {
      eval (car (forms), environ);
      return eval_sequence (cdr (forms), environ);
    }
  return NULL;
}

void
__bind_args (struct lisp_type *formals,
	     struct lisp_type *argl,
	     struct lisp_type **formals_out,
	     struct lisp_type **argl_out)
{
  if (nilp (formals))
    {
      if (nilp (argl))
	{
	  *argl_out = NIL_VALUE;
	  *formals_out = NIL_VALUE;
	  return;
	}
      else
	{
	  scheme_signal_eval_error ("Too many arguments");
	}
    }
  else if (!consp (formals) && consp (argl))
    {
      *argl_out =  make_cons (argl, NIL_VALUE);
      *formals_out = make_cons (formals, NIL_VALUE);
      return;
    }
  else if (!consp (formals) && nilp (argl))
    {
      *argl_out = make_cons (NIL_VALUE, NIL_VALUE); 
      *formals_out = make_cons (formals, NIL_VALUE);
      return;
    }
  else if (consp (formals) && consp (argl))
    {
      struct lisp_type *argl_tmp = NULL, *formals_tmp = NULL;
      __bind_args (cdr (formals),
		   cdr (argl),
		   &formals_tmp,
		   &argl_tmp);
      *argl_out = make_cons (car (argl),
			     argl_tmp);
      *formals_out = make_cons (car (formals),
				formals_tmp);
      return;
    }
  else
    {
      scheme_signal_eval_error ("Bad argument list.");
    }

}
struct lisp_type *
eval_inner_apply (struct lisp_type *proc,
		  struct lisp_type *form,
		  struct lisp_type *environ,
		  struct lisp_type *eval_argl)
{
  struct lisp_type *lisp_rval = NULL;
  /* push_form(eval_argl); */
  
  PUSH_STACK (formstack, eval_argl);
  struct lisp_type *lambda_env =
    scheme_proc_environ (proc);
  struct lisp_type *lambda_formals =
    scheme_proc_formals (proc);
  struct lisp_type *arguments = NULL, *formals = NULL;
  __bind_args (lambda_formals,
	       eval_argl,
	       &formals,
	       &arguments);
  assert (formals);
  assert (arguments);

  struct lisp_type *new_environ
    = environment_extend (lambda_env,
			  formals,
			  arguments);
  struct lisp_type *btent = make_bt_entry(form,
					  formals,
					  arguments,
					  new_environ);
  PUSH_STACK (backtrace,
	      btent); 
  PUSH_STACK (formstack, btent); //protect btent from GC.
  PUSH_STACK (formstack, arguments);
  PUSH_STACK (formstack, formals);
  lisp_rval =  eval_sequence (scheme_proc_body (proc),
			      new_environ);

  if (scheme_macrop (proc))
    {
      assert (lisp_rval);
      lisp_rval = eval (lisp_rval, environ);
    }
  POP_STACK (backtrace); // btent
  POP_STACK (formstack); // formals
  POP_STACK (formstack); // arguments
  POP_STACK (formstack); // btent 
  POP_STACK (formstack); // eval_argl
  return lisp_rval;
}

struct lisp_type *eval_apply (struct lisp_type *proc,
			      struct lisp_type *args,
			      struct lisp_type *environ)
{
    eval_assert (proc, "PROC is null in C, evaluator bug.");
  eval_assert ((scheme_procedurep (proc)
		|| primitive_procedurep (proc)
		|| scheme_macrop (proc)),
	       "Form is not a procedure");

  /* push_form (proc); */
  PUSH_STACK (formstack, proc);

  struct lisp_type *lisp_rval = NULL;
  if (primitive_procedurep (proc))
    {
      PUSH_STACK (backtrace,
		  make_bt_entry (form,
				 NIL_VALUE,
				 eval_argl,
				 the_global_environment));
      lisp_rval = primitive_procedure_proc (proc)(eval_argl);
      POP_STACK (backtrace);
    }
  else if (scheme_procedurep (proc) || scheme_macrop (proc))
    {
      lisp_rval = eval_inner_apply (proc,
				    form,
				    environ,
				    eval_argl);
    }
  else
    {
      fprintf (stderr, "Bad procedure type.\n");
      abort();
    }
  /* pop_form (); */
  POP_STACK (formstack); // proc
  //printf ("return v\n");
  return lisp_rval;
}

struct lisp_type *eval_application (struct lisp_type *form,
				    struct lisp_type *environ)
{
  //assert (cdr (form));
  eval_assert (cdr (form), "Form is malformed, must be a nil terminated list!");
  struct lisp_type *proc = eval (car (form), environ);
    struct lisp_type *argl = cdr (form);
  /* push_form (argl); */
  struct lisp_type *eval_argl = NULL;
  if (!scheme_macrop (proc))
    eval_argl
      = eval_arglist (argl, environ);
  else
    eval_argl = argl;

  return eval_apply (proc, eval_argl, environ);
}


struct lisp_type *
list_seq_manip (struct lisp_type *form_orig,
		struct lisp_type *form_now,
		char *sequence,
		int sequence_it,
		char *errorstr) {
  if (sequence[sequence_it] == '\0' || sequence[sequence_it] == 'r')
    return form_now;
  else if (sequence[sequence_it] == 'a')
    {
      if (!consp (form_now))
	goto err;
      else
	return list_seq_manip (form_orig,
			      car (form_now),
			      sequence,
			      ++sequence_it,
			      errorstr);
    }
  else if (sequence[sequence_it] == 'd')
    {
      if (!consp (form_now))
	goto err;
      else
	return list_seq_manip (form_orig,
			       cdr (form_now),
			       sequence,
			       ++sequence_it,
			       errorstr);
    }
 err:
  /* fprintf (stderr, "Invalid syntax: %s. Form: \n", */
  /* 	   errorstr); */
  /* write (form_orig); */
  /* longjmp (*jmpbuffer, READER_INVALID_SYNTAX); */
  scheme_signal_eval_error ("Invalid syntax: %s", errorstr);
  return NULL;
}


#define if_pred(x) \
  list_seq_manip (x, x, "da", 0, "Invalid if predicate (bad if syntax)")

#define if_consequent(x) \
  list_seq_manip (x, x, "dda", 0, "Invalid if consequent (bad if syntax)")
#define if_alternative_exists_p(x) \
  (consp (cdr (cdr (cdr (x)))))
#define if_alternative(x) \
  list_seq_manip (x, x, "ddda", 0, "Invalid if alternative")

struct lisp_type *eval_conditional (struct lisp_type *form,
				    struct lisp_type *environ)
{
  struct lisp_type *pred_val = eval (if_pred (form), environ);
  bool istrue = truep (pred_val);
  if (!is_immutable (pred_val))
    PUSH_STACK (eval_rval_stack, pred_val);

  if (istrue)
    {
      return eval (if_consequent (form), environ);
    }
  else if (if_alternative_exists_p (form))
    {
      return eval (if_alternative (form), environ);
    }
  else
    return NIL_VALUE;
}

#define lambda_formals(x) \
  list_seq_manip (x, x, "da", 0, "Invalid lambda form (no formals)")
#define lambda_body(x) \
  list_seq_manip (x, x, "dd", 0, "invalid lambda form (no body)")


struct lisp_type *eval_lambda (struct lisp_type *form,
			       struct lisp_type *environ)
{
  struct lisp_type *lambda = make_lambda (lambda_formals (form),
					  lambda_body (form),
					  environ);
  return lambda;
}

#define macro_formals lambda_formals
#define macro_body lambda_body
struct lisp_type *eval_macro (struct lisp_type *form,
			       struct lisp_type *environ)
{
  struct lisp_type *macro = make_macro (macro_formals (form),
					macro_body (form),
					environ);
  return macro;
}

#define definition_is_function(x) \
  (consp (list_seq_manip (x, x, "da", 0, "Invalid definition form")))

#define definition_simple_variable(x) \
  (list_seq_manip (x, x, "dda", 0, "Invalid definition variable"))

#define definition_function_formals(x) \
  (list_seq_manip (x, x, "dad", 0, "Invalid formals"))

#define definition_function_body(x) \
  (list_seq_manip (x, x, "dd", 0, "Invalid function body"))

struct lisp_type *
definition_value (struct lisp_type *form,
		  struct lisp_type *environ)
{
  if (definition_is_function (form))
    {
      return make_lambda (definition_function_formals (form),
			  definition_function_body (form),
			  environ);
    }
  else
    return eval (definition_simple_variable (form),
		 environ);
}

struct lisp_type *
definition_variable (struct lisp_type *form,
		     struct lisp_type *environ)
{
  if (definition_is_function (form))
    {
      return list_seq_manip (form, form,
			     "daa", 0,
			     "Bad function definition form, no name.");
    }
  else
    return list_seq_manip (form, form,
			   "da", 0,
			   "Bad definition form, no variable.");
}

struct lisp_type *eval_definition (struct lisp_type *form,
				   struct lisp_type *environ)
{
  struct lisp_type *val = definition_value (form, environ);
  env_frame_set_var_value(environ_first_frame (environ),
			  definition_variable (form, environ),
			  val);
  return val;
}

struct lisp_type *
set_assignee (struct lisp_type *form)
{
  return list_seq_manip (form, form,
			 "da",
			 0,
			 "Bad set form, no variable");
}

struct lisp_type *
set_assigner (struct lisp_type *form)
{
  return list_seq_manip (form, form,
			 "dda",
			 0,
			 "Bad set form, no assignment expression");
}

struct lisp_type *
eval_assignment (struct lisp_type *form,
		 struct lisp_type *environ)
{

  struct lisp_type *assignee = set_assignee (form);
  struct lisp_type *assigner_val_exp = set_assigner (form);
  struct lisp_type *assigner_val = eval (assigner_val_exp, environ);
  struct lisp_type *var = env_find_var (environ,
					assignee);
  if (var)
    {
      set_cdr (var, assigner_val);
      return assigner_val;
    }
  else
    {
      fprintf (stderr, "Undefined variable: ");
      write0 (assignee);
      fprintf (stderr, "\n");
      longjmp (*jmpbuffer, EVAL_ERROR);
    }

}

struct lisp_type *
__eval_quasiquote (struct lisp_type *form,
		 struct lisp_type *environ)
{
  if (consp (form))
    {
      if (symbol_string_value_equals (car (form), "unquote"))
	{
	  return eval (car (cdr (form)), environ);
	}
      else if (consp (cdr (form))
	       && consp (car (cdr (form)))
	       && symbol_string_value_equals (car (car (cdr (form))),
					      "unquote-splicing"))
	{
	  struct lisp_type *expr
	    = list_seq_manip (form, form, "dada", 0, "Invalid unquote-splicing form");
	  struct lisp_type *tosplice = eval (expr, environ);
	  PUSH_STACK (formstack, tosplice);
	  struct lisp_type *total = lists_append (tosplice,
						  __eval_quasiquote (cdr (cdr (form)),
								     environ));
	  PUSH_STACK (formstack, total);
	  struct lisp_type *first_quoted = __eval_quasiquote (car (form), environ);
	  POP_STACK (formstack);
	  POP_STACK (formstack);
	  return make_cons (first_quoted, total);
	}
      else
	{
	  struct lisp_type *careval = __eval_quasiquote (car (form), environ);
	  PUSH_STACK (formstack, careval);
	  struct lisp_type *cdreval = __eval_quasiquote (cdr (form), environ);
	  POP_STACK (formstack);
	  return make_cons (careval, cdreval);
	}
    }
  else
    return form;
}

struct lisp_type *
eval_quoted (struct lisp_type *form, struct lisp_type *environ)
{
  if (symbol_string_value_equals (car (form), "quasiquote"))
    {
      return __eval_quasiquote (car (cdr (form)), environ);
    }
  else
    return car (cdr (form));
}


struct lisp_type *eval (struct lisp_type *form, struct lisp_type *environ)
{
  form_global = form;
  environ_global = environ;
  PUSH_STACK (formstack, form);
  PUSH_STACK (formstack, environ);
  struct lisp_type *rval = NULL;
  if (self_evaluatingp (form))
    rval = form;
  else if (variablep (form))
    rval = eval_var (form, environ);
  else if (conditionalp (form))
    rval = eval_conditional (form, environ);
  else if (lambdap (form))
    rval = eval_lambda (form, environ);
  else if (macrop (form))
    rval = eval_macro (form, environ);
  else if (definitionp (form))
    rval = eval_definition (form, environ);
  else if (sequencep (form))
    rval = eval_sequence (cdr (form), environ);
  else if (assignmentp (form))
    rval = eval_assignment (form, environ);
  else if (quotedp (form))
    rval = eval_quoted (form, environ);
  else if (applicationp (form))
    rval = eval_application (form, environ);
  else
    {
      fprintf (stderr, "Unrecognized form: \n");
      write1 (form);
      exit (1);
      return NULL;
    }

  form_global = form;
  environ_global = environ;

  PUSH_STACK (formstack, rval);
  gc (false);
  POP_STACK (formstack);
  POP_STACK (formstack);
  POP_STACK (formstack);
  return rval;
}

struct lisp_type *
init_environ (struct lisp_type *base)
{
  

  struct lisp_type *frame
    = make_env_frame ();
  struct lisp_type *env_cur
    = make_cons (frame, base);
  
  add_env_proc ("+", add);
  add_env_proc ("-", sub);
  add_env_proc ("*", mul);
  add_env_proc ("assert", lisp_assert);
  add_env_proc ("=", lisp_int_equal);
  add_env_proc ("symbol=?", lisp_symb_equal);
  add_env_proc ("eq?", scheme_eq);
  add_env_proc ("<", less_than);
  add_env_proc ("list", list_func);
  add_env_proc ("pair?", scheme_consp);
  add_env_proc ("car", scheme_car);
  add_env_proc ("cdr", scheme_cdr);
  add_env_proc ("not", scheme_not);
  add_env_proc ("load", lisp_load);
  add_env_proc ("car", lisp_car);
  add_env_proc ("cdr", lisp_cdr);
  add_env_proc ("cons", lisp_cons);
  add_env_proc ("error", scheme_error);
  add_env_proc ("dbg-up", scheme_dbg_up);
  add_env_proc ("dbg-down", scheme_dbg_down);
  add_env_proc ("sys-open", scheme_open);
  add_env_proc ("sys-read", scheme_sys_read);
  add_env_proc ("sys-close", scheme_close);
  add_env_proc ("sys-write", scheme_sys_write);
  add_env_proc ("string=?", scheme_string_equalp);
  add_env_proc ("symbol->string", scheme_symbol_to_string);
  add_env_proc ("string->symbol", scheme_string_to_symbol);
  add_env_proc ("make-vector", scheme_make_vector);
  add_env_proc ("vector-ref", scheme_vector_ref);
  add_env_proc ("vector-set!", scheme_vector_set);
  add_env_proc ("vector-concat", scheme_vector_concat);
  add_env_proc ("vector-len", scheme_vector_len);
  add_env_proc ("vector?", scheme_vectorp);
  add_env_proc ("read", scheme_read);
  add_env_proc ("write", scheme_write);
  add_env_val ("true", TRUE_VALUE);
  add_env_val ("false", FALSE_VALUE);

  return env_cur;
}


void
init_pairs (void)
{
  /* bzero (conses, sizeof (conses)); */
  /* conses_idx = 0; */
  bzero (cars, sizeof (cars));
  bzero (cdrs, sizeof (cdrs));
}


void
init_stacks ()
{
  INIT_STACK (formstack);
  INIT_STACK (conses);
  INIT_STACK (eval_rval_stack);
  INIT_STACK (backtrace);
}

struct lisp_type *
user_prompt (struct port *inp, char *prompt)
{
  printf (";; %s Eval Input =>\n", prompt);
  fflush (stdout);
  return read1 (inp);
}

void
user_print (struct lisp_type *in)
{
  printf (";; Value =>\n");
  fflush (stdout);
  write1 (in);
}

struct lisp_type *
repl (struct lisp_type *environ,
      char *prompt,
      struct port *inp)
{
  struct lisp_type *form = NULL;
  while ((form = user_prompt (inp, prompt)))
    {
      if (form == EOF_VALUE)
	return EOF_VALUE;
      struct lisp_type *rval = eval (form, environ);
      PUSH_STACK (eval_rval_stack, rval);
      user_print (rval);
    }
  return NULL;
}

void
init_io (void)
{
  STDIN_CPORT_READER.state = stdin;
}

int
main (int argc, char **argv)
{
  init_stacks ();
  init_pairs ();
  init_io ();
  jmp_buf jumpbuffer_main;
  FILE *inp;
  char *toload = NULL;
  int rval = 0;
  if (argc == 2)
    {
      toload = argv[1];
      inp = stdin;
    }
  else if (argc == 1)
    {
      inp = stdin;
    }
  else
    {
      fprintf (stderr, "Invalid arguments!");
      rval = EXITCODE_BAD_ARGS;
      goto exit_normal;
    }

  STDIN_CPORT_READER.state = inp;
  struct lisp_type *environ = NIL_VALUE;
  environ = init_environ (environ);
  struct lisp_type *form = NULL;
  PUSH_STACK (formstack, environ);
  the_global_environment = environ;
  int unwind_stack_idx = formstack->index;
  jmpbuffer = &jumpbuffer_main;
  if ((rval = setjmp (jumpbuffer_main)) != 0)
    {
      if (rval == READER_EOF)
	{
	  rval = EXITCODE_SUCCESS;
	  goto exit_eof;
	}
      else if (rval == ASSERTION_FAILURE)
	{
	  goto exit_assert;
	}
      else if (rval == DEBUG_RESTART)
	{
	  formstack->index = unwind_stack_idx;
	  backtrace->index = 0;
	  fprintf (stderr, ";; Leaving debugger...\n");
	  goto repl_enter;
	}
      else
	{
	  formstack->index = unwind_stack_idx;
	  fprintf(stderr, "Unknown error occured, recovering...\n ");
	  goto repl_enter;
	}
    }
  if (toload)
    {
      write1 (__lisp_load (toload, the_global_environment));
      printf(";; loaded %s\n", toload);
    }
 repl_enter:
  repl (environ, "Toplevel", &STDIN_CPORT_READER);
  goto exit_normal;
 exit_assert:
  fprintf (stderr, "Assertion failure\n");
  fflush (stdout);
  fflush (stderr);
  goto exit_normal;
 exit_eof:
  printf("EOF\n");
 exit_normal:
  if (inp != stdin) fclose (inp);
  CLEAR_STACK (formstack);
  gc (true);
  return rval;
}
