#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <setjmp.h>
#define NPAIRS 2048

enum lisp_types {
  NUMBER,
  SYMBOL,
  STRING,
  PAIR,
  BOOLEAN,
  NIL_TYPE,
  PRIMITIVE_PROC,
  SCHEME_PROC,
};
jmp_buf jmpbuffer;
struct scheme_proc
{
    struct lisp_type *scheme_proc_body;
    struct lisp_type *scheme_proc_formals;
    struct lisp_type *scheme_proc_env;
};

struct scheme_string
{
  bool free;
  char *str;
};

#define READER_EOF 2
#define READER_INVALID_SYNTAX 3
#define ASSERTION_FAILURE 4
struct lisp_type
{
  enum lisp_types type;
  int copied;
  union
  {
    /* char *strval; */
    struct scheme_string string;
    int intval;
    unsigned pair_index;
    struct scheme_proc scheme_proc;
    struct lisp_type *(*proc_value)(struct lisp_type *argl);
  };
};

struct lisp_item_stack {
  unsigned index;
  unsigned size;
  struct lisp_type *items[];
};

#define DECLARE_LISP_STACK(name, nitems)				\
  char name##_storage[(sizeof(struct lisp_item_stack) / sizeof(char)) +	\
		      nitems * (sizeof(struct lisp_item *) / sizeof(char))] = {0}; \
  struct lisp_item_stack *name = (struct lisp_item_stack *)name##_storage; \
  const int name##_size = nitems;

#define PUSH_STACK(stack, item)			\
  do {						\
    assert (stack->index + 1 <= stack->size);	\
    stack->items[stack->index++] = item;	\
  } while (0)

#define POP_STACK(stack)			\
  do {						\
    assert (stack->index > 0);			\
    stack->index--;				\
  } while (0)

#define INIT_STACK(stack) do {			\
    stack->size = stack##_size;			\
    stack->index = 0;				\
  } while (0)

#define CLEAR_STACK(stack) stack->index = 0; 
struct lisp_type *cars[NPAIRS];
struct lisp_type *cdrs[NPAIRS];

/* struct lisp_type *formstack[NPAIRS]; */



/*
  The (stupid) way we allocate types means that the heads of conses
  lists are not on the actual cars/cdrs arrays but must be kept track
  of separately so we don't leak memory.
 */
/* struct lisp_type *conses[NPAIRS]; */
/* unsigned conses_idx = 0; */

/* #define push_cons(x) do {			\ */
/*     assert (consp (x));				\ */
/*     assert (conses_idx < NPAIRS);		\ */
/*     conses[conses_idx++] = x;			\ */
/*   } while (0)					\ */

DECLARE_LISP_STACK (formstack, NPAIRS);

DECLARE_LISP_STACK (eval_rval_stack, NPAIRS * 5);

DECLARE_LISP_STACK (conses, NPAIRS);
/* unsigned formstack_idx = 0; */
/* #define push_form(x)				\ */
/*   do {						\ */
/*     assert(formstack_idx < NPAIRS);		\ */
/*     formstack[formstack_idx++] = x;		\ */
/*   } while (0); */

/* #define pop_form() \ */
/*   formstack_idx-- */

unsigned max_cons_idx = 0;
#define GC_THRESH 0.75

struct lisp_type *eval (struct lisp_type *form, struct lisp_type *environ);
const struct lisp_type NIL = {
  .type = NIL_TYPE
};
const struct lisp_type TRUE = {
  .type = BOOLEAN,
  .intval = 1
};

const struct lisp_type FALSE = {
  .type = BOOLEAN,
  .intval = 0
};

#define NIL_VALUE (struct lisp_type *)&NIL
#define TRUE_VALUE  (struct lisp_type *)&TRUE
#define FALSE_VALUE (struct lisp_type *)&FALSE

bool
is_immutable (struct lisp_type *it)
{
  return (it == NIL_VALUE
	  || it == TRUE_VALUE
	  || it == FALSE_VALUE);
}

#define symbolp(x) ((x)->type == SYMBOL)
#define stringp(x) ((x)->type == STRING)
#define numberp(x) ((x)->type == NUMBER)
#define variablep(x) ((x)->type == SYMBOL)
#define symbol_string_value(x) ((x)->string.str)
#define number_value(x) ((x)->intval)
#define consp(x) ((x)->type == PAIR)
#define primitive_procedurep(x) ((x)->type == PRIMITIVE_PROC)
#define primitive_procedure_proc(x) ((x)->proc_value)
#define self_evaluatingp(x) (numberp (x) || stringp (x))
#define applicationp(x) (consp (x))
#define falsep(x) ((x)->type == BOOLEAN && x->intval == 0)
#define truep(x) (!falsep (x) && !nilp (x))
#define scheme_proc_environ(x) ((x)->scheme_proc.scheme_proc_env)
#define scheme_proc_formals(x) ((x)->scheme_proc.scheme_proc_formals)
#define scheme_proc_body(x) ((x)->scheme_proc.scheme_proc_body)
#define scheme_procedurep(x) ((x)->type == SCHEME_PROC)
#define string_c_string symbol_string_value
#define definitionp(x) (consp (x) &&					\
			symbolp (car (x)) &&				\
			symbol_string_value_equals (car (x), "define"))

#define environ_first_frame car
#define enclosing_environ cdr
#define lambdap(x) \
  (consp (x) && symbol_string_value_equals (car (x), "lambda"))
#define symbol_string_value_equals(sym, c_string)			\
  (symbolp (sym) && (strcmp (c_string, sym->string.str) == 0))
#define conditionalp(x) (consp (x) &&					\
			 symbolp (car (x)) &&				\
			 symbol_string_value_equals (car (x), "if"))

#define symbol_equalp(x, y) \
  (strcmp (x->string.str, y->string.str) == 0)

#define nilp(x) ((x) == &NIL)

void
free_lisp_type (struct lisp_type *t)
{
  assert (t);
  assert (!is_immutable (t));
  if (stringp (t) || symbolp (t))
    {
      if (t->string.free)
	free (t->string.str);
      bzero (t, sizeof *t);
      free (t);
    }
  else
    {
      bzero (t, sizeof *t);
      free (t);
    }
}

struct lisp_type *
make_symbol(char *str, bool shouldfree)
{
  struct lisp_type *rval = calloc(1, sizeof *rval);
  rval->type = SYMBOL;
  rval->string.str = str;
  rval->string.free = shouldfree;
  return rval;
}

struct lisp_type *
make_lambda (struct lisp_type *formals,
	     struct lisp_type *body,
	     struct lisp_type *env)
{
  struct lisp_type *rval = calloc (1, sizeof *rval);
  rval->type = SCHEME_PROC;
  rval->scheme_proc.scheme_proc_formals = formals;
  rval->scheme_proc.scheme_proc_body = body;
  rval->scheme_proc.scheme_proc_env = env;
  return rval;
}


struct lisp_type *
make_primitive_procedure (struct lisp_type *(*proc)(struct lisp_type *))
{
  struct lisp_type *rval = calloc(1, sizeof *rval);
  rval->type = PRIMITIVE_PROC;
  rval->proc_value = proc;
  return rval;
}

struct lisp_type *
make_string (char *str, bool shouldfree)
{
  struct lisp_type *rval = calloc (1, sizeof *rval);
  rval->type = STRING;
  rval->string.str = str;
  rval->string.free = shouldfree;
  return rval;
}

struct lisp_type *
make_number(int num)
{
  struct lisp_type *rval = calloc(1, sizeof *rval);
  rval->type = NUMBER;
  rval->intval = num;
  return rval;
}

struct lisp_type *
car (struct lisp_type *pair)
{
  assert(pair->type == PAIR);
  return cars[pair->pair_index];
}

struct lisp_type *
cdr (struct lisp_type *pair)
{
  assert (pair->type == PAIR);
  return cdrs[pair->pair_index];
}

struct lisp_type *
make_cons(struct lisp_type *car,
	  struct lisp_type *cdr)
{
  struct lisp_type *rval = calloc(1, sizeof *rval);
  rval->type = PAIR;
  assert (max_cons_idx < NPAIRS);
  cars[max_cons_idx] = car;
  cdrs[max_cons_idx] = cdr;
  rval->pair_index = max_cons_idx++;
  PUSH_STACK (conses, rval);
  return rval;
}

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
  cdrs[pair1->pair_index] = val;
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
    return cdr (valuecel);
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
read0 (FILE *fp);

struct lisp_type *
read_list (FILE *fp)
{
  //int c = getc (stdin);
  struct lisp_type *h = read0 (fp);
  if (h)
    {
      return make_cons (h, read_list (fp));
    }
  else
    {
      return (struct lisp_type *)&NIL;
    }
}

#define NUMSIZ 128

struct lisp_type *
read_num (int c, FILE *fp)
{
  char numbuf[NUMSIZ] = {c}, *endptr = NULL;;
  int idx = 1;
  while ( isdigit (c = getc (fp)) )
    {
      numbuf[idx++] = c;
    }
  ungetc (c, fp);
  numbuf[idx] = '\0';
  int rval = strtol (numbuf, &endptr, 10);
  if (*endptr == '\0')
    return make_number(rval);
  else
    {
      fprintf(stderr, "Invalid number: %s\n", numbuf);
      return NULL;
    }
}
#define SYMBSIZE 1024

static bool
isspecial (int c)
{
  return (c == '(' || c == ')' || isspace (c) || c == '.');
}

struct lisp_type *
read_symbol (int c, FILE *fp)
{
  char symbuf[SYMBSIZE] = {c};
  int index = 1;

  while ( !isspecial (c = getc (fp)) )
    {
      symbuf[index++] = c;
    }
  symbuf[index] = '\0';
  ungetc (c, fp);
  return make_symbol (strdup (symbuf), true);
}

struct lisp_type *
read_string (int c, FILE *fp)
{
  char strbuf[SYMBSIZE] = {};
  int index = 0;

  while ( (c = getc (fp)) != '"')
    strbuf[index++] = c;

  strbuf[index] = '\0';
  return make_string (strdup (strbuf), true);
    
}

struct lisp_type *
read0 (FILE *fp)
{
  int c = getc (fp);
  if (c == '(')
    return read_list (fp);
  else if (isdigit (c))
    return read_num (c, fp);
  else if (c == '"')
    return read_string (c, fp);
  else if (isspace (c))
    return read0 (fp);
  else if ((isalpha (c) || isgraph (c)) && !isspecial(c))
    return read_symbol (c, fp);
  else if (c == ')')
    return NULL;
  else if (c == EOF)
    {
      longjmp (jmpbuffer, READER_EOF);
    }
  else
    {
      fprintf(stderr, "Unspecified reader error!\n");
      exit(1);
    }
  return NULL;
}

struct lisp_type *
read (FILE *fp)
{
  struct lisp_type *rval = read0 (fp);
  if (! rval)
    {
      fprintf (stderr, "Illegal list term\n");
      exit (1);
    }
  else
    return rval;
}

static int
add_inner(struct lisp_type *argl)
{
  if (nilp (argl))
    {
      return 0;
    }
  else
    {
      assert (consp (argl));
      return (number_value (car (argl))
	      + add_inner (cdr (argl)));
    }
}

static int
sub_inner (struct lisp_type *argl)
{
  if (nilp (argl))
    return 0;
  else
    {
      assert (consp (argl));
      return (number_value (car (argl))
	      - sub_inner (cdr (argl)));
    }
}

struct lisp_type *
sub (struct lisp_type *argl)
{
  return make_number (sub_inner (argl));
}


struct lisp_type *
lisp_assert (struct lisp_type *argl)
{
  
  if (truep (car (argl)))
    return TRUE_VALUE;
  else
    {
      longjmp(jmpbuffer, ASSERTION_FAILURE);
    }
}
struct lisp_type *
add (struct lisp_type *argl)
{

  if (nilp (argl))
    return 0;
  else
    {
      assert (consp (argl));
      int result = add_inner(argl);
      return make_number (result);
    }
}


struct lisp_type *
less_than (struct lisp_type *argl)
{
  assert (consp (argl));
  if (number_value (car (argl))
      < number_value (car (cdr (argl))))
    {
      return TRUE_VALUE;
    }
  return FALSE_VALUE;
}


void
write0 (struct lisp_type *t);

void
write_list (struct lisp_type *t, bool space)
{
  if (t->type == NIL_TYPE)
    printf (")");
  else
    {
      if (space)
	printf (" ");
      write0 (car (t));
      write_list (cdr (t), true);
    }
}

void
write0 (struct lisp_type *t)
{
  switch (t->type)
    {
    case NUMBER:
      printf ("%d", t->intval);
      break;
    case NIL_TYPE:
      printf ("nil");
      break;
    case SYMBOL:
      printf ("%s", symbol_string_value (t));
      break;
    case PAIR:
      printf ("(");
      write_list (t, false);
      break;
    case STRING:
      printf("\"%s\"", string_c_string (t));
      break;
    case SCHEME_PROC:
      printf("#[scheme procedure]");
      break;
    case PRIMITIVE_PROC:
      printf("#[primitive procedure]");
      break;
    case BOOLEAN:
      if (t == FALSE_VALUE)
	printf("#f");
      else if (t == TRUE_VALUE)
	printf("#t");
      else
	{
	  fprintf (stderr, "Invalid boolean, there is a bug!\n");
	  exit (1);
	}
      break;
    default:
      fprintf (stderr, "can't write this type\n");
      exit (1);
    }
}

void write (struct lisp_type *t)
{
  write0 (t);
  printf("\n");
}

struct lisp_type *
eval_var (struct lisp_type *form,
	  struct lisp_type *environ)
{
  assert (symbolp (form));
  struct lisp_type *r = env_find_var (environ, form);
  if (r)
    return r;
  else
    {
      fprintf (stderr, "Error: No such variable: %s\n",
	       symbol_string_value (form));
      exit (1);
    }
}

struct lisp_type *eval_arglist (struct lisp_type *form,
				struct lisp_type *environ)
{
  assert (form);
  assert (consp (form) || nilp (form));
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
      write (symbol_list);
      write (value_list);
      exit (1);
    }
  return make_cons (frame, environ);
}

struct lisp_type *eval_sequence (struct lisp_type *forms,
				 struct lisp_type *environ)
{
  assert (consp (forms));
  if (nilp (cdr (forms)))
      return eval (car (forms), environ);
  else
    {
      eval (car (forms), environ);
      return eval_sequence (cdr (forms), environ);
    }
  return NULL;
}

struct lisp_type *eval_application (struct lisp_type *form,
				    struct lisp_type *environ)
{
  assert (cdr (form));
  struct lisp_type *proc = eval (car (form), environ);
  assert (proc);
  assert (scheme_procedurep (proc)
	  || primitive_procedurep (proc));

  /* push_form (proc); */
  PUSH_STACK (formstack, proc);

  struct lisp_type *argl = cdr (form);
  /* push_form (argl); */

  struct lisp_type *eval_argl
    = eval_arglist (argl, environ);

  struct lisp_type *lisp_rval = NULL;
  if (primitive_procedurep (proc))
    {
      lisp_rval = primitive_procedure_proc (proc)(eval_argl);
    }
  else if (scheme_procedurep (proc))
    {
      /* push_form(eval_argl); */
      PUSH_STACK (formstack, eval_argl);
      struct lisp_type *lambda_env =
	scheme_proc_environ (proc);
      struct lisp_type *lambda_formals =
	scheme_proc_formals (proc);
      struct lisp_type *new_environ
	= environment_extend (lambda_env,
			      lambda_formals,
			      eval_argl);
      lisp_rval =  eval_sequence (scheme_proc_body (proc),
				  new_environ);
      /* pop_form (); */
      POP_STACK (formstack);
    }
  else
    {
      fprintf (stderr, "Bad procedure type.\n");
      //exit (1);
      abort();
    }
  /* pop_form (); */
  POP_STACK (formstack);
  //printf ("return v\n");
  return lisp_rval;
}

struct lisp_type *
syntax_car (struct lisp_type *form, char *errorstr)
{
  if (!consp (form))
    {
      fprintf (stderr, "CAR: Below form has invalid syntax: %s\n",
	       errorstr);
      write (form);
      exit (1);
    }
  else
    {
      return car (form);
    }
}

struct lisp_type *
syntax_cdr (struct lisp_type *form, char *errorstr)
{
  if (!consp (form))
    {
      fprintf (stderr, "CDR: Below form has invalid syntax: %s\n",
	       errorstr);
      write (form);
      exit (1);
    }
  else
    return cdr (form);
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
  fprintf (stderr, "Invalid syntax: %s. Form: \n",
	   errorstr);
  write (form_orig);
  longjmp (jmpbuffer, READER_INVALID_SYNTAX);
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
  if (number_value (arg1) == number_value (arg2))
    {
      return TRUE_VALUE;
    }
  else
    {
      return FALSE_VALUE;
    }
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

void gc (bool force);
struct lisp_type *eval (struct lisp_type *form, struct lisp_type *environ)
{
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
  else if (definitionp (form))
    rval = eval_definition (form, environ);
  else if (applicationp (form))
    rval = eval_application (form, environ);
  else
    {
      fprintf (stderr, "Unrecognized form: \n");
      write (form);
      exit (1);
      return NULL;
    }

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
  
#define add_env_proc(str, sym)					\
  do {									\
    struct lisp_type *strval = make_symbol (str, false);		\
    struct lisp_type *prim = make_primitive_procedure (sym);		\
    PUSH_STACK (eval_rval_stack, prim);					\
    PUSH_STACK (eval_rval_stack, strval);				\
    env_frame_set_var_value (frame, strval, prim);			\
  } while (0)


#define add_env_val(str, expr)				\
  do {								\
    struct lisp_type *strval = make_symbol (str, false);	\
    env_frame_set_var_value (frame, strval, expr);		\
    PUSH_STACK (eval_rval_stack, strval);			\
  } while (0)


  struct lisp_type *frame
    = make_env_frame ();
  struct lisp_type *env_cur
    = make_cons (frame, base);
  
  add_env_proc ("+", add);
  add_env_proc ("-", sub);
  add_env_proc ("assert", lisp_assert);
  add_env_proc ("=", lisp_int_equal);
  add_env_proc ("<", less_than);

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
  pair->pair_index = newcells->next++;
  pair->copied = true;

  if (consp (carpair))
    {
      copy_cons_cells (carpair, newcells);
    }
  else if (scheme_procedurep (carpair))
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
  else if (scheme_procedurep (cdrpair))
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
  assert (scheme_procedurep (proc));
  proc->copied = true;
  copy_cons_cells (scheme_proc_body (proc),
		   newcells);
  copy_cons_cells (scheme_proc_environ (proc),
		   newcells);
  copy_cons_cells (scheme_proc_formals (proc),
		   newcells);		   
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
      else if (scheme_procedurep (roots[i]))
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
  if (item && scheme_procedurep (item))
    {
      scheme_proc_body (item)->copied = false;
      scheme_proc_formals (item)->copied = false;
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

  memcpy (cdrs, cells.cdrs, sizeof (cdrs));
  memcpy (cars, cells.cars, sizeof (cars));
  max_cons_idx = cells.next;
}

void
init_stacks ()
{
  INIT_STACK (formstack);
  INIT_STACK (conses);
  INIT_STACK (eval_rval_stack);
}



int
main (int argc, char **argv)
{
  init_stacks ();
  init_pairs ();
  FILE *inp;
  int rval = 0;
  if (argc == 2)
    {
      inp = fopen (argv[1], "r");
    }
  else if (argc == 1)
    {
      inp = stdin;
    }
  else
    {
      fprintf (stderr, "Invalid arguments!");
      rval = 1;
      goto exit_normal;
    }

  struct lisp_type *environ = NIL_VALUE;
  environ = init_environ (environ);
  struct lisp_type *form = NULL;
  PUSH_STACK (formstack, environ);
  int unwind_stack_idx = formstack->index;
  if ((rval = setjmp (jmpbuffer)) != 0)
    {
      if (rval == READER_EOF)
	goto exit_eof;
      else if (rval == ASSERTION_FAILURE)
	goto exit_assert;
      else
	{
	  formstack->index = unwind_stack_idx;
	  fprintf(stderr, "recovering..\n");
	}
    }
  while ((form = read (inp)))
    {
      struct lisp_type *rval = eval (form, environ);
      PUSH_STACK (eval_rval_stack, rval);
      write (rval);
    }
 exit_assert:
  rval = 2;
  printf ("Assertion failure\n");
  goto exit_normal;
 exit_eof:
  printf("EOF\n");
 exit_normal:
  if (inp != stdin) fclose (inp);
  CLEAR_STACK (formstack);
  gc (true);
  return rval;
}
