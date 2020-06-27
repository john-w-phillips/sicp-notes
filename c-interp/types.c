#include "scheme.h"
const char *types_to_syms[] = {
  [NUMBER] = "int",
  [SYMBOL] = "symbol",
  [PAIR] = "pair",
  [BOOLEAN] = "boolean",
  [NIL_TYPE] = "nil",
  [PRIMITIVE_PROC] = "prim-proc",
  [SCHEME_PROC] = "proc",
  [MACRO] = "macro",
  [SCHEME_EOF] = "eof",
  [DOT] = "dot",
  [SCHEME_VECTOR] = "vector",
};

const struct lisp_type NIL = {
  .type = NIL_TYPE
};
const struct lisp_type TRUE = {
  .type = BOOLEAN,
  .v = { 
    .intval = 1
  }
};

const struct lisp_type FALSE = {
  .type = BOOLEAN,
  .v = {
    .intval = 0
  }
};

const struct lisp_type QUOTESYM = {
  .type = SYMBOL,
  .v = {
    .string = {
      .str = "quote",
      .free = false,
    }
  }
};

const struct lisp_type QUASIQUOTE = {
  .type = SYMBOL,
  .v = {
    .string = {
      .str = "quasiquote",
      .free = false
    }
  }
};

const struct lisp_type UNQUOTE = {
  .type = SYMBOL,
  .v =  {
    .string = {
      .str = "unquote",
      .free = false
    }
  }
};

const struct lisp_type UNQUOTE_SPLICE = {
  .type = SYMBOL,
  .v = {
    .string = {
      .str = "unquote-splicing",
      .free = false
    }
  }
};

const struct lisp_type SCHEME_EOF_V = {
  .type = SCHEME_EOF
};

const struct lisp_type SCHEME_DOT = {
  .type = DOT
};


void
free_lisp_type (struct lisp_type *t)
{
  assert (t);
  assert (!is_immutable (t));
  if (vectorp (t))
    {
      free (t->v.vec.mem);
      free (t);
    }
  else if (stringp (t) || symbolp (t))
    {
      if (t->v.string.free)
	free (t->v.string.str);
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
make_char (int c)
{
  struct lisp_type *rval = calloc (1, sizeof *rval);
  rval->type = SCHEME_CHAR;
  rval->v.intval = c;
  return rval;
}

struct lisp_type *
make_symbol(char *str, bool shouldfree)
{
  struct lisp_type *rval = calloc(1, sizeof *rval);
  rval->type = SYMBOL;
  rval->v.string.str = str;
  rval->v.string.free = shouldfree;
  return rval;
}

struct lisp_type *
make_macro (struct lisp_type *formals,
	    struct lisp_type *body,
	    struct lisp_type *env)
{
  struct lisp_type *rval = calloc (1, sizeof *rval);
  rval->type = MACRO;
  rval->v.scheme_proc.scheme_proc_formals = formals;
  rval->v.scheme_proc.scheme_proc_body = body;
  rval->v.scheme_proc.scheme_proc_env = env;
  return rval;
}

struct lisp_type *
make_lambda (struct lisp_type *formals,
	     struct lisp_type *body,
	     struct lisp_type *env)
{
  struct lisp_type *rval = calloc (1, sizeof *rval);
  rval->type = SCHEME_PROC;
  rval->v.scheme_proc.scheme_proc_formals = formals;
  rval->v.scheme_proc.scheme_proc_body = body;
  rval->v.scheme_proc.scheme_proc_env = env;
  return rval;
}


struct lisp_type *
make_primitive_procedure (struct lisp_type *(*proc)(struct lisp_type *))
{
  struct lisp_type *rval = calloc(1, sizeof *rval);
  rval->type = PRIMITIVE_PROC;
  rval->v.proc_value = proc;
  return rval;
}

struct lisp_type *
make_string (char *str, bool shouldfree)
{
  int str_length = strlen(str);
  union scheme_value *mem = calloc (str_length, sizeof(union scheme_value));
  for (int i = 0; i < str_length; ++i)
    {
      mem[i].intval = str[i];
    }
  return make_prealloc_vector(SCHEME_CHAR, str_length, mem);
}

struct lisp_type *
make_number(int num)
{
  struct lisp_type *rval = calloc(1, sizeof *rval);
  rval->type = NUMBER;
  rval->v.intval = num;
  return rval;
}

struct lisp_type *
car (struct lisp_type *pair)
{
  //assert(pair->type == PAIR);
  eval_assert((pair->type == PAIR),
	      "CAR of a non-pair!");
  return cars[pair->v.pair_index];
}

struct lisp_type *
cdr (struct lisp_type *pair)
{
  eval_assert ((pair->type == PAIR),
	       "CDR of a non-pair!");
  return cdrs[pair->v.pair_index];
}

enum lisp_types
sym_to_type (struct lisp_type *symb) {
  for (int i = 0; i < ARRAY_SIZE(types_to_syms); ++i) {
    if (strcmp (types_to_syms[i],
		symbol_string_value (symb)) == 0)
      return i;
  }
  return INVALID;
}

struct lisp_type *
make_prealloc_vector (enum lisp_types type,
		      unsigned nitems,
		      union scheme_value *mem)
{
  struct lisp_type *rval = calloc (1, sizeof *rval);
  rval->type = SCHEME_VECTOR;
  rval->v.vec.mem = mem;
  rval->v.vec.nitems = nitems;
  rval->v.vec.type = type;
  rval->copied = false;
  return rval;
}

struct lisp_type *
make_vector (enum lisp_types type,
	     struct lisp_type *items) {
  struct lisp_type *rval = calloc (1, sizeof *rval);
  unsigned nitems = __list_length (0, items);  
  rval->type = SCHEME_VECTOR;
  rval->v.vec.mem = calloc(nitems, sizeof(union scheme_value));
  rval->v.vec.nitems = nitems;
  rval->v.vec.type = type;
  rval->copied = false;
  int elem = 0;
  for (struct lisp_type *i = items; i != NIL_VALUE; ((i = cdr (i)),elem = elem + 1))
    {
      struct lisp_type *item = car (i);
      if (item->type != type)
	scheme_signal_eval_error("Bad item type in make-vector argument list, %d",
				 elem);
      rval->v.vec.mem[elem] = item->v;
      assert (elem < nitems);
    }
  return rval;
}


#define MAX_STRING 2048
char strbuf[MAX_STRING];

char *
string_c_string (struct lisp_type *s)
{
  eval_assert (stringp (s), "Value is not a string");
  int i = 0;
  for (i = 0; i < s->v.vec.nitems; ++i)
    {
      strbuf[i] = s->v.vec.mem[i].intval;
    }
  strbuf[i] = '\0';
  return strbuf;
}

struct lisp_type *
make_cons (struct lisp_type *car,
	  struct lisp_type *cdr)
{
  struct lisp_type *rval = calloc(1, sizeof *rval);
  rval->type = PAIR;
  assert (max_cons_idx < NPAIRS);
  cars[max_cons_idx] = car;
  cdrs[max_cons_idx] = cdr;
  rval->v.pair_index = max_cons_idx++;
  PUSH_STACK (conses, rval);
  return rval;
}
