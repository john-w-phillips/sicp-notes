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
  [SCHEME_VECTOR_MIXED] = "vector-mixed",
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
  if (mixed_vectorp (t))
    {
      free (vector_mixedmem(t));
      free (t);
    }
  else if (vectorp (t))
    {
      free (t->v.vec.mem);
      free (t);
    }
  else if (symbolp (t))
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
  PUSH_STACK (conses, rval);
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
  PUSH_STACK (conses, rval);
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
  PUSH_STACK (conses, rval);
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
    if (types_to_syms[i] && strcmp (types_to_syms[i],
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
  PUSH_STACK (conses, rval);
  return rval;
}

struct lisp_type *
make_prealloc_mixed_vector (unsigned nitems,
			    struct lisp_type **mem)
{
  struct lisp_type *rval = calloc (1, sizeof *rval);
  rval->type = SCHEME_VECTOR_MIXED;
  rval->v.vec.mixed_mem = mem;
  rval->v.vec.nitems = nitems;
  rval->v.vec.type = SCHEME_VECTOR_MIXED;
  rval->copied = false;
  PUSH_STACK (conses, rval);
  return rval;
}

struct lisp_type *
make_mixed_vector (struct lisp_type *items)
{
  struct lisp_type *rval = calloc (1, sizeof *rval);
  unsigned nitems = __list_length (0, items);
  rval->type = SCHEME_VECTOR_MIXED;
  rval->v.vec.mixed_mem = calloc (nitems, sizeof(struct lisp_type));
  rval->v.vec.nitems = nitems;
  struct lisp_type *iter = items;
  
  for (int i = 0; i < nitems; ++i,iter=cdr(iter))
    {
      rval->v.vec.mixed_mem[i] = car (iter);
    }
  PUSH_STACK (conses, rval);
  return rval;
}

struct lisp_type *
make_vector (enum lisp_types type,
	     struct lisp_type *items)
{
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
  PUSH_STACK (conses, rval);
  return rval;
}


struct lisp_type *
mixed_vector_concat (struct lisp_type *v1,
		     struct lisp_type *v2)
{
  assert (mixed_vectorp (v1) && mixed_vectorp (v2));
  unsigned len1 = vector_len (v1);
  unsigned len2 = vector_len (v2);

  struct lisp_type **mem = calloc( len1 + len2, sizeof(struct lisp_type *));

  memcpy (mem,
	  vector_mixedmem (v1),
	  len1 * sizeof (struct lisp_type *));
  memcpy (mem + len1,
	  vector_mixedmem (v2),
	  len2 * sizeof (struct lisp_type *));
  return make_prealloc_mixed_vector (len1 + len2,
				     mem);
}

struct lisp_type *
vector_concat (struct lisp_type *v1,
	       struct lisp_type *v2)
{
  assert (vectorp (v1) && vectorp (v2));
  assert ((vector_elemtype (v1) == vector_elemtype (v2)) && v1->type != SCHEME_VECTOR_MIXED);
  unsigned len1 = vector_len (v1);
  unsigned len2 = vector_len (v2);

  union scheme_value *mem = calloc (len1 + len2, sizeof(union scheme_value));

  memcpy (mem,
	  vector_unimem (v1),
	  len1 * sizeof(union scheme_value));
  memcpy (mem + len1,
	  vector_unimem (v2),
	  len2 * sizeof(union scheme_value));
  return make_prealloc_vector (vector_elemtype (v1),
			       len1 + len2,
			       mem);
}

struct lisp_type *
mixed_vector_push (struct lisp_type *v1,
		   struct lisp_type *i)
{
}

struct lisp_type *
vector_push (struct lisp_type *v1, struct lisp_type *i)
{
}

struct lisp_type *
vector_pop (struct lisp_type *v1)
{
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

