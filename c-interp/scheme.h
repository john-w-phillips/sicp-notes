#ifndef _SCHEME_H_
#define _SCHEME_H_
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <setjmp.h>
#include <stdarg.h>
#define NPAIRS 2048

enum lisp_types {
  NUMBER,
  SYMBOL,
  PAIR,
  BOOLEAN,
  NIL_TYPE,
  PRIMITIVE_PROC,
  SCHEME_PROC,
  MACRO,
  SCHEME_EOF,
  DOT,
  SCHEME_VECTOR,
  SCHEME_CHAR,
  INVALID,
};
#define ARRAY_SIZE(arr) (sizeof (arr) / sizeof (arr[0]))
extern const char *types_to_syms[];
extern jmp_buf *jmpbuffer;
extern struct lisp_type *the_global_environment;
extern struct lisp_type *form_global;
extern struct lisp_type *environ_global;



enum lisp_types
sym_to_type (struct lisp_type *symb);

void
scheme_signal_eval_error (char *msg, ...);
#define eval_assert(x, msg) do { if (!(x)) {scheme_signal_eval_error(msg);} } while (0)
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

/*
  All items have the same type, specified by type.
 */
struct scheme_vec
{
  enum lisp_types type;
  unsigned nitems;
  union scheme_value  *mem;
};


#define READER_EOF 2
#define READER_INVALID_SYNTAX 3
#define ASSERTION_FAILURE 4
#define EVAL_ERROR 5
#define DEBUG_RESTART 6

#define EXITCODE_BAD_ARGS 1
#define EXITCODE_SUCCESS 2
union scheme_value
{
    struct scheme_vec vec;
    struct scheme_string string;
    int intval;
    unsigned pair_index;
    struct scheme_proc scheme_proc;
    struct lisp_type *(*proc_value)(struct lisp_type *argl);
};


struct lisp_type
{
  enum lisp_types type;
  int copied;
  union scheme_value v;
};

struct lisp_item_stack {
  unsigned index;
  unsigned size;
  struct lisp_type *items[];
};

struct port {
  void *state;
  int (*getbyte)(void *port);
  void (*ungetbyte)(int c, void *port);
  void (*writebyte)(void *port, int c);
};


#define cport_getbyte(x) ((x)->getbyte(x->state))
#define cport_ungetbyte(c, x) ((x)->ungetbyte(c, x->state))
#define cport_writebyte(x, c) ((x)->writebyte(x->state, c))

extern struct port STDIN_CPORT_READER;

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

extern struct lisp_item_stack *backtrace;
extern struct lisp_item_stack *formstack;
extern struct lisp_item_stack *eval_rval_stack;
extern struct lisp_item_stack *conses;
extern struct lisp_type *cars[];
extern struct lisp_type *cdrs[];
extern unsigned max_cons_idx;
#define GC_THRESH 0.75
extern const struct lisp_type SCHEME_DOT;
extern const struct lisp_type SCHEME_EOF_V;
extern const struct lisp_type UNQUOTE_SPLICE;
extern const struct lisp_type NIL;
extern const struct lisp_type TRUE;
extern const struct lisp_type FALSE;
extern const struct lisp_type QUOTESYM;
extern const struct lisp_type QUASIQUOTE;
extern const struct lisp_type UNQUOTE;
extern const struct lisp_type UNQUOTE_SPLICE;
#define DOT_VALUE               ((struct lisp_type *)&SCHEME_DOT)
#define EOF_VALUE               ((struct lisp_type *)&SCHEME_EOF_V)
#define QUOTE_VALUE		((struct lisp_type *)&QUOTESYM)
#define QUASIQUOTE_VALUE	((struct lisp_type *)&QUASIQUOTE)
#define UNQUOTE_VALUE		((struct lisp_type *)&UNQUOTE)
#define NIL_VALUE		((struct lisp_type *)&NIL)
#define TRUE_VALUE		((struct lisp_type *)&TRUE)
#define FALSE_VALUE		((struct lisp_type *)&FALSE)
#define UNQUOTE_SPLICE_VALUE    ((struct lisp_type *)&UNQUOTE_SPLICE)

#define symbolp(x) ((x)->type == SYMBOL)
#define stringp(x) ((x)->type == SCHEME_VECTOR && (x)->v.vec.type == SCHEME_CHAR)
#define numberp(x) ((x)->type == NUMBER)
#define variablep(x) ((x)->type == SYMBOL)
#define booleanp(x) ((x)->type == BOOLEAN)
#define vectorp(x) ((x)->type == SCHEME_VECTOR)
#define symbol_string_value(x) ((x)->v.string.str)
#define number_value(x) ((x)->v.intval)
#define consp(x) ((x)->type == PAIR)
#define primitive_procedurep(x) ((x)->type == PRIMITIVE_PROC)
#define primitive_procedure_proc(x) ((x)->v.proc_value)

#define scheme_macrop(x) ((x)->type == MACRO)
#define falsep(x) ((x)->type == BOOLEAN && x->v.intval == 0)
#define truep(x) (!falsep (x) && !nilp (x))
#define scheme_proc_environ(x) ((x)->v.scheme_proc.scheme_proc_env)
#define scheme_proc_formals(x) ((x)->v.scheme_proc.scheme_proc_formals)
#define scheme_proc_body(x) ((x)->v.scheme_proc.scheme_proc_body)
#define scheme_procedurep(x) ((x)->type == SCHEME_PROC)
#define nilp(x) ((x) == &NIL)
#define charp(x) ((x)->type == SCHEME_CHAR)
//#define string_c_string symbol_string_value
char *string_c_string (struct lisp_type *t);
#define symbol_equalp(x, y) \
  (strcmp (x->v.string.str, y->v.string.str) == 0)
#define symbol_string_value_equals(sym, c_string)			\
  (symbolp (sym) && (strcmp (c_string, sym->v.string.str) == 0))

struct lisp_type *
env_frame_insert_var (struct lisp_type *frame,
		      struct lisp_type *name_symbol,
		      struct lisp_type *value);
struct lisp_type *
env_frame_find_var (struct lisp_type *frame,
		    struct lisp_type *name_symbol);

struct lisp_type *
env_frame_set_var_value (struct lisp_type *frame,
			 struct lisp_type *name_symbol,
			 struct lisp_type *val);

struct lisp_type *
env_find_var (struct lisp_type *environ,
	      struct lisp_type *name_symbol);

struct lisp_type *
env_set_var (struct lisp_type *environ,
	     struct lisp_type *name_symbol,
	     struct lisp_type *value);

#define add_env_proc(str, sym)						\
  do {									\
    struct lisp_type *strval = make_symbol (str, false);		\
    struct lisp_type *prim = make_primitive_procedure (sym);		\
    PUSH_STACK (eval_rval_stack, prim);					\
    PUSH_STACK (eval_rval_stack, strval);				\
    env_frame_set_var_value (frame, strval, prim);		\
  } while (0)


#define add_env_val(str, expr)					\
  do {								\
    struct lisp_type *strval = make_symbol (str, false);		\
    env_frame_set_var_value (frame, strval, expr);		\
    PUSH_STACK (eval_rval_stack, strval);				\
  } while (0)


//read/write.
void
write0 (struct lisp_type *t);

struct lisp_type *
read0 (struct port *fp);

struct lisp_type *
read1 (struct port *fp);

void
write1 (struct lisp_type *t);

bool
is_immutable (struct lisp_type *it);

struct lisp_type *
list_seq_manip (struct lisp_type *form_orig,
		struct lisp_type *form_now,
		char *sequence,
		int sequence_it,
		char *errorstr);

struct lisp_type *
make_symbol(char *str, bool shouldfree);

struct lisp_type *
make_char (int c);
struct lisp_type *
make_macro (struct lisp_type *formals,
	    struct lisp_type *body,
	    struct lisp_type *env);


struct lisp_type *
make_lambda (struct lisp_type *formals,
	     struct lisp_type *body,
	     struct lisp_type *env);

struct lisp_type *
make_primitive_procedure (struct lisp_type *(*proc)(struct lisp_type *));

struct lisp_type *
make_string (char *str, bool shouldfree);

struct lisp_type *
make_number(int num);

struct lisp_type *
car (struct lisp_type *pair);

struct lisp_type *
cdr (struct lisp_type *pair);

struct lisp_type *
make_cons(struct lisp_type *car,
	  struct lisp_type *cdr);

struct lisp_type *
make_env_frame (void);

struct lisp_type *
make_vector (enum lisp_types type,
	     struct lisp_type *items);

struct lisp_type *
make_prealloc_vector (enum lisp_types type,
		      unsigned nitems,
		      union scheme_value *mem);

struct lisp_type *
last_pair (struct lisp_type *list);

struct lisp_type *
set_cdr (struct lisp_type *pair1,
	 struct lisp_type *val);

struct lisp_type *
assq (struct lisp_type *symbol,
      struct lisp_type *assoclist);

struct lisp_type *
make_bt_entry (struct lisp_type *form,
	       struct lisp_type *formals,
	       struct lisp_type *args,
	       struct lisp_type *new_environ);

void
free_lisp_type (struct lisp_type *t);
void
gc (bool force);
struct lisp_type *eval (struct lisp_type *form, struct lisp_type *environ);


// Primitive interpreter functions.
struct lisp_type *
scheme_consp (struct lisp_type *argl);

struct lisp_type *
scheme_eq (struct lisp_type *argl);

struct lisp_type *
lisp_symb_equal (struct lisp_type *argl);

struct lisp_type *
lisp_load (struct lisp_type *argl);

struct lisp_type *
__lisp_load (char *file, struct lisp_type *environ);

struct lisp_type *
lists_append (struct lisp_type *l1,
	      struct lisp_type *l2);


struct lisp_type *
less_than (struct lisp_type *argl);

struct lisp_type *
add (struct lisp_type *argl);

struct lisp_type *
lisp_cdr (struct lisp_type *argl);

struct lisp_type *
lisp_car (struct lisp_type *argl);

struct lisp_type *
lisp_cons (struct lisp_type *argl);

struct lisp_type *
lisp_assert (struct lisp_type *argl);

struct lisp_type *
sub (struct lisp_type *argl);

struct lisp_type *
mul (struct lisp_type *argl);

struct lisp_type *
list_func (struct lisp_type *argl);

struct lisp_type *
scheme_cdr (struct lisp_type *argl);

struct lisp_type *
scheme_car (struct lisp_type *argl);

struct lisp_type *
lisp_int_equal (struct lisp_type *argl);

struct lisp_type *
scheme_not (struct lisp_type *argl);

struct lisp_type *
scheme_open (struct lisp_type *argl);
struct lisp_type *
scheme_sys_read (struct lisp_type *argl);

struct lisp_type *
scheme_close (struct lisp_type *argl);

struct lisp_type *
scheme_vector_ref (struct lisp_type *argl);
struct lisp_type *
scheme_vector_set (struct lisp_type *argl);

struct lisp_type *
scheme_write (struct lisp_type *argl);

struct lisp_type *
scheme_read (struct lisp_type *argl);

struct lisp_type *
eval_inner_apply (struct lisp_type *proc,
		  struct lisp_type *form,
		  struct lisp_type *environ,
		  struct lisp_type *eval_argl);

int
__list_length (unsigned accum, struct lisp_type *argl);
struct lisp_type *
repl (struct lisp_type *environ,
      char *prompt,
      struct port *inp);
struct lisp_type *
scheme_make_vector (struct lisp_type *argl);
#define MAX_STRING 2048
#endif
