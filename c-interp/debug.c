#include "scheme.h"

void
debugger (struct lisp_type *environ,
	  struct lisp_type *form);


#define MAX_ERRORLEN 1024
void
scheme_signal_eval_error (char *msg, ...)
{
  char str[MAX_ERRORLEN] = {0};
  va_list args;
  va_start(args, msg);
  vsnprintf(str, MAX_ERRORLEN, msg, args);
  fprintf (stderr, "%s\n", str);
  va_end(args);
  debugger (environ_global, form_global);
}



struct lisp_type *
make_bt_entry (struct lisp_type *form,
	       struct lisp_type *formals,
	       struct lisp_type *args,
	       struct lisp_type *new_environ) {
  return make_cons (form,
		    make_cons (formals,
			       make_cons (args,
					  make_cons (new_environ,
						     NIL_VALUE))));
}

void
print_bt_entry (struct lisp_type *bt_ent) {
  printf ("\nForm: ");
  write0 (car (bt_ent));
  printf ("\nFormals: ");
  write0 (car (cdr (bt_ent)));
  printf ("\nArgs: ");
  write0 (car (cdr (cdr (bt_ent))));
  printf("\n");
}


struct lisp_type *
show_backtrace (struct lisp_type *argl, struct lisp_type *env)
{
  for (int i = backtrace->index-1; i >= 0; --i) {
    print_bt_entry (backtrace->items[i]);
  }

  return NIL_VALUE;
}

struct lisp_type *
leave_debug (struct lisp_type *argl, struct lisp_type *env)
{
  longjmp (*jmpbuffer, DEBUG_RESTART);
}

struct lisp_type *
init_debug_environ (struct lisp_type *base)
{
  struct lisp_type *frame = make_env_frame ();
  struct lisp_type *env = make_cons (frame, base);
  
  add_env_proc ("backtrace", show_backtrace);
  add_env_proc ("leave", leave_debug);
  return env;
}

void
debugger (struct lisp_type *environ,
	  struct lisp_type *form)
{
  jmp_buf debugbuf_stack;
  printf (";; Error evaluating: ");
  if (form)
    {
      write1 (form);
    }
  else
    printf (" (unknown)" );
  struct lisp_type *env_dbg, *top_env_dbg;
  env_dbg = top_env_dbg = init_debug_environ (environ);
  debugbuf = &debugbuf_stack;
  int cur_stack_item = backtrace->index;
  int rval = 0;
  if ((rval = setjmp (debugbuf_stack)) != 0)
    switch (rval)
      {
      case DEBUG_UP:
	if (cur_stack_item > 0)
	  {
	    env_dbg = car (
		cdr (cdr (cdr (backtrace->items[--cur_stack_item]))));
	    fprintf (stderr, ";; Stack item %d\n", cur_stack_item);
	    break;
	  }
	else
	  fprintf (stderr, ";; No more stack items\n");
	break;
      case DEBUG_DOWN:
	if (cur_stack_item < backtrace->index)
	  {
	    env_dbg = car (
		cdr (cdr (cdr (backtrace->items[++cur_stack_item]))));
	    fprintf (stderr, ";; Stack item %d\n", cur_stack_item);
	  }
	else
	  fprintf (stderr, ";; Bottom of stack\n");
	break;
      case DEBUG_LEAVE:
	{
	  debugbuf = NULL;
	  leave_debug(NIL_VALUE, environ);
	  break;
	}
      default:
	abort();
      }
  repl (env_dbg, "(Debug)", &STDIN_CPORT_READER);
  leave_debug(NIL_VALUE, environ);
}


struct lisp_type *
scheme_dbg_up (struct lisp_type *argl, struct lisp_type *env)
{
  if (debugbuf)
    {
      POP_STACK (backtrace);
      longjmp (*debugbuf, DEBUG_UP);
    }
  else
    return NIL_VALUE;
}

struct lisp_type *
scheme_dbg_down (struct lisp_type *argl, struct lisp_type *env)
{
  if (debugbuf)
    {
      POP_STACK (backtrace); // eliminate ourselves from the bt.
      longjmp (*debugbuf, DEBUG_DOWN);
    }
  else
    return NIL_VALUE;
}
