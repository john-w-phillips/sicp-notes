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
show_backtrace (struct lisp_type *argl)
{
  for (int i = backtrace->index-1; i >= 0; --i) {
    print_bt_entry (backtrace->items[i]);
  }

  return NIL_VALUE;
}

struct lisp_type *
leave_debug (struct lisp_type *argl)
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
  printf (";; Error evaluating: ");
  if (form)
    {
      write1 (form);
    }
  else
    printf (" (unknown)" );
  struct lisp_type *env_dbg = init_debug_environ (environ);
  repl (env_dbg, "(Debug)", stdin);
  leave_debug (NIL_VALUE);
}
