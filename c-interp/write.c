#include "scheme.h"

void write_string (struct lisp_type *t)
{
  int nchars = t->v.vec.nitems;
  putc('"', stdout);
  for (int i = 0; i < nchars; ++i)
    {
      if (vector_unimem(t)[i].intval == '\n')
	{
	  putc ('\\', stdout);
	  putc ('n', stdout);
	}
      else if (vector_unimem(t)[i].intval == '"')
	{
	  putc ('\\', stdout);
	  putc ('"', stdout);
	}
      else
	putc(t->v.vec.mem[i].intval, stdout);
    }
  putc('"', stdout);
}

void write_vec (struct lisp_type *t)
{
  if (stringp (t))
    {
      write_string (t);
      return;
    }
  else if (mixed_vectorp (t))
    {
      printf ("#(");
      for (int i = 0; i < vector_len(t); ++i) {
	write0 (vector_mixedmem(t)[i]);
	if (i != vector_len(t)-1)
	  putc (' ', stdout);
      }
      printf(")");
    }
  else
    {
      printf("#(");
      enum lisp_types typeval = t->v.vec.type_flags;
      struct lisp_type dummy_val = {
	.type_flags = typeval
      };
      for (int i = 0; i < t->v.vec.nitems; ++i) {
	dummy_val.v = t->v.vec.mem[i];
	write0 (&dummy_val);
	if (i+1 != t->v.vec.nitems)
	  printf(" ");
      }
      printf(")");
    }
}

void
write_list (struct lisp_type *t, bool space)
{
  if (get_type_enum (t) == NIL_TYPE)
    printf (")");
  else if (!consp (t))
    {
      printf (" . ");
      write0 (t);
      printf(")");
    }
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
  switch (get_type_enum (t))
    {
    case NUMBER:
      printf ("%d", t->v.intval);
      break;
    case SCHEME_VECTOR:
    case SCHEME_VECTOR_MIXED: 
      write_vec (t);
      break;
    case NIL_TYPE:
      printf ("nil");
      break;
    case SYMBOL:
      printf ("%s", symbol_string_value (t));
      break;
    case SCHEME_CHAR:
      printf ("?%c", number_value (t));
      break;
    case PAIR:
      printf ("(");
      write_list (t, false);
      break;
    /* case STRING: */
    /*   printf("\"%s\"", string_c_string (t)); */
    /*   break; */
    case SCHEME_PROC:
      printf("#[scheme procedure]");
      break;
    case MACRO:
      printf("#[macro]");
      break;
    case PRIMITIVE_PROC:
      printf("#[primitive procedure]");
      break;
    case DOT:
      printf("|.|");
      break;
    case SCHEME_EOF:
      printf("#eof");
      break;
    case BOOLEAN: {
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
    }
    default:
      fprintf (stderr, "can't write this type: %d!\n", get_type_enum (t));
      //exit (1);
    }
}

void write1 (struct lisp_type *t)
{
  write0 (t);
  printf("\n");
}
