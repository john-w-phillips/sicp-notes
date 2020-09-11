#include "scheme.h"

struct lisp_type *
read_list (struct port *fp)
{
  //int c = cport_getbyte (stdin);
  struct lisp_type *h = read0 (fp);
  PUSH_STACK (formstack, h);
  if (h == DOT_VALUE)
    {
      struct lisp_type *tail = read0 (fp);
      PUSH_STACK (formstack, tail);
      struct lisp_type *end = read0 (fp);
      if (end)
	{
	  fprintf (stderr, "Bad lisp form\n");
	  longjmp (*jmpbuffer, READER_INVALID_SYNTAX);
	}
      else
	{
	  POP_STACK (formstack);
	  POP_STACK (formstack);
	  return tail;
	}
    }
  else if (h)
    {
      struct lisp_type *rval = make_cons (h, read_list (fp));
      POP_STACK (formstack);
      return rval;
    }
  else
    {
      POP_STACK (formstack);
      return (struct lisp_type *)&NIL;
    }
}

#define NUMSIZ 128

struct lisp_type *
read_num (int c, struct port *fp)
{
  char numbuf[NUMSIZ] = {c}, *endptr = NULL;;
  int idx = 1;
  while ( isdigit (c = cport_getbyte (fp)) )
    {
      numbuf[idx++] = c;
    }
  cport_ungetbyte (c, fp);
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

bool
isspecial (int c)
{
  return (c == '(' || c == ')' || isspace (c) || c == '.');
}

struct lisp_type *
read_symbol (int c, struct port *fp)
{
  char symbuf[SYMBSIZE] = {c};
  int index = 1;

  while ( !isspecial (c = cport_getbyte (fp)) )
    {
      symbuf[index++] = c;
    }
  symbuf[index] = '\0';
  cport_ungetbyte (c, fp);
  return make_symbol (strdup (symbuf), true);
}

int
read_escaped (struct port *fp)
{
  int c = cport_getbyte (fp);
  switch (c)
    {
    case 'n':
      return '\n';
    default:
      return c;
    }
}
struct lisp_type *
read_string (int c, struct port *fp)
{
  int index = 0;
  union scheme_value strbuf[MAX_STRING] = {0};
  while ( ((c = cport_getbyte (fp)) != '"') && (index < MAX_STRING))
    {
      if (c == '\\')
	{
	  strbuf[index++].intval = read_escaped (fp);
	}
      else
	strbuf[index++].intval = c;
    }

  union scheme_value *mem = calloc (index, sizeof(union scheme_value));
  memcpy (mem, strbuf, sizeof(union scheme_value) * index);
  return make_prealloc_vector (SCHEME_CHAR,
			       index,
			       mem);
}

struct lisp_type *read_quoted (struct port *fp)
{
  return make_cons (QUOTE_VALUE,
		    make_cons (read0 (fp), NIL_VALUE));
}

struct lisp_type *read_quasiquote (struct port *fp)
{
  return make_cons (QUASIQUOTE_VALUE,
		    make_cons (read0 (fp), NIL_VALUE));
	
}

struct lisp_type *read_comma (struct port *fp)
{
  int c = 0;

  if ((c = cport_getbyte (fp)) == '@')
    {
      return make_cons (UNQUOTE_SPLICE_VALUE,
			make_cons (read0 (fp),
				   NIL_VALUE));
    }
  else
    {
      cport_ungetbyte (c, fp);
      return make_cons (UNQUOTE_VALUE,
			make_cons (read0 (fp), NIL_VALUE));
    }
}

void
read_comment (struct port *fp)
{
  int c = 0;
  while ( (c = cport_getbyte (fp)) != '\n') {}
}
struct lisp_type *
read_char (struct port *fp)
{
  int c = cport_getbyte (fp);
  return make_char (c);
}

struct lisp_type *
read0 (struct port *fp)
{
  int c = cport_getbyte (fp);
  if (c == '(')
    return read_list (fp);
  else if (isdigit (c))
    return read_num (c, fp);
  else if (c == ';')
    {
      read_comment (fp);
      return read0 (fp);
    }
  else if (c == '.')
    return DOT_VALUE;
  else if (c == '"')
    return read_string (c, fp);
  else if (c == ',')
    return read_comma (fp);
  else if (c == '?')
    return read_char (fp);
  else if (c == '\'')
    return read_quoted (fp);
  else if (c == '`')
    return read_quasiquote (fp);
  else if (isspace (c))
    return read0 (fp);
  else if ((isalpha (c) || isgraph (c)) && !isspecial(c))
    return read_symbol (c, fp);
  else if (c == ')')
    return NULL;
  else if (c == EOF)
    return EOF_VALUE;
  else
    {
      fprintf(stderr, "Unspecified reader error! got c: %c\n",
	      c);
      exit(1);
    }
  return NULL;
}

struct lisp_type *
read1 (struct port *fp)
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

