#include "scheme.h"

struct lisp_type *
read_list (FILE *fp)
{
  //int c = getc (stdin);
  struct lisp_type *h = read0 (fp);
  if (h == DOT_VALUE)
    {
      struct lisp_type *tail = read0 (fp);
      struct lisp_type *end = read0 (fp);
      if (end)
	{
	  fprintf (stderr, "Bad lisp form\n");
	  longjmp (*jmpbuffer, READER_INVALID_SYNTAX);
	}
      else
	return tail;
    }
  else if (h)
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

#define MAX_STRING 1024

struct lisp_type *
read_string (int c, FILE *fp)
{
  int index = 0;
  union scheme_value strbuf[MAX_STRING] = {0};
  while ( ((c = getc (fp)) != '"') && (index < MAX_STRING))
    {
      strbuf[index++].intval = c;
    }

  union scheme_value *mem = calloc (index, sizeof(union scheme_value));
  memcpy (mem, strbuf, sizeof(union scheme_value) * index);
  return make_prealloc_vector (SCHEME_CHAR,
			       index,
			       mem);
}

struct lisp_type *read_quoted (FILE *fp)
{
  return make_cons (QUOTE_VALUE,
		    make_cons (read0 (fp), NIL_VALUE));
}

struct lisp_type *read_quasiquote (FILE *fp)
{
  return make_cons (QUASIQUOTE_VALUE,
		    make_cons (read0 (fp), NIL_VALUE));
	
}

struct lisp_type *read_comma (FILE *fp)
{
  int c = 0;

  if ((c = getc (fp)) == '@')
    {
      return make_cons (UNQUOTE_SPLICE_VALUE,
			make_cons (read0 (fp),
				   NIL_VALUE));
    }
  else
    {
      ungetc (c, fp);
      return make_cons (UNQUOTE_VALUE,
			make_cons (read0 (fp), NIL_VALUE));
    }
}

void
read_comment (FILE *fp)
{
  int c = 0;
  while ( (c = getc (fp)) != '\n') {}
}
struct lisp_type *
read_char (FILE *fp)
{
  int c = getc (fp);
  return make_char (c);
}

struct lisp_type *
read0 (FILE *fp)
{
  int c = getc (fp);
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
      fprintf(stderr, "Unspecified reader error!\n");
      exit(1);
    }
  return NULL;
}

struct lisp_type *
read1 (FILE *fp)
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

