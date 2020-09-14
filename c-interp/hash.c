#include "scheme.h"
#include <stdio.h>
#define ALPHABET_START ' '
#define ALPHABET_STOP '~'
#define ALPHABET_SIZE (ALPHABET_START - ALPHABET_STOP + 1)
#define char(x) ((x) - ALPHABET_START)

unsigned
hash_string(char *string, unsigned m)
{
  unsigned long hash = 0;
  for (int i = 0; ;++i)
    {
      if (string[i] == 0)
	break;
      else
	{
	  hash =  string[i] + (
	      ALPHABET_SIZE * (hash)) % m;
	      
	}
    }
  return hash % m;
}
struct lisp_type **symbol_table = NULL;
unsigned symbol_table_size = 0;

void
init_symbol_table (unsigned size)
{
  symbol_table = calloc (size, sizeof(struct lisp_type *));
  for (int i = 0; i < size; ++i)
    symbol_table[i] = NIL_VALUE;
  symbol_table_size = size;
}

void
unintern_all (void)
{
  for (int i = 0; i < symbol_table_size; ++i)
    symbol_table[i] = NIL_VALUE;
}

void
destroy_symbol_table (void)
{
  free (symbol_table);
  symbol_table = NULL;
}
struct lisp_type *
symbol_lookup (unsigned hash, char *str)
{
  struct lisp_type *head = symbol_table[hash];
  for (; head != NIL_VALUE; head = cdr (head))
    {
      if (symbol_string_value_equals (car (head), str))
	return car (head);
    }
  return NULL;
}

struct lisp_type *
intern_symb (char *str, bool free_mem)
{
  assert (symbol_table_size);
  unsigned hash_val = hash_string (str,
				   symbol_table_size);
  struct lisp_type *symb = NULL;
  if ((symb = symbol_lookup (hash_val, str)))
    {
      if (free_mem)
	free (str);
      return symb;
    }
  else
    {
      symb = make_symbol (str, free_mem);
      symbol_table[hash_val] = make_cons (
	  symb,
	  symbol_table[hash_val]);
      return symb;
    }
}

