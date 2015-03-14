/*
 * GNU m4 -- A simple macro processor
 * Copyright (C) 1989-1992 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 * This file contains the functions to evaluate integer expressions for
 * the "eval" macro.  It is a little, fairly self-contained module, with
 * its own scanner, and a recursive descent parser.  The only entry
 * point is evaluate ().
 */

#include "m4.h"

/*
 * Evaluates token types.
 */
typedef enum eval_token
  {
    ERROR,
    PLUS, MINUS,
    EXPONENT,
    TIMES, DIVIDE, MODULO,
    EQ, NOTEQ, GT, GTEQ, LS, LSEQ,
    NOT,
    LAND, LOR,
    AND, OR,
    LEFTP, RIGHTP,
    NUMBER, EOTEXT
  }
eval_token;

/*
 * Error types.
 */
typedef enum eval_error
  {
    NO_ERROR,
    MISSING_RIGHT,
    SYNTAX_ERROR,
    UNKNOWN_INPUT,
    DIVIDE_ZERO,
    MODULO_ZERO
  }
eval_error;

static eval_error logical_or_term ();
static eval_error logical_and_term ();
static eval_error or_term ();
static eval_error and_term ();
static eval_error not_term ();
static eval_error cmp_term ();
static eval_error add_term ();
static eval_error mult_term ();
static eval_error exp_term ();
static eval_error unary_term ();
static eval_error simple_term ();

/*
 * Lexical functions.
 */

/* Pointer to next character of input text.  */
static char *eval_text;

/* Value of eval_text, from before last call of eval_lex ().  This is so we
   can back up, if we have read too much.  */
static char *last_text;

static void
eval_init_lex (char *text)
{
  eval_text = text;
  last_text = NULL;
}

static void
eval_undo (void)
{
  eval_text = last_text;
}


/* VAL is numerical value, if any.  */

static eval_token
eval_lex (int *val)
{
  while (isspace (*eval_text))
    eval_text++;

  last_text = eval_text;

  if (*eval_text == '\0')
    return EOTEXT;

  if (isdigit (*eval_text))
    {
      char *digits, *tmp;
      int base;

      if (*eval_text == '0')
	{
	  if (*++eval_text == 'x' || *eval_text == 'X')
	    {
	      base = 16;
	      digits = "0123456789abcdef";
	      eval_text++;
	    }
	  else
	    {
	      base = 8;
	      digits = "01234567";
	    }
	}
      else
	{
	  base = 10;
	  digits = "0123456789";
	}
      (*val) = 0;
      while (*eval_text && (tmp = index (digits, *eval_text)) != NULL)
	{
	  (*val) = (*val) * base + (tmp - digits);
	  eval_text++;
	}
      return NUMBER;
    }

  switch (*eval_text++)
    {
    case '+':
      return PLUS;
    case '-':
      return MINUS;
    case '*':
      if (*eval_text == '*')
	{
	  eval_text++;
	  return EXPONENT;
	}
      else
	return TIMES;
    case '^':
      return EXPONENT;
    case '/':
      return DIVIDE;
    case '%':
      return MODULO;
    case '=':
      if (*eval_text == '=')
	eval_text++;
      return EQ;
    case '!':
      if (*eval_text == '=')
	{
	  eval_text++;
	  return NOTEQ;
	}
      else
	return NOT;
    case '>':
      if (*eval_text == '=')
	{
	  eval_text++;
	  return GTEQ;
	}
      else
	return GT;
    case '<':
      if (*eval_text == '=')
	{
	  eval_text++;
	  return LSEQ;
	}
      else
	return LS;
    case '&':
      if (*eval_text == '&')
	{
	  eval_text++;
	  return LAND;
	}
      else
	return AND;
    case '|':
      if (*eval_text == '|')
	{
	  eval_text++;
	  return LOR;
	}
      else
	return OR;
    case '(':
      return LEFTP;
    case ')':
      return RIGHTP;
    default:
      return ERROR;
    }
}

/*
 * Main entry point, called from "eval".
 */
boolean
evaluate (char *expr, int *val)
{
  eval_token et;
  eval_error err;

  eval_init_lex (expr);
  et = eval_lex (val);
  err = logical_or_term (et, val);

  switch (err)
    {
    case NO_ERROR:
      break;
    case MISSING_RIGHT:
      error ("bad expression in eval (missing right paren): %s", expr);
      break;
    case SYNTAX_ERROR:
      error ("bad expression in eval: %s", expr);
      break;
    case UNKNOWN_INPUT:
      error ("bad expression in eval (bad input): %s", expr);
      break;
    case DIVIDE_ZERO:
      error ("divide by zero in eval: %s", expr);
      break;
    case MODULO_ZERO:
      error ("modulo by zero in eval: %s", expr);
      break;
    default:
      internal_error ("Bad error code in evaluate ()");
      break;
    }

  return (boolean) (err != NO_ERROR);
}

/*
 * Recursive descent parser.
 */
static eval_error
logical_or_term (eval_token et, int *v1)
{
  int v2;
  eval_error er;

  if ((er = logical_and_term (et, v1)) != NO_ERROR)
    return er;

  while ((et = eval_lex (&v2)) == LOR)
    {
      et = eval_lex (&v2);
      if (et == ERROR)
	return UNKNOWN_INPUT;

      if ((er = logical_and_term (et, &v2)) != NO_ERROR)
	return er;

      *v1 = *v1 || v2;
    }
  if (et == ERROR)
    return UNKNOWN_INPUT;

  eval_undo ();
  return NO_ERROR;
}

static eval_error
logical_and_term (eval_token et, int *v1)
{
  int v2;
  eval_error er;

  if ((er = or_term (et, v1)) != NO_ERROR)
    return er;

  while ((et = eval_lex (&v2)) == LAND)
    {
      et = eval_lex (&v2);
      if (et == ERROR)
	return UNKNOWN_INPUT;

      if ((er = or_term (et, &v2)) != NO_ERROR)
	return er;

      *v1 = *v1 && v2;
    }
  if (et == ERROR)
    return UNKNOWN_INPUT;

  eval_undo ();
  return NO_ERROR;
}

static eval_error
or_term (eval_token et, int *v1)
{
  int v2;
  eval_error er;

  if ((er = and_term (et, v1)) != NO_ERROR)
    return er;

  while ((et = eval_lex (&v2)) == OR)
    {
      et = eval_lex (&v2);
      if (et == ERROR)
	return UNKNOWN_INPUT;

      if ((er = and_term (et, &v2)) != NO_ERROR)
	return er;

      *v1 = *v1 | v2;
    }
  if (et == ERROR)
    return UNKNOWN_INPUT;

  eval_undo ();
  return NO_ERROR;
}

static eval_error
and_term (eval_token et, int *v1)
{
  int v2;
  eval_error er;

  if ((er = not_term (et, v1)) != NO_ERROR)
    return er;

  while ((et = eval_lex (&v2)) == AND)
    {
      et = eval_lex (&v2);
      if (et == ERROR)
	return UNKNOWN_INPUT;

      if ((er = not_term (et, &v2)) != NO_ERROR)
	return er;

      *v1 = *v1 & v2;
    }
  if (et == ERROR)
    return UNKNOWN_INPUT;

  eval_undo ();
  return NO_ERROR;
}

static eval_error
not_term (eval_token et, int *v1)
{
  eval_error er;

  if (et == NOT)
    {
      et = eval_lex (v1);
      if (et == ERROR)
	return UNKNOWN_INPUT;

      if ((er = not_term (et, v1)) != NO_ERROR)
	return er;
      *v1 = !*v1;
    }
  else
    if ((er = cmp_term (et, v1)) != NO_ERROR)
      return er;

  return NO_ERROR;
}

static eval_error
cmp_term (eval_token et, int *v1)
{
  eval_token op;
  int v2;
  eval_error er;

  if ((er = add_term (et, v1)) != NO_ERROR)
    return er;

  while ((op = eval_lex (&v2)) == EQ || op == NOTEQ
	 || op == GT || op == GTEQ
	 || op == LS || op == LSEQ)
    {

      et = eval_lex (&v2);
      if (et == ERROR)
	return UNKNOWN_INPUT;

      if ((er = add_term (et, &v2)) != NO_ERROR)
	return er;

      switch (op)
	{
	case EQ:
	  *v1 = *v1 == v2;
	  break;
	case NOTEQ:
	  *v1 = *v1 != v2;
	  break;
	case GT:
	  *v1 = *v1 > v2;
	  break;
	case GTEQ:
	  *v1 = *v1 >= v2;
	  break;
	case LS:
	  *v1 = *v1 < v2;
	  break;
	case LSEQ:
	  *v1 = *v1 <= v2;
	  break;
	default:
	  internal_error ("Bad comparison operator in cmp_term ()");
	  break;
	}
    }
  if (op == ERROR)
    return UNKNOWN_INPUT;

  eval_undo ();
  return NO_ERROR;
}

static eval_error
add_term (eval_token et, int *v1)
{
  eval_token op;
  int v2;
  eval_error er;

  if ((er = mult_term (et, v1)) != NO_ERROR)
    return er;

  while ((op = eval_lex (&v2)) == PLUS || op == MINUS)
    {
      et = eval_lex (&v2);
      if (et == ERROR)
	return UNKNOWN_INPUT;

      if ((er = mult_term (et, &v2)) != NO_ERROR)
	return er;

      if (op == PLUS)
	*v1 = *v1 + v2;
      else
	*v1 = *v1 - v2;
    }
  if (op == ERROR)
    return UNKNOWN_INPUT;

  eval_undo ();
  return NO_ERROR;
}

static eval_error
mult_term (eval_token et, int *v1)
{
  eval_token op;
  int v2;
  eval_error er;

  if ((er = exp_term (et, v1)) != NO_ERROR)
    return er;

  while ((op = eval_lex (&v2)) == TIMES || op == DIVIDE || op == MODULO)
    {
      et = eval_lex (&v2);
      if (et == ERROR)
	return UNKNOWN_INPUT;

      if ((er = exp_term (et, &v2)) != NO_ERROR)
	return er;

      switch (op)
	{
	case TIMES:
	  *v1 = *v1 * v2;
	  break;
	case DIVIDE:
	  if (v2 == 0)
	    return DIVIDE_ZERO;
	  else
	    *v1 = *v1 / v2;
	  break;
	case MODULO:
	  if (v2 == 0)
	    return MODULO_ZERO;
	  else
	    *v1 = *v1 % v2;
	  break;
	default:
	  internal_error ("Bad operator in mult_term ()");
	  break;
	}
    }
  if (op == ERROR)
    return UNKNOWN_INPUT;

  eval_undo ();
  return NO_ERROR;
}

static eval_error
exp_term (eval_token et, int *v1)
{
  register int result;
  int v2;
  eval_error er;

  if ((er = unary_term (et, v1)) != NO_ERROR)
    return er;
  result = *v1;

  while ((et = eval_lex (&v2)) == EXPONENT)
    {
      et = eval_lex (&v2);
      if (et == ERROR)
	return UNKNOWN_INPUT;

      if ((er = exp_term (et, &v2)) != NO_ERROR)
	return er;

      result = 1;
      while (v2-- > 0)
	result *= *v1;
      *v1 = result;
    }
  if (et == ERROR)
    return UNKNOWN_INPUT;

  eval_undo ();
  return NO_ERROR;
}

static eval_error
unary_term (eval_token et, int *v1)
{
  eval_token et2 = et;
  eval_error er;

  if (et == PLUS || et == MINUS)
    {
      et2 = eval_lex (v1);
      if (et2 == ERROR)
	return UNKNOWN_INPUT;

      if ((er = simple_term (et2, v1)) != NO_ERROR)
	return er;

      if (et == MINUS)
	*v1 = -*v1;
    }
  else
    if ((er = simple_term (et, v1)) != NO_ERROR)
      return er;

  return NO_ERROR;
}

static eval_error
simple_term (eval_token et, int *v1)
{
  int v2;
  eval_error er;

  switch (et)
    {
    case LEFTP:
      et = eval_lex (v1);
      if (et == ERROR)
	return UNKNOWN_INPUT;

      if ((er = logical_or_term (et, v1)) != NO_ERROR)
	return er;

      et = eval_lex (&v2);
      if (et == ERROR)
	return UNKNOWN_INPUT;

      if (et != RIGHTP)
	return MISSING_RIGHT;

      break;
    case NUMBER:
      break;
    default:
      return SYNTAX_ERROR;
    }
  return NO_ERROR;
}
