/*	Copyright (C) 1990, 1992, 1993 Free Software Foundation, Inc.

This file is part of Oleo, the GNU Spreadsheet.

Oleo is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Oleo is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Oleo; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "funcdef.h"
#include "sysdef.h"
#include "global.h"
#include "eval.h"
#include "cell.h"
#include "io-utils.h"



struct pr_node
  {
    int tightness;
    int len;
    char string[1];
  };


static VOIDSTAR save_decomp;
static CELLREF decomp_row;
static CELLREF decomp_col;

/* These #defines are so that we don't have to duplicate code below */
/* JF: now obsolete, and should be trashed. */

#define F0	a0?"@%s()":"%s()"
#define F1	a0?"@%s(%s)":"%s(%s)"
#define F2	a0?"@%s(%s, %s)":"%s(%s, %s)"
#define F3	a0?"@%s(%s, %s, %s)":"%s(%s, %s, %s)"
#define F4	a0?"@%s(%s, %s, %s, %s)":"%s(%s, %s, %s, %s)"
#define FN1	a0?"@%s(%s":"%s(%s"

/* We decompile things with these wierd node-things.  It's ugly, but it works.
 */
#ifdef __STDC__
static struct pr_node *
n_alloc (int size, int tightness, char *fmt,...)
#else
static struct pr_node *
n_alloc (size, tightness, fmt, va_alist)
     int size;
     int tightness;
     char *fmt;
     va_dcl
#endif
{
  struct pr_node *ret;
  va_list args;

  ret = ck_malloc (sizeof (struct pr_node) + size + 1);
  ret->len = size;
  ret->tightness = tightness;
  var_start (args, fmt);
  vsprintf (ret->string, fmt, args);
  va_end (args);
  return ret;
}

#define n_free(x)	free(x)

static struct pr_node *
byte_decompile (expr)
     unsigned char *expr;
{
  unsigned char byte;
  double tmp_flt;
  long tmp_lng;
  char *tmp_str;
  struct var *v;
  unsigned long_skp;
  unsigned char save_val;
  unsigned jumpto;

  struct pr_node *new = 0;
  int pri;
  int aso;
  char *chr;

  static struct pr_node **the_line;
  static int line_alloc;

  static struct pr_node **c_node;
  struct function *f;

  if (!the_line)
    {
      the_line = (struct pr_node **) ck_malloc (20 * sizeof (struct pr_node *));
      line_alloc = 20;
      c_node = the_line;
    }

#ifdef TEST
  if (!expr)
    panic ("No expression to decompile");
#endif
next_byte:
  byte = *expr++;
  if (byte < USR1)
    f = &the_funs[byte];
  else if (byte < SKIP)
    {
      tmp_lng = *expr++;
      f = &usr_funs[byte - USR1][tmp_lng];
    }
  else
    f = &skip_funs[byte - SKIP];

  if (f->fn_argn & X_J)
    jumpto = *expr++;
  else if (f->fn_argn & X_JL)
    {
      jumpto = expr[0] + ((unsigned) (expr[1]) << 8);
      expr += 2;
    }
#ifdef TEST
  else
    jumpto = 0;
#endif
  switch (GET_COMP (f->fn_comptype))
    {
    case C_IF:
      if (expr[jumpto - 2] != SKIP)
	{
	  long_skp = 1;
	  save_val = expr[jumpto - 3];
	  expr[jumpto - 3] = 0;
	}
      else
	{
	  long_skp = 0;
	  save_val = expr[jumpto - 2];
	  expr[jumpto - 2] = 0;
	}
      c_node[0] = byte_decompile (expr);
      c_node++;

      if (long_skp)
	{
	  expr[jumpto - 3] = save_val;
	  expr += jumpto;
	  jumpto = expr[-2] + ((unsigned) (expr[-1]) << 8);
	}
      else
	{
	  expr[jumpto - 2] = save_val;
	  expr += jumpto;
	  jumpto = expr[-1];
	}
      save_val = expr[jumpto];
      expr[jumpto] = 0;
      c_node[0] = byte_decompile (expr);
      c_node -= 2;
      expr[jumpto] = save_val;
      expr += jumpto;
      if (byte == IF || byte == IF_L)
	{
	  if (c_node[0]->tightness <= 1)
	    new = n_alloc (8 + c_node[0]->len + c_node[1]->len + c_node[2]->len,
			   1,
			   "(%s) ? %s : %s", c_node[0]->string, c_node[1]->string, c_node[2]->string);
	  else
	    new = n_alloc (6 + c_node[0]->len + c_node[1]->len + c_node[2]->len,
			   1,
			   "%s ? %s : %s", c_node[0]->string, c_node[1]->string, c_node[2]->string);
	}
      else
	new = n_alloc (6 + c_node[0]->len + c_node[1]->len + c_node[2]->len + strlen (f->fn_str),
		       1000,
		       F3, f->fn_str, c_node[0]->string, c_node[1]->string, c_node[2]->string);
      n_free (c_node[0]);
      n_free (c_node[1]);
      n_free (c_node[2]);
      break;

    case C_ANDOR:
      save_val = expr[jumpto];
      expr[jumpto] = 0;
      c_node[0] = byte_decompile (expr);
      expr[jumpto] = save_val;
      expr += jumpto;
      c_node++;
      if (ISINFIX (f->fn_comptype))
	{
	  pri = GET_IN (f->fn_comptype);
	  aso = GET_ASO (f->fn_comptype);
	  chr = f->fn_str;
	  goto do_infix;
	}
      else
	goto do_fn2;

    case C_STR:
      tmp_str = backslash_a_string ((char *) expr + jumpto, 1);
      new = n_alloc (strlen (tmp_str) + 1,
		     1000,
		     tmp_str);
      break;

    case C_CELL:
      {
	int num1, num2;
	char *str;
	CELLREF row, col;

	row = GET_ROW (expr);
	col = GET_COL (expr);
	expr += EXP_ADD;

	if (a0)
	  {
	    new = n_alloc (30, 1000, f->fn_str, col_to_str (col), row);
	  }
	else
	  {
	    if (byte & ROWREL)
	      {
		num1 = row - decomp_row;
		if (byte & COLREL)
		  {
		    num2 = col - decomp_col;
		    if (row == decomp_row && col == decomp_col)
		      str = "rc";
		    else if (row == decomp_row)
		      {
			str = "rc[%+d]";
			num1 = num2;
		      }
		    else if (col == decomp_col)
		      str = "r[%+d]c";
		    else
		      str = "r[%+d]c[%+d]";
		  }
		else if (row == decomp_row)
		  {
		    str = "rc%u";
		    num1 = num2 = col;
		  }
		else
		  {
		    str = "r[%+d]c%u";
		    num2 = col;
		  }
	      }
	    else if (byte & COLREL)
	      {
		num1 = row;
		num2 = col - decomp_col;
		if (col == decomp_col)
		  str = "r%uc";
		else
		  str = "r%uc[%+d]";
	      }
	    else
	      {
		str = "r%uc%u";
		num1 = row;
		num2 = col;
	      }
	    new = n_alloc (30, 1000, str, num1, num2);
	  }
	new->len = strlen (new->string);
      }
      break;

    case C_RANGE:
      {
	char tmprbuf[40];
	char tmpcbuf[40];
	struct rng rng;

	GET_RNG (expr, &rng);
	expr += EXP_ADD_RNG;

	if (a0)
	  new = n_alloc (40, 1000, f->fn_str, col_to_str (rng.lc), rng.lr, col_to_str (rng.hc), rng.hr);
	else
	  {
	    /* Check for special cases */
	    if (rng.lr == rng.hr && ((byte & LRREL) ? 1 : 0) == ((byte & HRREL) ? 1 : 0))
	      {
		if (byte & LRREL)
		  {
		    if (rng.lr == decomp_row)
		      {
			tmprbuf[0] = 'r';
			tmprbuf[1] = '\0';
		      }
		    else
		      (void) sprintf (tmprbuf, "r[%+d]", rng.lr - decomp_row);
		  }
		else
		  sprintf (tmprbuf, "r%u", rng.lr);
	      }
	    else if ((byte & LRREL) && (byte & HRREL))
	      {
		int r1, r2, rtmp;

		r1 = rng.lr - decomp_row;
		r2 = rng.hr - decomp_row;
		if (r1 < r2)
		  rtmp = r1, r1 = r2, r2 = rtmp;
		(void) sprintf (tmprbuf, "r[%+d:%+d]", r1, r2);
	      }
	    else if ((byte & LRREL))
	      (void) sprintf (tmprbuf, "r[%+d]:%u", rng.lr - decomp_row, rng.hr);
	    else if (byte & HRREL)
	      (void) sprintf (tmprbuf, "r%u:[%+d]", rng.lr, rng.hr - decomp_row);
	    else if (rng.lr < rng.hr)
	      (void) sprintf (tmprbuf, "r%u:%u", rng.lr, rng.hr);
	    else
	      (void) sprintf (tmprbuf, "r%u:%u", rng.hr, rng.lr);

	    if (rng.lc == rng.hc && ((byte & LCREL) ? 1 : 0) == ((byte & HCREL) ? 1 : 0))
	      {
		if (byte & LCREL)
		  {
		    if (rng.lc == decomp_col)
		      {
			tmpcbuf[0] = 'c';
			tmpcbuf[1] = '\0';
		      }
		    else
		      sprintf (tmpcbuf, "c[%+d]", rng.lc - decomp_col);
		  }
		else
		  sprintf (tmpcbuf, "c%u", rng.lc);
	      }
	    else if ((byte & LCREL) && (byte & HCREL))
	      {
		int c1, c2, ctmp;

		c1 = rng.lc - decomp_col;
		c2 = rng.hc - decomp_col;
		if (c1 < c2)
		  ctmp = c1, c1 = c2, c2 = ctmp;
		(void) sprintf (tmpcbuf, "c[%+d:%+d]", c1, c2);
	      }
	    else if ((byte & LCREL))
	      (void) sprintf (tmpcbuf, "c[%+d]:%u", rng.lc - decomp_col, rng.hc);
	    else if (byte & HCREL)
	      (void) sprintf (tmpcbuf, "c%u:[%+d]", rng.lc, rng.hc - decomp_col);
	    else if (rng.lc < rng.hc)
	      (void) sprintf (tmpcbuf, "c%u:%u", rng.lc, rng.hc);
	    else
	      (void) sprintf (tmpcbuf, "c%u:%u", rng.hc, rng.lc);

	    new = n_alloc (40, 1000, "%s%s", tmprbuf, tmpcbuf);
	  }
	new->len = strlen (new->string);
      }
      break;

    case C_CONST:
      new = n_alloc (strlen (f->fn_str) + 1, 1000, f->fn_str);
      break;

    case C_FN0:
    case C_FN0X:
    case C_FN0 | C_T:
      new = n_alloc (strlen (f->fn_str) + 3,
		     1000,
		     F0,
		     f->fn_str);
      break;

    case C_FN1:
      --c_node;
      new = n_alloc (c_node[0]->len + strlen (f->fn_str) + 3,
		     1000,
		     F1, f->fn_str, c_node[0]->string);
      n_free (*c_node);
      break;

    case C_UNA:
      --c_node;
      if (c_node[0]->tightness < 9)
	{
	  new = n_alloc (3 + c_node[0]->len,
			 9,
			 "%s(%s)", f->fn_str, c_node[0]->string);
	}
      else
	{
	  new = n_alloc (1 + c_node[0]->len,
			 9,
			 "%s%s", f->fn_str, c_node[0]->string);
	}
      n_free (*c_node);
      break;

    case C_INF:
      pri = GET_IN (f->fn_comptype);
      aso = GET_ASO (f->fn_comptype);
      chr = f->fn_str;

    do_infix:
      c_node -= 2;
      if (c_node[0]->tightness < pri || (c_node[0]->tightness == pri && aso != 1))
	{
	  if (c_node[1]->tightness < pri || (c_node[1]->tightness == pri && aso != -1))
	    new = n_alloc (7 + c_node[0]->len + c_node[1]->len,
			   pri,
		 "(%s) %s (%s)", c_node[0]->string, chr, c_node[1]->string);
	  else
	    new = n_alloc (5 + c_node[0]->len + c_node[1]->len,
			   pri,
		   "(%s) %s %s", c_node[0]->string, chr, c_node[1]->string);
	}
      else if (c_node[1]->tightness < pri || (c_node[1]->tightness == pri && aso != -1))
	new = n_alloc (5 + c_node[0]->len + c_node[1]->len,
		       pri,
		   "%s %s (%s)", c_node[0]->string, chr, c_node[1]->string);
      else
	new = n_alloc (3 + c_node[0]->len + c_node[1]->len,
		       pri,
		     "%s %s %s", c_node[0]->string, chr, c_node[1]->string);

      n_free (c_node[0]);
      n_free (c_node[1]);
      break;

    case C_FN2:
    do_fn2:
      c_node -= 2;
      new = n_alloc (c_node[0]->len + c_node[1]->len + strlen (f->fn_str) + 5,
		     1000,
		     F2, f->fn_str, c_node[0]->string, c_node[1]->string);
      n_free (c_node[0]);
      n_free (c_node[1]);
      break;

    case C_FN3:
      c_node -= 3;
      new = n_alloc (c_node[0]->len + c_node[1]->len + c_node[2]->len + strlen (f->fn_str) + 7,
		     1000,
		     F3,
		     f->fn_str,
		     c_node[0]->string,
		     c_node[1]->string,
		     c_node[2]->string);
      n_free (c_node[0]);
      n_free (c_node[1]);
      n_free (c_node[2]);
      break;

    case C_FNN:
      aso = *expr++;
      c_node -= aso;

      if (aso == 1)
	new = n_alloc (3 + c_node[0]->len + strlen (f->fn_str),
		       1000,
		       F1, f->fn_str, c_node[0]->string);
      else
	{
	  new = n_alloc (2 + c_node[0]->len + strlen (f->fn_str),
			 1000,
			 FN1, f->fn_str, c_node[0]->string);
	  --aso;
	  for (pri = 1; pri < aso; pri++)
	    {
	      n_free (c_node[0]);
	      c_node[0] = new;
	      new = n_alloc (2 + new->len + c_node[pri]->len,
			     1000,
			     "%s, %s", new->string, c_node[pri]->string);
	    }
	  n_free (c_node[0]);
	  c_node[0] = new;
	  new = n_alloc (3 + new->len + c_node[aso]->len,
			 1000,
			 "%s, %s)", new->string, c_node[aso]->string);
	}
      n_free (c_node[0]);
      break;

    case C_FN4:
      c_node -= 4;
      new = n_alloc (c_node[0]->len + c_node[1]->len + c_node[2]->len + c_node[3]->len + strlen (f->fn_str) + 6,
		     1000,
		     F4,
		     f->fn_str,
		     c_node[0]->string,
		     c_node[1]->string,
		     c_node[2]->string,
		     c_node[3]->string);
      n_free (c_node[0]);
      n_free (c_node[1]);
      n_free (c_node[2]);
      n_free (c_node[3]);
      break;

    case C_ERR:
      tmp_str = (char *) expr + jumpto;
      expr++;
      new = n_alloc (strlen (tmp_str) + 1, 1000, tmp_str);
      /* bcopy((VOIDSTAR)expr,(VOIDSTAR)&tmp_str,sizeof(char *));
		expr+=sizeof(char *);
		new=n_alloc(strlen(tmp_str)+1,1000,f->fn_str,tmp_str); */
      break;

    case C_FLT:
      bcopy ((VOIDSTAR) expr, (VOIDSTAR) & tmp_flt, sizeof (double));
      expr += sizeof (double);
      new = n_alloc (20, 1000, f->fn_str, tmp_flt);
      new->len = strlen (new->string);
      break;

    case C_INT:
      bcopy ((VOIDSTAR) expr, (VOIDSTAR) & tmp_lng, sizeof (long));
      expr += sizeof (long);
      new = n_alloc (20, 1000, f->fn_str, tmp_lng);
      new->len = strlen (new->string);
      break;

    case C_VAR:
      bcopy ((VOIDSTAR) expr, (VOIDSTAR) & v, sizeof (struct var *));
      expr += sizeof (struct var *);
      new = n_alloc (strlen (v->var_name) + 1,
		     1000,
		     f->fn_str, v->var_name);
      break;


    default:
      panic ("Bad decompile %d", f->fn_comptype);
    }
  *c_node++ = new;
  if (c_node == &the_line[line_alloc])
    {
      line_alloc *= 2;
      the_line = ck_realloc (the_line, line_alloc * sizeof (struct pr_node *));
      c_node = &the_line[line_alloc / 2];
    }

  if (*expr)
    goto next_byte;

  /* if(c_node != &the_line[1]) {
		io_error_msg("%d values on decompile stack!",c_node - the_line);
		return the_line[0];
	} */
  new = *--c_node;
  /* free(the_line); */
  return new;
}

/* Actual entry points to this file */
/* decomp(row, col, cell) returns a string that can be byte_compiled to create
   cell->formula  decomp_free() frees up the allocated string */
#if __STDC__
char *
decomp (CELLREF r, CELLREF c, CELL *cell)
#else
char *
decomp (r, c, cell)
     CELLREF r;
     CELLREF c;
     CELL *cell;
#endif
{
  struct pr_node *ret;
  char *str;
  extern char *bname[];

  decomp_row = r;
  decomp_col = c;
  if (cell->cell_formula == 0)
    {
      switch (GET_TYP (cell))
	{
	case 0:
	  str = ck_malloc (1);
	  str[0] = '\0';
	  break;
	case TYP_FLT:
	  str = strdup (flt_to_str (cell->cell_flt));
	  break;
	case TYP_INT:
	  str = ck_malloc (20);
	  sprintf (str, "%ld", cell->cell_int);
	  break;
	case TYP_STR:
	  str = strdup (backslash_a_string (cell->cell_str, 1));
	  break;
	case TYP_BOL:
	  str = strdup (bname[cell->cell_bol]);
	  break;
	case TYP_ERR:
	  str = strdup (ename[cell->cell_bol]);
	  break;
#ifdef TEST
	default:
	  str = 0;
	  panic ("Unknown type %d in decomp", GET_TYP (cell));
#endif
	}
      save_decomp = (VOIDSTAR) str;
      return str;
    }
  else
    ret = byte_decompile (cell->cell_formula);
  save_decomp = (VOIDSTAR) ret;
  return &(ret->string[0]);
}

void
decomp_free ()
{
#ifdef TEST
  if (!save_decomp)
    panic ("No save decomp");
  n_free (save_decomp);
  save_decomp = (VOIDSTAR) 0;
#else
  n_free (save_decomp);
#endif
}

/* This takes a string and returns a backslashed form suitable for printing.
   Iff add_quote is true, it'll add "s at the beginning and end.
   Note that this returns a pointer to a static area that is overwritten with
   each call. . .
 */
char *
backslash_a_string (string, add_quote)
     char *string;
     int add_quote;
{
  char *pf;
  char *pt;
  int ch;
  int size;
  int len;

  static char *cbuf;
  static int s_cbuf;

#define ALLOC_PT()				\
	len=strlen(pf);				\
	size=pf-string;				\
	if(s_cbuf<3+size+4*len) {		\
		s_cbuf=3+size+4*len;		\
		cbuf= (cbuf) ? ck_realloc(cbuf,s_cbuf) : ck_malloc(s_cbuf); \
	}					\
	if(size)				\
		bcopy(string,cbuf,size);	\
	pt=cbuf+size;				\


  pt = 0;
  pf = string;
  if (add_quote)
    {
      ALLOC_PT ()
	* pt++ = '"';
    }
  for (; *pf; pf++)
    {
      ch = *pf;
      if (ch >= ' ' && ch <= '~' && ch != '\\' && (ch != '"' || !add_quote))
	{
	  if (pt)
	    *pt++ = ch;
	  continue;
	}

      if (!pt)
	{
	  ALLOC_PT ()
	}
      if (ch == '\\')
	{
	  *pt++ = '\\';
	  *pt++ = '\\';
	}
      else if (ch == '"')
	{
	  *pt++ = '\\';
	  *pt++ = ch;
	}
      else
	{
	  *pt++ = '\\';
	  *pt++ = ((ch >> 6) & 0x3) + '0';
	  *pt++ = ((ch >> 3) & 0x7) + '0';
	  *pt++ = (ch & 0x7) + '0';
	}
    }
  if (add_quote)
    *pt++ = '"';
  if (pt)
    {
      *pt++ = '\0';
      return cbuf;
    }
  return string;
}

#ifdef TEST
void
dbg_print_formula (expr)
     unsigned char *expr;
{
  unsigned char byte;
  struct function *f;
  double tmp_flt;
  char *tmp_str;
  long tmp_int;
  struct var *v;
  struct rng rng;
  char *buf;
  unsigned jumpto;
  extern char print_buf[];
  extern char *strcpy ();

  if (!expr)
    return;
  strcpy (print_buf, "Formula: ");
  buf = print_buf + 9;
  while (*expr)
    {
      byte = *expr++;

      if (byte < USR1)
	f = &the_funs[byte];
      else if (byte < SKIP)
	{
	  tmp_int = *expr++;
	  f = &usr_funs[byte - USR1][tmp_int];
	}
      else
	f = &skip_funs[byte - SKIP];

      if (f->fn_argn & X_J)
	jumpto = *expr++;
      else if (f->fn_argn & X_JL)
	{
	  jumpto = expr[0] + ((unsigned) (expr[1]) << 8);
	  expr += 2;
	}
      else
	jumpto = 0;

      switch (GET_COMP (f->fn_comptype))
	{
	case C_IF:
	  sprintf (buf, " if%d.%u", byte, jumpto);
	  break;
	case C_ANDOR:
	  sprintf (buf, " andor%d.%u", byte, jumpto);
	  break;
	case C_ERR:
	  tmp_str = (char *) (expr + jumpto);
	  byte = *expr++;
	  sprintf (buf, " err%d.%p(%s)", byte, tmp_str, tmp_str);
	  break;
	case C_FLT:
	  bcopy ((VOIDSTAR) expr, (VOIDSTAR) & tmp_flt, sizeof (double));
	  expr += sizeof (double);
	  sprintf (buf, " flt.%.15g", tmp_flt);
	  break;
	case C_INT:
	  bcopy ((VOIDSTAR) expr, (VOIDSTAR) & tmp_int, sizeof (long));
	  expr += sizeof (long);
	  sprintf (buf, " int.%ld", tmp_int);
	  break;
	case C_STR:
	  tmp_str = (char *) (expr + jumpto);
	  sprintf (buf, " str.%p.%s", tmp_str, tmp_str);
	  break;
	case C_VAR:
	  bcopy ((VOIDSTAR) expr, (VOIDSTAR) & v, sizeof (struct var *));
	  expr += sizeof (struct var *);
	  sprintf (buf, " var.%p.%s", v, v->var_name);
	  break;
	case C_CELL:
	  sprintf (buf, " ref%d.%u.%u", byte, GET_ROW (expr), GET_COL (expr));
	  expr += EXP_ADD;
	  break;
	case C_RANGE:
	  GET_RNG (expr, &rng);
	  sprintf (buf, " rng%d.%u-%u,%u-%u", byte, rng.lr, rng.hr, rng.lc, rng.hc);
	  expr += EXP_ADD_RNG;
	  break;
	case C_FN0:
	case C_FN0X:
	case C_FN1:
	case C_FN2:
	case C_FN3:
	case C_FN4:
	case C_UNA:
	case C_INF:
	  sprintf (buf, " fun%d.%s.%p", byte, f->fn_str, f->fn_fun);
	  break;

	case C_FNN:
	  sprintf (buf, " funn%d.%s.%p.%d", byte, f->fn_str, f->fn_fun, *expr++);
	  break;
	case C_CONST:
	  sprintf (buf, " const%s", f->fn_str);
	  break;
	case C_SKIP:
	  sprintf (buf, " skip.%d", jumpto);
	  break;
	case 0:
	  sprintf (buf, " ???%d", byte);
	  break;
	default:
	  io_error_msg ("Unknown decompile type %d", f->fn_comptype);
	}
      buf += strlen (buf);
    }
  io_text_line (print_buf);
}

#endif
