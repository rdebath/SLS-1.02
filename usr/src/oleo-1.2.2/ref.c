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
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <signal.h>
#include "sysdef.h"
#include "global.h"
#include "cell.h"
#include "eval.h"
#include "io-abstract.h"
#include "io-generic.h"
#include "hash.h"
#include "byte-compile.h"
#include "parse.h"
#include "ref.h"


#ifdef __STDC__
static void add_ref_fm (struct ref_fm **, CELLREF, CELLREF);
static void flush_ref_fm (struct ref_fm **, CELLREF, CELLREF);
static void flush_range_ref (struct rng *, CELLREF, CELLREF);
extern void shift_formula (int r, int c, int dn, int ov);
#ifdef SPLIT_REFS
/* These two are tunable paramaters */
#define REF_START	3
#define REF_INC		*=2
#else
static void flush_ref_to (struct ref_to **);
static void flush_fm_ref (struct ref_fm *);
#endif /* SPLIT_REFS */

#else /* !__STDC__ */

static void add_ref_fm ();
static void flush_ref_fm ();
static void flush_range_ref ();
extern void shift_formula ();

#ifdef SPLIT_REFS
/* These two are tunable paramaters */
#define REF_START	3
#define REF_INC		*=2
#else
static void flush_ref_to ();
static void flush_fm_ref ();
#endif /* SPLIT_REFS */
#endif /* __STDC__ */

/* More tunable paramaters */

#define FIFO_START	40
#define FIFO_INC	*=2

#define TO_MAGIC(row,col)	(((long)(row)<<BITS_PER_CELLREF)|(col))
#define MAGIC_ROW(magic)	(((magic)>>BITS_PER_CELLREF)&CELLREF_MASK)
#define MAGIC_COL(magic)	((magic)&CELLREF_MASK)

#define BETWEEN(mid,lo,hi)	((mid>=lo)&&(mid<=hi))

static VOIDSTAR moving;

/* These three for async cell updating */
extern unsigned signal_ticks;

int timer_active = 0;
struct ref_fm *timer_cells;

CELL *my_cell;

#ifdef TEST
extern int debug;
#endif

/* Functions for dealing exclusively with variables */
struct hash_control *the_vars;

struct value
  {
    int type;
    union vals c_z;
  };

/* For the fifo-buffer */
struct pos
  {
    CELLREF row;
    CELLREF col;
  };

struct cell_buf
  {
    unsigned int size;
    struct pos *buf;
    struct pos *push_to_here;
    struct pos *pop_frm_here;
  };


/* Set the cell ROW,COL to STRING, parsing string as needed */
void
set_cell (row, col, string)
     CELLREF row;
     CELLREF col;
     char *string;
{
  unsigned char *ret;

  cur_row = row;
  cur_col = col;

#ifdef TEST
  if (!string)
    {
      io_error_msg ("Null string to set_cell %s", cell_name (row, col));
      return;
    }
#endif
  while (*string == ' ')
    string++;

  if (!*string)
    {
      my_cell = find_cell (cur_row, cur_col);
      if (!my_cell)
	return;
      flush_old_value ();
      return;
    }

  my_cell = find_or_make_cell (cur_row, cur_col);
  flush_old_value ();

  ret = parse_and_compile (string);
  my_cell->cell_formula = ret;
}

extern int default_lock;

/* new_value() calls set_cell, but refuses to change locked cells, and
   updates and prints the results.  It returns an error msg on error. . .
 */
#if __STDC__
char *
new_value (CELLREF row, CELLREF col, char *string)
#else
char *
new_value (row, col, string)
     CELLREF row;
     CELLREF col;
     char *string;
#endif
{
  CELL *cp;

  cp = find_cell (row, col);
  if (((!cp || GET_LCK (cp) == LCK_DEF) && default_lock == LCK_LCK) || (cp && GET_LCK (cp) == LCK_LCK))
    {
      return "cell is locked";
    }

  set_cell (row, col, string);
  if (my_cell)
    {
      update_cell (my_cell);
      if (is_constant (my_cell->cell_formula))
	{
	  byte_free (my_cell->cell_formula);
	  my_cell->cell_formula = 0;
	}
      io_pr_cell (row, col, my_cell);
      my_cell = 0;
    }
  return 0;
}

/* This sets the cell to a constant, stored in VALUE, whose type is in TYPE */
#if __STDC__
char *
set_new_value (CELLREF row, CELLREF col, int type, union vals *value)
#else
char *
set_new_value (row, col, type, value)
     CELLREF row;
     CELLREF col;
     int type;
     union vals *value;
#endif
{
  CELL *cp;
  extern int default_lock;

  if (type == TYP_ERR)
    type = 0;
  cur_row = row;
  cur_col = col;
  if (type == 0)
    {
      cp = find_cell (row, col);
      if (cp && GET_TYP (cp))
	{
	  if ((GET_LCK (cp) == LCK_DEF && default_lock == LCK_LCK) || GET_LCK (cp) == LCK_LCK)
	    return "cell is locked";
	  my_cell = cp;
	  flush_old_value ();
	  SET_TYP (cp, 0);
	}
      my_cell = 0;
      return 0;
    }
  else
    {
      cp = find_or_make_cell (row, col);
      if ((GET_LCK (cp) == LCK_DEF && default_lock == LCK_LCK) || GET_LCK (cp) == LCK_LCK)
	return "cell is locked";
      my_cell = cp;
      flush_old_value ();
      SET_TYP (cp, type);
      /* cp->c_z= *value; */
      switch (type)
	{
	case TYP_FLT:
	  cp->cell_flt = value->c_d;
	  cp->cell_formula = 0;
	  break;

	case TYP_INT:
	  cp->cell_int = value->c_l;
	  cp->cell_formula = 0;
	  break;

	case TYP_STR:
	  cp->cell_str = strdup (value->c_s);
	  cp->cell_formula = 0;
	  break;

	case TYP_BOL:
	  cp->cell_bol = value->c_i;
	  cp->cell_formula = 0;
	  break;

	case TYP_ERR:
	  cp->cell_err = value->c_i;
	  cp->cell_formula = 0;
	  break;
#ifdef TEST
	default:
	  panic ("Unknown type %d in set_new_value", GET_TYP (cp));
#endif
	}
    }
  push_refs (cp->cell_refs_from);
  io_pr_cell (row, col, cp);
  my_cell = 0;
  return 0;
}

/* We're reading in a cell, whose formula is FORM, and whose current value
   is VAL.  Parse both of them. . .  (Parsing of VAL is quite primitive)
 */
#if __STDC__
char *
read_new_value (CELLREF row, CELLREF col, char *form, char *val)
#else
char *
read_new_value (row, col, form, val)
     CELLREF row;
     CELLREF col;
     char *form;
     char *val;
#endif
{
  unsigned char *new_bytes;
  extern double __plinf, __neinf, __nan;

  cur_row = row;
  cur_col = col;
  my_cell = find_or_make_cell (cur_row, cur_col);
  flush_old_value ();
  SET_TYP (my_cell, 0);

  if (form)
    {
      new_bytes = parse_and_compile (form);
      my_cell->cell_formula = new_bytes;
    }

  if (val)
    {
      if (val[0] == '"')
	{
	  char *sp, *nsp;

	  sp = val + 1;
	  SET_TYP (my_cell, TYP_STR);
	  while (*sp)
	    sp++;
	  if (*--sp != '"')
	    {
	      if (*sp == '\r' && sp[-1] == '"')
		--sp;
	      else
		panic ("Can't find \" in read_new value");
	    }
	  *sp = '\0';
	  nsp = my_cell->cell_str = ck_malloc (sp - val);
	  for (sp = val + 1; *sp;)
	    *nsp++ = *sp++;
	  *nsp++ = '\0';
	}
      else if (isdigit (val[0]) || val[0] == '.' || val[0] == '-' || val[0] == '+')
	{
	  char *v;

	  v = val;
	  SET_TYP (my_cell, TYP_INT);
	  my_cell->cell_int = astol (&v);
	  if (*v)
	    {
	      SET_TYP (my_cell, TYP_FLT);
	      v = val;
	      my_cell->cell_flt = astof (&v);
	      if (*v)
		return "unknown number";
	    }
	}
      else if (val[0] == '#')
	{
	  char **en;

	  if (!stricmp (tname, val))
	    {
	      SET_TYP (my_cell, TYP_BOL);
	      my_cell->cell_bol = 1;
	    }
	  else if (!stricmp (fname, val))
	    {
	      SET_TYP (my_cell, TYP_BOL);
	      my_cell->cell_bol = 0;
	    }
	  else if (!stricmp (iname, val))
	    {
	      SET_TYP (my_cell, TYP_FLT);
	      my_cell->cell_flt = __plinf;
	    }
	  else if (!stricmp (iname, val))
	    {
	      SET_TYP (my_cell, TYP_FLT);
	      my_cell->cell_flt = __plinf;
	    }
	  else if (!stricmp (mname, val))
	    {
	      SET_TYP (my_cell, TYP_FLT);
	      my_cell->cell_flt = __neinf;
	    }
	  else if (!stricmp (nname, val))
	    {
	      SET_TYP (my_cell, TYP_FLT);
	      my_cell->cell_flt = __nan;
	    }
	  else
	    {
	      SET_TYP (my_cell, TYP_ERR);
	      for (en = ename; *en; en++)
		if (!stricmp (*en, val))
		  break;
	      if (*en)
		my_cell->cell_err = en - &ename[0];
	      else
		my_cell->cell_err = 1;
	    }
	}
      else
	panic ("What is a '%s'?", val);
    }

  my_cell = 0;
  return 0;
}

/* This moves the contents, format, etc from RF,CF to RT,CT  RF or RT may be
   NON_ROW, in which case the cell's contents are moved to/from a static
   storage area.  Moving anything from NON_ROW before moving anything into it
   or moving two things at a time into NON_ROW are both bad ideas. . .

   Also note that move_cell does not call move_outside, which may or may not
   be a bug. . .  Move_cell is only called as part of sorting, which is why
   we may *not* want to call move_outside. . .
 */

#if __STDC__
void
move_cell (CELLREF rf, CELLREF cf, CELLREF rt, CELLREF ct)
#else
void
move_cell (rf, cf, rt, ct)
     CELLREF rf;
     CELLREF cf;
     CELLREF rt;
     CELLREF ct;
#endif
{
  CELL *cpf;

  static CELLREF non_rf, non_cf;
  static struct cell non_cell;

  if (rf == NON_ROW)
    {
      cur_row = rt;
      cur_col = ct;
      my_cell = find_cell (cur_row, cur_col);
      if (my_cell)
	flush_old_value ();
      else if (!non_cell.cell_flags && !non_cell.cell_formula
	       && !non_cell.cell_font)
	return;
      else
	my_cell = find_or_make_cell (cur_row, cur_col);

      my_cell->cell_flags = non_cell.cell_flags;
      my_cell->cell_refs_to = non_cell.cell_refs_to;
      my_cell->cell_formula = non_cell.cell_formula;
      my_cell->cell_cycle = non_cell.cell_cycle;
      my_cell->cell_font = non_cell.cell_font;
      my_cell->c_z = non_cell.c_z;
      push_refs (my_cell->cell_refs_from);
      if (my_cell->cell_refs_to)
	shift_formula (cur_row, cur_col, rt - non_rf, ct - non_cf);
      my_cell = 0;
      return;
    }

  cpf = find_cell (rf, cf);

  if (rt == NON_ROW)
    {
      non_rf = rf;
      non_cf = cf;
      if (!cpf)
	bzero (&non_cell, sizeof (non_cell));
      else
	{
	  non_cell.cell_flags = cpf->cell_flags;
	  non_cell.cell_refs_to = cpf->cell_refs_to;
	  non_cell.cell_formula = cpf->cell_formula;
	  non_cell.cell_cycle = cpf->cell_cycle;
	  non_cell.cell_font = cpf->cell_font;
	  non_cell.c_z = cpf->c_z;
	  cpf->cell_flags = 0;
	  cpf->cell_refs_to = 0;
	  cpf->cell_formula = 0;
	  cpf->cell_cycle = 0;
	  cpf->cell_font = 0;
	}
      return;
    }

  cur_row = rt;
  cur_col = ct;
  my_cell = find_cell (cur_row, cur_col);
  if ((!cpf || (!cpf->cell_flags && !cpf->cell_formula && !cpf->cell_font))
      && !my_cell)
    return;
  if (!my_cell)
    {
      my_cell = find_or_make_cell (cur_row, cur_col);
      cpf = find_cell (rf, cf);	/* FOO */
    }
  else
    flush_old_value ();

  if (!cpf)
    return;

  my_cell->cell_flags = cpf->cell_flags;
  my_cell->cell_refs_to = cpf->cell_refs_to;
  my_cell->cell_formula = cpf->cell_formula;
  my_cell->cell_cycle = cpf->cell_cycle;
  my_cell->cell_font = cpf->cell_font;
  my_cell->c_z = cpf->c_z;

  cpf->cell_flags = 0;
  cpf->cell_refs_to = 0;
  cpf->cell_formula = 0;
  cpf->cell_cycle = 0;
  cpf->cell_font = 0;

  push_refs (my_cell->cell_refs_from);
  if (my_cell->cell_refs_to)
    shift_formula (cur_row, cur_col, rt - rf, ct - cf);
  my_cell = 0;
}

#if __STDC__
void
copy_cell (CELLREF rf, CELLREF cf, CELLREF rt, CELLREF ct)
#else
void
copy_cell (rf, cf, rt, ct)
     CELLREF rf;
     CELLREF cf;
     CELLREF rt;
     CELLREF ct;
#endif
{
  CELL *cpf;

  cpf = find_cell (rf, cf);
  cur_row = rt;
  cur_col = ct;
  my_cell = find_cell (cur_row, cur_col);
  if ((!cpf || (!cpf->cell_flags && !cpf->cell_formula && !cpf->cell_font))
      && !my_cell)
    return;
  if (!my_cell)
    {
      my_cell = find_or_make_cell (cur_row, cur_col);
      cpf = find_cell (rf, cf);	/* FOO */
    }
  else
    flush_old_value ();

  if (!cpf)
    return;

  my_cell->cell_flags = cpf->cell_flags;
  my_cell->cell_cycle = cpf->cell_cycle;
  my_cell->cell_font = cpf->cell_font;
  my_cell->cell_refs_to = cpf->cell_refs_to;

  if (my_cell->cell_refs_to)
    my_cell->cell_refs_to->refs_refcnt++;

  if (GET_TYP (my_cell) == TYP_STR)
    my_cell->cell_str = strdup (cpf->cell_str);
  else
    my_cell->c_z = cpf->c_z;

  if (cpf->cell_formula)
    {
      unsigned char *fp;
      unsigned char *hi;
      unsigned char byte;
      CELLREF trr, tcc;
      struct rng trng;
      struct function *f;
      size_t len;
      struct var *v;
      CELL *tcp;

      fp = cpf->cell_formula;
      hi = 0;
      if (!moving)
	moving = init_stack ();
      while ((byte = *fp++) != ENDCOMP)
	{
	  if (byte < USR1)
	    f = &the_funs[byte];
	  else if (byte < SKIP)
	    {
	      int tmp;
#ifdef TEST
	      if (byte - USR1 >= n_usr_funs)
		panic ("Only have %d usr-function slots, but found byte for slot %d", n_usr_funs, 1 + byte - USR1);
#endif
	      tmp = *fp++;
	      f = &usr_funs[byte - USR1][tmp];
	    }
	  else
	    f = &skip_funs[byte - SKIP];

	  if (f->fn_argn & X_J)
	    fp++;
	  else if (f->fn_argn & X_JL)
	    fp += 2;

	  if ((f->fn_argn & X_ARGS) == X_AN)
	    fp++;

	  switch (byte)
	    {
	    case CONST_FLT:
	      fp += sizeof (double);
	      break;

	    case CONST_INT:
	      fp += sizeof (long);
	      break;

	    case CONST_STR:
	      if (!hi)
		hi = fp + fp[-1];
	      break;

	    case CONST_STR_L:
	      if (!hi)
		hi = fp + fp[-2] + ((unsigned) (fp[-1]) << 8);
	      break;

	    case CONST_ERR:
	      fp += 1 /* +sizeof(char *) */ ;
	      break;

	    case VAR:
	      bcopy (fp, &v, sizeof (struct var *));
	      fp += sizeof (struct var *);
	      add_ref_fm (&(v->var_ref_fm), cur_row, cur_col);
	      switch (v->var_flags)
		{
		case VAR_UNDEF:
		  break;
		case VAR_CELL:
		  tcp = find_cell (v->v_rng.lr, v->v_rng.lc);
		  add_ref_fm (&(tcp->cell_refs_from), cur_row, cur_col);
		  break;
		case VAR_RANGE:
		  add_range_ref (&(v->v_rng));
		  /* sparse array bug fixed here */
		  my_cell = find_cell (cur_row, cur_col);
		  cpf = find_cell (rf, cf);
		  break;
		}
	      break;

	    case R_CELL:
	    case R_CELL | COLREL:
	    case R_CELL | ROWREL:
	    case R_CELL | ROWREL | COLREL:
	      push_stack (moving, fp);
	      fp += EXP_ADD;
	      break;

	    case RANGE:
	    case RANGE | LRREL:
	    case RANGE | LRREL | LCREL:
	    case RANGE | LRREL | LCREL | HCREL:
	    case RANGE | LRREL | HCREL:
	    case RANGE | LRREL | HRREL:
	    case RANGE | LRREL | HRREL | LCREL:
	    case RANGE | LRREL | HRREL | LCREL | HCREL:
	    case RANGE | LRREL | HRREL | HCREL:
	    case RANGE | HRREL:
	    case RANGE | HRREL | LCREL:
	    case RANGE | HRREL | LCREL | HCREL:
	    case RANGE | HRREL | HCREL:
	    case RANGE | LCREL:
	    case RANGE | LCREL | HCREL:
	    case RANGE | HCREL:
	      push_stack (moving, fp);
	      fp += EXP_ADD_RNG;
	      break;

	    default:
	      break;
	    }
	}
      if (!hi)
	hi = fp;
      else
	hi += strlen ((char *) hi);
      hi++;
      len = hi - cpf->cell_formula;
      my_cell->cell_formula = cpf->cell_formula;
      cpf->cell_formula = ck_malloc (hi - cpf->cell_formula);
      bcopy (my_cell->cell_formula, cpf->cell_formula, len);
      while (fp = pop_stack (moving))
	{
	  byte = fp[-1];
	  if ((byte | ROWREL | COLREL) == (R_CELL | ROWREL | COLREL))
	    {
	      trr = GET_ROW (fp);
	      tcc = GET_COL (fp);
	      if (byte & ROWREL)
		{
		  trr += rt - rf;
		  PUT_ROW (fp, trr);
		}
	      if (byte & COLREL)
		{
		  tcc += ct - cf;
		  PUT_COL (fp, tcc);
		}
	      tcp = find_or_make_cell (trr, tcc);
	      add_ref_fm (&(tcp->cell_refs_from), cur_row, cur_col);
	    }
#ifdef TEST
	  else if ((byte | LRREL | HRREL | LCREL | HCREL) !=
		   (RANGE | LRREL | HRREL | LCREL | HCREL))
	    panic ("Unknown byte %x in copy_cell", byte);
#endif
	  else
	    {
	      GET_RNG (fp, &trng);
	      if (byte & LRREL)
		trng.lr += rt - rf;
	      if (byte & HRREL)
		trng.hr += rt - rf;
	      if (byte & LCREL)
		trng.lc += ct - cf;
	      if (byte & HCREL)
		trng.hc += ct - cf;
	      PUT_RNG (fp, &trng);
	      add_range_ref (&trng);
	      /* sparse array bug fixed here */
	      my_cell = find_cell (cur_row, cur_col);
	      cpf = find_cell (rf, cf);
	    }
	}
      update_cell (my_cell);
    }
  else
    {
      my_cell->cell_formula = 0;
    }
  io_pr_cell (cur_row, cur_col, my_cell);

  push_refs (my_cell->cell_refs_from);
  my_cell = 0;
}

/* Take away the value of CP.  This means getting rid of all the references
   to it, etc.
 */
void
flush_old_value ()
{
  struct ref_to *ref;
  unsigned char *refloc;
  int n;
  unsigned char byte;
  CELL *other_cell;
  struct var *varp;

  ref = my_cell->cell_refs_to;
  if (ref)
    {
      for (n = 0; n < ref->refs_used; n++)
	{
	  /* Switch on formula[ref->to_refs[n]] */
	  refloc = &(my_cell->cell_formula[ref->to_refs[n]]);
	  byte = refloc[0];
	  switch (byte)
	    {
	    case F_ROW:
	    case F_COL:
	      break;

	    case R_CELL:
	    case R_CELL | ROWREL:
	    case R_CELL | COLREL:
	    case R_CELL | ROWREL | COLREL:
	      other_cell = find_cell (GET_ROW (refloc + 1), GET_COL (refloc + 1));
	      if (other_cell)
		flush_ref_fm (&(other_cell->cell_refs_from), cur_row, cur_col);
#ifdef TEST
	      else
		io_error_msg ("Can't find other_cell in flush_old_value");
#endif
	      break;
	    case RANGE:
	    case RANGE | LRREL:
	    case RANGE | LRREL | LCREL:
	    case RANGE | LRREL | LCREL | HCREL:
	    case RANGE | LRREL | HCREL:
	    case RANGE | LRREL | HRREL:
	    case RANGE | LRREL | HRREL | LCREL:
	    case RANGE | LRREL | HRREL | LCREL | HCREL:
	    case RANGE | LRREL | HRREL | HCREL:
	    case RANGE | HRREL:
	    case RANGE | HRREL | LCREL:
	    case RANGE | HRREL | LCREL | HCREL:
	    case RANGE | HRREL | HCREL:
	    case RANGE | LCREL:
	    case RANGE | LCREL | HCREL:
	    case RANGE | HCREL:
	      {
		struct rng rng;

		GET_RNG (refloc + 1, &rng);
		flush_range_ref (&rng, cur_row, cur_col);
	      }
	      break;

	    case VAR:
	      bcopy (&refloc[1], &varp, sizeof (struct var *));
	      flush_ref_fm (&(varp->var_ref_fm), cur_row, cur_col);
	      if (varp->var_flags == VAR_CELL)
		{
		  other_cell = find_cell (varp->v_rng.lr, varp->v_rng.lc);
		  if (other_cell)
		    flush_ref_fm (&(other_cell->cell_refs_from), cur_row, cur_col);
		}
	      else if (varp->var_flags == VAR_RANGE)
		flush_range_ref (&(varp->v_rng), cur_row, cur_col);
#ifdef TEST
	      else if (varp->var_flags != VAR_UNDEF)
		panic ("Unknown var type %d", varp->var_flags);
#endif
	      break;

	    default:
	      {
		struct function *fun;

		if (byte < USR1)
		  fun = &the_funs[byte];
#ifdef TEST
		else if (byte >= SKIP)
		  fun = 0, panic ("SKIP? in flush_old_value()");
#endif
		else
		  fun = &usr_funs[byte - USR1][refloc[1]];

		if (fun->fn_comptype & C_T)
		  {
#ifdef TEST
		    if (!timer_cells || !timer_cells->refs_used)
		      panic ("No timer cells in flush_timer_cell");
#endif
		    flush_ref_fm (&timer_cells, cur_row, cur_col);
		    if (!timer_cells || timer_cells->refs_used == 0)
		      {
			timer_active = 0;
			alarm_active = 0;
		      }
		    break;
		  }
		else
		  io_error_msg ("Bad ref_to of %d.%x ignored", ref->to_refs[n], byte);
	      }
	      break;
	    }
	}
#ifdef SPLIT_REFS
      ref->refs_used = 0;
#else
      flush_ref_to (&(my_cell->cell_refs_to));
#endif
    }
  if (my_cell->cell_formula)
    {
      byte_free (my_cell->cell_formula);
      my_cell->cell_formula = 0;
    }
  if (GET_TYP (my_cell) == TYP_STR)
    free (my_cell->cell_str);
  SET_TYP (my_cell, 0);
}

/* --------- Routines for dealing with cell references to other cells ------ */

#if __STDC__
void
add_ref (CELLREF row, CELLREF col)
#else
void
add_ref (row, col)
     CELLREF row;
     CELLREF col;
#endif
{
  CELL *other_cell;

  other_cell = find_or_make_cell (row, col);
  add_ref_fm (&(other_cell->cell_refs_from), cur_row, cur_col);
}

void
add_range_ref (rng)
     struct rng *rng;
{
  CELL *other_cell;
  struct ref_fm *oldref, *newref;
  struct ref_fm nonref;


  make_cells_in_range (rng);

  /* Be efficient:  If cells in the range currently have the same
   * references, they'll have the same references afterward, so just
   * adjust the refcounts
   */
  nonref.refs_refcnt = 1;
  other_cell = next_cell_in_range ();
  oldref = other_cell->cell_refs_from;
  if (oldref && oldref->refs_refcnt == 1)
    oldref = &nonref;

  add_ref_fm (&(other_cell->cell_refs_from), cur_row, cur_col);
  newref = other_cell->cell_refs_from;
  while (other_cell = next_cell_in_range ())
    {
      if (other_cell->cell_refs_from == oldref)
	{
	  if (oldref)
	    {
	      if (oldref->refs_refcnt == 1)
		{
		  flush_fm_ref (oldref);
		  oldref = &nonref;
		}
	      else
		oldref->refs_refcnt--;
	    }
	  other_cell->cell_refs_from = newref;
	  newref->refs_refcnt++;
	}
      else if (oldref == &nonref && (!other_cell->cell_refs_from || other_cell->cell_refs_from->refs_refcnt > 1))
	{
	  oldref = other_cell->cell_refs_from;
	  add_ref_fm (&(other_cell->cell_refs_from), cur_row, cur_col);
	  newref = other_cell->cell_refs_from;
	}
      else
	add_ref_fm (&(other_cell->cell_refs_from), cur_row, cur_col);
    }
  /* if(oldref && oldref->refs_refcnt==0) {
		oldref->refs_refcnt=1;
		flush_fm_ref(oldref);
	} */
}

#if __STDC__
static void
flush_range_ref (struct rng *rng, CELLREF rr, CELLREF cc)
#else
static void
flush_range_ref (rng, rr, cc)
     struct rng *rng;
     CELLREF rr;
     CELLREF cc;
#endif
{
  CELL *other_cell;
#ifndef SPLIT_REFS
  struct ref_fm *oldref, *newref;
  struct ref_fm nonref;
#endif
  /* This is horribly inefficient:  Simply referencing a cell makes
	   it appear.  On the other hand, there is no other easy way to deal
	   with the references to the cells (That I know of, anyway) */
  find_cells_in_range (rng);
#ifndef SPLIT_REFS
  /* Be efficient:  If cells in the range currently have the same
	   references, they'll have the same references afterward, so just
	   adjust the refcounts */
  nonref.refs_refcnt = 1;
  other_cell = next_cell_in_range ();
  if (!other_cell)
    return;
  oldref = other_cell->cell_refs_from;
  if (oldref && oldref->refs_refcnt == 1)
    oldref = &nonref;

  flush_ref_fm (&(other_cell->cell_refs_from), rr, cc);
  newref = other_cell->cell_refs_from;
  while (other_cell = next_cell_in_range ())
    {
      if (other_cell->cell_refs_from == oldref)
	{
	  if (oldref)
	    {
	      if (oldref->refs_refcnt == 1)
		{
		  flush_fm_ref (oldref);
		  oldref = &nonref;
		}
	      else
		oldref->refs_refcnt--;
	    }
	  other_cell->cell_refs_from = newref;
	  if (newref)
	    newref->refs_refcnt++;
	}
      else if (oldref == &nonref && (!other_cell->cell_refs_from || other_cell->cell_refs_from->refs_refcnt > 1))
	{
	  oldref = other_cell->cell_refs_from;
	  flush_ref_fm (&(other_cell->cell_refs_from), rr, cc);
	  newref = other_cell->cell_refs_from;
	}
      else
	flush_ref_fm (&(other_cell->cell_refs_from), rr, cc);
    }
#else
  while (other_cell = next_cell_in_range ())
    flush_ref_fm (&(other_cell->cell_refs_from), rr, cc);
#endif
}

/* SPLIT_REFS should probably go away, since it is a performance loss.
   When it is defined, we allocate a a reference structure for each cell with
   references, and update them individually.

   The default is to allocate one reference structure for each *different*
   reference, and have multiple cells point to it.  We use reference counts
   to keep track of when to delete the reference structure.  This would
   appear to be a performance loss, but because of the increased efficiency
   in referencing ranges, it may actually be faster.  It also uses *far* less
   memory than SPLIT_REFS */


#ifndef SPLIT_REFS
#ifdef __TURBOC__
#define FM_HASH_NUM 51
#define TO_HASH_NUM 13
#else
#define FM_HASH_NUM 503
#define TO_HASH_NUM 29
#endif
#ifdef TEST
static int fm_misses = 0;
static int to_misses = 0;
#endif

static struct ref_fm *fm_list[FM_HASH_NUM];
static struct ref_fm *fm_tmp_ref;
static unsigned fm_tmp_ref_alloc;

static struct ref_to *to_list[TO_HASH_NUM];
static struct ref_to *to_tmp_ref;
static unsigned to_tmp_ref_alloc;

void
flush_refs ()
{
  int n;
  struct ref_fm *ftmp, *oftmp;
  struct ref_to *ttmp, *ottmp;

  for (n = 0; n < FM_HASH_NUM; n++)
    {
      for (ftmp = fm_list[n]; ftmp; ftmp = oftmp)
	{
	  oftmp = ftmp->refs_next;
	  free (ftmp);
	}
      fm_list[n] = 0;
    }
  for (n = 0; n < TO_HASH_NUM; n++)
    {
      for (ttmp = to_list[n]; ttmp; ttmp = ottmp)
	{
	  ottmp = ttmp->refs_next;
	  free (ttmp);
	}
      to_list[n] = 0;
    }
}

static struct ref_fm *
find_fm_ref ()
{
  struct ref_fm *tmp;
  int n;
  unsigned long hash;

#if 1
  for (hash = 0, n = 0; n < fm_tmp_ref->refs_used; n++)
    {
      hash += (n + 1) * (((fm_tmp_ref->fm_refs[n].ref_row) << BITS_PER_CELLREF) +
			 fm_tmp_ref->fm_refs[n].ref_col);
    }
  hash %= FM_HASH_NUM;
#else
  hash = fm_tmp_ref->refs_used;
#endif
  for (tmp = fm_list[hash]; tmp; tmp = tmp->refs_next)
    {
      if (tmp->refs_used != fm_tmp_ref->refs_used)
	continue;
      if (!bcmp (tmp->fm_refs, fm_tmp_ref->fm_refs, fm_tmp_ref->refs_used * sizeof (struct ref_array)))
	{
	  tmp->refs_refcnt++;
	  return tmp;
	}
#ifdef TEST
      else
	fm_misses++;
#endif
    }

  tmp = ck_malloc (sizeof (struct ref_fm) + (fm_tmp_ref->refs_used - 1) * sizeof (struct ref_array));
  tmp->refs_next = fm_list[hash];
  fm_list[hash] = tmp;
  tmp->refs_refcnt = 1;
  tmp->refs_used = fm_tmp_ref->refs_used;
  bcopy (fm_tmp_ref->fm_refs, tmp->fm_refs, tmp->refs_used * sizeof (struct ref_array));

  return tmp;
}

static void 
flush_fm_ref (old)
     struct ref_fm *old;
{
  struct ref_fm *tmp;
  int n;
  unsigned long hash;

  --(old->refs_refcnt);

#ifdef DEFER_FREE
  return;
#endif
  if (!old->refs_refcnt)
    {
#if 1
      for (hash = 0, n = 0; n < old->refs_used; n++)
	{
	  hash += (n + 1) * (((old->fm_refs[n].ref_row) << BITS_PER_CELLREF) +
			     old->fm_refs[n].ref_col);
	}
      hash %= FM_HASH_NUM;
#else
      hash = old->refs_used;
#endif
      if (fm_list[hash] == old)
	fm_list[hash] = old->refs_next;
      else
	{
	  for (tmp = fm_list[hash]; tmp && tmp->refs_next != old; tmp = tmp->refs_next)
	    ;
#ifdef TEST
	  if (!tmp)
	    {
	      io_error_msg ("Old not in refs_list in flush_fm_ref(%p)", old);
	      return;
	    }
#endif
	  tmp->refs_next = old->refs_next;
	}
      free (old);
    }
}

/* This adds a from reference to a cells reference list.
 * Note that the ref_fm structures themselves are hash-consed.
 */
#if __STDC__
static void
add_ref_fm (struct ref_fm **where, CELLREF r, CELLREF c)
#else
static void
add_ref_fm (where, r, c)
     struct ref_fm **where;
     CELLREF r;
     CELLREF c;
#endif
{
  struct ref_fm *from;
  int n;

  from = *where;
  if (!from)
    {
      if (!fm_tmp_ref)
	{
	  fm_tmp_ref = ck_malloc (sizeof (struct ref_fm));
	  fm_tmp_ref_alloc = 1;
	}
      fm_tmp_ref->refs_used = 1;
      fm_tmp_ref->fm_refs[0].ref_row = r;
      fm_tmp_ref->fm_refs[0].ref_col = c;
    }
  else
    {
      if (fm_tmp_ref_alloc <= from->refs_used)
	{
	  fm_tmp_ref =
	    ck_realloc (fm_tmp_ref, sizeof (struct ref_fm)
			+ from->refs_used * sizeof (struct ref_array)) ;
	  fm_tmp_ref_alloc = from->refs_used + 1;
	}
      fm_tmp_ref->refs_used = from->refs_used + 1;
      n = 0;
      while (n < from->refs_used
	     && (from->fm_refs[n].ref_row < r
       || (from->fm_refs[n].ref_row == r && from->fm_refs[n].ref_col <= c)))
	{
	  fm_tmp_ref->fm_refs[n] = from->fm_refs[n];
	  n++;
	}
      fm_tmp_ref->fm_refs[n].ref_row = r;
      fm_tmp_ref->fm_refs[n].ref_col = c;
      while (n < from->refs_used)
	{
	  fm_tmp_ref->fm_refs[n + 1] = from->fm_refs[n];
	  n++;
	}
    }
  *where = find_fm_ref ();
  if (from)
    flush_fm_ref (from);
}

#if __STDC__
static void
flush_ref_fm (struct ref_fm **where, CELLREF r, CELLREF c)
#else
static void
flush_ref_fm (where, r, c)
     struct ref_fm **where;
     CELLREF r;
     CELLREF c;
#endif
{
  struct ref_fm *from;
  int n;

  from = *where;
#ifdef TEST
  if (!from)
    {
      io_error_msg ("No refs in flush_ref_fm(%p,%u,%u)", where, r, c);
      return;
    }
#endif
  if (!from)
    return;
  if (from->refs_used == 1)
    {
      *where = 0;
      flush_fm_ref (from);
      return;
    }
  fm_tmp_ref->refs_used = from->refs_used - 1;
  n = 0;
  while (n < from->refs_used
	 && (from->fm_refs[n].ref_row < r
	|| (from->fm_refs[n].ref_row == r && from->fm_refs[n].ref_col < c)))
    {
      fm_tmp_ref->fm_refs[n] = from->fm_refs[n];
      n++;
    }
#ifdef TEST
  if (n == from->refs_used)
    {
      io_error_msg ("No refs from %u,%u in %p in flush_refs_fm", r, c, where);
      return;
    }
#endif
  while (n < fm_tmp_ref->refs_used)
    {
      fm_tmp_ref->fm_refs[n] = from->fm_refs[n + 1];
      n++;
    }
  *where = find_fm_ref ();
  flush_fm_ref (from);
}

#ifdef TEST

void
dbg_print_ref_fm (rf)
     struct ref_fm *rf;
{
  int nr;
  char *bufp;

  if (rf)
    {
      io_text_line ("fm %p: refcnt %u  next %p  used %u",
		    rf, rf->refs_refcnt, rf->refs_next, rf->refs_used);
      for (nr = 0, bufp = print_buf; nr < rf->refs_used; nr++)
	{
	  (void) sprintf (bufp, " %s", cell_name (rf->fm_refs[nr].ref_row, rf->fm_refs[nr].ref_col));
	  if (nr % 10 == 9)
	    {
	      io_text_line (print_buf);
	      bufp = print_buf;
	    }
	  else
	    bufp += strlen (bufp);
	}
      if (nr % 10)
	io_text_line (print_buf);
    }
}

#endif

static struct ref_to *
find_to_ref ()
{
  struct ref_to *tmp;
  int n;
  unsigned long hash;

  /* io_error_msg("find_to_ref %u %u",to_tmp_ref->refs_used,to_tmp_ref->to_refs[0]); */
#if 1
  for (hash = 0, n = 0; n < to_tmp_ref->refs_used; n++)
    hash += (n + 1) * to_tmp_ref->to_refs[n];

  hash %= TO_HASH_NUM;
#else
  hash = to_tmp_ref->refs_used;
#endif
  for (tmp = to_list[hash]; tmp; tmp = tmp->refs_next)
    {
      /* io_error_msg("%p(%u)->%p  %u %u",tmp,tmp->refs_refcnt,
			tmp->refs_next,tmp->refs_used,tmp->to_refs[0]); */
      if (tmp->refs_used != to_tmp_ref->refs_used)
	continue;
      if (!bcmp (tmp->to_refs, to_tmp_ref->to_refs, to_tmp_ref->refs_used))
	{
	  /* io_error_msg("Hit!"); */
	  tmp->refs_refcnt++;
	  return tmp;
	}
#ifdef TEST
      else
	to_misses++;
#endif
    }

  /* io_error_msg("Miss. .."); */
  tmp = ck_malloc (sizeof (struct ref_to) + to_tmp_ref->refs_used - 1);
  tmp->refs_next = to_list[hash];
  to_list[hash] = tmp;
  tmp->refs_refcnt = 1;
  tmp->refs_used = to_tmp_ref->refs_used;
  bcopy (to_tmp_ref->to_refs, tmp->to_refs, tmp->refs_used);

  return tmp;
}

void
add_ref_to (whereto)
     int whereto;
{
  struct ref_to *from;
  int n;

  from = my_cell->cell_refs_to;
  if (!from)
    {
      if (!to_tmp_ref)
	{
	  to_tmp_ref = ck_malloc (sizeof (struct ref_to));
	  to_tmp_ref_alloc = 1;
	}
      to_tmp_ref->refs_used = 1;
      to_tmp_ref->to_refs[0] = whereto;
    }
  else
    {
      if (to_tmp_ref_alloc <= from->refs_used)
	{
	  to_tmp_ref = ck_realloc (to_tmp_ref, sizeof (struct ref_to) + from->refs_used);
	  to_tmp_ref_alloc = from->refs_used + 1;
	}
      to_tmp_ref->refs_used = from->refs_used + 1;
      n = 0;
      while (n < from->refs_used && from->to_refs[n] < whereto)
	{
	  to_tmp_ref->to_refs[n] = from->to_refs[n];
	  n++;
	}
      to_tmp_ref->to_refs[n] = whereto;
      while (n < from->refs_used)
	{
	  to_tmp_ref->to_refs[n + 1] = from->to_refs[n];
	  n++;
	}
      flush_ref_to (&(my_cell->cell_refs_to));
    }
  my_cell->cell_refs_to = find_to_ref ();
}

static void
flush_ref_to (where)
     struct ref_to **where;
{
  struct ref_to *tmp;
  struct ref_to *old;
  int n;
  unsigned long hash;

#ifdef TEST
  if (!where || !*where)
    {
      io_error_msg ("null flush_ref_to(%p)", where);
      return;
    }
#endif
  old = *where;
  *where = 0;
  --(old->refs_refcnt);

#ifdef DEFER_FREE
  return;
#endif
  if (!old->refs_refcnt)
    {
#if 1
      for (hash = 0, n = 0; n < old->refs_used; n++)
	hash += (n + 1) * old->to_refs[n];

      hash %= TO_HASH_NUM;
#else
      hash = old->refs_used;
#endif
      if (to_list[hash] == old)
	to_list[hash] = old->refs_next;
      else
	{
	  for (tmp = to_list[hash]; tmp && tmp->refs_next != old; tmp = tmp->refs_next)
	    ;
#ifdef TEST
	  if (!tmp)
	    {
	      io_error_msg ("Old not in refs_list in flush_to_ref(%p)", old);
	      return;
	    }
#endif
	  tmp->refs_next = old->refs_next;
	}
      free (old);
    }
}

#ifdef TEST
void
dbg_print_ref_to (rt, form)
     struct ref_to *rt;
     unsigned char *form;
{
  int nr;
  char *bufp;

  if (rt)
    {
      io_text_line ("to %p: refcnt %u  next %p  used %u",
		    rt, rt->refs_refcnt, rt->refs_next, rt->refs_used);
      for (nr = 0, bufp = print_buf; nr < rt->refs_used; nr++)
	{
	  (void) sprintf (bufp, " %3d (%#4x)", rt->to_refs[nr], form[rt->to_refs[nr]]);
	  if (nr % 7 == 6)
	    {
	      io_text_line (print_buf);
	      bufp = print_buf;
	    }
	  else
	    bufp += strlen (bufp);
	}
      if (nr % 7)
	io_text_line (print_buf);
    }
}

void
ref_stats ()
{
  int n;
  int cur;
  struct ref_fm *rf;
  struct ref_to *rt;

  int rf_max = 0;
  int rf_num = 0;
  int rf_shared = 0;
  int rf_saved = 0;
  int rf_zero = 0;

  int rt_max = 0;
  int rt_num = 0;
  int rt_shared = 0;
  int rt_saved = 0;
  int rt_zero = 0;

  for (n = 0; n < FM_HASH_NUM; n++)
    {
      cur = 0;
      for (rf = fm_list[n]; rf; rf = rf->refs_next)
	{
	  if (rf->refs_refcnt == 0)
	    rf_zero++;
	  if (rf->refs_refcnt > 1)
	    {
	      rf_shared++;
	      rf_saved += rf->refs_refcnt - 1;
	    }
	  rf_num++;
	  cur++;
	}
      if (cur > rf_max)
	rf_max = cur;
    }
  for (n = 0; n < TO_HASH_NUM; n++)
    {
      cur = 0;
      for (rt = to_list[n]; rt; rt = rt->refs_next)
	{
	  if (rt->refs_refcnt == 0)
	    rt_zero++;
	  if (rt->refs_refcnt > 1)
	    {
	      rt_shared++;
	      rt_saved += rt->refs_refcnt - 1;
	    }
	  rt_num++;
	  cur++;
	}
      if (cur > rt_max)
	rt_max = cur;
    }
  io_text_line ("from: %d refs, max_length %d, shared %d, saved %d, zero_ref %d, missed %d\n", rf_num, rf_max, rf_shared, rf_saved, rf_zero, fm_misses);
  io_text_line ("to: %d refs, max_length %d, shared %d, saved %d, zero_ref %d, missed %d\n", rt_num, rt_max, rt_shared, rt_saved, rt_zero, to_misses);
}

#endif
#else

static void
add_ref_fm (where, r, c)
     struct ref_fm **where;
     CELLREF r;
     CELLREF c;
{
  struct ref_fm *ref;

  ref = *where;
  if (!ref)
    {
      *where = ref = ck_malloc (sizeof (struct ref_fm) + (REF_START - 1) * sizeof (struct ref_array));
      ref->refs_alloc = REF_START;
      ref->refs_used = 0;
    }
  else if (ref->refs_alloc == ref->refs_used)
    {
      ref->refs_alloc REF_INC;
      *where = ref = ck_realloc (ref, sizeof (struct ref_fm) + (ref->refs_alloc - 1) * sizeof (struct ref_array));
    }
  ref->fm_refs[ref->refs_used].ref_row = r;
  ref->fm_refs[ref->refs_used].ref_col = c;
  ref->refs_used++;
}

static void
flush_ref_fm (where, r, c)
     struct ref_fm **where;
     CELLREF r;
     CELLREF c;
{
  int n;
  struct ref_fm *ref;

  ref = *where;
#ifdef TEST
  if (!ref)
    {
      io_error_msg ("%s->No refs in flush_ref_fm(%d,%d)", cell_name (cur_row, cur_col), r, c);
      return;
    }
#endif
  for (n = 0; n < ref->refs_used; n++)
    if (ref->fm_refs[n].ref_row == r && ref->fm_refs[n].ref_col == c)
      {
	ref->fm_refs[n] = ref->fm_refs[ref->refs_used - 1];
	--(ref->refs_used);
	return;
      }
#ifdef TEST
  io_error_msg ("%s->Can't flush_ref_fm(%d,%d)", cell_name (cur_row, cur_col), r, c);
  return;
#endif
}

void
add_ref_to (whereto)
     int whereto;
{
  struct ref_to *ref;

  ref = my_cell->cell_refs_to;
  if (!ref)
    {
      my_cell->cell_refs_to = ref = ck_malloc (sizeof (struct ref_to) + (REF_START - 1) * sizeof (unsigned char));
      ref->refs_alloc = REF_START;
      ref->refs_used = 0;
    }
  else if (ref->refs_alloc == ref->refs_used)
    {
      ref->refs_alloc REF_INC;
      my_cell->cell_refs_to = ref = ck_realloc (ref, sizeof (struct ref_to) +
			    (ref->refs_alloc - 1) * sizeof (unsigned char));
    }
  ref->to_refs[ref->refs_used] = whereto;
  ref->refs_used++;
}

#ifdef TEST

void
dbg_print_ref_fm (rf)
     struct ref_fm *rf;
{
  int nr;
  char *bufp;

  if (rf)
    {
      io_text_line ("fm %p: alloc %u  used %u",
		    rf, rf->refs_alloc, rf->refs_used);
      for (nr = 0, bufp = print_buf; nr < rf->refs_used; nr++)
	{
	  (void) sprintf (bufp, " %s", cell_name (rf->fm_refs[nr].ref_row, rf->fm_refs[nr].ref_col));
	  if (nr % 10 == 9)
	    {
	      io_text_line (print_buf);
	      bufp = print_buf;
	    }
	  else
	    bufp += strlen (bufp);
	}
      if (nr % 10)
	io_text_line (print_buf);
    }
}

void
dbg_print_ref_to (rt, form)
     struct ref_to *rt;
     unsigned char *form;
{
  int nr;
  char *bufp;

  if (rt)
    {
      io_text_line ("to %p:  alloc %u  used %u",
		    rt, rt->refs_alloc, rt->refs_used);
      for (nr = 0, bufp = print_buf; nr < rt->refs_used; nr++)
	{
	  (void) sprintf (bufp, " %3d (%#4x)", rt->to_refs[nr], form[rt->to_refs[nr]]);
	  if (nr % 7 == 6)
	    {
	      io_text_line (print_buf);
	      bufp = print_buf;
	    }
	  else
	    bufp += strlen (bufp);
	}
      if (nr % 7)
	io_text_line (print_buf);
    }
}

void
ref_stats ()
{
  CELL *cp;

  int rf_num = 0;
  int rf_zero = 0;
  int rf_biggest = 0;

  int rt_num = 0;
  int rt_zero = 0;
  int rt_biggest = 0;

  find_cells_in_range (&all_rng);
  while (cp = next_cell_in_range ())
    {
      if (cp->cell_refs_from)
	{
	  if (cp->cell_refs_from->refs_used > rf_biggest)
	    rf_biggest = cp->cell_refs_from->refs_used;
	  if (cp->cell_refs_from->refs_used == 0)
	    rf_zero++;
	  rf_num++;
	}
      if (cp->cell_refs_to)
	{
	  if (cp->cell_refs_to->refs_used > rt_biggest)
	    rt_biggest = cp->cell_refs_to->refs_used;
	  if (cp->cell_refs_to->refs_used == 0)
	    rt_zero++;
	  rt_num++;
	}
    }
  io_text_line ("from: %d refs, biggest %d, zero_ref %d\n", rf_num, rf_biggest, rf_zero);
  io_text_line ("to: %d refs, biggest %d,zero_ref %d\n", rt_num, rt_biggest, rt_zero);
}

#endif

#endif

/* ------------- Routines for dealing with moving cells -------------------- */

static struct rng *shift_fm;
static int shift_ov;
static int shift_dn;

static void 
shift_var (name, v)
     char *name;
     struct var *v;
{
  int n;
  int nn;


  n = (BETWEEN (v->v_rng.hc, shift_fm->lc, shift_fm->hc) << 3)
    + (BETWEEN (v->v_rng.lc, shift_fm->lc, shift_fm->hc) << 2)
    + (BETWEEN (v->v_rng.hr, shift_fm->lr, shift_fm->hr) << 1)
    + BETWEEN (v->v_rng.lr, shift_fm->lr, shift_fm->hr);
  switch (n)
    {
    case 0:
    case 1:
    case 2:
    case 3:
    case 4:
    case 8:
    case 12:
      /* Null intersection, ignore it */
      break;

    case 5:			/* The bottom and right */
    case 6:			/* The bottom and left */
    case 9:			/* The top and right */
    case 10:			/* The top and left */
      /* The var sticks out of the range we're moving */
      /* on two sides.  what should we do? */
      io_error_msg ("'%s' can't be adjusted", v->var_name);
      break;

    case 7:			/* v->hc sticks out the right */
    case 11:			/* v->lc sticks out the left */
    case 13:			/* v->hr sticks out the bottom */
    case 14:			/* v->lr sticks out the top */
      /* It only sticks out on one side.  We can
		   (try to) adjust it */
#if 0
      io_error_msg ("'%s' sticks out; adjusted", v->var_name);
#endif
    case 15:			/* var is completely inside the range */
      if (v->var_ref_fm)
	{
	  for (nn = 0; nn < v->var_ref_fm->refs_used; nn++)
	    {
	      flush_range_ref (&(v->v_rng),
			       v->var_ref_fm->fm_refs[nn].ref_row,
			       v->var_ref_fm->fm_refs[nn].ref_col);
	    }
	}
      if (n != 7)
	v->v_rng.hc += shift_ov;
      if (n != 11)
	v->v_rng.lc += shift_ov;
      if (n != 13)
	v->v_rng.hr += shift_dn;
      if (n != 14)
	v->v_rng.lr += shift_dn;
      if (!v->var_ref_fm)
	return;

      for (n = 0; n < v->var_ref_fm->refs_used; n++)
	{
	  cur_row = v->var_ref_fm->fm_refs[n].ref_row;
	  cur_col = v->var_ref_fm->fm_refs[n].ref_col;
	  add_range_ref (&(v->v_rng));
	  if (my_cell)
	    panic ("shift_outside: my_cell lost.");
	  /* If this panic occurs, the caller should be recomputing
	   * my_cell after shift_outside returns (and this useful panic
	   * will have to be removed or my_cell set temporarily to 0).
	   */
	}
      break;
#ifdef TEST
    default:
      panic ("Bad ###%d", n);
#endif
    }
}
#define RIGHT	8
#define LEFT	4
#define BOTTOM	2
#define TOP	1

/*
 * This iterates over the region FM, preparing the cells there to be shifted
 * OV(er) and D(ow)N.
 *
 * After this, the ref_fm and ref_to lists of a cell within the region should
 * be appropriate to the location that cell will be shifted to.
 * 
 * Variables and references to variables are also shifted.
 */
void
shift_outside (fm, dn, ov)
     struct rng *fm;
     int dn;
     int ov;
{
  CELL *cp;
  CELL *fcp;
  CELL *tcp;
  int n;
  int fn;
  CELLREF rr, cc;
  CELLREF frr, fcc;
  CELLREF trr, tcc;
  unsigned char *ffp;
  unsigned char *fp;
  struct var *v;
  struct rng orng;

  char *ptr;
  unsigned long val;
  static char DEF_REF[] = "DEFREF";
  static char DEF_RNG[] = "DEFRNG";

  /* Variables and references to variables are also shifted. */
  shift_fm = fm;
  shift_dn = dn;
  shift_ov = ov;
  for_all_vars (shift_var);

  /* This stack is used to defer adjustments to references that are entirely 
   * within FM.  Intra-FM references are adjusted after references into and
   * out of FM.
   */

  if (!moving)
    moving = init_stack ();

  find_cells_in_range (fm);

  while (cp = next_row_col_in_range (&rr, &cc))
    {
      /* cp/rr/cc is a cell in FM. */

      /* First, adjust references FROM the region */
      if (cp->cell_refs_to)
	{
	  for (n = 0; n < cp->cell_refs_to->refs_used; n++)
	    {
	      fp = &(cp->cell_formula[cp->cell_refs_to->to_refs[n]]);
	      switch (*fp)
		{
		case R_CELL:
		case R_CELL | ROWREL:
		case R_CELL | COLREL:
		case R_CELL | ROWREL | COLREL:
		  /* Trr/cc/cp is the cell being referenced */
		  trr = GET_ROW (fp + 1);
		  tcc = GET_COL (fp + 1);
		  tcp = find_cell (trr, tcc);

		  /* Get rid of the backpointer to this reference. */
		  flush_ref_fm (&(tcp->cell_refs_from), rr, cc);

		  /* frr/fcc is the new address of the referenced cell.
		   * The address will change if Trr/cc happens to be 
	   	   * in the region that is moving, or if the reference
		   * is a relative reference.
		   */
		  fn = (BETWEEN (trr, fm->lr, fm->hr)
			&& BETWEEN (tcc, fm->lc, fm->hc)); 
		  frr = (fn || (((*fp) & ROWREL))) ? trr + dn : trr;
		  fcc = (fn || (((*fp) & COLREL))) ? tcc + ov : tcc;

		  PUT_ROW (fp + 1, frr); /* Adjust the formula byte-code. */
		  PUT_COL (fp + 1, fcc); /* This might even be a noop. */

		  /* Reinstall the backreference, unless the new address of the
		   * referenced cell is w/in the region being moved. (In which
		   * case, defer making the backreference).
		   */
		  if (BETWEEN(frr, fm->lr, fm->hr) && BETWEEN(fcc, fm->lc, fm->hc))
		    {
		      push_stack (moving, (VOIDSTAR) TO_MAGIC (rr + dn, cc + ov));
		      push_stack (moving, (VOIDSTAR) TO_MAGIC (frr, fcc));
		      push_stack (moving, DEF_REF);
		    }
		  else
		    {
		      tcp = find_or_make_cell (frr, fcc);
		      add_ref_fm (&(tcp->cell_refs_from), rr + dn, cc + ov);
		      cp = find_cell (rr, cc);
		    }

		  break;

		case RANGE:
		case RANGE | LRREL:
		case RANGE | HRREL:
		case RANGE | LCREL:
		case RANGE | HCREL:
		case RANGE | LRREL | HRREL:
		case RANGE | LRREL | LCREL:
		case RANGE | LRREL | HCREL:
		case RANGE | HRREL | LCREL:
		case RANGE | HRREL | HCREL:
		case RANGE | LCREL | HCREL:
		case RANGE | LRREL | LCREL | HCREL:
		case RANGE | LRREL | HRREL | LCREL:
		case RANGE | LRREL | HRREL | HCREL:
		case RANGE | HRREL | LCREL | HCREL:
		case RANGE | LRREL | HRREL | LCREL | HCREL:
		  /* orng is the range being referenced. */
		  GET_RNG (fp + 1, &orng);

		  /* Get rid of backpointers to this reference. */
		  flush_range_ref (&orng, rr, cc);

				  /* This asks -- does the referenced region
			   	   * intersect the region being moved at the: 
				   */
		  fn = ((BETWEEN (orng.hc, fm->lc, fm->hc) << 3)   /* right?  8 */
		       | (BETWEEN (orng.lc, fm->lc, fm->hc) << 2)  /* left?   4 */
		       | (BETWEEN (orng.hr, fm->lr, fm->hr) << 1)  /* bottom? 2 */
		       | BETWEEN (orng.lr, fm->lr, fm->hr));       /* top?    1 */

		  /* In this switch, a union of masks represents a conjunction 
		   * of intersections.  So, LEFT | TOP means `interects at left
		   * and top'. 
		   */
		  switch (fn)
		    {
		      /* Most of the time, the referenced region is moved only
		       * if the reference is relative.
    		       */
		    case LEFT | TOP:
		    case LEFT | BOTTOM:
		    case RIGHT | TOP:
		    case RIGHT | BOTTOM:

		      /* There used to be a warning given to the user here, but
		       * that seems silly, no? 
		       */

		    case 0:
		    case TOP:
		    case BOTTOM:
		    case TOP | BOTTOM:
		    case LEFT:
		    case RIGHT:
		    case LEFT | RIGHT:
		      if ((*fp) & LRREL)
			orng.lr += dn;
		      if ((*fp) & HRREL)
			orng.hr += dn;
		      if ((*fp) & LCREL)
			orng.lc += ov;
		      if ((*fp) & HCREL)
			orng.hc += ov;
		      break;

		      /* If the referenced range contains rows or columns that
		       * are entirely within the region being moved, then 
		       * the region is moved, shrunk or stretched.
		       */
		    case LEFT | BOTTOM | TOP:
		    case RIGHT | BOTTOM | TOP:
		    case RIGHT | LEFT | TOP:
		    case RIGHT | LEFT | BOTTOM:
		    case RIGHT | LEFT | BOTTOM | TOP:
		      if (fn != LEFT | BOTTOM | TOP)
			orng.hc += ov;
		      if (fn != RIGHT | BOTTOM | TOP)
			orng.lc += ov;
		      if (fn != RIGHT | LEFT | TOP)
			orng.hr += dn;
		      if (fn != RIGHT | LEFT | BOTTOM)
			orng.lr += dn;
		      break;
		    }
		  PUT_RNG (fp + 1, &orng); /* Patch the bytecode. */

		  push_stack (moving, (VOIDSTAR) fp);
		  push_stack (moving, (VOIDSTAR) TO_MAGIC (rr + dn, cc + ov));
		  push_stack (moving, DEF_RNG);
		  break;

		default:
		  {
		    struct function *fun;

		    if (*fp < USR1)
		      fun = &the_funs[*fp];
		    else
		      fun = &usr_funs[*fp - USR1][fp[1]];

		    if (fun->fn_comptype & C_T)
		      {
			flush_ref_fm (&timer_cells, rr, cc);
			add_ref_fm (&timer_cells, rr + dn, cc + ov);
		      }
		  }
		  break;
		}

	    }
	}

      /* Next, adjust references TO the region */
      for (n = 0; cp->cell_refs_from && n < cp->cell_refs_from->refs_used; n++)
	{
	  /* The second enclosed loop over the bytecode will fix all of the
	   * references to this cell.  This loop is here because a 
	   * refs_fm structure may contain more than one occurence of the
	   * referencing cell.  We don't want to adjust the same bytecode
	   * twice.
	   */
	  while ((n < cp->cell_refs_from->refs_used - 1)
		 && (cp->cell_refs_from->fm_refs[n].ref_row == 
		     cp->cell_refs_from->fm_refs[n + 1].ref_row)
		 && (cp->cell_refs_from->fm_refs[n].ref_col == 
		     cp->cell_refs_from->fm_refs[n + 1].ref_col))
	    ++n;

	  /* For each cell that referenced this one, look
	   * at the type of reference involved
	   */
	  frr = cp->cell_refs_from->fm_refs[n].ref_row;
	  fcc = cp->cell_refs_from->fm_refs[n].ref_col;

	  /* Unless the reference is from inside the region we're moving, in 
	   * which case, it has already been adjusted.
	   * 
	   * (This test seems unnecessary but harmless. -tl)
	   */
	  if (BETWEEN (frr, fm->lr, fm->hr) && BETWEEN (fcc, fm->lc, fm->hc))
	    continue;

	  /* Find the cell that references cp. */
	  fcp = find_cell (frr, fcc);

	  /* Search the byte-code for the reference. */
	  for (fn = 0; fcp->cell_refs_to && fn < fcp->cell_refs_to->refs_used; fn++)
	    {

	      ffp = &(fcp->cell_formula[fcp->cell_refs_to->to_refs[fn]]);
	      switch (*ffp)
		{
		case R_CELL:
		case R_CELL | ROWREL:
		case R_CELL | COLREL:
		case R_CELL | ROWREL | COLREL:

		  trr = GET_ROW (ffp + 1);
		  tcc = GET_COL (ffp + 1);

		  if (trr != rr || tcc != cc)
		    continue;

		  /* Found it!  Find the cell that fcp should be referencing now. */
		  if (!((*ffp) & ROWREL))
		    {
		      trr += dn;
		      PUT_ROW (ffp + 1, trr);
		    }
		  if (!((*ffp) & COLREL))
		    {
		      tcc += ov;
		      PUT_COL (ffp + 1, tcc);
		    }
		  else
		    /* If this is an absolute reference, it doesn't change. */
		    continue;

		  if (BETWEEN (trr, fm->lr, fm->hr) && BETWEEN (tcc, fm->lc, fm->hc))
		    {
		      push_stack (moving, (VOIDSTAR) TO_MAGIC (frr, fcc));
		      push_stack (moving, (VOIDSTAR) TO_MAGIC (trr, tcc));
		      push_stack (moving, DEF_REF);
		    }
		  else
		    {
		      cp = find_or_make_cell (trr, tcc);
		      add_ref_fm (&(cp->cell_refs_from), frr, fcc);
		    }
		  goto flushref;

		case VAR:
		  bcopy (&ffp[1], &v, sizeof (struct var *));
		  switch (v->var_flags)
		    {
		    case VAR_UNDEF:
		      break;

		    case VAR_CELL:
		      if (v->v_rng.lr == rr || v->v_rng.lc == cc)
			/* Right variable */
			continue;
		      break;

		    case VAR_RANGE:
		      if (BETWEEN (rr, v->v_rng.lr, v->v_rng.hr) && BETWEEN (cc, v->v_rng.lc, v->v_rng.hc))
			continue;
		      break;
		    }
		  break;

		case RANGE:
		case RANGE | LRREL:
		case RANGE | LRREL | LCREL:
		case RANGE | LRREL | LCREL | HCREL:
		case RANGE | LRREL | HCREL:
		case RANGE | LRREL | HRREL:
		case RANGE | LRREL | HRREL | LCREL:
		case RANGE | LRREL | HRREL | LCREL | HCREL:
		case RANGE | LRREL | HRREL | HCREL:
		case RANGE | HRREL:
		case RANGE | HRREL | LCREL:
		case RANGE | HRREL | LCREL | HCREL:
		case RANGE | HRREL | HCREL:
		case RANGE | LCREL:
		case RANGE | LCREL | HCREL:
		case RANGE | HCREL:
		  GET_RNG (ffp + 1, &orng);

		  if (!BETWEEN (rr, orng.lr, orng.hr) || !BETWEEN (cc, orng.lc, orng.hc))
		    break;

		  val = ((BETWEEN (orng.hc, fm->lc, fm->hc) << 3) /* right */
			 + (BETWEEN (orng.lc, fm->lc, fm->hc) << 2) /* left */
			 + (BETWEEN (orng.hr, fm->lr, fm->hr) << 1) /* bottom */
			 + BETWEEN (orng.lr, fm->lr, fm->hr)); /* top */

		  /* If the reference is absolute, or relative only in directions
		   * that aren't changing, there's nothing to do. 
		   */
		  if (!(*ffp == RANGE
			|| (!dn && ((*ffp) | LRREL | HRREL) == (RANGE | LRREL | HRREL))
			|| (!ov && ((*ffp) | LCREL | HCREL) == (RANGE | LCREL | HCREL))))
		    continue;

		  /* If it's a case we don't know how to adjust, there's
		   * nothing to do.  If there is no overlap, there's nothing to
		   * do.
		   */
		  if ((val != (LEFT | BOTTOM | TOP))
		      && (val != (RIGHT | BOTTOM | TOP))
		      && (val != (RIGHT | LEFT | TOP))
		      && (val != (RIGHT | LEFT | BOTTOM))
		      && (val != (RIGHT | LEFT | BOTTOM | TOP)))
		    continue;
		  
		  flush_range_ref (&orng, frr, fcc);

		  if (val != RIGHT | LEFT | BOTTOM)
		    orng.lr += dn;
		  if (val != RIGHT | LEFT | TOP)
		    orng.hr += dn;
		  if (val != RIGHT | BOTTOM | TOP)
		    orng.lc += ov;
		  if (val != LEFT | BOTTOM  | TOP)
		    orng.hc += ov;
		  PUT_RNG (ffp + 1, &orng);
		  push_stack (moving, (VOIDSTAR) ffp);
		  push_stack (moving, (VOIDSTAR) TO_MAGIC (frr, fcc));
		  push_stack (moving, DEF_RNG);
		  continue;
#ifdef TEST
		default:
		  { struct function *fun; if (*ffp < USR1) fun =
		      &the_funs[*ffp]; else if (*ffp >= SKIP) fun = 0, panic
			("SKIP? in shift_outside()"); else fun =
			  &usr_funs[*ffp][ffp[1]];
		    if ((fun->fn_comptype & C_T) == 0) io_error_msg
		      ("Unknown byte (%d) for reference_to #%d %d",
		       *ffp, fn, fcp->cell_refs_to->to_refs[fn]);
		  }
		  break; 
#endif
		}
	      
	    flushref:
	      /* If we get here, a ref from frr,fcc should be deleted. */
	      flush_ref_fm (&(cp->cell_refs_from), frr, fcc);
	    }
	}
    }

  while (ptr = pop_stack (moving))
    {
      if (ptr == DEF_REF)
	{
	  val = (unsigned long) pop_stack (moving);
	  trr = MAGIC_ROW (val);
	  tcc = MAGIC_COL (val);
	  val = (unsigned long) pop_stack (moving);
	  cp = find_or_make_cell (trr, tcc);
	  add_ref_fm (&(cp->cell_refs_from), MAGIC_ROW (val), MAGIC_COL (val));
	}
      else if (ptr == DEF_RNG)
	{
	  val = (unsigned long) pop_stack (moving);
	  cur_row = MAGIC_ROW (val);
	  cur_col = MAGIC_COL (val);
	  ffp = (unsigned char *) pop_stack (moving);

	  GET_RNG (ffp + 1, &orng);
	  add_range_ref (&orng);
	  if (my_cell)
	    panic ("shift_outside: my_cell lost.");
	  /* If this panic occurs, the caller should be recomputing
	   * my_cell after shift_outside returns (and this useful panic
	   * will have to be removed or my_cell set temporarily to 0).
	   */
	}
#ifdef TEST
      else
	panic ("Now what (%p)?", ptr);
#endif
    }
  /* flush_stack(moving); */
}

/* The formula in cell my_cell has moved by DN down and OV over, adjust
   everything so it'll still work */
#ifdef __STDC__
void
shift_formula (int r, int c, int dn, int ov)
#else
void
shift_formula (r, c, dn, ov)
     int r;			/* Address of my_cell. */
     int c;
     int dn;
     int ov;
#endif
{
  int n;
  unsigned char *fp;

  for (n = 0; n < my_cell->cell_refs_to->refs_used; n++)
    {
      fp = &(my_cell->cell_formula[my_cell->cell_refs_to->to_refs[n]]);
      switch (*fp)
	{
	case F_ROW:
	case F_COL:
	  push_cell (cur_row, cur_col);
	  break;

	case R_CELL:
	case R_CELL | ROWREL:
	case R_CELL | COLREL:
	case R_CELL | ROWREL | COLREL:
	  {
	    CELLREF trr, tcc;
	    CELL *tcp;

	    /* These are more difficult */
	    trr = GET_ROW (fp + 1);
	    tcc = GET_COL (fp + 1);
	    tcp = find_cell (trr, tcc);
#ifdef TEST
	    if (!tcp)
	      panic ("Can't find_cell(%s) in shift_formula", cell_name (trr, tcc));
#endif
	    flush_ref_fm (&(tcp->cell_refs_from), cur_row - dn, cur_col - ov);

	    if (((*fp) & ROWREL) && dn)
	      {
		trr += dn;
		PUT_ROW (fp + 1, trr);
	      }
	    if (((*fp) & COLREL) && ov)
	      {
		tcc += ov;
		PUT_COL (fp + 1, tcc);
	      }
	    tcp = find_or_make_cell (trr, tcc);
	    add_ref_fm (&(tcp->cell_refs_from), cur_row, cur_col);
	  }
	  break;

	case RANGE:
	case RANGE | LRREL:
	case RANGE | LRREL | LCREL:
	case RANGE | LRREL | LCREL | HCREL:
	case RANGE | LRREL | HCREL:
	case RANGE | LRREL | HRREL:
	case RANGE | LRREL | HRREL | LCREL:
	case RANGE | LRREL | HRREL | LCREL | HCREL:
	case RANGE | LRREL | HRREL | HCREL:
	case RANGE | HRREL:
	case RANGE | HRREL | LCREL:
	case RANGE | HRREL | LCREL | HCREL:
	case RANGE | HRREL | HCREL:
	case RANGE | LCREL:
	case RANGE | LCREL | HCREL:
	case RANGE | HCREL:
	  {
	    struct rng orng;
	    GET_RNG (fp + 1, &orng);

	    flush_range_ref (&orng, cur_row - dn, cur_col - ov);

	    if ((*fp) & LRREL)
	      orng.lr += dn;
	    if ((*fp) & HRREL)
	      orng.hr += dn;
	    if ((*fp) & LCREL)
	      orng.lc += ov;
	    if ((*fp) & HCREL)
	      orng.hc += ov;
	    PUT_RNG (fp + 1, &orng);
	    add_range_ref (&orng);
	    /* sparse array bug fixed here */
	    my_cell = find_cell (r, c);
	  }
	  break;

	case VAR:
	  {
	    struct var *v;
	    struct cell *tcp;

	    bcopy (&fp[1], &v, sizeof (struct var *));
	    flush_ref_fm (&(v->var_ref_fm), cur_row - dn, cur_col - ov);
	    add_ref_fm (&(v->var_ref_fm), cur_row, cur_col);
	    switch (v->var_flags)
	      {
	      case VAR_UNDEF:
		break;

	      case VAR_CELL:
		tcp = find_cell (v->v_rng.lr, v->v_rng.lc);
#ifdef TEST
		if (!tcp)
		  panic ("Can't find_cell(%s) in shift_formula", cell_name (v->v_rng.lr, v->v_rng.lc));
#endif
		flush_ref_fm (&(tcp->cell_refs_from), cur_row - dn, cur_col - ov);
		add_ref_fm (&(tcp->cell_refs_from), cur_row, cur_col);
		break;

	      case VAR_RANGE:
		flush_range_ref (&(v->v_rng), cur_row - dn, cur_col - ov);
		add_range_ref (&(v->v_rng));
		/* sparse array bug fixed here */
		my_cell = find_cell (r, c);
		break;

#ifdef TEST
	      default:
		panic ("Unknown var type %d", v->var_flags);
#endif
	      }
	  }
	  break;

	default:
	  {
	    struct function *fun;

	    if (*fp < USR1)
	      fun = &the_funs[*fp];
#ifdef TEST
	    else if (*fp >= SKIP)
	      fun = 0, panic ("SKIP? in shift_formula?");
#endif
	    else
	      fun = &usr_funs[*fp][fp[1]];
	    /* These are easy */
	    if (fun->fn_comptype & C_T)
	      {
		flush_ref_fm (&timer_cells, cur_row - dn, cur_col - ov);
		add_ref_fm (&timer_cells, cur_row, cur_col);
	      }
#ifdef TEST
	    else
	      panic ("How do I deal with byte %d in shift_formula()?", *fp);
#endif
	  }
	  break;
	}
    }
}


/* ---------------- Routines for dealing with async functions -------------- */


/* This function is called when the alarm has gone off (but not from inside
 * the signal handler!). It schedules timer_cells->fm_refs for recalc. 
 */
#ifdef __STDC__
void
deal_alarm (void)
#else
void
deal_alarm ()
#endif
{
  int n;
  static time_t last_time = 0;
  if (alarm_active)
    {
      time_t this_time = time(0);
      if ((this_time - last_time) < alarm_seconds)
	return;
      last_time = this_time;
      current_cycle++;
      for (n = 0; n < timer_cells->refs_used; n++)
	push_cell (timer_cells->fm_refs[n].ref_row,
		   timer_cells->fm_refs[n].ref_col);
    }
}

/* All the timer_cells are going away, 'cuz everything is going away. . . */
void
flush_all_timers ()
{
  if (timer_active)
    {
#ifdef SPLIT_REFS
      timer_cells->refs_used = 0;
#else
      flush_fm_ref (timer_cells);
      timer_cells = 0;
#endif
      timer_active = 0;
      alarm_active = 0;
    }
}

/* Add CUR_ROW, CUR_COL to the list of active timer-cells, turning on
   the timer_active, if it isn't already */
void
add_timer_ref (whereto)
     int whereto;
{
  add_ref_to (whereto);
  add_ref_fm (&timer_cells, cur_row, cur_col);
  if (!timer_active)
    {
      timer_active++;
      alarm_active = 1;
    }
}

/* ---------- Routines and vars for dealing with the eval FIFO ------------ */
static struct cell_buf cell_buffer;

/* Start up the FIFO of cells to update */
void
init_refs ()
{
  cell_buffer.size = FIFO_START;
  cell_buffer.buf = (struct pos *) ck_malloc (cell_buffer.size * sizeof (struct pos));
  bzero (cell_buffer.buf, cell_buffer.size * sizeof (struct pos));
  cell_buffer.push_to_here = cell_buffer.buf;
  cell_buffer.pop_frm_here = cell_buffer.buf;
  the_vars = hash_new ();
}

/* Push the cells in REF onto the FIFO.  This calls push_cell to do the
   actual work. . . */
void
push_refs (ref)
     struct ref_fm *ref;
{
  int n;

  if (!ref || !ref->refs_used)
    return;
  n = ref->refs_used;
  while (n--)
    {
#ifdef TEST
      CELL *cp;

      if (debug & 04)
	io_error_msg ("Push %s", cell_name (ref->fm_refs[n].ref_row, ref->fm_refs[n].ref_col));
      cp = find_cell (ref->fm_refs[n].ref_row, ref->fm_refs[n].ref_col);
      if (cp->cell_cycle == current_cycle)
	{
	  if (debug & 01)
	    io_error_msg ("Cycle detected from %s to %s",
			  cell_name (cur_row, cur_col),
	      cell_name (ref->fm_refs[n].ref_row, ref->fm_refs[n].ref_col));
	  push_cell (ref->fm_refs[n].ref_row,
		     ref->fm_refs[n].ref_col);
	}
      else
#endif
	push_cell (ref->fm_refs[n].ref_row, ref->fm_refs[n].ref_col);
    }
}

/* Push a cell onto the FIFO of cells to evaluate, checking for cells
   that are already on the FIFO, etc.

   This does not implement best-order recalculation, since there may be
   intersecting branches in the dependency tree, however, it's close enough
   for most people.
 */
#if __STDC__
void
push_cell (CELLREF row, CELLREF col)
#else
void
push_cell (row, col)
     CELLREF row;
     CELLREF col;
#endif
{
  struct pos *dup;
  CELL *cp;
  struct ref_fm *rf;

  if (cell_buffer.push_to_here + 1 == cell_buffer.pop_frm_here || (cell_buffer.pop_frm_here == cell_buffer.buf && cell_buffer.push_to_here == cell_buffer.buf + (cell_buffer.size - 1)))
    {
      int f, t, from_num;

      f = cell_buffer.pop_frm_here - cell_buffer.buf;
      t = cell_buffer.push_to_here - cell_buffer.buf;
      from_num = cell_buffer.size - f;

      cell_buffer.size FIFO_INC;
      cell_buffer.buf = (struct pos *) ck_realloc ((VOIDSTAR) cell_buffer.buf, cell_buffer.size * sizeof (struct pos));
      if (t == 0)
	{
	  cell_buffer.push_to_here = cell_buffer.buf + f + from_num;
	  cell_buffer.pop_frm_here = cell_buffer.buf + f;
	}
      else if (t > f)
	{
	  cell_buffer.push_to_here = cell_buffer.buf + t;
	  cell_buffer.pop_frm_here = cell_buffer.buf + f;
	}
      else
	{
	  cell_buffer.push_to_here = cell_buffer.buf + t;
	  cell_buffer.pop_frm_here = cell_buffer.buf + (cell_buffer.size - from_num);
	  if (from_num)
	    bcopy (cell_buffer.buf + f,
		   cell_buffer.pop_frm_here,
		   from_num * sizeof (struct pos));
	}
    }

#if 1
  if (cell_buffer.pop_frm_here != cell_buffer.push_to_here)
    {
      dup = cell_buffer.pop_frm_here;

      cp = find_cell (row, col);
      if (!cp)
	{
	  return;
	}
      rf = cp->cell_refs_from;
      for (; dup != cell_buffer.push_to_here;)
	{
	  if (dup->row == row && dup->col == col)
	    {
#ifdef TEST
	      if (debug & 010)
		io_error_msg ("Flushed dup ref to %s", cell_name (row, col));
#endif
	      *dup = *(cell_buffer.pop_frm_here);
	      cell_buffer.pop_frm_here++;
	      if (cell_buffer.pop_frm_here == cell_buffer.buf + cell_buffer.size)
		cell_buffer.pop_frm_here = cell_buffer.buf;
	      break;
	    }
#if 0
	  if (rf)
	    {
	      for (n = 0; n < rf->refs_used; n++)
		if (rf->fm_refs[n].ref_row == dup->row && rf->fm_refs[n].ref_col == dup->col)
		  {
#ifdef TEST
		    if (debug & 01)
		      io_error_msg ("Swapped %s and %s", cell_name (row, col), cell_name (dup->row, dup->col));
#endif
		    dup->row = row;
		    dup->col = col;
		    row = rf->fm_refs[n].ref_row;
		    col - rf->fm_refs[n].ref_col;
		    goto breakout;
		  }
	    }
#endif

	  if (++dup == cell_buffer.buf + cell_buffer.size)
	    dup = cell_buffer.buf;
	}
    }
#endif

  cell_buffer.push_to_here->row = row;
  cell_buffer.push_to_here->col = col;
  cell_buffer.push_to_here++;
  if (cell_buffer.push_to_here == cell_buffer.buf + cell_buffer.size)
    cell_buffer.push_to_here = cell_buffer.buf;
}

/* Pop a cell off CELL_BUFFER, and evaluate it, displaying the result. . .
   This returns 0 if there are no more cells to update, or if it gets
   an error. */

int
eval_next_cell ()
{
  CELL *cp;
  static loop_counter = 40;

  if (cell_buffer.pop_frm_here == cell_buffer.push_to_here)
    return 0;

  cur_row = cell_buffer.pop_frm_here->row;
  cur_col = cell_buffer.pop_frm_here->col;
  cell_buffer.pop_frm_here++;
  if (cell_buffer.pop_frm_here == cell_buffer.buf + cell_buffer.size)
    cell_buffer.pop_frm_here = cell_buffer.buf;

  cp = find_cell (cur_row, cur_col);
  if (cp)
    {
      if (cp->cell_cycle == current_cycle)
	--loop_counter;
      else
	loop_counter = 40;
      update_cell (cp);
      io_pr_cell (cur_row, cur_col, cp);
      return loop_counter;
    }
  else
    return 0;
}

#ifdef TEST
void
cell_buffer_contents ()
{
  struct pos *ptr;

  if (cell_buffer.pop_frm_here != cell_buffer.push_to_here)
    {
      ptr = cell_buffer.pop_frm_here;
      for (;;)
	{
	  printf ("Ref to %s\r\n", cell_name (ptr->row, ptr->col));
	  if (++ptr == cell_buffer.buf + cell_buffer.size)
	    ptr = cell_buffer.buf;
	  if (ptr == cell_buffer.push_to_here)
	    break;
	}
    }
  printf ("End of buffer\r\n");
}

#endif

/* ----------------- Routines for dealing with variables ------------------ */

/* This sets the variable V_NAME to V_NEWVAL
   It returns error msg, or 0 on success.
   All the appropriate cells have their ref_fm arrays adjusted appropriatly
   This could be smarter; when changing a range var, only the cells that
   were in the old value but not in the new one need their references flushed,
   and only the cells that are new need references added.
   This might also be changed to use add_range_ref()?
 */
char *
new_var_value (v_name, v_namelen, v_newval)
     char *v_name;
     int v_namelen;
     char *v_newval;
{
  struct var *var;
  int n;
  int newflag;
  struct rng tmp_rng;

  cur_row = MIN_ROW;
  cur_col = MIN_COL;
  if (v_newval && *v_newval)
    {
      n = parse_cell_or_range (&v_newval, &tmp_rng);
      if (!n)
	return "Can't parse cell or range";
      if (*v_newval)
	return "Junk after cell or range";
      newflag = ((n | ROWREL | COLREL) == (R_CELL | ROWREL | COLREL)) ? VAR_CELL : VAR_RANGE;
    }
  else
    {
      tmp_rng.lr = tmp_rng.hr = NON_ROW;
      tmp_rng.lc = tmp_rng.hc = NON_COL;
      newflag = VAR_UNDEF;
    }

  var = find_or_make_var (v_name, v_namelen);

  if (var->var_ref_fm)
    {
      if (var->var_flags != VAR_UNDEF)
	{
	  for (n = 0; n < var->var_ref_fm->refs_used; n++)
	    {
	      flush_range_ref (&(var->v_rng),
			       var->var_ref_fm->fm_refs[n].ref_row,
			       var->var_ref_fm->fm_refs[n].ref_col);
	    }
	}
      var->v_rng = tmp_rng;

      if (var->v_rng.lr != NON_ROW)
	{
	  for (n = 0; n < var->var_ref_fm->refs_used; n++)
	    {
	      cur_row = var->var_ref_fm->fm_refs[n].ref_row;
	      cur_col = var->var_ref_fm->fm_refs[n].ref_col;
	      add_range_ref (&(var->v_rng));
	    }
	}
      for (n = 0; n < var->var_ref_fm->refs_used; n++)
	push_cell (var->var_ref_fm->fm_refs[n].ref_row,
		   var->var_ref_fm->fm_refs[n].ref_col);
    }
  else
    var->v_rng = tmp_rng;

  var->var_flags = newflag;

  return 0;
}

void
#ifdef __STDC__
for_all_vars (void (*func) (char *, struct var *))
#else
for_all_vars (func)
     void (*func) ();
#endif
{
  hash_apply (the_vars, func);
}

/* Find a variable in the list of variables, or create it if it doesn't
   exist.  Takes a name and a length so the name doesn't have to be
   null-terminated
 */
struct var *
find_or_make_var (string, len)
     char *string;
     int len;
{
  struct var *ret;
  int ch;

  ch = string[len];
  string[len] = '\0';

  ret = (struct var *) hash_find (the_vars, string);
  if (ret)
    {
      string[len] = ch;
      return ret;
    }

  ret = (struct var *) ck_malloc (sizeof (struct var) + len);
  bcopy (string, ret->var_name, len + 1);
  ret->var_flags = VAR_UNDEF;
  ret->v_rng.lr = 0;
  ret->v_rng.lc = 0;
  ret->v_rng.hr = 0;
  ret->v_rng.hc = 0;
  ret->var_ref_fm = 0;
  hash_insert (the_vars, ret->var_name, ret);
  string[len] = ch;
  return ret;
}

/* Like find-or-make-var except returns 0 if it doesn't exist */
struct var *
find_var (string, len)
     char *string;
     int len;
{
  int ch;
  struct var *ret;

  ch = string[len];
  string[len] = '\0';
  ret = (struct var *) hash_find (the_vars, string);
  string[len] = ch;
  return ret;
}

/* This adds a reference from CUR_ROW,CUR_COL to the variable VAR
   It calls add_ref or add_range_ref to have the cell(s) in VAR be
   referenced by CUR_ROW,CUR_COL
 */
void
add_var_ref (vvar)
     void * vvar;
{
  struct var *var = (struct var *)vvar;
  add_ref_fm (&(var->var_ref_fm), cur_row, cur_col);
  switch (var->var_flags)
    {
    case VAR_UNDEF:
      break;
    case VAR_CELL:
      add_ref (var->v_rng.lr, var->v_rng.lc);
      break;
    case VAR_RANGE:
      add_range_ref (&(var->v_rng));
      break;
#ifdef TEST
    default:
      panic ("Unknown var type %d in add_var_ref", var->var_flags);
#endif
    }
}

static void
flush_var (name, var)
     char *name;
     struct var *var;
{
#ifdef SPLIT_REFS
  if (var->var_ref_fm)
    free (var->var_ref_fm);
#endif
  free (var);
}


/* Free up all the variables, and (if SPLIT_REFS) the ref_fm structure
   associated with each variable.  Note that this does not get rid of
   the struct var *s in cell expressions, so it can only be used when all
   the cells are being freed also
 */
#ifdef __STDC__
void
flush_variables (void)
#else
void
flush_variables ()
#endif
{
  for_all_vars (flush_var);
  hash_die (the_vars);
  the_vars = hash_new ();
}
