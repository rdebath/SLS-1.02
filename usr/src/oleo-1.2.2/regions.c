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
#include "sysdef.h"
#include "global.h"
#include "cell.h"
#include "io-generic.h"
#include "io-abstract.h"
#include "io-utils.h"
#include "lists.h"
#include "ref.h"
#include "regions.h"
#include "io-term.h"

extern CELL *my_cell;
extern int modified;
extern CELLREF cur_row, cur_col;


struct rng all_rng =
{MIN_ROW, MIN_COL, MAX_ROW, MAX_COL};

/* Take a struct rng (R) and init its elements to R1 C1 R2 C2, making sure
   they are put in in the right order.
 */
#if __STDC__
void
set_rng (struct rng *r, CELLREF r1, CELLREF c1, CELLREF r2, CELLREF c2)
#else
void
set_rng (r, r1, c1, r2, c2)
     struct rng *r;
     CELLREF r1;
     CELLREF c1;
     CELLREF r2;
     CELLREF c2;
#endif
{
  if (r1 <= r2)
    {
      r->lr = r1;
      r->hr = r2;
    }
  else
    {
      r->lr = r2;
      r->hr = r1;
    }
  if (c1 <= c2)
    {
      r->lc = c1;
      r->hc = c2;
    }
  else
    {
      r->lc = c2;
      r->hc = c1;
    }
}

/* Flush all the cells in a region */
void
delete_region (where)
     struct rng *where;
{
  CELLREF rr, cc;
  CELL *pp;

  modified = 1;

  find_cells_in_range (where);
  while (pp = next_row_col_in_range (&rr, &cc))
    {
      if (!pp->cell_formula && !GET_TYP (pp))
	{
	  pp->cell_flags = 0;
	  pp->cell_font = 0;
	  continue;
	}
      cur_row = rr;
      cur_col = cc;
      my_cell = pp;
      flush_old_value ();
      pp->cell_formula = 0;
      pp->cell_flags = 0;
      pp->cell_font = 0;
      push_refs (pp->cell_refs_from);
      io_pr_cell (rr, cc, pp);
    }
  my_cell = 0;
}

/* Turn on/off the locked bits in a region */
void
lock_region (where, locked)
     struct rng *where;
     int locked;
{
  CELL *cp;

  modified = 1;
  make_cells_in_range (where);
  while (cp = next_cell_in_range ())
    SET_LCK (cp, locked);
}

void
format_region (where, fmt, just)
     struct rng *where;
     int fmt;
     int just;
{
  CELL *cp;
  CELLREF rr, cc;

  modified = 1;
  make_cells_in_range (where);
  while (cp = next_row_col_in_range (&rr, &cc))
    {
      if (fmt != -1)
	SET_FMT (cp, fmt);
      if (just != -1)
	SET_JST (cp, just);
      io_pr_cell (rr, cc, cp);
    }
}

unsigned short print_width;

void
print_region (fp, print)
     FILE *fp;
     struct rng *print;
{
  CELLREF rr, cc;
  CELL *cp;
  char *ptr;
  int w;
  int j;
  int lenstr;
  int spaces;
  CELLREF c_lo, c_hi;

  extern int default_jst, default_fmt;

  for (c_lo = print->lc, c_hi = 0; c_hi != print->hc; c_lo = c_hi + 1)
    {
      w = 0;
      for (w = get_width (cc = c_lo); w <= print_width && cc <= print->hc; w += get_width (++cc))
	;
      if (cc != c_lo)
	--cc;
      c_hi = cc;

      for (rr = print->lr; rr <= print->hr; rr++)
	{
	  spaces = 0;
	  for (cc = c_lo; cc <= c_hi; cc++)
	    {
	      w = get_width (cc);
	      if (!w)
		continue;
	      cp = find_cell (rr, cc);
	      if (!cp || !GET_TYP (cp))
		{
		  spaces += w;
		  continue;
		}
	      ptr = print_cell (cp);
	      lenstr = strlen (ptr);
	      if (lenstr == 0)
		{
		  spaces += w;
		  continue;
		}
	      if (spaces)
		{
		  fprintf (fp, "%*s", spaces, "");
		  spaces = 0;
		}
	      j = GET_JST (cp);
	      if (j == JST_DEF)
		j = default_jst;
	      if (lenstr <= w - 1)
		{
		  if (j == JST_LFT)
		    {
		      fprintf (fp, "%s", ptr);
		      spaces = w - lenstr;
		    }
		  else if (j == JST_RGT)
		    {
		      fprintf (fp, "%*s", w - 1, ptr);
		      spaces = 1;
		    }
		  else if (j == JST_CNT)
		    {
		      w = (w - 1) - lenstr;
		      fprintf (fp, "%*s", w / 2 + lenstr, ptr);
		      spaces = (w + 3) / 2;
		    }
#ifdef TEST
		  else
		    {
		      panic ("What just %d", j);
		    }
#endif
		}
	      else
		{
		  CELLREF ccc = cc;
		  CELL *ccp;
		  int tmp_wid;
		  unsigned short ww;

		  for (ww = w;; tmp_wid = get_width (ccc), w += tmp_wid, spaces -= tmp_wid)
		    {
		      if (lenstr < w - 1)
			break;
		      if (++ccc > c_hi)
			break;
		      ccp = find_cell (rr, ccc);
		      if (!ccp || !GET_TYP (ccp) || GET_FMT (ccp) == FMT_HID)
			continue;
		      if (GET_FMT (ccp) == FMT_DEF && default_fmt == FMT_HID)
			continue;
		      break;
		    }
		  if (lenstr > w - 1)
		    {
		      if (GET_TYP (cp) == TYP_FLT)
			{
			  ptr = adjust_prc (ptr, cp, w - 1, ww - 1, j);
			  lenstr = strlen (ptr);
			}
		      else if (GET_TYP (cp) == TYP_INT)
			{
			  ptr = numb_oflo;
			  lenstr = 80;
			}
		      fprintf (fp, "%.*s", w - 1, ptr);
		      if (lenstr < w)
			spaces += w - lenstr;
		      else
			spaces++;
		    }
		  else
		    {
		      fprintf (fp, "%s", ptr);
		      spaces += w - lenstr;
		    }
		}
	    }
	  (void) putc ('\n', fp);
	}
    }
}


/*
   Set up regions for the move/copy functions.  This deals with default
   sizing of the target region, regions that don't fit, etc.

   This returns
	-1 if the regions overlap
	0 if there is a *real* error
	1 if the target is a multiple of the source
	2 if everything is OK.
 */

static int
set_to_region (fm, to)
     struct rng *fm;
     struct rng *to;
{
  /* Delta {row,col} {from,to} */
  int drf, dcf;
  int drt, dct;
  int ret = 2;

  drf = fm->hr - fm->lr;
  drt = to->hr - to->lr;
  if (drt == 0)
    {
      if (to->lr > MAX_ROW - drf)
	{
	  io_error_msg ("The range won't fit this far down!");
	  return 0;
	}
      to->hr = to->lr + drf;
    }
  else if (drf != drt)
    {
      if ((drt + 1) % (drf + 1) == 0)
	ret = 1;
      else
	{
	  io_error_msg ("Rows %u:%u and %u:%u don't fit", fm->lr, fm->hr, to->lr, to->hr);
	  return 0;
	}
    }
  dcf = fm->hc - fm->lc;
  dct = to->hc - to->lc;
  if (dct == 0)
    {
      if (to->lc > MAX_COL - dcf)
	{
	  io_error_msg ("The range won't fit this far over!");
	  return 0;
	}
      to->hc = to->lc + dcf;
    }
  else if (dcf != dct)
    {
      if ((dct + 1) % (dcf + 1) == 0)
	ret = 1;
      else
	{
	  io_error_msg ("Cols %u:%u and %u:%u don't fit", fm->lc, fm->hc, to->lc, to->hc);
	  return 0;
	}
    }

  if (fm->lr == to->lr && fm->lc == to->lc)
    {
      io_error_msg ("Regions are in the same place");
      return 0;
    }

  if (((fm->lr <= to->lr && to->lr <= fm->hr) || (fm->lr <= to->hr && to->hr <= fm->hr))
      && ((fm->lc <= to->lc && to->lc <= fm->hc) || (fm->lc <= to->hc && to->hc <= fm->hc)))
    return -1;
  modified = 1;
  return ret;
}

/* This is only complicated because it must deal with overlap, and it wants
   to be smart about copying empty space. . .
 */
void
move_region (fm, to)
     struct rng *fm;
     struct rng *to;
{
  /* Delta {row,col} */
  int dr, dc;
  int nr, nc;
  int ov, dn;
  struct rng del_to_1, del_to_2;
  int do_2, dirs[2];
  int maxr, maxc;
  CELLREF cmax, rmax;
  int cdmax, rdmax;
  int ret = 0;

  switch (set_to_region (fm, to))
    {
    case 0:
      return;

    case 1:
      io_error_msg ("Can't move source to multiple targets");
      return;

    case 2:
      del_to_1 = *to;

      do_2 = 0;
      dirs[0] = 1;
      dirs[1] = 1;

      /* del_fm_1= *fm; */
      break;

    default:
      /* They overlap.  There are eight ways that
		   they can overlap.  */
      if (to->lc == fm->lc && to->lr < fm->lr)
	{
	  /* State 1:  'from' on bottom */
	  del_to_1.lr = to->lr;
	  del_to_1.lc = to->lc;
	  del_to_1.hr = fm->lr - 1;
	  del_to_1.hc = to->hc;

	  do_2 = 0;
	  dirs[0] = 1;
	  dirs[1] = 1;

	  /* del_fm_1.lr=to->hr+1;	del_fm_1.lc=fm->lc;
			del_fm_1.hr=fm->hr;	del_fm_1.hc=fm->hc; */
	}
      else if (to->lc == fm->lc)
	{
	  /* State 2: 'from' on top */
	  del_to_1.lr = fm->hr + 1;
	  del_to_1.lc = to->lc;
	  del_to_1.hr = to->hr;
	  del_to_1.hc = to->hc;

	  do_2 = 0;
	  dirs[0] = -1;
	  dirs[1] = 1;

	  /* del_fm_1.lr=fm->lr;	del_fm_1.lc=fm->lc;
			del_fm_1.hr=to->lr-1;	del_fm_1.hc=fm->hc; */
	}
      else if (to->lr == fm->lr && to->lc < fm->lc)
	{
	  /* State 3: 'from' on right */
	  del_to_1.lr = to->lr;
	  del_to_1.lc = to->lc;
	  del_to_1.hr = to->hr;
	  del_to_1.hc = fm->lc - 1;

	  do_2 = 0;
	  dirs[0] = 1;
	  dirs[1] = 1;

	  /* del_fm_1.lr=fm->lr;	del_fm_1.lc=to->hc+1;
			del_fm_1.hr=fm->hr;	del_fm_1.hc=fm->hc; */
	}
      else if (to->lr == fm->lr)
	{
	  /* State 4: 'from' on left */
	  del_to_1.lr = to->lr;
	  del_to_1.lc = fm->hc + 1;
	  del_to_1.hr = to->hr;
	  del_to_1.hc = to->hc;

	  do_2 = 0;
	  dirs[0] = 1;
	  dirs[1] = -1;

	  /* del_fm_1.lr=fm->lr;	del_fm_1.lc=fm->lc;
			del_fm_1.hr=fm->hr;	del_fm_1.hc=to->lc-1; */
	}
      else if (fm->lr < to->lr && fm->lc < to->lc)
	{
	  /* State 5: From on topleft */

	  del_to_1.lr = to->lr;
	  del_to_1.lc = fm->hc + 1;
	  del_to_1.hr = fm->hr;
	  del_to_1.hc = to->hc;

	  del_to_2.lr = fm->hr + 1;
	  del_to_2.lc = to->lc;
	  del_to_2.hr = to->hr;
	  del_to_2.hc = to->hc;

	  do_2 = 1;
	  dirs[0] = -1;
	  dirs[1] = -1;

	  /* del_fm_1.lr=fm->lr;	del_fm_1.lc=fm->lc;
			del_fm_1.hr=to->lr-1;	del_fm_1.hc=fm->hc;

			del_fm_2.lr=to->lr;	del_fm_2.lc=fm->lc;
			del_fm_2.hr=fm->hr;	del_fm_2.hc=to->lc-1; */
	}
      else if (fm->lr < to->lr)
	{
	  /* State 6: 'from' on topright */
	  del_to_1.lr = to->lr;
	  del_to_1.lc = to->lc;
	  del_to_1.hr = fm->hr;
	  del_to_1.hc = fm->lc - 1;

	  del_to_2.lr = fm->hr + 1;
	  del_to_2.lc = to->lc;
	  del_to_2.hr = to->hr;
	  del_to_2.hc = to->hc;

	  do_2 = 1;
	  dirs[0] = -1;
	  dirs[1] = 1;

	  /* del_fm_1.lr=fm->lr;	del_fm_1.lc=fm->lc;
			del_fm_1.hr=to->lr-1;	del_fm_1.hc=fm->hc;

			del_fm_2.lr=to->lr;	del_fm_2.lc=to->hc+1;
			del_fm_2.hr=fm->hr;	del_fm_2.hc=fm->hc; */
	}
      else if (fm->lc < to->lc)
	{
	  /* State 7: 'from on bottomleft */
	  del_to_1.lr = to->lr;
	  del_to_1.lc = to->lc;
	  del_to_1.hr = fm->lr - 1;
	  del_to_1.hc = to->hc;

	  del_to_2.lr = fm->lr;
	  del_to_2.lc = fm->hc;
	  del_to_2.hr = to->hr;
	  del_to_2.hc = to->hc;

	  do_2 = 1;
	  dirs[0] = 1;
	  dirs[1] = -1;

	  /* del_fm_1.lr=fm->lr;	del_fm_1.lc=fm->lc;
			del_fm_1.hr=to->hr;	del_fm_1.hc=to->lc-1;

			del_fm_2.lr=to->hr+1;	del_fm_2.lc=fm->lc;
			del_fm_2.hr=to->hr+1;	del_fm_2.hc=to->lc-1; */
	}
      else
	{
	  /* State 8: 'from' on bottomright */
	  del_to_1.lr = to->lr;
	  del_to_1.lc = to->lc;
	  del_to_1.hr = fm->lr - 1;
	  del_to_1.hc = to->hc;

	  del_to_2.lr = fm->lr;
	  del_to_2.lc = to->lc;
	  del_to_2.hr = to->hr;
	  del_to_2.hc = fm->lc - 1;

	  do_2 = 1;
	  dirs[0] = 1;
	  dirs[1] = 1;

	  /* del_fm_1.lr=fm->lr;	del_fm_1.lc=to->hc+1;
			del_fm_1.hr=to->hr;	del_fm_1.hc=fm->hc;

			del_fm_2.lr=to->hr+1;	del_fm_2.lc=fm->lc;
			del_fm_2.hr=fm->hr;	del_fm_2.hc=fm->hc; */
	}
    }
  dn = to->hr - fm->hr;
  ov = to->hc - fm->hc;

  dr = fm->hr - fm->lr;
  dc = fm->hc - fm->lc;

  delete_region (&del_to_1);
  if (do_2)
    delete_region (&del_to_2);

  if (to->lr == MIN_ROW && to->hr == MAX_ROW)
    {
      shift_widths (ov, fm->lc, fm->hc);
      ret = 1;
    }

  shift_outside (fm, dn, ov);

  rmax = highest_row ();
  if (rmax < fm->lr)
    rdmax = -1;
  else if (rmax > fm->hr)
    rdmax = dr;
  else
    rdmax = rmax - fm->lr;
  nr = (dirs[0] > 0) ? 0 : rdmax;
  maxr = (dirs[0] > 0) ? rdmax + 1 : -1;
  for (; nr != maxr; nr += dirs[0])
    {
      cmax = max_col (fm->lr + nr);
      if (cmax < fm->lc)
	cdmax = -1;
      else if (cmax > fm->hc)
	cdmax = dc;
      else
	{
	  cdmax = cmax - fm->lc;
	}
      nc = (dirs[1] > 0) ? 0 : cdmax;
      maxc = (dirs[1] > 0) ? cdmax + 1 : -1;
      for (; nc != maxc; nc += dirs[1])
	{
	  CELLREF rf, cf, rt, ct;
	  CELL *cpf;

	  rf = fm->lr + nr;
	  cf = fm->lc + nc;
	  rt = to->lr + nr;
	  ct = to->lc + nc;

	  cpf = find_cell (rf, cf);
	  cur_row = rt;
	  cur_col = ct;
	  my_cell = find_cell (cur_row, cur_col);
	  if ((!cpf
	       || (!cpf->cell_font && !cpf->cell_flags && !cpf->cell_formula))
	      && !my_cell)
	    continue;

	  if (!cpf)
	    {
	      my_cell->cell_flags = 0;
	      my_cell->cell_font = 0;
	      my_cell->cell_refs_to = 0;
	      my_cell->cell_formula = 0;
	      my_cell->cell_cycle = 0;
	      my_cell = 0;
	      continue;
	    }
	  if (!my_cell)
	    {
	      my_cell = find_or_make_cell (cur_row, cur_col);
	      cpf = find_cell (rf, cf);
	    }
	  else
	    flush_old_value ();

	  my_cell->cell_flags = cpf->cell_flags;
	  my_cell->cell_font = cpf->cell_font;
	  my_cell->cell_refs_to = cpf->cell_refs_to;
	  my_cell->cell_formula = cpf->cell_formula;
	  my_cell->cell_cycle = cpf->cell_cycle;
	  my_cell->c_z = cpf->c_z;

	  cpf->cell_flags = 0;
	  cpf->cell_font = 0;
	  cpf->cell_refs_to = 0;
	  cpf->cell_formula = 0;
	  cpf->cell_cycle = 0;

	  push_cell (cur_row, cur_col);

	  if (!ret)
	    {
	      if (cpf)
		io_pr_cell (rf, cf, cpf);
	      if (my_cell)
		io_pr_cell (rt, ct, my_cell);
	    }
	  my_cell = 0;
	}
    }
  return;
}

void
copy_region (fm, to)
     struct rng *fm;
     struct rng *to;
{
  CELLREF rf, rt, cf, ct;

  if (set_to_region (fm, to) < 1)
    return;

  for (rf = fm->lr, rt = to->lr; (rt > 0) && (rt <= to->hr); rt++, rf++)
    {
      for (cf = fm->lc, ct = to->lc; (ct > 0) && (ct <= to->hc); ct++, cf++)
	{
	  copy_cell (rf, cf, rt, ct);

	  if (cf == fm->hc)
	    cf = fm->lc - 1;
	}
      if (rf == fm->hr)
	rf = fm->lr - 1;
    }
}

void
copy_values_region (fm, to)
     struct rng *fm;
     struct rng *to;
{
  CELLREF rf, rt, cf, ct;
  union vals dummy;
  CELL *cpf;

  if (set_to_region (fm, to) < 1)
    return;

  for (rf = fm->lr, rt = to->lr; rt <= to->hr; rt++, rf++)
    {
      for (cf = fm->lc, ct = to->lc; ct <= to->hc; ct++, cf++)
	{
	  cpf = find_cell (rf, cf);
	  set_new_value (rt, ct, cpf ? GET_TYP (cpf) : 0, cpf ? &(cpf->c_z) : &dummy);

	  if (cf == fm->hc)
	    cf = fm->lc - 1;
	}
      if (rf == fm->hr)
	rf = fm->lr - 1;
    }
}

struct rng sort_rng;
struct rng sort_ele;
struct cmp *sort_keys;
int sort_keys_alloc;
int sort_keys_num = 0;

static int srdiff, erdiff, scdiff, ecdiff;

#ifdef TEST
extern int debug;
#endif

void
sort_region ()
{
  srdiff = 1 + sort_rng.hr - sort_rng.lr;
  erdiff = 1 + sort_ele.hr - sort_ele.lr;

  scdiff = 1 + sort_rng.hc - sort_rng.lc;
  ecdiff = 1 + sort_ele.hc - sort_ele.lc;

  if (srdiff != erdiff && srdiff % erdiff != 0)
    {
      io_error_msg ("Rows %u:%u and %u:%u don't fit", sort_rng.lr, sort_rng.hr, sort_ele.lr, sort_ele.hr);
      return;
    }
  if (scdiff != ecdiff && scdiff % ecdiff != 0)
    {
      io_error_msg ("Cols %u:%u and %u:%u don't fit", sort_rng.lc, sort_rng.hc, sort_ele.lc, sort_ele.hc);
      return;
    }
  if (scdiff != ecdiff && srdiff != erdiff)
    {
      io_error_msg ("Can't sort this region!");
      return;
    }
  modified = 1;
  if (scdiff != ecdiff)
    {
      erdiff = 0;
      sort (scdiff / ecdiff, cmp_cells, swp_cells, rot_cells);
    }
  else
    {
      ecdiff = 0;
      sort (srdiff / erdiff, cmp_cells, swp_cells, rot_cells);
    }
}

extern int
cmp_cells (n1, n2)
     int n1;
     int n2;
{
  CELL *c1, *c2;
  int t1, t2;
  union vals v1, v2;
  CELLREF row1, row2, col1, col2;
  int keyn;
  int cmpval;

  if (n1 == n2)
    return 0;

  for (keyn = 0; keyn < sort_keys_num; keyn++)
    {
      row1 = sort_rng.lr + (n1 * erdiff) + sort_keys[keyn].row;
      col1 = sort_rng.lc + (n1 * ecdiff) + sort_keys[keyn].col;
      row2 = sort_rng.lr + (n2 * erdiff) + sort_keys[keyn].row;
      col2 = sort_rng.lc + (n2 * ecdiff) + sort_keys[keyn].col;
#ifdef TEST
      if (debug & 04)
	io_error_msg ("Cmp %u %u  r%uc%u <-%u-> r%uc%u", n1, n2, row1, col1, sort_keys[keyn].mult, row2, col2);
#endif
      c1 = find_cell (row1, col1);
      c2 = find_cell (row2, col2);
      if (!c1 && !c2)
	continue;

      if (c1)
	{
	  t1 = GET_TYP (c1);
	  v1 = c1->c_z;
	}
      else
	t1 = 0;
      if (c2)
	{
	  t2 = GET_TYP (c2);
	  v2 = c2->c_z;
	}
      else
	t2 = 0;

      if (t1 == TYP_ERR || t1 == TYP_BOL)
	{
	  t1 = TYP_STR;
	  v1.c_s = print_cell (c1);
	}
      if (t2 == TYP_ERR || t2 == TYP_BOL)
	{
	  t2 = TYP_STR;
	  v2.c_s = print_cell (c2);
	}
      if (t1 != t2)
	{
	  if (t1 == 0)
	    {
	      if (t2 == TYP_STR)
		{
		  t1 = TYP_STR;
		  v1.c_s = "";
		}
	      else if (t2 == TYP_INT)
		{
		  t1 = TYP_INT;
		  v1.c_l = 0;
		}
	      else
		{
		  t1 = TYP_FLT;
		  v1.c_d = 0.0;
		}
	    }
	  else if (t2 == 0)
	    {
	      if (t1 == TYP_STR)
		{
		  t2 = TYP_STR;
		  v2.c_s = "";
		}
	      else if (t1 == TYP_INT)
		{
		  t2 = TYP_INT;
		  v2.c_l = 0;
		}
	      else
		{
		  t2 = TYP_FLT;
		  v2.c_d = 0.0;
		}
	    }
	  else if (t1 == TYP_STR)
	    {
	      t2 = TYP_STR;
	      v2.c_s = print_cell (c2);
	    }
	  else if (t2 == TYP_STR)
	    {
	      t1 = TYP_STR;
	      v1.c_s = print_cell (c1);
	      /* If we get here, one is INT, and the other
			   is FLT  Make them both FLT */
	    }
	  else if (t1 == TYP_INT)
	    {
	      t1 = TYP_FLT;
	      v1.c_d = (double) v1.c_l;
	    }
	  else
	    {
	      t2 = TYP_FLT;
	      v2.c_d = (double) v2.c_l;
	    }
	}
      if (t1 == TYP_STR)
	cmpval = strcmp (v1.c_s, v2.c_s);
      else if (t1 == TYP_FLT)
	cmpval = (v1.c_d < v2.c_d) ? -1 : ((v1.c_d > v2.c_d) ? 1 : 0);
      else if (t1 == TYP_INT)
	cmpval = (v1.c_l < v2.c_l) ? -1 : ((v1.c_l > v2.c_l) ? 1 : 0);
      else
	cmpval = 0;
      if (cmpval)
	return cmpval * sort_keys[keyn].mult;
    }

  return 0;
}

extern void
swp_cells (n1, n2)
     int n1;
     int n2;
{
  int rn, cn;
  CELLREF r1, r2, c1, c2;

#ifdef TEST
  if (debug & 04)
    io_error_msg ("Swap %u<-->%u", n1, n2);
#endif
  for (rn = sort_ele.lr; rn <= sort_ele.hr; rn++)
    for (cn = sort_ele.lc; cn <= sort_ele.hc; cn++)
      {
	r1 = sort_rng.lr + (n1 * erdiff) + rn;
	r2 = sort_rng.lr + (n2 * erdiff) + rn;
	c1 = sort_rng.lc + (n1 * ecdiff) + cn;
	c2 = sort_rng.lc + (n2 * ecdiff) + cn;
#ifdef TEST
	if (debug & 04)
	  io_error_msg ("Save  r%uc%u", r1, c1);
#endif
	move_cell (r1, c1, NON_ROW, NON_COL);
#ifdef TEST
	if (debug & 04)
	  io_error_msg ("Copy r%uc%u --> r%uc%u", r2, c2, r1, c1);
#endif
	move_cell (r2, c2, r1, c1);

#ifdef TEST
	if (debug & 04)
	  io_error_msg ("Restore r%uc%u", r2, c2);
#endif
	move_cell (NON_ROW, NON_COL, r2, c2);

	/* push_cell(r1,c1);
			push_cell(r2,c2); */
      }
}

extern void
rot_cells (n1, n2)
     int n1;
     int n2;
{
  int rn, cn;
  int nn;
  CELLREF r1, r2, c1, c2;

  if (n1 + 1 == n2 || n2 + 1 == n1)
    {
      swp_cells (n1, n2);
      return;
    }
#ifdef TEST
  if (debug & 04)
    io_error_msg ("Rot cells %u -- %u", n1, n2);
#endif
  for (rn = sort_ele.lr; rn <= sort_ele.hr; rn++)
    for (cn = sort_ele.lc; cn <= sort_ele.hc; cn++)
      {

	/* store a copy of cell # n2 */
	r2 = sort_rng.lr + (n2 * erdiff) + rn;
	c2 = sort_rng.lc + (n2 * ecdiff) + cn;
	move_cell (r2, c2, NON_ROW, NON_COL);

#ifdef TEST
	if (debug & 04)
	  io_error_msg ("Save r%uc%u", r2, c2);
#endif
	/* Copy each cell from n1 to n2-1 up one */
	for (nn = n2; nn > n1; --nn)
	  {
	    r2 = sort_rng.lr + (nn * erdiff) + rn;
	    c2 = sort_rng.lc + (nn * ecdiff) + cn;

	    r1 = sort_rng.lr + ((nn - 1) * erdiff) + rn;
	    c1 = sort_rng.lc + ((nn - 1) * ecdiff) + cn;

	    move_cell (r1, c1, r2, c2);
#ifdef TEST
	    if (debug & 04)
	      io_error_msg ("Copy r%uc%u --> r%uc%u", r1, c1, r2, c2);
#endif
	    /* push_cell(r2,c2); */
	  }

	r1 = sort_rng.lr + (nn * erdiff) + rn;
	c1 = sort_rng.lc + (nn * ecdiff) + cn;
#ifdef TEST
	if (debug & 04)
	  io_error_msg ("Restore r%uc%u", r1, c1);
#endif
	move_cell (NON_ROW, NON_COL, r1, c1);

	/* push_cell(r1,c1); */
      }
}

/* End of functions for sort_region() */
