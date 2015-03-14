/*	Copyright (C) 1992, 1993 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
#include "display.h"
#include "lists.h"
#include "cell.h"
#include "io-utils.h"

#if __STDC__
struct cell_display *
cell_display_of (struct display *disp, CELLREF r, CELLREF c)
#else
struct cell_display *
cell_display_of (disp, r, c)
     struct display *disp;
     CELLREF r;
     CELLREF c;
#endif
{
  int cols = disp->range.hc - disp->range.lc + 1;
  r -= disp->range.lr;
  c -= disp->range.lc;
  return disp->cells + r * cols + c;
}


void 
free_display (disp)
     struct display *disp;
{
  int rows = disp->range.hr - disp->range.lr + 1;
  int cols = disp->range.hc - disp->range.lc + 1;
  struct cell_display *cd = disp->cells;
  int x, y;
  for (y = 0; y < rows; ++y)
    for (x = 0; x < cols; ++x)
      {
	if (cd->unclipped)
	  free (cd->unclipped);
	if (cd->clipped)
	  free (cd->clipped);
      }
  free (disp->widths);
  free (disp->heights);
  free (disp->cells);
  free (disp->rowy);
  free (disp->colx);
}

void 
damage (disp, cd)
     struct display *disp;
     struct cell_display *cd;
{
  if (cd && !cd->next_damaged)
    {
      cd->next_damaged = disp->damaged;
      disp->damaged = cd;
    }
}

#if __STDC__
int
pr_display_cell (struct display *disp, CELLREF r, CELLREF c, CELL *cp)
#else
int
pr_display_cell (disp, r, c, cp)
     struct display *disp;
     CELLREF r;
     CELLREF c;
     CELL *cp;
#endif
{
  int cols = disp->range.hc - disp->range.lc + 1;
  struct cell_display *cd =
  &disp->cells[cols * (r - disp->range.lr) + (c - disp->range.lc)];
  xx_IntRectangle ir = &cd->layout;
  struct font_memo * new_font = 0;
  char * new_unclipped = 0;
  int new_type = 0;
  int new_jst = 0;

  if (cp && disp->widths[c - disp->range.lc]
      && disp->heights[r - disp->range.lr])
    {
      new_unclipped = print_cell (cp);
      if (!new_unclipped || *new_unclipped == '\0')
	new_unclipped = 0;
      else
	{
	  new_type = GET_TYP (cp);
	  new_jst = GET_JST (cp);
	  new_font = cp->cell_font;
	  if (new_jst == JST_DEF)
	    new_jst = default_jst;
	}
    }


  if (((!new_unclipped && !cd->unclipped)
       || ((new_unclipped && cd->unclipped)
	   && !strcmp(cd->unclipped, new_unclipped))) 
      && (cd->font == new_font)
      && (cd->cell_type == new_type)
      && (cd->justification == new_jst))
    return 0;
	       
  record_display_damage (disp, xx_IRx (ir), xx_IRy (ir),
			 xx_IRw (ir), xx_IRh (ir));
  if (cd->unclipped)
    {
      free (cd->unclipped);
      cd->unclipped = 0;
    }
  if (cd->clipped)
    {
      free (cd->clipped);
      cd->clipped = 0;
    }
  cd->font = new_font;
  cd->cell_type = new_type;
  cd->justification = new_jst;
  if (!cp || !GET_TYP (cp))
    {
      xx_IRinit (&cd->goal, 0, 0, 0, 0);
      xx_IRinit (&cd->clip, 0, 0, 0, 0);
      cd->unclipped = 0;
      return 1;
    }
  cd->unclipped = ck_savestr (new_unclipped);
  cd->clipped = 0;
  disp->metric (cd, disp);
  if (new_type == TYP_INT)
    cd->numeric.integer = cp->c_z.c_l;
  else if (new_type == TYP_FLT)
    cd->numeric.dbl = cp->c_z.c_d;
  else
    {
      cd->clipped = ck_savestr (cd->unclipped);
      cd->clip = cd->goal;
    }
  return 1;
}

static void null_metric (cd, disp)
     struct cell_display * cd;
     struct display * disp;
{}

static void 
_build_display (disp, range, metric, vdata, scalep)
     struct display *disp;
     struct rng *range;
     cell_display_metric metric;
     void *vdata;
     int scalep;
{
  /* This would be more useful if it handled scrolling. */
  int r, c;
  int rows = range->hr - range->lr + 1;
  int cols = range->hc - range->lc + 1;
  int x, y;
  disp->range = *range;
  disp->widths = (int *) ck_malloc (sizeof (int) * cols);
  disp->heights = (int *) ck_malloc (sizeof (int) * rows);
  disp->rowy = (int *) ck_malloc (sizeof (int) * rows);
  disp->colx = (int *) ck_malloc (sizeof (int) * cols);
  disp->metric = metric ? metric : null_metric;
  disp->vdata = vdata;
  disp->cells = ((struct cell_display *)
		 ck_calloc (sizeof (struct cell_display) * rows * cols));
  disp->damaged = (struct cell_display *) disp;
  for (x = 0, r = range->lr; r <= range->hr; ++r)
    {
      disp->rowy[r - range->lr] = x;
      x += disp->heights[r - range->lr] = (scalep ? get_scaled_height : get_height) (r);
    }
  for (y = 0, c = range->lc; c <= range->hc; ++c)
    {
      disp->colx[c - range->lc] = y;
      y += disp->widths[c - range->lc] = (scalep ? get_scaled_width : get_width) (c);
      for (r = range->lr; r <= range->hr; ++r)
	{
	  struct cell_display *cd = cell_display_of (disp, r, c);
	  cd->r = r;
	  cd->c = c;
	  pr_display_cell (disp, r, c, find_cell (r, c));
	}
    }
}

void 
build_display (disp, range, metric, vdata)
     struct display *disp;
     struct rng *range;
     cell_display_metric metric;
     void *vdata;
{
  _build_display (disp, range, metric, vdata, 1);
}

void 
build_unscaled_display (disp, range, metric, vdata)
     struct display *disp;
     struct rng *range;
     cell_display_metric metric;
     void *vdata;
{
  _build_display (disp, range, metric, vdata, 0);
}

void 
display_range (rng, disp, x, y, w, h)
     struct rng *rng;
     struct display *disp;
     int x, y, w, h;
{
  int t;
  struct rng *winrng = &disp->range;
  int *rowy = disp->rowy;
  int *heights = disp->heights;
  int *colx = disp->colx;
  int *widths = disp->widths;
  int rows = winrng->hr - winrng->lr + 1;
  int cols = winrng->hc - winrng->lc + 1;

  for (t = 0; t < rows - 1; ++t)
    if (rowy[t] + heights[t] - 1 >= y)
      break;
  rng->lr = t + winrng->lr;

  while (t < rows - 1)
    {
      if (rowy[t] + heights[t] >= y + h)
	break;
      t++;
    }
  rng->hr = t + winrng->lr;

  for (t = 0; t < cols - 1; ++t)
    if (colx[t] + widths[t] - 1 >= x)
      break;
  rng->lc = t + winrng->lc;

  while (t < cols - 1)
    {
      if (colx[t] + widths[t] >= x + w)
	break;
      t++;
    }
  rng->hc = t + winrng->lc;
}

extern void 
record_display_damage (disp, x, y, w, h)
     struct display *disp;
     int x, y, w, h;
{
  CELLREF r, c;
  struct rng rng;
  display_range (&rng, disp, x, y, w, h);
  for (r = rng.lr; r <= rng.hr && r > 0; ++r)
    for (c = rng.lc; c <= rng.hc && c > 0; ++c)
      damage (disp, cell_display_of (disp, r, c));
}


void 
layout (disp)
     struct display *disp;
{
  int *widths = disp->widths;
  int *heights = disp->heights;
  int *rowy = disp->rowy;
  int *colx = disp->colx;
  int rows = disp->range.hr - disp->range.lr + 1;
  int cols = disp->range.hc - disp->range.lc + 1;
  int ri, ci;
  struct cell_display *cd;

  /* This assigns each non-empty cell's space to itself.  The rest of the */
  /* function allocates the space held by empty cells. */
  for (cd = disp->cells, ri = 0; ri < rows; ++ri)
    for (ci = 0; ci < cols; ++ci, ++cd)
      {
	cd->was_used_by = cd->used_by;
	cd->used_by = cd->unclipped ? cd : 0;
      }

  for (ri = 0; ri < rows; ++ri)
    for (ci = 0; ci < cols; ++ci)
      {
	struct cell_display *cd =
	cell_display_of (disp, ri + disp->range.lr, ci + disp->range.lc);
	if (cd->unclipped)
	  {
	    xx_IntRectangle gr = &cd->goal;
	    int xl = xx_IRxl (gr);
	    int xh = xx_IRxh (gr);
	    int yl = xx_IRyl (gr);
	    int rl_answer, rh_answer, cl_answer, ch_answer;
	    int rl, rh, cl, ch;
	    int rit, cit;
	    int xt, yt;

	    for (cl = ci, xt = colx[cl];
		 cl && (xt > xl);
		 --cl, xt = colx[cl]);
	    for (rl = ri, yt = rowy[rl];
		 rl && (yt > yl);
		 --rl, yt = rowy[rl]);
	    for (ch = ci, xt = colx[ch] + widths[ch];
		 (ch < (cols - 1)) && (xt < xh);
		 ++ch, xt = colx[ch] + widths[ch]);
	    rh = ri;
	    /* rl/h & cl/h bound the cells covered by the goal rectangle.
	     * Of these, at least ri, ci is unused.  The goal here is to
	     * allocate additional cells for use by ri,ci.
	     */
	    for (rit = ri - 1; rit >= rl; --rit)
	      for (cit = cl; ci <= ch; ++cit)
		{
		  struct cell_display *cdt = disp->cells + (rit * cols) + cit;
		  if (cdt->used_by)
		    goto got_rl;
		}
	  got_rl:
	    ++rit;
	    rl_answer = rit;

	    for (rit = ri + 1; rit <= rh; ++rit)
	      for (cit = cl; ci <= ch; ++cit)
		{
		  struct cell_display *cdt = disp->cells + (rit * cols) + cit;
		  if (cdt->used_by)
		    goto got_rh;
		}
	  got_rh:
	    --rit;
	    rh_answer = rit;

	    for (cit = ci - 1; cit >= cl; --cit)
	      for (rit = rl_answer; rit <= rh_answer; ++rit)
		{
		  struct cell_display *cdt = disp->cells + (rit * cols) + cit;
		  if (cdt->used_by)
		    goto got_cl;
		}
	  got_cl:
	    ++cit;
	    cl_answer = cit;

	    for (cit = ci + 1; cit <= ch; ++cit)
	      for (rit = rl_answer; rit <= rh_answer; ++rit)
		{
		  struct cell_display *cdt = disp->cells + (rit * cols) + cit;
		  if (cdt->used_by)
		    goto got_ch;
		}
	  got_ch:
	    --cit;
	    ch_answer = cit;

	    for (rit = rl_answer; rit <= rh_answer; ++rit)
	      for (cit = cl_answer; cit <= ch_answer; ++cit)
		{
		  struct cell_display *cdt = disp->cells + (rit * cols) + cit;
		  cdt->used_by = cd;
		  if (cdt->was_used_by != cdt->used_by)
		    {
		      damage (disp, cdt);
		      damage (disp, cdt->was_used_by);
		    }
		}

	    xx_IRinit (&cd->layout,
		       colx[cl_answer], rowy[rl_answer],
		       colx[ch_answer] + widths[ch_answer] - colx[cl_answer],
		       rowy[rh_answer] + heights[rh_answer] - rowy[rl_answer]);
	  }
      }

  /* Unused cells are given to themselves. */
  for (cd = disp->cells, ri = 0; ri < rows; ++ri)
    for (ci = 0; ci < cols; ++ci, ++cd)
      if (!cd->used_by)
	{
	  cd->used_by = cd;
	  if (cd->was_used_by != cd)
	    {
	      damage (disp, cd);
	      damage (disp, cd->was_used_by);
	    }
	  xx_IRinit (&cd->layout, colx[ci], rowy[ri],
		     widths[ci], heights[ri]);
	}
}
