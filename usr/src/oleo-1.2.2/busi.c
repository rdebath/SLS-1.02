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
#include "cell.h"
#include "eval.h"
#include "errors.h"

struct value
  {
    int type;
    union vals x;
  };

#define Float	x.c_d
#define String	x.c_s
#define Int	x.c_l
#define Value	x.c_i
#define Rng	x.c_r

static double
pmt (principal, rate, term)
     double principal;
     double rate;
     double term;
{
  return (principal * rate) / (1 - pow (1 + rate, -(term)));
}


static int
npv (rng, rate, putres)
     struct rng *rng;
     double rate;
     double *putres;
{
  double npv;
  int i;
  double f;
  CELL *cell_ptr;
  char *strptr;

  find_cells_in_range (rng);
  for (i = 1, npv = 0.0; cell_ptr = next_cell_in_range (); i++)
    {
      switch (GET_TYP (cell_ptr))
	{
	case 0:
	  f = 0.0;
	  goto know_f;

	case TYP_INT:
	  f = (double) (cell_ptr->cell_int);
	  goto know_f;

	case TYP_FLT:
	  f = cell_ptr->cell_flt;
	  goto know_f;

	case TYP_STR:
	  strptr = cell_ptr->cell_str;
	  f = astof (&strptr);
	  if (*strptr)
	    return NON_NUMBER;
	know_f:
	  npv += f * (1.0 / (pow (1.0 + rate, (double) i)));
	  break;

	case TYP_ERR:
	  return cell_ptr->cell_err;

	default:
	  return NON_NUMBER;
	}
    }

  *putres = npv;
  return 0;
}

static void
do_pmt (p)
     struct value *p;
{
  p->Float = pmt (p->Float, (p + 1)->Float, (p + 2)->Float);
}

static void
do_pv (p)
     struct value *p;
{
  double payment, interest, term;

  payment = p[0].Float;
  interest = p[1].Float;
  term = p[2].Float;

  p->Float = payment * ((1 - pow (1 + interest, -term)) / interest);
}

static void
do_npv (p)
     struct value *p;
{
  int tmp;

  tmp = npv (&(p->Rng), (p + 1)->Float, &(p->Float));
  if (tmp)
    {
      p->Value = tmp;
      p->type = TYP_ERR;
    }
  p->type = TYP_FLT;
}

static void
do_irr (p)
     struct value *p;
{
  double try;
  double res;
  double mint, maxt;
  double minr, maxr;
  int i;
  int tmp;

  minr = maxr = 0;
  mint = maxt = 0;
  while (minr >= 0)
    {
      mint += 1;
      tmp = npv (&(p->Rng), mint, &minr);
      if (tmp)
	{
	  p->Value = tmp;
	  p->type = TYP_ERR;
	  return;
	}
    }
  while (maxr <= 0)
    {
      maxt -= 1;
      tmp = npv (&(p->Rng), maxt, &maxr);
      if (tmp)
	{
	  p->Value = tmp;
	  p->type = TYP_ERR;
	  return;
	}
    }
  try = (p + 1)->Float;
  for (i = 0;; i++)
    {
      if (i == 40)
	{
	  p->Value = BAD_INPUT;
	  p->type = TYP_ERR;
	  return;
	}
      tmp = npv (&(p->Rng), try, &res);
      if (tmp)
	{
	  p->Value = tmp;
	  p->type = TYP_ERR;
	  return;
	}
      if (fabs (res * 1000000.0) < 1)
	break;
      if (res > 0)
	{
	  maxt = try;
	  maxr = res;
	}
      else if (res < 0)
	{
	  mint = try;
	  minr = res;
	}
      if (minr / -10 > maxr)
	{
	  /* it is quite near maxt */
	  try = (maxt * 10 + mint) / 11;
	}
      else if (minr / -2 > maxr)
	{
	  try = (maxt * 2 + mint) / 3;
	}
      else if (minr * -10 < maxr)
	{
	  /* It is quite near mint */
	  try = (maxt + mint * 10) / 11;
	}
      else if (minr * -2 < maxr)
	{
	  try = (maxt + mint * 2) / 3;
	}
      else
	try = (maxt + mint) / 2;
    }
  p->Float = try;
  p->type = TYP_FLT;
}

static void
do_fv (p)
     struct value *p;
{
  double payment = p->Float;
  double interest = (p + 1)->Float;
  double term = (p + 2)->Float;

  p->Float = payment * ((pow (1 + interest, term) - 1) / interest);
}

static void
do_rate (p)
     struct value *p;
{
  double future = p->Float;
  double present = (p + 1)->Float;
  double term = (p + 2)->Float;

  p->Float = pow (future / present, 1 / term) - 1;
}

static void
do_term (p)
     struct value *p;
{
  double payment = p->Float;
  double interest = (p + 1)->Float;
  double future = (p + 2)->Float;

  p->Float = log (1 + future * (interest / payment)) / log (1 + interest);
}

static void
do_cterm (p)
     struct value *p;
{
  double interest = (p)->Float;
  double future = (p + 1)->Float;
  double present = (p + 2)->Float;

  p->Float = log (future / present) / log (1 + interest);
}

static void
do_sln (p)
     struct value *p;
{
  double cost = (p)->Float;
  double salvage = (p + 1)->Float;
  double life = (p + 2)->Float;

  p->Float = (cost - salvage) / life;
}

static void
do_syd (p)
     struct value *p;
{
  double cost, salvage, life, period;

  cost = p->Float;
  salvage = (p + 1)->Float;
  life = (p + 2)->Float;
  period = (p + 3)->Float;

  if (period > life)		/* JF is this right? */
    p->Float = salvage;
  else
    p->Float = ((cost - salvage) * (life - period + 1)) / (life * ((life + 1) / 2));
}


static void
do_ddb (p)
     struct value *p;
{
  double cost = (p)->Float;
  double salvage = (p + 1)->Float;
  long life = (p + 2)->Int;
  long period = (p + 3)->Int;

  double bookval, tmp;
  long n;

  if (period < 1 || period > life || life < 1)
    {
      p->Value = OUT_OF_RANGE;
      p->type = TYP_ERR;
      return;
    }
  bookval = cost;
  tmp = 0;
  for (n = 0; n < period; n++)
    {
      tmp = (bookval * 2) / life;
      bookval -= tmp;
      if (bookval < salvage)
	{
	  tmp += bookval - salvage;
	  bookval = salvage;
	}
    }
  p->Float = tmp;
}

static void
do_anrate (p)
     struct value *p;
{
  double in_pmt = (p)->Float;
  double present = (p + 1)->Float;
  double term = (p + 2)->Float;

  double tr_lo, tr_hi;
  double mytry;
  double try_pmt;
  int n;

  if (in_pmt * term == present)
    {
      p->Float = 0.0;
      return;
    }
  if (in_pmt * term < present)
    {
      tr_lo = -1;
      tr_hi = 0;
      while (pmt (present, tr_lo, term) > in_pmt)
	{
	  tr_hi = tr_lo;
	  tr_lo *= 2;
	}
    }
  else
    {
      tr_lo = 0;
      tr_hi = 1;
      while (pmt (present, tr_hi, term) < in_pmt)
	{
	  tr_lo = tr_hi;
	  tr_hi *= 2;
	}
    }
  for (n = 0; n < 40; n++)
    {
      mytry = (tr_lo + tr_hi) / 2;
      try_pmt = pmt (present, mytry, term);
      if (try_pmt < in_pmt)
	tr_lo = mytry;
      else if (try_pmt > in_pmt)
	tr_hi = mytry;
      else
	break;
    }
  p->Float = mytry;
}

static void
do_anterm (p)
     struct value *p;
{
  double payment = (p)->Float;
  double principal = (p + 1)->Float;
  double rate = (p + 2)->Float;

  p->Float = (-log (1 - principal * (rate / payment))) / log (1 + rate);
}


static void
do_balance (p)
     struct value *p;
{
  double principal = (p)->Float;
  double rate = (p + 1)->Float;
  long term = (p + 2)->Int;
  long period = (p + 3)->Int;

  double tmp_pmt, int_part;
  long num;

  if (term < period)
    {
      p->Value = OUT_OF_RANGE;
      p->type = TYP_ERR;
      return;
    }
  tmp_pmt = pmt (principal, rate, (double) term);
  for (num = 0; num < period; num++)
    {
      int_part = rate * principal;
      if (int_part > tmp_pmt)
	{
	  p->Value = OUT_OF_RANGE;
	  p->type = TYP_ERR;
	  return;
	}
      principal -= tmp_pmt - int_part;
    }
  p->Float = principal;
}

static void
do_paidint (p)
     struct value *p;
{
  double principal = (p)->Float;
  double rate = (p + 1)->Float;
  long term = (p + 2)->Int;
  long period = (p + 3)->Int;

  double tmp_pmt, int_part, retval;
  long num;

  if (term < period)
    {
      p->Value = OUT_OF_RANGE;
      p->type = TYP_ERR;
      return;
    }
  tmp_pmt = pmt (principal, rate, (double) term);
  retval = 0;
  for (num = 0; num < period; num++)
    {
      int_part = rate * principal;
      if (int_part > tmp_pmt)
	{
	  p->Value = OUT_OF_RANGE;
	  p->type = TYP_ERR;
	  return;
	}
      principal -= tmp_pmt - int_part;
      retval += int_part;
    }
  p->Float = retval;
}

static void
do_kint (p)
     struct value *p;
{
  double principal = (p)->Float;
  double rate = (p + 1)->Float;
  long term = (p + 2)->Int;
  long period = (p + 3)->Int;

  double tmp_pmt, int_part = 0;
  long num;

  if (term < period)
    {
      p->Value = OUT_OF_RANGE;
      p->type = TYP_ERR;
      return;
    }

  tmp_pmt = pmt (principal, rate, (double) term);
  for (num = 0; num < period; num++)
    {
      int_part = rate * principal;
      if (int_part > tmp_pmt)
	{
	  p->Value = OUT_OF_RANGE;
	  p->type = TYP_ERR;
	  return;
	}
      principal -= tmp_pmt - int_part;
    }
  p->Float = int_part;
}

static void
do_kprin (p)
     struct value *p;
{
  double principal = (p)->Float;
  double rate = (p + 1)->Float;
  long term = (p + 2)->Int;
  long period = (p + 3)->Int;
  double tmp_pmt, int_part = 0;
  long num;

  if (term < period)
    {
      p->Value = OUT_OF_RANGE;
      p->type = TYP_ERR;
      return;
    }

  tmp_pmt = pmt (principal, rate, (double) term);
  for (num = 0; num < period; num++)
    {
      int_part = rate * principal;
      if (int_part > tmp_pmt)
	{
	  p->Value = OUT_OF_RANGE;
	  p->type = TYP_ERR;
	  return;
	}
      principal -= tmp_pmt - int_part;
    }
  p->Float = tmp_pmt - int_part;
}

static void
do_compbal (p)
     struct value *p;
{
  double principal = (p)->Float;
  double rate = (p + 1)->Float;
  double term = (p + 2)->Float;

  p->Float = principal * pow (1 + rate, term);
}

struct function busi_funs[] =
{
  {C_FN2, X_A2, "RF", do_npv, "npv"},
  {C_FN2, X_A2, "RF", do_irr, "irr"},

  {C_FN3, X_A3, "FFF", do_pmt, "pmt"},
  {C_FN3, X_A3, "FFF", do_pv, "pv"},
  {C_FN3, X_A3, "FFF", do_fv, "fv"},
  {C_FN3, X_A3, "FFF", do_rate, "rate"},
  {C_FN3, X_A3, "FFF", do_term, "term"},
  {C_FN3, X_A3, "FFF", do_cterm, "cterm"},
  {C_FN3, X_A3, "FFF", do_sln, "sln"},
  {C_FN3, X_A3, "FFF", do_anrate, "anrate"},
  {C_FN3, X_A3, "FFF", do_anterm, "anterm"},
  {C_FN3, X_A3, "FFF", do_compbal, "compbal"},

  {C_FN4, X_A4, "FFFF", do_syd, "syd"},
  {C_FN4, X_A4, "FFII", do_ddb, "ddb"},
  {C_FN4, X_A3, "FFII", do_balance, "balance"},
  {C_FN4, X_A4, "FFII", do_paidint, "paidint"},
  {C_FN4, X_A4, "FFII", do_kint, "kint"},
  {C_FN4, X_A4, "FFII", do_kprin, "kprin"},

  {0, 0, "", 0, 0},
};
