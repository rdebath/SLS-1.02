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
#include "io-generic.h"
#include "io-abstract.h"
#include "global.h"
#include "cell.h"
#include "line.h"
#include "io-term.h"
#include "lists.h"
#include "io-utils.h"
#include "ref.h"
#include "regions.h"
#include "window.h"


extern unsigned short default_width;

/* These functions read and write Microsoft Multiplan SYLK style files
 * as well as SYLK-NOA0 files.  SYLK-NOA0 is the same as SYLK except that
 * cell references are in rc format instead of a0 format.
 */

int sylk_a0 = 1;		/* Determines sylk vs. sylk_noa0 format. */

void
sylk_read_file (fp, ismerge)
     FILE *fp;
     int ismerge;
{
  char *ptr;
  CELLREF crow = 0, ccol = 0, czrow = 0, czcol = 0;
  int lineno;
  char cbuf[1024];
  char expbuf[1024];
  char *vname, *vval;
  int vlen = 0;
  int cprot;
  char *cexp, *cval;
  CELL *cp;
  struct rng rng;
  int fmt = 0;
  int jst = 0;
  long mx_row = MAX_ROW, mx_col = MAX_COL;
  int next_a0;
  int old_a0;

  old_a0 = a0;
  next_a0 = old_a0;
  a0 = sylk_a0;

  lineno = 0;
  if (!ismerge)
    clear_spreadsheet ();
  while (fgets (cbuf, sizeof (cbuf), fp))
    {
      lineno++;
      if (lineno % 50 == 0)
	io_info_msg ("Line %d", lineno);
      if (ptr = (char *)index (cbuf, '\n'))
	*ptr = '\0';
      ptr = cbuf;
      switch (*ptr)
	{
	case 'I':		/* ID field, ignored */
	  if (ptr[1] != 'D' || ptr[2] != ';')
	    goto bad_field;
	  break;
	case 'F':		/* Format field */
	  vlen = 0;
	  ptr++;
	  while (*ptr)
	    {
	      if (*ptr != ';')
		goto bad_field;
	      ptr++;
	      switch (*ptr++)
		{
		  int clo, chi, cwid;
		case 'C':	/* Column from rows 1 to 255 */
		  czcol = astol (&ptr);
		  vlen = 2;
		  break;

		case 'D':	/* Default format */
		  switch (*ptr++)
		    {
		    case 'G':
		      default_fmt = FMT_GEN - PRC_FLT;
		      break;
		    case 'E':
		      default_fmt = FMT_EXP - PRC_FLT;
		      break;
		    case 'F':
		      default_fmt = FMT_FXT - PRC_FLT;
		      break;
		    case '$':
		      default_fmt = FMT_DOL - PRC_FLT;
		      break;
		    case '*':	/* * format implemented as +- format */
		      default_fmt = FMT_GPH;
		      break;
		    case ',':	/* JF */
		      default_fmt = FMT_CMA - PRC_FLT;
		      break;
		    case 'U':
		      default_fmt = FMT_USR - PRC_FLT;
		      break;
		    case '%':
		      default_fmt = FMT_PCT - PRC_FLT;
		      break;
		    case 'H':
		      default_fmt = FMT_HID;
		      break;
		      /* End of JF */
		    case 'C':	/* Continuous not supported */
		    default:
		      io_error_msg ("Line %d: format %c not supported", lineno, ptr[-1]);
		      break;
		    }
		  if (*ptr == 'F')
		    {
		      default_fmt += PRC_FLT;
		      ptr++;
		    }
		  else
		    default_fmt += astol (&ptr);
		  switch (*ptr++)
		    {
		    case 'C':
		      default_jst = JST_CNT;
		      break;
		    case 'L':
		      default_jst = JST_LFT;
		      break;
		    case 'R':
		      default_jst = JST_RGT;
		      break;
		    case 'G':	/* General format not supported */
		    default:
		      io_error_msg ("Line %d: Alignment %c not supported", lineno, ptr[-1]);
		      break;
		    }
		  default_width = astol (&ptr);
		  break;

		case 'F':
		  switch (*ptr++)
		    {
		    case 'D':
		      fmt = FMT_DEF;
		      break;
		    case 'G':
		      fmt = FMT_GEN - PRC_FLT;
		      break;
		    case 'E':
		      fmt = FMT_EXP - PRC_FLT;
		      break;
		    case 'F':
		      fmt = FMT_FXT - PRC_FLT;
		      break;
		    case '$':
		      fmt = FMT_DOL - PRC_FLT;
		      break;
		    case '*':	/* JF implemented as +- format */
		      fmt = FMT_GPH;
		      break;
		    case ',':	/* JF */
		      fmt = FMT_CMA - PRC_FLT;
		      break;
		    case 'U':
		      fmt = FMT_USR - PRC_FLT;
		      break;
		    case '%':
		      fmt = FMT_PCT - PRC_FLT;
		      break;
		    case 'H':
		      fmt = FMT_HID;
		      break;	/* END of JF */
		    case 'C':
		    default:
		      io_error_msg ("Line %d: format %c not supported", lineno, ptr[-1]);
		      fmt = FMT_DEF;
		      break;
		    }
		  if (*ptr == 'F')
		    {
		      fmt += PRC_FLT;
		      ptr++;
		    }
		  else
		    fmt += astol (&ptr);
		  switch (*ptr++)
		    {
		    case 'C':
		      jst = JST_CNT;
		      break;
		    case 'L':
		      jst = JST_LFT;
		      break;
		    case 'R':
		      jst = JST_RGT;
		      break;
		    case 'D':
		      jst = JST_DEF;
		      break;
		    default:
		      io_error_msg ("Line %d: Alignment %c not supported", lineno, ptr[-1]);
		      jst = JST_DEF;
		      break;
		    }
		  vlen = 1;
		  break;
		case 'R':	/* Row from cols 1 to 63 */
		  czrow = astol (&ptr);
		  vlen = 4;
		  break;

		case 'W':	/* Width of clo to chi is cwid */
		  clo = astol (&ptr);
		  chi = astol (&ptr);
		  cwid = astol (&ptr) + 1;
		  for (; clo <= chi; clo++)
		    set_width (clo, cwid);
		  break;

		case 'H':	/* JF: extension */
		  clo = astol (&ptr);
		  chi = astol (&ptr);
		  cwid = astol (&ptr) + 1;
		  for (; clo <= chi; clo++)
		    set_height (clo, cwid);
		  break;
		case 'X':
		  ccol = astol (&ptr);
		  break;
		case 'Y':
		  crow = astol (&ptr);
		  break;

		default:
		  goto bad_field;
		}
	    }
	  switch (vlen)
	    {
	    case 1:
	      cp = find_or_make_cell (crow, ccol);
	      SET_FMT (cp, fmt);
	      SET_JST (cp, jst);
	      break;
	    case 2:
	      rng.lr = MIN_ROW;
	      rng.lc = czcol;
	      rng.hr = mx_row;
	      rng.hc = czcol;
	      make_cells_in_range (&rng);
	      while (cp = next_cell_in_range ())
		{
		  SET_FMT (cp, fmt);
		  SET_JST (cp, jst);
		}
	      break;
	    case 4:
	      rng.lr = czrow;
	      rng.lc = MIN_COL;
	      rng.hr = czrow;
	      rng.hc = mx_col;
	      make_cells_in_range (&rng);
	      while (cp = next_cell_in_range ())
		{
		  SET_FMT (cp, fmt);
		  SET_JST (cp, jst);
		}
	      break;
	    default:
	      break;
	    }
	  break;

	case 'B':		/* Boundry field, ignored */
	  ptr++;
	  while (*ptr)
	    {
	      if (*ptr != ';')
		goto bad_field;
	      ptr++;
	      switch (*ptr++)
		{
		case 'X':
		  mx_col = astol (&ptr);
		  if (mx_col > MAX_COL)
		    {
		      io_error_msg ("Boundry column %lu too large!", mx_col);
		      mx_col = MAX_COL;
		    }
		  break;
		case 'Y':
		  mx_row = astol (&ptr);
		  if (mx_row > MAX_ROW)
		    {
		      io_error_msg ("Boundry row %lu too large!", mx_row);
		      mx_row = MAX_ROW;
		    }
		  break;
		default:
		  goto bad_field;
		}
	    }
	  break;

	case 'N':		/* A Name field */
	  if (ptr[1] != 'N')
	    goto bad_field;
	  ptr += 2;
	  vname = 0;
	  vval = 0;
	  while (*ptr)
	    {
	      if (*ptr != ';')
		goto bad_field;
	      *ptr++ = '\0';
	      switch (*ptr++)
		{
		case 'N':	/* Name is */
		  vname = ptr;
		  while (*ptr && *ptr != ';')
		    ptr++;
		  vlen = ptr - vname;
		  break;
		case 'E':	/* Expression is */
		  vval = ptr;
		  while (*ptr && *ptr != ';')
		    ptr++;
		  break;
		default:
		  --ptr;
		  goto bad_field;
		}
	    }
	  if (!vname || !vval)
	    goto bad_field;
	  *ptr = '\0';
	  ptr = new_var_value (vname, vlen, vval);
	  if (ptr)
	    io_error_msg ("Line %d: Couldn't set %.*s to %s: %s", lineno, vlen, vname, vval, ptr);
	  break;

	case 'C':		/* A Cell entry */
	  cprot = 0;
	  cval = 0;
	  cexp = 0;
	  cval = 0;
	  ptr++;
	  while (*ptr)
	    {
	      int quotes;

	      if (*ptr != ';')
		goto bad_field;
	      *ptr++ = '\0';
	      switch (*ptr++)
		{
		case 'X':
		  ccol = astol (&ptr);
		  break;
		case 'Y':
		  crow = astol (&ptr);
		  break;
		case 'R':
		  czrow = astol (&ptr);
		  break;
		case 'C':
		  czcol = astol (&ptr);
		  break;
		case 'P':	/* This cell is Protected */
		  cprot++;
		  break;
		case 'K':	/* This cell's Konstant value */
		  cval = ptr;
		  quotes = 0;
		  while (*ptr && (*ptr != ';' || quotes > 0))
		    if (*ptr++ == '"')
		      quotes = !quotes;
		  break;
		case 'E':	/* This cell's Expression */
		  cexp = ptr;
		  quotes = 0;
		  while (*ptr && (*ptr != ';' || quotes > 0))
		    if (*ptr++ == '"')
		      quotes = !quotes;

		  break;
		case 'G':
		  strcpy (expbuf, cval);
		  break;
		case 'D':
		  strcpy (expbuf, cexp);
		  break;
		case 'S':
		  cexp = expbuf;
		  break;
		default:
		  --ptr;
		  goto bad_field;
		}
	    }
	  *ptr = '\0';
	  if (cexp && cval && strcmp (cexp, cval))
	    {
	      ptr = read_new_value (crow, ccol, cexp, cval);
	      if (ptr)
		{
		  io_error_msg ("Line %d: %d,%d: Read '%s' %s", lineno, crow, ccol, cexp, ptr);
		  break;
		}
	    }
	  else if (cval)
	    {
	      ptr = read_new_value (crow, ccol, 0, cval);
	      if (ptr)
		{
		  io_error_msg ("Line %d: %d,%d: Val '%s' %s", lineno, crow, ccol, cexp, ptr);
		  break;
		}
	    }
	  else if (cexp)
	    {
	      ptr = read_new_value (crow, ccol, cexp, 0);
	      if (ptr)
		{
		  io_error_msg ("Line %d: %d,%d: Exp '%s' %s", lineno, crow, ccol, cexp, ptr);
		  break;
		}
	    }
	  if (cprot)
	    SET_LCK (find_or_make_cell (crow, ccol), LCK_LCK);
	  if (ismerge)
	    push_cell (crow, ccol);
	  /* ... */
	  break;
	case 'E':
	  break;
	case 'W':
	  io_read_window_config (ptr + 2);
	  break;
	case 'U':
	  /* JF extension:  read user-defined formats */
	  read_mp_usr_fmt (ptr + 1);
	  break;
	  /* JF extension: read uset-settable options */
	case 'O':
	  a0 = next_a0;
	  read_mp_options (ptr + 2);
	  next_a0 = a0;
	  a0 = sylk_a0;
	  break;
	default:
	bad_field:
	  a0 = old_a0;
	  if (!ismerge)
	    clear_spreadsheet ();
	  io_recenter_all_win ();
	  io_error_msg ("Line %d: Unknown SYLK line \"%s\"", lineno, cbuf);
	  return;
	}
    }
  a0 = next_a0;
  io_recenter_all_win ();
}



static char *
sylk_fmt_to_str (f1)
     int f1;
{
  static char p_buf[40];
  int p1;

  p_buf[1] = '\0';
  switch (f1)
    {
    case FMT_DEF:
      p_buf[0] = 'D';
      break;
    case FMT_HID:
      p_buf[0] = 'H';
      break;
    case FMT_GPH:
      p_buf[0] = '*';
      break;
    default:
      p1 = GET_PRC (f1);
      if (p1 == PRC_FLT)
	{
	  p_buf[1] = 'F';
	  p_buf[2] = '\0';
	}
      else
	sprintf (&p_buf[1], "%d", p1);
      switch (f1 | PRC_FLT)
	{
	case FMT_USR:
	  p_buf[0] = 'U';
	  break;
	case FMT_GEN:
	  p_buf[0] = 'G';
	  break;
	case FMT_DOL:
	  p_buf[0] = '$';
	  break;
	case FMT_PCT:
	  p_buf[0] = '%';
	  break;
	case FMT_FXT:
	  p_buf[0] = 'F';
	  break;
	case FMT_CMA:
	  p_buf[0] = ',';
	  break;
	case FMT_EXP:
	  p_buf[0] = 'E';
	  break;
	default:
	  p_buf[0] = '?';
	  break;
	}
      break;
    }
  return p_buf;
}

static char
jst_to_chr (just)
     int just;
{
  switch (just)
    {
    case JST_DEF:
      return 'D';
    case JST_LFT:
      return 'L';
    case JST_RGT:
      return 'R';
    case JST_CNT:
      return 'C';
    default:
      return '?';
    }
}

static FILE *sylk_fp;
static struct rng *sylk_rng;

static void
sylk_write_var (name, var)
     char *name;
     struct var *var;
{
  if (var->var_flags == VAR_UNDEF && (!var->var_ref_fm || var->var_ref_fm->refs_used == 0))
    return;
  switch (var->var_flags)
    {
    case VAR_UNDEF:
      break;
    case VAR_CELL:
      if (var->v_rng.lr >= sylk_rng->lr && var->v_rng.lr <= sylk_rng->hr && var->v_rng.lc >= sylk_rng->lc && var->v_rng.lc <= sylk_rng->hc)
	(void) fprintf (sylk_fp, "NN;N%s;E%s\n", var->var_name, cell_name (var->v_rng.lr, var->v_rng.lc));
      break;
    case VAR_RANGE:
      if (var->v_rng.lr < sylk_rng->lr || var->v_rng.hr > sylk_rng->hr || var->v_rng.lc < sylk_rng->lc || var->v_rng.hc > sylk_rng->hc)
	break;

      (void) fprintf (sylk_fp, "NN;N%s;E%s\n", var->var_name, range_name (&(var->v_rng)));
      break;
#ifdef TEST
    default:
      panic ("Unknown var type %d", var->var_flags);
#endif
    }
}

static void
write_mp_windows (fp)
     FILE *fp;
{
  struct line line;
  line.alloc = 0;
  line.buf = 0;
  io_write_window_config (&line);
  fputs (line.buf, fp);
  free (line.buf);
}

void
sylk_write_file (fp, rng)
     FILE *fp;
     struct rng *rng;
{
  CELLREF r, c;
  CELL *cp;
  CELLREF crow = 0, ccol = 0;
  unsigned short w;
  extern void for_all_vars ();
  /* struct var *var; */
  int old_a0;

  (void) fprintf (fp, "ID;POLEO\n");

  /* If no range given, write the entire file */
  if (!rng)
    {
      int n;
      int fmts;
      char *data[9];

      rng = &all_rng;

      (void) fprintf (fp, "F;D%s%c%u\n", sylk_fmt_to_str (default_fmt),
		      jst_to_chr (default_jst), default_width); 

      fmts = usr_set_fmts ();
      for (n = 0; n < 16; n++)
	{
	  if (fmts & (1 << n))
	    {
	      get_usr_stats (n, data);
	      fprintf (fp, "U;N%u;P%s;S%s", n + 1, data[7], data[8]);
	      if (data[0][0])
		fprintf (fp, ";HP%s", data[0]);
	      if (data[1][0])
		fprintf (fp, ";HN%s", data[1]);
	      if (data[2][0])
		fprintf (fp, ";TP%s", data[2]);
	      if (data[3][0])
		fprintf (fp, ";TN%s", data[3]);
	      if (data[4][0])
		fprintf (fp, ";Z%s", data[4]);
	      if (data[5][0])
		fprintf (fp, ";C%s", data[5]);
	      if (data[6])
		fprintf (fp, ";D%s", data[6]);
	      putc ('\n', fp);
	    }
	}
      write_mp_options (fp);

      (void) fprintf (fp, "B;Y%u;X%u\n", highest_row (), highest_col ());

    }

  old_a0 = a0;
  a0 = sylk_a0;

  find_widths (rng->lc, rng->hc);
  w = next_width (&c);
  while (w)
    {
      CELLREF cc, ccc;
      unsigned short ww;
      cc = c;
      do
	ww = next_width (&ccc);
      while (ccc == ++cc && ww == w);
      (void) fprintf (fp, "F;W%u %u %u\n", c, cc - 1, w - 1);
      c = ccc;
      w = ww;
    }

  find_heights (rng->lr, rng->hr);
  w = next_height (&c);
  while (w)
    {
      CELLREF rr, rrr;
      unsigned short ww;

      rr = r;
      do
	ww = next_height (&rrr);
      while (rrr == ++rr && ww == w);
      (void) fprintf (fp, "F;H%u %u %u\n", r, rr - 1, w - 1);
      r = rrr;
      w = ww;
    }

  sylk_fp = fp;
  sylk_rng = rng;
  for_all_vars (sylk_write_var);
  find_cells_in_range (rng);
  while (cp = next_row_col_in_range (&r, &c))
    {
      char *ptr;
      int f1, j1;
      char p_buf[40];

      f1 = GET_FMT (cp);
      j1 = GET_JST (cp);
      if (f1 != FMT_DEF || j1 != JST_DEF)
	{
	  (void) fprintf (fp, "F;");
	  if (c != ccol)
	    {
	      (void) fprintf (fp, "X%u;", c);
	      ccol = c;
	    }
	  if (r != crow)
	    {
	      (void) fprintf (fp, "Y%u;", r);
	      crow = r;
	    }
	  (void) fprintf (fp, "F%s%c\n", sylk_fmt_to_str (f1), jst_to_chr (j1));
	}

      if (!GET_TYP (cp) && !cp->cell_formula)
	continue;

      (void) fprintf (fp, "C;");
      if (c != ccol)
	{
	  (void) fprintf (fp, "X%u;", c);
	  ccol = c;
	}
      if (r != crow)
	{
	  (void) fprintf (fp, "Y%u;", r);
	  crow = r;
	}

      if (cp->cell_formula)
	{
	  (void) fprintf (fp, "E%s", decomp (r, c, cp));
	  decomp_free ();
	}

      switch (GET_TYP (cp))
	{
	case 0:
	  ptr = 0;
	  break;
	case TYP_STR:
	  ptr = 0;
	  if (cp->cell_formula)
	    putc (';', fp);
	  (void) fprintf (fp, "K\"%s\"", cp->cell_str);
	  break;
	case TYP_FLT:
	  ptr = flt_to_str (cp->cell_flt);
	  break;
	case TYP_INT:
	  sprintf (p_buf, "%ld", cp->cell_int);
	  ptr = p_buf;
	  break;
	case TYP_BOL:
	  ptr = bname[cp->cell_bol];
	  break;
	case TYP_ERR:
	  ptr = ename[cp->cell_err];
	  break;
#ifdef TEST
	default:
	  ptr = 0;
	  panic ("What cell type %d", GET_TYP (cp));
#endif
	}

      if (ptr)
	{
	  if (cp->cell_formula)
	    putc (';', fp);
	  (void) fprintf (fp, "K%s", ptr);
	}
      if (GET_LCK (cp) == LCK_LCK)
	(void) fprintf (fp, ";P");

      putc ('\n', fp);
    }

  if (rng == &all_rng)
    write_mp_windows (fp);

  (void) fprintf (fp, "E\n");
  a0 = old_a0;
}

int
sylk_set_options (set_opt, option)
     int set_opt;
     char *option;
{
  return -1;
}

void
sylk_show_options ()
{
  io_text_line ("File format: sylk  (Microsoft Multiplan interchange format)");
}
