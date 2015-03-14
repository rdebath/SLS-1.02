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
#include "sysdef.h"
#include "global.h"
#include "cell.h"
#include "io-generic.h"
#include "io-abstract.h"
#include "regions.h"
#include "io-utils.h"





static char sl_sep = '\t';



/* This file reads/writes files containing values in separated lists,
   sl_sep is the separating character.  This isn't really a save-file
   format, but it is useful for reading and writing tables written by other
   programs.

   Note that this format loses *most* of the information about the cells,
   including formuale, formats, column widths, etc
 */
void
list_read_file (fp, ismerge)
     FILE *fp;
     int ismerge;
{
  char cbuf[1024];
  CELLREF row, col;
  char *bptr;
  char *eptr;
  char *ptr;
  char tchar;
  int endit;
  unsigned lineno;
  int string;

  lineno = 0;
  if (!ismerge)
    clear_spreadsheet ();
  row = curow;
  col = cucol;

  while (fgets (&cbuf[1], sizeof (cbuf) - 3, fp))
    {
      lineno++;
      if (lineno % 50 == 0)
	io_info_msg ("Line %d", lineno);
      endit = 0;
      for (bptr = &cbuf[1];; bptr = eptr + 1)
	{
	  eptr = (char *)index (bptr, sl_sep);
	  if (!eptr)
	    {
	      eptr = (char *)index (bptr, '\n');
	      endit++;
	    }
	  string = 0;
	  for (ptr = bptr; ptr != eptr; ptr++)
	    if (!isdigit (*ptr) && *ptr != '.' && *ptr != 'e' && *ptr != 'E')
	      {
		string++;
		break;
	      }
	  if (string)
	    {
	      bptr[-1] = '"';
	      eptr[0] = '"';
	      tchar = eptr[1];
	      eptr[1] = '\0';
	      new_value (row, col, &bptr[-1]);
	      eptr[1] = tchar;
	    }
	  else
	    {
	      eptr[0] = '\0';
	      new_value (row, col, bptr);
	    }
	  if (endit)
	    break;
	  col++;
	}
      row++;
      col = cucol;
    }
}

void
list_write_file (fp, rng)
     FILE *fp;
     struct rng *rng;
{
  CELLREF row, col;
  int repressed;
  CELL *cp;

  if (!rng)
    rng = &all_rng;
  for (row = rng->lr;; row++)
    {
      repressed = 0;
      for (col = rng->lc;; col++)
	{
	  if ((cp = find_cell (row, col)) && GET_TYP (cp))
	    {
	      while (repressed > 0)
		{
		  putc (sl_sep, fp);
		  --repressed;
		}
	      repressed = 1;
	      switch (GET_TYP (cp))
		{
		case TYP_FLT:
		  fputs (flt_to_str (cp->cell_flt), fp);
		  break;
		case TYP_INT:
		  fprintf (fp, "%ld", cp->cell_int);
		  break;
		case TYP_STR:
		  fputs (cp->cell_str, fp);
		  break;
		case TYP_BOL:
		  fputs (bname[cp->cell_bol], fp);
		  break;
		case TYP_ERR:
		  fputs (ename[cp->cell_err], fp);
		  break;
#ifdef TEST
		default:
		  panic ("Unknown type %d in write_sl_file()", GET_TYP (cp));
		  break;
#endif
		}
	    }
	  else
	    repressed++;
	  if (col == rng->hc)
	    break;
	}
      putc ('\n', fp);
      if (row == rng->hr)
	break;
    }
}

int
list_set_options (set_opt, option)
     int set_opt;
     char *option;
{
  if (set_opt && !strincmp (option, "list ", 5))
    {
      option += 5;
      sl_sep = string_to_char (&option);
      return 0;
    }
  return -1;
}

void
list_show_options ()
{
  io_text_line ("File format: list    (character separated list of cell values)");
  io_text_line ("Save file element separator: %s", char_to_string (sl_sep));
}
