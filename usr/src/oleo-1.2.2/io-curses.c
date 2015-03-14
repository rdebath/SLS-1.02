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


#include "proto.h"
#include "funcdef.h"
#include <stdio.h>
#include <curses.h>
#include <fcntl.h>
#include <errno.h>
#include <ctype.h>
#include <signal.h>
#undef NULL
#include "sysdef.h"
#include "global.h"
#include "cell.h"
#include "cmd.h"
#include "line.h"
#include "io-generic.h"
#include "io-edit.h"
#include "io-term.h"
#include "io-abstract.h"
#include "io-utils.h"
#include "lists.h"
#include "window.h"

extern unsigned short print_width;

/* If 2, clear the top line before reading a character */
/* if 1, clear it after reading a char */
static int topclear = 0;

#define MIN_WIN_HEIGHT	(cwin->flags&WIN_EDGES ? 2 : 1)
#define MIN_WIN_WIDTH	(cwin->flags&WIN_EDGES ? 6 : 1)

static int redrew = 0;
static int textout = 0;
static int term_cursor_claimed = 0;

#ifdef __STDC__
static void move_cursor_to (struct window *, CELLREF, CELLREF, int);
#else
static void move_cursor_to ();
#endif



static void 
_io_display_cell_cursor ()
{
  int cell_cursor_row;
  int cell_cursor_col;
  int cc;
  int rr;
  int cwid;
  int n;
  int x, y;

  getyx (stdscr, y, x);
  cell_cursor_col = cwin->win_over;
  for (cc = cwin->screen.lc; cc < cucol; cc++)
    cell_cursor_col += get_width (cc);
  cell_cursor_row = cwin->win_down;
  for (rr = cwin->screen.lr; rr < curow; rr++)
    cell_cursor_row += get_height (rr);
  cwid = get_width (cucol);
  if (cwid > cwin->numc)
    cwid = cwin->numc;
  move (cell_cursor_row, cell_cursor_col);
  standout ();
  for (n = cwid; n; n--)
#ifdef A_STANDOUT
    addch (inch () | A_STANDOUT);
#else
    addch (inch ());
#endif
  standend ();
  move (y, x);
}

void 
_io_hide_cell_cursor ()
{
  int cc;
  int rr;
  int cell_cursor_row;
  int cell_cursor_col;
  int cwid;
  int n;
  int x, y;

  getyx (stdscr, y, x);
  cell_cursor_col = cwin->win_over;
  for (cc = cwin->screen.lc; cc < cucol; cc++)
    cell_cursor_col += get_width (cc);
  cell_cursor_row = cwin->win_down;
  for (rr = cwin->screen.lr; rr < curow; rr++)
    cell_cursor_row += get_height (rr);
  cwid = get_width (cucol);
  if (cwid > cwin->numc)
    cwid = cwin->numc;
  move (cell_cursor_row, cell_cursor_col);
  for (n = cwid; n; n--)
#ifdef A_STANDOUT
    addch (inch () & ~A_STANDOUT);
#else
    addch (inch ());
#endif
  move (y, x);
}



/* Functions, etc for dealing with cell contents being displayed
	on top of other cells. */

struct slops
{
  int s_alloc, s_used;
  struct s
    {
      CELLREF row, clo, chi;
    } s_b[1];
};

static void 
flush_slops (where)
     VOIDSTAR where;
{
  struct slops *s;

  s = where;
  if (s)
    s->s_used = 0;
}

static int 
find_slop (where, r, c, cclp, cchp)
     VOIDSTAR where;
     CELLREF r;
     CELLREF c;
     CELLREF *cclp;
     CELLREF *cchp;
{
  int n;
  struct slops *s;

  s = where;
  if (!s)
    return 0;
  for (n = 0; n < s->s_used; n++)
    {
      if (s->s_b[n].row == r && s->s_b[n].clo <= c && s->s_b[n].chi >= c)
	{
	  *cclp = s->s_b[n].clo;
	  *cchp = s->s_b[n].chi;
	  return 1;
	}
    }
  return 0;
}

static void 
kill_slop (where, r, clo, chi)
     VOIDSTAR where;
     CELLREF r;
     CELLREF clo;
     CELLREF chi;
{
  int n;
  struct slops *s;

  s = where;
  for (n = 0; n < s->s_used; n++)
    {
      if (s->s_b[n].row == r && s->s_b[n].clo == clo && s->s_b[n].chi == chi)
	{
	  --(s->s_used);
	  s->s_b[n] = s->s_b[s->s_used];
	  return;
	}
    }
}

static void 
set_slop (wherep, r, clo, chi)
     VOIDSTAR *wherep;
     CELLREF r;
     CELLREF clo;
     CELLREF chi;
{
  int n;
  struct slops **sp;

  sp = (struct slops **) wherep;
  if (!*sp)
    {
      (*sp) = ck_malloc (sizeof (struct slops) + 2 * sizeof (struct s));
      (*sp)->s_alloc = 2;
      (*sp)->s_used = 1;
      n = 0;
    }
  else
    {
      n = (*sp)->s_used++;
      if ((*sp)->s_alloc == n)
	{
	  (*sp)->s_alloc = n * 2;
	  (*sp) = ck_realloc ((*sp), sizeof (struct slops) + n * 2 * sizeof (struct s));
	}
    }
  (*sp)->s_b[n].row = r;
  (*sp)->s_b[n].clo = clo;
  (*sp)->s_b[n].chi = chi;
}

static void 
change_slop (where, r, olo, ohi, lo, hi)
     VOIDSTAR where;
     CELLREF r;
     CELLREF olo;
     CELLREF ohi;
     CELLREF lo;
     CELLREF hi;
{
  int n;
  struct slops *s;

  s = where;
  for (n = 0; n < s->s_used; n++)
    {
      if (s->s_b[n].row == r && s->s_b[n].clo == olo && s->s_b[n].chi == ohi)
	{
	  s->s_b[n].clo = lo;
	  s->s_b[n].chi = hi;
	  return;
	}
    }
}

static void 
_io_open_display ()
{
  initscr ();
  scrollok (stdscr, 0);
#ifdef HAVE_CBREAK
  cbreak ();
#else
  crmode ();
#endif  
  noecho ();
  nonl ();
  /* Must be after initscr() */
  io_init_windows (LINES, COLS - 1, 1, 2, 1, 1, 1, 1);
  print_width = COLS;		/* Make ascii print width == terminal width. */
}

static int cursor_cellized = 1;
static void 
_io_cellize_cursor ()
{
  cursor_cellized = 1;
}
static void 
_io_inputize_cursor ()
{
  cursor_cellized = 0;
}

static void 
_io_redisp ()
{
  if (!term_cursor_claimed)
    if (cursor_cellized)
      move_cursor_to (cwin, curow, cucol, 0);
    else;
  refresh ();
}

static void 
_io_repaint_win (win)
     struct window *win;
{
  io_repaint ();
}

static void 
_io_repaint ()
{
  CELLREF cc, rr;
  int n, n1;
  CELL *cp;
  struct window *win;

  clear ();
  redrew++;
  for (win = wins; win < &wins[nwin]; win++)
    {
      if (win->lh_wid)
	{
	  move (win->win_down - 1, win->win_over - win->lh_wid);
	  printw ("#%*d ", win->lh_wid - 2, 1 + win - wins);
	  if (win->flags & WIN_EDGE_REV)
	    standout ();
	  cc = win->screen.lc;
	  do
	    {
	      n = get_width (cc);
	      if (n > win->numc)
		n = win->numc;
	      if (n > 1)
		{
		  char *ptr;
		  char buf[30];

		  if (a0)
		    ptr = col_to_str (cc);
		  else
		    {
		      sprintf (buf, "C%u", cc);
		      ptr = buf;
		    }
		  --n;
		  n1 = strlen (ptr);
		  if (n < n1)
		    printw ("%.*s ", n, "###############");
		  else
		    {
		      n1 = (n - n1) / 2;
		      printw ("%*s%-*s ", n1, "", n - n1, ptr);
		    }
		}
	      else if (n == 1)
		addstr ("#");
	    }
	  while (cc++ < win->screen.hc);

	  rr = win->screen.lr;
	  n = win->win_down;
	  do
	    {
	      n1 = get_height (rr);
	      if (n1)
		{
		  move (n, win->win_over - win->lh_wid);
		  if (a0)
		    printw ("%-*d ", win->lh_wid - 1, rr);
		  else
		    printw ("R%-*d", win->lh_wid - 1, rr);
		  n += n1;
		}
	    }
	  while (rr++ < win->screen.hr);

	  if (win->flags & WIN_EDGE_REV)
	    standend ();
	}
      flush_slops (win->win_slops);
      find_cells_in_range (&(win->screen));
      while (cp = next_row_col_in_range (&rr, &cc))
	if (GET_TYP (cp))
	  io_pr_cell_win (win, rr, cc, cp);
    }
  if (!(cp = find_cell (curow, cucol)) || !GET_TYP (cp))
    io_display_cell_cursor ();
  io_update_status ();
}

static void 
_io_close_display ()
{
  clear ();
  refresh ();
  (void) endwin ();
}

/* This is extern because it was convenient to leave the signal */
/* handler that increments it in io_term.c */
int input_avail_val;

static int 
_io_input_avail ()
{
  return (FD_ISSET (0, &read_pending_fd_set)
	  || FD_ISSET (0, &exception_pending_fd_set));
}

static void 
_io_scan_for_input (block)
     int block;
{
  /* This function only exists because X kbd events don't generate */
  /* SIGIO. Under curses, the SIGIO hander does the work of this */
  /* function. */
}

static void 
_io_wait_for_input ()
{
  pause ();
}

static int 
_io_read_kbd (buf, size)
     VOLATILE char *buf;
     int size;
{
  int r = read (0, buf, size);
  FD_CLR (0, &read_pending_fd_set);
  FD_CLR (0, &exception_pending_fd_set);
  return r;
}


#if defined(SIGIO)
static void 
_io_nodelay (delayp)
     int delayp;
{
  panic ("Trying to curses nodelay on a system with SIGIO.");
}

#else
static void 
_io_nodelay (delayp)
     int delayp;
{
  nodelay (stdscr, delayp);
}

#endif

static int 
_io_getch ()
{
  char ch;
  return ((io_read_kbd (&ch, 1) != 1)
	  ? EOF
	  : ch);
}


static int 
_io_get_chr (prompt)
     char *prompt;
{
  int x;
  mvaddstr (input, 0, prompt);
  clrtoeol ();
  topclear = 2;
  refresh ();
  ++term_cursor_claimed;
  x = get_chr ();
  --term_cursor_claimed;
  return x;
}

#define BUFFER 10

#define MORE(bytes)				\
  line_len+=bytes;				\
  line->buf=ck_realloc(sptr,line_len+1);	\
  eptr= eptr-sptr + line->buf;			\
  ptr= ptr-sptr + line->buf;			\
  sptr= line->buf;

#define KILLCH()				\
{						\
 if(ptr!=eptr) {				\
   char *tptr;					\
   for(tptr=ptr;tptr<=eptr;tptr++) \
     tptr[-1]= tptr[0];	\
 }					\
 --ptr;					\
 --eptr;					\
 --sfilled;				\
 --col;					\
 if(col<BUFFER)				\
   doscroll++;			\
 else if(!doscroll) {			\
   move(input,col);		\
   delch();			\
   if(eptr-sptr+plen>=COLS && ptr+(COLS-1-col)<eptr) {	\
     mvaddch(input,COLS-1,ptr[COLS-1-col]);\
     domove++;		\
   }				\
 }					\
}

static void 
local_putchar (ch)
     int ch;
{
  (void) putc (ch, stdout);
  /* (void)putchar(ch); */
}

static void
local_puts (s)
    char *s;
{
   (void) tputs (s, 1, (int (*)()) local_putchar);
}

static void 
beep ()
{
#ifndef HAVE_GETCAP
  putchar ('\007');
#else
  static char *vb;
  static int called = 0;

  if (!called)
    {
      called++;
      vb = getcap ("vb");
    }
  if (vb)
    {
      local_puts (vb);
    }
  else
    {
      local_putchar ('\007');
    }
#endif
}


/* Return zero on success,
   one on empty input,
   and two on abort */
static int 
_io_get_line (prompt, line)
     char *prompt;
     struct line *line;
{
  static int reenter = 0;
  static int over;		/* Overwrite or insert */
  char *ptr;			/* Current position in the line */
  char *sptr;			/* Start of the line */
  char *eptr;			/* End of the line */
  int line_len;			/* length of the line */
  int col = 0;			/* Current cursor column */
  int plen;			/* Length of the prompt */
  int domove = 0;
  int doscroll = 0;
  int c;
  int sfilled;
  char *in_str;
  int n;
  int do_free;
  char vbuf[50];
  int term_cursor_was_claimed = term_cursor_claimed;

  if (reenter)
    return 2;
  ++reenter;

  topclear = 2;

  if (macro_func_arg)
    {
      set_line (line, macro_func_arg);
      macro_func_arg = 0;
      --reenter;
      return 0;
    }
  if (!line->alloc)
    {
      line->alloc = LINE_MIN;
      line->buf = ck_malloc (LINE_MIN);
      line->buf[0] = '\0';
    }
  line_len = line->alloc - 1;	/* Leave room for the NULL */
  sptr = line->buf;

  sfilled = strlen (sptr);
  plen = strlen (prompt) + 3;
  ptr = sptr + sfilled;
  eptr = ptr;
  over = 0;

  input_active = 1;		/* This effects other-window. */
  term_cursor_claimed = 1;
  doscroll++;

  input_active = 1;
  for (;;)
    {
      if (domove)
	{
	  domove = 0;
	  if (col < BUFFER || col > COLS - BUFFER)
	    doscroll++;
	  else
	    move (input, col);
	}
      if (doscroll)
	{
	  doscroll = 0;
	  redrew = 0;
	  if (textout == 2)
	    {
	      /* [MORE] the error msg, so the luser has
		 a chance to see it. . . */
	      mvaddstr (input, COLS - 7, "[MORE]");
	      (void) get_chr ();
	    }
	  textout = 0;
	  move (input, 0);
	  clrtoeol ();
	  if (plen + (ptr - sptr) < COLS - BUFFER)
	    {
	      *eptr = 0;
	      printw ("%s:  %.*s", prompt, COLS - plen, sptr);
	      col = plen + (ptr - sptr);
	    }
	  else
	    {
	      char *p;
	      p = sptr + COLS - 2 * BUFFER - plen;
	      while (ptr - p >= COLS - BUFFER)
		p += COLS - 2 * BUFFER;
	      *eptr = 0;
	      printw ("%.*s", COLS, p);
	      col = ptr - p;
	    }
	  move (input, col);
	}
      if (how_many != 1)
	{
	  how_many = 1;
	  io_update_status ();
	  move (input, col);
	}
      term_cursor_claimed = input_active;
      if (!term_cursor_was_claimed && term_cursor_claimed)
	move (input, col);
      term_cursor_was_claimed = term_cursor_claimed;
      map_chr (input_active ? EDIT_MAP : NAVIGATE_MAP);
    swtch:
      if (cur_vector != EDIT_VECTOR)
	{
	  n = global_cmd (EDIT_MAP);
	  if (redrew || textout)
	    doscroll++;
	  if (n == -3)
	    {
	      *eptr++ = '\0';
	      input_active = 0;
	      term_cursor_claimed = 0;
	      --reenter;
	      input_active = 0;
	      window_after_input = -1;
	      return 2;
	    }
	  else if (n == -2)
	    {
	      beep ();
	      beep ();
	    }
	  else if (n >= 0)
	    goto swtch;
	  continue;
	}
      switch (cur_cmd - edit_funcs)
	{
	case L_BEG_LINE:
	  col -= ptr - sptr;
	  ptr = sptr;
	  domove++;
	  break;

	case L_BK_CHR:
	  for (n = 0; ptr != sptr && n < how_many; n++)
	    {
	      --ptr;
	      --col;
	    }
	  domove++;
	  break;

	case L_BK_WORD:
	  for (n = 0; ptr != sptr && n < how_many; n++)
	    {
	      while (ptr != sptr && !isalnum (ptr[-1]))
		{
		  --ptr;
		  --col;
		}
	      while (ptr != sptr && isalnum (ptr[-1]))
		{
		  --ptr;
		  --col;
		}
	    }
	  domove++;
	  break;

	case L_FW_DEL_CHR:
	  for (n = 0; ptr != eptr && n < how_many; n++)
	    {
	      ptr++;
	      col++;
	      KILLCH ()
	    }
	  break;

	case L_FW_DEL_WORD:
	  for (n = 0; ptr != eptr && n < how_many; n++)
	    {
	      while (ptr != eptr && !isalnum (ptr[0]))
		{
		  ptr++;
		  col++;
		  KILLCH ()
		}
	      while (ptr != eptr && isalnum (ptr[0]))
		{
		  ptr++;
		  col++;
		  KILLCH ()

		}
	    }
	  break;

	case L_END_LINE:
	  col += eptr - ptr;
	  ptr = eptr;
	  domove++;
	  break;

	case L_FW_CHR:
	  for (n = 0; ptr != eptr && n < how_many; n++)
	    {
	      col++;
	      ptr++;
	    }
	  domove++;
	  break;

	case L_FW_WORD:
	  for (n = 0; ptr != eptr && n < how_many; n++)
	    {
	      while (ptr != eptr && !isalnum (ptr[0]))
		{
		  col++;
		  ptr++;
		}
	      while (ptr != eptr && isalnum (ptr[0]))
		{
		  col++;
		  ptr++;
		}
	    }
	  domove++;
	  break;

	case L_FW_DEL_END:
	  eptr = ptr;
	  clrtoeol ();
	  break;

	case L_TOGGLE_OVER:
	  over = !over;
	  break;

	case L_BK_DEL_WORD:
	  for (n = 0; ptr != sptr && n < how_many; n++)
	    {
	      while (ptr != sptr && !isalnum (ptr[-1]))
		KILLCH ()

		  while (ptr != sptr && isalnum (ptr[-1]))
		  KILLCH ()
		  }

		  break;

	case L_BK_DEL_END:
	      while (ptr != sptr)
		KILLCH ()
		  break;

	case L_BK_DEL_CHR:
	      for (n = 0; ptr != sptr && n < how_many; n++)
		KILLCH ()
		  break;


	case L_FINISH:
	      *eptr++ = '\0';
	      textout = 1;
	      input_active = 0;
	      term_cursor_claimed = 0;
	      --reenter;
	      input_active = 0;
	      return eptr - sptr == 1 ? 1 : 0;

	case L_INS_EXPR:
	      {
		CELL *cp;

		if (!(cp = find_cell (curow, cucol)))
		  break;
		in_str = decomp (curow, cucol, cp);
		do_free = 1;
	      }
	      goto insert_string;

	case L_INS_VAL:
	      in_str = cell_value_string (curow, cucol);
	      do_free = 0;
	      goto insert_string;

	case L_INS_REL:
	      if (a0)
		{
		  if (mkrow != NON_ROW)
		    {
		      struct rng r;

		      set_rng (&r, curow, cucol, mkrow, mkcol);
		      in_str = range_name (&r);
		    }
		  else
		    in_str = cell_name (curow, cucol);
		}
	      else
		{
		  if (mkrow != NON_ROW)
		    {
		      switch (((curow == setrow) << 3)
			      + ((mkrow == setrow) << 2)
			      + ((cucol == setcol) << 1)
			      + (mkcol == setcol))
			{
			case 0:
			case 1:
			case 2:
			case 4:
			case 5:
			case 6:
			case 8:
			case 9:
			case 10:
			  sprintf (vbuf, "r[%+d:%+d]c[%+d:%+d]",
				   (curow < mkrow ? curow : mkrow) - setrow,
				   (curow < mkrow ? mkrow : curow) - setrow,
				   (cucol < mkcol ? cucol : mkcol) - setcol,
				   (cucol < mkcol ? mkcol : cucol) - setcol);
			  break;
			case 3:
			case 7:
			case 11:
			  sprintf (vbuf, "r[%+d:%+d]c",
				   (curow < mkrow ? curow : mkrow) - setrow,
				   (curow < mkrow ? mkrow : curow) - setrow);
			  break;
			case 12:
			case 14:
			case 13:
			  sprintf (vbuf, "rc[%+d:%+d]",
				   (cucol < mkcol ? cucol : mkcol) - setcol,
				   (cucol < mkcol ? mkcol : cucol) - setcol);
			  break;
			case 15:
			  strcpy (vbuf, "rc");
			  break;
#ifdef TEST
			default:
			  panic ("Unknown value");
#endif
			}
		    }
		  else
		    {
		      switch (((curow == setrow) << 1) + (cucol == setcol))
			{
			case 0:
			  sprintf (vbuf, "r[%+d]c[%+d]", curow - setrow, cucol - setcol);
			  break;
			case 1:
			  sprintf (vbuf, "r[%+d]c", curow - setrow);
			  break;
			case 2:
			  sprintf (vbuf, "rc[%+d]", cucol - setcol);
			  break;
			case 3:
			  strcpy (vbuf, "rc");
			  break;
#ifdef TEST
			default:
			  panic ("huh what");
#endif
			}
		    }
		  in_str = vbuf;
		}
	      do_free = 0;
	      goto insert_string;

	case L_INS_ABS:
	      /* Insert current cell/range name as an absolute reference */
	      if (a0)
		{
		  if (mkrow != NON_ROW)
		    sprintf (vbuf, "$%s$%u:$%s:$%u", col_to_str (cucol), curow, col_to_str (mkcol), mkrow);
		  else
		    sprintf (vbuf, "$%s$%u", col_to_str (cucol), curow);
		  in_str = vbuf;
		}
	      else
		{
		  if (mkrow != NON_ROW)
		    {
		      struct rng r;

		      set_rng (&r, curow, cucol, mkrow, mkcol);
		      in_str = range_name (&r);
		    }
		  else
		    in_str = cell_name (curow, cucol);
		}
	      do_free = 0;

	    insert_string:
	      c = strlen (in_str);
	      if ((sfilled + (over ? c + (ptr - eptr) : c)) >= line_len)
		{
		  n = over ? c + (ptr - eptr) : c;
		  if (n < LINE_MIN)
		    n = LINE_MIN;
		  MORE (n);
		}
	      if (over)
		{
		  if (ptr + c > eptr)
		    {
		      sfilled += (ptr + c) - eptr;
		      eptr = ptr + c;
		    }
		}
	      else
		{
		  if (ptr != eptr)
		    {
		      char *tptr;

		      for (tptr = eptr; tptr >= ptr; --tptr)
			tptr[c] = tptr[0];
		    }
		  eptr += c;
		  sfilled += c;
		}
	      bcopy (in_str, ptr, c);
	      ptr += c;
	      if (col + c >= COLS - BUFFER)
		{
		  doscroll++;
		}
	      else
		{
		  col += c;
		  if (!over && ptr != eptr)
		    while (c--)
		      insch (' ');
		  addstr (in_str);
		}
	      if (do_free)
		decomp_free ();
	      break;

	case L_INS_CHR:
	      if ((sfilled + how_many + (over ? (ptr - eptr) : (0))) >= line_len)
		{
		  n = over ? how_many + (ptr - eptr) : how_many;
		  if (n < LINE_MIN)
		    n = LINE_MIN;
		  MORE (n);
		}
	      if (over)
		{
		  if (ptr + how_many > eptr)
		    {
		      sfilled += (ptr + how_many) - eptr;
		      eptr = ptr + how_many;
		    }
		}
	      else
		{
		  if (ptr != eptr)
		    {
		      char *tptr;

		      for (tptr = eptr; tptr >= ptr; --tptr)
			tptr[how_many] = tptr[0];
		    }
		  eptr += how_many;
		  sfilled += how_many;
		}
	      for (n = 0; n < how_many; n++)
		*ptr++ = cur_chr;

	      if (col + how_many >= COLS - BUFFER)
		{
		  doscroll++;
		}
	      else if (over || ptr == eptr)
		{
		  col += how_many;
		  for (n = 0; n < how_many; n++)
		    addch (cur_chr);
		}
	      else
		{
		  col += how_many;
		  for (n = 0; n < how_many; n++)
		    {
		      insch (cur_chr);
		      move (input, col);
		    }
		}
	      break;
	    }
	}
#undef KILLCH
#undef BUFFER
#undef MORE
    }

#if __STDC__
  static void move_cursor_to (struct window *win, CELLREF r, CELLREF c, int dn)
#else
  static void
    move_cursor_to (win, r, c, dn)
  struct window *win;
  CELLREF r;
  CELLREF c;
  int dn;
#endif
  {
    int cc;
    int cell_cursor_col;
    int rr;
    int cell_cursor_row;

    cell_cursor_col = win->win_over;
    for (cc = win->screen.lc; cc < c; cc++)
      cell_cursor_col += get_width (cc);
    cell_cursor_row = win->win_down + dn;
    for (rr = win->screen.lr; rr < r; rr++)
      cell_cursor_row += get_height (rr);
    move (cell_cursor_row, cell_cursor_col);
  }

  void _io_update_status ()
  {
    CELL *cp;
    char *dec;
    char *ptr;
    static char hmbuf[40];
    int wid;
    int plen;
    int dlen;
    int yy, xx;

    if (!user_status)
        return;
      getyx (stdscr, yy, xx);
      move (status, 0);
      wid = COLS - 2;

    if (mkrow != NON_ROW)
      {
	struct rng r;

	  addch ('*');
	--wid;
	  set_rng (&r, curow, cucol, mkrow, mkcol);
	  ptr = range_name (&r);
      }
    else
        ptr = cell_name (curow, cucol);

    addstr (ptr);
    wid -= strlen (ptr);

    if (how_many != 1)
      {
	sprintf (hmbuf, " {%u}", how_many);
	addstr (hmbuf);
	wid -= strlen (hmbuf);
      }

    if ((cp = find_cell (curow, cucol)) && cp->cell_formula)
      {
	dec = decomp (curow, cucol, cp);
	dlen = strlen (dec);
      }
    else
      {
	dec = 0;
	dlen = 0;
      }

    ptr = cell_value_string (curow, cucol);
    plen = strlen (ptr);

    if (dec)
      {
	wid -= 4;
	if (dlen + plen > wid)
	  {
	    if (plen + 3 > wid)
	      printw (" %.*s... [...]", wid - 6, ptr);
	    else
	      printw (" %s [%.*s...]", ptr, wid - plen - 3, dec);
	  }
	else
	  printw (" %s [%s]", ptr, dec);
	decomp_free ();
      }
    else if (plen)
      {
	--wid;
	if (plen > wid)
	  printw (" %.*s...", wid - 3, ptr);
	else
	  printw (" %s", ptr);
      }

    clrtoeol ();
    move (yy, xx);
  }

#ifdef __STDC__
static void 
_io_info_msg (char * str, ...)
#else
static void 
_io_info_msg (str, va_alist)
     char *str;
     va_dcl
#endif
{
    va_list foo;
    char buf[1000];

    var_start (foo, str);
    vsprintf (buf, str, foo);
    if (!status)
      {
	if (textout == 2)
	  {
	    mvaddstr (input, COLS - 7, "[MORE]");
	    ++term_cursor_claimed;
	    (void) get_chr ();
	    --term_cursor_claimed;
	  }
	textout = 1;
	mvaddstr (input, 0, buf);
      }
    else
      mvaddstr (status, 0, buf);
    clrtoeol ();
    refresh ();
  }

#ifdef __STDC__
static void 
_io_error_msg (char * str, ...)
#else
static void 
_io_error_msg (str, va_alist)
     char *str;
     va_dcl
#endif
{
    va_list foo;
    char buf[1000];
    /* (fixme) Sigh.  What I'd give for vprintw() */

#ifdef TEST
    extern int isatty ();

    if (!dbg_do_stderr)
      dbg_do_stderr = isatty (fileno (stderr)) ? 1 : 2;
#endif
    var_start (foo, str);
    vsprintf (buf, str, foo);
#ifdef TEST
    if (dbg_do_stderr == 2)
      {
	fputs (buf, stderr);
	putc ('\n', stderr);
	fflush (stderr);
      }
#endif
    if (textout == 2)
      {
	mvaddstr (input, COLS - 7, "[MORE]");
	++term_cursor_claimed;
	(void) get_chr ();
	--term_cursor_claimed;
      }
    else
      textout = 2;
    mvaddstr (input, 0, buf);
    clrtoeol ();
    topclear = 1;
    refresh ();
  }

  static int nline;
  static int save_auto;
  extern int auto_recalc;

  static void _io_text_start ()
  {
    clear ();
    io_redisp ();
    nline = 0;
    save_auto = auto_recalc;
    auto_recalc = 0;
    redrew++;
    ++term_cursor_claimed;
  }

#ifdef __STDC__
static void 
_io_text_line (char * ptr, ...)
#else
static void 
_io_text_line (ptr, va_alist)
     char *ptr;
     va_dcl
#endif
{
    va_list ap;
    char sav;
    char buf[1000];

    var_start (ap, ptr);
    vsprintf (buf, ptr, ap);
    va_end (ap);
    ptr = buf;

    for (;;)
      {
	if (nline == LINES - 1)
	  {
	    move (nline, 0);
	    addstr ("Press a key to to proceed:  ");
	    refresh ();
	    (void) get_chr ();
	    clear ();
	    nline = 0;
	  }
	move (nline++, 0);
	if (strlen (ptr) <= COLS)
	  {
	    addstr (ptr);
	    break;
	  }
	sav = ptr[COLS];
	ptr[COLS] = '\0';
	addstr (ptr);
	ptr[COLS] = sav;
	refresh ();
	ptr += COLS;
      }
  }

  static void _io_text_finish ()
  {
    if (nline != 0)
      {
	move (nline, 0);
	addstr ("Press a key to continue:  ");
	refresh ();
	(void) get_chr ();
      }
    auto_recalc = save_auto;
    --term_cursor_claimed;
    io_repaint ();
  }

  static void _io_clear_input_before ()
  {
    textout = 0;
    if (topclear == 2)
      {
	move (input, 0);
	clrtoeol ();
	topclear = 0;
      }
    move (0, 0);
  }

  static void _io_clear_input_after ()
  {
    if (topclear)
      {
	move (input, 0);
	clrtoeol ();
	topclear = 0;
      }
  }


#if __STDC__
  static void _io_pr_cell_win (struct window *win, CELLREF r, CELLREF c, CELL *cp)
#else
  static void _io_pr_cell_win (win, r, c, cp)
  struct window *win;
  CELLREF r;
  CELLREF c;
  CELL *cp;
#endif
  {
    int glowing;
    int lenstr;
    int j;
    int wid, wwid;
    int hgt;
    char *ptr;
    int yy, xx;

    wid = get_width (c);
    if (!wid)
      return;
    if (wid > win->numc)
      wid = win->numc;
    hgt = get_height (r);
    if (!hgt)
      return;
    if (hgt > win->numr)
      hgt = win->numr;

    getyx (stdscr, yy, xx);
    glowing = (r == curow && c == cucol && win == cwin);
    ptr = print_cell (cp);
    move_cursor_to (win, r, c, 0);
    if (glowing)
      standout ();
    j = GET_JST (cp);
    if (j == JST_DEF)
      j = default_jst;
    lenstr = strlen (ptr);

    if (lenstr <= wid - 1)
      {
	CELLREF ccl, cch;

	if (j == JST_LFT)
	  printw ("%-*.*s", wid, wid - 1, ptr);
	else if (j == JST_RGT)
	  printw ("%*.*s ", wid - 1, wid - 1, ptr);
	else if (j == JST_CNT)
	  {
	    wwid = (wid - 1) - lenstr;
	    printw ("%*s%*s ", (wwid + 1) / 2 + lenstr, ptr, wwid / 2, "");
	  }
#ifdef TEST
	else
	  panic ("Unknown justification");
#endif
	if (glowing)
	  standend ();

	if (lenstr == 0 && c > win->screen.lc
	    && find_slop (win->win_slops, r, c - 1, &ccl, &cch))
	  {
	    CELLREF ccdl, ccdh;

	    if (find_slop (win->win_slops, r, c, &ccdl, &ccdh) && ccdl == c)
	      {
		kill_slop (win->win_slops, r, ccdl, ccdh);
		for (; ccdh != ccdl; --ccdh)
		  if (ccdh != c && (wwid = get_width (ccdh)))
		    {
		      move_cursor_to (win, r, ccdh, 0);
		      printw ("%*s", wwid, "");
		    }
	      }
	    kill_slop (win->win_slops, r, ccl, cch);
	    io_pr_cell (r, ccl, find_cell (r, ccl));
	  }
	else if (find_slop (win->win_slops, r, c, &ccl, &cch))
	  {
	    kill_slop (win->win_slops, r, ccl, cch);
	    for (; cch != ccl; --cch)
	      if (cch != c && (wwid = get_width (cch)))
		{
		  move_cursor_to (win, r, cch, 0);
		  printw ("%*s", wwid, "");
		}
	    io_pr_cell (r, ccl, find_cell (r, ccl));
	  }
      }
    else
      {
	CELLREF cc = c;
	CELL *ccp;
	CELLREF ccl, cch;

	for (wwid = wid; lenstr > wwid - 1; wwid += get_width (cc))
	  {
	    if (++cc > win->screen.hc
		|| ((ccp = find_cell (r, cc))
		    && GET_TYP (ccp)
		    && (GET_FMT (ccp) != FMT_HID
			|| (GET_FMT (ccp) == FMT_DEF
			    && default_fmt != FMT_HID))))
	      {
		--cc;
		break;
	      }
	  }

	if (lenstr > wwid - 1)
	  if (GET_TYP (cp) == TYP_FLT)
	    ptr = adjust_prc (ptr, cp, wwid - 1, wid - 1, j);
	  else if (GET_TYP (cp) == TYP_INT)
	    ptr = (char *) numb_oflo;

	if (wwid == 1)
	  {
	    addch (' ');
	    if (glowing)
	      standend ();
	  }
	else if (wwid == wid)
	  {
	    printw ("%-*.*s ", wwid - 1, wwid - 1, ptr);
	    if (glowing)
	      standend ();
	  }
	else if (glowing)
	  {
	    printw ("%.*s", wid, ptr);
	    standend ();
	    printw ("%-*.*s ", wwid - wid - 1, wwid - wid - 1, ptr + wid);
	  }
	else if (r == curow && (cucol > c && cucol <= cc))
	  {
	    CELLREF ctmp;
	    int w_left;
	    int w_here;

	    w_left = wid;
	    for (ctmp = c + 1; ctmp < cucol; ctmp++)
	      w_left += get_width (ctmp);
	    printw ("%.*s", w_left, ptr);
	    standout ();
	    w_here = get_width (cucol);
	    if (wwid > w_left + w_here)
	      {
		printw ("%-*.*s", w_here, w_here, ptr + w_left);
		standend ();
		printw ("%-*.*s ",
		 wwid - (w_left + w_here) - 1, wwid - (w_left + w_here) - 1,
			ptr + w_left + w_here);
	      }
	    else
	      {
		printw ("%-*.*s", w_here, w_here - 1, ptr + w_left);
		standend ();
	      }
	  }
	else
	  printw ("%-*.*s ", wwid - 1, wwid - 1, ptr);

	if (find_slop (win->win_slops, r, c, &ccl, &cch))
	  {
	    change_slop (win->win_slops, r, ccl, cch, c, cc);
	    for (; cch > cc; --cch)
	      if (wwid = get_width (cch))
		{
		  move_cursor_to (win, r, cch, 0);
		  printw ("%*s", wwid, "");
		}
	    for (cch = c - 1; cch > ccl; --cch)
	      if (wwid = get_width (cch))
		{
		  move_cursor_to (win, r, cch, 0);
		  printw ("%*s", wwid, "");
		}
	    if (ccl != c)
	      io_pr_cell (r, ccl, find_cell (r, ccl));
	  }
	else
	  set_slop ((VOIDSTAR *) (&(win->win_slops)), r, c, cc);
      }
    if (hgt > 1)
      {
	move_cursor_to (win, r, c, 1);
	ptr = decomp (r, c, cp);
	printw ("%.*s ", wid - 1, ptr);
	decomp_free ();
      }
    if (glowing)
      io_update_status ();
    move (yy, xx);
  }



#ifdef __STDC__
static void
_io_flush (void)
#else
static void
_io_flush ()
#endif
{
  refresh ();
}

#ifdef __STDC__
void
tty_graphics (void)
#else
void
tty_graphics ()
#endif
{

  FD_SET (0, &read_fd_set);
  FD_SET (0, &exception_fd_set);
  IO_SETUP;
}
