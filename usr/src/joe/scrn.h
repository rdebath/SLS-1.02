/* Device independant tty interface for JOE
   Copyright (C) 1992 Joseph H. Allen

This file is part of JOE (Joe's Own Editor)

JOE is free software; you can redistribute it and/or modify it under the 
terms of the GNU General Public License as published by the Free Software 
Foundation; either version 1, or (at your option) any later version.  

JOE is distributed in the hope that it will be useful, but WITHOUT ANY 
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more 
details.  

You should have received a copy of the GNU General Public License along with 
JOE; see the file COPYING.  If not, write to the Free Software Foundation, 
675 Mass Ave, Cambridge, MA 02139, USA.  */ 

#ifndef _Iscrn
#define _Iscrn 1

#include "config.h"
#include "termcap.h"
#include "tty.h"

typedef struct seq SEQ;
typedef struct scrn SCRN;

/* Number of key sequence translation entries */

#define NKEYS 20

/* Sepecial key sequence structure */

struct seq
 {
 char *seq;
 int code;
 char *name;
 };

extern SEQ seqs[];

/* Each terminal has one of these */

struct scrn
 {
 CAP *cap;		/* Termcap/Terminfo data */

 int li;			/* Screen height */
 int co;			/* Screen width */

 char *ti;			/* Initialization string */
 char *cl;			/* Home and clear screen... really an
 				   init. string */
 char *cd;			/* Clear to end of screen */
 char *te;			/* Restoration string */

 int hz;			/* Terminal can't print ~s */
 int os;			/* Terminal overstrikes */
 int eo;			/* Can use blank to erase even if os */
 int ul;			/* _ overstrikes */
 int am;			/* Terminal has autowrap, but not magicwrap */
 int xn;			/* Terminal has magicwrap */

 char *so;			/* Enter standout (inverse) mode */
 char *se;			/* Exit standout mode */

 char *us;			/* Enter underline mode */
 char *ue;			/* Exit underline mode */
 char *uc;			/* Single time underline character */

 int ms;			/* Ok to move when in standout/underline mode */

 char *mb;			/* Enter blinking mode */
 char *md;			/* Enter bold mode */
 char *mh;			/* Enter dim mode */
 char *mr;			/* Enter inverse mode */
 char *me;			/* Exit above modes */

 int da, db;			/* Extra lines exist above, below */
 char *al, *dl, *AL, *DL;	/* Insert/delete lines */
 char *cs;			/* Set scrolling region */
 int rr;			/* Set for scrolling region relative addressing */
 char *sf, *SF, *sr, *SR;	/* Scroll */

 char *dm, *dc, *DC, *ed;	/* Delete characters */
 char *im, *ic, *IC, *ip, *ei;	/* Insert characters */
 int mi;			/* Set if ok to move while in insert mode */

 char *bs;			/* Move cursor left 1 */
 int cbs;
 char *lf;			/* Move cursor down 1 */
 int clf;
 char *up;			/* Move cursor up 1 */
 int cup;
 char *nd;			/* Move cursor right 1 */

 char *ta;			/* Move cursor to next tab stop */
 int cta;
 char *bt;			/* Move cursor to previous tab stop */
 int cbt;
 int tw;			/* Tab width */

 char *ho;			/* Home cursor to upper left */
 int cho;
 char *ll;			/* Home cursor to lower left */
 int cll;
 char *cr;			/* Move cursor to left edge */
 int ccr;
 char *RI;			/* Move cursor right n */
 int cRI;
 char *LE;			/* Move cursor left n */
 int cLE;
 char *UP;			/* Move cursor up n */
 int cUP;
 char *DO;			/* Move cursor down n */
 int cDO;
 char *ch;			/* Set cursor column */
 int cch;
 char *cv;			/* Set cursor row */
 int ccv;
 char *cb;			/* Goto beginning of specified line */
 int ccb;
 char *cm;			/* Set cursor row and column */
 int ccm;

 char *ce;			/* Clear to end of line */
 int cce;

 /* Key-sequence translation table */

 struct
  {
  char *s;			/* Key sequence string */
  int l;			/* Key sequence string length */
  int n;			/* Value which should be returned for this string */
  } ktab[NKEYS];

 int tabsize;			/* Number of entries in translation table */

 /* Input buffer for translations */

 char kbuf[32];			/* Keyboard buffer */
 int kbufp;			/* Keyboard buffer index */
 int dumpptr;			/* When we pass unmatched chars */

 /* Current state of terminal */
 int *scrn;			/* Current contents of screen */
 int x,y;			/* Current cursor position (-1 for unknown) */
 int top,bot;			/* Current scrolling region */
 int attrib;			/* Current character attributes */
 int ins;			/* Set if we're in insert mode */

 int *updtab;			/* Dirty lines table */
 int avattr;			/* Bits set for available attributes */
 int *sary;			/* Scroll buffer array */
 };

/* SCRN *nopen(void);
 *
 * Open the screen (sets TTY mode so that screen may be used immediatly after
 * the 'nopen').
 */
SCRN *nopen();

/* void nresize(SCRN *t,int w,int h);
 *
 * Change size of screen.  For example, call this when you find out that
 * the Xterm changed size.
 */
void nresize();

/* void nredraw(SCRN *t);
 *
 * Invalidate all state variables for the terminal.  This way, everything gets
 * redrawn.
 */
void nredraw();

void nescape();
void nreturn();

/* void nclose(SCRN *t);
 *
 * Close the screen and restore TTY to initial state.
 *
 * if 'flg' is set, tclose doesn't mess with the signals.
 */
void nclose();

/* int ngetc(SCRN *t);
 *
 * Get next input character.  Arrow keys are translated into the integer codes
 * shown below.
 */
int ngetc();

#define KEYUP 256	/* Arrow keys */			/* ku */
#define KEYDOWN 257						/* kd */
#define KEYLEFT 258						/* kl */
#define KEYRIGHT 259						/* kr */
#define KEYF0 260	/* Function keys (is F0 really F10?) */ /* k0 */
#define KEYF1 261						/* k1 */
#define KEYF2 262
#define KEYF3 263
#define KEYF4 270
#define KEYF5 265
#define KEYF6 266
#define KEYF7 267
#define KEYF8 268
#define KEYF9 269						/* k9 */
#define KEYDEL 383	/* Delete character */			/* kD */
#define KEYINS 271	/* Insert character */			/* kI */
#define KEYHOME 272	/* Home key */				/* kh */
#define KEYEND 273	/* End key */				/* kH */
#define KEYPGDN 274	/* Page down key */			/* kN */
#define KEYPGUP 275	/* Page up key */			/* kP */
#define KEYBACKS 264	/* Backspace key */			/* kb */

/* void cpos(SCRN *t,int x,int y);
 *
 * Set cursor position
 */
void cpos();

/* void attr(SCRN *t,int a);
 *
 * Set attributes
 */
void attr();

/* void outatr(SCRN *t,int x,int y,int c);
 *
 * Output a character at the given screen cooridinate.  The cursor position
 * after this function is executed is indeterminate.
 */
/* void outatr(); */
#define outatr(ztz,i,j,c) \
 ( \
  ( ( (ztz)->x!=(i) || (ztz)->y!=(j) ) ? (cpos(ztz,(i),(j)),0) : 0 ), \
  ( ((c)&~255)!=(ztz)->attrib ? (attr(ztz,(c)&~255),0) : 0 ), \
  ++ztz->x, \
  ttputc(c) \
 )

/* Character attribute bits */

#define INVERSE 256
#define UNDERLINE 512
#define BOLD 1024
#define BLINK 2048
#define DIM 4096

/* int eraeol(SCRN *t,int x,int y);
 *
 * Erase from screen coordinate to end of line.
 */
int eraeol();

/* void nscrlup(SCRN *t,int top,int bot,int amnt);
 *
 * Buffered scroll request.  Request that some lines up.  'top' and 'bot'
 * indicate which lines to scroll.  'bot' is the last line to scroll + 1.
 * 'amnt' is distance in lines to scroll.
 */
void nscrlup();

/* void nscrldn(SCRN *t,int top,int bot,int amnt);
 *
 * Buffered scroll request.  Scroll some lines down.  'top' and 'bot'
 * indicate which lines to scroll.  'bot' is the last line to scroll + 1.
 * 'amnt' is distance in lines to scroll.
 */
void nscrldn();

/* void nscroll(SCRN *t);
 *
 * Execute buffered scroll requests
 */
void nscroll();
/* Macros to get width and height of screen */
#define NWIDTH(t) ((t)->co)
#define NHEIGHT(t) ((t)->li)

#endif
