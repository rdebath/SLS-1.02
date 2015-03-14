/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 */
#include <sys/types.h>
#ifdef _POSIX_SOURCE
#  include <stdlib.h>
#  include <unistd.h>
#  undef NULL
#endif
#include <time.h>
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include "window.h"
#include "keyserv.h"
#include "minicom.h"
#include "charmap.h"

/*
 * This file is called vt100.c, but in fact it interprets
 * the escape code of three displays, namely:
 * vt100, minix-am (sortof vt 220), ansi-subset (ansi-bbs).
 */

/*
 * The global variable esc_s holds the escape sequence status:
 * 0 - normal
 * 1 - ESC
 * 2 - ESC [
 * 3 - ESC [ ?
 * 4 - ESC (
 * 5 - ESC )
 * 6 - ESC #
 * 7 - ESC P
 */
static int esc_s = 0;

#define ESC 27
#define NORMAL 1
#define APPL 2

/* parameters and index into the array */
static int escparms[8];
static int ptr = -2;

static int keypadmode = NORMAL;
static int cursormode = NORMAL;

/* New size of scrolling region. */
short newy1 = 0;
short newy2 = 23;

extern char *ibmchar; /* Initializedin window.c */
static int ibmcode[] = { 201, 205, 187, 200, 186, 188, 218, 196, 191, 192, 179,
			 217, 199, 182, 137, 135, 144, 129, 136, 136, 134, 143,
			 141, 142, 126, 128 };
/*
 * Reset some internals of this emulator code.
 */
void init_vt()
{
  newy1 = 0;
  newy2 = 23 + (terminal == MINIX);
  wresetregion(us);
  keyserv(KKPST, 0);
  keyserv(KCURST, 0);
  keypadmode = NORMAL;
  cursormode = NORMAL;
  if (terminal == ANSI) {
	wsetfgcol(us, WHITE);
 	wsetbgcol(us, BLACK);
  } else {
 	wsetfgcol(us, SFG);
 	wsetbgcol(us, SBG);
  }
}

/*
 * Output a string to the serial port.
 */
void termout(s)
char *s;
{
  write(portfd, s, strlen(s));
}

/*
 * Escape code handling.
 */

/*
 * ESC was seen the last time. Process the next character.
 */
static void state1(c)
int c;
{
  short x, y, f;
  static short savex = 0, savey = 0, saveattr = A_NORMAL, savecol = 112;

  switch(c) {
	case '[': /* ESC [ */
		esc_s = 2;
		return;
	case '(': /* ESC ( */
		esc_s = 4;
		return;
	case ')': /* ESC ) */
		esc_s = 5;
		return;
	case '#': /* ESC # */
		esc_s = 6;
		return;
	case 'P': /* ESC P (DCS, Device Control String) */
		esc_s = 7;
		return;
	case 'D': /* Cursor down */
	case 'M': /* Cursor up */
		x = us->curx;
		if (c == 'D')  { /* Down. */
			y = us->cury + 1;
			if (y == newy2 + 1)
				wscroll(us, S_UP);
			else if (us->cury < lines)
				wlocate(us, x, y);
		}		
		if (c == 'M')  { /* Up. */
			y = us->cury - 1;
			if (y == newy1 - 1)
				wscroll(us, S_DOWN);
			else if (y >= 0)
				wlocate(us, x, y);
		}
		break;
	case 'E': /* CR + NL */
 		wputs(us, "\r\n");
 		break;
 	case '7': /* Save attributes and cursor position */
 	case 's':
 		savex = us->curx;
 		savey = us->cury;
 		saveattr = us->attr;
 		savecol = us->color;
 		break;
 	case '8': /* Restore them */
 	case 'u':	
 		us->color = savecol; /* HACK should use wsetfgcol etc */
 		wsetattr(us, saveattr);
 		wlocate(us, savex, savey);
 		break;
 	case '=': /* Keypad into applications mode */
 		keypadmode = APPL;
 		keyserv(KKPAPP, 0);
 		break;
 	case '>': /* Keypad into numeric mode */
 		keypadmode = NORMAL;
 		keyserv(KKPST, 0);
 		break;
 	case 'Z': /* Report terminal type */
		if (terminal == VT100)
			termout("\033[?1;0c");
 		else	
 			termout("\033[?c");
 		break;	
 	case 'c': /* Reset to initial state */
 		f = A_NORMAL;
 		wsetattr(us, f);
 		wlocate(us, 0, 0);
		us->wrap = (terminal != VT100);
		if (linewrap != -1) us->wrap = linewrap;
		init_vt();
 		break;
 	case 'N': /* G2 character set for next character only*/
 	case 'O': /* G3 "				"    */
 	case 'H': /* Set tab in current position */
 	case '<': /* Exit vt52 mode */
 	default:
 		/* ALL IGNORED */
 		break;
  }
  esc_s = 0;
  return;
}

/*
 * ESC [ ... was seen the last time. Process next character.
 */
static void state2(c)
int c;
{
  short x, y, attr, f;
  char temp[16];
  char did_esc = 1;

  /* See if a number follows */
  if (c >= '0' && c <= '9') {
	if (ptr < 0) ptr = 0;
	escparms[ptr] = 10*(escparms[ptr]) + c - '0';
	return;
  }
  /* Separation between numbers ? */
  if (c == ';') {
	if (ptr < 15 && ptr >= 0) ptr++;
	return;
  }
  /* ESC [ something-without-argument? */
  if (ptr < 0) switch(c) {
	case '?': /* ESC [ ? */
		esc_s = 3;
		return;
	case 'K': /* Clear to end of line */
		wclreol(us);
		break;
	case 'J': /* Clear to end of screen */
		wclreos(us);
		break;
	case 'c': /* Identify Terminal Type */
		if (terminal == VT100)
			termout("\033[?1;0c");
		else	
			termout("\033[?c");
		break;	
	case 'i': /* print page */
	case 'g': /* clear tab stop */
		/* IGNORED */
		break;
	default: /* We did not find it. Maybe it's a variable-argument func. */
		did_esc = 0;
		break;
  }
  if (ptr < 0 && did_esc) {
	esc_s = 0;
	return;
  }
  /* ESC [ one-argument-only something ? */
  if (ptr == 0) switch(c) {
	case 'K': /* Line erasing */
		switch(escparms[0]) {
			case 0:
				wclreol(us);
				break;
			case 1:
				wclrbol(us);
				break;
			case 2:
				wclrel(us);
				break;
		}
		break;
	case 'J': /* Screen erasing */
		x = us->color;
		y = us->attr;
		if (terminal == ANSI) {
			wsetattr(us, A_NORMAL);
			wsetfgcol(us, WHITE);
			wsetbgcol(us, BLACK);
		}
		switch(escparms[0]) {
			case 0:
				wclreos(us);
				break;
			case 1:
				wclrbos(us);
				break;
			case 2:
				winclr(us);
				break;
		}
		if (terminal == ANSI) {
			us->color = x;
			us->attr = y;
		}
		break;
	case 'n': /* Requests / Reports */
		switch(escparms[0]) {
			case 5: /* Status */
				termout("\033[0n");
				break;
			case 6:	/* Cursor Position */
				sprintf(temp, "\033[%d;%dR", 
					us->cury + 1, us->curx + 1);
				termout(temp);
				break;
		}
		break;
	case 'c': /* Identify Terminal Type */
		if (terminal == VT100) {
			termout("\033[?1;0c");
			break;
		}
		termout("\033[?c");
		break;
	case 'g': /* Tabulation */
	case 'i': /* Printing */
	default:
		/* IGNORED */
		break;
  }

  /* Process functions with zero, one, two or more arguments */
  switch(c) {
	case 'A':
	case 'B':
	case 'C':
	case 'D': /* Cursor motion */
		if ((f = escparms[0]) == 0) f = 1;
		x = us->curx;
		y = us->cury;
		x += f * ((c == 'C') - (c == 'D'));
		if (x < 0) x = 0;
		if (x >= cols) x = cols - 1;
		if (c == 'B') { /* Down. */
			y += f;
			if (y >= lines) y = lines - 1;
			if (y == newy2 + 1) y = newy2;
		}
		if (c == 'A') { /* Up. */
			y -= f;
	 		if (y < 0) y = 0;
			if (y == newy1 - 1) y = newy1;
		}	
		wlocate(us, x, y);
		break;
	case 'H':
	case 'f': /* Set cursor position */
		if ((y = escparms[0]) == 0) y = 1;
		if ((x = escparms[1]) == 0) x = 1;
		wlocate(us, x - 1, y - 1);
		break;
	case 'm': /* Set attributes */
		  /* Without argument, esc-parms[0] is 0 */
		if (ptr < 0) ptr = 0;  
		attr = wgetattr((us));
		for (f = 0; f <= ptr; f++) {
		    if (escparms[f] >= 30 && escparms[f] <= 37)
			wsetfgcol(us, escparms[f] - 30);
		    if (escparms[f] >= 40 && escparms[f] <= 47)
			wsetbgcol(us, escparms[f] - 40);
		    switch(escparms[f]) {
			case 0:
				attr = A_NORMAL;
				if (terminal == ANSI) {
					wsetfgcol(us, WHITE);
					wsetbgcol(us, BLACK);
				} else {
					wsetfgcol(us, SFG);
					wsetbgcol(us, SBG);
				}
				break;
			case 4:
				attr |= A_UNDERLINE;
				break;
			case 7:
				attr |= A_REVERSE;
				break;
			case 1:
				attr |= A_BOLD;
				break;
			case 5:
				attr |= A_BLINK;
				break;
			case 22: /* Bold off */
				attr &= ~A_BOLD;
				break;
			case 24: /* Not underlined */
				attr &=~A_UNDERLINE;
				break;
			case 25: /* Not blinking */
				attr &= ~A_BLINK;
				break;
			case 27: /* Not reverse */
				attr &= ~A_REVERSE;
				break;
			case 39: /* Default fg color */
				if (terminal == ANSI)
					wsetfgcol(us, WHITE);
				else
					wsetfgcol(us, SFG);
				break;
			case 49: /* Default bg color */
				if (terminal == ANSI)
					wsetbgcol(us, BLACK);
				else
					wsetbgcol(us, SBG);
				break;
				
		    }
		}
		wsetattr(us, attr);
		break;
	case 'L': /* Insert lines */
		if ((x = escparms[0]) == 0) x = 1;
		for(f = 0; f < x; f++)
			winsline(us);
		break;	
	case 'M': /* Delete lines */
		if ((x = escparms[0]) == 0) x = 1;
		for(f = 0; f < x; f++)
			wdelline(us);
		break;	
	case 'P': /* Delete Characters */
		if ((x = escparms[0]) == 0) x = 1;
		for(f = 0; f < x; f++)
			wdelchar(us);
		break;	
	case '@': /* Insert Characters */		
		if ((x = escparms[0]) == 0) x = 1;
		for(f = 0; f < x; f++)
			winschar(us);
		break;	
	case 'r': /* Set scroll region */
		if ((newy1 = escparms[0]) == 0) newy1 = 1;
		if ((newy2 = escparms[1]) == 0) newy2 = 1;
		newy1-- ; newy2--;

		wsetregion(us, newy1, newy2);
		break;
	case 'y': /* Self test modes */
	default:
		/* IGNORED */
		break;
  }
  /* Ok, our escape sequence is all done */
  esc_s = 0;
  ptr = -2;
  return;		
}
  
/*
 * ESC [ ? ... seen.
 */
static void state3(c)
int c;
{
  /* See if a number follows */
  if (c >= '0' && c <= '9') {
	if (ptr < 0) ptr = 0;
	escparms[ptr] = 10*(escparms[ptr]) + c - '0';
	return;
  }
  /* ESC [ ? number seen */
  if (ptr < 0) {
	esc_s = 0;
	return;
  }
  switch(c) {
	case 'h':
		switch(escparms[0]) {
			case 7: /* Auto wrap on (automatic margins) */
				us->wrap = 1;
				break;
			case 6: /* Set scroll region */
				wsetregion(us, newy1, newy2);
				break;
			case 1: /* Cursor keys in appl. mode */
				cursormode = APPL;
				keyserv(KCURAPP, 0);
				break;
			case 25: /* Cursor on */
				wcursor(us, CNORMAL);
				break;	
			default: /* Mostly set up functions */
				/* IGNORED */
				break;
		}
		break;
	case 'l':
		switch(escparms[0]) {
			case 7: /* Auto wrap off */
				us->wrap = 0;
				break;
			case 6: /* Whole screen mode */
				newy1 = 0;
				newy2 = 23 + (terminal == MINIX);
				wresetregion(us);
				break;
			case 1: /* Cursor keys in cursor pos. mode */
				cursormode = NORMAL;
				keyserv(KCURST, 0);
				break;
			case 25: /* Cursor off */
				wcursor(us, CNONE);
				break;
			default: /* Mostly set up functions */
				/* IGNORED */
				break;
		}
		break;
	case 'i': /* Printing */
	case 'n': /* Request printer status */
	default:
		/* IGNORED */
		break;
  }
  esc_s = 0;
  ptr = -2;
  return;
}

/*
 * ESC ( Seen.
 */
static void state4(c)
int c;
{
  /* Switch Character Sets. */
  /* IGNORED */
  esc_s = 0;
}

/*
 * ESC ) Seen.
 */
static void state5(c)
int c;
{
  /* Switch Character Sets. */
  /* IGNORED */
  esc_s = 0;
}

/*
 * ESC # Seen.
 */
static void state6(c)
int c;
{
  /* Double Height and Double width character sets */
  /* IGNORED */
  esc_s = 0;
}

/*
 * ESC P Seen.
 */
static void state7(c)
int c;
{
  /*
   * Device dependant control strings. The Minix virtual console package
   * uses these sequences. We can only turn cursor on or off, because
   * that's the only one supported in termcap. The rest is ignored.
   */
  static char buf[17];
  static int pos = 0;
  static int state = 0;

  if (c == ESC) {
  	state = 1;
  	return;
  }
  if (state == 1) {
  	buf[pos] = 0;
  	pos = 0;
  	state = 0;
  	esc_s = 0;
  	if (c != '\\') return;
  	/* Process string here! */
  	if (!strcmp(buf, "cursor.on")) wcursor(us, CNORMAL);
  	if (!strcmp(buf, "cursor.off")) wcursor(us, CNONE);
  	if (!strcmp(buf, "linewrap.on")) {
  		linewrap = -1;
  		us->wrap = 1;
  	}
  	if (!strcmp(buf, "linewrap.off")) {
  		linewrap = -1;
  		us->wrap = 0;
  	}
  	return;
  }
  if (pos > 15) return;
  buf[pos++] = c;
}

void out_vt100(ch)
int ch;
{
  int f;
  short x;
  unsigned char c;

  if (!ch) return;

  if (ptr == -2) { /* Initialize */
	ptr = -1;
	for(f = 0; f < 8; f++) escparms[f] = 0;
  }

  c = (unsigned char)ch;
  
  switch(esc_s) {
	case 0: /* Normal character */
		switch(c) {
			case '\r': /* Carriage return */
				wputc(us, c);
				if (addlf) {
					wputc(us, '\n');
					if (docap) fputc('\n', capfp);
				}
				break;
			case '\t': /* Non - destructive TAB */
				x = ((us->curx / 8) + 1) * 8;
				wlocate(us, x, us->cury);
				if (docap) fputc(c, capfp);
				break;
			case 013: /* Old Minix: CTRL-K = up */
				wlocate(us, us->curx, us->cury - 1);
				break;
			case 014: /* Old Minix: CTRL-L = right */
				wlocate(us, us->curx + 1, us->cury);
				break;
			case ESC: /* Begin escape sequence */
				esc_s = 1;
				break;
			default: /* Printable character */
				if (c < 32) c  = charmap[c];
				  else if (c > 127) {
					for(f = 0; f < 26; f++)
						if (ibmcode[f] == c) {
							c = ibmchar[f];
							break;
						}
					if (f == 26) c = charmap[c];
				}	
			case '\n': /* Printable by wputc too */
			case '\b':
			case 7: /* Bell */
				wputc(us, c);
				if (docap)
					fputc(c, capfp);
				break;
		}
		break;
	case 1: /* ESC seen */
		state1(c);
		break;
	case 2: /* ESC [ ... seen */
		state2(c);
		break;
	case 3:
		state3(c);
		break;
	case 4:
		state4(c);
		break;
	case 5:
		state5(c);
		break;
	case 6:
		state6(c);
		break;
	case 7:
		state7(c);
		break;	
  }
}
