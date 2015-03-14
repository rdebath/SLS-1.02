/* Window management
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

#ifndef _Iw
#define _Iw 1

#include "config.h"
#include "queue.h"
#include "kbd.h"
#include "scrn.h"
#include "b.h"

typedef struct watom WATOM;
typedef struct screen SCREEN;
typedef struct window W;

struct watom
 {
 CONTEXT *context;
 void (*disp)();
 void (*follow)();
 void (*kill)();
 void (*resize)();
 void (*move)();
 void (*ins)();
 void (*del)();
 int what;		/* Type of this thing */
 };

struct screen
 {
 SCRN *t;			/* Screen data on this screen is output to */

 int wind;			/* Number of help lines on this screen */
 
 W *topwin;			/* Top-most window showing on screen */
 W *curwin;			/* Window cursor is in */

 int w,h;			/* Width and height of this screen */
 
 char *pattern;			/* pattern being searched for */
 char *replace;			/* what to replace with */
 int options;			/* Search/Replace options */
 int repeat;
 int foundlen;
 
 P *markb;			/* Beginning and end of marked block */
 P *markk;

 int arg;
 };

struct window
 {
 LINK(W) link;			/* Linked list of windows in order they
 				   appear on the screen */
 SCREEN *t;			/* Screen this thing is on */
 int x,y,w,h;			/* Position and size of window */
                                /* Currently, x=0, w=width or screen. */
                                /* y== -1 if window is not on screen */
 int ny,nh;			/* Temporary values for wfit */
 int reqh;			/* Requested new height or 0 for same */
 				/* This is an argument for wfit */
 int hh;			/* Height before help was turned on */
 W *win;			/* Window this one operates on */
 W *main;			/* Main window of this family */
 W *orgwin;			/* Window where space from this window came */
 int curx, cury;		/* Cursor position within window */
 KBD *kbd;			/* Keyboard handler for this window */
 WATOM *watom;			/* The type of this window */
 void *object;			/* Object which inherits this */

 char *msgt;			/* Message at top of window */

 char *msgb;			/* Message at bottom of window */

 char *huh;			/* Name of window for context sensitive hlp */
 };

/***************/
/* Subroutines */
/***************/

void scrlup();
void scrldn();
void prming();

/* int getgrouph(W *);
 * Get height of a family of windows
 */
int getgrouph();

/* W *findtopw(W *);
 * Find first (top-most) window of a family
 */
W *findtopw();

/* W *findbotw(W *);
 * Find last (bottom-most) window a family
 */
W *findbotw();

W *lastw();

/* void wfit(SCREEN *);
 *
 * Fit all of the windows onto the screen
 */
void wfit();

/*****************/
/* Main routines */
/*****************/

/* SCREEN *screate(SCRN *);
 *
 * Create a screen
 */
SCREEN *screate();

/* void sresize(SCREEN *t);
 * Screen size changed
 */
void sresize();

/* W *wcreate(SCREEN *t,WATOM *watom,W *where,W *target,W *original,int height);
 *
 * Try to create a window
 *
 * 't'		Is the screen the window is placed on
 * 'watom'	Type of new window
 * 'where'	The window placed after this window, or if 'where'==0, the
 *		window is placed on the end of the screen
 * 'target'	The window operates on this window.  The window becomes a
 *		member of 'target's family or starts a new family if
 *		'target'==0.
 * 'original'	Attempt to get 'height' from this window.  When the window is
 *              aborted, the space gets returned to 'original' if it still
 *		exists.  If 'original'==0, the window will force other
 *		windows to go off of the screen.
 * 'height'	The height of the window
 *
 * Returns the new window or returns 0 if there was not enough space to
 * create the window and maintain family integrity.
 */
W *wcreate();

/* void wabort(W *w);
 *
 * Kill a window and it's children
 */
void wabort();

/* int wnext(SCREEN *);
 *
 * Switch to next window
 */
int wnext();

/* int wprev(SCREEN *);
 *
 * Switch to previous window
 */
int wprev();

/* int wgrow(W *);
 *
 * increase size of window.  Return 0 for success, -1 for fail.
 */
int wgrow();

/* int wshrink(W *);
 *
 * Decrease size of window.  Returns 0 for success, -1 for fail.
 */
int wshrink();

/* void wshowone(W *);
 *
 * Show only one window on the screen
 */
void wshowone();

/* void wshowall(SCREEN *);
 *
 * Show all windows on the screen, including the given one
 */
void wshowall();

/* void wredraw(W *);
 *
 * Force complete redraw of window
 */
void wredraw();

/* void wsquish(SCREEN *,int h);
 *
 * Squish all of the windows on the screen so that h lines can fit
 */
void wsquish();

/* void wrestore(SCREEN *);
 *
 * Restore windows to their original heights
 */
void wrestore();

/* void msg(W *w,char *text);
 * Display a message and then continue editing after the next keypress.
 * The message is placed in the last line of 'w'
 */
int msgout();
void msg();

void msgnw();
void msgnwt();

/* int query(W *w,char *text);
 * Single-key query.  'func' gets the key as it's first arg.  The message is
 * displayed on the last line of 'w'.
 */
int query();

/* int queryn(W *w,char *text);
 * As above, but leave cursor in current window
 */
int queryn();

#endif
