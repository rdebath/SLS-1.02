/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* static data items for window manager */

/*{{{}}}*/
/*{{{  #includes*/
#include "bitblit.h"
#include "font.h"

#include "defs.h"

#include "cut.h"
#include "destroy.h"
#include "do_button.h"
#include "font_subs.h"
#include "move.h"
/*}}}  */

/* aarg! */

static int nothing() { return 0; }

/* menus */

char *active_menu[] = {		/* active-window menu */
   "reshape",
   "move",
   "bury",
   "cut",
   "paste",
   "- - - -",
   "destroy",
   (char *) 0};

char *main_menu[] = {		/* primary menu */
   "new window",
   "redraw",
   "quit",
   (char *) 0};

char *full_menu[] = {		/* primary menu  - no more windows allowed */
   "redraw",
   "quit",
   (char *) 0};

char *quit_menu[] = {		/* to verify quit */
   "cancel",
   "suspend",
   "- - - -",
   "really quit",
   (char *) 0};
  

/* menu functions - these have a 1-1 corrospondance with the menu items */

int new_window(), shape_window();
int redraw();

function main_functions[] = {
   new_window,
   redraw,
   quit,
   (function) 0 };

function full_functions[] = {
   redraw,
   quit,
   (function) 0 };

function active_functions[] = {
   shape_window,
   move_window,
   hide_win,
   cut,
   paste,
   nothing,
   destroy_window,
   (function) 0 };

/* default font info */

char *font_dir = FONTDIR;
char *fontlist[MAXFONT];

/* default icon info */
char *icon_dir = ICONDIR;

/* color index map for fixed colors */

unsigned char color_map[COLORMAP_SIZE] = {
	202,		/* logo fg */
	2,	/* logo bg */
	232,		/* cr   fg */
	2,		/* cr   bg (unused) */
	88,		/* menu-fg */
	2,		/* menu bg */
	11,	/* pat fg */
	113,	/* pat bg */
	};

/* raster of table for inverted screen (this doesn't belong here) */

int rev_ops[] = {
   0xf,0x7,0xb,0x3,0xd,0x5,0x9,0x1,0xe,0x6,0xa,0x2,0xc,0x4,0x8,0x0
   };

BITMAP *m_rop;								/* current mouse bit map */
BITMAP *mouse_save;					/* where to keep what cursor's on */
int next_window=0;						/* next available window count */
struct font *font;						/* default font */
BITMAP *screen;							/* default screen */
WINDOW *active = (WINDOW *) 0;		/* window connected to keyboard */
WINDOW *last_active = (WINDOW *) 0;	/* previous window connected to keyboard */
int button_state = 0;					/* state of the mouse buttons */
int mouse, mousex, mousey;				/* mouse fd, x-coord, y-coord */
int debug = 0;								/* ==1 for debug prints */
int mouse_on = 0;							/* 1 iff mouse track is on */
char *snarf = (char *) 0;				/* place to keep snarfed text */
char *message = (char *) 0;			/* place to keep message */
char *start_command;						/* command for shell at window start */
char *init_command;						/* str to send window upon creation */
int id_message = 0;						/* id of message sender */
short buckey_map = 0;					/* mapping from ptty to buckey key */
unsigned int init_flags = INIT_FLAGS;	/* initial flags for new windows */
int mask = 0;								/* process mask for select */
int poll = 0;								/* processes with non-processed but
												 * already read data */
#ifdef DEBUG
char debug_level[] = "                                ";	/* debug flags */
#endif
