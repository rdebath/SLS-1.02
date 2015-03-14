/*{{{}}}*/
/*{{{  Notes*/
/*                        Copyright (c) 1987 Bellcore
 *                            All Rights Reserved
 *       Permission is granted to copy or use this program, EXCEPT that it
 *       may not be sold for profit, the copyright notice must be reproduced
 *       on copies, and credit should be given to Bellcore where it is due.
 *       BELLCORE MAKES NO WARRANTY AND ACCEPTS NO LIABILITY FOR THIS PROGRAM.
 */

/* defines for mgr */
/*}}}  */

/* potentially changable defines for mgr */

#define MAXWIN 		24		/* Maximum number of windows */
					/* < getdtablesize()-5 */
#define MAXCLIENT	25		/* max number of client windows */
#define MAXESC		20		/* max number of leading esc. digits */
#define TEXT_COUNT	MAXESC-1	/* text download index */
#define MAXBUF		80		/* max chars put to window per cycle */
#define MAXSHELL	127		/* max chars read from shell at once (128 causes an error) ?! */
#define POLL_INT	0		/* us's of pause at each select */
#define MAXNAME		35		/* max argv[0] length */
#define MAXTTY		35		/* max tty device name length */
#define MAXFONT		100		/* # of different fonts */
#define MAXMENU		20		/* max number of menus per window */
#define MAXBITMAPS	50		/* max number of bitmaps per window */
#define MAXITEMS	200		/* 2 * max # of menu items */
#define MAXEVENTS	22		/* max number of events, -4 thru 17,
					see event.h */
#define MIN_Y		1		/* minimum # of rows in a window */
#define MIN_X		5		/* min # of columns in a window */
#define MAX_PATH	100		/* max path length for font file */
#define GMAX		1000L		/* max graphics coordinate */
#define MOUSE_BUFF	30		/* size of mouse input buffer */
#define MSG_MODEMASK	02		/* invalid permission mask for tty */

#define STARTFILE	".mgrc"		/* name of mgr startup file */
#define HOST		"win "		/* name of host for utmp file */
#define DEFAULT_FONT	"DEFAULT_FONT"	/* default font environ variable (full path name) */
#define DUP_CHAR	'\005'		/* default dup character for DUPKEY mode */

#define COLORMAP_SIZE	10		/* max number of "built-in" colors */
#define MENU_COLOR	BUILDOP(BIT_SRC,color_map[4],color_map[5])/* menu colors */

#define BLK_BDR		2		/* thickness of border */
#define WH_BDR		3		/* border gap */
#define SUM_BDR		(BLK_BDR + WH_BDR)
#define STRIPE		2		/* # windowless pixels on left */

#define M_QUIT		3		/* confirm quit choice */
#define M_SUSPEND	1		/* suspend instead of quit */

#ifdef SYSV
#define index		strchr
#define rindex		strrchr
#endif

#ifdef DEBUG
extern char debug_level[];
char *index();
#define dprintf(x) \
	if (debug && index(debug_level,'x')) fprintf
#endif

/* macros -- for speed */

#ifdef FASTMOUSE
#define MoUSE(m,a,b)     bit_blit(m, a, b, 16,16,BIT_XOR, m_rop,0,0)
#define MOUSE_ON(m,a,b)	{ if (!mouse_on) { mouse_on=1; MoUSE(m,a,b); }}
#define MOUSE_OFF(m,a,b) { if (mouse_on) { mouse_on=0; MoUSE(m,a,b); }}
#else
/* turn on the mouse only if it is off */
#define MOUSE_ON(m,a,b) { if (!mouse_on) { mouse_on=1; bit_blit(mouse_save,0,0,16,16,BIT_SRC,m,a,b); bit_blit(m,a,b,16,16,BIT_OR,m_rop,0,0); bit_blit(m,a,b,16,16,BIT_NAND,m_rop,0,16); } }
/* turn off the mouse only if it is on */
#define MOUSE_OFF(m,a,b) { if (mouse_on) { mouse_on=0; bit_blit(m,a,b,16,16,BIT_SRC,mouse_save,0,0); } }
#endif

#define CLEAR(s,op)	bit_blit((s),0,0,BIT_WIDE(s),BIT_HIGH(s),op,0,0,0);

#define ACTIVE_ON() \
			border(active,SUM_BDR-1,1), \
			last_active = active!=last_active ? \
			  ( \
			  do_event(EVENT_DEACTIVATED,last_active,E_MAIN), \
			  do_event(EVENT_ACTIVATED,active,E_MAIN), \
			  active ) \
			: \
			  last_active
 			  	
#define ACTIVE_OFF() border(active,BLK_BDR,WH_BDR)

#define SETMOUSEICON(x)	(m_rop = x)

/* short hand */

#define Min(x,y)	((x)<(y)?(x):(y))
#define Max(x,y)	((x)>(y)?(x):(y))
#define W(x)		(win->x)
#define BETWEEN(a,x,b)	((x)<(a)?a:((x)>(b)?b:x))
#define ACTIVE(x)	(active->x)
#define ABS(x)		((x)>0 ? (x) : -(x))
#define LIMIT(x,max)	((x)>(max)?1:(-(x)>(max)?-1:0))

/* mouse buttons */

#define BUTTON_SYS	4		/* system menu */
#define BUTTON_2	2		/* middle button (unused) */
#define BUTTON_1	1		/* right button (vi?) */

/* Window table flags */

#define W_ACTIVE	0x000001L	/* Window is non_occluded */
#define W_ESCAPE	0x000002L	/* An escape sequence is in progress */
#define W_STOPPED	0x000004L	/* Window is stopped ( unused ) */
#define W_REVERSE	0x000008L	/* window is white on black */
#define W_STANDOUT	0x000010L	/* window is in standout mode */
#define W_DIED		0x000020L	/* window has died */
#define W_EXPOSE	0x000040L	/* expose window upon shell output */
#define W_BACKGROUND	0x000080L	/* permit obscured window to update */
#define W_NOKILL	0x000100L	/* don't kill window upon exit */
#define W_VI		0x000200L	/* temporary vi hack -- */
#define W_TEXT		0x000400L	/* downloading text */
#define W_NOINPUT	0x000800L	/* don't accept keyboard input */
#define W_NOWRAP	0x001000L	/* don't auto wrap */
#define W_OVER		0x002000L	/* overstrike mode */
#define W_ABSCOORDS	0x004000L	/* use absolute coordinates */
#define W_MINUS		0x008000L	/* negative coord value */
#define W_SNARFABLE	0x000004L	/* ok to gather data into cut buffer */
#define W_SNARFLINES	0x010000L	/* snarf only lines */
#define W_SNARFTABS	0x020000L	/* change spaces to tabs in snarf */
#define W_SNARFHARD	0x040000L	/* snarf even if errors */
#define W_SNARFLAGS	(W_SNARFTABS | W_SNARFHARD | W_SNARFLINES)
#define W_INHERIT	0x080000L	/* inherit menus and bitmaps */
#define W_DUPKEY	0x100000L	/* duplicate key mode */
#define W_NOBUCKEY	0x200000L	/* Buckey keys (i.e. "Left-<char>" and
					"Right-<char>") have no effect */
#define W_CLIPDONE	0x400000L	/* clip list for background update valid */

/* new character attributes */

#define W_BOLD          0x800000L       /* bold face */
#define W_UNDER         0x1000000L      /* underlined */

#define W_SPECIAL       (W_BOLD|W_UNDER)        /* special treatment */

#define W_LOOK \
	(W_EXPOSE | W_BACKGROUND)	/* Window ready to accept data */

#define W_STATE \
	(W_ESCAPE | W_TEXT)		/* terminal emulator states */

#define W_SAVE \
	(W_REVERSE | W_STANDOUT | W_EXPOSE | W_BACKGROUND | \
	 W_VI | W_NOINPUT | W_NOWRAP | \
	 W_SNARFLAGS | \
	 W_OVER | W_ABSCOORDS)		/* savable flags */

#define INIT_FLAGS	W_BACKGROUND	/* default window creation flags */

/* Structure definitions */

typedef struct {	/* used for text regions */
  int x,y,wide,high;
  } rectangle;

typedef struct  {	/* some day */
  int x,y;
  } point;

typedef struct window {		/* primary window structure */
  struct window *next;	/* next window */
  struct window *prev;	/* previous window */
  struct window *stack;	/* stack of saved window environments */
  struct window *main;	/* main window (or 0 if main window ) */
  struct window *alt;	/* alternate window ( 0 if none) */
  BITMAP *border;	/* window + border */
  BITMAP *window;	/* This is the window */
  BITMAP *save;		/* pointer to window bit-image if inactive */
  BITMAP *bitmap;	/* for use when downloading bitmaps */
  BITMAP *bitmaps[MAXBITMAPS];	/* scratchpad space */
  BITMAP *cursor;	/* bitmap of cursor (future; unused just now) */
  struct font *font;	/* this is the font */
  char *clip_list;	/* pointer to clip list for BG updates */

  rectangle text;	/* location of text region within window */
  int x0,y0;		/* origin of window on screen */
  int x,y;		/* cursor character position */
  int gx,gy;		/* graphics cursor */
  int op;		/* raster op function (see bitmap.h)  */
  int style;		/* character style normal/inverse video */
  int background;	/* background color WOB or BOW */
  int curs_type;	/* cursor type */

  int esc_cnt;		/* # of escape digits entered */
  int esc[MAXESC];	/* escape array (as in ESCnn,nnm) */
  int code;		/* code for text function */
  char dup;		/* char to duplicate from keyboard */

  struct menu_state *menus[MAXMENU];	/* menus */
  short menu[2];	/* index into menus for current button 1&2 menu */
  long event_mask;	/* event mask  (see event.h) */
  char *events[MAXEVENTS];	/* place for event strings */
  char *snarf;			/* temporary snarf buffer */

  unsigned long flags;		/* misc. window flags (see defines above) */

  char buff[MAXSHELL];	/* shell input buffer */
  int  max;		/* the # of chars in buff */
  int  current;		/* the current char in buff */
  int  to_fd;		/* file descriptor to shell */
  int  from_fd;		/* file descriptor from shell */
  int  pid;		/* process number of the shell */
  int  setid;		/* window set id, 1..N, one per pid */
  int  num;		/* window number ( for multiple window/proc) */
			/* The window set id and the window number together
			uniquely identify a window. */

  char tty[MAXTTY];	/* name of shell's tty */
  } WINDOW;

typedef int (*function)();
int new_window(), move_window(), quit();
int redraw();
int catch();

/* static data items (described in data.c) */

extern char *full_menu[];
extern char *main_menu[];
extern char *active_menu[];
extern char *test_menu[];
extern char *quit_menu[];
extern function full_functions[];
extern function main_functions[];
extern function active_functions[];

extern BITMAP *m_rop;
extern int next_window;
extern BITMAP *mouse_save;
extern struct font *font;
extern BITMAP *screen;
extern int mask;
extern int poll;
extern WINDOW *active;
extern WINDOW *last_active;
extern int button_state;
extern mouse, mousex, mousey, mouse_on;
extern int debug;
extern char *fontlist[], *font_dir;
extern char *icon_dir;
extern char *snarf;
extern char *message;
extern char *start_command;
extern char *init_command;
extern int id_message;
extern short buckey_map;
extern unsigned int init_flags;
extern unsigned char color_map[];
