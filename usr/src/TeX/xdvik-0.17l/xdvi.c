/*
 * DVI previewer for X.
 *
 * Eric Cooper, CMU, September 1985.
 *
 * Code derived from dvi-imagen.c.
 *
 * Modification history:
 * 1/1986	Modified for X.10 by Bob Scheifler, MIT LCS.
 * 7/1988	Modified for X.11 by Mark Eichin, MIT
 * 12/1988	Added 'R' option, toolkit, magnifying glass
 *			--Paul Vojta, UC Berkeley.
 * 2/1989	Added tpic support	--Jeffrey Lee, U of Toronto
 * 4/1989	Modified for System V	--Donald Richardson, Clarkson Univ.
 * 3/1990	Added VMS support	--Scott Allendorf, U of Iowa
 * 3/1992	Rewrite of font routines--karl@cs.umb.edu
 *
 *	See Makefile.in and config.h for compilation options.
 */

#define EXTERN

#include "config.h"
#include <ctype.h>

#include "pxl-open.h"

#ifndef	A4
#define	DEFAULT_PAPER		"us"
#else
#define	DEFAULT_PAPER		"a4"
#endif

/* Use buttons by default.  (This is not mentioned in Makefile.in,
   because they can be turned off with -expert.)  */
#define BUTTONS

#if	!defined(X10) && !defined(NOTOOL)
#define	TOOLKIT
#else
#undef	TOOLKIT
#undef	BUTTONS
#endif

#ifndef X10
#undef Boolean
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include "xdvi.icon"
#else /* X10 */
#include <X/Xlib.h>
#endif /* X10 */

#ifdef	TOOLKIT
#include <X11/Intrinsic.h>
#ifdef OLD_X11_TOOLKIT
#include <X11/Atoms.h>
#else /* not OLD_X11_TOOLKIT */
#include <X11/Xatom.h>
#include <X11/StringDefs.h>
#endif /* not OLD_X11_TOOLKIT */
#include <X11/Shell.h>		/* needed for def. of XtNiconX */
#ifndef	XtSpecificationRelease
#define	XtSpecificationRelease	0
#endif
#if	XtSpecificationRelease >= 4
#include <X11/Xaw/Viewport.h>
#ifdef	BUTTONS
#include <X11/Xaw/Command.h>
#endif
#else /* XtSpecificationRelease < 4 */
#include <X11/Viewport.h>
#ifdef	BUTTONS
#include <X11/Command.h>
#endif
#endif /* XtSpecificationRelease */
#else /* !TOOLKIT */
#define	XtNumber(arr)	(sizeof(arr)/sizeof(arr[0]))
typedef int Position;
typedef unsigned int Dimension;
#ifndef X10
typedef unsigned int Pixel;
#define	XtPending()	XPending(DISP)
#else
#define	XtPending	XPending
#define	XMoveResizeWindow XConfigureWindow
#endif
#endif /* TOOLKIT */

#ifdef	HAS_SIGIO
#include <fcntl.h>
#include <signal.h>
#endif

#ifndef X10
static Display *DISP;
#define	DPY	DISP,
static Screen *SCRN;
static Cursor redraw_cursor, ready_cursor;

#ifdef	VMS
/*
 * Magnifying glass cursor
 *
 * Developed by Tom Sawyer, April 1990
 * Contibuted by Hunter Goatley, January 1991
 *
 */

#define mag_glass_width 16
#define mag_glass_height 16
#define mag_glass_x_hot 6
#define mag_glass_y_hot 6
static char mag_glass_bits[]=
{
  0xf8, 0x03, 0x0c, 0x06, 0xe2, 0x09, 0x13, 0x1a, 0x01, 0x14, 0x01, 0x14,
  0x01, 0x10, 0x01, 0x10, 0x01, 0x10, 0x03, 0x10, 0x02, 0x18, 0x0c, 0x34,
  0xf8, 0x6f, 0x00, 0xd8, 0x00, 0xb0, 0x00, 0xe0
};
#include <decw$cursor.h>	/* Include the DECWindows cursor symbols */
static int DECWCursorFont;	/* Space for the DECWindows cursor font  */
static Pixmap MagnifyPixmap;	/* Pixmap to hold our special mag-glass  */
#endif /* VMS */

#define	SetCursor(x)	XDefineCursor(DISP, WINDOW(mane), x)
#define	ClearPage(wr)	XClearWindow(DISP, WINDOW(wr));
#define	ClearArea(win, x, y, w, h)	XClearArea(DISP, win, x, y, w, h, False)
#define	DarkenArea(win, x, y, w, h) \
			XFillRectangle(DISP, win, ruleGC, x, y, w, h)
#define	CopyArea(win, x, y, w, h, x2, y2) \
			XCopyArea(DISP, win, win, DefaultGCOfScreen(SCRN), \
				x, y, w, h, x2, y2)
#define	Flush()		XFlush(DISP)
#ifndef X11HEIGHT
#define	X11HEIGHT	8	/* Height of server default font */
#endif
#else /* X10 */
#define	DPY
#define	GC		int
#define	SetCursor(x)
#define	ClearPage(wr)	XClear(WINDOW(wr));
#define	ClearArea(win, x, y, w, h)	XPixSet(win, x, y, w, h, backpix)
#define	DarkenArea(win, x, y, w, h)	XPixSet(win, x, y, w, h, foreGC)
#define	CopyArea(win, x, y, w, h, x2, y2) \
			XMoveArea(win, x, y, x2, y2, w, h, GXcopy);
#define	XBell(a,b)	XFeep(b/10-1)
#define	Flush()		XFlush()
#define	ConnectionNumber(DISP)	(_XlibCurrentDisplay->fd)
#ifndef X10FONT
#define	X10FONT	"helv10b"	/* Font for X10 error messages */
#define	X10HEIGHT	10
#endif
#endif /* X10 */

#define	MAGBORD	1		/* border size for magnifier */

/*
 * Command line flags.
 */
int debug = 0;
boolean list_fonts = False;

int density = 40;
int pixels_per_inch = 300;
int offset_x, offset_y;
int unshrunk_paper_w, unshrunk_paper_h;
int unshrunk_page_w, unshrunk_page_h;
boolean hush_spec = False;
boolean hush_chars = False;
static char *paper = DEFAULT_PAPER;
static char *sidemargin, *topmargin;
static char *xoffset, *yoffset;
static boolean reverse;
static Dimension bwidth = 2;
static int bak_shrink;
static char *debug_arg;
static int mg_size[5] =
{200, 350, 600, 900, 1200};

char *dvi_name = NULL;
FILE *dvi_file;			/* user's file */
char *prog;
char *curr_page = NULL;

int pageno_correct = 1;
boolean keep_flag = False;
static double specialConv;
#ifdef	BUTTONS
boolean expert = False;
#endif

#ifndef X10
#ifdef	TOOLKIT
 /* fg and bg colors */
static Arg fore_args =
{XtNforeground, (XtArgVal) 0};
#define	fore_Pixel	fore_args.value
static Arg back_args =
{XtNbackground, (XtArgVal) 0};
#define	back_Pixel	back_args.value
#else /* !TOOLKIT */
static Pixel fore_Pixel, back_Pixel;
#endif /* TOOLKIT */
static Pixel brdr_Pixel, hl_Pixel, cr_Pixel;
#endif /* X10 */

static char *fore_color;
static char *back_color;
static char *brdr_color;
static char *high_color;
static char *curs_color;
static GC foreGC, highGC;
#ifndef X10
static GC ruleGC;
static GC foreGC2;
#else /* X10 */
#define	ruleGC	foreGC
#endif /* X10 */

int page_w, page_h;
#define	clip_w	mane.width
#define	clip_h	mane.height
static Dimension window_w, window_h;
#ifndef X10
static Position main_x, main_y;
static XImage *image;
#else /* X10 */
#define	main_x	0
#define	main_y	0
static int GXfunc;
static int backpix, backmap, bdrmap;
/*
 * Cursor and mask for valid cursor
 */
#include "xdvi_curs.h"
#include "xdvi_mask.h"
#endif /* X10 */

static Position mag_x, mag_y, new_mag_x, new_mag_y;
static boolean mag_moved = False;
static int home_x, home_y;
static int min_x, max_x, min_y, max_y;

struct WindowRec mane =
{NULL, 3, 0, 0, 0, 0, MAXDIM, 0, MAXDIM, 0};
struct WindowRec alt =
{NULL, 1, 0, 0, 0, 0, MAXDIM, 0, MAXDIM, 0};
/*	curr is temporary storage except for within redraw() */
struct WindowRec curr =
{NULL, 3, 0, 0, 0, 0, MAXDIM, 0, MAXDIM, 0};

#ifdef	TOOLKIT
static Widget top_level, vport_widget, draw_widget, clip_widget;
#ifdef	BUTTONS
static Widget form_widget, line_widget, right_widget;
#endif
static Widget x_bar, y_bar;	/* horizontal and vertical scroll bars */

static Arg vport_args[]=
{
#ifdef	BUTTONS
  {XtNwidth, (XtArgVal) 0},
  {XtNheight, (XtArgVal) 0},
  {XtNborderWidth, (XtArgVal) 0},
  {XtNtop, (XtArgVal) XtChainTop},
  {XtNbottom, (XtArgVal) XtChainBottom},
  {XtNleft, (XtArgVal) XtChainLeft},
  {XtNright, (XtArgVal) XtChainRight},
#endif
  {XtNallowHoriz, (XtArgVal) True},
  {XtNallowVert, (XtArgVal) True},
};

/*	Note:  Argument order in the following is important! */

static Arg draw_args[]=
{
  {XtNwidth, (XtArgVal) 0},
  {XtNheight, (XtArgVal) 0},
  {XtNx, (XtArgVal) 0},
  {XtNy, (XtArgVal) 0},
  {XtNlabel, (XtArgVal) ""},
};

#ifdef	BUTTONS
static Arg form_args[]=
{
  {XtNdefaultDistance, (XtArgVal) 0},
};
#define	XTRA_WID	79

static Arg line_args[]=
{
  {XtNbackground, (XtArgVal) 0},
  {XtNwidth, (XtArgVal) 1},
  {XtNheight, (XtArgVal) 0},
  {XtNfromHoriz, (XtArgVal) NULL},
  {XtNborderWidth, (XtArgVal) 0},
  {XtNtop, (XtArgVal) XtChainTop},
  {XtNbottom, (XtArgVal) XtChainBottom},
  {XtNleft, (XtArgVal) XtChainRight},
  {XtNright, (XtArgVal) XtChainRight},
};

static Arg right_args[]=
{
  {XtNfromHoriz, (XtArgVal) NULL},
  {XtNwidth, (XtArgVal) (XTRA_WID - 1)},
  {XtNheight, (XtArgVal) 0},
  {XtNborderWidth, (XtArgVal) 0},
  {XtNtop, (XtArgVal) XtChainTop},
  {XtNbottom, (XtArgVal) XtChainBottom},
  {XtNleft, (XtArgVal) XtChainRight},
  {XtNright, (XtArgVal) XtChainRight},
};

static struct
{
  char *label;
  char *name;
  int closure;
  int y_pos;
}

  command_table[]=
{
  {
    "Quit", "quit", 'q', 50
  },
  {
    "Shrink1", "sh1", 1 << 8 | 's', 150
  },
  {
    "Shrink2", "sh2", 2 << 8 | 's', 200
  },
  {
    "Shrink3", "sh3", 3 << 8 | 's', 250
  },
  {
    "Shrink4", "sh4", 4 << 8 | 's', 300
  },
  {
    "Page-10", "prev10", 10 << 8 | 'p', 400
  },
  {
    "Page-5", "prev5", 5 << 8 | 'p', 450
  },
  {
    "Prev", "prev", 'p', 500
  },
  {
    "Next", "next", 'n', 600
  },
  {
    "Page+5", "next5", 5 << 8 | 'n', 650
  },
  {
    "Page+10", "next10", 10 << 8 | 'n', 700
  },
};

static void handle_command ();

static XtCallbackRec command_call[]=
{
  {handle_command, NULL},
  {NULL, NULL},
};

static Arg command_args[]=
{
  {XtNlabel, (XtArgVal) NULL},
  {XtNx, (XtArgVal) 6},
  {XtNy, (XtArgVal) 0},
  {XtNwidth, (XtArgVal) 64},
  {XtNheight, (XtArgVal) 30},
  {XtNcallback, (XtArgVal) command_call},
};

static void
create_buttons (h)
     XtArgVal h;
{
  int i;

  line_args[2].value = h;
  line_args[3].value = (XtArgVal) vport_widget;
  line_widget = XtCreateManagedWidget ("line", widgetClass, form_widget,
				       line_args, XtNumber (line_args));
  right_args[0].value = (XtArgVal) line_widget;
  right_args[2].value = h;
  right_widget = XtCreateManagedWidget ("right", compositeWidgetClass,
			    form_widget, right_args, XtNumber (right_args));

  command_args[2].value = (XtArgVal) vport_widget;
  for (i = 0; i < XtNumber (command_table); ++i)
    {
      command_args[0].value = (XtArgVal) command_table[i].label;
      command_args[2].value = (XtArgVal) command_table[i].y_pos;
      command_call[0].closure = (caddr_t) command_table[i].closure;
      (void) XtCreateManagedWidget (command_table[i].name,
				    commandWidgetClass, right_widget,
				    command_args, XtNumber (command_args));
    }
}

#endif /* BUTTONS */

#else /* !TOOLKIT */
#define	BAR_WID		12	/* width of darkened area */
#define	BAR_THICK	15	/* gross amount removed */

static Window top_level;
static Window x_bar, y_bar;
static int x_bgn, x_end, y_bgn, y_end;	/* scrollbar positions */
#endif /* TOOLKIT */

/*
 *	Mechanism to keep track of the magnifier window.  The problems are,
 *	(a) if the button is released while the window is being drawn, this
 *	could cause an X error if we continue drawing in it after it is
 *	destroyed, and
 *	(b) creating and destroying the window too quickly confuses the window
 *	manager, which is avoided by waiting for an expose event before
 *	destroying it.
 */
static short alt_stat;		/* 1 = wait for expose, */
 /* -1 = destroy upon expose */
static boolean alt_canit;	/* stop drawing this window */

/*
 *	Data for buffered events.
 */

static boolean canit = False, arg = False;
static short event_counter = 0, event_freq = 70;
static int number = 0, sign = 1;
static jmp_buf canit_env;

static void can_exposures (), read_events (), keystroke ();

#ifdef	lint
#ifndef	X10
char xdvi_bits[288];
#ifdef	TOOLKIT
WidgetClass viewportWidgetClass, widgetClass;
#ifdef	BUTTONS
WidgetClass formWidgetClass, compositeWidgetClass, commandWidgetClass;
#endif /* BUTTONS */
#endif /* TOOLKIT */
#else /* X10 */
short xdvi_bits[15], xdvi_mask_bits[15];
Display *_XlibCurrentDisplay;
#endif /* X10 */
#endif /* lint */

#ifdef	sun
char *sprintf ();
#endif

/* double atof (); */

/********************************
 *	  tpic routines		*
 *******************************/

/* Things we need from spec_draw, unfortunately */

/* (ignored for now)
extern int pen_size, blacken, whiten, shade;
*/

#define	toint(x)	((int) ((x) + 0.5))
#define	xconv(x)	(toint(specialConv*(x))/shrink_factor + PXL_H)
#define	yconv(y)	(toint(specialConv*(y))/shrink_factor + PXL_V)

/*
 *	Draw a line from (fx,fy) to (tx,ty).
 *	Right now, we ignore pen_size.
 */
void
line_btw (fx, fy, tx, ty)
     int fx, fy, tx, ty;
{
  register int fcx = xconv (fx), tcx = xconv (tx), fcy = yconv (fy), tcy = yconv (ty);

  if ((fcx < max_x || tcx < max_x) && (fcx >= min_x || tcx >= min_x) &&
      (fcy < max_y || tcy < max_y) && (fcy >= min_y || tcy >= min_y))
#ifndef X10
    XDrawLine (DISP, WINDOW (curr), ruleGC,
	       fcx - curr.base_x, fcy - curr.base_y,
	       tcx - curr.base_x, tcy - curr.base_y);
#else
    XLine (WINDOW (curr),
	   fcx - curr.base_x, fcy - curr.base_y,
	   tcx - curr.base_x, tcy - curr.base_y,
	   1, 1, ruleGC, GXcopy, AllPlanes);
#endif
}

/*
 *	Draw a dot at (x,y)
 */
void
dot_at (x, y)
{
  register int cx = xconv (x), cy = yconv (y);

  if (cx < max_x && cx >= min_x && cy < max_y && cy >= min_y)
#ifndef X10
    XDrawPoint (DISP, WINDOW (curr), ruleGC,
		cx - curr.base_x, cy - curr.base_y);
#else
    XPixSet (WINDOW (curr), cx - curr.base_x, cy - curr.base_y,
	     1, 1, ruleGC);
#endif
}

/*
 *	Apply the requested attributes to the last path (box) drawn.
 *	Attributes are reset.
 *	(Not currently implemented.)
 */
 /* ARGSUSED */
void
do_attribute_path (last_min_x, last_max_x, last_min_y, last_max_y)
     int last_min_x, last_max_x, last_min_y, last_max_y;
{
}

/**
 **	Put a rectangle on the screen.  hl determines the GC.
 **/

put_rectangle (x, y, w, h, hl)
     int x, y, w, h;
     boolean hl;
{
  if (x < max_x && x + w >= min_x && y < max_y && y + h >= min_y)
    {
      if (--event_counter == 0)
	read_events (False);
#ifndef X10
      XFillRectangle (DISP, WINDOW (curr), hl ? highGC : ruleGC,
		    x - curr.base_x, y - curr.base_y, w ? w : 1, h ? h : 1);
#else
      XPixSet (WINDOW (curr), x - curr.base_x, y - curr.base_y,
	       w ? w : 1, h ? h : 1, hl ? highGC : ruleGC);
#endif
    }
}

put_bitmap (bitmap, x, y)
     register struct bitmap *bitmap;
     register int x, y;
{

  if (debug & DBG_BITMAP)
    Printf ("X(%d,%d)\n", x - curr.base_x, y - curr.base_y);
  if (x < max_x && x + bitmap->w >= min_x &&
      y < max_y && y + bitmap->h >= min_y)
    {
      if (--event_counter == 0)
	read_events (False);
#ifndef X10
      image->width = bitmap->w;
      image->height = bitmap->h;
      image->data = bitmap->bits;
      image->bytes_per_line = bitmap->bytes_wide;
      XPutImage (DISP, WINDOW (curr), foreGC, image,
		 0, 0,
		 x - curr.base_x, y - curr.base_y,
		 bitmap->w, bitmap->h);
      if (foreGC2)
	XPutImage (DISP, WINDOW (curr), foreGC2, image,
		   0, 0,
		   x - curr.base_x, y - curr.base_y,
		   bitmap->w, bitmap->h);
#else
      XBitmapBitsPut (WINDOW (curr), x - curr.base_x, y - curr.base_y,
		      bitmap->w, bitmap->h, bitmap->bits,
		      foreGC, backpix, NULL, GXfunc, AllPlanes);
#endif
    }
}

put_border (w, h, t)
     int w, h, t;
{
  put_rectangle (0, 0, w, t, True);	/* top */
  put_rectangle (w, 0, t, h, True);	/* right */
  put_rectangle (t, h, w, t, True);	/* bottom */
  put_rectangle (0, t, t, h, True);	/* left */
}

/*
 *	Event-handling routines
 */

static void
expose (windowrec, x, y, w, h)
     register struct WindowRec *windowrec;
     int x, y, w, h;
{
  if (windowrec->min_x > x)
    windowrec->min_x = x;
  if (windowrec->max_x < x + w)
    windowrec->max_x = x + w;
  if (windowrec->min_y > y)
    windowrec->min_y = y;
  if (windowrec->max_y < y + h)
    windowrec->max_y = y + h;
}

static void
clearexpose (windowrec, x, y, w, h)
     struct WindowRec *windowrec;
     int x, y, w, h;
{
  ClearArea (WINDOW (*windowrec), x, y, w, h);
  expose (windowrec, x, y, w, h);
}

static void
scrollwindow (windowrec, x0, y0)
     register struct WindowRec *windowrec;
     int x0, y0;
{
  int x, y;
  int x2 = 0, y2 = 0;
  int ww, hh;

  x = x0 - windowrec->base_x;
  y = y0 - windowrec->base_y;
  ww = windowrec->width - x;
  hh = windowrec->height - y;
  windowrec->base_x = x0;
  windowrec->base_y = y0;
  if (curr.win == windowrec->win)
    {
      curr.base_x = x0;
      curr.base_y = y0;
    }
  windowrec->min_x -= x;
  if (windowrec->min_x < 0)
    windowrec->min_x = 0;
  windowrec->max_x -= x;
  if (windowrec->max_x > windowrec->width)
    windowrec->max_x = windowrec->width;
  windowrec->min_y -= y;
  if (windowrec->min_y < 0)
    windowrec->min_y = 0;
  windowrec->max_y -= y;
  if (windowrec->max_y > windowrec->height)
    windowrec->max_y = windowrec->height;
  if (x < 0)
    {
      x2 = -x;
      x = 0;
      ww = windowrec->width - x2;
    }
  if (y < 0)
    {
      y2 = -y;
      y = 0;
      hh = windowrec->height - y2;
    }
  if (ww <= 0 || hh <= 0)
    {
      ClearPage (*windowrec);
      windowrec->min_x = windowrec->min_y = 0;
      windowrec->max_x = windowrec->width;
      windowrec->max_y = windowrec->height;
    }
  else
    {
      CopyArea (WINDOW (*windowrec), x, y, ww, hh, x2, y2);
      if (x > 0)
	clearexpose (windowrec, ww, 0, x, windowrec->height);
      if (x2 > 0)
	clearexpose (windowrec, 0, 0, x2, windowrec->height);
      if (y > 0)
	clearexpose (windowrec, 0, hh, windowrec->width, y);
      if (y2 > 0)
	clearexpose (windowrec, 0, 0, windowrec->width, y2);
    }
}

#ifdef	TOOLKIT
/*
 *	routines for X11 toolkit
 */

static Arg arg_wh[]=
{
  {XtNwidth, (XtArgVal) & window_w},
  {XtNheight, (XtArgVal) & window_h},
};

static Position window_x, window_y;
static Arg arg_xy[]=
{
  {XtNx, (XtArgVal) & window_x},
  {XtNy, (XtArgVal) & window_y},
};

#define	get_xy()	XtGetValues(draw_widget, arg_xy, XtNumber(arg_xy))

#define	mane_base_x	0
#define	mane_base_y	0

static void
home (scrl)
     boolean scrl;
{
  register int coord;

  if (!scrl)
    XUnmapWindow (DISP, WINDOW (mane));
  get_xy ();
  if (x_bar)
    {
      coord = 0;
      if (page_w > clip_w)
	{
	  coord = (page_w - clip_w) / 2;
	  if (coord > home_x / mane.shrinkfactor)
	    coord = home_x / mane.shrinkfactor;
	}
      XtCallCallbacks (x_bar, XtNscrollProc, window_x + coord);
    }
  if (y_bar)
    {
      coord = 0;
      if (page_h > clip_h)
	{
	  coord = (page_h - clip_h) / 2;
	  if (coord > home_y / mane.shrinkfactor)
	    coord = home_y / mane.shrinkfactor;
	}
      XtCallCallbacks (y_bar, XtNscrollProc, window_y + coord);
    }
  if (!scrl)
    {
      XMapWindow (DISP, WINDOW (mane));
      /* Wait for the server to catch up---this eliminates flicker. */
      XSync (DISP, False);
    }
}

static boolean resized = False;

static
get_geom ()
{
  static Dimension new_clip_w, new_clip_h;
  static Arg arg_wh_clip[]=
  {
    {XtNwidth, (XtArgVal) & new_clip_w},
    {XtNheight, (XtArgVal) & new_clip_h},
  };
  register int old_clip_w;

  XtGetValues (vport_widget, arg_wh, XtNumber (arg_wh));
  XtGetValues (clip_widget, arg_wh_clip, XtNumber (arg_wh_clip));
  /* Note:  widgets may be destroyed but not forgotten */
  x_bar = page_w <= new_clip_w ? NULL
    : XtNameToWidget (vport_widget, "horizontal");
  y_bar = page_h <= new_clip_h ? NULL
    : XtNameToWidget (vport_widget, "vertical");
  old_clip_w = clip_w;
  /* we need to do this because */
  /* sizeof(Dimension) != sizeof(int) */
  clip_w = new_clip_w;
  clip_h = new_clip_h;
  if (old_clip_w == 0)
    home (False);
  resized = False;
}

static void
center (x, y)
     int x, y;
{
/*	We use the clip widget here because it gives a more exact value. */
  x -= clip_w / 2;
  y -= clip_h / 2;
  if (x_bar)
    XtCallCallbacks (x_bar, XtNscrollProc, x);
  if (y_bar)
    XtCallCallbacks (y_bar, XtNscrollProc, y);
  XWarpPointer (DISP, None, None, 0, 0, 0, 0, -x, -y);
}

/*
 *	callback routines
 */

/* The following callback routine should never be called. */
 /*ARGSUSED*/
static void
handle_key (widget, junk, event)
     Widget widget;
     caddr_t junk;
     XEvent *event;
{
  XBell (DISP, 20);
}

 /*ARGSUSED*/
static void
handle_resize (widget, junk, event)
     Widget widget;
     caddr_t junk;
     XEvent *event;
{
  resized = True;
}

#ifdef	BUTTONS
 /*ARGSUSED*/
static void
handle_command (widget, client_data, call_data)
     Widget widget;
     caddr_t client_data;
     caddr_t call_data;
{
  keystroke (((int) client_data) & 0xff, ((int) client_data) >> 8,
	     (((int) client_data) >> 8) != 0, (XEvent *) NULL);
}

#endif /* BUTTONS */

#else /* !TOOLKIT */

/*
 *	brute force scrollbar routines
 */

static void
paint_x_bar ()
{
  register int new_x_bgn = mane.base_x * clip_w / page_w;
  register int new_x_end = (mane.base_x + clip_w) * clip_w / page_w;

  if (new_x_bgn >= x_end || x_bgn >= new_x_end)
    {				/* no overlap */
      ClearArea (x_bar, x_bgn, 1, x_end - x_bgn, BAR_WID);
      DarkenArea (x_bar, new_x_bgn, 1, new_x_end - new_x_bgn, BAR_WID);
    }
  else
    {				/* this stuff avoids flicker */
      if (x_bgn < new_x_bgn)
	ClearArea (x_bar, x_bgn, 1, new_x_bgn - x_bgn, BAR_WID);
      else
	DarkenArea (x_bar, new_x_bgn, 1, x_bgn - new_x_bgn, BAR_WID);
      if (new_x_end < x_end)
	ClearArea (x_bar, new_x_end, 1, x_end - new_x_end, BAR_WID);
      else
	DarkenArea (x_bar, x_end, 1, new_x_end - x_end, BAR_WID);
    }
  x_bgn = new_x_bgn;
  x_end = new_x_end;
}

static void
paint_y_bar ()
{
  register int new_y_bgn = mane.base_y * clip_h / page_h;
  register int new_y_end = (mane.base_y + clip_h) * clip_h / page_h;

  if (new_y_bgn >= y_end || y_bgn >= new_y_end)
    {				/* no overlap */
      ClearArea (y_bar, 1, y_bgn, BAR_WID, y_end - y_bgn);
      DarkenArea (y_bar, 1, new_y_bgn, BAR_WID, new_y_end - new_y_bgn);
    }
  else
    {				/* this stuff avoids flicker */
      if (y_bgn < new_y_bgn)
	ClearArea (y_bar, 1, y_bgn, BAR_WID, new_y_bgn - y_bgn);
      else
	DarkenArea (y_bar, 1, new_y_bgn, BAR_WID, y_bgn - new_y_bgn);
      if (new_y_end < y_end)
	ClearArea (y_bar, 1, new_y_end, BAR_WID, y_end - new_y_end);
      else
	DarkenArea (y_bar, 1, y_end, BAR_WID, new_y_end - y_end);
    }
  y_bgn = new_y_bgn;
  y_end = new_y_end;
}

static void
scrollmane (x, y)
     int x, y;
{
  register int old_base_x = mane.base_x;
  register int old_base_y = mane.base_y;
  if (x > page_w - clip_w)
    x = page_w - clip_w;
  if (x < 0)
    x = 0;
  if (y > page_h - clip_h)
    y = page_h - clip_h;
  if (y < 0)
    y = 0;
  scrollwindow (&mane, x, y);
  if (old_base_x != mane.base_x && x_bar)
    paint_x_bar ();
  if (old_base_y != mane.base_y && y_bar)
    paint_y_bar ();
}

static void
reconfig ()
{
  int x_thick = 0;
  int y_thick = 0;
#ifdef	X10
  int old_clip_w = clip_w;
  int old_clip_h = clip_h;
  int old_x_thick = x_thick;
  int old_y_thick = y_thick;
#endif

  /* determine existence of scrollbars */
  if (window_w < page_w)
    x_thick = BAR_THICK;
  if (window_h - x_thick < page_h)
    y_thick = BAR_THICK;
  clip_w = window_w - y_thick;
  if (clip_w < page_w)
    x_thick = BAR_THICK;
  clip_h = window_h - x_thick;

  /* process drawing (clip) window */
  if (mane.win == NULL)
    {				/* initial creation */
#ifndef X10
      mane.win = (caddr_t) XCreateSimpleWindow (DISP, top_level,
						y_thick, x_thick,
			    (unsigned int) clip_w, (unsigned int) clip_h, 0,
						brdr_Pixel, back_Pixel);
      XSelectInput (DPY WINDOW (mane), ExposureMask |
		    ButtonPressMask | ButtonMotionMask | ButtonReleaseMask);
#else
      mane.win = (caddr_t) XCreateWindow (top_level,
					y_thick, x_thick, clip_w, clip_h, 0,
					  bdrmap, backmap);
      XSelectInput (WINDOW (mane), ExposeRegion | ExposeCopy |
		    ButtonPressed | ButtonReleased |
		    LeftDownMotion | MiddleDownMotion | RightDownMotion);
#endif
      XMapWindow (DPY WINDOW (mane));
    }
  else
#ifdef	X10
    if (clip_w != old_clip_w || clip_h != old_clip_h ||
	x_thick != old_x_thick || y_thick != old_y_thick)
    {
#endif
      XMoveResizeWindow (DPY WINDOW (mane),
			 y_thick, x_thick, clip_w, clip_h);
#ifdef	X10
      XSync (False);
    }
#endif

  /* process scroll bars */
  if (x_thick)
    {
      if (x_bar)
	{
	  XMoveResizeWindow (DPY x_bar,
			     y_thick - 1, -1, clip_w, BAR_THICK - 1);
	  paint_x_bar ();
	}
      else
	{
#ifndef X10
	  x_bar = XCreateSimpleWindow (DISP, top_level, y_thick - 1, -1,
				    (unsigned int) clip_w, BAR_THICK - 1, 1,
				       brdr_Pixel, back_Pixel);
	  XSelectInput (DISP, x_bar,
			ExposureMask | ButtonPressMask | Button2MotionMask);
#else
	  x_bar = XCreateWindow (top_level,
				 y_thick - 1, -1, clip_w, BAR_THICK - 1, 1,
				 bdrmap, backmap);
	  XSelectInput (x_bar,
			ExposeRegion | ButtonPressed | MiddleDownMotion);
#endif
	  XMapWindow (DPY x_bar);
	}
      x_bgn = mane.base_x * clip_w / page_w;
      x_end = (mane.base_x + clip_w) * clip_w / page_w;
    }
  else if (x_bar)
    {
      XDestroyWindow (DPY x_bar);
      x_bar = NULL;
    }

  if (y_thick)
    {
      if (y_bar)
	{
	  XMoveResizeWindow (DPY y_bar,
			     -1, x_thick - 1, BAR_THICK - 1, clip_h);
	  paint_y_bar ();
	}
      else
	{
#ifndef X10
	  y_bar = XCreateSimpleWindow (DISP, top_level, -1, x_thick - 1,
				    BAR_THICK - 1, (unsigned int) clip_h, 1,
				       brdr_Pixel, back_Pixel);
	  XSelectInput (DISP, y_bar,
			ExposureMask | ButtonPressMask | Button2MotionMask);
#else
	  y_bar = XCreateWindow (top_level,
				 -1, x_thick - 1, BAR_THICK - 1, clip_h, 1,
				 bdrmap, backmap);
	  XSelectInput (y_bar,
			ExposeRegion | ButtonPressed | MiddleDownMotion);
#endif
	  XMapWindow (DPY y_bar);
	}
      y_bgn = mane.base_y * clip_h / page_h;
      y_end = (mane.base_y + clip_h) * clip_h / page_h;
    }
  else if (y_bar)
    {
      XDestroyWindow (DPY y_bar);
      y_bar = NULL;
    }
}

static void
home (scrl)
     boolean scrl;
{
  int x = 0, y = 0;

  if (page_w > clip_w)
    {
      x = (page_w - clip_w) / 2;
      if (x > home_x / mane.shrinkfactor)
	x = home_x / mane.shrinkfactor;
    }
  if (page_h > clip_h)
    {
      y = (page_h - clip_h) / 2;
      if (y > home_y / mane.shrinkfactor)
	y = home_y / mane.shrinkfactor;
    }
  if (scrl)
    scrollmane (x, y);
  else
    {
      mane.base_x = x;
      mane.base_y = y;
      if (curr.win == mane.win)
	{
	  curr.base_x = x;
	  curr.base_y = y;
	}
      if (x_bar)
	paint_x_bar ();
      if (y_bar)
	paint_y_bar ();
    }
}

#define	get_xy()
#define	window_x 0
#define	window_y 0
#define	mane_base_x	mane.base_x
#define	mane_base_y	mane.base_y
#endif /* TOOLKIT */

static void
compute_mag_pos (xp, yp)
     int *xp, *yp;
{
  register int t;

  t = mag_x + main_x - alt.width / 2;
#ifndef X10
  if (t > WidthOfScreen (SCRN) - alt.width - 2 * MAGBORD)
    t = WidthOfScreen (SCRN) - alt.width - 2 * MAGBORD;
#else
  if (t > (int) window_w - alt.width - 2 * MAGBORD)
    t = window_w - alt.width - 2 * MAGBORD;
#endif
  if (t < 0)
    t = 0;
  *xp = t;
  t = mag_y + main_y - alt.height / 2;
#ifndef X10
  if (t > HeightOfScreen (SCRN) - alt.height - 2 * MAGBORD)
    t = HeightOfScreen (SCRN) - alt.height - 2 * MAGBORD;
#else
  if (t > (int) window_h - alt.height - 2 * MAGBORD)
    t = window_h - alt.height - 2 * MAGBORD;
#endif
  if (t < 0)
    t = 0;
  *yp = t;
}

#ifdef	TOOLKIT
 /*ARGSUSED*/
static void
handle_button (widget, junk, event)
     Widget widget;
     caddr_t junk;
#else /* !TOOLKIT */
static void
handle_button (event)
#endif /* TOOLKIT */
     XButtonEvent *event;
{
  int x, y;
#ifndef X10
  XSetWindowAttributes attr;

  alt.width = alt.height = mg_size[event->button - 1];
#else
  alt.width = alt.height = mg_size[2 - (event->detail & ValueMask)];
#endif
  if (alt.win != NULL || mane.shrinkfactor == 1 || alt.width <= 0)
    XBell (DISP, 20);
  else
    {
      mag_x = event->x;
      mag_y = event->y;
#ifndef X10
      main_x = event->x_root - mag_x;
      main_y = event->y_root - mag_y;
#endif
      compute_mag_pos (&x, &y);
      alt.base_x = (event->x + mane_base_x) * mane.shrinkfactor -
	alt.width / 2;
      alt.base_y = (event->y + mane_base_y) * mane.shrinkfactor -
	alt.height / 2;
#ifndef X10
      attr.save_under = True;
      attr.border_pixel = brdr_Pixel;
      attr.background_pixel = back_Pixel;
      attr.override_redirect = True;
      alt.win = (caddr_t) XCreateWindow (DISP, RootWindowOfScreen (SCRN),
				       x, y, alt.width, alt.height, MAGBORD,
					 0,	/* depth from parent */
					 InputOutput, CopyFromParent,
				 CWSaveUnder | CWBorderPixel | CWBackPixel |
					 CWOverrideRedirect, &attr);
      XSelectInput (DISP, WINDOW (alt), ExposureMask);
#else
      alt.win = (caddr_t) XCreateWindow (WINDOW (mane),
				       x, y, alt.width, alt.height, MAGBORD,
					 bdrmap, backmap);
      XSelectInput (WINDOW (alt), ExposeRegion);
#endif
      XMapWindow (DPY WINDOW (alt));
      alt_stat = 1;		/* waiting for exposure */
    }
}

#ifdef	TOOLKIT
 /*ARGSUSED*/
static void
handle_motion (widget, junk, event)
     Widget widget;
     caddr_t junk;
     XMotionEvent *event;
{
  new_mag_x = event->x;
  main_x = event->x_root - new_mag_x;
  new_mag_y = event->y;
  main_y = event->y_root - new_mag_y;
  mag_moved = (new_mag_x != mag_x || new_mag_y != mag_y);
}

#endif /* TOOLKIT */

static void
movemag (x, y)
     int x, y;
{
  int xx, yy;

  mag_x = x;
  mag_y = y;
  if (mag_x == new_mag_x && mag_y == new_mag_y)
    mag_moved = False;
  compute_mag_pos (&xx, &yy);
  XMoveWindow (DPY WINDOW (alt), xx, yy);
  scrollwindow (&alt, (x + mane_base_x) * mane.shrinkfactor - alt.width / 2,
		(y + mane_base_y) * mane.shrinkfactor - alt.height / 2);
}

#ifdef	TOOLKIT
 /*ARGSUSED*/
static void
handle_release (widget, junk, event)
     Widget widget;
     caddr_t junk;
     XButtonEvent *event;
#else /* !TOOLKIT */
static void
handle_release ()
#endif /* TOOLKIT */
{
  if (alt.win)
    if (alt_stat)
      alt_stat = -1;		/* destroy upon expose */
    else
      {
	XDestroyWindow (DPY WINDOW (alt));
	if (curr.win == alt.win)
	  alt_canit = True;
	alt.win = NULL;
	mag_moved = False;
	can_exposures (&alt);
      }
}

#ifdef	TOOLKIT
 /*ARGSUSED*/
static void
handle_exp (widget, windowrec, event)
     Widget widget;
     struct WindowRec *windowrec;
     register XExposeEvent *event;
{
  if (windowrec == &alt)
    if (alt_stat < 0)
      {				/* destroy upon exposure */
	alt_stat = 0;
	handle_release (widget, (caddr_t) NULL, (XButtonEvent *) event);
	return;
      }
    else
      alt_stat = 0;
  expose (windowrec, event->x, event->y, event->width, event->height);
}

#endif /* TOOLKIT */

/* |||
 *	Currently the event handler does not coordinate XCopyArea requests
 *	with GraphicsExpose events.  This can lead to problems if the window
 *	is partially obscured and one, for example, drags a scrollbar.
 */

#ifndef X10
#define	XKEY(ev)	(ev).xkey
#ifndef	TOOLKIT
#define	XANY(ev)	(ev).xany
#define	XCONFIG(ev)	(ev).xconfigure
#define	XEXPOSE(ev)	(ev).xexpose
#define	XMOTION(ev)	(ev).xmotion
#define	XBUTTON(ev)	(ev).xbutton
#define	ISEXPOSE(ev)	((ev).type == Expose)
#endif /* TOOLKIT */
#else /* X10 */
#define	XANY(ev)	(ev)
#define	XCONFIG(ev)	(*((XExposeEvent *) &(ev)))
#define	XEXPOSE(ev)	(*((XExposeEvent *) &(ev)))
#define	XMOTION(ev)	(*((XMouseMovedEvent *) &(ev)))
#define	XBUTTON(ev)	(*((XButtonEvent *) &(ev)))
#define	XKEY(ev)	(*((XKeyEvent *) &(ev)))
#define	ConfigureNotify	ExposeWindow
#define	Expose		ExposeRegion
#define	ISEXPOSE(ev)	((ev).type == ExposeWindow || (ev).type == ExposeRegion)
#define	MotionNotify	MouseMoved
#define	ButtonPress	ButtonPressed
#define	ButtonRelease	ButtonReleased
#define	KeyPress	KeyPressed
#endif /* X10 */

static void
keystroke (ch, number0, arg0, eventp)
     char ch;
     int number0;
     boolean arg0;
     XEvent *eventp;
{
  int next_page;
#ifdef	TOOLKIT
  Window ww;
#endif

  next_page = current_page;
  switch (ch)
    {
    case 'q':
    case '\003':		/* control-C */
    case '\004':		/* control-D */
#ifdef	VMS
    case '\032':		/* control-Z */
#endif
      exit (0);
    case 'n':
    case 'f':
    case ' ':
    case '\r':
    case '\n':
      /* scroll forward; i.e. go to relative page */
      next_page = current_page + (arg0 ? number0 : 1);
      break;
    case 'p':
    case 'b':
    case '\b':
    case '\177':		/* Del */
      /* scroll backward */
      next_page = current_page - (arg0 ? number0 : 1);
      break;
    case 'g':
      /* go to absolute page */
      next_page = (arg0 ? number0 - pageno_correct :
		   total_pages - 1);
      break;
    case 'P':			/* declare current page */
      pageno_correct = arg0 * number0 - current_page;
      return;
    case 'k':			/* toggle keep-position flag */
      keep_flag = (arg0 ? number0 : !keep_flag);
      return;
    case '\f':
      /* redisplay current page */
      break;
    case '^':
      home (True);
      return;
#ifdef	TOOLKIT
    case 'l':
      if (!x_bar)
	goto bad;
      XtCallCallbacks (x_bar, XtNscrollProc,
		       -2 * (int) clip_w / 3);
      return;
    case 'r':
      if (!x_bar)
	goto bad;
      XtCallCallbacks (x_bar, XtNscrollProc,
		       2 * (int) clip_w / 3);
      return;
    case 'u':
      if (!y_bar)
	goto bad;
      XtCallCallbacks (y_bar, XtNscrollProc,
		       -2 * (int) clip_h / 3);
      return;
    case 'd':
      if (!y_bar)
	goto bad;
      XtCallCallbacks (y_bar, XtNscrollProc,
		       2 * (int) clip_h / 3);
      return;
    case 'c':
      center (eventp->xkey.x, eventp->xkey.y);
      return;
    case 'M':
      XTranslateCoordinates (DISP, eventp->xkey.window,
			     WINDOW (mane), eventp->xkey.x, eventp->xkey.y,
			     &home_x, &home_y, &ww);	/* throw away last argument */
      home_x *= mane.shrinkfactor;
      home_y *= mane.shrinkfactor;
      return;
#ifdef	BUTTONS
    case 'x':
      if (arg0 && expert == (number0 != 0))
	return;
      if (expert)
	{			/* create buttons */
	  XtResizeWidget (vport_widget, window_w -= XTRA_WID, window_h,
			  0);
	  create_buttons ((XtArgVal) window_h);
	  expert = False;
	}
      else
	{			/* destroy buttons */
	  XtResizeWidget (vport_widget, window_w += XTRA_WID, window_h,
			  0);
	  XtDestroyWidget (right_widget);
	  XtDestroyWidget (line_widget);
	  expert = True;
	}
      return;
#endif /* BUTTONS */
#else /* !TOOLKIT */
    case 'l':
      if (mane.base_x <= 0)
	goto bad;
      scrollmane (mane.base_x - 2 * clip_w / 3, mane.base_y);
      return;
    case 'r':
      if (mane.base_x >= page_w - clip_w)
	goto bad;
      scrollmane (mane.base_x + 2 * clip_w / 3, mane.base_y);
      return;
    case 'u':
      if (mane.base_y <= 0)
	goto bad;
      scrollmane (mane.base_x, mane.base_y - 2 * clip_h / 3);
      return;
    case 'd':
      if (mane.base_y >= page_h - clip_h)
	goto bad;
      scrollmane (mane.base_x, mane.base_y + 2 * clip_h / 3);
      return;
    case 'c':			/* unchecked scrollmane() */
      scrollwindow (&mane, mane.base_x + XKEY (*eventp).x - clip_w / 2,
		    mane.base_y + XKEY (*eventp).y - clip_h / 2);
      if (x_bar)
	paint_x_bar ();
      if (y_bar)
	paint_y_bar ();
#ifndef X10
      XWarpPointer (DISP, None, None, 0, 0, 0, 0,
	      clip_w / 2 - XKEY (*eventp).x, clip_h / 2 - XKEY (*eventp).y);
#else
      XWarpMouse (WINDOW (mane), clip_w / 2, clip_h / 2, GXcopy);
#endif
      return;
    case 'M':
      home_x = (XKEY (*eventp).x - (y_bar ? BAR_THICK : 0)
		+ mane.base_x) * mane.shrinkfactor;
      home_y = (XKEY (*eventp).y - (x_bar ? BAR_THICK : 0)
		+ mane.base_y) * mane.shrinkfactor;
      return;
#endif /* TOOLKIT */

#ifndef X10
    case '\020':		/* Control P */
      Printf ("Unit = %d, bitord = %d, byteord = %d\n",
	      BitmapUnit (DISP), BitmapBitOrder (DISP),
	      ImageByteOrder (DISP));
      return;
#endif
    case 's':
      if (!arg0)
	{
	  int temp;
	  number0 = ROUNDUP (unshrunk_page_w, window_w - 2);
	  temp = ROUNDUP (unshrunk_page_h, window_h - 2);
	  if (number0 < temp)
	    number0 = temp;
	}
      if (number0 <= 0)
	goto bad;
      if (number0 == mane.shrinkfactor)
	return;
      mane.shrinkfactor = number0;
      init_page ();
      if (number0 != 1 && number0 != bak_shrink)
	{
	  bak_shrink = number0;
	  free_shrunken_fonts ();
	}
#ifdef	TOOLKIT
      draw_args[0].value = (XtArgVal) page_w;
      draw_args[1].value = (XtArgVal) page_h;
      XtSetValues (draw_widget, draw_args, (Cardinal) 2);
      get_geom ();
      home (False);
#else /* TOOLKIT */
      reconfig ();
      home (False);
#endif /* TOOLKIT */
      break;
    case 'S':
      if (!arg0)
	goto bad;
      if (number0 < 0)
	goto bad;
      if (number0 == density)
	return;
      density = number0;
      free_shrunken_fonts ();
      if (mane.shrinkfactor == 1)
	return;
      break;
    case 'R':
      /* reread DVI file */
      --dvi_time;		/* then it will notice a change */
      break;
    default:
      goto bad;
    }
  if (0 <= next_page && next_page < total_pages)
    {
      if (current_page != next_page)
	{
	  current_page = next_page;
	  hush_spec_now = hush_spec;
	  if (!keep_flag)
	    home (False);
	}
      canit = True;
      Flush ();
      return;			/* don't use longjmp here:  it might be called from
				 * within the toolkit, and we don't want to longjmp out
				 * of Xt routines. */
    }
bad:XBell (DISP, 10);
}

#ifndef X10
#define	TRSIZE	100
#endif /* X10 */
static void
read_events (wait)
     boolean wait;
{
  char ch;
  boolean arg0;
  int number0;
  XEvent event;
#ifndef X10
  char trbuf[TRSIZE];
#endif
  char *str;
  int nbytes;

  alt_canit = False;
  for (;;)
    {
      ch = '\0';
      event_counter = event_freq;
      /*
       * The above line clears the flag indicating that an event is
       * pending.  So if an event comes in right now, the flag will be
       * set again needlessly, but we just end up making an extra call.
       * Also, watch out, if we destroy the magnifying glass while
       * writing it.
       */
      if (!XtPending ()&& (!wait || canit || mane.min_x < MAXDIM ||
			   alt.min_x < MAXDIM || mag_moved))
	if (!wait && (canit || alt_canit))
	  longjmp (canit_env, 1);
	else
	  return;
#ifdef	TOOLKIT
      XtNextEvent (&event);
      if (resized)
	get_geom ();
      if (event.xany.window == WINDOW (alt) &&
	  event.type == Expose)
	{
	  handle_exp ((Widget) NULL, &alt, &event.xexpose);
	  continue;
	}
      if (event.type != KeyPress)
	{
	  XtDispatchEvent (&event);
	  continue;
	}
      str = trbuf;
      nbytes = XLookupString (&event.xkey, str, TRSIZE, NULL, NULL);
      if (nbytes > 1)
	ch = '?';
      if (nbytes != 0)
	ch = *str;
#else /* !TOOLKIT */

      XNextEvent (DPY & event);
      if (XANY (event).window == WINDOW (mane) ||
	  XANY (event).window == WINDOW (alt))
	{

	  struct WindowRec *wr = &mane;

	  if (XANY (event).window == WINDOW (alt))
	    {
	      wr = &alt;
	      /* check in case we already destroyed the window */
	      if (alt_stat < 0)
		{		/* destroy upon exposure */
		  alt_stat = 0;
		  handle_release ();
		  continue;
		}
	      else
		alt_stat = 0;
	    }
	  switch (event.type)
	    {
#ifndef X10
	    case GraphicsExpose:
#else
	    case ExposeWindow:
#endif
	    case Expose:
#ifdef X10
	      if (XEXPOSE (event).detail & ExposeCopy)
		ClearArea (event.window,
			   XEXPOSE (event).x, XEXPOSE (event).y,
			   XEXPOSE (event).width, XEXPOSE (event).height);
#endif
	      expose (wr, XEXPOSE (event).x, XEXPOSE (event).y,
		      XEXPOSE (event).width, XEXPOSE (event).height);
#ifdef X10
	    case ExposeCopy:	/* throw away junk event */
#endif
	      break;

	    case MotionNotify:
#ifdef X10
	    case LeftDownMotion:
	    case MiddleDownMotion:
	    case RightDownMotion:
#endif
	      new_mag_x = XMOTION (event).x;
	      new_mag_y = XMOTION (event).y;
	      mag_moved = (new_mag_x != mag_x || new_mag_y != mag_y);
	      break;

	    case ButtonPress:
	      handle_button (&XBUTTON (event));
	      break;

	    case ButtonRelease:
	      handle_release ();
	      break;
	    }			/* end switch */
	}			/* end if window == {mane,alt}.win */

      else if (XANY (event).window == x_bar)
	{
	  if (ISEXPOSE (event))
	    DarkenArea (x_bar, x_bgn, 1, x_end - x_bgn, BAR_WID);
	  else if (event.type == MotionNotify)
	    scrollmane (XMOTION (event).x * page_w / clip_w,
			mane.base_y);
#ifndef X10
	  else
	    switch (XBUTTON (event).button)
#else
	  else if (event.type == ButtonPress)
	    switch (3 - (XBUTTON (event).detail & ValueMask))
#endif
	      {
	      case 1:
		scrollmane (mane.base_x + XBUTTON (event).x, mane.base_y);
		break;
	      case 2:
		scrollmane (XBUTTON (event).x * page_w / clip_w,
			    mane.base_y);
		break;
	      case 3:
		scrollmane (mane.base_x - XBUTTON (event).x, mane.base_y);
	      }
	}

      else if (XANY (event).window == y_bar)
	{
	  if (ISEXPOSE (event))
	    DarkenArea (y_bar, 1, y_bgn, BAR_WID, y_end - y_bgn);
	  else if (event.type == MotionNotify)
	    scrollmane (mane.base_x,
			XMOTION (event).y * page_h / clip_h);
#ifndef X10
	  else
	    switch (XBUTTON (event).button)
#else
	  else if (event.type == ButtonPress)
	    switch (3 - (XBUTTON (event).detail & ValueMask))
#endif
	      {
	      case 1:
		scrollmane (mane.base_x, mane.base_y + XBUTTON (event).y);
		break;
	      case 2:
		scrollmane (mane.base_x,
			    XBUTTON (event).y * page_h / clip_h);
		break;
	      case 3:
		scrollmane (mane.base_x, mane.base_y - XBUTTON (event).y);
	      }
	}

      else if (XANY (event).window == top_level)
	switch (event.type)
	  {
	  case ConfigureNotify:
	    if (XANY (event).window == top_level &&
		(XCONFIG (event).width != window_w ||
		 XCONFIG (event).height != window_h))
	      {
		register caddr_t old_mane_win = mane.win;

		window_w = XCONFIG (event).width;
		window_h = XCONFIG (event).height;
		reconfig ();
		if (old_mane_win == NULL)
		  home (False);
	      }
	    break;

#ifndef X10
	  case MapNotify:	/* if running w/o WM */
	    if (mane.win == NULL)
	      {
		reconfig ();
		home (False);
	      }
	    break;
#endif

	  case KeyPress:
#ifndef X10
	    str = trbuf;
	    nbytes = XLookupStr (&event.xkey, str, TRSIZE, NULL,
				 NULL);
#else
	    str = XLookupMapping (&event, &nbytes);
#endif
	    if (nbytes > 1)
	      ch = '?';
	    if (nbytes != 0)
	      ch = *str;
	    break;
	  }
#endif /* TOOLKIT */
      if (ch == '\0')
	continue;
      if (ch >= '0' && ch <= '9')
	{
	  arg = True;
	  number = number * 10 + sign * (ch - '0');
	  continue;
	}
      else if (ch == '-')
	{
	  arg = True;
	  sign = -1;
	  number = 0;
	  continue;
	}
      number0 = number;
      number = 0;
      sign = 1;
      arg0 = arg;
      arg = False;
      keystroke (ch, number0, arg0, &event);
    }
}

static
redraw (windowrec)
     struct WindowRec *windowrec;
{
  char *errtext;
#ifdef X10
  static FontInfo *font = 0;
#endif

  curr = *windowrec;
  min_x = curr.min_x + curr.base_x;
  min_y = curr.min_y + curr.base_y;
  max_x = curr.max_x + curr.base_x;
  max_y = curr.max_y + curr.base_y;
  can_exposures (windowrec);

  if (debug & DBG_EVENT)
    Printf ("Redraw %d x %d at (%d, %d) (base=%d,%d)\n", max_x - min_x,
	    max_y - min_y, min_x, min_y, curr.base_x, curr.base_y);
  SetCursor (redraw_cursor);
  if (errtext = (char *) setjmp (dvi_env))
    {
      ClearPage (mane);
#ifndef X10
      get_xy ();
      XDrawString (DISP, WINDOW (mane), foreGC,
		   5 - window_x, 5 + X11HEIGHT - window_y,
		   errtext, strlen (errtext));
#else
      if (!font)
	font = XOpenFont (X10FONT);
      XTextMask (WINDOW (mane), 5, 5 + X10HEIGHT, errtext, strlen (errtext),
		 font->id, foreGC);
#endif
      if (dvi_file)
	{
	  Fclose (dvi_file);
	  dvi_file = NULL;
	}
    }
  else
    {
      draw_page ();
      hush_spec_now = True;
    }
}

redraw_page ()
{
  if (debug & DBG_EVENT)
    Fputs ("Redraw page:  ", stdout);
  get_xy ();
  ClearPage (mane);
  mane.min_x = -window_x;
  mane.max_x = -window_x + clip_w;
  mane.min_y = -window_y;
  mane.max_y = -window_y + clip_h;
  redraw (&mane);
}

/*
 *	Interrupt system for receiving events.  The program sets a flag
 *	whenever an event comes in, so that at the proper time (i.e., when
 *	reading a new dvi item), we can check incoming events to see if we
 *	still want to go on printing this page.  This way, one can stop
 *	displaying a page if it is about to be erased anyway.  We try to read
 *	as many events as possible before doing anything and base the next
 *	action on all events read.
 *	Note that the Xlib and Xt routines are not reentrant, so the most we
 *	can do is set a flag in the interrupt routine and check it later.
 *	Also, sometimes the interrupts are not generated (some systems only
 *	guarantee that SIGIO is generated for terminal files, and on the system
 *	I use, the interrupts are not generated if I use "(xdvi foo &)" instead
 *	of "xdvi foo").  Therefore, there is also a mechanism to check the
 *	event queue every 70 drawing operations or so.  This mechanism is
 *	disabled if it turns out that the interrupts do work.
 *	For a fuller discussion of some of the above, see xlife in
 *	comp.sources.x.
 */

static void
can_exposures (windowrec)
     struct WindowRec *windowrec;
{
  windowrec->min_x = windowrec->min_y = MAXDIM;
  windowrec->max_x = windowrec->max_y = 0;
}

static void
handle_intr ()
{
  event_counter = 1;
  event_freq = -1;		/* forget Plan B */
}

#ifdef	HAS_SIGIO
static void
enable_intr ()
{
  int socket = ConnectionNumber (DISP);
  if (!isatty (0))
    {
      puts ("trying...");
      if (dup2 (socket, 0) == -1)
	perror (prog);
      socket = 0;
    }
  (void) signal (SIGIO, handle_intr);
  (void) fcntl (socket, F_SETOWN, getpid ());
  (void) fcntl (socket, F_SETFL, fcntl (socket, F_GETFL, 0) | FASYNC);
}

#endif /* HAS_SIGIO */

static
do_pages ()
{
  if (debug & DBG_BATCH)
    {
#ifdef	TOOLKIT
      while (mane.min_x == MAXDIM)
	read_events (True);
#else /* !TOOLKIT */
      while (mane.min_x == MAXDIM)
	if (setjmp (canit_env))
	  break;
	else
	  read_events (True);
#endif /* TOOLKIT */
      for (current_page = 0; current_page < total_pages; ++current_page)
	redraw_page ();
      exit (0);
    }
  else
    {				/* normal operation */
#ifdef	HAS_SIGIO
      enable_intr ();
#endif
      (void) setjmp (canit_env);
      for (;;)
	{
	  if (mane.win)
	    SetCursor (ready_cursor);
	  read_events (True);
	  if (canit)
	    {
	      canit = False;
	      can_exposures (&mane);
	      can_exposures (&alt);
	      redraw_page ();
	    }
	  else if (mag_moved)
	    {
	      if (alt.win == NULL)
		mag_moved = False;
	      else if (abs (new_mag_x - mag_x) >
		       2 * abs (new_mag_y - mag_y))
		movemag (new_mag_x, mag_y);
	      else if (abs (new_mag_y - mag_y) >
		       2 * abs (new_mag_x - mag_x))
		movemag (mag_x, new_mag_y);
	      else
		movemag (new_mag_x, new_mag_y);
	    }
	  else if (alt.min_x < MAXDIM)
	    redraw (&alt);
	  else if (mane.min_x < MAXDIM)
	    redraw (&mane);
	  Flush ();
	}
    }
}

static
usage ()
{
#ifndef X10
#ifndef	VMS
#ifdef	BUTTONS
  Fputs ("\
Usage: xdvi [+[<page>]] [-s <shrink>] [-S <density>] [-p <pixels>] [-l] [-rv]\n\
	[-expert] [-paper <papertype>] [-mgs[n] <size>] [-altfont <font>]\n\
	[-margins <dimen>] [-sidemargin <dimen>] [-topmargin <dimen>]\n\
	[-offsets <dimen>] [-xoffset <dimen>] [-yoffset <dimen>] [-keep]\n\
	[-hushspecials] [-hushchars] [-hush]\n\
	[-fg <color>] [-bg <color>] [-hl <color>] [-bd <color>] \
[-cr <color>]\n\
	[-bw <width>] [-geometry <geometry>] [-icongeometry <geometry>]\n\
	[-iconic] [-display <host:display>] [-copy] [-thorough] dvi_file\n",
	 stderr);
#else /* !BUTTONS */
  Fputs ("\
Usage: xdvi [+[<page>]] [-s <shrink>] [-S <density>] [-p <pixels>] [-l] [-rv]\n\
	[-paper <papertype>] [-mgs[n] <size>] [-altfont <font>]\n\
	[-margins <dimen>] [-sidemargin <dimen>] [-topmargin <dimen>]\n\
	[-offsets <dimen>] [-xoffset <dimen>] [-yoffset <dimen>] [-keep]\n\
	[-hushspecials] [-hushchars] [-hush]\n\
	[-fg <color>] [-bg <color>] [-hl <color>] [-bd <color>] \
[-cr <color>]\n\
	[-bw <width>] [-geometry <geometry>] [-icongeometry <geometry>]\n\
	[-iconic] [-display <host:display>] [-copy] [-thorough] dvi_file\n",
	 stderr);
#endif /* BUTTONS */
#else /* VMS */
  Fputs ("\
Usage: xdvi [+[<page>]] [-s <shrink>] [-density <%>] [-p <pixels>] [-l] [-rv]\n\
	[-paper <papertype>] [-mgs[n] <size>] [-altfont <font>]\n\
	[-margins <dimen>] [-sidemargin <dimen>] [-topmargin <dimen>]\
\n", stderr);
  Fputs ("\
	[-offsets <dimen>] [-xoffset <dimen>] [-yoffset <dimen>] [-keep]\n\
	[-hushspecials] [-hushchars] [-hush]\n\
	[-fg <color>] [-bg <color>] [-hl <color>] [-bd <color>] \
[-cr <color>]\n\
	[-bw <width>] [-geometry <geometry>] [-icongeometry <geometry>]\n\
	[-iconic] [-display <host::display>] [-copy] [-thorough] dvi_file\n",
	 stderr);
#endif /* VMS */
#else /* X10 */
  Fputs ("\
Usage: xdvi [+[<page>]] [-s <shrink>] [-S <density>] [-p <pixels>] [-l]\n\
	[-paper <papertype>] [-mgs[n] <size>] [-altfont <font>]\n\
	[-margins <dimen>] [-sidemargin <dimen>] [-topmargin <dimen>]\n\
	[-offsets <dimen>] [-xoffset <dimen>] [-yoffset <dimen>] [-keep]\n\
	[-hushspecials] [-hushchars] [-hush]\n\
	[-fg <color>] [-bg <color>] [-hl <color>] [-bd <color>] \
[-cr <color>]\n\
	[-bw <width>] [-geometry <geometry> | =<geometry>]\n\
	[-display <host:display> | <host:display>] dvi_file\n", stderr);
#endif /* X10 */
  exit (1);
}

static int
atopix (arg)
     char *arg;
{
  int len = strlen (arg);

  return (len > 2 && arg[len - 2] == 'c' && arg[len - 1] == 'm' ?
	  1.0 / 2.54 : 1.0) * atof (arg) * pixels_per_inch + 0.5;
}

/**
 **	Main programs start here.
 **/

#ifndef X10
static char *icon_geometry;
static boolean copy = 2;
static boolean thorough;
#endif /* X10 */

#ifdef	TOOLKIT

static XrmOptionDescRec options[]=
{
  {"-d", ".debugLevel", XrmoptionSepArg, (caddr_t) NULL},
  {"-s", ".shrinkFactor", XrmoptionSepArg, (caddr_t) NULL},
#ifndef	VMS
  {"-S", ".densityPercent", XrmoptionSepArg, (caddr_t) NULL},
#endif
  {"-density", ".densityPercent", XrmoptionSepArg, (caddr_t) NULL},
  {"-p", ".pixelsPerInch", XrmoptionSepArg, (caddr_t) NULL},
  {"-margins", ".Margin", XrmoptionSepArg, (caddr_t) NULL},
  {"-sidemargin", ".sideMargin", XrmoptionSepArg, (caddr_t) NULL},
  {"-topmargin", ".topMargin", XrmoptionSepArg, (caddr_t) NULL},
  {"-offsets", ".Offset", XrmoptionSepArg, (caddr_t) NULL},
  {"-xoffset", ".xOffset", XrmoptionSepArg, (caddr_t) NULL},
  {"-yoffset", ".yOffset", XrmoptionSepArg, (caddr_t) NULL},
  {"-paper", ".paper", XrmoptionSepArg, (caddr_t) NULL},
  {"-altfont", ".altFont", XrmoptionSepArg, (caddr_t) NULL},
  {"-l", ".listFonts", XrmoptionNoArg, (caddr_t) "on"},
  {"+l", ".listFonts", XrmoptionNoArg, (caddr_t) "off"},
  {"-hushspecials", ".hushSpecials", XrmoptionNoArg, (caddr_t) "on"},
  {"+hushspecials", ".hushSpecials", XrmoptionNoArg, (caddr_t) "off"},
  {"-hushchars", ".hushLostChars", XrmoptionNoArg, (caddr_t) "on"},
  {"+hushchars", ".hushLostChars", XrmoptionNoArg, (caddr_t) "off"},
  {"-hush", ".Hush", XrmoptionNoArg, (caddr_t) "on"},
  {"+hush", ".Hush", XrmoptionNoArg, (caddr_t) "off"},
  {"-fg", ".foreground", XrmoptionSepArg, (caddr_t) NULL},
  {"-foreground", ".foreground", XrmoptionSepArg, (caddr_t) NULL},
  {"-bg", ".background", XrmoptionSepArg, (caddr_t) NULL},
  {"-background", ".background", XrmoptionSepArg, (caddr_t) NULL},
  {"-hl", ".highlight", XrmoptionSepArg, (caddr_t) NULL},
  {"-cr", ".cursorColor", XrmoptionSepArg, (caddr_t) NULL},
  {"-icongeometry", ".iconGeometry", XrmoptionSepArg, (caddr_t) NULL},
  {"-keep", ".keepPosition", XrmoptionNoArg, (caddr_t) "on"},
  {"+keep", ".keepPosition", XrmoptionNoArg, (caddr_t) "off"},
  {"-copy", ".copy", XrmoptionNoArg, (caddr_t) "on"},
  {"+copy", ".copy", XrmoptionNoArg, (caddr_t) "off"},
  {"-thorough", ".thorough", XrmoptionNoArg, (caddr_t) "on"},
  {"+thorough", ".thorough", XrmoptionNoArg, (caddr_t) "off"},
#ifdef	BUTTONS
  {"-expert", ".expert", XrmoptionNoArg, (caddr_t) "on"},
  {"+expert", ".expert", XrmoptionNoArg, (caddr_t) "off"},
#endif
  {"-mgs", ".magnifierSize1", XrmoptionSepArg, (caddr_t) NULL},
  {"-mgs1", ".magnifierSize1", XrmoptionSepArg, (caddr_t) NULL},
  {"-mgs2", ".magnifierSize2", XrmoptionSepArg, (caddr_t) NULL},
  {"-mgs3", ".magnifierSize3", XrmoptionSepArg, (caddr_t) NULL},
  {"-mgs4", ".magnifierSize4", XrmoptionSepArg, (caddr_t) NULL},
  {"-mgs5", ".magnifierSize5", XrmoptionSepArg, (caddr_t) NULL},
};

static XtResource resources[]=
{
  {"debugLevel", "DebugLevel", XtRString, sizeof (char *),
   (Cardinal) & debug_arg, XtRString, NULL},
  {"shrinkFactor", "ShrinkFactor", XtRInt, sizeof (int),
   (Cardinal) & shrink_factor, XtRInt, (caddr_t) & shrink_factor},
  {"densityPercent", "DensityPercent", XtRInt, sizeof (int),
   (Cardinal) & density, XtRInt, (caddr_t) & density},
  {"pixelsPerInch", "PixelsPerInch", XtRInt, sizeof (int),
   (Cardinal) & pixels_per_inch, XtRInt, (caddr_t) & pixels_per_inch},
  {"sideMargin", "Margin", XtRString, sizeof (char *),
   (Cardinal) & sidemargin, XtRString, NULL},
  {"topMargin", "Margin", XtRString, sizeof (char *),
   (Cardinal) & topmargin, XtRString, NULL},
  {"xOffset", "Offset", XtRString, sizeof (char *),
   (Cardinal) & xoffset, XtRString, NULL},
  {"yOffset", "Offset", XtRString, sizeof (char *),
   (Cardinal) & yoffset, XtRString, NULL},
  {"paper", "Paper", XtRString, sizeof (char *),
   (Cardinal) & paper, XtRString, (caddr_t) DEFAULT_PAPER},
  {"altFont", "AltFont", XtRString, sizeof (char *),
   (Cardinal) & alt_font, XtRString, (caddr_t) ALTFONT},
  {"listFonts", "ListFonts", XtRBoolean, sizeof (boolean),
   (Cardinal) & list_fonts, XtRBoolean, (caddr_t) & list_fonts},
  {"reverseVideo", "ReverseVideo", XtRBoolean, sizeof (boolean),
   (Cardinal) & reverse, XtRBoolean, (caddr_t) & reverse},
  {"hushSpecials", "Hush", XtRBoolean, sizeof (boolean),
   (Cardinal) & hush_spec, XtRBoolean, (caddr_t) & hush_spec},
  {"hushLostChars", "Hush", XtRBoolean, sizeof (boolean),
   (Cardinal) & hush_chars, XtRBoolean, (caddr_t) & hush_chars},
  {"foreground", "Foreground", XtRPixel, sizeof (Pixel),
   (Cardinal) & fore_Pixel, XtRPixel, (caddr_t) & fore_Pixel},
  {"foreground", "Foreground", XtRString, sizeof (char *),
   (Cardinal) & fore_color, XtRString, NULL},
  {"background", "Background", XtRPixel, sizeof (Pixel),
   (Cardinal) & back_Pixel, XtRPixel, (caddr_t) & back_Pixel},
  {"background", "Background", XtRString, sizeof (char *),
   (Cardinal) & back_color, XtRString, NULL},
  {"borderColor", "BorderColor", XtRPixel, sizeof (Pixel),
   (Cardinal) & brdr_Pixel, XtRPixel, (caddr_t) & brdr_Pixel},
  {"borderColor", "BorderColor", XtRString, sizeof (char *),
   (Cardinal) & brdr_color, XtRString, NULL},
  {"highlight", "Highlight", XtRPixel, sizeof (Pixel),
   (Cardinal) & hl_Pixel, XtRPixel, (caddr_t) & hl_Pixel},
  {"highlight", "Highlight", XtRString, sizeof (char *),
   (Cardinal) & high_color, XtRString, NULL},
  {"cursorColor", "CursorColor", XtRPixel, sizeof (Pixel),
   (Cardinal) & cr_Pixel, XtRPixel, (caddr_t) & cr_Pixel},
  {"cursorColor", "CursorColor", XtRString, sizeof (char *),
   (Cardinal) & curs_color, XtRString, NULL},
  {"iconGeometry", "IconGeometry", XtRString, sizeof (char *),
   (Cardinal) & icon_geometry, XtRString, NULL},
  {"keepPosition", "KeepPosition", XtRBoolean, sizeof (boolean),
   (Cardinal) & keep_flag, XtRBoolean, (caddr_t) & keep_flag},
  {"copy", "Copy", XtRBoolean, sizeof (boolean),
   (Cardinal) & copy, XtRBoolean, (caddr_t) & copy},
  {"thorough", "Thorough", XtRBoolean, sizeof (boolean),
   (Cardinal) & thorough, XtRBoolean, (caddr_t) & thorough},
#ifdef	BUTTONS
  {"expert", "Expert", XtRBoolean, sizeof (boolean),
   (Cardinal) & expert, XtRBoolean, (caddr_t) & expert},
#endif
  {"magnifierSize1", "MagnifierSize", XtRInt, sizeof (int),
   (Cardinal) & mg_size[0], XtRInt, (caddr_t) & mg_size[0]},
  {"magnifierSize2", "MagnifierSize", XtRInt, sizeof (int),
   (Cardinal) & mg_size[1], XtRInt, (caddr_t) & mg_size[1]},
  {"magnifierSize3", "MagnifierSize", XtRInt, sizeof (int),
   (Cardinal) & mg_size[2], XtRInt, (caddr_t) & mg_size[2]},
  {"magnifierSize4", "MagnifierSize", XtRInt, sizeof (int),
   (Cardinal) & mg_size[3], XtRInt, (caddr_t) & mg_size[3]},
  {"magnifierSize5", "MagnifierSize", XtRInt, sizeof (int),
   (Cardinal) & mg_size[4], XtRInt, (caddr_t) & mg_size[4]},
};

static Arg temp_args1[]=
{
  {XtNiconX, (XtArgVal) 0},
  {XtNiconY, (XtArgVal) 0},
};

static Arg temp_args2 =
{XtNborderWidth, (XtArgVal) & bwidth};

static Arg temp_args3[]=
{
  {XtNwidth, (XtArgVal) 0},
  {XtNheight, (XtArgVal) 0},
  {XtNiconPixmap, (XtArgVal) 0},
  {XtNinput, (XtArgVal) True},
  {XtNtitle, (XtArgVal) 0},
};

#else /* !TOOLKIT */

static char *display;
static char *geometry;
static char *margins;
static char *offsets;
static boolean hush;

#ifndef X10
static boolean iconic = False;

static Pixel
string_to_pixel (strp)		/* adapted from the toolkit */
     char **strp;
{
  char *str = *strp;
  Status status;
  XColor color, junk;

  if (*str == '#')
    {				/* an rgb definition */
      status = XParseColor (DISP, DefaultColormapOfScreen (SCRN),
			    str, &color);
      if (status != 0)
	status = XAllocColor (DISP, DefaultColormapOfScreen (SCRN),
			      &color);
    }
  else				/* a name */
    status = XAllocNamedColor (DISP, DefaultColormapOfScreen (SCRN),
			       str, &color, &junk);
  if (status == 0)
    {
      Fprintf (stderr, "Cannot allocate colormap entry for \"%s\"\n", str);
      *strp = NULL;
      return (Pixel) 0;
    }
  return color.pixel;
}

#endif /* X10 */

static struct option
{
  char *name;
  char *resource;
  enum
  {
    FalseArg, TrueArg, StickyArg, SepArg
  } argclass;
  enum
  {
    booleanArg, StringArg, NumberArg
  } argtype;
  int classcount;
  caddr_t address;
} options[]=

{
  /* the display option MUST be first */
  {
    "-display", NULL, SepArg, StringArg, 1, (caddr_t) & display
  },
  {
    "-d", "debugLevel", SepArg, StringArg, 1, (caddr_t) & debug_arg
  },
  {
    "+", NULL, StickyArg, StringArg, 1, (caddr_t) & curr_page
  },
  {
    "-s", "shrinkFactor", SepArg, NumberArg, 1, (caddr_t) & shrink_factor
  },
  {
    "-S", NULL, SepArg, NumberArg, 2, (caddr_t) & density
  },
  {
    "-density", "densityPercent", SepArg, NumberArg, 1, (caddr_t) & density
  },
  {
    "-p", "pixelsPerInch", SepArg, NumberArg, 1, (caddr_t) & pixels_per_inch
  },
  {
    "-margins", "Margin", SepArg, StringArg, 3, (caddr_t) & margins
  },
  {
    "-sidemargin", "sideMargin", SepArg, StringArg, 1, (caddr_t) & sidemargin
  },
  {
    "-topmargin", "topMargin", SepArg, StringArg, 1, (caddr_t) & topmargin
  },
  {
    "-offsets", "Offset", SepArg, StringArg, 3, (caddr_t) & offsets
  },
  {
    "-xoffset", "xOffset", SepArg, StringArg, 1, (caddr_t) & xoffset
  },
  {
    "-yoffset", "yOffset", SepArg, StringArg, 1, (caddr_t) & yoffset
  },
  {
    "-paper", "paper", SepArg, StringArg, 1, (caddr_t) & paper
  },
  {
    "-altfont", "altFont", SepArg, StringArg, 1, (caddr_t) & alt_font
  },
  {
    "-l", NULL, TrueArg, booleanArg, 2, (caddr_t) & list_fonts
  },
  {
    "+l", "listFonts", FalseArg, booleanArg, 1, (caddr_t) & list_fonts
  },
  {
    "-rv", NULL, TrueArg, booleanArg, 2, (caddr_t) & reverse
  },
  {
    "+rv", "reverseVideo", FalseArg, booleanArg, 1, (caddr_t) & reverse
  },
  {
    "-hush", NULL, TrueArg, booleanArg, 6, (caddr_t) & hush
  },
  {
    "+hush", "Hush", FalseArg, booleanArg, 5, (caddr_t) & hush
  },
  {
    "-hushspecials", NULL, TrueArg, booleanArg, 2, (caddr_t) & hush_spec
  },
  {
    "+hushspecials", "hushSpecials", FalseArg, booleanArg, 1, (caddr_t) & hush_spec
  },
  {
    "-hushchars", NULL, TrueArg, booleanArg, 2, (caddr_t) & hush_chars
  },
  {
    "+hushchars", "hushLostChars", FalseArg, booleanArg, 1, (caddr_t) & hush_chars
  },
  {
    "-bw", NULL, SepArg, NumberArg, 2, (caddr_t) & bwidth
  },
  {
    "-borderwidth", "borderWidth", SepArg, NumberArg, 1, (caddr_t) & bwidth
  },
  {
    "-fg", NULL, SepArg, StringArg, 2, (caddr_t) & fore_color
  },
  {
    "-foreground", "foreground", SepArg, StringArg, 1, (caddr_t) & fore_color
  },
  {
    "-bg", NULL, SepArg, StringArg, 2, (caddr_t) & back_color
  },
  {
    "-background", "background", SepArg, StringArg, 1, (caddr_t) & back_color
  },
  {
    "-bd", NULL, SepArg, StringArg, 2, (caddr_t) & brdr_color
  },
  {
    "-bordercolor", "borderColor", SepArg, StringArg, 1, (caddr_t) & brdr_color
  },
  {
    "-hl", "highlight", SepArg, StringArg, 1, (caddr_t) & high_color
  },
  {
    "-cr", "cursorColor", SepArg, StringArg, 1, (caddr_t) & curs_color
  },
#ifdef	X10
  {
    "=", NULL, StickyArg, StringArg, 2, (caddr_t) & geometry
  },
#endif
  {
    "-geometry", "geometry", SepArg, StringArg, 1, (caddr_t) & geometry
  },
#ifndef	X10
  {
    "-icongeometry", "iconGeometry", StickyArg, StringArg, 1, (caddr_t) & icon_geometry
  },
  {
    "-iconic", NULL, TrueArg, booleanArg, 2, (caddr_t) & iconic
  },
  {
    "+iconic", "iconic", FalseArg, booleanArg, 1, (caddr_t) & iconic
  },
  {
    "-keep", NULL, TrueArg, booleanArg, 2, (caddr_t) & keep_flag
  },
  {
    "+keep", "keepPosition", FalseArg, booleanArg, 1, (caddr_t) & keep_flag
  },
  {
    "-copy", NULL, TrueArg, booleanArg, 2, (caddr_t) & copy
  },
  {
    "+copy", "copy", FalseArg, booleanArg, 1, (caddr_t) & copy
  },
  {
    "-thorough", NULL, TrueArg, booleanArg, 2, (caddr_t) & thorough
  },
  {
    "+thorough", "thorough", FalseArg, booleanArg, 1, (caddr_t) & thorough
  },
#endif /* X10 */
  {
    "-mgs", NULL, SepArg, NumberArg, 2, (caddr_t) & mg_size[0]
  },
  {
    "-mgs1", "magnifierSize1", SepArg, NumberArg, 1, (caddr_t) & mg_size[0]
  },
  {
    "-mgs2", "magnifierSize2", SepArg, NumberArg, 1, (caddr_t) & mg_size[1]
  },
  {
    "-mgs3", "magnifierSize3", SepArg, NumberArg, 1, (caddr_t) & mg_size[2]
  },
#ifndef X10
  {
    "-mgs4", "magnifierSize4", SepArg, NumberArg, 1, (caddr_t) & mg_size[3]
  },
  {
    "-mgs5", "magnifierSize5", SepArg, NumberArg, 1, (caddr_t) & mg_size[4]
  },
#endif
};

/*
 *	Process the option table.  This is not guaranteed for all possible
 *	option tables, but at least it works for this one.
 */

static void
parse_options (argc, argv)
     int argc;
     char **argv;
{
  char **arg;
  char **argvend = argv + argc;
  char *optstring;
  caddr_t addr;
  struct option *opt, *lastopt, *candidate;
  int len1, len2, matchlen;

  /*
   * Step 1.  Process command line options.
   */
  for (arg = argv + 1; arg < argvend; ++arg)
    {
      len1 = strlen (*arg);
      candidate = NULL;
      matchlen = 0;
      for (opt = options; opt < options + XtNumber (options); ++opt)
	{
	  len2 = strlen (opt->name);
	  if (opt->argclass == StickyArg)
	    {
	      if (matchlen <= len2 && !strncmp (*arg, opt->name, len2))
		{
		  candidate = opt;
		  matchlen = len2;
		}
	    }
	  else if (len1 <= len2 && matchlen <= len1 &&
		   !strncmp (*arg, opt->name, len1))
	    {
	      if (len1 == len2)
		{
		  candidate = opt;
		  break;
		}
	      if (matchlen < len1)
		candidate = opt;
	      else if (candidate && candidate->argclass != StickyArg)
		candidate = NULL;
	      matchlen = len1;
	    }
	}
      if (candidate == NULL)
	{
#ifdef	X10
	  if (**arg == '-')
	    usage ();
	  if (index (*arg, ':') != NULL)
	    {			/* display */
	      --arg;
	      candidate = options;
	    }
	  else if (dvi_name)
	    usage ();
#else
	  if (**arg == '-' || dvi_name)
	    usage ();
#endif
	  else
	    {
	      dvi_name = *arg;
	      continue;
	    }
	}
      addr = candidate->address;
      switch (candidate->argclass)
	{
	case FalseArg:
	  *((boolean *) addr) = False;
	  break;
	case TrueArg:
	  *((boolean *) addr) = True;
	  break;
	case StickyArg:
	  optstring = *arg + strlen (candidate->name);
	  break;
	case SepArg:
	  ++arg;
	  if (arg >= argvend)
	    usage ();
	  optstring = *arg;
	  break;
	}
      switch (candidate->argtype)
	{
	case StringArg:
	  *((char **) addr) = optstring;
	  break;
	case NumberArg:
	  *((int *) addr) = atoi (optstring);
	  break;
	}
      /* flag it for subsequent processing */
      candidate->resource = (char *) candidate;
    }
  /*
   * Step 2.  Propagate classes for command line arguments.  Backwards.
   */
  for (opt = options + XtNumber (options) - 1; opt >= options; --opt)
    if (opt->resource == (char *) opt)
      {
	addr = opt->address;
	lastopt = opt + opt->classcount;
	for (candidate = opt; candidate < lastopt; ++candidate)
	  {
	    if (candidate->resource != NULL)
	      {
		switch (opt->argtype)
		  {
		  case booleanArg:
		    *((boolean *) candidate->address) =
		      *((boolean *) addr);
		    break;
		  case StringArg:
		    *((char **) candidate->address) =
		      *((char **) addr);
		    break;
		  case NumberArg:
		    *((int *) candidate->address) = *((int *) addr);
		    break;
		  }
		candidate->resource = NULL;
	      }
	  }
      }

#ifndef X10
  if ((DISP = XOpenDisplay (display)) == NULL)
    oops ("Can't open display");
  SCRN = DefaultScreenOfDisplay (DISP);
#else
  if (XOpenDisplay (display) == NULL)
    oops ("Can't open display");
#endif
  /*
   * Step 3.  Handle resources (including classes).
   */
  for (opt = options; opt < options + XtNumber (options); ++opt)
    if (opt->resource &&
#ifndef X10
	((optstring = XGetDefault (DISP, prog, opt->resource)) ||
	 (optstring = XGetDefault (DISP, "XDvi", opt->resource))))
#else
	(optstring = XGetDefault (DPY prog, opt->resource)))
#endif
    {
      lastopt = opt + opt->classcount;
      for (candidate = opt; candidate < lastopt; ++candidate)
	if (candidate->resource != NULL)
	  switch (opt->argtype)
	    {
	    case StringArg:
	      *((char **) candidate->address) = optstring;
	      break;
	    case NumberArg:
	      *((int *) candidate->address) = atoi (optstring);
	      break;
	    case booleanArg:
	      *((boolean *) candidate->address) =
		(strcmp (optstring, "on") == 0);
	    }
    }
}

#endif /* TOOLKIT */

static char *paper_types[]=
{
  "us", "8.5x11",
  "usr", "11x8.5",
  "legal", "8.5x14",
  "foolscap", "13.5x17.0",	/* ??? */

 /* ISO `A' formats, Portrait */
  "a1", "59.4x84.0cm",
  "a2", "42.0x59.4cm",
  "a3", "29.7x42.0cm",
  "a4", "21.0x29.7cm",
  "a5", "14.85x21.0cm",
  "a6", "10.5x14.85cm",
  "a7", "7.42x10.5cm",

 /* ISO `A' formats, Landscape */
  "a1r", "84.0x59.4cm",
  "a2r", "59.4x42.0cm",
  "a3r", "42.0x29.7cm",
  "a4r", "29.7x21.0cm",
  "a5r", "21.0x14.85cm",
  "a6r", "14.85x10.5cm",
  "a7r", "10.5x7.42cm",

 /* ISO `B' formats, Portrait */
  "b1", "70.6x100.0cm",
  "b2", "50.0x70.6cm",
  "b3", "35.3x50.0cm",
  "b4", "25.0x35.3cm",
  "b5", "17.6x25.0cm",
  "b6", "13.5x17.6cm",
  "b7", "8.8x13.5cm",

 /* ISO `B' formats, Landscape */
  "b1r", "100.0x70.6cm",
  "b2r", "70.6x50.0cm",
  "b3r", "50.0x35.3cm",
  "b4r", "35.3x25.0cm",
  "b5r", "25.0x17.6cm",
  "b6r", "17.6x13.5cm",
  "b7r", "13.5x8.8cm",

 /* ISO `C' formats, Portrait */
  "c1", "64.8x91.6cm",
  "c2", "45.8x64.8cm",
  "c3", "32.4x45.8cm",
  "c4", "22.9x32.4cm",
  "c5", "16.2x22.9cm",
  "c6", "11.46x16.2cm",
  "c7", "8.1x11.46cm",

 /* ISO `C' formats, Landscape */
  "c1r", "91.6x64.8cm",
  "c2r", "64.8x45.8cm",
  "c3r", "45.8x32.4cm",
  "c4r", "32.4x22.9cm",
  "c5r", "22.9x16.2cm",
  "c6r", "16.2x11.46cm",
  "c7r", "11.46x8.1cm",
};

static boolean
set_paper_type ()
{
  char *arg;
  char temp[21];
  char **p;
  char *q;

  if (strlen (paper) > sizeof (temp) - 1)
    return False;
  arg = paper;
  q = temp;
  for (;;)
    {				/* convert to lower case */
      char c = *arg++;
      if (c >= 'A' && c <= 'Z')
	c ^= ('a' ^ 'A');
      *q++ = c;
      if (c == '\0')
	break;
    }
  arg = temp;
  /* perform substitutions */
  for (p = paper_types; p < paper_types + XtNumber (paper_types); p += 2)
    if (strcmp (temp, *p) == 0)
      {
	arg = p[1];
	break;
      }
  q = index (arg, 'x');
  if (q == NULL)
    return False;
  unshrunk_paper_w = atopix (arg);
  unshrunk_paper_h = atopix (q + 1);
  return (unshrunk_paper_w != 0 && unshrunk_paper_h != 0);
}

/*
 *	main program
 */
int
main (argc, argv)
     int argc;
     char **argv;
{

#ifdef	TOOLKIT
#ifdef	BUTTONS
  int xtra_wid;
#endif
#else /* !TOOLKIT */
#ifndef X10
  XSizeHints size_hints;
  XWMHints wmhints;
#else
  OpaqueFrame frame;
  char def[32];
  int mouspix;
  Color cdef;
  int x_thick, y_thick;
#endif
#endif /* TOOLKIT */
  int screen_w, screen_h;

#ifndef	VMS
  prog = rindex (*argv, '/');
#else
  prog = rindex (*argv, ']');
#endif
  if (prog != NULL)
    ++prog;
  else
    prog = *argv;

#ifdef	VMS
  if (index (prog, '.') != NULL)
    *index (prog, '.') = '\0';
#endif

#ifdef	TOOLKIT
  top_level = XtInitialize (prog, "XDvi", options, XtNumber (options),
			    &argc, argv);
  while (--argc > 0)
    {
      if (*(*++argv) == '+')
	if (curr_page != NULL)
	  usage ();
	else
	  curr_page = *argv + 1;
      else if (dvi_name != NULL)
	usage ();
      else
	dvi_name = *argv;
    }

  XtGetApplicationResources (top_level, (caddr_t) NULL, resources,
			     XtNumber (resources), NULL, 0);
  DISP = XtDisplay (top_level);
  SCRN = XtScreen (top_level);

#else /* !TOOLKIT */

  parse_options (argc, argv);
#ifndef X10
  if (fore_color)
    fore_Pixel = string_to_pixel (&fore_color);
  if (back_color)
    back_Pixel = string_to_pixel (&back_color);
  if (brdr_color)
    brdr_Pixel = string_to_pixel (&brdr_color);
  if (high_color)
    hl_Pixel = string_to_pixel (&high_color);
  if (curs_color)
    cr_Pixel = string_to_pixel (&curs_color);
#endif

#endif /* TOOLKIT */

  if (shrink_factor <= 0 || density <= 0 || pixels_per_inch <= 0 ||
      dvi_name == NULL)
    usage ();
  if (shrink_factor != 1)
    bak_shrink = shrink_factor;
  mane.shrinkfactor = shrink_factor;
  specialConv = pixels_per_inch / 1000.0;
  if (debug_arg != NULL)
    debug = isdigit (*debug_arg) ? atoi (debug_arg) : DBG_ALL;
  if (sidemargin)
    home_x = atopix (sidemargin);
  if (topmargin)
    home_y = atopix (topmargin);
  offset_x = xoffset ? atopix (xoffset) : pixels_per_inch;
  offset_y = yoffset ? atopix (yoffset) : pixels_per_inch;
  if (!set_paper_type ())
    oops ("Don't recognize paper type %s", paper);

  init_pxl_open ();
  open_dvi_file ();
  if (curr_page)
    {
      current_page = (*curr_page ? atoi (curr_page) : total_pages) - 1;
      if (current_page < 0 || current_page >= total_pages)
	usage ();
    }

#ifndef X10

  /*
   *	X11 colors
   */

  if (reverse)
    {
      if (!fore_color)
	fore_Pixel = WhitePixelOfScreen (SCRN);
      if (!back_color)
	back_Pixel = BlackPixelOfScreen (SCRN);
      fore_color = back_color = (char *) &fore_color;	/* nonzero */
    }
  else
    {
      if (!fore_color)
	fore_Pixel = BlackPixelOfScreen (SCRN);
      if (!back_color)
	back_Pixel = WhitePixelOfScreen (SCRN);
    }
  if (!brdr_color)
    brdr_Pixel = fore_Pixel;
  {
    XGCValues values;
    Pixel set_bits = (Pixel) (fore_Pixel & ~back_Pixel);
    Pixel clr_bits = (Pixel) (back_Pixel & ~fore_Pixel);
#define	MakeGC(fcn, fg, bg)	(values.function = fcn, values.foreground=fg,\
		values.background=bg,\
		XCreateGC(DISP, RootWindowOfScreen(SCRN),\
			GCFunction|GCForeground|GCBackground, &values))

    if (copy == 2)
      copy = (PlanesOfScreen (SCRN) > 1);
    if (copy || (set_bits && clr_bits))
      ruleGC = MakeGC (GXcopy, fore_Pixel, back_Pixel);
    if (copy)
      foreGC = ruleGC;
    else if (!thorough && ruleGC)
      {
	foreGC = ruleGC;
	puts ("Note:  overstrike characters may be incorrect.");
      }
    else
      {
	if (set_bits)
	  foreGC = MakeGC (GXor, set_bits, 0);
	if (clr_bits)
	  *(foreGC ? &foreGC2 : &foreGC) =
	    MakeGC (GXandInverted, clr_bits, 0);
	if (!ruleGC)
	  ruleGC = foreGC;
      }
    highGC = ruleGC;
    if (high_color)
      highGC = MakeGC (GXcopy, hl_Pixel, back_Pixel);
  }

#ifndef	VMS
  ready_cursor = XCreateFontCursor (DISP, XC_cross);
  redraw_cursor = XCreateFontCursor (DISP, XC_watch);
#else
  DECWCursorFont = XLoadFont (DISP, "DECW$CURSOR");
  XSetFont (DISP, foreGC, DECWCursorFont);
  redraw_cursor = XCreateGlyphCursor (DISP, DECWCursorFont, DECWCursorFont,
				 decw$c_wait_cursor, decw$c_wait_cursor + 1,
				      &fore_color, &back_color);
  MagnifyPixmap = XCreateBitmapFromData (DISP, RootWindowOfScreen (SCRN),
			 mag_glass_bits, mag_glass_width, mag_glass_height);
  ready_cursor = XCreatePixmapCursor (DISP, MagnifyPixmap, MagnifyPixmap,
		&back_color, &fore_color, mag_glass_x_hot, mag_glass_y_hot);
#endif /* VMS */

  if (!curs_color)
    cr_Pixel = high_color ? hl_Pixel : fore_Pixel;
  {
    XColor bg_Color, cr_Color;

    bg_Color.pixel = back_Pixel;
    XQueryColor (DISP, DefaultColormapOfScreen (SCRN), &bg_Color);
    cr_Color.pixel = cr_Pixel;
    XQueryColor (DISP, DefaultColormapOfScreen (SCRN), &cr_Color);
    XRecolorCursor (DISP, ready_cursor, &cr_Color, &bg_Color);
    XRecolorCursor (DISP, redraw_cursor, &cr_Color, &bg_Color);
  }

#ifdef	TOOLKIT

  /*
   *	X11 windows (toolkit)
   */

  /* The following code is lifted from Xterm */
  if (icon_geometry != NULL)
    {
      int scr, junk;

      for (scr = 0;		/* yyuucchh */
	   SCRN != ScreenOfDisplay (DISP, scr);
	   scr++);

      XGeometry (DISP, scr, icon_geometry, "", 0, 0, 0, 0, 0,
		 (int *) &temp_args1[0].value,
		 (int *) &temp_args1[1].value, &junk, &junk);
      XtSetValues (top_level, temp_args1, XtNumber (temp_args1));
    }
  /* Set default window size and icon */
  XtGetValues (top_level, &temp_args2, 1);	/* get border width */
  screen_w = WidthOfScreen (SCRN) - 2 * bwidth;
  screen_h = HeightOfScreen (SCRN) - 2 * bwidth;
#ifdef	BUTTONS
  xtra_wid = expert ? 0 : XTRA_WID;
#else
#define	xtra_wid	0
#endif
  temp_args3[0].value = (XtArgVal) (page_w + xtra_wid < screen_w ?
				    page_w + xtra_wid : screen_w);
  temp_args3[1].value = (XtArgVal) (page_h < screen_h ? page_h : screen_h);
  temp_args3[2].value = (XtArgVal) (XCreateBitmapFromData (DISP,
						  RootWindowOfScreen (SCRN),
				       xdvi_bits, xdvi_width, xdvi_height));
  temp_args3[4].value = (XtArgVal) dvi_name;
  XtSetValues (top_level, temp_args3, XtNumber (temp_args3));

#ifdef	BUTTONS
  form_widget = XtCreateManagedWidget ("form", formWidgetClass,
				top_level, form_args, XtNumber (form_args));

  vport_args[0].value = temp_args3[0].value - xtra_wid;
  vport_args[1].value = temp_args3[1].value;
  vport_widget = XtCreateManagedWidget ("vport", viewportWidgetClass,
			    form_widget, vport_args, XtNumber (vport_args));

  line_args[0].value = (XtArgVal) high_color ? hl_Pixel : fore_Pixel;
  if (!expert)
    create_buttons (temp_args3[1].value);
#else /* !BUTTONS */
  vport_widget = XtCreateManagedWidget ("vport", viewportWidgetClass,
			      top_level, vport_args, XtNumber (vport_args));
#define	form_widget	vport_widget	/* for calls to XtAddEventHandler */
#endif /* BUTTONS */
  clip_widget = XtNameToWidget (vport_widget, "clip");
  draw_args[0].value = (XtArgVal) page_w;
  draw_args[1].value = (XtArgVal) page_h;
  draw_widget = XtCreateManagedWidget ("drawing", widgetClass,
			     vport_widget, draw_args, XtNumber (draw_args));
  if (fore_color)
    XtSetValues (draw_widget, &fore_args, 1);
  if (back_color)
    {
      XtSetValues (draw_widget, &back_args, 1);
      XtSetValues (clip_widget, &back_args, 1);
    }
  XtAddEventHandler (form_widget, KeyPressMask, False, handle_key,
		     (caddr_t) NULL);
  XtAddEventHandler (vport_widget, StructureNotifyMask, False,
		     handle_resize, (caddr_t) NULL);
  XtAddEventHandler (draw_widget, ExposureMask, False, handle_exp,
		     (caddr_t) & mane);
  XtAddEventHandler (draw_widget, ButtonPressMask, False, handle_button,
		     (caddr_t) NULL);
  XtAddEventHandler (draw_widget, ButtonMotionMask, False, handle_motion,
		     (caddr_t) NULL);
  XtAddEventHandler (draw_widget, ButtonReleaseMask, False, handle_release,
		     (caddr_t) NULL);
  XtRealizeWidget (top_level);

  curr.win = mane.win = (caddr_t) XtWindow (draw_widget);

#else /* !TOOLKIT */

  /*
   *	X11 windows (non toolkit)
   */

  screen_w = WidthOfScreen (SCRN) - 2 * bwidth;
  screen_h = HeightOfScreen (SCRN) - 2 * bwidth;
  size_hints.flags = PMinSize;
  size_hints.min_width = size_hints.min_height = 50;
  size_hints.x = size_hints.y = 0;
  if (geometry != NULL)
    {
      int flag = XParseGeometry (geometry, &size_hints.x, &size_hints.y,
				 &window_w, &window_h);

      if (flag & (XValue | YValue))
	size_hints.flags |= USPosition;
      if (flag & (WidthValue | HeightValue))
	size_hints.flags |= USSize;
      if (flag & XNegative)
	size_hints.x += screen_w - window_w;
      if (flag & YNegative)
	size_hints.y += screen_h - window_h;
    }
  if (!(size_hints.flags & USSize))
    {
      int x_thick = 0;
      int y_thick = 0;
      if (screen_w < page_w)
	x_thick = BAR_THICK;
      if (screen_h < page_h + x_thick)
	y_thick = BAR_THICK;
      window_w = page_w + y_thick;
      if (window_w > screen_w)
	{
	  x_thick = BAR_THICK;
	  window_w = screen_w;
	}
      window_h = page_h + x_thick;
      if (window_h > screen_h)
	window_h = screen_h;
      size_hints.flags |= PSize;
    }
  size_hints.width = window_w;
  size_hints.height = window_h;
  top_level = XCreateSimpleWindow (DISP, RootWindowOfScreen (SCRN),
		     size_hints.x, size_hints.y, window_w, window_h, bwidth,
				   brdr_Pixel, back_Pixel);
  XSetStandardProperties (DISP, top_level, dvi_name, prog, NULL,
			  argv, argc, &size_hints);

  wmhints.flags = InputHint | StateHint | IconPixmapHint;
  wmhints.input = True;		/* window manager must direct input */
  wmhints.initial_state = iconic ? IconicState : NormalState;
  wmhints.icon_pixmap = XCreateBitmapFromData (DISP,
					       RootWindowOfScreen (SCRN),
					xdvi_bits, xdvi_width, xdvi_height);
  if (icon_geometry != NULL)
    {
      int junk;

      wmhints.flags |= IconPositionHint;
      XGeometry (DISP, DefaultScreen (DISP), icon_geometry, "",
	     0, 0, 0, 0, 0, &wmhints.icon_x, &wmhints.icon_y, &junk, &junk);
    }
  XSetWMHints (DISP, top_level, &wmhints);

  XSelectInput (DISP, top_level, KeyPressMask | StructureNotifyMask);
  XMapWindow (DISP, top_level);
  Flush ();

#endif /* TOOLKIT */

  XRebindKeysym (DISP, XK_Home, NULL, 0, (ubyte *) "^", 1);
  XRebindKeysym (DISP, XK_Left, NULL, 0, (ubyte *) "l", 1);
  XRebindKeysym (DISP, XK_Up, NULL, 0, (ubyte *) "u", 1);
  XRebindKeysym (DISP, XK_Right, NULL, 0, (ubyte *) "r", 1);
  XRebindKeysym (DISP, XK_Down, NULL, 0, (ubyte *) "d", 1);
  XRebindKeysym (DISP, XK_Prior, NULL, 0, (ubyte *) "b", 1);
  XRebindKeysym (DISP, XK_Next, NULL, 0, (ubyte *) "f", 1);

  image = XCreateImage (DISP, DefaultVisualOfScreen (SCRN), 1, XYBitmap, 0,
			(char *) NULL, 0, 0, BITS_PER_BMUNIT, 0);
  image->bitmap_unit = BITS_PER_BMUNIT;
#ifndef	MSBITFIRST
  image->bitmap_bit_order = LSBFirst;
#else
  image->bitmap_bit_order = MSBFirst;
#endif
  {
    short endian = MSBFirst << 8 | LSBFirst;
    image->byte_order = *((char *) &endian);
  }

#else /* X10 */

  /*
   *	X10 colors
   */

  if (reverse)
    {
      foreGC = WhitePixel;
      highGC = WhitePixel;
      backpix = BlackPixel;
      backmap = BlackPixmap;
      bdrmap = WhitePixmap;
      mouspix = WhitePixel;
      GXfunc = GXor;
    }
  else
    {
      foreGC = BlackPixel;
      highGC = BlackPixel;
      backpix = WhitePixel;
      backmap = WhitePixmap;
      bdrmap = BlackPixmap;
      mouspix = BlackPixel;
      GXfunc = GXand;
    }
  if (DisplayCells ()> 2)
    {
      if (fore_color && XParseColor (fore_color, &cdef) &&
	  XGetHardwareColor (&cdef))
	foreGC = cdef.pixel;
      if (back_color && XParseColor (back_color, &cdef) &&
	  XGetHardwareColor (&cdef))
	{
	  backpix = cdef.pixel;
	  backmap = XMakeTile (backpix);
	}
      if (brdr_color && XParseColor (brdr_color, &cdef) &&
	  XGetHardwareColor (&cdef))
	bdrmap = XMakeTile (cdef.pixel);
      if (high_color && XParseColor (high_color, &cdef) &&
	  XGetHardwareColor (&cdef))
	highGC = cdef.pixel;
      if (curs_color && XParseColor (curs_color, &cdef) &&
	  XGetHardwareColor (&cdef))
	mouspix = cdef.pixel;
    }

  /*
   *	X10 windows
   */

  frame.bdrwidth = bwidth;
  screen_w = DisplayWidth ()- 2 * bwidth;
  screen_h = DisplayHeight ()- 2 * bwidth;
  x_thick = y_thick = 0;
  if (screen_w < page_w)
    x_thick = BAR_THICK;
  if (screen_h < page_h + x_thick)
    y_thick = BAR_THICK;
  frame.width = page_w + y_thick;
  if (frame.width > screen_w)
    {
      x_thick = BAR_THICK;
      frame.width = screen_w;
    }
  frame.height = page_h + x_thick;
  if (frame.height > screen_h)
    frame.height = screen_h;
  frame.border = bdrmap;
  frame.background = backmap;
  frame.x = 0;
  frame.y = 0;
  Sprintf (def, "=%dx%d+0+0", frame.width, frame.height);
  top_level = XCreate ("DVI Previewer", prog, geometry, def,
		       &frame, 50, 50);
  XSelectInput (top_level, ExposeWindow | KeyPressed);
  XMapWindow (top_level);
  XDefineCursor (top_level,
	  XCreateCursor (xdvi_width, xdvi_height, xdvi_bits, xdvi_mask_bits,
			 xdvi_x_hot, xdvi_y_hot, mouspix, backpix, GXcopy));
#endif /* X10 */

  do_pages ();
  /* NOTREACHED */
}
