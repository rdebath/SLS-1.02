#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)tty_newtxt.c 1.36 91/09/14";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 * 
 * tty_newtxt.c	-- faster routines for text output
 * 
 * Experimental routines for faster text thruput. Jim Becker 4/3/89
 * 
 * These routines are being added as needed to speed up the text output. Note
 * that, since I am not changing the main body of code, any initialization of
 * the routines is done the `firsttime' they are called.
 * 
 * This logic is being augmented to perform text clipping on the line level for
 * the rendering of text to the screen. The current tty and text logic always
 * repaints all of the screen. This prevents lines that are out of the
 * clipping regions from being displayed.
 * 
 * There are three local GCs that are allocated in this file for all the text
 * needs.
 */


#include <xview/window.h>
#include <xview_private/pw_impl.h>
#include <pixrect/pixrect.h>
#include <pixrect/pixfont.h>
#include <xview/xv_xrect.h>
#include <xview_private/i18n_impl.h>
#include <xview/font.h>

#define	BADVAL		-1
#define	ALL		AllPlanes
#define	MAX_LINES	128

#define	convert_op(op)	XlatOp[(op) >> PIX_OP_SHIFT]

#ifdef OW_I18N
extern int      XwcDrawString(), XwcDrawImageString();
#endif
extern int      XDrawImageString(), XDrawString();
extern Xv_xrectlist *screen_get_clip_rects();

typedef struct tty_gc_list {
    int depth;
    unsigned long fore, back;
    GC  gcs[3];
    struct tty_gc_list *next;
} Tty_GC_List;

#define INVERTED_GC	0
#define DEFAULT_GC	1
#define BACK_GC		2

int TTY_CURRENT_FONT_KEY;
int TTY_GC_LIST_KEY;
static int      XlatOp[16];
static short    line_visible[MAX_LINES];
static short    filter_lines = FALSE;
static int      font_height = 0;


/*
 * create a local GC for the user. This is used the first time that the text
 * routines are used to make the three GC used herein.
 */

static          GC
create_GC(display, drawable, foreground, background, function)
    Display        *display;
    Drawable        drawable;
    int             foreground;
    int             background;
    int             function;
{
    unsigned long   gcvaluemask;
    XGCValues       gcvalues;
    GC              gc;

    gcvalues.function = function;
    gcvalues.plane_mask = AllPlanes;
    gcvalues.foreground = foreground;
    gcvalues.background = background;

    gcvaluemask = GCFunction | GCPlaneMask |
	GCForeground | GCBackground;

    gc = XCreateGC(display, drawable, gcvaluemask, &gcvalues);

    return gc;
}

/*
 * get_gc_list - return a list of GC's that can be used with the given 
 * 		 window's drawable info.
 */
static GC *
get_gc_list(info)
    Xv_Drawable_info *info;
{
    Display *display;
    Xv_Screen	screen;
    Drawable xid;
    Tty_GC_List *front, *gc_list;
    int depth;
    unsigned long fore, back;
    
    screen = xv_screen(info);

    depth = xv_depth(info);
    fore = xv_fg(info);
    back = xv_bg(info);
    front = (Tty_GC_List *)xv_get(screen, XV_KEY_DATA, TTY_GC_LIST_KEY);
    gc_list = front;
    while (gc_list != (Tty_GC_List *)NULL) {
	if (gc_list->depth == depth &&
	    gc_list->fore == fore &&
	    gc_list->back == back)
	  return(gc_list->gcs);
	else
	  gc_list = gc_list->next;
    }

    /* Couldn't find one that was cached, so create a new list */
    display = xv_display(info);
    xid = xv_xid(info);
    gc_list = (Tty_GC_List *)malloc(sizeof(Tty_GC_List));
    gc_list->depth = depth;
    gc_list->fore = fore;
    gc_list->back = back;
    gc_list->next = front;
    gc_list->gcs[INVERTED_GC] = create_GC(display, xid, fore ^ back, back, GXxor);
    gc_list->gcs[DEFAULT_GC] = create_GC(display, xid, fore, back, GXcopy);
    gc_list->gcs[BACK_GC] = create_GC(display, xid, fore, back, GXcopy);
    xv_set(screen, XV_KEY_DATA, TTY_GC_LIST_KEY, gc_list, NULL);
    return(gc_list->gcs);
}

/*
 * set the fonts in the GCs to the Pixfont specified. called once only!
 */
static void
setup_font(window, pixfont)
    Xv_opaque       window;
    Xv_opaque       pixfont;
{
#ifdef OW_I18N
    XFontSet            font_set;
    XFontSetExtents     *font_set_extents;

    font_set = (XFontSet)xv_get(pixfont, FONT_SET_ID);
    font_set_extents = (XFontSetExtents *)XExtentsOfFontSet(font_set);
    font_height = font_set_extents->max_logical_extent.height;
#else
    Xv_Drawable_info *info;
    Display        *display;
    Font            font;
    XFontStruct    *fontinfo;
    GC		    *gc_list;

    DRAWABLE_INFO_MACRO(window, info);
    display = xv_display(info);
    font = (Font) xv_get(pixfont, XV_XID);

    /* it should always be valid, but be careful */
    if (font != NULL) {
	gc_list = get_gc_list(info);
	
	XSetFont(display, gc_list[DEFAULT_GC], font);
	XSetFont(display, gc_list[INVERTED_GC], font);
	
	/* determine font height -- don't trust globals!! */
	fontinfo = (XFontStruct *)xv_get(pixfont, FONT_INFO);
	
	font_height = fontinfo->ascent + fontinfo->descent;
    }
#endif
}

/*
 * setup the correct pens and such in the graphics context
 */
static void
setup_GC(display, info, gc, pix_op)
    Display        *display;
    Xv_Drawable_info *info;
    GC              gc;
    int             pix_op;
{
    int		lfore,
		lback;
    int             lplanes = AllPlanes;
    int             lfunc = convert_op(pix_op);


    if(! info)
      return;

    lfore = xv_fg(info);
    lback = xv_bg(info);

    /* convert functions from SunView expectation to X understanding */
    switch (lfunc) {
      case GXclear:
	lfore = lback;
	lfunc = GXcopy;
	break;
      case GXset:
	lfunc = GXcopy;
	break;
      case GXxor:
	lfore = lfore ^ lback;
	break;
      case GXinvert:
	lplanes = lfore ^ lback;
	break;
      case GXcopyInverted: /* jcb 11/15/89:1/23/90  to fix lack of highlight */
	lfore = lback;
	lback = xv_fg(info);
 	lfunc = GXcopy;
    }

    XSetState(display, gc, lfore, lback, lfunc, lplanes);
}


/*
 * called the firsttime we have some access to this module to perform
 * self-initialization.. This keeps this code self-contained to some degree,
 * as it can be called from anywhere.
 */
static void
firsttime_init()
{
    if (!TTY_GC_LIST_KEY) 
        TTY_GC_LIST_KEY = xv_unique_key();

    /* init the opcode translation table */
    /* this table was stolen from xv_rop.c	 */
    XlatOp[PIX_CLR >> PIX_OP_SHIFT] = GXclear;
    XlatOp[PIX_SET >> PIX_OP_SHIFT] = GXcopy;
    XlatOp[PIX_DST >> PIX_OP_SHIFT] = GXnoop;
    XlatOp[PIX_NOT(PIX_DST) >> PIX_OP_SHIFT] = GXinvert;
    XlatOp[PIX_SRC >> PIX_OP_SHIFT] = GXcopy;
    XlatOp[PIX_NOT(PIX_SRC) >> PIX_OP_SHIFT] = GXcopyInverted;
    XlatOp[(PIX_SRC & PIX_DST) >> PIX_OP_SHIFT] = GXand;
    XlatOp[(PIX_SRC & PIX_NOT(PIX_DST)) >> PIX_OP_SHIFT] = GXandReverse;
    XlatOp[(PIX_NOT(PIX_SRC) & PIX_DST) >> PIX_OP_SHIFT] = GXandInverted;
    XlatOp[(PIX_SRC ^ PIX_DST) >> PIX_OP_SHIFT] = GXxor;
    XlatOp[(PIX_SRC | PIX_DST) >> PIX_OP_SHIFT] = GXor;
    XlatOp[(PIX_NOT(PIX_SRC) & PIX_NOT(PIX_DST)) >> PIX_OP_SHIFT] = GXnor;
    XlatOp[(PIX_NOT(PIX_SRC) ^ PIX_DST) >> PIX_OP_SHIFT] = GXequiv;
    XlatOp[(PIX_SRC | PIX_NOT(PIX_DST)) >> PIX_OP_SHIFT] = GXorReverse;
    XlatOp[(PIX_NOT(PIX_SRC) | PIX_DST) >> PIX_OP_SHIFT] = GXorInverted;
    XlatOp[(PIX_NOT(PIX_SRC) | PIX_NOT(PIX_DST)) >> PIX_OP_SHIFT] = GXnand;
}

/*
 * basic mechanism is to cache the information between use of this routine.
 * this assumes that certain of the attributes are not going to change.
 * 
 * Note that this also assumes that all will be the same dependent on the
 * "window" pointer. [There would still be too much work to look at the even
 * the window XID for me to be happy...]
 */
void
tty_newtext(window, xbasew, ybasew, op, pixfont, string, len)
    Xv_opaque       window;
    int             op;
    register int    xbasew, ybasew;
    Xv_opaque       pixfont;
    CHAR	   *string;
    int             len;
{
    static int      old_op = BADVAL;
    Xv_Drawable_info *info;
    Display *display;
    Drawable drawable;
    static GC      *gc;
    static int      (*routine) ();
    Xv_Screen screen;
    static Xv_Screen old_screen;
    static int	     old_depth = 0;
    int             new_fg, new_bg;
    GC		   *gc_list;
    XGCValues      *gv;

    if (len == 0)
	return;

    /* determine if this line is clipped from sight */
    if (filter_lines && !line_visible[ybasew / font_height])
	return;

    DRAWABLE_INFO_MACRO(window, info);
    display = xv_display(info);
    drawable = xv_xid(info);
    screen = xv_screen(info);

    if (!TTY_GC_LIST_KEY)
      firsttime_init();
    gc_list = get_gc_list(info);

    if (!TTY_CURRENT_FONT_KEY)
      TTY_CURRENT_FONT_KEY = xv_unique_key();
    
    if (pixfont != (Xv_opaque) xv_get(screen, XV_KEY_DATA, TTY_CURRENT_FONT_KEY)) {
        setup_font(window, pixfont);
        xv_set(screen, XV_KEY_DATA, TTY_CURRENT_FONT_KEY, pixfont, 0);
    }
    
    if (((op = PIX_OP(op)) != old_op) || 
	(screen != old_screen) || (xv_depth(info) != old_depth)){
        old_screen = screen;
	old_depth = xv_depth(info);
        if (op == PIX_NOT(PIX_DST)) {
	    gc = &gc_list[INVERTED_GC];
	} else {
	    gc = &gc_list[DEFAULT_GC];
	    setup_GC(display, info, *gc, op);
	}

#ifdef OW_I18N
        if (op == PIX_SRC || op == PIX_NOT(PIX_SRC))
            routine = XwcDrawImageString;
        else
            routine = XwcDrawString;
#else
	if (op == PIX_SRC || op == PIX_NOT(PIX_SRC))
	  routine = XDrawImageString;
	else
	  routine = XDrawString;
#endif
	
	old_op = op;
    }
    gv = &(*gc)->values;
    new_fg = xv_fg(info);
    new_bg = xv_bg(info);
    if (((new_fg != gv->foreground) || 
	 (new_bg != gv->background)) && 
	(op != PIX_NOT(PIX_SRC)/* jcb -- in this case pens switched */)) {

	XGCValues       gc_values;

	if (gc == &gc_list[INVERTED_GC]) {
	    gc_values.foreground = new_fg ^ new_bg;
	} else {
	    gc_values.foreground = new_fg;
	}
	gc_values.background = new_bg;

	XChangeGC(display, *gc, GCForeground | GCBackground, &gc_values);
    }
#ifdef OW_I18N
    (void) (*routine) (display, drawable, xv_get(pixfont, FONT_SET_ID),
                *gc, xbasew, ybasew, string, len);
#else
    (void) (*routine) (display, drawable, *gc, xbasew, ybasew, string, len);
#endif
}




/*
 * routine to set, clear or invert the background.
 * 
 * takes the same parameters as pw_writebackground()
 */

void
tty_background(window, x, y, w, h, op)
    Xv_opaque       window;
    int             x, y, w, h;
    int             op;
{
    Xv_Drawable_info *info;
    Display *display;
    Drawable drawable;
    Xv_Screen screen;
    GC	*gc_list;


    DRAWABLE_INFO_MACRO(window, info);
    display = xv_display(info);
    drawable = xv_xid(info);
    screen = xv_screen(info);

    if (!TTY_GC_LIST_KEY)
      firsttime_init();
    gc_list = get_gc_list(info);
    
    setup_GC(display, info, gc_list[BACK_GC] , op);
    XFillRectangle(display,drawable, gc_list[BACK_GC], x, y, w, h);
}

/*
 * copy bits from one place to another on same window.
 */
void
tty_copyarea(window, sX, sY, W, H, dX, dY)
    Xv_opaque       window;
    int             sX, sY, W, H, dX, dY;
{
    Xv_Drawable_info *info;
    Display        *display;
    Drawable        drawable;
    GC	*gc_list;

    DRAWABLE_INFO_MACRO(window, info);
    display = xv_display(info);
    drawable = xv_xid(info);

    /* reset to normal mode for copying */
    if (!TTY_GC_LIST_KEY)
      firsttime_init();
    gc_list = get_gc_list(info);

    XSetState(display, gc_list[BACK_GC], xv_fg(info), xv_bg(info), GXcopy, AllPlanes);
    XCopyArea(display, drawable, drawable, gc_list[BACK_GC],
		sX, sY, W, H, dX, dY);
}



/*
 * The following routines are used to interact with the cliping regions that
 * have been setup by the XEvent handling logic. This is done because the
 * current implementation of the text logic renders the entire rectangle of
 * the text during repaint events. These clipping rectangles minimize the
 * visible flash to the user.
 */

void
tty_setup_clip_rectangles(window)
    Xv_opaque       window;
{
    Xv_Drawable_info *info;
    Display        *display;
    Xv_Screen       screen;
    Xv_xrectlist   *clip_xrects;
    int             count;
    XRectangle     *rects;
    XRectangle     *rptr;
    int             i, j;
    int             nlines, fline;
    GC	*gc_list;

    DRAWABLE_INFO_MACRO(window, info);
    display = xv_display(info);
    screen = xv_screen(info);
    clip_xrects = screen_get_clip_rects(screen);
    count = clip_xrects->count;
    rects = clip_xrects->rect_array;
    rptr = rects;

    if (!TTY_GC_LIST_KEY)
      firsttime_init();
    gc_list = get_gc_list(info);

    /* we have some cliprects and the char height is setup.. */
    if (count > 0 && font_height > 0) {

	/* make all the clipping regions bigger */
/*	for (i = 0, rptr = rects; i < count; i++, rptr++) {

	    if (rptr->y > 0) {
 		rptr->height += rptr->y;
		rptr->y = 0;
	    }
	}
*/
	/* set on all the possible gc's we have */
    XSetClipRectangles(display, gc_list[DEFAULT_GC],
    	0, 0, rects, count, Unsorted);
					 
    XSetClipRectangles(display, gc_list[INVERTED_GC],
        0, 0, rects, count, Unsorted);
							  
    XSetClipRectangles(display, gc_list[BACK_GC],
        0, 0, rects, count, Unsorted);

	/* setup internal line clipping logic array */
	for (i = 0; i < MAX_LINES; i++)
	    line_visible[i] = (short) FALSE;

	for (i = 0, rptr = rects; i < count; i++, rptr++) {

	    /* mark lines in the region as being paintable */
	    fline = rptr->y / font_height;
	    nlines = ((int)rptr->height + font_height) / font_height;

	    /* set visible, include next line because of baseline calc */
	    for (j = 0; j <= nlines; j++)
		line_visible[j + fline] = (short) TRUE;
	}

	filter_lines = TRUE;
    }
}

void
tty_clear_clip_rectangles(window)
    Xv_opaque       window;
{
    Display        *display;
    Xv_Drawable_info *info;
    GC	*gc_list;

    DRAWABLE_INFO_MACRO(window, info);
    display = xv_display(info);

    if (!TTY_GC_LIST_KEY)
      firsttime_init();
    gc_list = get_gc_list(info);

    XSetClipMask(display, gc_list[INVERTED_GC], None);
    XSetClipMask(display, gc_list[DEFAULT_GC], None);
    XSetClipMask(display, gc_list[BACK_GC], None);
    filter_lines = FALSE;
}
