/*
 * $XConsortium: fontgrid.c,v 1.25 91/07/18 14:59:57 rws Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Jim Fulton, MIT X Consortium
 */


#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/SimpleP.h>
#include <X11/Xmu/Converters.h>
#include "fontgridP.h"


#define Bell(w) XBell(XtDisplay(w), 50)


static void ClassInitialize(), Initialize(), Realize(), Redisplay(), Notify();
static void Destroy(), Resize(), paint_grid();
static Boolean SetValues();

#define Offset(field) XtOffsetOf(FontGridRec, fontgrid.field)

static XtResource resources[] = {
    { XtNfont, XtCFont, XtRFontStruct, sizeof(XFontStruct *),
	Offset(text_font), XtRString, (XtPointer) NULL },
    { XtNcellColumns, XtCCellColumns, XtRInt, sizeof(int),
	Offset(cell_cols), XtRImmediate, (XtPointer) 0 },
    { XtNcellRows, XtCCellRows, XtRInt, sizeof(int),
	Offset(cell_rows), XtRImmediate, (XtPointer) 0 },
    { XtNcellWidth, XtCCellWidth, XtRInt, sizeof(int),
	Offset(cell_width), XtRImmediate, (XtPointer) 0 },
    { XtNcellHeight, XtCCellHeight, XtRInt, sizeof(int),
	Offset(cell_height), XtRImmediate, (XtPointer) 0 },
    { XtNstartChar, XtCStartChar, XtRDimension, sizeof(Dimension),
	Offset(start_char), XtRImmediate, (XtPointer) 0xffff },
    { XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
	Offset(foreground_pixel), XtRString, (XtPointer) XtDefaultForeground },
    { XtNcenterChars, XtCCenterChars, XtRBoolean, sizeof(Boolean),
	Offset(center_chars), XtRImmediate, (XtPointer) FALSE },
    { XtNboxChars, XtCBoxChars, XtRBoolean, sizeof(Boolean),
	Offset(box_chars), XtRImmediate, (XtPointer) FALSE },
    { XtNboxColor, XtCForeground, XtRPixel, sizeof(Pixel),
	Offset(box_pixel), XtRString, (XtPointer) XtDefaultForeground },
    { XtNcallback, XtCCallback, XtRCallback, sizeof(XtPointer),
	Offset(callbacks), XtRCallback, (XtPointer) NULL },
    { XtNinternalPad, XtCInternalPad, XtRInt, sizeof(int),
	Offset(internal_pad), XtRImmediate, (XtPointer) 4 },
    { XtNgridWidth, XtCGridWidth, XtRInt, sizeof(int),
	Offset(grid_width), XtRImmediate, (XtPointer) 1 },
};

#undef Offset

static char defaultTranslations[] = 
  "<ButtonPress>:  notify()";

static XtActionsRec actions_list[] = {
    { "notify",		Notify },
};

FontGridClassRec fontgridClassRec = {
  { /* core fields */
    /* superclass               */      (WidgetClass) &simpleClassRec,
    /* class_name               */      "FontGrid",
    /* widget_size              */      sizeof(FontGridRec),
    /* class_initialize         */      ClassInitialize,
    /* class_part_initialize    */      NULL,
    /* class_inited             */      FALSE,
    /* initialize               */      Initialize,
    /* initialize_hook          */      NULL,
    /* realize                  */      Realize,
    /* actions                  */      actions_list,
    /* num_actions              */      XtNumber(actions_list),
    /* resources                */      resources,
    /* num_resources            */      XtNumber(resources),
    /* xrm_class                */      NULLQUARK,
    /* compress_motion          */      TRUE,
    /* compress_exposure        */      TRUE,
    /* compress_enterleave      */      TRUE,
    /* visible_interest         */      FALSE,
    /* destroy                  */      Destroy,
    /* resize                   */      Resize,
    /* expose                   */      Redisplay,
    /* set_values               */      SetValues,
    /* set_values_hook          */      NULL,
    /* set_values_almost        */      XtInheritSetValuesAlmost,
    /* get_values_hook          */      NULL,
    /* accept_focus             */      NULL,
    /* version                  */      XtVersion,
    /* callback_private         */      NULL,
    /* tm_table                 */      defaultTranslations,
    /* query_geometry           */      XtInheritQueryGeometry,
    /* display_accelerator      */      XtInheritDisplayAccelerator,
    /* extension                */      NULL
  },
  { /* simple fields */
    /* change_sensitive		*/	XtInheritChangeSensitive
  }
};

WidgetClass fontgridWidgetClass = (WidgetClass) &fontgridClassRec;


/*
 * public routines
 */

void GetFontGridCellDimensions (w, startp, ncolsp, nrowsp)
    Widget w;
    Dimension *startp;
    int *ncolsp, *nrowsp;
{
    FontGridWidget fgw = (FontGridWidget) w;
    *startp = (long)fgw->fontgrid.start_char;
    *ncolsp = fgw->fontgrid.cell_cols;
    *nrowsp = fgw->fontgrid.cell_rows;
}


void GetPrevNextStates (w, prevvalidp, nextvalidp)
    Widget w;
    Bool *prevvalidp, *nextvalidp;
{
    FontGridWidget fgw = (FontGridWidget) w;

    XFontStruct *fs = fgw->fontgrid.text_font;
    long minn = (long) ((fs->min_byte1 << 0) | fs->min_char_or_byte2);
    long maxn = (long) ((fs->max_byte1 << 8) | fs->max_char_or_byte2);

    *prevvalidp = ((long)fgw->fontgrid.start_char > minn);
    *nextvalidp = (((long)fgw->fontgrid.start_char +
		    (fgw->fontgrid.cell_cols * fgw->fontgrid.cell_rows))
		   < maxn);
}



/*
 * private routines and methods
 */


static GC get_gc (fgw, fore)
    FontGridWidget fgw;
    Pixel fore;
{
    XtGCMask mask;
    XGCValues gcv;

    mask = (GCForeground | GCBackground | GCFunction | GCFont);
    gcv.foreground = fore;
    gcv.background = fgw->core.background_pixel;
    gcv.function = GXcopy;
    gcv.font = fgw->fontgrid.text_font->fid;
    gcv.cap_style = CapProjecting;
    mask |= GCCapStyle;
    if (fgw->fontgrid.grid_width > 0) {
	mask |= GCLineWidth;
	gcv.line_width = ((fgw->fontgrid.grid_width < 2) ? 0 : 
			  fgw->fontgrid.grid_width);
    }
    return (XtGetGC ((Widget) fgw, mask, &gcv));
}


static void ClassInitialize ()
{
    XtAddConverter (XtRString, XtRLong, XmuCvtStringToLong, NULL, 0);
}


static void Initialize (request, new)
    Widget request, new;
{
    FontGridWidget reqfg = (FontGridWidget) request;
    FontGridWidget newfg = (FontGridWidget) new;
    XFontStruct *fs = newfg->fontgrid.text_font;
    unsigned maxn;

    if (reqfg->fontgrid.cell_cols <= 0)
      newfg->fontgrid.cell_cols = 16;

    if (reqfg->fontgrid.cell_rows <= 0) {
	if (fs && fs->max_byte1 == 0) {
	    newfg->fontgrid.cell_rows = (fs->max_char_or_byte2 / 
					 newfg->fontgrid.cell_cols) + 1;
	    if (newfg->fontgrid.cell_rows > 16)
	      newfg->fontgrid.cell_rows = 16;
	} else
	  newfg->fontgrid.cell_rows = 16;
    }

    if (reqfg->fontgrid.cell_width <= 0)
      newfg->fontgrid.cell_width = (fs ? DefaultCellWidth (newfg) : 1);
    if (reqfg->fontgrid.cell_height <= 0)
      newfg->fontgrid.cell_height = (fs ? DefaultCellHeight (newfg) : 1);

    /* give a nice size that fits one screen full */
    if (newfg->core.width == 0)
      newfg->core.width = (newfg->fontgrid.cell_width *
			   newfg->fontgrid.cell_cols +
			   newfg->fontgrid.grid_width *
			   (newfg->fontgrid.cell_cols + 1));

    if (newfg->core.height == 0) 
      newfg->core.height = (newfg->fontgrid.cell_height * 
			    newfg->fontgrid.cell_rows +
			    newfg->fontgrid.grid_width *
			    (newfg->fontgrid.cell_rows + 1));

    /*
     * select the first character
     */

    if (newfg->fontgrid.start_char == 0xffff) {
	newfg->fontgrid.start_char = (fs ? (unsigned)(fs->min_byte1 << 8) : 0);
    }
    if (fs) {
	maxn = ((fs->max_byte1 << 8) | fs->max_char_or_byte2);
	if (newfg->fontgrid.start_char > maxn) 
	  newfg->fontgrid.start_char = (maxn + 1 - 
					(newfg->fontgrid.cell_cols * 
					 newfg->fontgrid.cell_rows));
    }
}

static void Realize (gw, valueMask, attributes)
    Widget gw;
    Mask *valueMask;
    XSetWindowAttributes *attributes;
{
    FontGridWidget fgw = (FontGridWidget) gw;
    FontGridPart *p = &fgw->fontgrid;

    p->text_gc = get_gc (fgw, p->foreground_pixel);
    p->box_gc = get_gc (fgw, p->box_pixel);
    Resize (gw);

    (*(XtSuperclass(gw)->core_class.realize)) (gw, valueMask, attributes);
    return;
}



static void Destroy (gw)
    Widget gw;
{
    FontGridWidget fgw = (FontGridWidget) gw;

    XtReleaseGC (gw, fgw->fontgrid.text_gc);
    XtReleaseGC (gw, fgw->fontgrid.box_gc);
}


static void Resize (gw)
    Widget gw;
{
    FontGridWidget fgw = (FontGridWidget) gw;

    /* recompute in case we've changed size */
    fgw->fontgrid.cell_width = CellWidth (fgw);
    if (fgw->fontgrid.cell_width <= 0)
	fgw->fontgrid.cell_width = 1;
    fgw->fontgrid.cell_height = CellHeight (fgw);
    if (fgw->fontgrid.cell_height <= 0)
	fgw->fontgrid.cell_height = 1;
    fgw->fontgrid.xoff = (fgw->fontgrid.cell_width -
			    DefaultCellWidth (fgw)) / 2;
    fgw->fontgrid.yoff = (fgw->fontgrid.cell_height -
			    DefaultCellHeight (fgw)) / 2;

}


/* ARGSUSED */
static void Redisplay (gw, event, region)
    Widget gw;
    XEvent *event;
    Region region;
{
    FontGridWidget fgw = (FontGridWidget) gw;
    XRectangle rect;			/* bounding rect for region */
    int left, right, top, bottom;	/* which cells were damaged */
    int cw, ch;				/* cell size */

    if (!fgw->fontgrid.text_font) {
	Bell (gw);
	return;
    }

    /*
     * compute the left, right, top, and bottom cells that were damaged
     */
    XClipBox (region, &rect);
    cw = fgw->fontgrid.cell_width + fgw->fontgrid.grid_width;
    ch = fgw->fontgrid.cell_height + fgw->fontgrid.grid_width;
    if ((left = (((int) rect.x) / cw)) < 0) left = 0;
    right = (((int) (rect.x + rect.width - 1)) / cw);
    if ((top = (((int) rect.y) / ch)) < 0) top = 0;
    bottom = (((int) (rect.y + rect.height - 1)) / ch);

    paint_grid (fgw, left, top, right - left + 1, bottom - top + 1);
}


static void paint_grid (fgw, col, row, ncols, nrows)
    FontGridWidget fgw;			/* widget in which to draw */
    int col, row;			/* where to start */
    int ncols, nrows;			/* number of cells */
{
    FontGridPart *p = &fgw->fontgrid;
    int i, j;
    Display *dpy = XtDisplay(fgw);
    Window wind = XtWindow(fgw);
    int cw = p->cell_width + p->grid_width;
    int ch = p->cell_height + p->grid_width;
    int tcols = p->cell_cols;
    int trows = p->cell_rows;
    int x1, y1, x2, y2, x, y;
    unsigned maxn = ((p->text_font->max_byte1 << 8) |
		     p->text_font->max_char_or_byte2);
    unsigned n, prevn;
    int startx;

    if (col + ncols >= tcols) {
	ncols = tcols - col;
	if (ncols < 1) return;
    }

    if (row + nrows >= trows) {
	nrows = trows - row;
	if (nrows < 1) return;
    }

    /*
     * paint the grid lines for the indicated rows 
     */
    if (p->grid_width > 0) {
	int half_grid_width = p->grid_width >> 1;
	x1 = col * cw + half_grid_width;
	y1 = row * ch + half_grid_width;
	x2 = x1 + ncols * cw;
	y2 = y1 + nrows * ch;
	for (i = 0, x = x1; i <= ncols; i++, x += cw) {
	    XDrawLine (dpy, wind, p->box_gc, x, y1, x, y2);
	}
	for (i = 0, y = y1; i <= nrows; i++, y += ch) {
	    XDrawLine (dpy, wind, p->box_gc, x1, y, x2, y);
	}
    }

	
    /*
     * Draw a character in every box; treat all fonts as if they were 16bit
     * fonts.  Store the high eight bits in byte1 and the low eight bits in 
     * byte2.
     */
    prevn = p->start_char + col + row * tcols;
    startx = col * cw + p->internal_pad + p->grid_width;
    for (j = 0,
	 y = row * ch + p->internal_pad + p->grid_width + p->text_font->ascent;
	 j < nrows; j++, y += ch) {
	n = prevn;
	for (i = 0, x = startx; i < ncols; i++, x += cw) {
	    XChar2b thechar;
	    int xoff = p->xoff, yoff = p->yoff;

	    if (n > maxn) goto done;	/* no break out of nested */

	    thechar.byte1 = (n >> 8);	/* high eight bits */
	    thechar.byte2 = (n & 255);	/* low eight bits */
	    if (p->box_chars || p->center_chars) {
		XCharStruct metrics;
		int direction, fontascent, fontdescent;

		XTextExtents16 (p->text_font, &thechar, 1, &direction,
				&fontascent, &fontdescent, &metrics);

		if (p->center_chars) {
		    /*
		     * We want to move the origin by enough to center the ink
		     * within the cell.  The left edge will then be at 
		     * (cell_width - (rbearing - lbearing)) / 2; so we subtract
		     * the lbearing to find the origin.  Ditto for vertical.
		     */
		    xoff = (((p->cell_width -
			      (metrics.rbearing - metrics.lbearing)) / 2) -
			    p->internal_pad - metrics.lbearing);
		    yoff = (((p->cell_height - 
			      (metrics.descent + metrics.ascent)) / 2) -
			    p->internal_pad -
			    p->text_font->ascent + metrics.ascent);
		}
		if (p->box_chars) {
		    XDrawRectangle (dpy, wind, p->box_gc,
				    x + xoff, y + yoff - p->text_font->ascent, 
				    metrics.width - 1,
				    fontascent + fontdescent - 1);
		}
	    }
	    XDrawString16 (dpy, wind, p->text_gc, x + xoff, y + yoff,
			   &thechar, 1);
	    n++;
	}
	prevn += tcols;
    }

  done:
    return;
}

/*ARGSUSED*/
static Boolean SetValues (current, request, new, args, num_args)
    Widget current, request, new;
    ArgList args;
    Cardinal *num_args;
{
    FontGridWidget curfg = (FontGridWidget) current;
    FontGridWidget newfg = (FontGridWidget) new;
    Boolean redisplay = FALSE;

    if (curfg->fontgrid.text_font != newfg->fontgrid.text_font ||
	curfg->fontgrid.internal_pad != newfg->fontgrid.internal_pad) {
	newfg->fontgrid.cell_width = DefaultCellWidth (newfg);
	newfg->fontgrid.cell_height = DefaultCellHeight (newfg);
	redisplay = TRUE;
    }

    if (curfg->fontgrid.foreground_pixel != newfg->fontgrid.foreground_pixel) {
	XtReleaseGC (new, curfg->fontgrid.text_gc);
	newfg->fontgrid.text_gc = get_gc (newfg,
					  newfg->fontgrid.foreground_pixel);
	redisplay = TRUE;
    }

    if (curfg->fontgrid.box_pixel != newfg->fontgrid.box_pixel) {
	XtReleaseGC (new, curfg->fontgrid.text_gc);
	newfg->fontgrid.box_gc = get_gc (newfg, newfg->fontgrid.box_pixel);
	redisplay = TRUE;
    }

    if (curfg->fontgrid.center_chars != newfg->fontgrid.center_chars ||
	curfg->fontgrid.box_chars != newfg->fontgrid.box_chars)
      redisplay = TRUE;

    if (curfg->fontgrid.start_char != newfg->fontgrid.start_char) {
	XFontStruct *fs = newfg->fontgrid.text_font;
	unsigned maxn = ((fs->max_byte1 << 8) | fs->max_char_or_byte2);

	if (newfg->fontgrid.start_char > maxn) 
	  newfg->fontgrid.start_char = (maxn + 1 - 
					(newfg->fontgrid.cell_cols * 
					 newfg->fontgrid.cell_rows));

	redisplay = (curfg->fontgrid.start_char != newfg->fontgrid.start_char);
    }

    return redisplay;
}


/* ARGSUSED */
static void Notify (gw, event, params, nparams)
    Widget gw;
    XEvent *event;
    String *params;
    Cardinal *nparams;
{
    FontGridWidget fgw = (FontGridWidget) gw;
    int x, y;				/* where the event happened */
    FontGridCharRec rec;		/* callback data */

    /*
     * only allow events with (x,y)
     */
    switch (event->type) {
      case KeyPress:
      case KeyRelease:
	x = event->xkey.x;
	y = event->xkey.y;
	break;
      case ButtonPress:
      case ButtonRelease:
	x = event->xbutton.x;
	y = event->xbutton.y;
	break;
      case MotionNotify:
	x = event->xmotion.x;
	y = event->xmotion.y;
	break;
      default:
	Bell (gw);
	return;
    }

    /*
     * compute the callback data
     */
    {
	int cw = fgw->fontgrid.cell_width, ch = fgw->fontgrid.cell_height;
	unsigned n;

	if (x > (fgw->fontgrid.cell_cols * cw)) {
	    Bell (gw);
	    return;
	}

	n= (fgw->fontgrid.start_char + 
	    ((y / ch) * fgw->fontgrid.cell_cols) + (x / cw));

	rec.thefont = fgw->fontgrid.text_font;
	rec.thechar.byte1 = (n >> 8);
	rec.thechar.byte2 = (n & 255);
    }

    XtCallCallbacks (gw, XtNcallback, (XtPointer) &rec);
}

