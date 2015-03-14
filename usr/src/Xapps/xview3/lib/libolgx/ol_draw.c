#ident "@(#)ol_draw.c	1.25 91/07/03 SMI"

/*
 * Copyright 1990 Sun Microsystems
 */

/*
 * OPEN LOOK object drawing package Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <X11/Xlib.h>

#include "olgx_impl.h"

int
calc_add_ins(width, add_ins)
    int             width;
    short           add_ins[STRING_SIZE];
{
    register int    this_log2 = 4;
    register int    this_bit;
    int             nchars = 0;

    this_bit = 1 << this_log2;

    for (this_bit = 1 << this_log2;
	 this_log2 >= 0 && width && nchars < STRING_SIZE;
	 this_bit = this_bit >> 1, this_log2--) {

	while (width >= this_bit) {
	    width -= this_bit;
	    add_ins[nchars++] = this_log2;

	}

    }
    return (nchars);
}

void
olgx_draw_box(info, win, x, y, width, height, state, fill_in)
    Graphics_info  *info;
    Window          win;
    int             x, y, width, height, state;
    Boolean         fill_in;

{


    width -= 1;			/* This takes care of the fact , that the
				 * width passed is the including the endlines
				 * of the box */
    height -= 1;



    if (info->three_d) {

	XPoint          point[5];

	if (fill_in && width > 1 && height > 1) {

	    XFillRectangle(info->dpy, win,
		       (state & OLGX_INVOKED) ? info->gc_rec[OLGX_BG2]->gc :
			   info->gc_rec[OLGX_BG1]->gc,
			   x + 1, y + 1, width - 1, height - 1);

	}
	point[0].x = x;
	point[1].x = x;
	point[1].y = y;
	point[2].y = y;
	point[0].y = y + height;
	point[2].x = x + width;

	XDrawLines(info->dpy, win,
		   (state & OLGX_INVOKED) ? info->gc_rec[OLGX_BG3]->gc :
		   info->gc_rec[OLGX_WHITE]->gc, point, 3, CoordModeOrigin);

	point[0].y = y + height;
	point[1].x = x + width;
	point[1].y = y + height;
	point[2].x = x + width;
	point[0].x = x + 1;
	point[2].y = y;
	if (info->three_d == OLGX_3D_MONO) {

	    /* Add the extra line needed for monochrome 3D */
	    /* Tricky drawing , to get everything on one  */
	    /* sever request , we use something like _||  */
	    /* to achieve double width line for mono3D    */

	    point[3].x = x + width - 1;
	    point[3].y = y;
	    point[4].x = x + width - 1;
	    point[4].y = y + height - 1;
	    XDrawLines(info->dpy, win,
		     (state & OLGX_INVOKED) ? info->gc_rec[OLGX_WHITE]->gc :
		       info->gc_rec[OLGX_BG3]->gc,
		       point, 5, CoordModeOrigin);

	} else {

	    XDrawLines(info->dpy, win,
		     (state & OLGX_INVOKED) ? info->gc_rec[OLGX_WHITE]->gc :
		       info->gc_rec[OLGX_BG3]->gc,
		       point, 3, CoordModeOrigin);

	}


    } else {

	/* 2d */

	if (state & OLGX_ERASE)
	   XFillRectangle(info->dpy, win, info->three_d ?
                                            info->gc_rec[OLGX_BG1]->gc :
		                            info->gc_rec[OLGX_WHITE]->gc,
                          x, y, width + 1, height + 1);

	if (state & OLGX_INVOKED) {
	    if (fill_in)
		XFillRectangle(info->dpy, win, info->gc_rec[OLGX_BLACK]->gc, x, y,
                               width + 1, height + 1);

	    else {

		/* Draw the special invoked state */

		XRectangle      rect[3];

		rect[0].x = x;
		rect[0].y = y;
		rect[0].width = width;
		rect[0].height = height;
		rect[1].x = x + 1;
		rect[1].y = y + 1;
		rect[1].width = width - 2;
		rect[1].height = height - 2;
		XDrawRectangles(info->dpy,win, info->gc_rec[OLGX_BLACK]->gc, rect, 2);

	    }

	} else
	    XDrawRectangle(info->dpy, win, info->gc_rec[OLGX_BLACK]->gc, x, y,
                           width, height);

    }
    if (state & OLGX_INACTIVE) {

	/*
	 * Inactive State grey out the entire thing
	 */

	olgx_stipple_rect(info, win, x, y, width, height);

    }
    
}



olgx_draw_choice_item(info, win, x, y, width, height, label, state)
    Graphics_info  *info;
    Window          win;
    void           *label;
    int             x, y, width, height, state;
{
    int             def_decr;
    int             centerx = 0;
    int             centery = 0;


    /*
     * Special inactive case, so pass invoked state to the draw box routine
     * and swish that out later
     */

    if (!(info->three_d) && (state & OLGX_INACTIVE))
	state |= OLGX_INVOKED;

    /* draw a box (3d or 2d) outline, filling it in only if invoked */

    olgx_draw_box(info, win, x, y, width, height,
		  state, (info->three_d) ? 1 : 0);

    if (!(info->three_d) && (state & OLGX_INACTIVE))
	state ^= OLGX_INVOKED;

    /* the default ring rectangle looks better with width-5 for 3d */

    def_decr = info->three_d ? 6 : 5;

    if (state & OLGX_DEFAULT) {

	/* draw an inner box for a default setting */

	XDrawRectangle(info->dpy, win, (info->three_d) ?
                                         info->gc_rec[OLGX_BG3]->gc :
		                         info->gc_rec[OLGX_BLACK]->gc,
                       x + 2, y + 2, (width - def_decr), (height - def_decr));
    }
    /*
     * Now place the label
     */


    if (label) {

	if (state & OLGX_LABEL_IS_PIXMAP) {

	    centerx = ((width - ((Pixlabel *) label)->width) >> 1);
	    centery = ((height - ((Pixlabel *) label)->height) >> 1);
	    olgx_draw_pixmap_label(info, win,
				   ((Pixlabel *) label)->pixmap,
				   x + ((centerx > 0) ? centerx : 0),
				   y + ((centery > 0) ? centery : 0),
				   ((Pixlabel *) label)->width,
				   ((Pixlabel *) label)->height,
				   state);

	} else {

	    int             flag = 0;

	    /*
	     * special case for choice invoked in drawing label where the
	     * invoked state is changed to uninvoked and sent to the label
	     * drawing routines
	     */

	    if (state & OLGX_INVOKED) {

		state ^= OLGX_INVOKED;
		flag = 1;

	    }
	    olgx_draw_text(info, win,
#ifdef OW_I18N
			   (wchar_t *) label,
#else
			   (char *) label,
#endif
	    /*
	     * a small hack to make sure , that the between the left side of
	     * the choice item and the text is okay under 14pt and 19pt
	     * size.. we are using the same info->base_off value
	     */
			   x + ((info->button_height > 20) ?
				info->base_off + 2 : info->base_off),
			   y + height - info->base_off,
			   width - ((info->button_height > 20) ?
				    info->base_off + 2 : info->base_off),
			   state);

	    /* reset to invoked state */

	    if (flag)
		state = state | OLGX_INVOKED;

	}

    }
    if (state & OLGX_INACTIVE) {

	/*
	 * Inactive State grey out the entire thing
	 */

	olgx_stipple_rect(info, win, x, y, width, height);

    }
}
