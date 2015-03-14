#ident "@(#)ol_button.c	1.40 91/09/06 SMI"

/*
 * Copyright 1989-1990 Sun Microsystems
 */

/*
 * OPEN LOOK object drawing package Sun Microsystems, Inc.
 * 
 * OLGX_button.c Menu button module
 */

#include <stdio.h>
#include <X11/Xlib.h>
#include "olgx_impl.h"

/*
 * Private function declarations
 */

void            olgx_set_busy_stipple();
void            olgx_draw_pixmap_label();

void
olgx_draw_button(info, win, x, y, width, height, label, state)
    Graphics_info  *info;
    Window          win;
    int             x, y, width, height;
    void           *label;
    int             state;
{
    XTextItem       item;
    char            string[STRING_SIZE];
    short           add_ins[STRING_SIZE];
    register int    i;
    int             num_add;
    int             inside_width;	/* width minus endcaps */
    int             top_color, bottom_color, fill_color;


    inside_width = width - (2 * info->endcap_width);
    num_add = calc_add_ins(inside_width - 1, add_ins);
    item.nchars = 2 + num_add;
    item.font = None;
    item.chars = string;
    item.delta = 0;


    if (height)
	/* variable height button-- possibly a pixmap label */

	olgx_draw_varheight_button(info, win, x, y, width, height, state);

    else {

	if (info->three_d) {

	    /*
	     * 3d determine what colors we should draw in
	     */

	    if (state & OLGX_INVOKED) {	/* invoked button */
		top_color = OLGX_BG3;
		bottom_color = OLGX_WHITE;
		fill_color = OLGX_BG2;

	    } else if ((state & OLGX_DEFAULT) && (state & OLGX_MENU_ITEM)) {

              	/* default menu item */
		top_color = bottom_color = OLGX_BG3;
		fill_color = OLGX_BG1;

	    } else if (state & OLGX_MENU_ITEM && state & OLGX_BUSY) {
              	/* busy menu item */

		fill_color = top_color = bottom_color = OLGX_BG1;

	    } else if (state & OLGX_MENU_ITEM) {	/* normal menu item */

		fill_color = top_color = bottom_color = NONE;

	    } else {		/* normal panel button */

		top_color = OLGX_WHITE;
		bottom_color = OLGX_BG3;
		fill_color = OLGX_BG1;

	    }

	    if (state & OLGX_BUSY) {

		/*
		 * This routine changes GC information on-the-fly, but it is
		 * assumed that OLGX_BUSY won't be called often, so it makes
		 * sense to use the same GC rather than one for ` each color.
		 */

		if (!info->gc_rec[OLGX_BUSYGC])
		    olgx_initialise_gcrec(info, OLGX_BUSYGC);
		fill_color = OLGX_BUSYGC;

	    }
	    /* only check erase on transparent items */

	    if (fill_color == NONE) {

		if (state & OLGX_ERASE) {

		    /*
		     * to improve performance, we erase a rectangle the size
		     * of a button rather than drawing a real button.
		     */

		   XFillRectangle(info->dpy, win, info->gc_rec[OLGX_BG1]->gc, 
                                  x, y, width, Button_Height(info));
		}
	    } else {		/* if not transparent, actually draw the
				 * button */

		if (top_color != NONE) {

		    /* draw the top part of the button */

		    string[0] = BUTTON_UL;
		    VARIABLE_LENGTH_MACRO(1, BUTTON_TOP_1);
		    string[i + 1] = BUTTON_UR;
		    XDrawText(info->dpy, win,
			      info->gc_rec[top_color]->gc, x, y, &item, 1);

		}
		if (bottom_color != NONE) {

		    /* draw the bottom part of the button */

		    string[0] = BUTTON_LL;
		    VARIABLE_LENGTH_MACRO(1, BUTTON_BOTTOM_1);
		    string[i + 1] = BUTTON_LR;
		    XDrawText(info->dpy, win,
			    info->gc_rec[bottom_color]->gc, x, y, &item, 1);

		}
		/* Fill in the button */

		string[0] = BUTTON_LEFT_ENDCAP_FILL;
		VARIABLE_LENGTH_MACRO(1, BUTTON_FILL_1);
		string[i + 1] = BUTTON_RIGHT_ENDCAP_FILL;
		XDrawText(info->dpy, win,
			  info->gc_rec[fill_color]->gc, x, y, &item, 1);

		/* draw the inner border of a default button (not menu item) */

		if (!(state & OLGX_MENU_ITEM) && (state & OLGX_DEFAULT)) {
		    string[0] = DFLT_BUTTON_LEFT_ENDCAP;
		    VARIABLE_LENGTH_MACRO(1, DFLT_BUTTON_MIDDLE_1);
		    string[i + 1] = DFLT_BUTTON_RIGHT_ENDCAP;
		    XDrawText(info->dpy, win,
			      info->gc_rec[OLGX_BG3]->gc, x, y, &item, 1);
		}
	    }			/* Not transparent */

	}
	/* End 3D */
	else {			/* draw 2d button */

	    if (state & OLGX_ERASE)
		XFillRectangle(info->dpy, win, info->gc_rec[OLGX_WHITE]->gc, x, y,
			       width + 1, Button_Height(info));


	    if ((state & OLGX_INVOKED)) {
		string[0] = BUTTON_FILL_2D_LEFTENDCAP;
		VARIABLE_LENGTH_MACRO(1, BUTTON_FILL_2D_MIDDLE_1);
		string[i + 1] = BUTTON_FILL_2D_RIGHTENDCAP;
		XDrawText(info->dpy, win,
			  info->gc_rec[OLGX_BLACK]->gc, x, y, &item, 1);

	    } else if (state & OLGX_BUSY) {

		if (!info->gc_rec[OLGX_BUSYGC])
		    olgx_initialise_gcrec(info, OLGX_BUSYGC);
		string[0] = BUTTON_FILL_2D_LEFTENDCAP;
		VARIABLE_LENGTH_MACRO(1, BUTTON_FILL_2D_MIDDLE_1);
		string[i + 1] = BUTTON_FILL_2D_RIGHTENDCAP;
		XDrawText(info->dpy, win,
			  info->gc_rec[OLGX_BUSYGC]->gc, x, y, &item, 1);

	    } else if (!(state & OLGX_MENU_ITEM) && (state & OLGX_DEFAULT)) {

		/* draw the 2d default ring if not menu-item */

		string[0] = DFLT_BUTTON_LEFT_ENDCAP;
		VARIABLE_LENGTH_MACRO(1, DFLT_BUTTON_MIDDLE_1);
		string[i + 1] = DFLT_BUTTON_RIGHT_ENDCAP;
		XDrawText(info->dpy, win,
			  info->gc_rec[OLGX_BLACK]->gc, x, y, &item, 1);

	    } else if (state & OLGX_DEFAULT) {

		/* draw the 2d default ring for menu item */

		string[0] = MENU_DFLT_OUTLINE_LEFT_ENDCAP;
		VARIABLE_LENGTH_MACRO(1, MENU_DFLT_OUTLINE_MIDDLE_1);
		string[i + 1] = MENU_DFLT_OUTLINE_RIGHT_ENDCAP;
		XDrawText(info->dpy, win,
			  info->gc_rec[OLGX_BLACK]->gc, x, y, &item, 1);

	    }
	    /* draw the button if it is not a menu item */

	    if (!(state & OLGX_MENU_ITEM)) {
		string[0] = BUTTON_OUTLINE_LEFT_ENDCAP;
		VARIABLE_LENGTH_MACRO(1, BUTTON_OUTLINE_MIDDLE_1);
		string[i + 1] = BUTTON_OUTLINE_RIGHT_ENDCAP;
		XDrawText(info->dpy, win,
			  info->gc_rec[OLGX_BLACK]->gc, x, y, &item, 1);
	    }
	}
    }

    /*
     * Place the label, if specified.
     */

    if (label) {

	if (state & OLGX_LABEL_IS_PIXMAP) {

	    int             centerx, centery;

	    centerx = (width - ((Pixlabel *) label)->width >> 1);
	    centery = (height - ((Pixlabel *) label)->height >> 1);
	    olgx_draw_pixmap_label(info, win,
				   ((Pixlabel *) label)->pixmap,
				   x + ((centerx > 0) ? centerx : 0),
				   y + ((centery > 0) ? centery : 0),
				   ((Pixlabel *) label)->width,
				  (height) ? height : Button_Height(info) - 2, state);
	} else {

#ifdef OW_I18N
	    /*
	     * FIX_ME: Binary compat. Need some flag to tell char or
	     * wchar_t.
	     */
	    olgx_draw_text(info, win, (wchar_t *) label,
#else
	    olgx_draw_text(info, win, (char *) label,
#endif /* OW_I18N */
			   x + info->endcap_width,
			   y + info->button_height - info->base_off,
			   inside_width -
			   ((state & OLGX_MENU_MARK) ?
			    info->mm_width : 0),
			   state);
	}
    }
    /*
     * Place the menu mark, if desired.
     */

    if (state & OLGX_MENU_MARK) {

	/*
	 * draw the menu mark. (fill_color != OLGX_BG2) causes the menu mark
	 * to be filled in only when necessary
	 */

	if (info->three_d)
	    olgx_draw_menu_mark(info, win,
			  x + (width - info->endcap_width - info->mm_width),
				y + (info->button_height - info->mm_height -
				     info->base_off),
				state, (fill_color != OLGX_BG2));
	else
	    olgx_draw_menu_mark(info, win,
			  x + (width - info->endcap_width - info->mm_width),
				y + (info->button_height - info->mm_height -
				     info->base_off),
				state, 0);
    }
    /*
     * Mark the item as inactive, if specified
     */

    if (state & OLGX_INACTIVE) {
	olgx_stipple_rect(info, win, x, y, width, 
                          (height) ? height + 8 : Button_Height(info));

    }
}


/*
 * Draw the outline of a variable height button Private Routine
 */

void
olgx_draw_varheight_button(info, win, x, y, width, height, state)
    Graphics_info  *info;
    Window          win;
    int             x, y, width, height;
    int             state;

{


    char            string[2];
    XSegment        seg[4];


    if (info->three_d) {

	/* 3D */
	/* Draw all the four corners */

	if (state & OLGX_INVOKED)
	    XFillRectangle(info->dpy, win, info->gc_rec[OLGX_BG2]->gc, x + 1,
			   y + 1, width - 2, height - 2);
	else
	    XFillRectangle(info->dpy, win, info->gc_rec[OLGX_BG1]->gc, x + 1,
			   y + 1, width - 2, height - 2);

	string[0] = PIXLABEL_BUTTON_UL;
	XDrawString(info->dpy, win, (state & OLGX_INVOKED) ?
	    info->gc_rec[OLGX_BG3]->gc : info->gc_rec[OLGX_WHITE]->gc, x, y,
		    string, 1);

	string[0] = PIXLABEL_BUTTON_UR;
	XDrawString(info->dpy, win, (state & OLGX_INVOKED) ?
		  info->gc_rec[OLGX_BG3]->gc : info->gc_rec[OLGX_WHITE]->gc,
		    x + width - VARHEIGHT_BUTTON_CORNER_DIMEN, y, string, 1);

	string[0] = PIXLABEL_BUTTON_LL;
	XDrawString(info->dpy, win, (state & OLGX_INVOKED) ?
	   info->gc_rec[OLGX_WHITE]->gc : info->gc_rec[OLGX_BG3]->gc, x, y +
		    height - VARHEIGHT_BUTTON_CORNER_DIMEN, string, 1);

	string[0] = PIXLABEL_BUTTON_LR;
	XDrawString(info->dpy, win, (state & OLGX_INVOKED) ?
		  info->gc_rec[OLGX_WHITE]->gc : info->gc_rec[OLGX_BG3]->gc,
		    x + width - VARHEIGHT_BUTTON_CORNER_DIMEN,
		    y + height - VARHEIGHT_BUTTON_CORNER_DIMEN, string, 1);

	seg[0].x1 = x + VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[0].y1 = seg[0].y2 = y;
	seg[0].x2 = x + width - VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[1].x1 = seg[1].x2 = x;
	seg[1].y1 = y + VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[1].y2 = y + height - VARHEIGHT_BUTTON_CORNER_DIMEN;
	XDrawSegments(info->dpy, win, (state & OLGX_INVOKED) ? 
                                        info->gc_rec[OLGX_BG3]->gc :
		                        info->gc_rec[OLGX_WHITE]->gc,
                     seg, 2);
	seg[0].x1 = x + VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[0].y1 = seg[0].y2 = y + height - 1;
	seg[0].x2 = x + width - VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[1].x1 = seg[1].x2 = x + width - 1;
	seg[1].y1 = y + VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[1].y2 = y + height - VARHEIGHT_BUTTON_CORNER_DIMEN;
	XDrawSegments(info->dpy, win, (state & OLGX_INVOKED) ? 
                                        info->gc_rec[OLGX_WHITE]->gc :
		                        info->gc_rec[OLGX_BG3]->gc,
                      seg, 2);


    } else {

	/* 2D */

	if (state & OLGX_INVOKED)
	    XFillRectangle(info->dpy, win, info->gc_rec[OLGX_BLACK]->gc, x + 3,
			   y + 3, width - 6, height - 6);
	else
	    XFillRectangle(info->dpy, win, info->gc_rec[OLGX_WHITE]->gc, x + 1,
			   y + 1, width - 2, height - 2);

	string[0] = PIXLABEL_BUTTON_UL;
	XDrawString(info->dpy, win, info->gc_rec[OLGX_BLACK]->gc, x, y, string, 1);
	string[0] = PIXLABEL_BUTTON_UR;
	XDrawString(info->dpy, win, info->gc_rec[OLGX_BLACK]->gc,
		    x + width - VARHEIGHT_BUTTON_CORNER_DIMEN, y, string, 1);

	string[0] = PIXLABEL_BUTTON_LL;
	XDrawString(info->dpy, win, info->gc_rec[OLGX_BLACK]->gc, x,
		    y + height - VARHEIGHT_BUTTON_CORNER_DIMEN, string, 1);

	string[0] = PIXLABEL_BUTTON_LR;
	XDrawString(info->dpy, win, info->gc_rec[OLGX_BLACK]->gc,
		    x + width - VARHEIGHT_BUTTON_CORNER_DIMEN,
		    y + height - VARHEIGHT_BUTTON_CORNER_DIMEN, string, 1);

	seg[0].x1 = x + VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[0].y1 = seg[0].y2 = y;
	seg[0].x2 = x + width - VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[1].x1 = seg[1].x2 = x;
	seg[1].y1 = y + VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[1].y2 = y + height - VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[2].x1 = x + VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[2].y1 = seg[2].y2 = y + height - 1;
	seg[2].x2 = x + width - VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[3].x1 = seg[3].x2 = x + width - 1;
	seg[3].y1 = y + VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[3].y2 = y + height - VARHEIGHT_BUTTON_CORNER_DIMEN;
	XDrawSegments(info->dpy, win, info->gc_rec[OLGX_BLACK]->gc, seg, 4);

    }

    /*
     * REMIND: the code below probably uses OLGX_BLACK incorrectly.  It should 
     * be changed to use OLGX_BG3 in 3D mode as appropriate.
     */

    if (state & OLGX_DEFAULT) {

	string[0] = PIXLABEL_DEF_BUTTON_UL;
	XDrawString(info->dpy, win, info->gc_rec[OLGX_BLACK]->gc, x, y,
		    string, 1);

	string[0] = PIXLABEL_DEF_BUTTON_UR;
	XDrawString(info->dpy, win, info->gc_rec[OLGX_BLACK]->gc, x + width -
		    VARHEIGHT_BUTTON_CORNER_DIMEN, y, string, 1);

	string[0] = PIXLABEL_DEF_BUTTON_LL;
	XDrawString(info->dpy, win,
		    info->gc_rec[OLGX_BLACK]->gc, x, y + height -
		    VARHEIGHT_BUTTON_CORNER_DIMEN, string, 1);

	string[0] = PIXLABEL_DEF_BUTTON_LR;
	XDrawString(info->dpy, win,
		    info->gc_rec[OLGX_BLACK]->gc, x + width -
		    VARHEIGHT_BUTTON_CORNER_DIMEN, y + height -
		    VARHEIGHT_BUTTON_CORNER_DIMEN, string, 1);

	seg[0].x1 = x + VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[0].y1 = seg[0].y2 = y + 2;
	seg[0].x2 = x + width - VARHEIGHT_BUTTON_CORNER_DIMEN - 1;
	seg[1].x1 = seg[1].x2 = x + 2;
	seg[1].y1 = y + VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[1].y2 = y + height - VARHEIGHT_BUTTON_CORNER_DIMEN - 1;
	seg[2].x1 = x + VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[2].y1 = seg[2].y2 = y + height - 1 - 2;
	seg[2].x2 = x + width - VARHEIGHT_BUTTON_CORNER_DIMEN - 1;
	seg[3].x1 = seg[3].x2 = x + width - 1 - 2;
	seg[3].y1 = y + VARHEIGHT_BUTTON_CORNER_DIMEN;
	seg[3].y2 = y + height - VARHEIGHT_BUTTON_CORNER_DIMEN - 1;
	XDrawSegments(info->dpy, win,
		      info->gc_rec[OLGX_BLACK]->gc, seg, 4);

    }
    if (state & OLGX_BUSY) {

	if (!info->gc_rec[OLGX_BUSYGC])
	    olgx_initialise_gcrec(info, OLGX_BUSYGC);

	XFillRectangle(info->dpy, win, info->gc_rec[OLGX_BUSYGC]->gc, x + 2,
		       y + 2, width - 4, height - 4);
    }
}


void
olgx_draw_menu_mark(info, win, x, y, state, fill_in)
    Graphics_info  *info;
    Window          win;
    int             state, fill_in;
{
    char            string[3];

    if (state & OLGX_VERT_MENU_MARK)
	string[0] = VERT_MENU_MARK_UL;
    else if (state & OLGX_HORIZ_MENU_MARK)
	string[0] = HORIZ_MENU_MARK_UL;
    else if (state & OLGX_HORIZ_BACK_MENU_MARK)
	string[0] = HORIZ_BACK_MENU_MARK_UL;
    else if (state & OLGX_VERT_BACK_MENU_MARK)
	string[0] = VERT_BACK_MENU_MARK_UL;

    string[1] = string[0] + 1;

    if ((state & OLGX_INVOKED) && (!info->three_d))
	XDrawString(info->dpy, win, info->gc_rec[OLGX_WHITE]->gc, x, y, &string[0],
		    info->three_d ? 1 : 2);
    else
	XDrawString(info->dpy, win, info->three_d ? info->gc_rec[OLGX_BG3]->gc :
					            info->gc_rec[OLGX_BLACK]->gc,
                    x, y, &string[0], info->three_d ? 1 : 2);

    if (info->three_d)
       XDrawString(info->dpy, win, info->gc_rec[OLGX_WHITE]->gc, x, y, &string[1], 1);

    /* fill in the menu mark, if requested */

    if (fill_in) {
	string[0] = string[0] + 2;
	XDrawString(info->dpy, win, (info->three_d)?info->gc_rec[OLGX_BG2]->gc :
		                                    info->gc_rec[OLGX_BLACK]->gc,
		    x, y, &string[0], 1);

    }
}




void
olgx_draw_abbrev_button(info, win, x, y, state)
    Graphics_info  *info;
    Window          win;
    int             x, y;
    int             state;
{
    XTextItem       item;
    char            string[3];
    int             top_color, bottom_color, fill_color;

    item.nchars = 1;
    item.font = None;
    item.chars = string;
    item.delta = 0;


    if (!info->three_d) {	/* 2d */

	if (state & OLGX_ERASE)
	    XFillRectangle(info->dpy, win, info->gc_rec[OLGX_WHITE]->gc, x, y,
	      Abbrev_MenuButton_Width(info), Abbrev_MenuButton_Width(info));

	if (state & OLGX_BUSY) {

	    if (!info->gc_rec[OLGX_BUSYGC])
		olgx_initialise_gcrec(info, OLGX_BUSYGC);
	    string[0] = ABBREV_MENU_FILL;
	    XDrawText(info->dpy, win, info->gc_rec[OLGX_BUSYGC]->gc, x, y, &item, 1);
	}
	if (state & OLGX_INVOKED) {

	    XFillRectangle(info->dpy, win, info->gc_rec[OLGX_WHITE]->gc, x + 2, y + 2,
			   (Abbrev_MenuButton_Width(info) - 4), 
                           (Abbrev_MenuButton_Width(info) - 4));
	    string[0] = OLG_ABBREV_MENU_BUTTON_INVERTED;
	    XDrawText(info->dpy, win, info->gc_rec[OLGX_BLACK]->gc, x, y, &item, 1);

	} else {

	    string[0] = OLG_ABBREV_MENU_BUTTON;
	    XDrawText(info->dpy, win, info->gc_rec[OLGX_BLACK]->gc, x, y, &item, 1);

	}

    } else {			/* 3d */

	if (state & OLGX_INVOKED) {

	    top_color = OLGX_BG3;
	    bottom_color = OLGX_WHITE;
	    fill_color = OLGX_BG2;

	} else {

	    top_color = OLGX_WHITE;
	    bottom_color = OLGX_BG3;
	    fill_color = OLGX_BG1;

	}

	if (state & OLGX_BUSY) {

	    if (!info->gc_rec[OLGX_BUSYGC])
		olgx_initialise_gcrec(info, OLGX_BUSYGC);
	    fill_color = OLGX_BUSYGC;

	}
	string[0] = ABBREV_MENU_UL;
	XDrawText(info->dpy, win, info->gc_rec[top_color]->gc, x, y, &item, 1);

	string[0] = ABBREV_MENU_LR;
	XDrawText(info->dpy, win, info->gc_rec[bottom_color]->gc, x, y, &item, 1);

	string[0] = ABBREV_MENU_FILL;
	XDrawText(info->dpy, win, info->gc_rec[fill_color]->gc, x, y, &item, 1);
	olgx_draw_menu_mark(info, win, x + ((Abbrev_MenuButton_Width(info)
					     - info->mm_width) >> 1),
			    y + ((1 + Abbrev_MenuButton_Height(info) -
				  info->mm_height) >> 1),
			    OLGX_VERT_MENU_MARK, 1);

    }

    /* If it is inactive fill the rectangle with inactive pixmap */

    if (state & OLGX_INACTIVE) {

	olgx_stipple_rect(info, win, x, y, Abbrev_MenuButton_Width(info),
			  Abbrev_MenuButton_Height(info));
    }
}


void
olgx_stipple_rect(info, win, x, y, width, height)
    Graphics_info  *info;
    Window          win;
    int             x, y, width, height;
{


    if (!info->gc_rec[OLGX_GREY_OUT])
	olgx_initialise_gcrec(info, OLGX_GREY_OUT);

    XFillRectangle(info->dpy, win, info->gc_rec[OLGX_GREY_OUT]->gc,
		   x, y, width, height);
}

void
olgx_draw_text(info, win, string, x, y, max_width, state)
    Graphics_info  *info;
    Window          win;
#ifdef OW_I18N
    wchar_t        *string;
#else
    char           *string;
#endif /* OW_I18N */
    int             x, y, max_width;
    int             state;
{

#ifdef OW_I18N
    int             len = wslen(string);
#else
    int             len = strlen(string);
#endif /* OW_I18N */
    register int    i;
    short           more_flag = 0;


    /*
     * if the string is too long, we'll have to truncate it max_width == 0
     * implies don't truncate.
     */

#ifdef OW_I18N
    if (max_width && XwcTextEscapement(info->textfontset,
    					string, len) > max_width) {
#else
    if (max_width && XTextWidth(info->textfont, string, len) > max_width) {
#endif /* OW_I18N */
       int             current_width = 0;		
       for (i = 0; (i < len && current_width <= max_width); i++) {
#ifdef OW_I18N
	   current_width +=
		XwcTextEscapement(info->textfontset, &string[i], 1);
#else
	       current_width += XTextWidth(info->textfont, &string[i], 1);
#endif /* OW_I18N */
	}	

	/*
	 * at this point, i-1 represents the number of chars of string that
	 * will fit into max_width.
	 */

	len = i - 2;
	if (state & OLGX_MORE_ARROW) 
           more_flag = 1;
    }	
    if (!info->gc_rec[OLGX_TEXTGC])
	olgx_initialise_gcrec(info, OLGX_TEXTGC);
    if (!info->three_d)
      if (!info->gc_rec[OLGX_TEXTGC_REV])
	    olgx_initialise_gcrec(info, OLGX_TEXTGC_REV);

    if ((state & OLGX_INVOKED) && !(info->three_d))
#ifdef OW_I18N
      XwcDrawString(info->dpy, win, info->textfontset,
      		    info->gc_rec[OLGX_TEXTGC_REV]->gc, x, y, string, len);
    else
      XwcDrawString(info->dpy, win, info->textfontset,
      		    info->gc_rec[OLGX_TEXTGC]->gc, x, y, string, len);
#else OW_I18N
      XDrawString(info->dpy, win, info->gc_rec[OLGX_TEXTGC_REV]->gc, x, y, 
                  string, len);
    else
      XDrawString(info->dpy, win, info->gc_rec[OLGX_TEXTGC]->gc, x, y, string, len);
#endif /* OW_I18N */

    if (more_flag) /* render a more arrow at the end of the string */ 
#ifdef OW_I18N
       olgx_draw_menu_mark(info,win,
       		x+XwcTextEscapement(info->textfontset,string,len)+1,
#else
       olgx_draw_menu_mark(info,win,x+XTextWidth(info->textfont,string,len)+1 ,
#endif /* OW_I18N */
			   y-MenuMark_Height(info),OLGX_HORIZ_MENU_MARK,1);
}

void
olgx_draw_pixmap_label(info, win, pix, x, y, width, height, state)
    Graphics_info  *info;
    Window          win;
    Pixmap          pix;
    int             x, y, width, height, state;
{


    unsigned long       savebg1;
    unsigned long       savebg2;
    Window              root;
    int                 x_dummy,y_dummy;
    unsigned int        w_dummy,h_dummy,bw_dummy;
    unsigned int        depth;
    	

    if (!info->gc_rec[OLGX_TEXTGC])
	olgx_initialise_gcrec(info, OLGX_TEXTGC);
    if (!info->three_d)
      if (!info->gc_rec[OLGX_TEXTGC_REV])
	    olgx_initialise_gcrec(info, OLGX_TEXTGC_REV);

    if ((state & OLGX_INVOKED) && (info->three_d)) {

	/*
	 * reset the value of the textgc background from bg1 to bg2 in
	 * invoked mode to get the transparent pixmap effect
	 */

	savebg1 = olgx_get_single_color(info, OLGX_BG1);
	savebg2 = olgx_get_single_color(info, OLGX_BG2);
	olgx_set_single_color(info, OLGX_BG1, savebg2, OLGX_SPECIAL);

    }

    /*
     * Performance Problem - RoundTrip request
     * Depth should be passed as part of Pixlabel struct
     */

    XGetGeometry(info->dpy,pix,&root,&x_dummy,&y_dummy,&w_dummy,
                 &h_dummy,&bw_dummy,&depth);
    if (depth > 1) 
        XCopyArea(info->dpy,
       	          pix,		/* src */
	          win,		/* dest */
	          info->gc_rec[OLGX_TEXTGC]->gc,
	          0, 0,		/* src x,y */
	          width, height,
	          x, y);
    else 
        XCopyPlane(info->dpy,
       	          pix,		/* src */
	          win,		/* dest */
	          info->gc_rec[OLGX_TEXTGC]->gc,
	          0, 0,		/* src x,y */
	          width, height,
	          x, y,
                  (unsigned long) 1);
       

    /* Restore the original colors to the textgc  */

    if ((state & OLGX_INVOKED) && (info->three_d))
	olgx_set_single_color(info, OLGX_BG1, savebg1, OLGX_SPECIAL);

}



void
olgx_draw_textscroll_button(info, win, x, y, state)
    Graphics_info  *info;
    Window          win;
    int             x, y;
    int             state;

{
    char            string[2];
    int             width, height;
    int             arr_x, arr_y;

    /*
     * A small hack to calculate the width, arrow postiton..etc since this
     * routine is expected to tbe used infrequently it is not included as
     * part of the info struct and the follwoing calculations take place each
     * time-- a penalty affordable at the cost of infrequency
     * 
     */

    if ((Abbrev_MenuButton_Height(info)) < 20) {

	width = height = Abbrev_MenuButton_Height(info);

	arr_y = 3;
	arr_x = (width / 3) - 1;

    } else {

	width = height = 25;	/* Special case size-19 */
	arr_y = 5;
	arr_x = 7;

    }


    if (!(info->three_d)) {	/* Start 2-D */

	if (state & OLGX_ERASE)
	    XFillRectangle(info->dpy, win, info->gc_rec[OLGX_WHITE]->gc, x, y, width,
                           height);

	if (state & OLGX_SCROLL_FORWARD) {

	    if (state & OLGX_INVOKED)
		string[0] = TEXTSCROLLBUTTON_RIGHT_INV;
	    else
		string[0] = TEXTSCROLLBUTTON_RIGHT;

	    XDrawString(info->dpy, win, info->gc_rec[OLGX_BLACK]->gc, x, y, string,1);

	} else if (state & OLGX_SCROLL_BACKWARD) {

	    if (state & OLGX_INVOKED)
		string[0] = TEXTSCROLLBUTTON_LEFT_INV;
	    else
		string[0] = TEXTSCROLLBUTTON_LEFT;
	    XDrawString(info->dpy, win, info->gc_rec[OLGX_BLACK]->gc, x, y, string,1);
	}
    }
    /* End 2-D */
    else {			/* Start 3-D */

	olgx_draw_box(info, win, x, y, width, height, state, 0);

	if (state & OLGX_SCROLL_FORWARD)
	    olgx_draw_menu_mark(info, win, x + arr_x, y + arr_y,
				OLGX_HORIZ_MENU_MARK | OLGX_INVOKED, 1);
	else
	    olgx_draw_menu_mark(info, win, x + arr_x - 1, y + arr_y,
				OLGX_HORIZ_BACK_MENU_MARK | OLGX_INVOKED, 1);

    }				/* End 3-D */

    if (state & OLGX_INACTIVE)
	olgx_stipple_rect(info, win, x, y, TextScrollButton_Width(info),
			  TextScrollButton_Height(info));

}






void
olgx_draw_numscroll_button(info, win, x, y, state)
    Graphics_info  *info;
    Window          win;
    int             x, y, state;

{

    char            string[2];
    int             width, height, arr_x, arr_y;

    width = height = TextScrollButton_Height(info);

    if (width < 20) {

	arr_y = 3;
	arr_x = (width / 3) - 1;

    } else {

	arr_y = 5;
	arr_x = 7;

    }


    if (!info->three_d) {	/* draw 2-D */

	if (state & OLGX_ERASE)
	    XFillRectangle(info->dpy, win, info->gc_rec[OLGX_WHITE]->gc, x, y,
                           NumScrollButton_Width(info), height);

	if (state & OLGX_SCROLL_FORWARD)
	    string[0] = NUMERIC_SCROLL_BUTTON_RIGHT_INV;

	else if (state & OLGX_SCROLL_BACKWARD)
	    string[0] = NUMERIC_SCROLL_BUTTON_LEFT_INV;

	else
	    string[0] = NUMERIC_SCROLL_BUTTON_NORMAL;

	XDrawString(info->dpy, win, info->gc_rec[OLGX_BLACK]->gc, x, y, string, 1);

    } else {			/* draw 3-D */

	olgx_draw_box(info, win, x, y, width, height,
                      (state & OLGX_SCROLL_BACKWARD) ?
		       OLGX_INVOKED : OLGX_NORMAL, 0);
	olgx_draw_box(info, win, x + width, y, width, height,
		      (state & OLGX_SCROLL_FORWARD) ?
		      OLGX_INVOKED : OLGX_NORMAL, 0);
	olgx_draw_menu_mark(info, win, x + arr_x, y + arr_y,
			    OLGX_VERT_BACK_MENU_MARK | OLGX_INVOKED, 1);
	olgx_draw_menu_mark(info, win, x + arr_x + width, y + arr_y,
			    OLGX_VERT_MENU_MARK | OLGX_INVOKED, 1);

    }

    if (state & OLGX_INACTIVE)
	olgx_stipple_rect(info, win, x, y, NumScrollButton_Width(info),
			  NumScrollButton_Height(info));

    if (state & OLGX_SCROLL_NO_FORWARD)
	olgx_stipple_rect(info, win, x + TextScrollButton_Width(info) - 1, y,
			  TextScrollButton_Width(info),
			  NumScrollButton_Height(info));

    if (state & OLGX_SCROLL_NO_BACKWARD)
	olgx_stipple_rect(info, win, x, y,
			  TextScrollButton_Width(info) - 2,
			  NumScrollButton_Height(info));
}





