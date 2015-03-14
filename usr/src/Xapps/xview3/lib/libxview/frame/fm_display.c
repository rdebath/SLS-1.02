#ifndef lint
#ifdef sccs
static char     sccsid[] = "@(#)fm_display.c 20.71 91/09/14";
#endif
#endif

/*
 *	(c) Copyright 1989 Sun Microsystems, Inc. Sun design patents 
 *	pending in the U.S. and foreign countries. See LEGAL NOTICE 
 *	file for terms of the license.
 */

/*
 * Handle frame displaying and size changes.
 */

#include <X11/Xlib.h>
#include <xview_private/fm_impl.h>
#include <xview_private/draw_impl.h>
#include <xview/server.h>
#include <xview/screen.h>
#include <xview/font.h>
#include <xview/cms.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>


Pkg_private void
frame_display_label(frame)
    register Frame_class_info *frame;
{
#ifdef OW_I18N
    Xv_Drawable_info *info;
    wchar_t    *wp;
    char       *p;

    DRAWABLE_INFO_MACRO(FRAME_PUBLIC(frame), info);
    for (wp = frame->label_wcs; *wp; wp++)
    {
        if (! iswascii(*wp))
        {
            /*
             * There are no ASCII characters, so, we have to
             * send it as Compound Text Atom.
             */
            p = wcstoctsdup(frame->label_wcs);
            XChangeProperty(xv_display(info), xv_xid(info), XA_WM_NAME,
                            xv_get(xv_server(info), SERVER_COMPOUND_TEXT), 
			    8, PropModeReplace, (unsigned char *)p, strlen(p));
            free(p);
	    return;
        }
    }
 
    /*
     * There are only ASCII characters, we can send it as STRING atom.
     */
    p = wcstombsdup(frame->label_wcs);
    XStoreName(xv_display(info), xv_xid(info), p);
    free(p);
    return;
#else
    Xv_Drawable_info *info;

    DRAWABLE_INFO_MACRO(FRAME_PUBLIC(frame), info);
    XStoreName(xv_display(info), xv_xid(info), frame->label);
#endif
}

Pkg_private void
frame_display_footer(frame_public, clear_first)
    Frame frame_public;
    int clear_first;
{
    Frame_class_info *frame = FRAME_PRIVATE(frame_public);
    Xv_Drawable_info	*info;
    int left_width, right_width;
    int max_left_width, max_right_width;
    int margin;
    int gap;
    int baseline;
    int footer_width;     
    int quarter_width;
    Frame_rescale_state scale;
    
    DRAWABLE_INFO_MACRO(frame->footer, info);
    
    scale = xv_get(xv_get(frame_public, XV_FONT), FONT_SCALE);

#ifdef OW_I18N
    if (frame->leftfooter_wcs == NULL) 
      left_width = 0;
    else 
      left_width = XwcTextEscapement(frame->ginfo->textfontset, 
			      frame->leftfooter_wcs, 
			      wslen(frame->leftfooter_wcs));
    if (frame->rightfooter_wcs == NULL)
      right_width = 0;
    else
      right_width = XwcTextEscapement(frame->ginfo->textfontset, 
			       frame->rightfooter_wcs, 
			       wslen(frame->rightfooter_wcs));
#else
    if (frame->left_footer == NULL) 
      left_width = 0;
    else 
      left_width = XTextWidth(frame->ginfo->textfont, 
			      frame->left_footer, 
			      strlen(frame->left_footer));
    if (frame->right_footer == NULL)
      right_width = 0;
    else
      right_width = XTextWidth(frame->ginfo->textfont, 
			       frame->right_footer, 
			       strlen(frame->right_footer));
#endif
    
    margin = frame_footer_margin(scale);
    gap = frame_inter_footer_gap(scale);  
    footer_width = (int)xv_get(frame_public, XV_WIDTH) - 2 * margin;
    quarter_width = footer_width / 4;
    baseline = (int)xv_get(frame->footer, XV_HEIGHT) -
		    frame_footer_baseline(scale);

    if ((left_width + gap + right_width) <= footer_width) {
	/* They both fit, no clipping */
	max_left_width = left_width;
	max_right_width = right_width;
    } else if (right_width < quarter_width) {
	/* right footer takes less than 1/4 of the footer */
	max_left_width = footer_width - gap - right_width;
	max_right_width = right_width;
    } else if (left_width < (footer_width - quarter_width - gap)) {
	/* left footer takes less than 3/4 of the footer */
	max_left_width = left_width;
	max_right_width = footer_width - max_left_width - gap;
    } else {
	/* must truncate both */
	max_left_width = footer_width - quarter_width - gap;
	max_right_width = quarter_width;
    }    

    if (clear_first)
      XClearWindow(xv_display(info), xv_xid(info));
#ifdef OW_I18N
    if (frame->leftfooter_wcs != NULL) {
	olgx_draw_text(frame->ginfo, xv_xid(info), frame->leftfooter_wcs, 
		       margin, baseline, max_left_width, 
		       OLGX_NORMAL | OLGX_MORE_ARROW);
    }
    if (frame->rightfooter_wcs != NULL) {
	olgx_draw_text(frame->ginfo, xv_xid(info), frame->rightfooter_wcs,
		       footer_width + margin - max_right_width, baseline,
		       max_right_width, OLGX_NORMAL | OLGX_MORE_ARROW);
    }
#else
    if (frame->left_footer != NULL) {
	olgx_draw_text(frame->ginfo, xv_xid(info), frame->left_footer, 
		       margin, baseline, max_left_width, 
		       OLGX_NORMAL | OLGX_MORE_ARROW);
    }
    if (frame->right_footer != NULL) {
	olgx_draw_text(frame->ginfo, xv_xid(info), frame->right_footer,
		       footer_width + margin - max_right_width, baseline,
		       max_right_width, OLGX_NORMAL | OLGX_MORE_ARROW);
    }
    XFlush(xv_display(info));
#endif
}

#ifdef OW_I18N
Pkg_private void
frame_display_IMstatus(frame_public, clear_first)
    Frame frame_public;
    int clear_first;
{
    Frame_class_info *frame = FRAME_PRIVATE(frame_public);
    Xv_Drawable_info	*info;
    int left_width, right_width;
    int max_left_width, max_right_width;
    int margin;
    int gap;
    int baseline;
    int footer_width;     
    int quarter_width;
    Frame_rescale_state scale;
    
    DRAWABLE_INFO_MACRO(frame->imstatus, info);
    
    scale = xv_get(xv_get(frame_public, XV_FONT), FONT_SCALE);

    if (frame->leftIMstatus_wcs == NULL) 
      left_width = 0;
    else 
      left_width = XwcTextEscapement(frame->ginfo->textfontset, 
			      frame->leftIMstatus_wcs, 
			      wslen(frame->leftIMstatus_wcs));
    if (frame->rightIMstatus_wcs == NULL)
      right_width = 0;
    else
      right_width = XwcTextEscapement(frame->ginfo->textfontset, 
			       frame->rightIMstatus_wcs, 
			       wslen(frame->rightIMstatus_wcs));
    
    margin = frame_footer_margin(scale);
    gap = frame_inter_footer_gap(scale);  
    footer_width = (int)xv_get(frame_public, XV_WIDTH) - 2 * margin;
    quarter_width = footer_width / 4;
    baseline = (int)xv_get(frame->imstatus, XV_HEIGHT) - frame_footer_baseline(scale);

    if ((left_width + gap + right_width) <= footer_width) {
	/* They both fit, no clipping */
	max_left_width = left_width;
	max_right_width = right_width;
    } else if (right_width < quarter_width) {
	/* right footer takes less than 1/4 of the footer */
	max_left_width = footer_width - gap - right_width;
	max_right_width = right_width;
    } else if (left_width < (footer_width - quarter_width - gap)) {
	/* left footer takes less than 3/4 of the footer */
	max_left_width = left_width;
	max_right_width = footer_width - max_left_width - gap;
    } else {
	/* must truncate both */
	max_left_width = footer_width - quarter_width - gap;
	max_right_width = quarter_width;
    }    

    if (clear_first)
      XClearWindow(xv_display(info), xv_xid(info));
    if (frame->leftIMstatus_wcs != NULL) {
	olgx_draw_text(frame->ginfo, xv_xid(info), frame->leftIMstatus_wcs, 
		       margin, baseline, max_left_width, 
		       OLGX_NORMAL | OLGX_MORE_ARROW);
    }
    if (frame->rightIMstatus_wcs != NULL) {
	olgx_draw_text(frame->ginfo, xv_xid(info), frame->rightIMstatus_wcs,
		       footer_width + margin - max_right_width, baseline,
		       max_right_width, OLGX_NORMAL | OLGX_MORE_ARROW);
    }
}
#endif OW_I18N

Pkg_private void
frame_display_busy(frame, status)
    register Frame_class_info *frame;
    int             status;

{
    Frame           frame_public = FRAME_PUBLIC(frame);
    Xv_Drawable_info *info;
    Xv_object       screen, server;


    DRAWABLE_INFO_MACRO(frame_public, info);
    screen = xv_get(frame_public, XV_SCREEN);
    server = xv_get(screen, SCREEN_SERVER);

    XChangeProperty(xv_display(info), xv_xid(info),
		    xv_get(server, SERVER_WM_WIN_BUSY), XA_INTEGER,
		    32, PropModeReplace, (unsigned char *)&status,
		    1);
    XFlush(xv_display(info));
}

/*
 * highlight subwindow border for sw.
 */
Xv_private void
frame_kbd_use(frame_public, sw, pw)
    Frame           frame_public;
    Xv_Window       sw;		/* frame subwindow */
    Xv_Window	    pw;		/* paint window that has the keyboard focus.
				 * This may or may not be equal to sw. */
{
    Frame_class_info *frame = FRAME_CLASS_PRIVATE(frame_public);
    Cms cms;

    if (frame->focus_subwindow != sw) {
	/* Remove caret from current frame focus subwindow */
	if (frame->focus_subwindow)
	    xv_set(frame->focus_subwindow, WIN_REMOVE_CARET, 0);
	/* Update frame focus subwindow and primary focus subwindow data */
	frame->focus_subwindow = sw;
	if (xv_get(sw, XV_FOCUS_RANK) == XV_FOCUS_PRIMARY)
	    frame->primary_focus_sw = sw;
    }

    /* Set the CMS, foreground and background color of the focus window to be
     * the same as the paint window with the input focus.
     */
    /* BUG: We should be keeping seperate focus window's for different
     * visuals.  Because we don't we can not just set the the cms from the 
     * paint window onto the focus window, because they may be of different visuals.
     */
    
    cms = (Cms)xv_get(pw, WIN_CMS);
    if (XVisualIDFromVisual((Visual *)xv_get(frame->focus_window, XV_VISUAL)) ==
	XVisualIDFromVisual((Visual *)xv_get(cms, XV_VISUAL)))
      xv_set(frame->focus_window,
	     WIN_CMS, cms,
	     WIN_FOREGROUND_COLOR, xv_get(pw, WIN_FOREGROUND_COLOR),
	     WIN_BACKGROUND_COLOR, xv_get(pw, WIN_BACKGROUND_COLOR),
	     0);
    
    /* Show caret in frame subwindow with input focus */
    xv_set(sw, WIN_KBD_FOCUS, TRUE, 0);
}

/*
 * unhighlight subwindow border for client.
 */
/*ARGSUSED*/
Xv_private void
frame_kbd_done(frame_public, sw)
    Frame           frame_public;
    Xv_Window       sw;
{
    xv_set(sw, WIN_KBD_FOCUS, FALSE, 0);
}

/* ARGSUSED */
Pkg_private
frame_set_color(frame, fg, bg)
    Frame_class_info *frame;
    XColor *fg;
    XColor *bg;
{
    Frame		frame_public = FRAME_PUBLIC(frame);
    Xv_Drawable_info	*info;
    Cms			cms;	
    XColor		xcolors[2];

    DRAWABLE_INFO_MACRO(frame_public, info);
    if (!fg && !bg) {
	return;
    }

    xcolors[0].red = bg->red;
    xcolors[0].green = bg->green;
    xcolors[0].blue = bg->blue;

    xcolors[1].red = fg->red;
    xcolors[1].green = fg->green;
    xcolors[1].blue = fg->blue;

    cms = (Cms)xv_create(xv_screen(info), CMS,
			 CMS_SIZE, 2,
			 CMS_X_COLORS, xcolors,
			 CMS_FRAME_CMS, TRUE,
			 CMS_TYPE, XV_STATIC_CMS,
			 XV_VISUAL, xv_get(frame_public, XV_VISUAL),
			 NULL);

    if (cms != (Cms)NULL)
      xv_set(frame_public, WIN_CMS, cms, NULL);
}
