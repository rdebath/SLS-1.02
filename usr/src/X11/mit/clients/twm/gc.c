/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/


/**********************************************************************
 *
 * $XConsortium: gc.c,v 1.22 91/01/09 17:13:12 rws Exp $
 *
 * Open the fonts and create the GCs
 *
 * 31-Mar-88 Tom LaStrange        Initial Version.
 *
 **********************************************************************/

#include <stdio.h>
#include "twm.h"
#include "util.h"
#include "screen.h"

/***********************************************************************
 *
 *  Procedure:
 *	CreateGCs - open fonts and create all the needed GC's.  I only
 *		    want to do this once, hence the first_time flag.
 *
 ***********************************************************************
 */

void
CreateGCs()
{
    static ScreenInfo *prevScr = NULL;
    XGCValues	    gcv;
    unsigned long   gcm;

    if (!Scr->FirstTime || prevScr == Scr)
	return;

    prevScr = Scr;

    /* create GC's */

    gcm = 0;
    gcm |= GCFunction;	    gcv.function = GXxor;
    gcm |= GCLineWidth;	    gcv.line_width = 0;
    gcm |= GCForeground;    gcv.foreground = Scr->XORvalue;
    gcm |= GCSubwindowMode; gcv.subwindow_mode = IncludeInferiors;

    Scr->DrawGC = XCreateGC(dpy, Scr->Root, gcm, &gcv);

    gcm = 0;
    gcm |= GCForeground;    gcv.foreground = Scr->MenuC.fore;
    gcm |= GCBackground;    gcv.background = Scr->MenuC.back;
    gcm |= GCFont;	    gcv.font =  Scr->MenuFont.font->fid;

    Scr->MenuGC = XCreateGC(dpy, Scr->Root, gcm, &gcv);

    gcm = 0;
    gcm |= GCPlaneMask;	    gcv.plane_mask = AllPlanes;
    /*
     * Prevent GraphicsExpose and NoExpose events.  We'd only get NoExpose
     * events anyway;  they cause BadWindow errors from XGetWindowAttributes
     * call in FindScreenInfo (events.c) (since drawable is a pixmap).
     */
    gcm |= GCGraphicsExposures;  gcv.graphics_exposures = False;
    gcm |= GCLineWidth;	    gcv.line_width = 0;

    Scr->NormalGC = XCreateGC(dpy, Scr->Root, gcm, &gcv);
}
