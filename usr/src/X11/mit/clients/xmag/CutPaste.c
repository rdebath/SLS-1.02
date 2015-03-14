/*
 * $XConsortium: CutPaste.c,v 1.3 91/07/19 18:29:10 dave Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
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
 * Author:  Davor Matic, MIT X Consortium
 */

#include <stdio.h>
#include <X11/Xatom.h>
#include <X11/IntrinsicP.h>
#include "ScaleP.h"
#include "Scale.h"

#define XtStrlen(s) ((s) ? strlen(s) : 0)

extern Pixmap SWGetPixmap();

/* ARGSUSED */
Boolean ConvertSelection(w, selection, target, type, vret, length, format)
    Widget w;
    Atom *selection, *target, *type;
    XtPointer *vret;
    unsigned long *length;
    int *format;
{
    caddr_t *value = (caddr_t *)vret;
    ScaleWidget sw = (ScaleWidget) w;
    Pixmap *pixmap;
 
    switch (*target) {
/*
    case XA_TARGETS:
	*type = XA_ATOM;
	*value = (caddr_t) scaleClassRec.scale_class.targets;
	*length = scaleClassRec.scale_class.num_targets;
	*format = 32;
	return True;
*/

    case XA_PIXMAP:
    case XA_BITMAP:
	pixmap = (Pixmap *) XtMalloc(sizeof(Pixmap));
	*pixmap = XCreatePixmap(XtDisplay(w), XtWindow(w),
				sw->scale.image->width, 
				sw->scale.image->height, 
				sw->scale.image->depth);
	XPutImage(XtDisplay(w), *pixmap, sw->scale.gc, sw->scale.image,
		  0, 0, 0, 0, sw->scale.image->width, sw->scale.image->height);
	*type = XA_PIXMAP;
	*value = (caddr_t) pixmap;
	*length = 1;
	*format = 32;
	return True;
	
    case XA_STRING:
	*type = XA_STRING;
	*value = (caddr_t)"Hello world!";
	*length = XtStrlen(*value);
	*format = 8;
	return True;
	
    default:
	return False;
    }
}

/* ARGSUSED */
void LoseSelection(w, selection)
    Widget w;
    Atom *selection;
{

	/*fprintf(stderr, "Lost Selection\n");*/
}

/* ARGSUSED */
void SelectionDone(w, selection, target)
    Widget w;
    Atom *selection, *target;
{
/*  
    ScaleWidget sw = (ScaleWidget) w;
    if (*target != XA_TARGETS)
	XtFree(sw->scale.value);
*/
}

void SWGrabSelection(w, time)
    Widget w;
    Time time;
{
    
    if (XtOwnSelection(w, XA_PRIMARY, time,
		       ConvertSelection, LoseSelection, SelectionDone))

	    /*fprintf(stderr, "Own the selection\n")*/;
}

/* ARGSUSED */
void SelectionCallback(w, clientData, selection, type, v, length, format)
    Widget w;
    XtPointer clientData; 
    Atom *selection, *type;
    XtPointer v;
    unsigned long *length;
    int *format;
{
    caddr_t value = (caddr_t)v;
    Pixmap *pixmap;
    XImage *image;
    Window root;
    int x, y;
    unsigned int width, height, border_width, depth;

    switch (*type) {
	
    case XA_PIXMAP:
	pixmap = (Pixmap *) value;
	XGetGeometry(XtDisplay(w), *pixmap, &root, &x, &y,
		     &width, &height, &border_width, &depth);
	image = XGetImage(XtDisplay(w), *pixmap, 0, 0, width, height, 
			  AllPlanes, ZPixmap);
	SWAutoscale(w);
	SWSetImage(w, image);
	XFree((char *)pixmap);
	XDestroyImage(image);
	break;
	
    case XA_STRING:
	    /*fprintf(stderr, "Received:%s\n", value);*/
	break;

    default:
	XtWarning(" selection request failed.  ScaleWidget");
	break;
    }
}

void SWRequestSelection(w, time)
    Widget w;
    Time time;
{
    /*fprintf(stderr, "------------------------------------------>\n");*/
    XtGetSelectionValue(w, XA_PRIMARY, XA_PIXMAP,
			SelectionCallback, NULL, time);
}

