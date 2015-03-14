/*
 * $XConsortium: CutPaste.c,v 1.6 91/07/24 15:33:34 converse Exp $
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

#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include "BitmapP.h"
    
#include <stdio.h>
#include <string.h>
#include <math.h>

#ifndef abs
#define abs(x)                        (((x) > 0) ? (x) : -(x))
#endif
#define min(x, y)                     (((x) < (y)) ? (x) : (y))
#define max(x, y)                     (((x) > (y)) ? (x) : (y))


extern Boolean DEBUG;

/*****************************************************************************
 *                               Cut and Paste                               *
 *****************************************************************************/

/* ARGSUSED */
Boolean ConvertSelection(w, selection, target, type, val_ret, length, format)
    Widget w;
    Atom *selection, *target, *type;
    XtPointer *val_ret;
    unsigned long *length;
    int *format;
{
    caddr_t *value = (caddr_t *)val_ret;
    BitmapWidget BW = (BitmapWidget) w;
    Pixmap *pixmap;
    char *data;
    XImage *image;
    Dimension width, height;
 
    switch (*target) {

/*  XA_TARGETS undefined ?!?

    case XA_TARGETS:
	*type = XA_ATOM;
	*value = (caddr_t) bitmapClassRec.bitmap_class.targets;
	*length = bitmapClassRec.bitmap_class.num_targets;
	*format = 32;
	return True;

*/

    case XA_BITMAP:
    case XA_PIXMAP:
	if (BWQueryMarked(w)) {
	  width = BW->bitmap.mark.to_x - BW->bitmap.mark.from_x + 1;
	  height = BW->bitmap.mark.to_y - BW->bitmap.mark.from_y + 1;
	  data = CreateCleanData(Length(width, height));
	  image = CreateBitmapImage(BW, data, width, height);
	  CopyImageData(BW->bitmap.image, image, 
			BW->bitmap.mark.from_x, BW->bitmap.mark.from_y,
			BW->bitmap.mark.to_x, BW->bitmap.mark.to_y, 0, 0);
	  pixmap = (Pixmap *) XtMalloc(sizeof(Pixmap));
	  *pixmap = GetPixmap(BW, image);
	  DestroyBitmapImage(&image);
	}
	else if (BWQueryStored(w)) {
	  pixmap = (Pixmap *) XtMalloc(sizeof(Pixmap));
	  *pixmap = GetPixmap(BW, BW->bitmap.storage);
	}
	else return False;
	*type = XA_PIXMAP;
	*value = (caddr_t) pixmap;
	*length = 1;
	*format = 32;
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
    BitmapWidget BW = (BitmapWidget) w;

    if (DEBUG)
	fprintf(stderr, "Lost Selection\n");
    BW->bitmap.selection.own = False;

    BWUnmark(w);
}

/* ARGSUSED */
void SelectionDone(w, selection, target)
    Widget w;
    Atom *selection, *target;
{
/*  Done Automatically ?!?

    BitmapWidget BW = (BitmapWidget) w; 

    if (*target != XA_TARGETS)
	XtFree(BW->bitmap.value);

*/
}

void BWGrabSelection(w, btime)
    Widget w;
    Time btime;
{
    BitmapWidget BW = (BitmapWidget) w;

    BW->bitmap.selection.own = XtOwnSelection(w, XA_PRIMARY, btime,
					      ConvertSelection, 
					      LoseSelection, 
					      SelectionDone);
	if (DEBUG && BW->bitmap.selection.own)
	    fprintf(stderr, "Own the selection\n");
}

XImage *GetImage();

/* ARGSUSED */
void SelectionCallback(w, cldat, selection, type, val, length, format)
    Widget w;
    XtPointer cldat;
    Atom *selection, *type;
    XtPointer val;
    unsigned long *length;
    int *format;
{
    caddr_t value = (caddr_t)val;
    BitmapWidget BW = (BitmapWidget) w;
    Pixmap *pixmap;

   switch (*type) {
	
    case XA_BITMAP:
    case XA_PIXMAP:
	DestroyBitmapImage(&BW->bitmap.storage);
	pixmap = (Pixmap *) value;
	BW->bitmap.storage = GetImage(BW, *pixmap);
	XFree((char *)pixmap);
	break;
	
    default:
	XtWarning(" selection request failed.  BitmapWidget");
	break;
    }

    BW->bitmap.selection.limbo = FALSE;
}

void BWRequestSelection(w, btime, wait)
    Widget w;
    Time btime;
    Boolean wait;
{
  BitmapWidget BW = (BitmapWidget) w;
  
  if (BW->bitmap.selection.own)
    BWStore(w);
  else {
    XtGetSelectionValue(w, XA_PRIMARY, XA_PIXMAP,
			SelectionCallback, NULL, btime);
    
    BW->bitmap.selection.limbo = TRUE;
    
    if (wait)
      while (BW->bitmap.selection.limbo) {
	XEvent event;
	XtNextEvent(&event);
	XtDispatchEvent(&event);
      }
  }
}

/* ARGSUSED */
/* Returns true if there is a transferable selection */
Boolean BWQuerySelection(w, btime)
    Widget w;
    Time btime;
{
/* To be written.  XA_TARGETS to be used.  So far undefined ?!? */

  return True;
}
/*****************************************************************************/
