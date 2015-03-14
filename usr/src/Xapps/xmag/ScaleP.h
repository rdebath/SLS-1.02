/*
 * $XConsortium: ScaleP.h,v 1.2 91/03/11 18:50:54 dave Exp $
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



#ifndef _ScaleP_h
#define _ScaleP_h

#include "Scale.h"
#include <X11/Xaw/SimpleP.h>

typedef struct {
    int foo;
} ScaleClassPart;

/* Full class record declaration */
typedef struct _ScaleClassRec {
  CoreClassPart          core_class;
  SimpleClassPart        simple_class;
  ScaleClassPart         scale_class;
} ScaleClassRec;

extern ScaleClassRec scaleClassRec;

typedef struct {
    Position *x, *y;
    Dimension *width, *height;
} Table;

/* New fields for the Scale widget record */

#ifndef XtGravity
#define  XtGravity int
#endif

typedef struct {
  /* resources */
    Pixel       foreground_pixel;
    Dimension   internal_width;
    Dimension   internal_height;
    XtGravity   gravity;
    String      scale_x_str, scale_y_str;
    String      aspect_ratio_str;
    String      precision_str;
    XImage      *image;
    Boolean     resize;
    Boolean     autoscale;
    Boolean     proportional;
    Boolean     paste_buffer;
    Cardinal    buffer_size;
    XtPointer   userData;
    Visual      *visual;
  /* private */
    float       scale_x, scale_y;
    float       aspect_ratio;
    float       precision;
    GC          gc;
    Position    x, y;
    Dimension   width, height;
    Table       table;
    XRectangle  *rectangles;
    Cardinal    nrectangles;
} ScalePart;

/* Full instance record declaration */
typedef struct _ScaleRec {
  CorePart      core;
  SimplePart    simple;
  ScalePart scale;
} ScaleRec;

#endif /* _ScaleP_h */




