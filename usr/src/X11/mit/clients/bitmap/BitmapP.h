/*
 * $XConsortium: BitmapP.h,v 1.11 91/02/01 16:39:28 dmatic Exp $
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



#ifndef _BitmapP_h
#define _BitmapP_h

#include "Bitmap.h"
#include <X11/Xaw/SimpleP.h>

typedef struct {
    Atom           *targets;
    Cardinal        num_targets;
    BWRequestRec   *requests;
    Cardinal        num_requests;
    BWRequestRec   *request[100];
  
} BitmapClassPart;

/* Full class record declaration */
typedef struct _BitmapClassRec {
  CoreClassPart          core_class;
  SimpleClassPart        simple_class;
  BitmapClassPart        bitmap_class;
} BitmapClassRec;

extern BitmapClassRec bitmapClassRec;

/**********/
struct _BWRequestRec {
  char       *name;
  int         status_size;
  void      (*engage)();
  caddr_t     engage_client_data;
  void      (*terminate)();
  caddr_t     terminate_client_data;
  void      (*remove)();
  caddr_t     remove_client_data;
} ;

typedef struct {
  Position from_x, from_y,
           to_x, to_y;
} BWArea;

typedef struct {
    BWRequestRec *request;
    caddr_t       status;
    Boolean       trap;
    caddr_t       call_data;
} BWRequestStack;

typedef struct {
    XImage   *image, *buffer;
    XPoint    hot;
    Position  at_x, at_y;
    Boolean   fold;
    Boolean   grid;
    Boolean   changed;
} BWZoom;

typedef struct {
    Boolean   own;
    Boolean   limbo;
} BWSelection;

/* New fields for the Bitmap widget record */
typedef struct {
  /* resources */
  Pixel            foreground_pixel;
  Pixel            highlight_pixel;
  Pixel            frame_pixel;
  Pixmap           stipple;
  Boolean          stippled;
  Boolean          proportional;
  Boolean          grid;
  Dimension        grid_tolerance;
  Pixmap           dashes;
  Boolean          dashed;
  Boolean          axes;
  Boolean          resize;
  Dimension        margin, squareW, squareH, width, height;
  XPoint           hot;
  int              button_function[5];
  String           filename, basename;
  /* private state */
  String           size;
  Position         horizOffset, vertOffset;
  void           (*notify)();
  BWRequestStack  *request_stack;
  Cardinal         cardinal, current;
  /*Boolean          trapping;*/
  XImage          *image, *buffer, *storage;
  XPoint           buffer_hot;
  BWArea           mark, buffer_mark;
  GC               drawing_gc;
  GC               highlighting_gc;
  GC               frame_gc;
  GC               axes_gc;
  Boolean          changed;
  Boolean          fold;
  Boolean          zooming;
  BWZoom           zoom;
  caddr_t         *value;
  char             status[80];
  BWSelection      selection;
  Boolean          stipple_change_expose_event;
} BitmapPart;

/* Full instance record declaration */
typedef struct _BitmapRec {
  CorePart      core;
  SimplePart    simple;
  BitmapPart    bitmap;
} BitmapRec;

/* Private functions */

#define Length(width, height)\
        (((int)(width) + 7) / 8 * (height))

#define InBitmapX(BW, x)\
	(Position)(min((Position)((Dimension)(max(BW->bitmap.horizOffset,x)  -\
				   BW->bitmap.horizOffset) /\
				   BW->bitmap.squareW), BW->bitmap.width - 1))
    
#define InBitmapY(BW, y)\
	(Position)(min((Position)((Dimension)(max(BW->bitmap.vertOffset, y)  -\
				   BW->bitmap.vertOffset) /\
				   BW->bitmap.squareH), BW->bitmap.height - 1))
    
#define InWindowX(BW, x)\
	(Position) (BW->bitmap.horizOffset + ((x) * BW->bitmap.squareW))

#define InWindowY(BW, y)\
	(Position) (BW->bitmap.vertOffset + ((y) * BW->bitmap.squareH))
     
#define GetPixmap(BW, image)\
    XCreateBitmapFromData(XtDisplay(BW), XtWindow(BW),\
			  image->data, image->width, image->height)


#define QuerySet(x, y) (((x) != NotSet) && ((y) != NotSet))

#define bit int

#define QueryZero(x, y) (((x) == 0) || ((y) == 0))

#define Swap(x, y) {Position t; t = x; x = y; y = t;}

#define QuerySwap(x, y) if(x > y) Swap(x, y)

#define QueryInBitmap(BW, x, y)\
  (((x) >= 0) && ((x) < BW->bitmap.image->width) &&\
   ((y) >= 0) && ((y) < BW->bitmap.image->height))

#define Value(BW, button)   (BW->bitmap.button_function[button - 1])

#define CreateCleanData(length) XtCalloc(length, sizeof(char))
XImage *CreateBitmapImage();
void DestroyBitmapImage();

#endif /* _BitmapP_h */
