/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

#ifndef IV_X11_Xext_h
#define IV_X11_Xext_h

extern "C" {

#include <IV-X11/Xdefs.h>

#define XShapeQueryExtension cc_XShapeQueryExtension
#define XShapeQueryVersion cc_XShapeQueryVersion
#define XShapeCombineRegion cc_XShapeCombineRegion
#define XShapeCombineRectangles cc_XShapeCombineRectangles
#define XShapeCombineMask cc_XShapeCombineMask
#define XShapeCombineShape cc_XShapeCombineShape
#define XShapeOffsetShape cc_XShapeOffsetShape
#define XShapeQueryExtents cc_XShapeQueryExtents
#define XShapeSelectInput cc_XShapeSelectInput
#define XShapeInputSelected cc_XShapeInputSelected
#define XShapeGetRectangles cc_XShapeGetRectangles

#include <X11/extensions/Xext.h>
#include <X11/extensions/shape.h>

#undef XShapeQueryExtension
#undef XShapeQueryVersion
#undef XShapeCombineRegion
#undef XShapeCombineRectangles
#undef XShapeCombineMask
#undef XShapeCombineShape
#undef XShapeOffsetShape
#undef XShapeQueryExtents
#undef XShapeSelectInput
#undef XShapeInputSelected
#undef XShapeGetRectangles

Bool XShapeQueryExtension (Display*, int*, int*);
Status XShapeQueryVersion (Display*, int*, int*);
void XShapeCombineRegion (Display*, XWindow, int, int, int, Region, int);
void XShapeCombineRectangles (
    Display*, XWindow, int, int, int, XRectangle*, int, int, int
);
void XShapeCombineMask (Display*, XWindow, int, int, int, Pixmap, int);
void XShapeCombineShape (Display*, XWindow, int, int, int, XWindow, int, int);
void XShapeOffsetShape (Display*, XWindow, int, int, int);
Status XShapeQueryExtents (
    Display*, XWindow,
    Bool*, int*, int*, unsigned int*, unsigned int*,
    Bool*, int*, int*, unsigned int*, unsigned int*
);
void XShapeSelectInput (Display*, XWindow, Bool);
unsigned long XShapeInputSelected (Display*, XWindow);
XRectangle *XShapeGetRectangles (Display*, XWindow, int, int*, int*);

#include <IV-X11/Xundefs.h>

}

#endif
