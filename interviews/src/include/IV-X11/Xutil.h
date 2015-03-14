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

#ifndef IV_X11_Xutil_h
#define IV_X11_Xutil_h

#include <IV-X11/Xlib.h>

extern "C" {

#include <IV-X11/Xdefs.h>
#include <X11/Xutil.h>
#include <IV-X11/Xundefs.h>

}

/*
 * These are macros that make use of untype-checking pointers to functions.
 */

#undef XDestroyImage
#undef XGetPixel
#undef XPutPixel
#undef XSubImage
#undef XAddPixel

inline int XDestroyImage(XImage* i) {
    typedef int (*PF)(XImage*);
    return (*((PF)i->f.destroy_image))(i);
}

inline unsigned long XGetPixel(XImage* i, int x, int y) {
    typedef unsigned long (*PF)(XImage*, int, int);
    return (*((PF)i->f.get_pixel))(i, x, y);
}

inline int XPutPixel(XImage* i, int x, int y, unsigned long pixel) {
    typedef int (*PF)(XImage*, int, int, unsigned long);
    return (*((PF)i->f.put_pixel))(i, x, y, pixel);
}

inline XImage* XSubImage(
    XImage* i, int x, int y, unsigned int width, unsigned int height
) {
    typedef XImage* (*PF)(XImage*, int, int, unsigned int, unsigned int);
    return (*((PF)i->f.sub_image))(i, x, y, width, height);
}

inline int XAddPixel(XImage* i, long value) {
    typedef int (*PF)(XImage*, long);
    return (*((PF)i->f.add_pixel))(i, value);
}

#endif
