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

/*
 * X11-dependent painter interface.
 */

#ifndef iv_xpainter_h
#define iv_xpainter_h

#include <IV-X11/Xlib.h>

class Brush;
class Pattern;
class Transformer;

class PainterRep {
public:
    PainterRep();
    ~PainterRep();

    void PrepareFill(const Pattern*);
    void PrepareDash(const Brush*);

    GC fillgc;
    GC dashgc;
    boolean fillbg;
    boolean overwrite;
    boolean xor;
    boolean clipped;
    Display* display;
    XRectangle xclip[1];
};

void DrawTransformedImage(
    XImage* src, int sx0, int sy0, XImage* mask, int mx0, int my0,
    XDrawable d, unsigned int height, int dx0, int dy0,
    boolean stencil, unsigned long fg, unsigned long bg,
    GC gc, const Transformer& matrix
);

#endif
