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
 * Shape constructors
 */

#include <IV-2_6/InterViews/shape.h>

Shape::Shape () {
    SetUndefined();
    Rigid(hfil, hfil, vfil, vfil);
    aspect = 0;
    hunits = 1;
    vunits = 1;
}

Shape::~Shape () {
    /* nothing to do for now */
}

void Shape::Square (int side) {
    width = side; height = side;
    Rigid();
    aspect = 1;
}

void Shape::Rect (int w, int h) {
    width = w; height = h;
    Rigid();
}

void Shape::Rigid (int hshr, int hstr, int vshr, int vstr) {
    hshrink = hshr;
    hstretch = hstr;
    vshrink = vshr;
    vstretch = vstr;
}

void Shape::SetUndefined () {
    width = 0;
    height = 0;
}

boolean Shape::Defined () {
    return width != 0;
}
