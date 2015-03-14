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
 * X11-dependent Font representation.
 */

#ifndef iv_xfont_h
#define iv_xfont_h

#include <InterViews/boolean.h>
#include <OS/enter-scope.h>
#include <IV-X11/Xlib.h>

#include <InterViews/resource.h>

class Display;
class KnownFonts;
class String;

class FontRep : public Resource {
public:
    FontRep(Display*, XFontStruct*, float);
    ~FontRep();

    Display* display_;
    XFontStruct* font_;
    float scale_;
    boolean unscaled_;
    String* name_;
    String* encoding_;
    float size_;
    KnownFonts* entry_;
};

class FontFamilyRep {
public:
    Display* display_;
    int count_;
    int min_weight_;
    int max_weight_;
    int min_width_;
    int max_width_;
    int min_slant_;
    int max_slant_;
    int min_size_;
    int max_size_;

    char** names_;
    int* weights_;
    int* slants_;
    int* widths_;
    int* sizes_;
};

#include <InterViews/_leave.h>

#endif
