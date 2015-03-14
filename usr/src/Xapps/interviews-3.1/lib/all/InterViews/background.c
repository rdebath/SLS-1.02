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
 * Background - opaque glyph
 */

#include <InterViews/background.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/printer.h>

Background::Background(Glyph* body, const Color* c) : MonoGlyph(body) {
    color_ = c;
    Resource::ref(color_);
}

Background::~Background() {
    Resource::unref(color_);
}

void Background::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    MonoGlyph::allocate(c, a, ext);
    ext.merge(c, a);
}

void Background::draw(Canvas* c, const Allocation& a) const {
    Extension ext;
    ext.set(c, a);
    if (c->damaged(ext)) {
	c->fill_rect(a.left(), a.bottom(), a.right(), a.top(), color_);
    }
    MonoGlyph::draw(c, a);
}

void Background::print(Printer* p, const Allocation& a) const {
    p->fill_rect(a.left(), a.bottom(), a.right(), a.top(), color_);
    MonoGlyph::print(p, a);
}
