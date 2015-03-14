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
 * Glyph - visible data
 */

#include <InterViews/glyph.h>
#include <InterViews/hit.h>
#include <InterViews/printer.h>

Glyph::Glyph() { }
Glyph::~Glyph() { }

void Glyph::request(Requisition&) const { }
void Glyph::allocate(Canvas*, const Allocation&, Extension&) { }
void Glyph::draw(Canvas*, const Allocation&) const { }
void Glyph::print(Printer* p, const Allocation& a) const { draw(p, a); }

void Glyph::pick(Canvas*, const Allocation& a, int depth, Hit& h) {
    Coord x = h.left();
    Coord y = h.bottom();
    if (x >= a.left() && x < a.right() && y >= a.bottom() && y < a.top()) {
        h.target(depth, this, 0);
    }
}

void Glyph::undraw() {
    GlyphIndex n = count();
    for (GlyphIndex i = 0; i < n; i++) {
	Glyph* g = component(i);
	if (g != nil) {
	    g->undraw();
	}
    }
}

Glyph* Glyph::clone() const {
    return nil;
}

Glyph* Glyph::compose(GlyphBreakType b) {
    return (b == no_break || b == pre_break) ? this : nil;
}

void Glyph::append(Glyph*) { }
void Glyph::prepend(Glyph*) { }
void Glyph::insert(GlyphIndex, Glyph*) { }
void Glyph::remove(GlyphIndex) { }
void Glyph::replace(GlyphIndex, Glyph*) { }
void Glyph::change(GlyphIndex) { }

GlyphIndex Glyph::count() const { return 0; }
Glyph* Glyph::component(GlyphIndex) const { return nil; }

void Glyph::allotment(GlyphIndex, DimensionName, Allotment& a) const {
    Allotment def;
    a = def;
}
