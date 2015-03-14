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
 * Placement - monoglyph with layout
 */

#include <InterViews/place.h>
#include <InterViews/printer.h>
#include <InterViews/superpose.h>
#include <OS/math.h>

Placement::Placement(Glyph* body, Layout* layout) : MonoGlyph(body) {
    layout_ = layout;
}

Placement::~Placement() {
    delete layout_;
}

void Placement::request(Requisition& requisition) const {
    Glyph* g = body();
    if (g != nil) {
	g->request(requisition);
    }
    layout_->request(0, nil, requisition);
}

static void place(
    Glyph* g, const Allocation& given, Layout* layout, Allocation& result
) {
    Requisition r;
    g->request(r);
    layout->allocate(given, 1, &r, &result);
}

void Placement::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    Glyph* g = body();
    if (g != nil) {
	Allocation b(a);
	place(g, a, layout_, b);
	g->allocate(c, b, ext);
    }
}    

void Placement::draw(Canvas* c, const Allocation& a) const {
    Glyph* g = body();
    if (g != nil) {
	Allocation b(a);
	place(g, a, layout_, b);
	g->draw(c, b);
    }
}

void Placement::print(Printer* p, const Allocation& a) const {
    Glyph* g = body();
    if (g != nil) {
	Allocation b(a);
	place(g, a, layout_, b);
	g->print(p, b);
    }
}

void Placement::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    Glyph* g = body();
    if (g != nil) {
	Allocation b(a);
	place(g, a, layout_, b);
	g->pick(c, b, depth, h);
    }
}

CenterLayout::CenterLayout(const DimensionName d, float alignment) {
    dimension_ = d;
    alignment_ = alignment;
}

CenterLayout::~CenterLayout() { }

void CenterLayout::request(
    GlyphIndex, const Requisition*, Requisition& result
) {
    Requirement& r = result.requirement(dimension_);
    r.alignment(alignment_);
}

void CenterLayout::allocate(
    const Allocation&, GlyphIndex, const Requisition* r, Allocation* result
) {
    Allotment& a = result[0].allotment(dimension_);
    float calign = r[0].requirement(dimension_).alignment();
    a.offset(Coord(float(a.span()) * (calign - a.alignment())));
    a.alignment(calign);
}

FixedLayout::FixedLayout(const DimensionName d, Coord span) {
    dimension_ = d;
    span_ = span;
}

FixedLayout::~FixedLayout() { }

void FixedLayout::request(
    GlyphIndex, const Requisition*, Requisition& result
) {
    Requirement& r = result.requirement(dimension_);
    r.natural(span_);
    r.stretch(0.0);
    r.shrink(0.0);
}

void FixedLayout::allocate(
    const Allocation&, GlyphIndex,
    const Requisition*, Allocation* result
) {
    Allotment& a = result[0].allotment(dimension_);
    a.span(span_);
}

VariableLayout::VariableLayout(
    const DimensionName d, Coord stretch, Coord shrink
) {
    dimension_ = d;
    stretch_ = stretch;
    shrink_ = shrink;
}

VariableLayout::~VariableLayout() { }

void VariableLayout::request(
    GlyphIndex, const Requisition*, Requisition& result
) {
    Requirement& r = result.requirement(dimension_);
    r.stretch(stretch_);
    r.shrink(Math::min(shrink_, r.natural()));
}

void VariableLayout::allocate(
    const Allocation&, GlyphIndex, const Requisition*, Allocation*
) {
    /* leave it as is */
}

NaturalLayout::NaturalLayout(const DimensionName d, Coord natural) {
    dimension_ = d;
    natural_ = natural;
}

NaturalLayout::~NaturalLayout() { }

void NaturalLayout::request(
    GlyphIndex, const Requisition*, Requisition& result
) {
    Requirement& r = result.requirement(dimension_);
    r.natural(natural_);
}

void NaturalLayout::allocate(
    const Allocation&, GlyphIndex, const Requisition*, Allocation*
) {
    /* leave it as is */
}

MarginLayout::MarginLayout(Coord margin) {
    lnatural_ = margin; lstretch_ = 0; lshrink_ = 0;
    rnatural_ = margin; rstretch_ = 0; rshrink_ = 0;
    bnatural_ = margin; bstretch_ = 0; bshrink_ = 0;
    tnatural_ = margin; tstretch_ = 0; tshrink_ = 0;
}

MarginLayout::MarginLayout(Coord hmargin, Coord vmargin) {
    lnatural_ = hmargin; lstretch_ = 0; lshrink_ = 0;
    rnatural_ = hmargin; rstretch_ = 0; rshrink_ = 0;
    bnatural_ = vmargin; bstretch_ = 0; bshrink_ = 0;
    tnatural_ = vmargin; tstretch_ = 0; tshrink_ = 0;
}

MarginLayout::MarginLayout(
    Coord lmargin, Coord rmargin, Coord bmargin, Coord tmargin
) {
    lnatural_ = lmargin; lstretch_ = 0; lshrink_ = 0;
    rnatural_ = rmargin; rstretch_ = 0; rshrink_ = 0;
    bnatural_ = bmargin; bstretch_ = 0; bshrink_ = 0;
    tnatural_ = tmargin; tstretch_ = 0; tshrink_ = 0;
}

MarginLayout::MarginLayout(
    Coord lmargin, Coord lstretch, Coord lshrink,
    Coord rmargin, Coord rstretch, Coord rshrink,
    Coord bmargin, Coord bstretch, Coord bshrink,
    Coord tmargin, Coord tstretch, Coord tshrink
) {
    lnatural_ = lmargin; lstretch_ = lstretch; lshrink_ = lshrink;
    rnatural_ = rmargin; rstretch_ = rstretch; rshrink_ = rshrink;
    bnatural_ = bmargin; bstretch_ = bstretch; bshrink_ = bshrink;
    tnatural_ = tmargin; tstretch_ = tstretch; tshrink_ = tshrink;
}

MarginLayout::~MarginLayout() { }

void MarginLayout::request(
    GlyphIndex, const Requisition*, Requisition& result
) {
    requisition_ = result;
    Requirement& x = requisition_.x_requirement();
    if (x.defined()) {
	x.natural(x.natural() + lnatural_ + rnatural_);
	x.stretch(x.stretch() + lstretch_ + rstretch_);
	x.shrink(x.shrink() + lshrink_ + rshrink_);
    }
    Requirement& y = requisition_.y_requirement();
    if (y.defined()) {
	y.natural(y.natural() + bnatural_ + tnatural_);
	y.stretch(y.stretch() + bstretch_ + tstretch_);
	y.shrink(y.shrink() + bshrink_ + tshrink_);
    }
    result = requisition_;
}

void MarginLayout::allocate(
    const Allocation&, GlyphIndex,
    const Requisition*, Allocation* result
) {
    Allotment& x = result[0].x_allotment();
    const Requirement& rx = requisition_.x_requirement();
    Coord lmargin = span(x.span(), rx, lnatural_, lstretch_, lshrink_);
    Coord rmargin = span(x.span(), rx, rnatural_, rstretch_, rshrink_);
    x.span(x.span() - (lmargin + rmargin));
    x.offset(Coord((1 - rx.alignment()) * lmargin - rx.alignment() * rmargin));

    Allotment& y = result[0].y_allotment();
    const Requirement& ry = requisition_.y_requirement();
    Coord bmargin = span(y.span(), ry, bnatural_, bstretch_, bshrink_);
    Coord tmargin = span(y.span(), ry, tnatural_, tstretch_, tshrink_);
    y.span(y.span() - (bmargin + tmargin));
    y.offset(Coord((1 - ry.alignment()) * bmargin - ry.alignment() * tmargin));
}

Coord MarginLayout::span(
    Coord span, const Requirement& total,
    Coord natural, Coord stretch, Coord shrink
) {
    Coord extra = span - total.natural();
    Coord result = natural;
    float ss = 0.0;
    Coord total_stretch = total.stretch();
    Coord total_shrink = total.shrink();
    if (extra > 0 && total_stretch > 0) {
        ss = stretch / total_stretch;
    } else if (extra < 0 && total_shrink > 0) {
        ss = shrink / total_shrink;
    }
    return result + Coord(ss * extra);
}
