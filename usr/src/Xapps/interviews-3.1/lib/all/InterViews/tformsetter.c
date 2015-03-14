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
 * TransformSetter
 */

#include <InterViews/canvas.h>
#include <InterViews/hit.h>
#include <InterViews/printer.h>
#include <InterViews/tformsetter.h>
#include <OS/math.h>

TransformSetter::TransformSetter(Glyph* g) : MonoGlyph(g) { }

TransformSetter::TransformSetter(
    Glyph* g, const Transformer& tx
) : MonoGlyph(g) {
    transformer_ = tx;
}

TransformSetter::~TransformSetter() { }

const Transformer& TransformSetter::transformer() const {
    return transformer_;
}

Transformer& TransformSetter::transformer() {
    return transformer_;
}

void TransformSetter::transformer(const Transformer& tx) {
    transformer_ = tx;
}

static void compute_req(Requirement& r, Coord first, Coord last) {
    Coord natural = last - first;
    r.natural(natural);
    r.stretch(0.0);
    r.shrink(0.0);
    if (Math::equal(natural, float(0), float(1e-3))) {
	r.alignment(0.0);
    } else {
	r.alignment(-first / natural);
    }
}

void TransformSetter::request(Requisition& req) const {
    TransformSetter* t = (TransformSetter*)this;
    MonoGlyph::request(req);
    Allocation& a = t->natural_allocation_;

    Requirement& rx = req.x_requirement();
    Allotment& ax = a.x_allotment();
    ax.origin(0.0);
    ax.span(rx.natural());
    ax.alignment(rx.alignment());

    Requirement& ry = req.y_requirement();
    Allotment& ay = a.y_allotment();
    ay.origin(0.0);
    ay.span(ry.natural());
    ay.alignment(ry.alignment());

    const Transformer& tx = transformer_;
    Coord left = ax.begin(), bottom = ay.begin();
    Coord right = ax.end(), top = ay.end();
    Coord x1, y1, x2, y2, x3, y3, x4, y4;
    tx.transform(left, bottom, x1, y1);
    tx.transform(left, top, x2, y2);
    tx.transform(right, top, x3, y3);
    tx.transform(right, bottom, x4, y4);
    left = Math::min(x1, x2, x3, x4);
    bottom = Math::min(y1, y2, y3, y4);
    right = Math::max(x1, x2, x3, x4);
    top = Math::max(y1, y2, y3, y4);

    compute_req(rx, left, right);
    compute_req(ry, bottom, top);
}

void TransformSetter::allocate(
    Canvas* c, const Allocation& a, Extension& ext
) {
    /*
     * Shouldn't need to test for nil canvas, but some old
     * applications (notably doc) pass nil as a canvas
     * when doing certain kinds of allocation.
     */
    if (c != nil) {
	push_transform(c, a, natural_allocation_);
	MonoGlyph::allocate(c, natural_allocation_, ext);
	c->pop_transform();
    }
}

void TransformSetter::draw(Canvas* c, const Allocation& a) const {
    push_transform(c, a, natural_allocation_);
    MonoGlyph::draw(c, natural_allocation_);
    c->pop_transform();
}

void TransformSetter::print(Printer* p, const Allocation& a) const {
    push_transform(p, a, natural_allocation_);
    MonoGlyph::print(p, natural_allocation_);
    p->pop_transform();
}

void TransformSetter::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    Transformer t(transformer_);
    transform(t, a, natural_allocation_);
    c->push_transform();
    c->transform(t);
    h.push_transform();
    h.transform(t);
    MonoGlyph::pick(c, natural_allocation_, depth, h);
    c->pop_transform();
    h.pop_transform();
}

void TransformSetter::push_transform(
    Canvas* c, const Allocation& a, const Allocation& natural
) const {
    Transformer t(transformer_);
    transform(t, a, natural);
    c->push_transform();
    c->transform(t);
}

void TransformSetter::transform(
    Transformer& t, const Allocation& a, const Allocation&
) const {
    t.translate(a.x(), a.y());
}

/* class TransformFitter */

TransformFitter::TransformFitter(Glyph* g) : TransformSetter(g) { }
TransformFitter::~TransformFitter() { }

void TransformFitter::transform(
    Transformer& t, const Allocation& a, const Allocation& natural
) const {
    const Allotment& natural_x = natural.x_allotment();
    const Allotment& natural_y = natural.y_allotment();
    if (!Math::equal(natural_x.span(), Coord(0), float(1e-2)) &&
	!Math::equal(natural_y.span(), Coord(0), float(1e-2))
    ) {
	const Allotment& ax = a.x_allotment();
	const Allotment& ay = a.y_allotment();
	t.scale(
	    a.x_allotment().span() / natural_x.span(),
	    a.y_allotment().span() / natural_y.span()
	);
    }
    t.translate(a.x(), a.y());
}
