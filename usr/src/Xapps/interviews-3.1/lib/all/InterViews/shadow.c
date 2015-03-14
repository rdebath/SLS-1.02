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
 * Shadow - a shadowing Glyph
 */

#include <InterViews/shadow.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/printer.h>
#include <OS/math.h>

Shadow::Shadow(
    Glyph* g, Coord x, Coord y, const Color* c, boolean single
) : MonoGlyph(g) {
    x_offset_ = x;
    y_offset_ = y;
    color_ = c;
    Resource::ref(color_);
    single_ = single;
}

Shadow::~Shadow() {
    Resource::unref(color_);
}

void Shadow::request(Requisition& req) const {
    MonoGlyph::request(req);
    compute_requirement(req.x_requirement(), x_offset_);
    compute_requirement(req.y_requirement(), y_offset_);
}

void Shadow::compute_requirement(Requirement& r, Coord offset) const {
    if (r.defined()) {
	Coord n = r.natural();
	if (offset > 0) {
	    r.natural(n + offset);
	    r.alignment(r.alignment() * n / r.natural());
	} else {
	    r.natural(n - offset);
	    r.alignment((r.alignment() * n - offset) / r.natural());
	}
    }
}

void Shadow::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    Allocation b(a);
    compute_allocation(b);
    MonoGlyph::allocate(c, b, ext);
    ext.merge(c, a);
}

void Shadow::draw(Canvas* c, const Allocation& a) const {
    Allocation b(a);
    compute_allocation(b);
    if (!single_) {
	draw_shadow(c, b);
	draw_body(c, b);
	return;
    }

    Coord b_left = b.left(), s_left = b_left + x_offset_;
    Coord b_bottom = b.bottom(), s_bottom = b_bottom + y_offset_;
    Coord b_right = b.right(), s_right = b_right + x_offset_;
    Coord b_top = b.top(), s_top = b_top + y_offset_;
    Coord x1, y1, x2, y2, x3, y3, x4, y4;
    if (x_offset_ > 0) {
	x1 = s_left; x2 = s_right; x3 = b_right; x4 = s_right;
    } else {
	x1 = s_left; x2 = s_right; x3 = s_left; x4 = b_left;
    }
    if (y_offset_ > 0) {
	y1 = b_top; y2 = s_top; y3 = s_bottom; y4 = b_top;
    } else {
	y1 = s_bottom; y2 = b_bottom; y3 = b_bottom; y4 = s_top;
    }
    Extension e1, e2;
    e1.set_xy(c, x1, y1, x2, y2);
    e2.set_xy(c, x3, y3, x4, y4);
    if (!c->damaged(e1) && !c->damaged(e2)) {
	draw_body(c, b);
	return;
    }
    c->front_buffer();
    draw_shadow(c, b);
    c->back_buffer();
    Extension ext;
    ext.set(c, b);
    c->restrict_damage(ext);
    draw_body(c, b);
}

void Shadow::draw_shadow(Canvas* c, const Allocation& a) const {
    c->fill_rect(
	a.left() + x_offset_, a.bottom() + y_offset_,
	a.right() + x_offset_, a.top() + y_offset_,
	color_
    );
}

void Shadow::draw_body(Canvas* c, const Allocation& a) const {
    MonoGlyph::draw(c, a);
}

void Shadow::print(Printer* p, const Allocation& a) const {
    Allocation b(a);
    compute_allocation(b);
    p->fill_rect(
	b.left() + x_offset_, b.bottom() + y_offset_,
	b.right() + x_offset_, b.top() + y_offset_,
	color_
    );
    MonoGlyph::print(p, b);
}

void Shadow::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    Allocation b(a);
    compute_allocation(b);
    MonoGlyph::pick(c, b, depth, h);
}

void Shadow::compute_allocation(Allocation& a) const {
    compute_allotment(a.x_allotment(), x_offset_);
    compute_allotment(a.y_allotment(), y_offset_);
}

void Shadow::compute_allotment(Allotment& a, Coord offset) const {
    Coord n = a.span();
    if (offset > 0) {
	a.span(n - offset);
	a.alignment(a.alignment() * n / a.span());
    } else {
	a.span(n + offset);
	a.alignment((a.alignment() * n + offset) / a.span());
    }
}
