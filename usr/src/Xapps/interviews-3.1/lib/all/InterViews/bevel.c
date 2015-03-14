/*
 * Copyright (c) 1991 Stanford University
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
 * Bevel - 3D framing of a glyph
 */

#include <IV-look/bevel.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/printer.h>
#include <InterViews/window.h>
#include <math.h>

BevelFrame::BevelFrame(
    Glyph* g, Coord t, float x, float y, boolean hmargin, boolean vmargin
) : MonoGlyph(g) {
    thickness_ = t;
    xalign_ = x;
    yalign_ = y;
    hmargin_ = hmargin;
    vmargin_ = vmargin;
}

BevelFrame::~BevelFrame() { }

void BevelFrame::request(Requisition& req) const {
    Glyph* g = body();
    if (g != nil) {
	g->request(req);
	if (hmargin_ || vmargin_) {
	    Coord t = thickness_ + thickness_;
	    Requirement& rx = req.x_requirement();
	    if (hmargin_ && rx.defined()) {
		rx.natural(rx.natural() + t);
	    }
	    Requirement& ry = req.y_requirement();
	    if (vmargin_ && ry.defined()) {
		ry.natural(ry.natural() + t);
	    }
	}
    }
}

void BevelFrame::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    Glyph* g = body();
    if (g != nil) {
	if (hmargin_ || vmargin_) {
	    Allocation interior(a);
	    allocate_body(g, thickness(c), interior);
	    g->allocate(c, interior, ext);
	} else {
	    g->allocate(c, a, ext);
	}
    }
    ext.merge(c, a);
}

void BevelFrame::draw(Canvas* c, const Allocation& a) const {
    Coord t = thickness(c);
    draw_frame(c, a, t);
    Glyph* g = body();
    if (g != nil) {
	if (hmargin_ || vmargin_) {
	    Allocation interior(a);
	    allocate_body(g, t, interior);
	    g->draw(c, interior);
	} else {
	    g->draw(c, a);
	}
    }
}

void BevelFrame::print(Printer* p, const Allocation& a) const {
    Coord t = thickness(p);
    draw_frame(p, a, t);
    Glyph* g = body();
    if (g != nil) {
	if (hmargin_ || vmargin_) {
	    Allocation interior(a);
	    allocate_body(g, t, interior);
	    g->print(p, interior);
	} else {
	    g->print(p, a);
	}
    }
}

void BevelFrame::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    Glyph* g = body();
    if (g != nil) {
	if (hmargin_ || vmargin_) {
	    Allocation interior(a);
	    allocate_body(g, thickness(c), interior);
	    g->pick(c, interior, depth, h);
	} else {
	    g->pick(c, a, depth, h);
	}
    }
}

void BevelFrame::draw_frame(Canvas*, const Allocation&, Coord) const { }

Coord BevelFrame::thickness(Canvas* c) const {
    return (c == nil) ? thickness_ : c->to_pixels_coord(thickness_);
}

void BevelFrame::allocate_body(Glyph* g, Coord t, Allocation& a) const {
    Requisition req;
    g->request(req);
    Coord h = hmargin_ ? t : 0, v = vmargin_ ? t : 0;
    Allotment& ax = a.x_allotment();
    Coord x_span = ax.span() - h - h;
    Coord x_offset = h;
    Coord x_align = ax.alignment();
    const Requirement& rx = req.x_requirement();
    if (rx.defined()) {
	Coord x_usable = rx.natural() + rx.stretch();
	if (x_span > x_usable) {
	    x_offset += xalign_ * (x_span - x_usable);
	    x_span = x_usable;
	}
    }
    ax.span(x_span);
    ax.offset(x_offset * (1 - x_align - x_align));

    Allotment& ay = a.y_allotment();
    Coord y_span = ay.span() - v - v;
    Coord y_offset = v;
    Coord y_align = ay.alignment();
    const Requirement& ry = req.y_requirement();
    if (ry.defined()) {
	Coord y_usable = ry.natural() + ry.stretch();
	if (y_span > y_usable) {
	    y_offset += yalign_ * (y_span - y_usable);
	    y_span = y_usable;
	}
    }
    ay.span(y_span);
    ay.offset(y_offset * (1 - y_align - y_align));
}

Bevel::Bevel(
    Glyph* g, Beveler b,
    const Color* light, const Color* medium, const Color* dark,
    Coord thickness, float x, float y, boolean hmargin, boolean vmargin
) : BevelFrame(g, thickness, x, y, hmargin, vmargin) {
    beveler_ = b;
    light_ = light;
    medium_ = medium;
    dark_ = dark;
    Resource::ref(light_);
    Resource::ref(medium_);
    Resource::ref(dark_);
}

Bevel::~Bevel() {
    Resource::unref(light_);
    Resource::unref(medium_);
    Resource::unref(dark_);
}

void Bevel::draw_frame(Canvas* c, const Allocation& a, Coord t) const {
    (*beveler_)(
	c, light_, medium_, dark_, t, a.left(), a.bottom(), a.right(), a.top()
    );
}

void Bevel::rect(
    Canvas* c, const Color* light, const Color* medium, const Color* dark,
    Coord thickness, Coord left, Coord bottom, Coord right, Coord top
) {
    if (medium != nil) {
	/* background */
	c->fill_rect(left, bottom, right, top, medium);
    }

    Coord left_inside = left + thickness;
    Coord bottom_inside = bottom + thickness;
    Coord right_inside = right - thickness;
    Coord top_inside = top - thickness;

    /* left edge */
    c->new_path();
    c->move_to(left, bottom);
    c->line_to(left, top);
    c->line_to(left_inside, top);
    c->line_to(left_inside, bottom);
    c->close_path();
    c->fill(light);

    /* top edge */
    c->new_path();
    c->move_to(left_inside, top_inside);
    c->line_to(left_inside, top);
    c->line_to(right, top);
    c->line_to(right, top_inside);
    c->close_path();
    c->fill(light);

    /* right edge */
    c->new_path();
    c->move_to(right_inside, bottom_inside);
    c->line_to(right_inside, top_inside);
    c->line_to(right, top);
    c->line_to(right, bottom);

    /* bottom edge (as part of same path) */
    c->line_to(left, bottom);
    c->line_to(left_inside, bottom_inside);
    c->line_to(right_inside, bottom_inside);
    c->close_path();
    c->fill(dark);
}

/*
 * A bit of a misnomer to call these arrows; they are really beveled
 * triangles.  The only tricky part is dropping the bevel down and to the
 * right of the original triangle, which requires finding delta x,y for
 * a given thickness and matching the slope of the triangle.
 */

void Bevel::left_arrow(
    Canvas* c, const Color* light, const Color* medium, const Color* dark,
    Coord thickness, Coord left, Coord bottom, Coord right, Coord top
) {
    Coord center_y = (bottom + top) * 0.5;
    float slope = ((top - bottom) / (right - left)) * 0.5;
    float delta_x = thickness / sqrt(slope * slope + 1);
    float delta_y = slope * delta_x;

    c->new_path();
    c->move_to(left, center_y);
    c->line_to(right, top);
    c->line_to(right, bottom);
    c->close_path();
    c->fill(medium);

    c->new_path();
    c->move_to(right - thickness, bottom + thickness);
    c->line_to(right - thickness, top - thickness);
    c->line_to(right, top);
    c->line_to(right, bottom);
    c->close_path();
    c->fill(dark);

    c->new_path();
    c->move_to(left, center_y);
    c->line_to(left + thickness, center_y);
    c->line_to(right - thickness, bottom + thickness);
    c->line_to(right, bottom);
    c->close_path();
    c->fill(dark);

    c->new_path();
    c->move_to(left + delta_x, center_y - delta_y);
    c->line_to(left, center_y);
    c->line_to(right, top);
    c->line_to(right, top - thickness);
    c->close_path();
    c->fill(light);
}

void Bevel::right_arrow(
    Canvas* c, const Color* light, const Color* medium, const Color* dark,
    Coord thickness, Coord left, Coord bottom, Coord right, Coord top
) {
    Coord center_y = (bottom + top) * 0.5;
    float slope = ((top - bottom) / (right - left)) * 0.5;
    float delta_x = thickness / sqrt(slope * slope + 1);
    float delta_y = slope * delta_x;

    c->new_path();
    c->move_to(left, bottom);
    c->line_to(left, top);
    c->line_to(right, center_y);
    c->close_path();
    c->fill(medium);

    c->new_path();
    c->move_to(left, bottom);
    c->line_to(left + thickness, bottom + thickness);
    c->line_to(right - thickness, center_y);
    c->line_to(right, center_y);
    c->close_path();
    c->fill(dark);

    c->new_path();
    c->move_to(left, bottom);
    c->line_to(left, top);
    c->line_to(left + thickness, top - thickness);
    c->line_to(left + delta_x, bottom + delta_y);
    c->close_path();
    c->fill(light);

    c->new_path();
    c->move_to(left + thickness, top - thickness);
    c->line_to(left, top);
    c->line_to(right, center_y);
    c->line_to(right - delta_x, center_y - delta_y);
    c->close_path();
    c->fill(light);
}

void Bevel::up_arrow(
    Canvas* c, const Color* light, const Color* medium, const Color* dark,
    Coord thickness, Coord left, Coord bottom, Coord right, Coord top
) {
    Coord center_x = (left + right) * 0.5;
    float slope = 2 * ((top - bottom) / (right - left));
    float delta_x = thickness / sqrt(slope * slope + 1);
    float delta_y = slope * delta_x;

    c->new_path();
    c->move_to(left, bottom);
    c->line_to(center_x, top);
    c->line_to(right, bottom);
    c->close_path();
    c->fill(medium);

    c->new_path();
    c->move_to(left, bottom);
    c->line_to(right, bottom);
    c->line_to(right - thickness, bottom + thickness);
    c->line_to(left + thickness, bottom + thickness);
    c->close_path();
    c->fill(dark);

    c->new_path();
    c->move_to(center_x, top - thickness);
    c->line_to(center_x, top);
    c->line_to(right, bottom);
    c->line_to(right - thickness, bottom + thickness);
    c->close_path();
    c->fill(dark);

    c->new_path();
    c->move_to(left, bottom);
    c->line_to(left + delta_x, bottom);
    c->line_to(center_x + delta_x, top - delta_y);
    c->line_to(center_x, top);
    c->close_path();
    c->fill(light);
}

void Bevel::down_arrow(
    Canvas* c, const Color* light, const Color* medium, const Color* dark,
    Coord thickness, Coord left, Coord bottom, Coord right, Coord top
) {
    Coord center_x = (left + right) * 0.5;
    float slope = 2* ((top - bottom) / (right - left));
    float delta_x = thickness / sqrt(slope * slope + 1);
    float delta_y = slope * delta_x;

    c->new_path();
    c->move_to(left, top);
    c->line_to(right, top);
    c->line_to(center_x, bottom);
    c->close_path();
    c->fill(medium);

    c->new_path();
    c->move_to(center_x, bottom);
    c->line_to(center_x, bottom + thickness);
    c->line_to(right - thickness, top - thickness);
    c->line_to(right, top);
    c->close_path();
    c->fill(dark);

    c->new_path();
    c->move_to(left, top);
    c->line_to(center_x, bottom);
    c->line_to(center_x + delta_x, bottom + delta_y);
    c->line_to(left + thickness, top - thickness);
    c->close_path();
    c->fill(light);

    c->new_path();
    c->move_to(left, top);
    c->line_to(right, top);
    c->line_to(right - delta_x, top - delta_y);
    c->line_to(left + thickness, top - thickness);
    c->close_path();
    c->fill(light);
}

void Bevel::diamond(
    Canvas* c, const Color* light, const Color* medium, const Color* dark,
    Coord thickness, Coord left, Coord bottom, Coord right, Coord top
) {
    Coord x_mid = (left + right) * 0.5;
    Coord y_mid = (bottom + top) * 0.5;
    Coord left_inside = left + thickness;
    Coord top_inside = top - thickness;
    Coord right_inside = right - thickness;
    Coord bottom_inside = bottom + thickness;

    /* interior of diamond */
    c->new_path();
    c->move_to(left, y_mid);
    c->line_to(x_mid, top);
    c->line_to(right, y_mid);
    c->line_to(x_mid, bottom);
    c->close_path();
    c->fill(medium);

    /* lower half */
    c->new_path();
    c->move_to(left, y_mid);
    c->line_to(x_mid, bottom);
    c->line_to(right, y_mid);
    c->line_to(right_inside, y_mid);
    c->line_to(x_mid, bottom_inside);
    c->line_to(left_inside, y_mid);
    c->close_path();
    c->fill(dark);

    /* upper half */
    c->new_path();
    c->move_to(left, y_mid);
    c->line_to(x_mid, top);
    c->line_to(right, y_mid);
    c->line_to(right_inside, y_mid);
    c->line_to(x_mid, top_inside);
    c->line_to(left_inside, y_mid);
    c->close_path();
    c->fill(light);
}
