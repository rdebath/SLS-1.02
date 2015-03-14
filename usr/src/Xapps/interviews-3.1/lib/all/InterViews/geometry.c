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

#include <InterViews/canvas.h>
#include <InterViews/compositor.h>
#include <InterViews/geometry.h>
#include <InterViews/glyph.h>
#include <InterViews/transformer.h>
#include <OS/math.h>

Requirement::Requirement(
    Coord natural_lead, Coord max_lead, Coord min_lead,
    Coord natural_trail, Coord max_trail, Coord min_trail
) {
    natural_lead = Math::max(min_lead, Math::min(max_lead, natural_lead));
    max_lead = Math::max(max_lead, natural_lead);
    min_lead = Math::min(min_lead, natural_lead);
    natural_trail = Math::max(min_trail, Math::min(max_trail, natural_trail));
    max_trail = Math::max(max_trail, natural_trail);
    min_trail = Math::min(min_trail, natural_trail);
    natural_ = natural_lead + natural_trail;
    if (natural_lead == 0) {
        shrink_ = natural_trail - min_trail;
        stretch_ = max_trail - natural_trail;
        alignment_ = 0;
    } else if (natural_trail == 0) {
        shrink_ = natural_lead - min_lead;
        stretch_ = max_lead - natural_lead;
        alignment_ = 1;
    } else {
        float fshrink = Math::max(
            float(min_lead)/float(natural_lead),
            float(min_trail)/float(natural_trail)
        );
        shrink_ = Coord(natural_ * (1 - fshrink));
        float fstretch = Math::min(
            float(max_lead)/float(natural_lead),
            float(max_trail)/float(natural_trail)
        );
        stretch_ = Coord(natural_ * (fstretch - 1));
        if (natural_ == 0) {
            alignment_ = 0;
        } else {
            alignment_ = float(natural_lead) / float(natural_);
        }
    }
}

boolean Requirement::equals(const Requirement& r, float epsilon) const {
    if (!Math::equal(natural_, r.natural_, epsilon)) {
        return false;
    } else if (!Math::equal(stretch_, r.stretch_, epsilon)) {
        return false;
    } else if (!Math::equal(shrink_, r.shrink_, epsilon)) {
        return false;
    } else if (!Math::equal(alignment_, r.alignment_, epsilon)) {
        return false;
    } else {
        return true;
    }
}

/* hack to persuade cfront to inline both subobject constructors */

Requisition::Requisition() : x_(), y_(-fil) {
    penalty_ = PenaltyBad;
}

Requisition::Requisition(const Requisition& r) : x_(r.x_), y_(r.y_) {
    penalty_ = r.penalty_;
}

boolean Requisition::equals(const Requisition& r, float epsilon) const {
    return x_.equals(r.x_, epsilon) && y_.equals(r.y_, epsilon);
}

void Requisition::require(DimensionName n, const Requirement& r) {
    switch (n) {
    case Dimension_X:
        x_ = r;
        break;
    case Dimension_Y:
        y_ = r;
        break;
    }
}

static Requirement* empty_requirement;

Requirement& Requisition::requirement(DimensionName n) {
    switch (n) {
    case Dimension_X:
        return x_;
    case Dimension_Y:
        return y_;
    }
    if (empty_requirement == nil) {
	empty_requirement = new Requirement;
    }
    return *empty_requirement;
}

const Requirement& Requisition::requirement(DimensionName n) const {
    switch (n) {
    case Dimension_X:
        return x_;
    case Dimension_Y:
        return y_;
    }
    if (empty_requirement == nil) {
	empty_requirement = new Requirement;
    }
    return *empty_requirement;
}

boolean Allotment::equals(const Allotment& a, float epsilon) const {
    if (!Math::equal(origin_, a.origin_, epsilon)) {
        return false;
    } else if (!Math::equal(span_, a.span_, epsilon)) {
        return false;
    } else if (!Math::equal(alignment_, a.alignment_, epsilon)) {
        return false;
    }
    return true;
}

/* hack to persuade cfront to inline both subobject constructors */

Allocation::Allocation() : x_(), y_(0, 0, 0) { }

Allocation::Allocation(const Allocation& a) : x_(a.x_), y_(a.y_) { }

void Allocation::allot(DimensionName n, const Allotment& a) {
    if (n == Dimension_X) {
	x_ = a;
    } else if (n == Dimension_Y) {
	y_ = a;
    }
}

static Allotment* empty_allotment;

Allotment& Allocation::allotment(DimensionName n) {
    if (n == Dimension_X) {
	return x_;
    } else if (n == Dimension_Y) {
	return y_;
    }
    if (empty_allotment == nil) {
	empty_allotment = new Allotment;
    }
    return *empty_allotment;
}

const Allotment& Allocation::allotment(DimensionName n) const {
    if (n == Dimension_X) {
	return x_;
    } else if (n == Dimension_Y) {
	return y_;
    }
    if (empty_allotment == nil) {
	empty_allotment = new Allotment;
    }
    return *empty_allotment;
}

boolean Allocation::equals(const Allocation& a, float epsilon) const {
    return x_.equals(a.x_, epsilon) && y_.equals(a.y_, epsilon);
}

Extension::Extension() {
    const Coord zero = 0;
    x_begin_ = zero;
    x_end_ = zero;
    y_begin_ = zero;
    y_end_ = zero;
}

Extension::Extension(const Extension& ext) {
    x_begin_ = ext.x_begin_;
    x_end_ = ext.x_end_;
    y_begin_ = ext.y_begin_;
    y_end_ = ext.y_end_;
}

void Extension::operator =(const Extension& ext) {
    x_begin_ = ext.x_begin_;
    x_end_ = ext.x_end_;
    y_begin_ = ext.y_begin_;
    y_end_ = ext.y_end_;
}

void Extension::transform_xy(
    Canvas* c, Coord& left, Coord& bottom, Coord& right, Coord& top
) {
    if (c != nil) {
	const Transformer& t = c->transformer();
	if (!t.identity()) {
	    Coord x1, y1, x2, y2, x3, y3, x4, y4;
	    t.transform(left, bottom, x1, y1);
	    t.transform(left, top, x2, y2);
	    t.transform(right, top, x3, y3);
	    t.transform(right, bottom, x4, y4);
	    left = Math::min(x1, x2, x3, x4);
	    bottom = Math::min(y1, y2, y3, y4);
	    right = Math::max(x1, x2, x3, x4);
	    top = Math::max(y1, y2, y3, y4);
	}
    }
}

void Extension::set(Canvas* c, const Allocation& a) {
    set_xy(c, a.left(), a.bottom(), a.right(), a.top());
}

void Extension::set_xy(Canvas* c, Coord l, Coord b, Coord r, Coord t) {
    Coord left = l;
    Coord bottom = b;
    Coord right = r;
    Coord top = t;
    transform_xy(c, left, bottom, right, top);
    x_begin_ = left;
    x_end_ = right;
    y_begin_ = bottom;
    y_end_ = top;
}

void Extension::clear() {
    x_begin_ = fil;
    x_end_ = -fil;
    y_begin_ = fil;
    y_end_ = -fil;
}

void Extension::merge(const Extension& ext) {
    x_begin_ = Math::min(x_begin_, ext.x_begin_);
    x_end_ = Math::max(x_end_, ext.x_end_);
    y_begin_ = Math::min(y_begin_, ext.y_begin_);
    y_end_ = Math::max(y_end_, ext.y_end_);
}

void Extension::merge(Canvas* c, const Allocation& a) {
    merge_xy(c, a.left(), a.bottom(), a.right(), a.top());
}

void Extension::merge_xy(Canvas* c, Coord l, Coord b, Coord r, Coord t) {
    Coord left = l;
    Coord bottom = b;
    Coord right = r;
    Coord top = t;
    transform_xy(c, left, bottom, right, top);
    x_begin_ = Math::min(x_begin_, left);
    x_end_ = Math::max(x_end_, right);
    y_begin_ = Math::min(y_begin_, bottom);
    y_end_ = Math::max(y_end_, top);
}
