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
 * LRMarker - mark text region
 */

#include <InterViews/color.h>
#include <InterViews/canvas.h>
#include <InterViews/lrmarker.h>
#include <OS/math.h>

LRMarker::LRMarker(
    Glyph* body, const Color* overlay, const Color* underlay
) : MonoGlyph(body) {
    overlay_ = overlay;
    Resource::ref(overlay_);
    underlay_ = underlay;
    Resource::ref(underlay_);
    marked_ = false;
    bounded_ = false;
    canvas_ = nil;
}

LRMarker::~LRMarker() {
    Resource::unref(overlay_);
    Resource::unref(underlay_);
    canvas_ = nil;
}

void LRMarker::unmark() {
    if (marked_) {
        mark(x1_, y1_, h1_, x1_, y1_, h1_);
        marked_ = false;
    }
}

static void do_draw(
    Canvas* canvas, const Color* color,
    Coord left, Coord right,
    Coord x1, Coord b1, Coord t1, Coord x2, Coord b2, Coord t2
) {
    if (x1 < x2) {
        canvas->fill_rect(left, b1, x1, b2, color);
        canvas->fill_rect(x2, t1, right, t2, color);
        canvas->fill_rect(x1, b2, x2, t1, color);
    } else if (x1 > x2) {
        canvas->fill_rect(left, b1, x2, b2, color);
        canvas->fill_rect(x1, t1, right, t2, color);
        canvas->fill_rect(x1, b1, x2, t2, color);
    } else {
        canvas->fill_rect(left, b1, x1, b2, color);
        canvas->fill_rect(x2, t1, right, t2, color);
    }
}

static void do_damage(
    Canvas* c, Coord left, Coord right,
    Coord x1, Coord b1, Coord t1, Coord x2, Coord b2, Coord t2
) {
    if (x1 < x2) {
        if (b1 != b2) {
	    c->damage(left, Math::min(b1, b2), x1, Math::max(b1, b2));
	}
        if (t1 != t2) {
	    c->damage(x2, Math::min(t1, t2), right, Math::max(t1, t2));
	}
        if (b2 != t1) {
	    c->damage(x1, Math::min(b2, t1), x2, Math::max(b2, t1));
	}
    } else if (x1 > x2) {
        if (b1 != b2) {
	    c->damage(left, Math::min(b1, b2), x2, Math::max(b1, b2));
	}
        if (t1 != t2) {
	    c->damage(x1, Math::min(t1, t2), right, Math::max(t1, t2));
	}
        if (b1 != t2) {
	    c->damage(x2, Math::min(t2, b1), x1, Math::max(t2, b1));
	}
    } else {
        if (b1 != b2) {
	    c->damage(left, Math::min(b1, b2), x2, Math::max(b1, b2));
	}
        if (t1 != t2) {
	    c->damage(x2, Math::min(t1, t2), right, Math::max(t1, t2));
	}
    }
}

void LRMarker::bound(Coord left, Coord bottom, Coord right, Coord top) {
    if (canvas_ != nil) {
        if (!bounded_) {
            canvas_->damage(left, bottom, right, top);
        } else {
            Coord lx = Math::max(left_, left);
            Coord ln = Math::min(left_, left);
            Coord bx = Math::max(bottom_, bottom);
            Coord bn = Math::min(bottom_, bottom);
            Coord rx = Math::max(right_, right);
            Coord rn = Math::min(right_, right);
            Coord tx = Math::max(top_, top);
            Coord tn = Math::min(top_, top);
            if (ln != lx) {
		canvas_->damage(ln, bn, lx, tn);
	    }
            if (tn != tx) {
		canvas_->damage(ln, tn, rn, tx);
	    }
            if (rn != rx) {
		canvas_->damage(rn, bx, rx, tx);
	    }
            if (bn != bx) {
		canvas_->damage(lx, bn, rx, bx);
	    }
        }
    }
    left_ = left;
    bottom_ = bottom;
    right_ = right;
    top_ = top;
    bounded_ = true;
}

void LRMarker::mark(
    Coord x1, Coord y1, Coord h1, Coord x2, Coord y2, Coord h2
) {
    if (canvas_ !=  nil) {
        if (!marked_) {
            do_damage(
                canvas_,
                left_, right_, x1, y1, y1 + h1, x2, y2, y2 + h2
            );
        } else if (
            (y2 > y1_ || y2 == y1_ && x2 < x1_)
            || (y1 < y2_ || y1 == y2_ && x1 > x2_)
        ) {
            do_damage(
                canvas_,
                left_, right_, x1_, y1_, y1_ + h1_, x2_, y2_, y2_ + h2_
            );
            do_damage(
                canvas_,
                left_, right_, x1, y1, y1 + h1, x2, y2, y2 + h2
            );
        } else {
            do_damage(
                canvas_,
                left_, right_, x1_, y1_, y1_ + h1_, x1, y1, y1 + h1
            );
            do_damage(
                canvas_,
                left_, right_, x2_, y2_, y2_ + h2_, x2, y2, y2 + h2
            );
        }
    }
    x1_ = x1;
    y1_ = y1;
    h1_ = h1;
    x2_ = x2;
    y2_ = y2;
    h2_ = h2;
    marked_ = true;
}

void LRMarker::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    MonoGlyph::allocate(c, a, ext);
    ext.merge(c, a);
    canvas_ = c;
}

void LRMarker::draw(Canvas* c, const Allocation& a) const {
    if (c != nil) {
        if (marked_ && underlay_ != nil) {
            do_draw(
                c, underlay_,
                left_, right_, x1_, y1_, y1_ + h1_, x2_, y2_, y2_ + h2_
            );
        }
    }
    MonoGlyph::draw(c, a);
    if (c != nil) {
        if (marked_ && overlay_ != nil) {
            do_draw(
                c, overlay_,
                left_, right_, x1_, y1_, y1_ + h1_, x2_, y2_, y2_ + h2_
            );
        }
    }
}

void LRMarker::undraw() {
    MonoGlyph::undraw();
    canvas_ = nil;
}
