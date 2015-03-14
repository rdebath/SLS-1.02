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
#include <InterViews/color.h>
#include <InterViews/xymarker.h>
#include <OS/math.h>

static const Coord th = 1;
static const float e = 0.01;

XYMarker::XYMarker(
    Glyph* body, const Color* overlay, const Color* underlay
) : MonoGlyph(body) {
    overlay_ = overlay;
    Resource::ref(overlay_);
    underlay_ = underlay;
    Resource::ref(underlay_);
    marked_ = false;
    canvas_ = nil;
}

XYMarker::~XYMarker() {
    Resource::unref(overlay_);
    overlay_ = nil;
    Resource::unref(underlay_);
    underlay_ = nil;
}

static void do_draw(
    Canvas* c, const Color* color,
    Coord left, Coord bottom, Coord right, Coord top
) {
    Coord l = Math::min(left, right);
    Coord b = Math::min(bottom, top);
    Coord r = Math::max(left, right);
    Coord t = Math::max(bottom, top);

    if ((r - l) < 2*th || (t - b) < 2*th) {
        c->fill_rect(l - th, b - th, r + th, t + th, color);
    } else {
        c->fill_rect(l - th, b + th, l + th, t + th, color);
        c->fill_rect(l + th, t + th, r + th, t - th, color);
        c->fill_rect(r + th, t - th, r - th, b - th, color);
        c->fill_rect(r - th, b - th, l - th, b + th, color);
    }
}

static void do_damage(
    Canvas* canvas, Coord left, Coord bottom, Coord right, Coord top
) {
    Coord l = Math::min(left, right);
    Coord b = Math::min(bottom, top);
    Coord r = Math::max(left, right);
    Coord t = Math::max(bottom, top);

    if ((r - l) < 2*th || (t - b) < 2*th) {
        canvas->damage(l - th, b - th, r + th, t + th);
    } else {
        canvas->damage(l - th, b + th, l + th, t + th);
        canvas->damage(l + th, t + th, r + th, t - th);
        canvas->damage(r + th, t - th, r - th, b - th);
        canvas->damage(r - th, b - th, l - th, b + th);
    }
}

void XYMarker::unmark() {
    if (marked_) {
	if (canvas_ != nil) {
	    do_damage(canvas_, left_, bottom_, right_, top_);
	}
        marked_ = false;
    }
}

void XYMarker::mark(Coord left, Coord bottom, Coord right, Coord top) {
    if (canvas_ != nil) {
        if (!marked_) {
            do_damage(canvas_, left, bottom, right, top);
        } else if (
            left > right_ || right < left_ || top < bottom_ || bottom > top_
        ) {
            do_damage(canvas_, left, bottom, right, top);
            do_damage(canvas_, left_, bottom_, right_, top_);
        } else {
            if (!Math::equal(left_, left, e)) {
                do_damage(canvas_, left, bottom_, left_, top_);
            }
            if (!Math::equal(right_, right, e)) {
                do_damage(canvas_, right, bottom_, right_, top_);
            }
            if (!Math::equal(bottom_, bottom, e)) {
                do_damage(canvas_, left_, bottom, right_, bottom_);
            }
            if (!Math::equal(top_, top, e)) {
                do_damage(canvas_, left_, top, right_, top_);
            }
            if (!Math::equal(top_, top, e) && !Math::equal(left_, left, e)) {
                do_damage(canvas_, left, top, left_, top_);
            }
            if (!Math::equal(top_, top, e) && !Math::equal(right_, right, e)) {
                do_damage(canvas_, right, top, right_, top_);
            }
            if (!Math::equal(bottom_, bottom, e) &&
		!Math::equal(left_, left, e)
	    ) {
                do_damage(canvas_, left, bottom, left_, bottom_);
            }
            if (!Math::equal(bottom_, bottom, e) &&
		!Math::equal(right_, right, e)
	    ) {
                do_damage(canvas_, right, bottom, right_, bottom_);
            }
        }
    }
    left_ = left;
    bottom_ = bottom;
    right_ = right;
    top_ = top;
    marked_ = true;
}

void XYMarker::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    MonoGlyph::allocate(c, a, ext);
    ext.merge_xy(
	c, a.left() - th, a.bottom() - th, a.right() + th, a.top() + th
    );
    canvas_ = c;
}

void XYMarker::draw(Canvas* c, const Allocation& a) const {
    if (marked_ && underlay_ != nil) {
	do_draw(c, underlay_, left_, bottom_, right_, top_);
    }
    MonoGlyph::draw(c, a);
    if (marked_ && overlay_ != nil) {
	do_draw(c, overlay_, left_, bottom_, right_, top_);
    }
}

void XYMarker::undraw() {
    MonoGlyph::undraw();
    canvas_ = nil;
}
