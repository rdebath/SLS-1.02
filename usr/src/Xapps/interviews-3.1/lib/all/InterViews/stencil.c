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
 * Stencil - mask glyph
 */

#include <InterViews/bitmap.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/stencil.h>

Stencil::Stencil(const Bitmap* mask, const Color* c) : Glyph() {
    mask_ = mask;
    Resource::ref(mask_);
    color_ = c;
    Resource::ref(color_);
}

Stencil::~Stencil() {
    Resource::unref(mask_);
    Resource::unref(color_);
}

void Stencil::request(Requisition& requisition) const {
    if (mask_ != nil) {
        Coord left_bearing = mask_->left_bearing();
        Coord right_bearing = mask_->right_bearing();
        Coord ascent = mask_->ascent();
        Coord descent = mask_->descent();
	Requirement rx(
	    left_bearing, left_bearing, left_bearing,
	    right_bearing, right_bearing, right_bearing
	);
	Requirement ry(
	    descent, descent, descent,
	    ascent, ascent, ascent
	);
        requisition.require(Dimension_X, rx);
        requisition.require(Dimension_Y, ry);
    }
}

void Stencil::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    if (mask_ != nil) {
        Coord x = a.x();
        Coord y = a.y();
	ext.set_xy(
	    c, x - mask_->left_bearing(), y - mask_->descent(),
	    x + mask_->right_bearing(), y + mask_->ascent()
	);
    }
}

void Stencil::draw(Canvas* c, const Allocation& a) const {
    if (mask_ != nil) {
	c->stencil(mask_, color_, a.x(), a.y());
    }
}
