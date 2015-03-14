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
 * Character - character glyph with font
 */

#include <InterViews/canvas.h>
#include <InterViews/character.h>
#include <InterViews/color.h>
#include <InterViews/font.h>
#include <InterViews/hit.h>

Character::Character(long ch, const Font* f, const Color* c) : Glyph() {
    c_ = ch;
    font_ = f;
    Resource::ref(font_);
    color_ = c;
    Resource::ref(color_);
    if (font_ != nil) {
	FontBoundingBox b;
	font_->char_bbox(c_, b);
	left_bearing_ = b.left_bearing();
	right_bearing_ = b.right_bearing();
	width_ = b.width();
	ascent_ = b.font_ascent();
	descent_ = b.font_descent();
	height_ = ascent_ + descent_;
	alignment_ = (height_ == 0) ? 0 : descent_ / height_;
    } else {
	left_bearing_ = 0;
	right_bearing_ = 0;
	ascent_ = 0;
	descent_ = 0;
	width_ = 0;
	height_ = 0;
	alignment_ = 0;
    }
}

Character::~Character() {
    Resource::unref(font_);
    Resource::unref(color_);
}

long Character::code() const { return c_; }

void Character::request(Requisition& requisition) const {
    Requirement rx(width_, 0, 0, 0);
    Requirement ry(height_, 0, 0, alignment_);
    requisition.require(Dimension_X, rx);
    requisition.require(Dimension_Y, ry);
}

void Character::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    Coord x = a.x();
    Coord y = a.y();
    ext.set_xy(
	c, x - left_bearing_, y - descent_, x + right_bearing_, y + ascent_
    );
}

void Character::pick(Canvas*, const Allocation& a, int depth, Hit& h) {
    Coord x = h.left();
    Coord y = h.bottom();
    Coord left = a.left();
    Coord right = a.right();
    if (x >= left && x < right) {
        h.target(depth, this, (x > (left+right)/2) ? 1 : 0);
    }
}

void Character::draw(Canvas* c, const Allocation& a) const {
    c->character(font_, c_, a.right() - a.left(), color_, a.x(), a.y());
}
