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
 * Rule - visible filler
 */

#include <InterViews/rule.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>

Rule::Rule(DimensionName d, const Color* c, Coord t) : Glyph() {
    dimension_ = d;
    color_ = c;
    Resource::ref(color_);
    thickness_ = t;
}

Rule::~Rule() {
    Resource::unref(color_);
}

void Rule::request(Requisition& req) const {
    Requirement r(thickness_, 0, 0, 0);
    req.require(dimension_, r);
}

void Rule::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    ext.set(c, a);
}

void Rule::draw(Canvas* c, const Allocation& a) const {
    c->fill_rect(a.left(), a.bottom(), a.right(), a.top(), color_);
}

HRule::HRule(const Color* c, Coord t) : Rule(Dimension_Y, c, t) { }
HRule::~HRule() { }

VRule::VRule(const Color* c, Coord t) : Rule(Dimension_X, c, t) { }
VRule::~VRule() { }
