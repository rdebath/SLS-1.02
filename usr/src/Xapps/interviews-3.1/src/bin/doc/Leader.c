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
 * Leader - visible filler glyph
 */

#include "Leader.h"

#include <InterViews/printer.h>

Leader::Leader(
    DimensionName d,
    Coord natural, Coord stretch, Coord shrink, float alignment,
    Glyph* g
) : Glyph() {
    dimension_ = d;
    Requirement r(natural, stretch, shrink, alignment);
    requisition_.require(d, r);
    glyph_ = g;
    if (glyph_ != nil) {
        glyph_->ref();
    }
}

Leader::~Leader() {
    if (glyph_ != nil) {
        glyph_->unref();
    }
}

void Leader::request(Requisition& requisition) const {
    requisition = requisition_;
}

void Leader::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    ext.merge(c, a);
}

void Leader::draw(Canvas* c, const Allocation& given) const {
    if (glyph_ != nil) {
        Requisition r;
        Allocation a(given);
        Extension e;
        glyph_->request(r);
        Coord span = r.requirement(dimension_).natural();
        Coord begin = given.allotment(dimension_).begin();
        Coord end = given.allotment(dimension_).end();
        Coord origin = (int(begin / span) + 1) * span;
        Allotment part(origin, span, 0);
        while (origin + span < end) {
            part.origin(origin);
            a.allot(dimension_, part);
            glyph_->allocate(c, a, e);
            glyph_->draw(c, a);
            origin += span;
        }
    }
}

void Leader::print(Printer* p, const Allocation& given) const {
    if (glyph_ != nil) {
        Requisition r;
        Allocation a(given);
        Extension e;
        glyph_->request(r);
        Coord span = r.requirement(dimension_).natural();
        Coord begin = given.allotment(dimension_).begin();
        Coord end = given.allotment(dimension_).end();
        Coord origin = (int(begin / span) + 1) * span;
        Allotment part(origin, span, 0);
        while (origin + span < end) {
            part.origin(origin);
            a.allot(dimension_, part);
            glyph_->allocate(p, a, e);
            glyph_->print(p, a);
            origin += span;
        }
    }
}

HLeader::HLeader(Glyph* g) : Leader(Dimension_X, 0, fil, 0, 0.0, g) { }

HLeader::HLeader(
    Coord natural, Glyph* g
) : Leader(Dimension_X, natural, fil, 0, 0.0, g) { }

HLeader::HLeader(
    Coord natural, Coord stretch, Coord shrink, Glyph* g
) : Leader(Dimension_X, natural, stretch, shrink, 0.0, g) { }

HLeader::HLeader(
    Coord natural, Coord stretch, Coord shrink, float alignment, Glyph* g
) : Leader(Dimension_X, natural, stretch, shrink, alignment, g) { }

HLeader::~HLeader() { }

VLeader::VLeader(Glyph* g) : Leader(Dimension_Y, 0, fil, 0, 0.0, g) { }

VLeader::VLeader(
    Coord natural, Glyph* g
) : Leader(Dimension_Y, natural, fil, 0, 0.0, g) { }

VLeader::VLeader(
    Coord natural, Coord stretch, Coord shrink, Glyph* g
) : Leader(Dimension_Y, natural, stretch, shrink, 0.0, g) { }

VLeader::VLeader(
    Coord natural, Coord stretch, Coord shrink, float alignment, Glyph* g
) : Leader(Dimension_Y, natural, stretch, shrink, alignment, g) { }

VLeader::~VLeader() { }
