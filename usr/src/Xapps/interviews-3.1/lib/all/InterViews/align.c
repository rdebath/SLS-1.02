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
 * Align - aligned layout
 */

#include <InterViews/align.h>
#include <OS/math.h>

Align::Align(DimensionName dimension) : Layout() {
    dimension_ = dimension;
}

Align::~Align() { }

void Align::request(
    GlyphIndex count, const Requisition* request, Requisition& result
) {
    Coord natural_lead = 0;
    Coord min_lead = -fil;
    Coord max_lead = fil;
    Coord natural_trail = 0;
    Coord min_trail = -fil;
    Coord max_trail = fil;
    for (int index = 0; index < count; ++index) {
        const Requirement& r = request[index].requirement(dimension_);
        if (r.defined()) {
	    Coord r_nat = r.natural();
	    Coord r_max = r_nat + r.stretch();
	    Coord r_min = r_nat - r.shrink();
	    Coord r_align = r.alignment();
	    Coord r_inv_align = 1.0 - r_align;
            natural_lead = Math::max(natural_lead, Coord(r_nat * r_align));
            max_lead = Math::min(max_lead, Coord(r_max * r_align));
            min_lead = Math::max(min_lead, Coord(r_min * r_align));
            natural_trail = Math::max(
                natural_trail, Coord(r_nat * r_inv_align)
            );
            max_trail = Math::min(max_trail, Coord(r_max * r_inv_align));
            min_trail = Math::max(min_trail, Coord(r_min * r_inv_align));
        }
    }
    Requirement r(
	natural_lead, max_lead, min_lead,
	natural_trail, max_trail, min_trail
    );
    result.require(dimension_, r);
}

void Align::allocate(
    const Allocation& given,
    GlyphIndex count, const Requisition* request, Allocation* result
) {
    const Allotment& g = given.allotment(dimension_);
    for (int index = 0; index < count; ++index) {
        const Requirement& r = request[index].requirement(dimension_);
        if (r.defined()) {
            Coord span = g.span();
            if (r.alignment() == 0) {
                span = Coord(float(span) * (1 - g.alignment()));
            } else if (r.alignment() == 1) {
                span = Coord(float(span) * g.alignment());
            } else {
                span = Coord(
                    float(span) * Math::min(
                        g.alignment()/r.alignment(),
                        (1 - g.alignment())/(1 - r.alignment())
                    )
                );
            }
            span = Math::min(span, r.natural() + r.stretch());
            span = Math::max(span, r.natural() - r.shrink());
	    Allotment a(g.origin(), span, r.alignment());
            result[index].allot(dimension_, a);
        } else {
            result[index].allot(dimension_, g);
        }
    }
}
