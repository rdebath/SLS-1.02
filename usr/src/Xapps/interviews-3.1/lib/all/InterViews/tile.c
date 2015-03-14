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

#include <InterViews/tile.h>
#include <OS/math.h>

static void compute_tile_request(
    DimensionName d, float a, GlyphIndex count, const Requisition* request,
    Requisition& result
) {
    Coord natural = 0, min_size = 0, max_size = 0;
    for (GlyphIndex i = 0; i < count; i++) {
        const Requirement& r = request[i].requirement(d);
        if (r.defined()) {
	    Coord n = r.natural();
	    natural += n;
	    max_size += n + r.stretch();
	    min_size += n - r.shrink();
	}
    }
    Requirement& nr = result.requirement(d);
    nr.natural(natural);
    nr.stretch(max_size - natural);
    nr.shrink(natural - min_size);
    nr.alignment(a);
}

Tile::Tile(DimensionName d) : Layout() { dimension_ = d; }
Tile::~Tile() { }

void Tile::request(
    GlyphIndex count, const Requisition* request, Requisition& result
) {
    compute_tile_request(dimension_, 0.0, count, request, result);
    requisition_ = result;
}

void Tile::allocate(
    const Allocation& given,
    GlyphIndex count, const Requisition* request, Allocation* result
) {
    const Allotment& g = given.allotment(dimension_);
    Requirement& r = requisition_.requirement(dimension_);
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
    Coord natural = r.natural();
    boolean growing = span > natural;
    boolean shrinking = span < natural;
    float f;
    if (growing && r.stretch() > 0) {
        f = float(span - natural) / float(r.stretch());
    } else if (shrinking && r.shrink() > 0) {
        f = float(natural - span) / float(r.shrink());
    } else {
        f = 0;
    }
    Coord p = g.origin();
    for (unsigned long index = 0; index < count; ++index) {
        const Requirement& r = request[index].requirement(dimension_);
        Allotment& a = result[index].allotment(dimension_);
        if (r.defined()) {
            Coord cspan = r.natural();
            if (growing) {
                cspan += Coord(float(r.stretch()) * f);
            } else if (shrinking) {
                cspan -= Coord(float(r.shrink()) * f);
            }
            a.span(cspan);
            a.origin(p + Coord(r.alignment() * cspan));
            a.alignment(r.alignment());
            p += cspan;
        } else {
            a.span(0);
            a.origin(p);
            a.alignment(0);
        }
    }
}

TileReversed::TileReversed(DimensionName d) : Layout() { dimension_ = d; }
TileReversed::~TileReversed() { }

void TileReversed::request(
    GlyphIndex count, const Requisition* request, Requisition& result
) {
    compute_tile_request(dimension_, 1.0, count, request, result);
    requisition_ = result;
}

void TileReversed::allocate(
    const Allocation& given,
    GlyphIndex count, const Requisition* request, Allocation* result
) {
    const Allotment& g = given.allotment(dimension_);
    Requirement& r = requisition_.requirement(dimension_);
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
    Coord natural = r.natural();
    boolean growing = span > natural;
    boolean shrinking = span < natural;
    float f;
    if (growing && r.stretch() > 0) {
        f = float(span - natural) / float(r.stretch());
    } else if (shrinking && r.shrink() > 0) {
        f = float(natural - span) / float(r.shrink());
    } else {
        f = 0;
    }
    Coord p = g.origin();
    for (unsigned long index = 0; index < count; ++index) {
        const Requirement& r = request[index].requirement(dimension_);
        Allotment a;
        if (r.defined()) {
            Coord cspan = r.natural();
            if (growing) {
                cspan += Coord(float(r.stretch()) * f);
            } else if (shrinking) {
                cspan -= Coord(float(r.shrink()) * f);
            }
            p -= cspan;
            a.span(cspan);
            a.origin(p + Coord(r.alignment() * cspan));
            a.alignment(r.alignment());
        } else {
            a.span(0);
            a.origin(p);
            a.alignment(0);
        }
        result[index].allot(dimension_, a);
    }
}

TileFirstAligned::TileFirstAligned(DimensionName dimension) : Layout() {
    dimension_ = dimension;
}

TileFirstAligned::~TileFirstAligned() { }

void TileFirstAligned::request(
    GlyphIndex count, const Requisition* request, Requisition& result
) {
    Coord natural_lead = 0;
    Coord min_lead = 0;
    Coord max_lead = 0;
    Coord natural_trail = 0;
    Coord min_trail = 0;
    Coord max_trail = 0;
    for (unsigned long index = 0; index < count; ++index) {
        const Requirement& r = request[index].requirement(dimension_);
        if (r.defined()) {
            if (index == 0) {
                natural_lead = Coord(r.natural() * r.alignment());
                max_lead = Coord((r.natural() + r.stretch()) * r.alignment());
                min_lead = Coord((r.natural() - r.shrink()) * r.alignment());
                natural_trail = Coord(r.natural() * (1 - r.alignment()));
                max_trail = Coord(
                    (r.natural() + r.stretch()) * (1 - r.alignment())
                );
                min_trail = Coord(
                    (r.natural() - r.shrink()) * (1 - r.alignment())
                );
            } else {
                natural_trail += r.natural();
                max_trail += r.natural() + r.stretch();
                min_trail += r.natural() - r.shrink();
            }
        }
    }
    Requirement nr(
	natural_lead, max_lead, min_lead,
	natural_trail, max_trail, min_trail
    );
    result.require(dimension_, nr);
    requisition_ = result;
}

void TileFirstAligned::allocate(
    const Allocation& given,
    GlyphIndex count, const Requisition* request, Allocation* result
) {
    const Allotment& g = given.allotment(dimension_);
    Requirement& r = requisition_.requirement(dimension_);
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
    Coord natural = r.natural();
    boolean growing = span > natural;
    boolean shrinking = span < natural;
    float f;
    if (growing && r.stretch() > 0) {
        f = float(span - natural) / float(r.stretch());
    } else if (shrinking && r.shrink() > 0) {
        f = float(natural - span) / float(r.shrink());
    } else {
        f = 0;
    }
    Coord p = g.origin();
    for (unsigned long index = 0; index < count; ++index) {
        const Requirement& r = request[index].requirement(dimension_);
        Allotment a;
        if (r.defined()) {
            Coord cspan = r.natural();
            if (growing) {
                cspan += Coord(float(r.stretch()) * f);
            } else if (shrinking) {
                cspan -= Coord(float(r.shrink()) * f);
            }
            if (index == 0) {
                p -= Coord(r.alignment() * float(cspan));
            }
            a.span(cspan);
            a.origin(p + Coord(r.alignment() * cspan));
            a.alignment(r.alignment());
            p += cspan;
        } else {
            a.span(0);
            a.origin(p);
            a.alignment(0);
        }
        result[index].allot(dimension_, a);
    }
}

TileReversedFirstAligned::TileReversedFirstAligned(
    DimensionName d
) : Layout() {
    dimension_ = d;
}

TileReversedFirstAligned::~TileReversedFirstAligned() { }

void TileReversedFirstAligned::request(
    GlyphIndex count, const Requisition* request, Requisition& result
) {
    Coord natural_lead = 0;
    Coord min_lead = 0;
    Coord max_lead = 0;
    Coord natural_trail = 0;
    Coord min_trail = 0;
    Coord max_trail = 0;
    for (unsigned long index = 0; index < count; ++index) {
        const Requirement& r = request[index].requirement(dimension_);
        if (r.defined()) {
            if (index == 0) {
                natural_lead = Coord(r.natural() * r.alignment());
                max_lead = Coord((r.natural() + r.stretch()) * r.alignment());
                min_lead = Coord((r.natural() - r.shrink()) * r.alignment());
                natural_trail = Coord(r.natural() * (1 - r.alignment()));
                max_trail = Coord(
                    (r.natural() + r.stretch()) * (1 - r.alignment())
                );
                min_trail = Coord(
                    (r.natural() - r.shrink()) * (1 - r.alignment())
                );
            } else {
                natural_lead += r.natural();
                max_lead += r.natural() + r.stretch();
                min_lead += r.natural() - r.shrink();
            }
        }
    }
    Requirement nr(
	natural_lead, max_lead, min_lead,
	natural_trail, max_trail, min_trail
    );
    result.require(dimension_, nr);
    requisition_ = result;
}

void TileReversedFirstAligned::allocate(
    const Allocation& given,
    GlyphIndex count, const Requisition* request, Allocation* result
) {
    const Allotment& g = given.allotment(dimension_);
    Requirement& r = requisition_.requirement(dimension_);
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
    Coord natural = r.natural();
    boolean growing = span > natural;
    boolean shrinking = span < natural;
    float f;
    if (growing && r.stretch() > 0) {
        f = float(span - natural) / float(r.stretch());
    } else if (shrinking && r.shrink() > 0) {
        f = float(natural - span) / float(r.shrink());
    } else {
        f = 0;
    }
    Coord p = g.origin();
    for (unsigned long index = 0; index < count; ++index) {
        const Requirement& r = request[index].requirement(dimension_);
        Allotment a;
        if (r.defined()) {
            Coord cspan = r.natural();
            if (growing) {
                cspan += Coord(float(r.stretch()) * f);
            } else if (shrinking) {
                cspan -= Coord(float(r.shrink()) * f);
            }
            if (index == 0) {
                p += Coord((1 - r.alignment()) * float(cspan));
            }
            p -= cspan;
            a.span(cspan);
            a.origin(p + Coord(r.alignment() * cspan));
            a.alignment(r.alignment());
        } else {
            a.span(0);
            a.origin(p);
            a.alignment(0);
        }
        result[index].allot(dimension_, a);
    }
}
