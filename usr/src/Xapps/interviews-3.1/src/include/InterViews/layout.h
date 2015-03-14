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
 * Layout - structured layout
 */

#ifndef iv_layout_h
#define iv_layout_h

#include <InterViews/deck.h>
#include <InterViews/monoglyph.h>
#include <InterViews/polyglyph.h>
#include <InterViews/scrbox.h>

class Layout {
protected:
    Layout();
public:
    virtual ~Layout();

    virtual void request(
        GlyphIndex count, const Requisition*, Requisition& result
    );
    virtual void allocate(
        const Allocation& given, GlyphIndex count, const Requisition*,
	Allocation* result
    );
};

class Color;
class Font;
class PolyGlyph;

class LayoutKit {
protected:
    LayoutKit();
public:
    virtual ~LayoutKit();

    static LayoutKit* instance();
protected:
    static void instance(LayoutKit*);
public:
    virtual PolyGlyph* box(Layout*, GlyphIndex size = 10) const;
    virtual PolyGlyph* hbox(GlyphIndex size) const;
    virtual PolyGlyph* hbox(
	Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil,
	Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil
    ) const;
    virtual PolyGlyph* vbox(GlyphIndex size) const;
    virtual PolyGlyph* vbox(
	Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil,
	Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil
    ) const;
    virtual PolyGlyph* hbox_first_aligned(GlyphIndex size) const;
    virtual PolyGlyph* hbox_first_aligned(
	Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil,
	Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil
    ) const;
    virtual PolyGlyph* vbox_first_aligned(GlyphIndex size) const;
    virtual PolyGlyph* vbox_first_aligned(
	Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil,
	Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil
    ) const;

    virtual ScrollBox* vscrollbox(GlyphIndex size = 10) const;

    virtual PolyGlyph* overlay(
	Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil,
	Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil
    ) const;

    virtual Deck* deck(GlyphIndex size) const;
    virtual Deck* deck(
	Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil,
	Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil, Glyph* = nil
    ) const;

    virtual MonoGlyph* back(Glyph*, Glyph* under) const;
    virtual MonoGlyph* front(Glyph*, Glyph* over) const;
    virtual MonoGlyph* between(Glyph*, Glyph* under, Glyph* over) const;

    virtual Glyph* glue(
	DimensionName, Coord natural, Coord stretch, Coord shrink,
	float alignment
    ) const;
    virtual Glyph* glue(const Requisition&) const;
    virtual Glyph* hglue() const;
    virtual Glyph* hglue(Coord natural) const;
    virtual Glyph* hglue(Coord natural, Coord stretch, Coord shrink) const;
    virtual Glyph* hglue(
	Coord natural, Coord stretch, Coord shrink, float alignment
    ) const;
    virtual Glyph* hspace(Coord natural) const;
    virtual Glyph* vglue() const;
    virtual Glyph* vglue(Coord natural) const;
    virtual Glyph* vglue(Coord natural, Coord stretch, Coord shrink) const;
    virtual Glyph* vglue(
	Coord natural, Coord stretch, Coord shrink, float alignment
    ) const;
    virtual Glyph* vspace(Coord natural) const;

    virtual Glyph* shape_of(Glyph*) const;
    virtual Glyph* shape_of_xy(Glyph*, Glyph*) const;

    virtual Glyph* discretionary(int penalty, Glyph*) const;
    virtual Glyph* discretionary(
	int penalty, Glyph* no, Glyph* before, Glyph* in, Glyph* after
    ) const;

    virtual Glyph* strut(
	const Font*, Coord natural = 0, Coord stretch = 0, Coord shrink = 0
    ) const;
    virtual Glyph* hstrut(
	Coord right_bearing, Coord left_bearing = 0,
	Coord natural = 0, Coord stretch = 0, Coord shrink = 0
    ) const;
    virtual Glyph* vstrut(
	Coord ascent, Coord descent = 0,
	Coord natural = 0, Coord stretch = 0, Coord shrink = 0
    ) const;

    virtual Glyph* spaces(
	int count, Coord each, const Font*, const Color*
    ) const;

    virtual MonoGlyph* center(Glyph*, float x = 0.5, float y = 0.5) const;
    virtual MonoGlyph* center_dimension(
	Glyph*, DimensionName, float align
    ) const;
    virtual MonoGlyph* hcenter(Glyph*, float x = 0.5) const;
    virtual MonoGlyph* vcenter(Glyph*, float y = 0.5) const;

    virtual MonoGlyph* fixed(Glyph*, Coord x, Coord y) const;
    virtual MonoGlyph* fixed_dimension(
	Glyph*, DimensionName, Coord
    ) const;
    virtual MonoGlyph* hfixed(Glyph*, Coord x) const;
    virtual MonoGlyph* vfixed(Glyph*, Coord y) const;

    virtual MonoGlyph* flexible(
	Glyph*, Coord stretch = fil, Coord shrink = fil
    ) const;
    virtual MonoGlyph* flexible_dimension(
	Glyph*, DimensionName, Coord stretch = fil, Coord shrink = fil
    ) const;
    virtual MonoGlyph* hflexible(
	Glyph*, Coord stretch = fil, Coord shrink = fil
    ) const;
    virtual MonoGlyph* vflexible(
	Glyph*, Coord stretch = fil, Coord shrink = fil
    ) const;

    virtual MonoGlyph* natural(Glyph*, Coord x, Coord y) const;
    virtual MonoGlyph* natural_dimension(
	Glyph*, DimensionName, Coord
    ) const;
    virtual MonoGlyph* hnatural(Glyph*, Coord x) const;
    virtual MonoGlyph* vnatural(Glyph*, Coord y) const;

    virtual MonoGlyph* margin(Glyph*, Coord) const;
    virtual MonoGlyph* margin(Glyph*, Coord hmargin, Coord vmargin) const;
    virtual MonoGlyph* margin(
	Glyph*,
	Coord lmargin, Coord rmargin, Coord bmargin, Coord tmargin
    ) const;
    virtual MonoGlyph* margin(
	Glyph*,
	Coord lmargin, Coord lstretch, Coord lshrink,
	Coord rmargin, Coord rstretch, Coord rshrink,
	Coord bmargin, Coord bstretch, Coord bshrink,
	Coord tmargin, Coord tstretch, Coord tshrink
    ) const;
    virtual MonoGlyph* hmargin(Glyph*, Coord) const;
    virtual MonoGlyph* hmargin(Glyph*, Coord lmargin, Coord rmargin) const;
    virtual MonoGlyph* hmargin(
	Glyph*,
	Coord lmargin, Coord lstretch, Coord lshrink,
	Coord rmargin, Coord rstretch, Coord rshrink
    ) const;
    virtual MonoGlyph* vmargin(Glyph*, Coord) const;
    virtual MonoGlyph* vmargin(Glyph*, Coord bmargin, Coord tmargin) const;
    virtual MonoGlyph* vmargin(
	Glyph*,
	Coord bmargin, Coord bstretch, Coord bshrink,
	Coord tmargin, Coord tstretch, Coord tshrink
    ) const;
    virtual MonoGlyph* lmargin(Glyph*, Coord) const;
    virtual MonoGlyph* lmargin(
	Glyph*, Coord, Coord stretch, Coord shrink
    ) const;
    virtual MonoGlyph* rmargin(Glyph*, Coord) const;
    virtual MonoGlyph* rmargin(
	Glyph*, Coord, Coord stretch, Coord shrink
    ) const;
    virtual MonoGlyph* bmargin(Glyph*, Coord) const;
    virtual MonoGlyph* bmargin(
	Glyph*, Coord, Coord stretch, Coord shrink
    ) const;
    virtual MonoGlyph* tmargin(Glyph*, Coord) const;
    virtual MonoGlyph* tmargin(
	Glyph*, Coord, Coord stretch, Coord shrink
    ) const;

    /* backward compatibility */
    MonoGlyph* fixed_span(Glyph*, Coord x, Coord y) const;
    MonoGlyph* fixed_span_dimension(Glyph*, DimensionName, Coord) const;
    MonoGlyph* h_fixed_span(Glyph*, Coord x) const;
    MonoGlyph* v_fixed_span(Glyph*, Coord y) const;

    MonoGlyph* variable_span(
	Glyph*, Coord stretch = fil, Coord shrink = fil
    ) const;
    MonoGlyph* variable_span_dimension(
	Glyph*, DimensionName, Coord stretch = fil, Coord shrink = fil
    ) const;
    MonoGlyph* h_variable_span(
	Glyph*, Coord stretch = fil, Coord shrink = fil
    ) const;
    MonoGlyph* v_variable_span(
	Glyph*, Coord stretch = fil, Coord shrink = fil
    ) const;

    MonoGlyph* natural_span(Glyph*, Coord x, Coord y) const;
    MonoGlyph* natural_span_dimension(
	Glyph*, DimensionName, Coord
    ) const;
    MonoGlyph* h_natural_span(Glyph*, Coord x) const;
    MonoGlyph* v_natural_span(Glyph*, Coord y) const;

    MonoGlyph* h_margin(Glyph*, Coord) const;
    MonoGlyph* h_margin(Glyph*, Coord lmargin, Coord rmargin) const;
    MonoGlyph* h_margin(
	Glyph*,
	Coord lmargin, Coord lstretch, Coord lshrink,
	Coord rmargin, Coord rstretch, Coord rshrink
    ) const;
    MonoGlyph* v_margin(Glyph*, Coord) const;
    MonoGlyph* v_margin(Glyph*, Coord bmargin, Coord tmargin) const;
    MonoGlyph* v_margin(
	Glyph*,
	Coord bmargin, Coord bstretch, Coord bshrink,
	Coord tmargin, Coord tstretch, Coord tshrink
    ) const;
    MonoGlyph* l_margin(Glyph*, Coord) const;
    MonoGlyph* l_margin(
	Glyph*, Coord, Coord stretch, Coord shrink
    ) const;
    MonoGlyph* r_margin(Glyph*, Coord) const;
    MonoGlyph* r_margin(
	Glyph*, Coord, Coord stretch, Coord shrink
    ) const;
    MonoGlyph* b_margin(Glyph*, Coord) const;
    MonoGlyph* b_margin(
	Glyph*, Coord, Coord stretch, Coord shrink
    ) const;
    MonoGlyph* t_margin(Glyph*, Coord) const;
    MonoGlyph* t_margin(
	Glyph*, Coord, Coord stretch, Coord shrink
    ) const;
};

/* inline old names to new names */

inline MonoGlyph* LayoutKit::fixed_span(Glyph* g, Coord x, Coord y) const {
    return fixed(g, x, y);
}

inline MonoGlyph* LayoutKit::fixed_span_dimension(
    Glyph* g, DimensionName d, Coord c
) const {
    return fixed_dimension(g, d, c);
}

inline MonoGlyph* LayoutKit::h_fixed_span(Glyph* g, Coord x) const {
    return hfixed(g, x);
}

inline MonoGlyph* LayoutKit::v_fixed_span(Glyph* g, Coord y) const {
    return vfixed(g, y);
}

inline MonoGlyph* LayoutKit::variable_span(
    Glyph* g, Coord stretch, Coord shrink
) const {
    return flexible(g, stretch, shrink);
}

inline MonoGlyph* LayoutKit::variable_span_dimension(
    Glyph* g, DimensionName d, Coord stretch, Coord shrink
) const {
    return flexible_dimension(g, d, stretch, shrink);
}

inline MonoGlyph* LayoutKit::h_variable_span(
    Glyph* g, Coord stretch, Coord shrink
) const {
    return hflexible(g, stretch, shrink);
}

inline MonoGlyph* LayoutKit::v_variable_span(
    Glyph* g, Coord stretch, Coord shrink
) const {
    return vflexible(g, stretch, shrink);
}

inline MonoGlyph* LayoutKit::natural_span(Glyph* g, Coord x, Coord y) const {
    return natural(g, x, y);
}

inline MonoGlyph* LayoutKit::natural_span_dimension(
    Glyph* g, DimensionName d, Coord c
) const {
    return natural_dimension(g, d, c);
}

inline MonoGlyph* LayoutKit::h_natural_span(Glyph* g, Coord x) const {
    return hnatural(g, x);
}

inline MonoGlyph* LayoutKit::v_natural_span(Glyph* g, Coord y) const {
    return vnatural(g, y);
}

inline MonoGlyph* LayoutKit::h_margin(Glyph* g, Coord m) const {
    return hmargin(g, m);
}

inline MonoGlyph* LayoutKit::h_margin(
    Glyph* g, Coord lmargin, Coord rmargin
) const {
    return hmargin(g, lmargin, rmargin);
}

inline MonoGlyph* LayoutKit::h_margin(
    Glyph* g,
    Coord lmargin, Coord lstretch, Coord lshrink,
    Coord rmargin, Coord rstretch, Coord rshrink
) const {
    return hmargin(g, lmargin, lstretch, lshrink, rmargin, rstretch, rshrink);
}

inline MonoGlyph* LayoutKit::v_margin(Glyph* g, Coord m) const {
    return vmargin(g, m);
}

inline MonoGlyph* LayoutKit::v_margin(
    Glyph* g, Coord bmargin, Coord tmargin
) const {
    return vmargin(g, bmargin, tmargin);
}

inline MonoGlyph* LayoutKit::v_margin(
    Glyph* g,
    Coord bmargin, Coord bstretch, Coord bshrink,
    Coord tmargin, Coord tstretch, Coord tshrink
) const {
    return vmargin(g, bmargin, bstretch, bshrink, tmargin, tstretch, tshrink);
}

inline MonoGlyph* LayoutKit::l_margin(Glyph* g, Coord m) const {
    return lmargin(g, m);
}

inline MonoGlyph* LayoutKit::l_margin(
    Glyph* g, Coord natural, Coord stretch, Coord shrink
) const {
    return lmargin(g, natural, stretch, shrink);
}

inline MonoGlyph* LayoutKit::r_margin(Glyph* g, Coord m) const {
    return rmargin(g, m);
}

inline MonoGlyph* LayoutKit::r_margin(
    Glyph* g, Coord natural, Coord stretch, Coord shrink
) const {
    return rmargin(g, natural, stretch, shrink);
}

inline MonoGlyph* LayoutKit::b_margin(Glyph* g, Coord m) const {
    return bmargin(g, m);
}

inline MonoGlyph* LayoutKit::b_margin(
    Glyph* g, Coord natural, Coord stretch, Coord shrink
) const {
    return bmargin(g, natural, stretch, shrink);
}

inline MonoGlyph* LayoutKit::t_margin(Glyph* g, Coord m) const {
    return tmargin(g, m);
}

inline MonoGlyph* LayoutKit::t_margin(
    Glyph* g, Coord natural, Coord stretch, Coord shrink
) const {
    return tmargin(g, natural, stretch, shrink);
}

#endif
