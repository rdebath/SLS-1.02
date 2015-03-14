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
 * Layout - geometry management
 */

#include <InterViews/align.h>
#include <InterViews/box.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/deck.h>
#include <InterViews/font.h>
#include <InterViews/glue.h>
#include <InterViews/hit.h>
#include <InterViews/layout.h>
#include <InterViews/place.h>
#include <InterViews/superpose.h>
#include <InterViews/tile.h>

Layout::Layout() { }
Layout::~Layout() { }

void Layout::request(GlyphIndex, const Requisition*, Requisition&) { }

void Layout::allocate(
    const Allocation&, GlyphIndex, const Requisition*, Allocation*
) { }

/*
 * LayoutKit -- create glyphs for layout
 */

class Discretionary : public Glyph {
public:
    Discretionary(int penalty, Glyph*);
    Discretionary(int penalty, Glyph* no, Glyph* pre, Glyph* in, Glyph* post);
    virtual ~Discretionary();

    virtual void request(Requisition&) const;
    virtual Glyph* compose(GlyphBreakType);
private:
    int penalty_;
    Glyph* nobreak_;
    Glyph* prebreak_;
    Glyph* inbreak_;
    Glyph* postbreak_;
};

class LayoutLayer : public MonoGlyph {
public:
    LayoutLayer(Glyph* between, Glyph* under, Glyph* over);
    virtual ~LayoutLayer();

    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void print(Printer*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);
    virtual void undraw();
private:
    Glyph* under_;
    Glyph* over_;
};

class ShapeOf : public Glyph {
public:
    ShapeOf(Glyph*, Glyph*);
    virtual ~ShapeOf();

    virtual void request(Requisition&) const;
private:
    Glyph* x_;
    Glyph* y_;
};

class Space : public Glyph {
public:
    Space(int count, Coord each, const Font*, const Color*);
    virtual ~Space();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);
private:
    int count_;
    Coord each_;
    const Font* font_;
    const Color* color_;
    Coord width_;
    Coord height_;
    float alignment_;
};

class Strut : public Glyph {
public:
    Strut(
        const Font*,
        Coord natural = 0, Coord stretch = 0, Coord shrink = 0
    );
    virtual ~Strut();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
private:
    const Font* font_;
    Coord natural_;
    Coord stretch_;
    Coord shrink_;
    Coord height_;
    float alignment_;
};

class HStrut : public Glyph {
public:
    HStrut(
        Coord right_bearing, Coord left_bearing = 0,
        Coord natural = 0, Coord stretch = 0, Coord shrink = 0
    );
    virtual ~HStrut();

    virtual void request(Requisition&) const;
private:
    Coord left_bearing_;
    Coord right_bearing_;
    Coord natural_;
    Coord stretch_;
    Coord shrink_;
};

class VStrut : public Glyph {
public:
    VStrut(
        Coord ascent, Coord descent = 0,
        Coord natural = 0, Coord stretch = 0, Coord shrink = 0
    );
    virtual ~VStrut();

    virtual void request(Requisition&) const;
private:
    Coord ascent_;
    Coord descent_;
    Coord natural_;
    Coord stretch_;
    Coord shrink_;
};

class LayoutKitImpl {
    friend class LayoutKit;

    static LayoutKit* instance_;

    static PolyGlyph* add(
	PolyGlyph*,
	Glyph* g1, Glyph* g2, Glyph* g3, Glyph* g4, Glyph* g5,
	Glyph* g6, Glyph* g7, Glyph* g8, Glyph* g9, Glyph* g10
    );
};

LayoutKit* LayoutKitImpl::instance_;

LayoutKit::LayoutKit() { }
LayoutKit::~LayoutKit() { }

LayoutKit* LayoutKit::instance() {
    if (LayoutKitImpl::instance_ == nil) {
	LayoutKitImpl::instance_ = new LayoutKit;
    }
    return LayoutKitImpl::instance_;
}

PolyGlyph* LayoutKit::box(Layout* layout, GlyphIndex size) const {
    return new Box(layout, size);
}

PolyGlyph* LayoutKit::hbox(GlyphIndex size) const {
    return new Box(
	new Superpose(new Tile(Dimension_X), new Align(Dimension_Y)),
	size
    );
}

PolyGlyph* LayoutKit::hbox(
    Glyph* g1, Glyph* g2, Glyph* g3, Glyph* g4, Glyph* g5,
    Glyph* g6, Glyph* g7, Glyph* g8, Glyph* g9, Glyph* g10
) const {
    return LayoutKitImpl::add(
	hbox(10), g1, g2, g3, g4, g5, g6, g7, g8, g9, g10
    );
}

PolyGlyph* LayoutKit::vbox(GlyphIndex size) const {
    return new Box(
	new Superpose(new TileReversed(Dimension_Y), new Align(Dimension_X)),
	size
    );
}

PolyGlyph* LayoutKit::vbox(
    Glyph* g1, Glyph* g2, Glyph* g3, Glyph* g4, Glyph* g5,
    Glyph* g6, Glyph* g7, Glyph* g8, Glyph* g9, Glyph* g10
) const {
    return LayoutKitImpl::add(
	vbox(10), g1, g2, g3, g4, g5, g6, g7, g8, g9, g10
    );
}

PolyGlyph* LayoutKit::hbox_first_aligned(GlyphIndex size) const {
    return new Box(
	new Superpose(
	    new TileFirstAligned(Dimension_X),
	    new Align(Dimension_Y)
	),
	size
    );
}

PolyGlyph* LayoutKit::hbox_first_aligned(
    Glyph* g1, Glyph* g2, Glyph* g3, Glyph* g4, Glyph* g5,
    Glyph* g6, Glyph* g7, Glyph* g8, Glyph* g9, Glyph* g10
) const {
    return LayoutKitImpl::add(
	hbox_first_aligned(10), g1, g2, g3, g4, g5, g6, g7, g8, g9, g10
    );
}

PolyGlyph* LayoutKit::vbox_first_aligned(GlyphIndex size) const {
    return new Box(
	new Superpose(
	    new TileReversedFirstAligned(Dimension_Y),
	    new Align(Dimension_X)
	),
	size
    );
}

PolyGlyph* LayoutKit::vbox_first_aligned(
    Glyph* g1, Glyph* g2, Glyph* g3, Glyph* g4, Glyph* g5,
    Glyph* g6, Glyph* g7, Glyph* g8, Glyph* g9, Glyph* g10
) const {
    return LayoutKitImpl::add(
	vbox_first_aligned(10), g1, g2, g3, g4, g5, g6, g7, g8, g9, g10
    );
}

ScrollBox* LayoutKit::vscrollbox(GlyphIndex size) const {
    return new TBScrollBox(size);
}

PolyGlyph* LayoutKit::overlay(
    Glyph* g1, Glyph* g2, Glyph* g3, Glyph* g4, Glyph* g5,
    Glyph* g6, Glyph* g7, Glyph* g8, Glyph* g9, Glyph* g10
) const {
    return LayoutKitImpl::add(
	new Box(
	    new Superpose(new Align(Dimension_X), new Align(Dimension_Y)),
	    10
	),
	g1, g2, g3, g4, g5, g6, g7, g8, g9, g10
    );
}

Deck* LayoutKit::deck(GlyphIndex size) const {
    return new Deck(size);
}

Deck* LayoutKit::deck(
    Glyph* g1, Glyph* g2, Glyph* g3, Glyph* g4, Glyph* g5,
    Glyph* g6, Glyph* g7, Glyph* g8, Glyph* g9, Glyph* g10
) const {
    Deck* d = new Deck(10);
    LayoutKitImpl::add(d, g1, g2, g3, g4, g5, g6, g7, g8, g9, g10);
    return d;
}

PolyGlyph* LayoutKitImpl::add(
    PolyGlyph* p,
    Glyph* g1, Glyph* g2, Glyph* g3, Glyph* g4, Glyph* g5,
    Glyph* g6, Glyph* g7, Glyph* g8, Glyph* g9, Glyph* g10
) {
    if (g1 != nil) {
	p->append(g1);
    }
    if (g2 != nil) {
	p->append(g2);
    }
    if (g3 != nil) {
	p->append(g3);
    }
    if (g4 != nil) {
	p->append(g4);
    }
    if (g5 != nil) {
	p->append(g5);
    }
    if (g6 != nil) {
	p->append(g6);
    }
    if (g7 != nil) {
	p->append(g7);
    }
    if (g8 != nil) {
	p->append(g8);
    }
    if (g9 != nil) {
	p->append(g9);
    }
    if (g10 != nil) {
	p->append(g10);
    }
    return p;
}

MonoGlyph* LayoutKit::back(Glyph* g, Glyph* under) const {
    return new LayoutLayer(g, under, nil);
}

MonoGlyph* LayoutKit::front(Glyph* g, Glyph* over) const {
    return new LayoutLayer(g, nil, over);
}

MonoGlyph* LayoutKit::between(Glyph* g, Glyph* under, Glyph* over) const {
    return new LayoutLayer(g, under, over);
}

Glyph* LayoutKit::glue(
    DimensionName d, Coord natural, Coord stretch, Coord shrink,
    float alignment
) const {
    return new Glue(d, natural, stretch, shrink, alignment);
}

Glyph* LayoutKit::glue(const Requisition& r) const {
    return new Glue(r);
}

Glyph* LayoutKit::hglue() const {
    return new Glue(Dimension_X, 0, fil, 0, 0.0);
}

Glyph* LayoutKit::hglue(Coord natural) const {
    return new Glue(Dimension_X, natural, fil, 0, 0.0);
}

Glyph* LayoutKit::hglue(Coord natural, Coord stretch, Coord shrink) const {
    return new Glue(Dimension_X, natural, stretch, shrink, 0.0);
}

Glyph* LayoutKit::hglue(
    Coord natural, Coord stretch, Coord shrink, float alignment
) const {
    return new Glue(Dimension_X, natural, stretch, shrink, alignment);
}

Glyph* LayoutKit::hspace(Coord natural) const {
    return new Glue(Dimension_X, natural, 0, 0, 0.0);
}

Glyph* LayoutKit::vglue() const {
    return new Glue(Dimension_Y, 0, fil, 0, 0.0);
}

Glyph* LayoutKit::vglue(Coord natural) const {
    return new Glue(Dimension_Y, natural, fil, 0, 0.0);
}

Glyph* LayoutKit::vglue(
    Coord natural, Coord stretch, Coord shrink
) const {
    return new Glue(Dimension_Y, natural, stretch, shrink, 0.0);
}

Glyph* LayoutKit::vglue(
    Coord natural, Coord stretch, Coord shrink, float alignment
) const {
    return new Glue(Dimension_Y, natural, stretch, shrink, alignment);
}

Glyph* LayoutKit::vspace(Coord natural) const {
    return new Glue(Dimension_Y, natural, 0, 0, 0.0);
}

Glyph* LayoutKit::shape_of(Glyph* g) const {
    return new ShapeOf(g, g);
}

Glyph* LayoutKit::shape_of_xy(Glyph* x, Glyph* y) const {
    return new ShapeOf(x, y);
}

Glyph* LayoutKit::discretionary(int penalty, Glyph* g) const {
    return new Discretionary(penalty, g);
}

Glyph* LayoutKit::discretionary(
    int penalty, Glyph* no, Glyph* before, Glyph* in, Glyph* after
) const {
    return new Discretionary(penalty, no, before, in, after);
}

Glyph* LayoutKit::strut(
    const Font* f, Coord natural, Coord stretch, Coord shrink
) const {
    return new Strut(f, natural, stretch, shrink);
}

Glyph* LayoutKit::hstrut(
    Coord right_bearing, Coord left_bearing,
    Coord natural, Coord stretch, Coord shrink
) const {
    return new HStrut(right_bearing, left_bearing, natural, stretch, shrink);
}

Glyph* LayoutKit::vstrut(
    Coord ascent, Coord descent,
    Coord natural, Coord stretch, Coord shrink
) const {
    return new VStrut(ascent, descent, natural, stretch, shrink);
}

Glyph* LayoutKit::spaces(
    int count, Coord each, const Font* f, const Color* c
) const {
    return new Space(count, each, f, c);
}

MonoGlyph* LayoutKit::center(Glyph* g, float xalign, float yalign) const {
    return new Placement(
	g,
	new Superpose(
	    new CenterLayout(Dimension_X, xalign),
	    new CenterLayout(Dimension_Y, yalign)
	)
    );
}

MonoGlyph* LayoutKit::center_dimension(
    Glyph* g, DimensionName d, float align
) const {
    return new Placement(g, new CenterLayout(d, align));
}

MonoGlyph* LayoutKit::hcenter(Glyph* g, float x) const {
    return center_dimension(g, Dimension_X, x);
}

MonoGlyph* LayoutKit::vcenter(Glyph* g, float y) const {
    return center_dimension(g, Dimension_Y, y);
}

MonoGlyph* LayoutKit::fixed(Glyph* g, Coord x, Coord y) const {
    return new Placement(
	g,
	new Superpose(
	    new FixedLayout(Dimension_X, x),
	    new FixedLayout(Dimension_Y, y)
	)
    );
}

MonoGlyph* LayoutKit::fixed_dimension(
    Glyph* g, DimensionName d, Coord span
) const {
    return new Placement(g, new FixedLayout(d, span));
}

MonoGlyph* LayoutKit::hfixed(Glyph* g, Coord x) const {
    return fixed_dimension(g, Dimension_X, x);
}

MonoGlyph* LayoutKit::vfixed(Glyph* g, Coord y) const {
    return fixed_dimension(g, Dimension_Y, y);
}

MonoGlyph* LayoutKit::flexible(
    Glyph* g, Coord stretch, Coord shrink
) const {
    return new Placement(
	g,
	new Superpose(
	    new VariableLayout(Dimension_X, stretch, shrink),
	    new VariableLayout(Dimension_Y, stretch, shrink)
	)
    );
}

MonoGlyph* LayoutKit::flexible_dimension(
    Glyph* g, DimensionName d, Coord stretch, Coord shrink
) const {
    return new Placement(g, new VariableLayout(d, stretch, shrink));
}

MonoGlyph* LayoutKit::hflexible(
    Glyph* g, Coord stretch, Coord shrink
) const {
    return flexible_dimension(g, Dimension_X, stretch, shrink);
}

MonoGlyph* LayoutKit::vflexible(
    Glyph* g, Coord stretch, Coord shrink
) const {
    return flexible_dimension(g, Dimension_Y, stretch, shrink);
}

MonoGlyph* LayoutKit::natural(
    Glyph* g, Coord x, Coord y
) const {
    return new Placement(
	g,
	new Superpose(
	    new NaturalLayout(Dimension_X, x),
	    new NaturalLayout(Dimension_Y, y)
	)
    );
}

MonoGlyph* LayoutKit::natural_dimension(
    Glyph* g, DimensionName d, Coord span
) const {
    return new Placement(g, new NaturalLayout(d, span));
}

MonoGlyph* LayoutKit::hnatural(Glyph* g, Coord x) const {
    return natural_dimension(g, Dimension_X, x);
}

MonoGlyph* LayoutKit::vnatural(Glyph* g, Coord y) const {
    return natural_dimension(g, Dimension_Y, y);
}

MonoGlyph* LayoutKit::margin(Glyph* g, Coord size) const {
    return new Placement(g, new MarginLayout(size));
}

MonoGlyph* LayoutKit::margin(
    Glyph* g, Coord hmargin, Coord vmargin
) const {
    return new Placement(g, new MarginLayout(hmargin, vmargin));
}

MonoGlyph* LayoutKit::margin(
    Glyph* g,
    Coord lmargin, Coord rmargin, Coord bmargin, Coord tmargin
) const {
    return new Placement(
	g, new MarginLayout(lmargin, rmargin, bmargin, tmargin)
    );
}

MonoGlyph* LayoutKit::margin(
    Glyph* g,
    Coord lmargin, Coord lstretch, Coord lshrink,
    Coord rmargin, Coord rstretch, Coord rshrink,
    Coord bmargin, Coord bstretch, Coord bshrink,
    Coord tmargin, Coord tstretch, Coord tshrink
) const {
    return new Placement(
	g,
	new MarginLayout(
	    lmargin, lstretch, lshrink, rmargin, rstretch, rshrink,
	    bmargin, bstretch, bshrink, tmargin, tstretch, tshrink
	)
    );
}

MonoGlyph* LayoutKit::hmargin(Glyph* g, Coord size) const {
    return margin(g, size, size, 0, 0);
}

MonoGlyph* LayoutKit::hmargin(
    Glyph* g, Coord lmargin, Coord rmargin
) const {
    return margin(g, lmargin, rmargin, 0, 0);
}

MonoGlyph* LayoutKit::hmargin(
    Glyph* g,
    Coord lmargin, Coord lstretch, Coord lshrink,
    Coord rmargin, Coord rstretch, Coord rshrink
) const {
    return margin(
	g,
	lmargin, lstretch, lshrink, rmargin, rstretch, rshrink,
	0, 0, 0, 0, 0, 0
    );
}

MonoGlyph* LayoutKit::vmargin(Glyph* g, Coord size) const {
    return margin(g, 0, 0, size, size);
}

MonoGlyph* LayoutKit::vmargin(
    Glyph* g, Coord bmargin, Coord tmargin
) const {
    return margin(g, 0, 0, bmargin, tmargin);
}

MonoGlyph* LayoutKit::vmargin(
    Glyph* g,
    Coord bmargin, Coord bstretch, Coord bshrink,
    Coord tmargin, Coord tstretch, Coord tshrink
) const {
    return margin(
	g,
	0, 0, 0, 0, 0, 0,
	bmargin, bstretch, bshrink, tmargin, tstretch, tshrink
    );
}

MonoGlyph* LayoutKit::lmargin(Glyph* g, Coord size) const {
    return margin(g, size, 0, 0, 0);
}

MonoGlyph* LayoutKit::lmargin(
    Glyph* g, Coord size, Coord stretch, Coord shrink
) const {
    return margin(g, size, stretch, shrink, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}

MonoGlyph* LayoutKit::rmargin(Glyph* g, Coord size) const {
    return margin(g, 0, size, 0, 0);
}

MonoGlyph* LayoutKit::rmargin(
    Glyph* g, Coord size, Coord stretch, Coord shrink
) const {
    return margin(g, 0, 0, 0, size, stretch, shrink, 0, 0, 0, 0, 0, 0);
}

MonoGlyph* LayoutKit::bmargin(Glyph* g, Coord size) const {
    return margin(g, 0, 0, size, 0);
}

MonoGlyph* LayoutKit::bmargin(
    Glyph* g, Coord size, Coord stretch, Coord shrink
) const {
    return margin(g, 0, 0, 0, 0, 0, 0, size, stretch, shrink, 0, 0, 0);
}

MonoGlyph* LayoutKit::tmargin(Glyph* g, Coord size) const {
    return margin(g, 0, 0, 0, size);
}

MonoGlyph* LayoutKit::tmargin(
    Glyph* g, Coord size, Coord stretch, Coord shrink
) const {
    return margin(g, 0, 0, 0, 0, 0, 0, 0, 0, 0, size, stretch, shrink);
}

/* class Discretionary */

Discretionary::Discretionary(int penalty, Glyph* glyph) : Glyph() {
    penalty_ = penalty;
    nobreak_ = glyph;
    if (nobreak_ != nil) {
        nobreak_->ref();
    }
    prebreak_ = glyph;
    if (prebreak_ != nil) {
        prebreak_->ref();
    }
    inbreak_ = nil;
    postbreak_ = nil;
}

Discretionary::Discretionary(
    int penalty, Glyph* no, Glyph* pre, Glyph* in, Glyph* post
) : Glyph() {
    penalty_ = penalty;
    nobreak_ = no;
    if (nobreak_ != nil) {
        nobreak_->ref();
    }
    prebreak_ = pre;
    if (prebreak_ != nil) {
        prebreak_->ref();
    }
    inbreak_ = in;
    if (inbreak_ != nil) {
        inbreak_->ref();
    }
    postbreak_ = post;
    if (postbreak_ != nil) {
        postbreak_->ref();
    }
}

Discretionary::~Discretionary() {
    if (nobreak_ != nil) {
        nobreak_->unref();
    }
    if (prebreak_ != nil) {
        prebreak_->unref();
    }
    if (inbreak_ != nil) {
        inbreak_->unref();
    }
    if (postbreak_ != nil) {
        postbreak_->unref();
    }
}    

void Discretionary::request(Requisition& requisition) const {
    if (nobreak_ != nil) {
        nobreak_->request(requisition);
    }
    requisition.penalty(penalty_);
}

Glyph* Discretionary::compose(GlyphBreakType b) {
    if (b == no_break) {
        return nobreak_;
    } else if (b == pre_break) {
        return prebreak_;
    } else if (b == in_break) {
        return inbreak_;
    } else if (b == post_break) {
        return postbreak_;
    } else {
        return nil;
    }
}

/* class Glue */

Glue::Glue(
    DimensionName d,
    Coord natural, Coord stretch, Coord shrink, float alignment
) : Glyph() {
    Requirement r(natural, stretch, shrink, alignment);
    requisition_.require(d, r);
}

Glue::Glue(const Requisition& r) : Glyph() {
    requisition_ = r;
}

Glue::~Glue() { }

void Glue::request(Requisition& r) const {
    r = requisition_;
}

void Glue::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    ext.merge(c, a);
}

/* class Space */

Space::Space(int count, float each, const Font* f, const Color* c) : Glyph() {
    count_ = count;
    each_ = each;
    font_ = f;
    Resource::ref(font_);
    color_ = c;
    Resource::ref(color_);
    if (font_ != nil) {
	FontBoundingBox b;
	font_->font_bbox(b);
	Coord ascent = b.ascent();
	Coord descent = b.descent();
	width_ = font_->width(' ') * each_ * count_;
	height_ = ascent + descent;
	alignment_ = (height_ == 0) ? 0 : descent / height_;
    } else {
	width_ = 0;
	height_ = 0;
	alignment_ = 0;
    }
}

Space::~Space() {
    Resource::unref(font_);
    Resource::unref(color_);
}

void Space::request(Requisition& requisition) const {
    Requirement rx(width_, width_*4, width_/3, 0);
    Requirement ry(height_, 0, 0, alignment_);
    requisition.require(Dimension_X, rx);
    requisition.require(Dimension_Y, ry);
}

void Space::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    ext.merge(c, a);
}

void Space::pick(Canvas*, const Allocation& a, int depth, Hit& h) {
    Coord x = h.left();
    Coord left = a.left();
    Coord right = a.right();
    if (x >= left && x < right) {
        h.target(depth, this, (x > (left+right)/2) ? 1 : 0);
    }
}

void Space::draw(Canvas* c, const Allocation& a) const {
    if (count_ > 0) {
        Coord x = a.x();
        Coord y = a.y();
        Coord each = (a.right() - a.left()) / count_;
        for (int i = 0; i < count_; ++i) {
            c->character(font_, ' ', each, color_, x, y);
            x += each;
        }
    }
}

/* class Strut */

Strut::Strut(
    const Font* font, Coord natural, Coord stretch, Coord shrink
) : Glyph() {
    font_ = font;
    Resource::ref(font_);
    if (font_ != nil) {
	FontBoundingBox b;
	font_->font_bbox(b);
	height_ = b.ascent() + b.descent();
	alignment_ = (height_ == 0) ? 0 : b.descent() / height_;
    }
    natural_ = natural;
    stretch_ = stretch;
    shrink_ = shrink;
}

Strut::~Strut() {
    Resource::unref(font_);
}

void Strut::request(Requisition& requisition) const {
    Requirement rx(natural_, stretch_, shrink_, 0);
    Requirement ry(height_, 0, 0, alignment_);
    requisition.require(Dimension_X, rx);
    requisition.require(Dimension_Y, ry);
}

void Strut::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    ext.merge(c, a);
}

HStrut::HStrut(
    Coord right_bearing, Coord left_bearing,
    Coord natural, Coord stretch, Coord shrink
) : Glyph() {
    left_bearing_ = left_bearing;
    right_bearing_ = right_bearing;
    natural_ = natural;
    stretch_ = stretch;
    shrink_ = shrink;
}

HStrut::~HStrut() { }

void HStrut::request(Requisition& requisition) const {
    Coord width = left_bearing_ + right_bearing_;
    Requirement rx(width, 0, 0, (width == 0) ? 0 : left_bearing_ / width);
    Requirement ry(natural_, stretch_, shrink_, 0);
    requisition.require(Dimension_X, rx);
    requisition.require(Dimension_Y, ry);
}

VStrut::VStrut(
    Coord ascent, Coord descent, Coord natural, Coord stretch, Coord shrink
) : Glyph() {
    ascent_ = ascent;
    descent_ = descent;
    natural_ = natural;
    stretch_ = stretch;
    shrink_ = shrink;
}

VStrut::~VStrut() { }

void VStrut::request(Requisition& requisition) const {
    Coord height = ascent_ + descent_;
    Requirement rx(natural_, stretch_, shrink_, 0);
    Requirement ry(height, 0, 0, (height == 0) ? 0 : descent_ / height);
    requisition.require(Dimension_X, rx);
    requisition.require(Dimension_Y, ry);
}

/* class LayoutLayer */

LayoutLayer::LayoutLayer(
    Glyph* between, Glyph* under, Glyph* over
) : MonoGlyph(between) {
    Resource::ref(under);
    under_ = under;
    Resource::ref(over);
    over_ = over;
}

LayoutLayer::~LayoutLayer() {
    Resource::unref(under_);
    Resource::unref(over_);
}

void LayoutLayer::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    if (under_ != nil) {
	under_->allocate(c, a, ext);
    }
    MonoGlyph::allocate(c, a, ext);
    if (over_ != nil) {
	over_->allocate(c, a, ext);
    }
}

void LayoutLayer::draw(Canvas* c, const Allocation& a) const {
    if (under_ != nil) {
	under_->draw(c, a);
    }
    MonoGlyph::draw(c, a);
    if (over_ != nil) {
	over_->draw(c, a);
    }
}

void LayoutLayer::print(Printer* p, const Allocation& a) const {
    if (under_ != nil) {
	under_->print(p, a);
    }
    MonoGlyph::print(p, a);
    if (over_ != nil) {
	over_->print(p, a);
    }
}

void LayoutLayer::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    if (under_ != nil) {
	under_->pick(c, a, depth, h);
    }
    MonoGlyph::pick(c, a, depth, h);
    if (over_ != nil) {
	over_->pick(c, a, depth, h);
    }
}

void LayoutLayer::undraw() {
    if (under_ != nil) {
	under_->undraw();
    }
    MonoGlyph::undraw();
    if (over_ != nil) {
	over_->undraw();
    }
}

/* class ShapeOf */

ShapeOf::ShapeOf(Glyph* x, Glyph* y) {
    x_ = x;
    y_ = y;
    Resource::ref(x_);
    Resource::ref(y_);
}

ShapeOf::~ShapeOf() {
    Resource::unref(x_);
    Resource::unref(y_);
}

void ShapeOf::request(Requisition& requisition) const {
    if (x_ == y_) {
	x_->request(requisition);
    } else {
	Requisition req;
	if (x_ != nil) {
	    x_->request(req);
	    requisition.require_x(req.x_requirement());
	}
	if (y_ != nil) {
	    y_->request(req);
	    requisition.require_y(req.y_requirement());
	}
    }
}
