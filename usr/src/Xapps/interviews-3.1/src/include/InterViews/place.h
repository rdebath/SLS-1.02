/*
 * Copyright (c) 1991 Stanford University
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
 * Placement - monoglyph that positions its body using a layout
 */

#ifndef iv_place_h
#define iv_place_h

#include <InterViews/layout.h>
#include <InterViews/monoglyph.h>

class Placement : public MonoGlyph {
public:
    Placement(Glyph*, Layout*);
    virtual ~Placement();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void print(Printer*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);
private:
    Layout* layout_;
};

class CenterLayout : public Layout {
public:
    CenterLayout(const DimensionName, float alignment = 0.5);
    virtual ~CenterLayout();

    virtual void request(
	GlyphIndex count, const Requisition*, Requisition& result
    );
    virtual void allocate(
	const Allocation& given, GlyphIndex count, const Requisition*,
	Allocation* result
    );
private:
    DimensionName dimension_;
    float alignment_;
};

class FixedLayout : public Layout {
public:
    FixedLayout(const DimensionName, Coord span);
    virtual ~FixedLayout();

    virtual void request(
	GlyphIndex count, const Requisition*, Requisition& result
    );
    virtual void allocate(
	const Allocation& given, GlyphIndex count, const Requisition*,
	Allocation* result
    );
private:
    DimensionName dimension_;
    Coord span_;
};

class VariableLayout : public Layout {
public:
    VariableLayout(const DimensionName, Coord stretch, Coord shrink);
    virtual ~VariableLayout();

    virtual void request(
	GlyphIndex count, const Requisition*, Requisition& result
    );
    virtual void allocate(
	const Allocation& given, GlyphIndex count, const Requisition*,
	Allocation* result
    );
private:
    DimensionName dimension_;
    Coord stretch_;
    Coord shrink_;
};

class NaturalLayout : public Layout {
public:
    NaturalLayout(const DimensionName, Coord natural);
    virtual ~NaturalLayout();

    virtual void request(
	GlyphIndex count, const Requisition*, Requisition& result
    );
    virtual void allocate(
	const Allocation& given, GlyphIndex count, const Requisition*,
	Allocation* result
    );
private:
    DimensionName dimension_;
    Coord natural_;
};

class MarginLayout : public Layout {
public:
    MarginLayout(Coord margin);
    MarginLayout(Coord hmargin, Coord vmargin);
    MarginLayout(Coord lmargin, Coord rmargin, Coord bmargin, Coord tmargin);
    MarginLayout(
        Coord lmargin, Coord lstretch, Coord lshrink,
        Coord rmargin, Coord rstretch, Coord rshrink,
        Coord bmargin, Coord bstretch, Coord bshrink,
        Coord tmargin, Coord tstretch, Coord tshrink
    );
    virtual ~MarginLayout();

    virtual void request(
	GlyphIndex count, const Requisition*, Requisition& result
    );
    virtual void allocate(
	const Allocation& given, GlyphIndex count, const Requisition*,
	Allocation* result
    );
private:
    Coord lnatural_, lstretch_, lshrink_;
    Coord rnatural_, rstretch_, rshrink_;
    Coord bnatural_, bstretch_, bshrink_;
    Coord tnatural_, tstretch_, tshrink_;
    Requisition requisition_;

    static Coord span(
	Coord span, const Requirement& total,
	Coord natural, Coord stretch, Coord shrink
    );
};

#endif
