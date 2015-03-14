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
 * Glyph - visible data
 */

#ifndef iv_glyph_h
#define iv_glyph_h

#include <InterViews/coord.h>
#include <InterViews/geometry.h>
#include <InterViews/resource.h>

#include <InterViews/_enter.h>

class Canvas;
class Hit;
class Printer;

typedef long GlyphIndex;
typedef unsigned int GlyphBreakType;

class Glyph : virtual public Resource {
public:
    enum { no_break, pre_break, in_break, post_break };

    virtual ~Glyph();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void print(Printer*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);
    virtual void undraw();

    virtual Glyph* clone() const;
    virtual Glyph* compose(GlyphBreakType);

    virtual void append(Glyph*);
    virtual void prepend(Glyph*);
    virtual void insert(GlyphIndex, Glyph*);
    virtual void remove(GlyphIndex);
    virtual void replace(GlyphIndex, Glyph*);
    virtual void change(GlyphIndex);

    virtual GlyphIndex count() const;
    virtual Glyph* component(GlyphIndex) const;
    virtual void allotment(GlyphIndex index, DimensionName, Allotment&) const;
protected:
    Glyph();
};

#include <InterViews/_leave.h>

#endif
