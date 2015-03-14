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

#ifndef iv_composition_h
#define iv_composition_h

#include <InterViews/monoglyph.h>

class CompositionComponent_List;
class Break_List;
class Compositor;
class Break;

class Composition : public MonoGlyph {
protected:
    Composition(
        Glyph* context, Compositor*, Glyph* separator,
        DimensionName, Coord span, Coord stretch, Coord shrink,
	GlyphIndex size
    );
public:
    virtual ~Composition();

    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual boolean repair();

    virtual GlyphIndex item(GlyphIndex index) const;
    virtual GlyphIndex beginning_of(GlyphIndex item) const;
    virtual GlyphIndex end_of(GlyphIndex item) const;
    virtual void margin(GlyphIndex item, Coord begin, Coord end);
    virtual void view(GlyphIndex first, GlyphIndex last);

    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);

    virtual void append(Glyph*);
    virtual void prepend(Glyph*);
    virtual void insert(GlyphIndex, Glyph*);
    virtual void remove(GlyphIndex);
    virtual void replace(GlyphIndex, Glyph*);
    virtual void change(GlyphIndex);

    virtual GlyphIndex count() const;
    virtual Glyph* component(GlyphIndex) const;

    virtual void allotment(GlyphIndex, DimensionName, Allotment&) const;
protected:
    virtual void do_repair(
        GlyphIndex first_component, GlyphIndex first_break,
	GlyphIndex* breaks, GlyphIndex break_count
    );
    virtual void damage(GlyphIndex first, GlyphIndex last);
    virtual Glyph* separator(Break&);
    virtual Glyph* make_item(Break&, boolean created);
    virtual Glyph* make(Break&);
private:
    Compositor* compositor_;
    CompositionComponent_List* component_;
    Break_List* breaks_;
    Glyph* separator_;
    boolean view_all_;
    boolean damaged_;
    GlyphIndex first_damage_;
    GlyphIndex last_damage_;
    GlyphIndex item_;
    DimensionName dimension_;
    Coord span_;
    boolean resizable_;
};

class LRComposition : public Composition {
public:
    LRComposition(
        Glyph* context, Compositor*, Glyph* separator,
        Coord width, Coord stretch = fil, Coord shrink = fil,
	GlyphIndex size = 10
    );
    virtual ~LRComposition();
protected:
    virtual Glyph* make(Break&);
};

class TBComposition : public Composition {
public:
    TBComposition(
        Glyph* context, Compositor*, Glyph* separator,
        Coord height, Coord stretch = fil, Coord shrink = fil,
	GlyphIndex size = 10
    );
    virtual ~TBComposition();
protected:
    virtual Glyph* make(Break&);
};

#endif
