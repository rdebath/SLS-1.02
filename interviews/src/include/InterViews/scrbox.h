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
 * ScrollBox - scrollable list of glyphs
 */

#ifndef iv_scrbox_h
#define iv_scrbox_h

#include <InterViews/adjust.h>
#include <InterViews/polyglyph.h>

#include <InterViews/_enter.h>

class ScrollBox : public PolyGlyph, public Adjustable {
protected:
    ScrollBox(GlyphIndex size = 10);
    virtual ~ScrollBox();

    virtual boolean shown(GlyphIndex) const;
    virtual GlyphIndex first_shown() const;
    virtual GlyphIndex last_shown() const;
};

class TBScrollBoxImpl;

class TBScrollBox : public ScrollBox {
public:
    TBScrollBox(GlyphIndex size = 10);
    virtual ~TBScrollBox();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);
    virtual void undraw();

    virtual void modified(GlyphIndex);
    virtual boolean shown(GlyphIndex) const;
    virtual GlyphIndex first_shown() const;
    virtual GlyphIndex last_shown() const;
    virtual void allotment(GlyphIndex, DimensionName, Allotment&) const;

    virtual Coord lower(DimensionName) const;
    virtual Coord upper(DimensionName) const;
    virtual Coord length(DimensionName) const;
    virtual Coord cur_lower(DimensionName) const;
    virtual Coord cur_upper(DimensionName) const;
    virtual Coord cur_length(DimensionName) const;

    virtual void scroll_forward(DimensionName);
    virtual void scroll_backward(DimensionName);
    virtual void page_forward(DimensionName);
    virtual void page_backward(DimensionName);

    virtual void scroll_to(DimensionName, Coord lower);
private:
    TBScrollBoxImpl* impl_;

    TBScrollBoxImpl& impl() const;
    void scroll_by(DimensionName, long);
    void do_scroll(DimensionName, GlyphIndex new_start, GlyphIndex new_end);
};

#include <InterViews/_leave.h>

#endif
