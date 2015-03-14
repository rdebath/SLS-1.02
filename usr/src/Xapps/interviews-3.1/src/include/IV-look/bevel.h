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
 * Bevel - beveled framing of a glyph
 */

#ifndef ivlook_bevel_h
#define ivlook_bevel_h

#include <InterViews/monoglyph.h>

class BevelFrame : public MonoGlyph {
public:
    BevelFrame(
	Glyph*, Coord, float xalign = 0.0, float yalign = 0.0,
	boolean hmargin = true, boolean vmargin = true
    );
    virtual ~BevelFrame();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void print(Printer*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);

    virtual void draw_frame(Canvas*, const Allocation&, Coord thickness) const;
private:
    Coord thickness_;
    float xalign_;
    float yalign_;
    boolean hmargin_ : 1;
    boolean vmargin_ : 1;

    Coord thickness(Canvas*) const;
    void allocate_body(Glyph*, Coord, Allocation&) const;
};

class Color;

typedef void (*Beveler)(
    Canvas*, const Color* light, const Color* medium, const Color* dark,
    Coord thickness, Coord left, Coord bottom, Coord right, Coord top
);

class Bevel : public BevelFrame {
public:
    Bevel(
	Glyph*, Beveler,
	const Color* light, const Color* medium, const Color* dark,
	Coord thickness, float xalign = 0.0, float yalign = 0.0,
	boolean hmargin = true, boolean vmargin = true
    );
    virtual ~Bevel();

    virtual void draw_frame(Canvas*, const Allocation&, Coord thickness) const;

    static void rect(
	Canvas*, const Color* light, const Color* medium, const Color* dark,
	Coord thickness, Coord left, Coord bottom, Coord right, Coord top
    );
    static void left_arrow(
	Canvas*, const Color* light, const Color* medium, const Color* dark,
	Coord thickness, Coord left, Coord bottom, Coord right, Coord top
    );
    static void right_arrow(
	Canvas*, const Color* light, const Color* medium, const Color* dark,
	Coord thickness, Coord left, Coord bottom, Coord right, Coord top
    );
    static void up_arrow(
	Canvas*, const Color* light, const Color* medium, const Color* dark,
	Coord thickness, Coord left, Coord bottom, Coord right, Coord top
    );
    static void down_arrow(
	Canvas*, const Color* light, const Color* medium, const Color* dark,
	Coord thickness, Coord left, Coord bottom, Coord right, Coord top
    );
    static void diamond(
	Canvas*, const Color* light, const Color* medium, const Color* dark,
	Coord thickness, Coord left, Coord bottom, Coord right, Coord top
    );
private:
    Beveler beveler_;
    const Color* light_;
    const Color* medium_;
    const Color* dark_;

    Coord thickness(Canvas*) const;
    void allocate_body(Glyph*, Coord, Allocation&) const;
};

#endif
