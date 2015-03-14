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
 * Canvas - an area for drawing
 */

#ifndef iv_canvas_h
#define iv_canvas_h

#include <InterViews/coord.h>

#include <InterViews/_enter.h>

class Bitmap;
class Brush;
class CanvasRep;
class Color;
class Extension;
class Font;
class Raster;
class Transformer;
class Window;

/* anachronism */
typedef unsigned int CanvasLocation;

class Canvas {
public:
    Canvas();
    virtual ~Canvas();

    virtual Window* window() const;

    virtual void size(Coord width, Coord height);
    virtual void psize(PixelCoord width, PixelCoord height);

    virtual Coord width() const;
    virtual Coord height() const;
    virtual PixelCoord pwidth() const;
    virtual PixelCoord pheight() const;

    virtual PixelCoord to_pixels(Coord) const;
    virtual Coord to_coord(PixelCoord) const;
    virtual Coord to_pixels_coord(Coord) const;

    virtual void new_path();
    virtual void move_to(Coord x, Coord y);
    virtual void line_to(Coord x, Coord y);
    virtual void curve_to(
	Coord x, Coord y, Coord x1, Coord y1, Coord x2, Coord y2
    );
    virtual void close_path();
    virtual void stroke(const Color*, const Brush*);
    virtual void line(
	Coord x1, Coord y1, Coord x2, Coord y2, const Color*, const Brush*
    );
    virtual void rect(
	Coord l, Coord b, Coord r, Coord t, const Color*, const Brush*
    );
    virtual void fill(const Color*);
    virtual void fill_rect(Coord l, Coord b, Coord r, Coord t, const Color*);

    virtual void character(
	const Font*, long ch, Coord width, const Color*, Coord x, Coord y
    );
    virtual void stencil(const Bitmap*, const Color*, Coord x, Coord y);
    virtual void image(const Raster*, Coord x, Coord y);

    virtual void push_transform();
    virtual void transform(const Transformer&);
    virtual void pop_transform();

    virtual void transformer(const Transformer&);
    virtual const Transformer& transformer() const;

    virtual void push_clipping();
    virtual void clip();
    virtual void clip_rect(Coord l, Coord b, Coord r, Coord t);
    virtual void pop_clipping();

    virtual void front_buffer();
    virtual void back_buffer();

    virtual void damage(const Extension&);
    virtual void damage(Coord left, Coord bottom, Coord right, Coord top);
    virtual boolean damaged(const Extension&) const;
    virtual boolean damaged(
	Coord left, Coord bottom, Coord right, Coord top
    ) const;
    virtual void damage_area(Extension&);
    virtual void damage_all();
    virtual boolean any_damage() const;
    virtual void restrict_damage(const Extension&);
    virtual void restrict_damage(
	Coord left, Coord bottom, Coord right, Coord top
    );
    virtual void redraw(Coord left, Coord bottom, Coord right, Coord top);
    virtual void repair();

    CanvasRep* rep() const;
private:
    CanvasRep* rep_;

    /* anachronisms */
public:
    enum { mapped, unmapped, offscreen };

    virtual CanvasLocation status() const;
    unsigned int Width() const;
    unsigned int Height() const;
    virtual void SetBackground(const Color*);
};

inline CanvasRep* Canvas::rep() const { return rep_; }

#include <InterViews/_leave.h>

#endif
