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
 * Bitmap - a two-dimensional boolean mask
 */

#ifndef iv_bitmap_h
#define iv_bitmap_h

#include <InterViews/coord.h>
#include <InterViews/resource.h>

#include <InterViews/_enter.h>

class BitmapRep;
class Font;
class Transformer;

class Bitmap : public Resource {
public:
    Bitmap(
	const void*, unsigned int width, unsigned int height,
	int x0 = -1, int y0 = -1
    );
    Bitmap(const Font*, long character, float scale = 1.0);
    Bitmap(const Bitmap&);
    virtual ~Bitmap();

    static Bitmap* open(const char* filename);

    virtual void poke(boolean set, int x, int y);
    virtual boolean peek(int x, int y) const;

    virtual Coord width() const;
    virtual Coord height() const;

    virtual unsigned int pwidth() const;
    virtual unsigned int pheight() const;

    virtual Coord left_bearing() const;
    virtual Coord right_bearing() const;
    virtual Coord ascent() const;
    virtual Coord descent() const;

    virtual void flush() const;

    BitmapRep* rep() const;
protected:
    Bitmap();
private:
    BitmapRep* rep_;

public:
    /* anachronisms */
    int Left() const;
    int Right() const;
    int Top() const;
    int Bottom() const;
    unsigned int Width() const;
    unsigned int Height() const;

    void Transform(const Transformer*);
    void Scale(float sx, float sy);
    void Rotate(float angle);

    void FlipHorizontal();
    void FlipVertical();
    void Rotate90();
    void Rotate180();
    void Rotate270();
    void Invert();

    boolean Peek(int x, int y) const;
    void Poke(boolean, int x, int y);
};

inline BitmapRep* Bitmap::rep() const { return rep_; }

inline unsigned int Bitmap::Width() const { return pwidth(); }
inline unsigned int Bitmap::Height() const { return pheight(); }

inline boolean Bitmap::Peek(int x, int y) const { return peek(x, y); }
inline void Bitmap::Poke(boolean bit, int x, int y) { poke(bit, x, y); }

#include <InterViews/_leave.h>

#endif
