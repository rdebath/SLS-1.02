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
 * Hit detection
 */

#ifndef iv_hit_h
#define iv_hit_h

#include <InterViews/coord.h>
#include <InterViews/glyph.h>

#include <InterViews/_enter.h>

class Event;
class Handler;
class HitImpl;
class Transformer;

class Hit {
protected:
    Hit(HitImpl*);
public:
    Hit(const Event*);
    Hit(Coord x, Coord y);
    Hit(Coord left, Coord bottom, Coord right, Coord top);
    virtual ~Hit();

    virtual const Event* event() const;
    virtual Coord left() const;
    virtual Coord bottom() const;
    virtual Coord right() const;
    virtual Coord top() const;

    virtual void push_transform();
    virtual void transform(const Transformer&);
    virtual void pop_transform();

    virtual void begin(int depth, Glyph*, GlyphIndex, Handler* = nil);
    virtual void target(int depth, Glyph*, GlyphIndex, Handler* = nil);
    virtual void end();
    virtual void remove(int depth, GlyphIndex target = 0);
    virtual void retarget(
	int depth, Glyph*, GlyphIndex, Handler* = nil, GlyphIndex target = 0
    );

    virtual boolean any() const;
    virtual int count() const;
    virtual int depth(GlyphIndex target = 0) const;
    virtual Glyph* target(int depth, GlyphIndex target = 0) const;
    virtual GlyphIndex index(int depth, GlyphIndex target = 0) const;
    virtual Handler* handler() const;
private:
    HitImpl* impl_;
    char free_store_[4000];

    void init();
};

#include <InterViews/_leave.h>

#endif
