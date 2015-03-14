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
 * Listeners are glyphs that are interested in input
 */

#ifndef listener_h
#define listener_h

#include <InterViews/boolean.h>
#include <InterViews/event.h>
#include <InterViews/monoglyph.h>

#include <InterViews/_enter.h>

class Handler;

class Listener : public MonoGlyph {
public:
    Listener(Glyph*, Handler*);
    virtual ~Listener();

    virtual void target(Handler*);
    virtual Handler* target() const;

    virtual void motion(boolean);
    virtual boolean motion() const;

    virtual void key(boolean);
    virtual boolean key() const;

    virtual void button(boolean, EventButton = Event::any);
    virtual boolean button(EventButton = Event::any) const;

    virtual boolean caught(const Event&) const;

    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);
    virtual void undraw();

    virtual boolean picks(Coord x, Coord y);
protected:
    Listener();
private:
    Handler* target_;
    boolean motion_ : 1;
    boolean key_ : 1;
    boolean left_ : 1;
    boolean middle_ : 1;
    boolean right_ : 1;

    Canvas* canvas_;
    Allocation allocation_;
};

#include <InterViews/_leave.h>

#endif
