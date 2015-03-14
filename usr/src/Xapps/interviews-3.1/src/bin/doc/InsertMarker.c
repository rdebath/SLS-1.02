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

#include "InsertMarker.h"

#include <InterViews/canvas.h>
#include <InterViews/color.h>

#include <Dispatch/iocallback.h>
#include <Dispatch/dispatcher.h>

declareIOCallback(InsertMarker)
implementIOCallback(InsertMarker)

InsertMarker::InsertMarker(
    Glyph* body, const Color* overlay, const Color* underlay, long flash
) : MonoGlyph(body) {
    overlay_ = overlay;
    Resource::ref(overlay_);
    underlay_ = underlay;
    Resource::ref(underlay_);
    flash_ = flash;
    flasher_ = new IOCallback(InsertMarker)(this, &InsertMarker::flash);
    marked_ = false;
    on_ = true;
    width_ = 0;
    canvas_ = nil;
}

InsertMarker::~InsertMarker() {
    Resource::unref(overlay_);
    Resource::unref(underlay_);
    Dispatcher::instance().stopTimer(flasher_);
}

static void do_draw(
    Canvas* c, const Color* color, Coord x, Coord y, Coord width, Coord height
) {
    c->fill_rect(x - width/2, y, x + width/2, y + height, color);
}

static void do_damage(
    Canvas* c, Coord x, Coord y, Coord width, Coord height
) {
    c->damage(x - width/2, y, x + width/2, y + height);
}

void InsertMarker::flash (long, long) {
    if (marked_) {
        if (canvas_ != nil) {
            do_damage(canvas_, x_, y_, width_, height_);
        }
        on_ = !on_;
        if (flash_ > 0) {
            Dispatcher::instance().stopTimer(flasher_);
            Dispatcher::instance().startTimer(0, flash_, flasher_);
        }
    }
}

void InsertMarker::unmark() {
    if (marked_) {
        if (canvas_ != nil) {
            do_damage(canvas_, x_, y_, width_, height_);
        }
        marked_ = false;
    }
}

void InsertMarker::mark(
    Coord x, Coord y, Coord width, Coord height
) {
    if (!marked_ || x!=x_ || y!=y_ || width!=width_ || height!=height_ ){
        if (canvas_ != nil) {
            do_damage(canvas_, x_, y_, width_, height_);
        }
        x_ = x;
        y_ = y;
        width_ = width;
        height_ = height;
        marked_ = true;
        on_ = false;
        flash(0, 0);
    }
}

void InsertMarker::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    MonoGlyph::allocate(c, a, ext);
    ext.merge_xy(
	c, a.left() - width_/2, a.right() + width_/2, a.bottom(), a.top()
    );
    canvas_ = c;
}

void InsertMarker::draw(Canvas* c, const Allocation& allocation) const {
    if (c != nil) {
        if (on_ && marked_ && underlay_ != nil) {
            do_draw(c, underlay_, x_, y_, width_, height_);
        }
    }
    MonoGlyph::draw(c, allocation);
    if (c != nil) {
        if (on_ && marked_ && overlay_ != nil) {
            do_draw(c, overlay_, x_, y_, width_, height_);
        }
    }
}

void InsertMarker::undraw() {
    MonoGlyph::undraw();
    canvas_ = nil;
}
