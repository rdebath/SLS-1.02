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

#include "doc-listener.h"
#include <InterViews/canvas.h>
#include <InterViews/handler.h>
#include <InterViews/hit.h>
#include <IV-2_6/InterViews/sensor.h>

Listener::Listener() : MonoGlyph(nil) {
    target_ = nil;
    motion_ = false;
    key_ = false;
    left_ = false;
    middle_ = false;
    right_ = false;
    canvas_ = nil;
}

Listener::Listener(Glyph* body, Handler* h) : MonoGlyph(body) {
    target_ = h;
    Resource::ref(target_);
    motion_ = false;
    key_ = false;
    left_ = false;
    middle_ = false;
    right_ = false;
    canvas_ = nil;
}

Listener::~Listener() {
    Resource::unref(target_);
    target_ = nil;
    canvas_ = nil;
}

void Listener::target(Handler* h) {
    Resource::ref(h);
    Resource::unref(target_);
    target_ = h;
}

Handler* Listener::target() const { return target_; }
void Listener::motion(boolean b) { motion_ = b; }
boolean Listener::motion() const { return motion_; }
void Listener::key(boolean b) { key_ = b; }
boolean Listener::key() const { return key_; }

void Listener::button(boolean b, EventButton pb) {
    switch (pb) {
    case Event::undefined:
	break;
    case Event::left:
	left_ = b;
	break;
    case Event::middle:
	middle_ = b;
	break;
    case Event::right:
	right_ = b;
	break;
    case Event::any:
	left_ = b;
	middle_ = b;
	right_ = b;
	break;
    case Event::other_button:
	break;
    }
}

boolean Listener::button(EventButton pb) const {
    switch (pb) {
    case Event::undefined:
	break;
    case Event::left:
	return left_;
    case Event::middle:
	return middle_;
    case Event::right:
	return right_;
    case Event::any:
	return left_ || middle_ || right_;
    case Event::other_button:
	break;
    }
    return false;
}

boolean Listener::caught(const Event& e) const {
    boolean found = false;
    switch (e.type()) {
    case Event::undefined:
        break;
    case Event::down:
    case Event::up:
        switch (e.pointer_button()) {
        case Event::left:
            found = left_;
            break;
        case Event::middle:
            found = middle_;
            break;
        case Event::right:
            found = right_;
            break;
        }
        break;
    case Event::key:
        found = key_;
        break;
    case Event::motion:
        found = motion_;
        break;
    case Event::other_event:
        break;
    }
    return found;
}

void Listener::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    MonoGlyph::allocate(c, a, ext);
    canvas_ = c;
    allocation_ = a;
}

void Listener::undraw() {
    MonoGlyph::undraw();
    canvas_ = nil;
}

void Listener::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    const Event* e = h.event();
    if (e != nil && caught(*e)) {
	h.begin(depth, this, 0, target_);
	MonoGlyph::pick(c, a, depth, h);
	h.end();
    } else {
	MonoGlyph::pick(c, a, depth, h);
    }
}

boolean Listener::picks(Coord x, Coord y) {
    Hit hit(x, y);
    MonoGlyph::pick(canvas_, allocation_, 0, hit);
    return hit.any();
}
