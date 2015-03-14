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
 * Patch - for repairing glyphs
 */

#include <InterViews/canvas.h>
#include <InterViews/patch.h>

Patch::Patch(Glyph* body) : MonoGlyph(body) {
    canvas_ = nil;
}

Patch::~Patch() { }

void Patch::redraw() const {
    Canvas* c = canvas_;
    if (c != nil) {
	c->damage(extension_);
    }
}

void Patch::reallocate() {
    Canvas* c = canvas_;
    if (c != nil) {
	/* remove these two lines when doc otherwise works */
	Requisition s;
	request(s);

	c->push_transform();
	c->transformer(transformer_);
	extension_.clear();
	allocate(c, allocation_, extension_);
	c->pop_transform();
    }
}

void Patch::repick(int depth, Hit& h) {
    Canvas* c = canvas_;
    if (c != nil) {
	c->push_transform();
	c->transformer(transformer_);
	pick(c, allocation_, depth, h);
	c->pop_transform();
    }
}

void Patch::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    canvas_ = c;
    transformer_ = c->transformer();
    allocation_ = a;
    MonoGlyph::allocate(c, a, ext);
    extension_ = ext;
}

void Patch::draw(Canvas* c, const Allocation& a) const {
    const Extension& ext = extension_;
    if (c->damaged(extension_)) {
	MonoGlyph::draw(c, a);
    }
}

void Patch::undraw() {
    MonoGlyph::undraw();
    canvas_ = nil;
}
