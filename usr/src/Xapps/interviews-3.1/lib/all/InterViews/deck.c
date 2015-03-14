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

#include <InterViews/align.h>
#include <InterViews/deck.h>
#include <InterViews/hit.h>

Deck::Deck(GlyphIndex size) : PolyGlyph(size) {
    card_ = -1;
    changed_ = true;
}

Deck::~Deck() { }

GlyphIndex Deck::card() const {
    return card_;
}

void Deck::flip_to(GlyphIndex index) {
    undraw();
    card_ = index;
}

void Deck::request(Requisition& req) const {
    if (changed_) {
	GlyphIndex n = count();
	Requisition* r = new Requisition[n];
	for (GlyphIndex i = 0; i < n; i++) {
	    Glyph* g = component(i);
	    if (g != nil) {
		g->request(r[i]);
	    }
	}
	Deck* d = (Deck*)this;
	Align x(Dimension_X);
	x.request(n, r, d->requisition_);
	Align y(Dimension_Y);
	y.request(n, r, d->requisition_);
	delete r;
	d->changed_ = false;
    }
    req = requisition_;
}

/*
 * When allocating a deck, we allocate the card (top component).
 * We also make the extension cover the entire allocation;
 * otherwise updating gets complicated in the presence of a flip_to.
 * Usually, this won't hurt.
 */

void Deck::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    allocation_ = a;
    if (card_ >= 0 && card_ < count()) {
	Glyph* g = component(card_);
	if (g != nil) {
	    g->allocate(c, a, ext);
	}
	ext.merge(c, a);
    }
}

void Deck::draw(Canvas* c, const Allocation& a) const {
    if (card_ >= 0 && card_ < count()) {
	Glyph* g = component(card_);
	if (g != nil) {
	    g->draw(c, a);
	}
    }
}

void Deck::print(Printer* p, const Allocation& a) const {
    if (card_ >= 0 && card_ < count()) {
	Glyph* g = component(card_);
	if (g != nil) {
	    g->print(p, a);
	}
    }
}

void Deck::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    if (card_ >= 0 && card_ < count()) {
	Glyph* g = component(card_);
        if (g != nil) {
	    h.begin(depth, this, card_);
	    g->pick(c, a, depth + 1, h);
	    h.end();
        }
    }
}

void Deck::undraw() {
    if (card_ >= 0 && card_ < count()) {
	Glyph* g = component(card_);
	if (g != nil) {
	    g->undraw();
	}
    }
}

void Deck::modified(GlyphIndex) {
    changed_ = true;
}

void Deck::allotment(GlyphIndex, DimensionName d, Allotment& a) const {
    a = allocation_.allotment(d);
}
