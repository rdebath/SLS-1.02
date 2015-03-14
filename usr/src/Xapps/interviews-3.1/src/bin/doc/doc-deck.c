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

#include "doc-deck.h"
#include <InterViews/align.h>
#include <InterViews/hit.h>
#include <OS/list.h>

class DeckInfo {
public:
    Glyph* glyph_;
};

declareList(DeckInfo_List,DeckInfo)
implementList(DeckInfo_List,DeckInfo)

DocDeck::DocDeck() : Glyph() {
    info_ = new DeckInfo_List();
    card_ = -1;
}

DocDeck::~DocDeck() {
    GlyphIndex count = info_->count();
    for (GlyphIndex i = 0; i < count; ++i) {
        DeckInfo& info = info_->item_ref(i);
	Resource::unref(info.glyph_);
    }
    delete info_;
    info_ = nil;
}

GlyphIndex DocDeck::card() const {
    return card_;
}

void DocDeck::flip_to(GlyphIndex index) {
    card_ = index;
}

void DocDeck::append(Glyph* glyph) {
    DeckInfo info;
    info.glyph_ = glyph;
    info_->append(info);
    Resource::ref(glyph);
}

void DocDeck::prepend(Glyph* glyph) {
    DeckInfo info;
    info.glyph_ = glyph;
    info_->prepend(info);
    Resource::ref(glyph);
}

void DocDeck::insert(GlyphIndex index, Glyph* glyph) {
    DeckInfo info;
    info.glyph_ = glyph;
    info_->insert(index, info);
    Resource::ref(glyph);
}

void DocDeck::remove(GlyphIndex index) {
    DeckInfo& info = info_->item_ref(index);
    Resource::unref(info.glyph_);
    info_->remove(index);
}

void DocDeck::replace(GlyphIndex index, Glyph* glyph) {
    DeckInfo& info = info_->item_ref(index);
    Resource::ref(glyph);
    Resource::unref(info.glyph_);
    info.glyph_ = glyph;
}

GlyphIndex DocDeck::count() const {
    return info_->count();
}

Glyph* DocDeck::component(GlyphIndex index) const {
    return info_->item_ref(index).glyph_;
}

void DocDeck::request(Requisition& requisition) const {
    GlyphIndex count = info_->count();
    Requisition* r = new Requisition[count];
    for (int index = 0; index < count; ++index) {
        DeckInfo& info = info_->item_ref(index);
        if (info.glyph_ != nil) {
            info.glyph_->request(r[index]);
        }
    }
    Align x(Dimension_X);
    x.request(count, r, requisition);
    Align y(Dimension_Y);
    y.request(count, r, requisition);
    delete r;
}

void DocDeck::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    allocation_ = a;
    GlyphIndex count = info_->count();
    for (GlyphIndex i = 0; i < count; ++i) {
        DeckInfo& info = info_->item_ref(i);
        if (info.glyph_ != nil) {
            info.glyph_->allocate((i == card_) ? c : nil, a, ext);
        }
    }
}

void DocDeck::draw(Canvas* c, const Allocation& a) const {
    GlyphIndex count = info_->count();
    if (card_ >= 0 && card_ < count) {
        DeckInfo& info = info_->item_ref(card_);
        if (info.glyph_ != nil) {
            info.glyph_->draw(c, a);
        }
    }
}

void DocDeck::print(Printer* p, const Allocation& a) const {
    GlyphIndex count = info_->count();
    if (card_ >= 0 && card_ < count) {
        DeckInfo& info = info_->item_ref(card_);
        if (info.glyph_ != nil) {
            info.glyph_->print(p, a);
        }
    }
}

void DocDeck::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    GlyphIndex count = info_->count();
    if (card_ >= 0 && card_ < count) {
        DeckInfo& info = info_->item_ref(card_);
        if (info.glyph_ != nil) {
	    h.begin(depth, this, card_);
	    info.glyph_->pick(c, a, depth + 1, h);
	    h.end();
        }
    }
}

void DocDeck::allotment(GlyphIndex, DimensionName res, Allotment& a) const {
    a = allocation_.allotment(res);
}
