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
 * Aggregate - composite glyph
 */

#include <InterViews/aggr.h>
#include <InterViews/canvas.h>
#include <InterViews/hit.h>
#include <InterViews/printer.h>
#include <OS/list.h>

class AggregateInfo {
public:
    AggregateInfo();

    Glyph* glyph_;
    Allocation allocation_;
    Extension extension_;
};

declareList(AggregateInfo_List,AggregateInfo)
implementList(AggregateInfo_List,AggregateInfo)

AggregateInfo::AggregateInfo() {
    glyph_ = nil;
}

Aggregate::Aggregate(GlyphIndex size) : Glyph() {
    info_ = new AggregateInfo_List(size);
}

Aggregate::~Aggregate() {
    GlyphIndex count = info_->count();
    for (GlyphIndex i = 0; i < count; ++i) {
        AggregateInfo& info = info_->item_ref(i);
	Resource::unref(info.glyph_);
    }
    delete info_;
    info_ = nil;
}

GlyphIndex Aggregate::count() const {
    return info_->count();
}

Glyph* Aggregate::component(GlyphIndex index) const {
    return info_->item_ref(index).glyph_;
}

void Aggregate::allotment(
    GlyphIndex index, DimensionName res, Allotment& a
) const {
    a = info_->item_ref(index).allocation_.allotment(res);
}

void Aggregate::allot(GlyphIndex i, DimensionName res, const Allotment& al) {
    Allocation& a = info_->item_ref(i).allocation_;
    a.allot(res, al);
}

void Aggregate::change(GlyphIndex) { }

void Aggregate::append(Glyph* glyph) {
    AggregateInfo info;
    info.glyph_ = glyph;
    info_->append(info);
    Resource::ref(glyph);
}

void Aggregate::prepend(Glyph* glyph) {
    AggregateInfo info;
    info.glyph_ = glyph;
    info_->prepend(info);
    Resource::ref(glyph);
}

void Aggregate::insert(GlyphIndex index, Glyph* glyph) {
    AggregateInfo info;
    info.glyph_ = glyph;
    info_->insert(index, info);
    Resource::ref(glyph);
}

void Aggregate::remove(GlyphIndex index) {
    AggregateInfo& info = info_->item_ref(index);
    Resource::unref(info.glyph_);
    info_->remove(index);
}

void Aggregate::replace(GlyphIndex index, Glyph* glyph) {
    AggregateInfo& info = info_->item_ref(index);
    Resource::ref(glyph);
    Resource::unref(info.glyph_);
    info.glyph_ = glyph;
}

void Aggregate::allocate(Canvas* canvas, const Allocation&, Extension& ext) {
    GlyphIndex count = info_->count();
    for (GlyphIndex index = 0; index < count; ++index) {
        AggregateInfo& info = info_->item_ref(index);
        if (info.glyph_ != nil) {
            Allocation& a = info.allocation_;
            Extension& b = info.extension_;
	    b.clear();
            info.glyph_->allocate(canvas, a, b);
            ext.merge(b);
        }
    }
}

void Aggregate::draw(Canvas* canvas, const Allocation&) const {
    GlyphIndex count = info_->count();
    for (GlyphIndex index = 0; index < count; ++index) {
        AggregateInfo& info = info_->item_ref(index);
        if (info.glyph_ != nil) {
            Allocation& a = info.allocation_;
            Extension& b = info.extension_;
            if (canvas->damaged(info.extension_)) {
                info.glyph_->draw(canvas, a);
            }
        }
    }
}

void Aggregate::print(Printer* p, const Allocation&) const {
    GlyphIndex count = info_->count();
    for (GlyphIndex index = 0; index < count; ++index) {
        AggregateInfo& info = info_->item_ref(index);
        if (info.glyph_ != nil) {
            Allocation& a = info.allocation_;
            if (p->damaged(info.extension_)) {
                info.glyph_->print(p, a);
            }
        }
    }
}

void Aggregate::pick(Canvas* c, const Allocation&, int depth, Hit& h) {
    Coord x = h.left();
    Coord y = h.bottom();
    GlyphIndex count = info_->count();
    for (GlyphIndex index = 0; index < count; ++index) {
        AggregateInfo& info = info_->item_ref(index);
        if (info.glyph_ != nil) {
            Extension& b = info.extension_;
            Allocation& a = info.allocation_;
            if (
                x >= b.left() && x < b.right()
                && y >= b.bottom() && y < b.top()
            ) {
		h.begin(depth, this, index);
		info.glyph_->pick(c, a, depth + 1, h);
		h.end();
            }
        }
    }
}
