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
 * Page - arbitrary placements
 */

#include <InterViews/canvas.h>
#include <InterViews/hit.h>
#include <InterViews/page.h>
#include <InterViews/printer.h>
#include <OS/list.h>

static const int PageInfoAllocated = 0x01;
static const int PageInfoExtended = 0x02;
static const int PageInfoPositioned = 0x04;
static const int PageInfoHidden = 0x08;

class PageInfo {
public:
    PageInfo();

    Glyph* glyph_;
    Allocation allocation_;
    Extension extension_;
    Coord x_;
    Coord y_;
    int status_;
};

PageInfo::PageInfo() {
    glyph_ = nil;
    x_ = 0;
    y_ = 0;
    status_ = 0;
}

declareList(PageInfo_List,PageInfo)
implementList(PageInfo_List,PageInfo)

static const float epsilon = 0.1;

Page::Page(Glyph* bg) : Glyph() {
    Resource::ref(bg);
    background_ = bg;
    canvas_ = nil;
    info_ = new PageInfo_List();
}

Page::~Page() {
    GlyphIndex count = info_->count();
    for (GlyphIndex i = 0; i < count; ++i) {
        PageInfo& info = info_->item_ref(i);
	Resource::unref(info.glyph_);
    }
    delete info_;
    info_ = nil;
    Resource::unref(background_);
    canvas_ = nil;
}

Coord Page::left() const { return allocation_.left(); }
Coord Page::right() const { return allocation_.right(); }
Coord Page::bottom() const { return allocation_.bottom(); }
Coord Page::top() const { return allocation_.top(); }
Coord Page::x() const { return allocation_.x(); }
Coord Page::y() const { return allocation_.y(); }

void Page::show(GlyphIndex index, boolean showing) {
    PageInfo& info = info_->item_ref(index);
    if (((info.status_ & PageInfoHidden) == 0) != showing) {
        if (canvas_ != nil) {
            canvas_->damage(info.extension_);
        }
        if (showing) {
            info.status_ &= ~PageInfoHidden;
        } else {
            info.status_ |= PageInfoHidden;
        }
    }
}

boolean Page::showing(GlyphIndex index) const {
    return (info_->item_ref(index).status_ & PageInfoHidden) == 0;
}

void Page::move(GlyphIndex index, Coord x, Coord y) {
    PageInfo& info = info_->item_ref(index);
    if (
        (info.status_ & PageInfoAllocated)
        && (info.x_ != x || info.y_ != y)
    ) {
        Extension& b = info.extension_;
        Allocation& a = info.allocation_;
        Allotment& ax = a.allotment(Dimension_X);
        Allotment& ay = a.allotment(Dimension_Y);
        Coord newx = allocation_.x() + x;
        Coord newy = allocation_.y() + y;
        Allotment n_ax(newx, ax.span(), ax.alignment());
        Allotment n_ay(newy, ay.span(), ay.alignment());
        a.allot(Dimension_X, n_ax);
        a.allot(Dimension_Y, n_ay);
        if (canvas_ != nil) {
            canvas_->damage(b);
        }
	b.clear();
        info.glyph_->allocate(canvas_, a, b);
        if (canvas_ != nil) {
            canvas_->damage(b);
        }
    }
    info.x_ = x;
    info.y_ = y;
}

void Page::location(GlyphIndex index, Coord& x, Coord& y) {
    PageInfo& info = info_->item_ref(index);
    x = info.x_;
    y = info.y_;
}

GlyphIndex Page::count() const {
    return info_->count();
}

Glyph* Page::component(GlyphIndex index) const {
    return info_->item_ref(index).glyph_;
}

void Page::allotment(GlyphIndex index, DimensionName res, Allotment& a) const {
    a = info_->item_ref(index).allocation_.allotment(res);
}

void Page::change(GlyphIndex index) {
    PageInfo& info = info_->item_ref(index);
    info.status_ &= ~(PageInfoAllocated);
}

void Page::append(Glyph* glyph) {
    PageInfo info;
    info.glyph_ = glyph;
    info.status_ = 0;
    info.x_ = 0;
    info.y_ = 0;
    info_->append(info);
    Resource::ref(glyph);
}

void Page::prepend(Glyph* glyph) {
    PageInfo info;
    info.glyph_ = glyph;
    info.status_ = 0;
    info.x_ = 0;
    info.y_ = 0;
    info_->prepend(info);
    Resource::ref(glyph);
}

void Page::insert(GlyphIndex index, Glyph* glyph) {
    PageInfo info;
    info.glyph_ = glyph;
    info.status_ = 0;
    info.x_ = 0;
    info.y_ = 0;
    info_->insert(index, info);
    Resource::ref(glyph);
}

void Page::remove(GlyphIndex index) {
    PageInfo& info = info_->item_ref(index);
    if (canvas_ != nil && (info.status_ & PageInfoAllocated)) {
        canvas_->damage(info.extension_);
    }
    Resource::unref(info.glyph_);
    info_->remove(index);
}

void Page::replace(GlyphIndex index, Glyph* glyph) {
    PageInfo& info = info_->item_ref(index);
    if (canvas_ != nil && (info.status_ & PageInfoAllocated)) {
        canvas_->damage(info.extension_);
    }
    Resource::ref(glyph);
    Resource::unref(info.glyph_);
    info.glyph_ = glyph;
    info.status_ &= ~(PageInfoAllocated);
}

void Page::request(Requisition& requisition) const {
    if (background_ != nil) {
        background_->request(requisition);
    }
}

void Page::allocate(Canvas* c, const Allocation& allocation, Extension& ext) {
    canvas_ = c;
    allocation_ = allocation;
    if (background_ != nil) {
        background_->allocate(c, allocation, ext);
    }
    GlyphIndex count = info_->count();
    for (GlyphIndex index = 0; index < count; ++index) {
        PageInfo& info = info_->item_ref(index);
        if (info.glyph_ != nil) {
            Allocation& a = info.allocation_;
            Extension& b = info.extension_;
            Requisition s;
            info.glyph_->request(s);
            Allotment ax = Allotment(
                allocation.x() + info.x_,
                s.requirement(Dimension_X).natural(),
                s.requirement(Dimension_X).alignment()
            );
            Allotment ay = Allotment(
                allocation.y() + info.y_,
                s.requirement(Dimension_Y).natural(),
                s.requirement(Dimension_Y).alignment()
            );
            if (
                !(info.status_ & PageInfoAllocated)
                || !ax.equals(a.allotment(Dimension_X), epsilon)
                || !ay.equals(a.allotment(Dimension_Y), epsilon)
            ) {
                if (c != nil && (info.status_ & PageInfoExtended)) {
                    c->damage(b);
                }
                a.allot(Dimension_X, ax);
                a.allot(Dimension_Y, ay);
		b.clear();
                info.glyph_->allocate(c, a, b);
                if (c != nil) {
                    c->damage(b);
                }
            }
            info.status_ |= PageInfoAllocated|PageInfoExtended;
            ext.merge(b);
        }
    }
}

void Page::draw(Canvas* canvas, const Allocation& a) const {
    if (background_ != nil) {
        background_->draw(canvas, a);
    }
    GlyphIndex count = info_->count();
    for (GlyphIndex index = 0; index < count; ++index) {
        PageInfo& info = info_->item_ref(index);
        if (info.glyph_ != nil && !(info.status_ & PageInfoHidden)) {
            Allocation& a = info.allocation_;
            Extension& b = info.extension_;
            if (canvas->damaged(b)) {
                info.glyph_->draw(canvas, a);
            }
        }
    }
}

void Page::print(Printer* p, const Allocation& a) const {
    if (background_ != nil) {
        background_->print(p, a);
    }
    GlyphIndex count = info_->count();
    for (GlyphIndex index = 0; index < count; ++index) {
        PageInfo& info = info_->item_ref(index);
        if (info.glyph_ != nil && !(info.status_ & PageInfoHidden)) {
            Allocation& a = info.allocation_;
            Extension& b = info.extension_;
            if (p->damaged(b)) {
                info.glyph_->print(p, a);
            }
        }
    }
}

void Page::undraw() {
    if (background_ != nil) {
	background_->undraw();
    }
    GlyphIndex count = info_->count();
    for (GlyphIndex i = 0; i < count; i++) {
        const PageInfo& info = info_->item_ref(i);
	Glyph* g = info.glyph_;
	if (g != nil) {
	    g->undraw();
	}
    }
    canvas_ = nil;
}

void Page::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    if (background_ != nil) {
        background_->pick(c, a, depth, h);
    }
    GlyphIndex count = info_->count();
    for (GlyphIndex index = 0; index < count; ++index) {
        PageInfo& info = info_->item_ref(index);
        if (info.glyph_ != nil && !(info.status_ & PageInfoHidden)) {
            Allocation& a = info.allocation_;
            if (h.right() >= a.left() && h.left() < a.right() &&
                h.top() >= a.bottom() && h.bottom() < a.top()
            ) {
		h.begin(depth, this, index);
                info.glyph_->pick(c, a, depth + 1, h);
		h.end();
            }
        }
    }
}
