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
 * Box - tiling glyph
 */

#include <InterViews/align.h>
#include <InterViews/alloctbl.h>
#include <InterViews/box.h>
#include <InterViews/canvas.h>
#include <InterViews/hit.h>
#include <InterViews/printer.h>
#include <InterViews/superpose.h>
#include <InterViews/tile.h>
#include <OS/list.h>
#include <OS/math.h>

class BoxImpl {
private:
    friend class Box;

    Box* box_;
    Layout* layout_;
    boolean requested_;
    Requisition requisition_;
    AllocationTable* allocations_;

    static Extension* empty_ext_;

    void request();
    AllocationInfo& info(Canvas*, const Allocation&, Extension&);
    void offset_allocate(AllocationInfo&, Coord dx, Coord dy);
    void full_allocate(AllocationInfo&);
    void invalidate();
};

Extension* BoxImpl::empty_ext_;

Box::Box(Layout* layout, GlyphIndex size) : PolyGlyph(size) {
    BoxImpl* b = new BoxImpl;
    impl_ = b;
    b->box_ = this;
    b->layout_ = layout;
    b->requested_ = false;
    b->allocations_ = nil;
}

Box::Box(
    Layout* layout,
    Glyph* g1, Glyph* g2, Glyph* g3, Glyph* g4, Glyph* g5,
    Glyph* g6, Glyph* g7, Glyph* g8, Glyph* g9, Glyph* g10
) : PolyGlyph(4) {
    BoxImpl* b = new BoxImpl;
    impl_ = b;
    b->box_ = this;
    b->layout_ = layout;
    b->requested_ = false;
    b->allocations_ = nil;
    if (g1 != nil) {
        append(g1);
    }
    if (g2 != nil) {
        append(g2);
    }
    if (g3 != nil) {
        append(g3);
    }
    if (g4 != nil) {
        append(g4);
    }
    if (g5 != nil) {
        append(g5);
    }
    if (g6 != nil) {
        append(g6);
    }
    if (g7 != nil) {
        append(g7);
    }
    if (g8 != nil) {
        append(g8);
    }
    if (g9 != nil) {
        append(g9);
    }
    if (g10 != nil) {
        append(g10);
    }
}

Box::~Box() {
    BoxImpl* b = impl_;
    delete b->layout_;
    delete b->allocations_;
    delete b;
}

void Box::request(Requisition& requisition) const {
    BoxImpl* b = impl_;
    if (!b->requested_) {
	b->request();
    }
    requisition = b->requisition_;
}

void Box::allocate(Canvas* c, const Allocation& allocation, Extension& ext) {
    impl_->info(c, allocation, ext);
}

void Box::draw(Canvas* c, const Allocation& allocation) const {
    BoxImpl* b = impl_;
    Extension ext;
    ext.clear();
    AllocationInfo& info = b->info(c, allocation, ext);
    if (c->damaged(ext)) {
	Allocation* a = info.component_allocations();
        GlyphIndex n = count();
        for (GlyphIndex i = 0; i < n; i++) {
            Glyph* g = component(i);
	    if (g != nil) {
		g->draw(c, a[i]);
	    }
        }
    }
}

void Box::print(Printer* p, const Allocation& allocation) const {
    BoxImpl* b = impl_;
    Extension ext;
    ext.clear();
    AllocationInfo& info = b->info(p, allocation, ext);
    if (p->damaged(ext)) {
	Allocation* a = info.component_allocations();
        GlyphIndex n = count();
        for (GlyphIndex i = 0; i < n; i++) {
            Glyph* g = component(i);
	    if (g != nil) {
		g->print(p, a[i]);
	    }
        }
    }
}

void Box::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    if (h.right() >= a.left() && h.left() < a.right() &&
	h.top() >= a.bottom() && h.bottom() < a.top()
    ) {
	BoxImpl* b = impl_;
	Extension ext;
	ext.clear();
	AllocationInfo& info = b->info(c, a, ext);
	Allocation* aa = info.component_allocations();
	GlyphIndex n = count();
	for (GlyphIndex i = 0; i < n; i++) {
	    Glyph* g = component(i);
	    if (g != nil) {
		h.begin(depth, this, i);
		g->pick(c, aa[i], depth + 1, h);
		h.end();
	    }
	}
    }
}

void Box::undraw() {
    AllocationTable* table = impl_->allocations_;
    if (table != nil) {
	table->flush();
    }
    PolyGlyph::undraw();
}

void Box::modified(GlyphIndex) {
    impl_->invalidate();
}

void Box::allotment(GlyphIndex index, DimensionName d, Allotment& a) const {
    AllocationTable* table = impl_->allocations_;
    if (table != nil) {
	AllocationInfo* info = table->most_recent();
	if (info != nil) {
	    Allocation* allocations = info->component_allocations();
	    a = allocations[index].allotment(d);
	}
    }
}

/* class BoxImpl */

void BoxImpl::request() {
    GlyphIndex count = box_->count();
    Requisition* r = new Requisition[count];
    for (GlyphIndex i = 0; i < count; i++) {
	Glyph* g = box_->component(i);
	if (g != nil) {
	    g->request(r[i]);
	}
    }
    layout_->request(count, r, requisition_);
    requested_ = true;
    delete r;
}

AllocationInfo& BoxImpl::info(Canvas* c, const Allocation& a, Extension& ext) {
    if (allocations_ == nil) {
	allocations_ = new AllocationTable(box_->count());
    }
    AllocationInfo* info = allocations_->find(c, a);
    if (info == nil) {
	Coord dx, dy;
	info = allocations_->find_same_size(c, a, dx, dy);
	if (info != nil) {
	    info->extension(ext);
	    offset_allocate(*info, dx, dy);
	} else {
	    info = allocations_->allocate(c, a);
	    info->extension(ext);
	    full_allocate(*info);
	}
    }
    ext = info->extension();
    return *info;
}

void BoxImpl::offset_allocate(AllocationInfo& info, Coord dx, Coord dy) {
    Canvas* c = info.canvas();
    Allocation* a = info.component_allocations();
    Extension& box = info.extension();
    Extension child;
    GlyphIndex n = box_->count();
    for (GlyphIndex i = 0; i < n; i++) {
        Glyph* g = box_->component(i);
        if (g != nil) {
	    Allocation& a_i = a[i];
	    Allotment& ax = a_i.x_allotment();
	    Allotment& ay = a_i.y_allotment();
	    ax.offset(dx);
	    ay.offset(dy);
	    child.clear();
            g->allocate(c, a_i, child);
	    box.merge(child);
        }
    }
}

void BoxImpl::full_allocate(AllocationInfo& info) {
    Canvas* c = info.canvas();
    GlyphIndex n = box_->count();
    Allocation* a = info.component_allocations();
    Requisition* r = new Requisition[n];
    GlyphIndex i;
    for (i = 0; i < n; i++) {
        Glyph* g = box_->component(i);
        if (g != nil) {
            g->request(r[i]);
        }
    }
    layout_->allocate(info.allocation(), n, r, a);
    delete r;

    Extension& box = info.extension();
    Extension child;
    for (i = 0; i < n; i++) {
        Glyph* g = box_->component(i);
        if (g != nil) {
	    child.clear();
            g->allocate(c, a[i], child);
	    box.merge(child);
        }
    }
}

void BoxImpl::invalidate() {
    requested_ = false;
    delete allocations_;
    allocations_ = nil;
}
