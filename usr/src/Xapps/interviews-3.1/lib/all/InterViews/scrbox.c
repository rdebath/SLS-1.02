/*
 * Copyright (c) 1991 Stanford University
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
 * ScrollBox - scrollable list of glyphs
 */

#include <InterViews/canvas.h>
#include <InterViews/hit.h>
#include <InterViews/scrbox.h>
#include <InterViews/transformer.h>
#include <OS/list.h>
#include <OS/math.h>

ScrollBox::ScrollBox(GlyphIndex size) : PolyGlyph(size) { }
ScrollBox::~ScrollBox() { }

boolean ScrollBox::shown(GlyphIndex i) const {
    return i >= 0 && i < count();
}

GlyphIndex ScrollBox::first_shown() const {
    return 0;
}

GlyphIndex ScrollBox::last_shown() const {
    return count() - 1;
}

struct TBScrollBoxInfo {
    /* workaround for g++ bug */
    TBScrollBoxInfo() { }
private:
    friend class TBScrollBox;
    friend class TBScrollBoxImpl;

    Glyph* glyph_;
    Allocation allocation_;
};

declareList(TBScrollBoxList,TBScrollBoxInfo)
implementList(TBScrollBoxList,TBScrollBoxInfo)

class TBScrollBoxImpl {
private:
    friend class TBScrollBox;

    ScrollBox* scrollbox_;
    GlyphIndex start_;
    GlyphIndex end_;
    boolean changed_;
    Requisition requisition_;
    Canvas* canvas_;
    Transformer transformer_;
    Allocation allocation_;
    Extension extension_;
    TBScrollBoxList visible_;

    void check(Canvas*, const Allocation&);
    void refresh();
    void reallocate();
    void redraw();
    void undraw_range(GlyphIndex begin, GlyphIndex end);
};

TBScrollBox::TBScrollBox(GlyphIndex size) : ScrollBox(size) {
    impl_ = new TBScrollBoxImpl;
    TBScrollBoxImpl& sb = *impl_;
    sb.scrollbox_ = this;
    sb.start_ = 0;
    sb.end_ = 0;
    sb.changed_ = true;
    sb.canvas_ = nil;
}

TBScrollBox::~TBScrollBox() {
    delete impl_;
}

void TBScrollBox::request(Requisition& req) const {
    GlyphIndex n = count();
    TBScrollBoxImpl& sb = *impl_;
    if (sb.changed_) {
	Requisition r;
	const Requirement& rx = r.x_requirement();
	const Requirement& ry = r.y_requirement();
	Coord natural_width = 0.0;
	Coord natural_height = 0.0;
	for (GlyphIndex i = 0; i < n; i++) {
	    Glyph* g = component(i);
	    if (g != nil) {
		g->request(r);
		Coord r_width = rx.natural();
		if (r_width > natural_width) {
		    natural_width = r_width;
		}
		natural_height += ry.natural();
	    }
	}
	Requirement& box_x = sb.requisition_.x_requirement();
	box_x.natural(natural_width);
	box_x.stretch(fil);
	box_x.shrink(natural_width);
	box_x.alignment(0.0);

	Requirement& box_y = sb.requisition_.y_requirement();
	box_y.natural(natural_height);
	box_y.stretch(fil);
	box_y.shrink(natural_height);
	box_y.alignment(1.0);
	sb.changed_ = false;
    }
    req = sb.requisition_;
}

void TBScrollBox::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    TBScrollBoxImpl& sb = *impl_;
    if (sb.changed_) {
	Requisition req;
	request(req);
    }
    ext.set(c, a);
    sb.canvas_ = c;
    if (c != nil) {
	sb.transformer_ = c->transformer();
    }
    sb.allocation_ = a;
    sb.extension_ = ext;
    sb.reallocate();
    notify(Dimension_X);
    notify(Dimension_Y);
}

void TBScrollBox::draw(Canvas* c, const Allocation& a) const {
    TBScrollBoxImpl& sb = *impl_;
    sb.check(c, a);
    Extension& e = sb.extension_;
    if (sb.canvas_->damaged(e)) {
	if (sb.changed_) {
	    sb.refresh();
	}
	c->push_clipping();
	c->clip_rect(a.left(), a.bottom(), a.right(), a.top());
	for (ListItr(TBScrollBoxList) i(sb.visible_); i.more(); i.next()) {
	    const TBScrollBoxInfo& info = i.cur_ref();
	    Glyph* g = info.glyph_;
	    g->draw(c, info.allocation_);
	}
	c->pop_clipping();
    }
}

void TBScrollBox::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    TBScrollBoxImpl& sb = *impl_;
    sb.check(c, a);
    if (h.left() < a.right() && h.right() >= a.left() &&
	h.bottom() < a.top() && h.top() >= a.bottom()
    ) {
	if (sb.changed_) {
	    sb.refresh();
	}
	GlyphIndex n = sb.start_;
	for (ListItr(TBScrollBoxList) i(sb.visible_); i.more(); i.next()) {
	    const TBScrollBoxInfo& info = i.cur_ref();
	    Glyph* g = info.glyph_;
	    h.begin(depth, this, n);
	    g->pick(c, info.allocation_, depth + 1, h);
	    h.end();
	    ++n;
	}
    }
}

void TBScrollBox::undraw() {
    impl_->canvas_ = nil;
    ScrollBox::undraw();
}

void TBScrollBox::modified(GlyphIndex) {
    impl_->changed_ = true;
}

boolean TBScrollBox::shown(GlyphIndex i) const {
    TBScrollBoxImpl& sb = impl();
    return i >= sb.start_ && i < sb.end_;
}

GlyphIndex TBScrollBox::first_shown() const {
    TBScrollBoxImpl& sb = impl();
    return sb.start_;
}

GlyphIndex TBScrollBox::last_shown() const {
    TBScrollBoxImpl& sb = impl();
    return sb.end_ - 1;
}

void TBScrollBox::allotment(
    GlyphIndex i, DimensionName d, Allotment& a
) const {
    TBScrollBoxImpl& sb = impl();
    if (i >= sb.start_ && i < sb.end_) {
	a = sb.visible_.item(i - sb.start_).allocation_.allotment(d);
    }
}

Coord TBScrollBox::lower(DimensionName) const {
    return Coord(0);
}

Coord TBScrollBox::upper(DimensionName) const {
    return Coord(count() - 1);
}

Coord TBScrollBox::length(DimensionName) const {
    return Coord(count());
}

Coord TBScrollBox::cur_lower(DimensionName) const {
    TBScrollBoxImpl& sb = impl();
    return Coord(count() - sb.end_);
}

Coord TBScrollBox::cur_upper(DimensionName) const {
    TBScrollBoxImpl& sb = impl();
    return Coord(count() - 1 - sb.start_);
}

Coord TBScrollBox::cur_length(DimensionName) const {
    TBScrollBoxImpl& sb = impl();
    return Coord(sb.end_ - sb.start_);
}

void TBScrollBox::scroll_forward(DimensionName d) {
    scroll_by(d, -long(small_scroll(d)));
}

void TBScrollBox::scroll_backward(DimensionName d) {
    scroll_by(d, long(small_scroll(d)));
}

void TBScrollBox::page_forward(DimensionName d) {
    scroll_by(d, -long(large_scroll(d)));
}

void TBScrollBox::page_backward(DimensionName d) {
    scroll_by(d, long(large_scroll(d)));
}

void TBScrollBox::scroll_to(DimensionName d, Coord lower) {
    TBScrollBoxImpl& sb = impl();
    GlyphIndex max_end = count();
    GlyphIndex new_end = max_end - Math::round(lower);
    GlyphIndex new_start = new_end - sb.end_ + sb.start_;
    do_scroll(d, new_start, new_end);
}

TBScrollBoxImpl& TBScrollBox::impl() const {
    TBScrollBoxImpl& sb = *impl_;
    if (sb.changed_) {
	sb.refresh();
    }
    return sb;
}

void TBScrollBox::scroll_by(DimensionName d, long offset) {
    TBScrollBoxImpl& sb = impl();
    do_scroll(d, sb.start_ + offset, sb.end_ + offset);
}

void TBScrollBox::do_scroll(
    DimensionName d, GlyphIndex new_start, GlyphIndex new_end
) {
    TBScrollBoxImpl& sb = *impl_;
    GlyphIndex max_end = count();
    if (new_start < 0) {
	new_start = 0;
    }
    if (new_end > max_end) {
	new_start -= (new_end - max_end);
	new_end = max_end;
    }
    if (new_start != sb.start_ || new_end != sb.end_) {
	sb.undraw_range(sb.start_, new_start - 1);
	GlyphIndex old_end = sb.end_;
	sb.start_ = new_start;
	sb.end_ = new_end;
	sb.reallocate();
	sb.undraw_range(sb.end_, old_end - 1);
	sb.redraw();
	notify(d);
    }
}

/* class TBScrollBoxImpl */

void TBScrollBoxImpl::check(Canvas* c, const Allocation& a) {
    if (canvas_ == nil || canvas_ != c ||
	transformer_ != c->transformer() || !allocation_.equals(a, 1e-4)
    ) {
	Extension ext;
	scrollbox_->allocate(c, a, ext);
    }
}

void TBScrollBoxImpl::refresh() {
    Requisition req;
    scrollbox_->request(req);
    start_ = 0;
    reallocate();
    redraw();
}

void TBScrollBoxImpl::reallocate() {
    if (canvas_ == nil) {
	return;
    }
    ScrollBox* s = scrollbox_;
    GlyphIndex n = s->count();
    end_ = n;
    TBScrollBoxList& list = visible_;
    list.remove_all();
    Requisition req;
    TBScrollBoxInfo info;
    Extension e_i;
    const Requirement& r = req.y_requirement();
    Coord p = allocation_.top();
    Coord bottom = allocation_.bottom();
    boolean found_start = false;
    for (GlyphIndex i = start_; i < n; i++) {
	Glyph* g = s->component(i);
	if (g != nil) {
	    g->request(req);
	    Coord span = r.natural();
	    if (!Math::equal(span, Coord(0), float(1e-2))) {
		if (!found_start) {
		    start_ = i;
		    found_start = true;
		}
		Coord alignment = r.alignment();
		p -= span;
		if (p < bottom) {
		    end_ = i;
		    break;
		}
		info.glyph_ = g;
		Allotment& ax = info.allocation_.x_allotment();
		ax = allocation_.x_allotment();
		Allotment& ay = info.allocation_.y_allotment();
		ay.span(span);
		ay.origin(p + Coord(alignment * span));
		ay.alignment(alignment);
		list.append(info);
		g->allocate(canvas_, info.allocation_, e_i);
	    }
	}
    }
}

void TBScrollBoxImpl::redraw() {
    if (canvas_ != nil) {
	canvas_->damage(extension_);
    }
}

void TBScrollBoxImpl::undraw_range(GlyphIndex begin, GlyphIndex end) {
    ScrollBox* s = scrollbox_;
    for (GlyphIndex i = begin; i <= end; i++) {
	Glyph* g = s->component(i);
	if (g != nil) {
	    g->undraw();
	}
    }
}
