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
 * Slider - common slider behavior
 */

#include <IV-look/slider.h>
#include <IV-look/stepper.h>
#include <IV-look/telltale.h>
#include <InterViews/adjust.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/display.h>
#include <InterViews/event.h>
#include <InterViews/patch.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <OS/math.h>

#define default_minimum_thumb_size 28.0

class SliderImpl {
private:
    friend class Slider;

    Glyph* normal_thumb_;
    Glyph* visible_thumb_;
    Patch* thumb_patch_;
    Patch* old_thumb_;
    Coord min_thumb_size_;
    boolean dragging_ : 1;
    boolean aborted_ : 1;
    boolean showing_old_thumb_ : 1;
    Stepper* forward_;
    Stepper* backward_;
    Stepper* stepper_;
    Coord xoffset_;
    Coord yoffset_;

    int hit_thumb(const Event&);
};

Slider::Slider(Style* style) : ActiveHandler(nil, style) {
    SliderImpl* s = new SliderImpl;
    impl_ = s;
    s->normal_thumb_ = nil;
    s->visible_thumb_ = nil;
    s->thumb_patch_ = nil;
    s->old_thumb_ = nil;
    s->min_thumb_size_ = default_minimum_thumb_size;
    style->find_attribute("minimumThumbSize", s->min_thumb_size_);
    s->dragging_ = false;
    s->aborted_ = false;
    s->showing_old_thumb_ = false;
    s->forward_ = nil;
    s->backward_ = nil;
    s->stepper_ = nil;
    s->xoffset_ = 0.0;
    s->yoffset_ = 0.0;
}

Slider::~Slider() {
    SliderImpl* s = impl_;
    Resource::unref(s->normal_thumb_);
    Resource::unref(s->visible_thumb_);
    Resource::unref(s->old_thumb_);
    Resource::unref(s->thumb_patch_);
    Resource::unref(s->forward_);
    Resource::unref(s->backward_);
    delete s;
}

void Slider::normal_thumb(Glyph* g) {
    SliderImpl& s = *impl_;
    Resource::ref(g);
    Resource::unref(s.normal_thumb_);
    s.normal_thumb_ = g;
    Resource::unref(s.thumb_patch_);
    s.thumb_patch_ = new Patch(g);
    Resource::ref(s.thumb_patch_);
}

void Slider::visible_thumb(Glyph* g) {
    SliderImpl& s = *impl_;
    Resource::ref(g);
    Resource::unref(s.visible_thumb_);
    s.visible_thumb_ = g;
}

void Slider::old_thumb(Glyph* g) {
    SliderImpl& s = *impl_;
    Patch* patch = new Patch(g);
    Resource::ref(patch);
    Resource::unref(s.old_thumb_);
    s.old_thumb_ = patch;
}

Coord Slider::minimum_thumb_size() const {
    return impl_->min_thumb_size_;
}

void Slider::request(Requisition& req) const {
    Requirement default_size(22.0, fil, 22.0, 0.0);
    req.require(Dimension_X, default_size);
    req.require(Dimension_Y, default_size);
}

void Slider::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    ext.merge(c, a);
    ActiveHandler::allocate(c, a, ext);
}

void Slider::draw(Canvas* c, const Allocation& a) const {
    SliderImpl& s = *impl_;
    c->push_clipping();
    c->clip_rect(a.left(), a.bottom(), a.right(), a.top());
    if (s.showing_old_thumb_) {
	s.old_thumb_->draw(c, s.old_thumb_->allocation());
    }
    s.thumb_patch_->draw(c, s.thumb_patch_->allocation());
    c->pop_clipping();
}

void Slider::undraw() {
    SliderImpl& s = *impl_;
    if (s.thumb_patch_ != nil) {
	s.thumb_patch_->undraw();
    }
    if (s.old_thumb_ != nil) {
	s.old_thumb_->undraw();
    }
    ActiveHandler::undraw();
}

void Slider::move(const Event& e) {
    SliderImpl& s = *impl_;
    if (s.visible_thumb_ != nil) {
	Glyph* g = (s.hit_thumb(e) == 0) ? s.visible_thumb_ : s.normal_thumb_;
	Patch& thumb = *s.thumb_patch_;
	if (thumb.body() != g) {
	    thumb.body(g);
	    thumb.reallocate();
	    thumb.redraw();
	}
    }
    ActiveHandler::move(e);
}

void Slider::press(const Event& e) {
    EventButton b = e.pointer_button();
    if (b == Event::right) {
	return;
    }

    SliderImpl& s = *impl_;
    Coord x = e.pointer_x();
    Coord y = e.pointer_y();
    const Allocation& slider = allocation();
    const Allocation& a = s.thumb_patch_->allocation();
    int rel = s.hit_thumb(e);
    if (rel == 0) {
	apply_adjustment(&Adjustable::begin_adjustment);
	s.xoffset_ = slider.left() + x - a.left();
	s.yoffset_ = slider.bottom() + y - a.bottom();
	s.dragging_ = true;
    } else if (b == Event::left) {
	s.stepper_ = (rel == 1) ? s.forward_ : s.backward_;
	if (s.stepper_ != nil) {
	    s.stepper_->start_stepping();
	}
    } else {
	/* middle ==> drag */
	apply_adjustment(&Adjustable::begin_adjustment);
	s.dragging_ = true;
	s.xoffset_ = slider.left() + (a.right() - a.left()) / 2.0;
	s.yoffset_ = slider.bottom() + (a.top() - a.bottom()) / 2.0;
	move_to(x - s.xoffset_, y - s.yoffset_);
	move(e);
    }
}

void Slider::drag(const Event& e) {
    SliderImpl& s = *impl_;
    if (!s.aborted_ && s.dragging_) {
	if (!s.showing_old_thumb_ && s.old_thumb_ != nil) {
	    s.showing_old_thumb_ = true;
	    Extension ext;
	    ext.clear();
	    s.old_thumb_->allocate(
		canvas(), s.thumb_patch_->allocation(), ext
	    );
	}
	move_to(e.pointer_x() - s.xoffset_, e.pointer_y() - s.yoffset_);
    }
}

void Slider::release(const Event& e) {
    SliderImpl& s = *impl_;
    if (s.dragging_) {
	if (s.showing_old_thumb_) {
	    s.showing_old_thumb_ = false;
	    s.old_thumb_->redraw();
	}
	s.dragging_ = false;
	if (s.aborted_) {
	    s.aborted_ = false;
	    return;
	}
	move_to(e.pointer_x() - s.xoffset_, e.pointer_y() - s.yoffset_);
	redraw_thumb();
	move(e);
	apply_adjustment(&Adjustable::commit_adjustment);
    } else if (s.stepper_ != nil) {
	s.stepper_->stop_stepping();
	s.stepper_ = nil;
	move(e);
    }
}

void Slider::allocation_changed(Canvas*, const Allocation& a) {
    allocate_thumb(a);
}

void Slider::update(Observable*) {
    if (canvas() != nil) {
	allocate_thumb(allocation());
    }
}

void Slider::forward_stepper(Stepper* stepper) {
    SliderImpl& s = *impl_;
    Resource::ref(stepper);
    Resource::unref(s.forward_);
    s.forward_ = stepper;
}

void Slider::backward_stepper(Stepper* stepper) {
    SliderImpl& s = *impl_;
    Resource::ref(stepper);
    Resource::unref(s.backward_);
    s.backward_ = stepper;
}

void Slider::move_to(Coord, Coord) { }
void Slider::allocate_thumb(const Allocation&) { }
void Slider::apply_adjustment(SliderAdjustment) { }

void Slider::do_adjustment(
    Adjustable* a, SliderAdjustment s, DimensionName d
) {
    if (a != nil) {
	(a->*s)(d);
    }
}

void Slider::allot_thumb_major_axis(
    const Allocation& slider, DimensionName d, Adjustable* adj,
    Coord min_thumb_size, float& scale, Allotment& new_a
) {
    const Allotment& a = slider.allotment(d);
    Coord length = adj->length(d);
    Coord cur_length = adj->cur_length(d);
    Coord slider_size = a.span();
    Coord thumb_size;
    Coord thumb_start;
    if (Math::equal(length, float(0.0), float(1e-3)) ||
	Math::equal(length, cur_length, float(1e-3))
    ) {
	thumb_size = slider_size;
	thumb_start = 0.0;
	scale = 1.0;
    } else {
	thumb_size = slider_size * cur_length / length;
	if (thumb_size > slider_size) {
	    thumb_size = slider_size;
	    thumb_start = 0.0;
	    scale = 1.0;
	} else {
	    if (thumb_size < min_thumb_size) {
		thumb_size = min_thumb_size;
	    }
	    scale = (slider_size - thumb_size) / (length - cur_length);
	    thumb_start = scale * (adj->cur_lower(d) - adj->lower(d));
	}
    }
    new_a.origin(a.begin() + thumb_start);
    new_a.span(thumb_size);
    new_a.alignment(0.0);
}

void Slider::allot_thumb_minor_axis(const Allotment& a, Allotment& new_a) {
    new_a.origin(a.begin());
    new_a.span(a.span());
    new_a.alignment(0.0);
}

void Slider::redraw_thumb() {
    impl_->thumb_patch_->redraw();
}

void Slider::reallocate_thumb(const Allocation& a) {
    Patch& thumb = *impl_->thumb_patch_;
    Extension ext;
    ext.clear();
    thumb.allocate(canvas(), a, ext);
    thumb.redraw();
}

/* class SliderImpl */

int SliderImpl::hit_thumb(const Event& event) {
    Coord x = event.pointer_x();
    Coord y = event.pointer_y();
    const Extension& e = thumb_patch_->extension();
    if (x >= e.left() && x < e.right() && y >= e.bottom() && y < e.top()) {
	return 0;
    }
    if (x < e.left() || y < e.bottom()) {
	return -1;
    }
    return 1;
}

/* class XSlider */

XSlider::XSlider(Style* s, Adjustable* a) : Slider(s) {
    adjustable_ = a;
    a->attach(Dimension_X, this);
    TelltaleState* t = new TelltaleState;
    forward_stepper(new ForwardPager(nil, s, t, a, Dimension_X));
    backward_stepper(new BackwardPager(nil, s, t, a, Dimension_X));
}

XSlider::~XSlider() {
    if (adjustable_ != nil) {
	adjustable_->detach(Dimension_X, this);
    }
}

void XSlider::move_to(Coord x, Coord) {
    Adjustable* a = adjustable_;
    a->scroll_to(Dimension_X, a->lower(Dimension_X) + x / xscale_);
}

void XSlider::allocate_thumb(const Allocation& a) {
    redraw_thumb();
    Allocation thumb_a;
    allot_thumb_major_axis(
	a, Dimension_X, adjustable_, minimum_thumb_size(),
	xscale_, thumb_a.x_allotment()
    );
    allot_thumb_minor_axis(a.y_allotment(), thumb_a.y_allotment());
    reallocate_thumb(thumb_a);
}

void XSlider::disconnect(Observable*) {
    adjustable_ = nil;
}

void XSlider::apply_adjustment(SliderAdjustment s) {
    do_adjustment(adjustable_, s, Dimension_X);
}

/* class YSlider */

YSlider::YSlider(Style* s, Adjustable* a) : Slider(s) {
    adjustable_ = a;
    a->attach(Dimension_Y, this);
    TelltaleState* t = new TelltaleState;
    forward_stepper(new ForwardPager(nil, s, t, a, Dimension_Y));
    backward_stepper(new BackwardPager(nil, s, t, a, Dimension_Y));
}

YSlider::~YSlider() {
    if (adjustable_ != nil) {
	adjustable_->detach(Dimension_Y, this);
    }
}

void YSlider::move_to(Coord, Coord y) {
    Adjustable* a = adjustable_;
    a->scroll_to(Dimension_Y, a->lower(Dimension_Y) + y / yscale_);
}

void YSlider::allocate_thumb(const Allocation& a) {
    redraw_thumb();
    Allocation thumb_a;
    allot_thumb_major_axis(
	a, Dimension_Y, adjustable_, minimum_thumb_size(),
	yscale_, thumb_a.y_allotment()
    );
    allot_thumb_minor_axis(a.x_allotment(), thumb_a.x_allotment());
    reallocate_thumb(thumb_a);
}

void YSlider::disconnect(Observable*) {
    adjustable_ = nil;
}

void YSlider::apply_adjustment(SliderAdjustment s) {
    do_adjustment(adjustable_, s, Dimension_Y);
}

/* class XYSlider */

XYSlider::XYSlider(Style* s, Adjustable* x, Adjustable* y) : Slider(s) {
    x_adjustable_ = x;
    y_adjustable_ = y;
    x->attach(Dimension_X, this);
    y->attach(Dimension_Y, this);
}

XYSlider::~XYSlider() {
    if (x_adjustable_ != nil) {
	x_adjustable_->detach(Dimension_X, this);
    }
    if (y_adjustable_ != nil) {
	y_adjustable_->detach(Dimension_Y, this);
    }
}

void XYSlider::move_to(Coord x, Coord y) {
    Adjustable* x_adjust = x_adjustable_;
    Adjustable* y_adjust = y_adjustable_;
    x_adjust->scroll_to(
	Dimension_X, x_adjust->lower(Dimension_X) + x / xscale_
    );
    y_adjust->scroll_to(
	Dimension_Y, y_adjust->lower(Dimension_Y) + y / yscale_
    );
}

void XYSlider::allocate_thumb(const Allocation& a) {
    redraw_thumb();
    Allocation thumb_a;
    allot_thumb_major_axis(
	a, Dimension_X, x_adjustable_, minimum_thumb_size(),
	xscale_, thumb_a.x_allotment()
    );
    allot_thumb_major_axis(
	a, Dimension_Y, y_adjustable_, minimum_thumb_size(),
	yscale_, thumb_a.y_allotment()
    );
    reallocate_thumb(thumb_a);
}

void XYSlider::disconnect(Observable*) {
    x_adjustable_ = nil;
    y_adjustable_ = nil;
}

void XYSlider::apply_adjustment(SliderAdjustment s) {
    do_adjustment(x_adjustable_, s, Dimension_X);
    do_adjustment(y_adjustable_, s, Dimension_Y);
}
