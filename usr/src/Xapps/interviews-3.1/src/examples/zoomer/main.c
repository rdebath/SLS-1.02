/*
 * Copyright (c) 1987, 1988, 1989 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <IV-look/kit.h>
#include <InterViews/adjust.h>
#include <InterViews/background.h>
#include <InterViews/layout.h>
#include <InterViews/patch.h>
#include <InterViews/session.h>
#include <InterViews/transformer.h>
#include <InterViews/tformsetter.h>
#include <InterViews/window.h>

class ZoomArea : public MonoGlyph, public Adjustable {
public:
    ZoomArea(Glyph*);
    virtual ~ZoomArea();

    virtual void update_area(Patch*);
    virtual void draw(Canvas*, const Allocation&) const;

    virtual Coord lower(DimensionName) const;
    virtual Coord upper(DimensionName) const;
    virtual Coord length(DimensionName) const;
    virtual Coord cur_lower(DimensionName) const;
    virtual Coord cur_upper(DimensionName) const;
    virtual Coord cur_length(DimensionName) const;

    virtual void scroll_to(DimensionName, Coord lower);
private:
    Coord scale_;
    TransformSetter* xform_;
    Patch* area_;
    Patch* body_;

    void set_xform();
};

int main(int argc, char** argv) {
    Session* session = new Session("Himom", argc, argv);
    WidgetKit& kit = *WidgetKit::instance();
    const LayoutKit& layout = *LayoutKit::instance();
    ZoomArea* z = new ZoomArea(
	layout.center(
	    layout.vbox(
		kit.push_button("Hi mom!", nil),
		kit.push_button("Look no hands!", nil)
	    )
	)
    );
    Patch* p = new Patch(
	layout.vcenter(layout.fixed_span(z, 4*72.0, 3*72.0), 1.0)
    );
    z->update_area(p);
    z->small_scroll(Dimension_Y, 0.02);
    z->large_scroll(Dimension_Y, 0.10);
    return session->run_window(
	new ApplicationWindow(
	    kit.inset_frame(
		layout.hbox(p, layout.vcenter(kit.vslider(z), 1.0))
	    )
	)
    );
}

ZoomArea::ZoomArea(Glyph* g) : MonoGlyph(nil) {
    scale_ = 1.0;
    xform_ = new TransformSetter(g);
    set_xform();
    area_ = nil;
    body_ = new Patch(xform_);
    body(body_);
}

ZoomArea::~ZoomArea() { }

void ZoomArea::update_area(Patch* p) {
    area_ = p;
}

void ZoomArea::draw(Canvas* c, const Allocation& a) const {
    c->push_clipping();
    c->clip_rect(a.left(), a.bottom(), a.right(), a.top());
    MonoGlyph::draw(c, a);
    c->pop_clipping();
}

Coord ZoomArea::lower(DimensionName) const { return 0.0; }
Coord ZoomArea::upper(DimensionName) const { return 2.0; }
Coord ZoomArea::length(DimensionName d) const { return upper(d); }
Coord ZoomArea::cur_lower(DimensionName) const { return scale_; }
Coord ZoomArea::cur_upper(DimensionName) const { return scale_; }
Coord ZoomArea::cur_length(DimensionName) const { return 0; }

void ZoomArea::scroll_to(DimensionName d, Coord lower) {
    Coord p = lower;
    constrain(d, p);
    if (p != scale_) {
	scale_ = p;
	notify(Dimension_X);
	notify(Dimension_Y);
	set_xform();
	body_->redraw();
	area_->reallocate();
	body_->redraw();
    }
}

void ZoomArea::set_xform() {
    Transformer t;
    t.scale(scale_, scale_);
    t.rotate(360 * scale_);
    xform_->transformer(t);
}
