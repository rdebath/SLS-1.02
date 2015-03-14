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
 * MonoKit -- object for creating common UI Motif-ish objects
 */

#include <IV-look/bevel.h>
#include <IV-look/choice.h>
#include <IV-look/mono_kit.h>
#include <IV-look/slider.h>
#include <InterViews/adjust.h>
#include <InterViews/background.h>
#include <InterViews/bitmap.h>
#include <InterViews/border.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/display.h>
#include <InterViews/font.h>
#include <InterViews/hit.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/monoglyph.h>
#include <InterViews/label.h>
#include <OS/list.h>
#include <OS/string.h>
#include <stdio.h>

static PropertyData kit_props[] = {
    { "*frameThickness", "2.0" },
    { "*moverSize", "20.0" },
    { "*radioScale", "0.7" },
    { "*sliderSize", "20.0" },
    { "*toggleScale", "0.5" },
    { "*FileChooser*filter", "on" },
    { "*FieldEditor*beveled", "on" },
    { "*Panner*minimumThumbSize", "18.0" },
    { "*ScrollBar*minimumThumbSize", "20.0" },
    { "*double_buffered", "on" },
    nil
};

class MonoKitInfo : public Resource {
public:
    MonoKitInfo(Style*);
    virtual ~MonoKitInfo();

    Style* style() const;

    Coord thickness() const;
    float toggle_scale() const;
    float radio_scale() const;
    Coord mover_size() const;
    Coord slider_size() const;

    const Color* flat() const;
    const Color* light() const;
    const Color* dull() const;
    const Color* dark() const;
    const Color* gray_out() const;
private:
    Style* style_;

    Coord thickness_;
    float toggle_scale_;
    float radio_scale_;
    Coord mover_size_;
    Coord slider_size_;

    const Color* flat_;
    const Color* light_;
    const Color* dull_;
    const Color* dark_;
    const Color* gray_out_;

    void load();
    void unload();
};

inline Style* MonoKitInfo::style() const { return style_; }

inline Coord MonoKitInfo::thickness() const { return thickness_; }
inline float MonoKitInfo::toggle_scale() const { return toggle_scale_; }
inline float MonoKitInfo::radio_scale() const { return radio_scale_; }
inline Coord MonoKitInfo::mover_size() const { return mover_size_; }
inline Coord MonoKitInfo::slider_size() const { return slider_size_; }

inline const Color* MonoKitInfo::flat() const { return flat_; }
inline const Color* MonoKitInfo::light() const { return light_; }
inline const Color* MonoKitInfo::dull() const { return dull_; }
inline const Color* MonoKitInfo::dark() const { return dark_; }
inline const Color* MonoKitInfo::gray_out() const { return gray_out_; }

class MonoKitInfo;

declarePtrList(MonoKitInfoList,MonoKitInfo)
implementPtrList(MonoKitInfoList,MonoKitInfo)

class MonoKitFrame : public BevelFrame {
public:
    MonoKitFrame(
	Glyph*, TelltaleState*, MonoKitInfo*, Coord,
	boolean target, boolean choosable
    );
    virtual ~MonoKitFrame();

    const MonoKitInfo& info() const;

    virtual void draw(Canvas*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);

    virtual void draw_frame(Canvas*, const Allocation&, Coord) const;
private:
    TelltaleState* state_;
    MonoKitInfo* info_;
    boolean target_;
    boolean choosable_;
};

const MonoKitInfo& MonoKitFrame::info() const { return *info_; }

class MonoKitMenuItem : public MonoGlyph {
public:
    MonoKitMenuItem(Glyph*, TelltaleState*);
    virtual ~MonoKitMenuItem();

    virtual void draw(Canvas*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);
private:
    TelltaleState* state_;
};

class MonoKitForeground : public MonoGlyph {
public:
    MonoKitForeground(Glyph*, const Color*);
    virtual ~MonoKitForeground();

    virtual void draw(Canvas*, const Allocation&) const;
private:
    const Color* foreground_;
};

class MonoKitImpl {
private:
    friend class MonoKit;

    MonoKit* kit_;
    LayoutKit* layout_;
    MonoKitInfo* info_;
    MonoKitInfoList info_list_;
    const char * saved_text_;

    MonoKitImpl(MonoKit*);
    ~MonoKitImpl();

    boolean match(const Style&, const Style&, const char*);
    Glyph* make_menu_item(TelltaleState*, Glyph* check, Glyph*);
    ChoiceItem* make_button(TelltaleState*, Glyph* normal, Glyph* pressed);
    ChoiceItem* make_button(
        TelltaleState*, Glyph* normal, Glyph* pressed, Glyph* disabled
    );
    ChoiceItem* make_toggle(TelltaleState*, Beveler, float scale);
    ChoiceItem* make_mover(TelltaleState*, Beveler);
    Slider* make_slider(Slider*);
};

MonoKit::MonoKit() {
    impl_ = new MonoKitImpl(this);
    Style* s = Session::instance()->style();
    for (PropertyData* p = kit_props; p->path != nil; p++) {
	s->attribute(p->path, p->value, -5);
    }
}

MonoKit::~MonoKit() {
    delete impl_;
}

const char* MonoKit::gui() const { return "monochrome"; }

static const char* style_attributes[] = {
    "flat", "frameThickness", "moverSize", "sliderSize", nil
};

void MonoKit::style_changed(Style* style) {
    MonoKitImpl& i = *impl_;
    for (ListItr(MonoKitInfoList) s(i.info_list_); s.more(); s.next()) {
	MonoKitInfo* info = s.cur();
	if (info->style() == style) {
	    i.info_ = info;
	    return;
	}
    }
    const Style& s1 = *style;
    for (ListItr(MonoKitInfoList) t(i.info_list_); t.more(); t.next()) {
	MonoKitInfo* info = t.cur();
	const Style& s2 = *info->style();
	for (const char** p = style_attributes; ; p++) {
	    if (*p == nil) {
		i.info_ = info;
		return;
	    }
	    if (!i.match(s1, s2, *p)) {
		break;
	    }
	}
    }
    i.info_ = new MonoKitInfo(style);
    Resource::ref(i.info_);
    i.info_list_.append(i.info_);
}

MonoGlyph* MonoKit::outset_frame(Glyph* g) const {
    const MonoKitInfo& i = *impl_->info_;
    return new Bevel(
	g, &Bevel::rect, i.light(), i.flat(), i.dark(), i.thickness()
    );
}

MonoGlyph* MonoKit::inset_frame(Glyph* g) const {
    const MonoKitInfo& i = *impl_->info_;
    return new Bevel(
	g, &Bevel::rect, i.dark(), i.flat(), i.light(), i.thickness()
    );
}

MonoGlyph* MonoKit::bright_inset_frame(Glyph* g) const {
    const MonoKitInfo& i = *impl_->info_;
    return new Bevel(
	g, &Bevel::rect, i.dark(), i.flat(), i.light(), 1.0,
	0.0, 0.0, false, false
    );
}

Glyph* MonoKit::menubar_look() const {
    MonoKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return outset_frame(layout.r_margin(layout.hbox(), 0.0, fil, 0.0));
}

Glyph* MonoKit::pulldown_look() const {
    MonoKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return outset_frame(layout.vbox());
}

Glyph* MonoKit::menubar_item_look(Glyph* g, TelltaleState* t) const {
    MonoKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    MonoKitInfo* info = i.info_;
    Glyph* item = layout.h_margin(g, 2.0);
    return new MonoKitFrame(item, t, info, info->thickness(), true, true);
}

Glyph* MonoKit::menu_item_look(Glyph* g, TelltaleState* t) const {
    MonoKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    MonoKitInfo* info = i.info_;
    return new MonoKitFrame(g, t, info, info->thickness(), true, true);
}

Glyph* MonoKit::check_menu_item_look(Glyph* g, TelltaleState* t) const {
    MonoKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    const MonoKitInfo& info = *i.info_;
    Coord box_size = font()->width('m') * info.toggle_scale();
    Glyph* dot = new Bevel(
	layout.center(layout.fixed_span(nil, box_size, box_size), 0.0, 0.0),
	&Bevel::rect, info.dark(), info.dull(), info.light(), info.thickness(),
	false, false
    );
    return i.make_menu_item(t, dot, g);
}

Glyph* MonoKit::radio_menu_item_look(Glyph* g, TelltaleState* t) const {
    MonoKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    const MonoKitInfo& info = *i.info_;
    Coord box_size = font()->width('m') * info.radio_scale();
    Glyph* diamond = new Bevel(
	layout.center(layout.fixed_span(nil, box_size, box_size), 0.0, 0.0),
	&Bevel::diamond, info.dark(), info.dull(), info.light(),
	info.thickness(), false, false
    );
    return i.make_menu_item(t, diamond, g);
}

Glyph* MonoKit::menu_item_separator_look() const {
    MonoKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return layout.v_margin(
	layout.v_fixed_span(
	    layout.h_margin(inset_frame(nil), 3.0), 3.0
	),
	2.0
    );
}

/*
 * This uses a kludge to avoid changing kit.c.  If label() has just been
 * called immediately prior to push_button_look, then we still have a char
 * pointer to the original text and we can create an inverse video label for
 * the pressed state.  There's probably a better way to do this, but I'm
 * just hacking right now to get things to work as quickly as possible.
 * Would be nice if I could specify multiple glyphs to push_button_look().
 */

Glyph* MonoKit::label(const char * text) const {
   MonoKit* kludge = (MonoKit*)this;
   kludge->impl_->saved_text_ = text;
   return WidgetKit::label(text);
}

Glyph* MonoKit::label(const String& string) const {
   MonoKit* kludge = (MonoKit*)this;
   kludge->impl_->saved_text_ = string.string();
   return WidgetKit::label(string);
}

Glyph* MonoKit::push_button_look(Glyph* g, TelltaleState* t) const {
    MonoKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    Coord margin = 5.0;
    Coord offset = 0.5 * i.info_->thickness();

    Glyph* pressed;
    Glyph* disabled;
    if (i.saved_text_) {
       const MonoKitInfo& info = *i.info_;
       pressed = new Label(i.saved_text_, font(), background());
       disabled = new Label(i.saved_text_, font(), info.light());
       MonoKit* kludge = (MonoKit*)this;
       kludge->impl_->saved_text_ = nil;
    } else {
       pressed = g;
       disabled = g;
    }

    return i.make_button(
	t,
	layout.margin(g, margin),
	layout.margin(pressed, margin + offset, margin - offset),
	layout.margin(disabled, margin)
    );
}

Glyph* MonoKit::default_button_look(Glyph* g, TelltaleState* t) const {
    MonoKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return inset_frame(layout.margin(push_button_look(g, t), 3.0));
}

Glyph* MonoKit::palette_button_look(Glyph* g, TelltaleState* t) const {
    return push_button_look(g, t);
}

Glyph* MonoKit::check_box_look(Glyph* g, TelltaleState* t) const {
    MonoKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return layout.hbox(
	layout.vcenter(
	    i.make_toggle(t, &Bevel::rect, i.info_->toggle_scale())
	),
	layout.hspace(5.0),
	layout.vcenter(g)
    );
}

Glyph* MonoKit::radio_button_look(Glyph* g, TelltaleState* t) const {
    MonoKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return layout.hbox(
	layout.vcenter(
	    i.make_toggle(t, &Bevel::diamond, i.info_->radio_scale())
	),
	layout.hspace(5.0),
	layout.vcenter(g)
    );
}

Glyph* MonoKit::slider_look(DimensionName d, Adjustable* a) const {
    MonoKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    const MonoKitInfo& info = *i.info_;
    Coord slider_margin = 2.0;
    Coord size = info.slider_size() - slider_margin - slider_margin;
    Glyph* g;
    switch (d) {
    case Dimension_X:
	g = layout.v_fixed_span(
	    i.make_slider(new XSlider(style(), a)), size
	);
	break;
    case Dimension_Y:
	g = layout.h_fixed_span(
	    i.make_slider(new YSlider(style(), a)), size
	);
    default:
	g = nil;
	break;
    }
    return g;
}

Glyph* MonoKit::scroll_bar_look(DimensionName d, Adjustable* a) const {
    MonoKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    const MonoKitInfo& info = *i.info_;
    Glyph* g, * box, * sep, * mover1, * slider, * mover2;
    Coord xspan, yspan;
    float align;
    Coord slider_margin = 2.0;
    switch (d) {
    case Dimension_X:
	xspan = info.mover_size();
	yspan = info.slider_size();
	align = 0.0;
	box = layout.hbox();
	g = layout.v_fixed_span(box, yspan);
	sep = layout.hspace(1.0);
	mover1 = left_mover(a);
	slider = layout.v_margin(
	    layout.v_fixed_span(
		i.make_slider(new XSlider(style(), a)),
		yspan - slider_margin - slider_margin
	    ),
	    slider_margin
	);
	mover2 = right_mover(a);
	break;
    case Dimension_Y:
	xspan = info.slider_size();
	yspan = info.mover_size();
	align = 1.0;
	box = layout.vbox();
	g = layout.h_fixed_span(box, xspan);
	sep = layout.vspace(1.0);
	mover1 = up_mover(a);
	slider = layout.h_margin(
	    layout.h_fixed_span(
		i.make_slider(new YSlider(style(), a)),
		xspan - slider_margin - slider_margin
	    ),
	    slider_margin
	);
	mover2 = down_mover(a);
	break;
    }
    box->append(
	layout.center_dimension(
	    layout.fixed_span(mover1, xspan, yspan), d, align
	)
    );
    box->append(sep);
    box->append(slider);
    box->append(sep);
    box->append(layout.fixed_span(mover2, xspan, yspan));
    Coord t = info.thickness();
    return new Bevel(
	g, &Bevel::rect, info.dark(), info.dull(), info.light(), t
    );
}

Glyph* MonoKit::panner_look(Adjustable* x, Adjustable* y) const {
    MonoKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return i.make_slider(new XYSlider(style(), x, y));
}

Glyph* MonoKit::enlarger_look(TelltaleState*) const {
    return nil;
}

Glyph* MonoKit::reducer_look(TelltaleState*) const {
    return nil;
}

Glyph* MonoKit::up_mover_look(TelltaleState* t) const {
    return impl_->make_mover(t, &Bevel::up_arrow);
}

Glyph* MonoKit::down_mover_look(TelltaleState* t) const {
    return impl_->make_mover(t, &Bevel::down_arrow);
}

Glyph* MonoKit::left_mover_look(TelltaleState* t) const {
    return impl_->make_mover(t, &Bevel::left_arrow);
}

Glyph* MonoKit::right_mover_look(TelltaleState* t) const {
    return impl_->make_mover(t, &Bevel::right_arrow);
}

/* class MonoKitFrame */

MonoKitFrame::MonoKitFrame(
    Glyph* g, TelltaleState* t, MonoKitInfo* i, Coord thickness,
    boolean target, boolean choosable
) : BevelFrame(g, thickness) {
    state_ = t;
    Resource::ref(state_);
    info_ = i;
    Resource::ref(info_);
    target_ = target;
    choosable_ = choosable;
}

MonoKitFrame::~MonoKitFrame() {
    Resource::unref(state_);
    Resource::unref(info_);
}

void MonoKitFrame::draw(Canvas* c, const Allocation& a) const {
    BevelFrame::draw(c, a);
    if (!state_->test(TelltaleState::is_enabled)) {
	c->fill_rect(
	    a.left(), a.bottom(), a.right(), a.top(), info_->gray_out()
	);
    }
}

void MonoKitFrame::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    if (target_) {
	Coord x = h.left();
	Coord y = h.bottom();
	if (x >= a.left() && x < a.right() && y >= a.bottom() && y < a.top()) {
	    h.target(depth, this, 0);
	}
    } else {
	BevelFrame::pick(c, a, depth, h);
    }
}

void MonoKitFrame::draw_frame(Canvas* c, const Allocation& a, Coord t) const {
    const MonoKitInfo& i = info();
    const Color* c1, * c2, * c3;
    c2 = i.flat();
    if (state_->test(TelltaleState::is_enabled_active)) {
	c1 = i.light();
	c3 = i.dark();
    } else if (choosable_ && state_->test(TelltaleState::is_enabled_chosen)) {
	c1 = i.dark();
	c3 = i.light();
    } else {
	c1 = c2;
	c3 = c2;
    }
    Bevel::rect(c, c1, c2, c3, t, a.left(), a.bottom(), a.right(), a.top());
}

/* class MonoKitMenuItem */

MonoKitMenuItem::MonoKitMenuItem(Glyph* g, TelltaleState* s) : MonoGlyph(g) {
    Resource::ref(s);
    state_ = s;
}

MonoKitMenuItem::~MonoKitMenuItem() {
    Resource::unref(state_);
}

void MonoKitMenuItem::draw(Canvas* c, const Allocation& a) const {
    if (state_->test(TelltaleState::is_chosen)) {
	MonoGlyph::draw(c, a);
    }
}

void MonoKitMenuItem::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    if (state_->test(TelltaleState::is_chosen)) {
	MonoGlyph::pick(c, a, depth, h);
    }
}

/* class MonoKitForeground */

MonoKitForeground::MonoKitForeground(Glyph* g, const Color* c) : MonoGlyph(g) {
    foreground_ = c;
    Resource::ref(foreground_);
}

MonoKitForeground::~MonoKitForeground() {
    Resource::unref(foreground_);
}

void MonoKitForeground::draw(Canvas* c, const Allocation& a) const {
    MonoGlyph::draw(c, a);
    c->fill_rect(a.left(), a.bottom(), a.right(), a.top(), foreground_);
}

/* class MonoKitImpl */

MonoKitImpl::MonoKitImpl(MonoKit* k) {
    kit_ = k;
    layout_ = LayoutKit::instance();
}

MonoKitImpl::~MonoKitImpl() {
    for (ListItr(MonoKitInfoList) i(info_list_); i.more(); i.next()) {
	MonoKitInfo* info = i.cur();
	Resource::unref(info);
    }
}

boolean MonoKitImpl::match(const Style& s1, const Style& s2, const char* n) {
    String name(n);
    String v1, v2;
    boolean b1 = s1.find_attribute(n, v1);
    boolean b2 = s2.find_attribute(n, v2);
    return (!b1 && !b2) || (b1 && b2 && v1 == v2);
}

Glyph* MonoKitImpl::make_menu_item(
    TelltaleState* t, Glyph* check, Glyph* g
) {
    const LayoutKit& layout = *layout_;
    return new MonoKitFrame(
	layout.hbox(
	    new MonoKitMenuItem(layout.vcenter(check), t),
	    layout.hspace(5),
	    layout.vcenter(g)
	),
	t, info_, info_->thickness(), true, false
    );
}

ChoiceItem* MonoKitImpl::make_button(
    TelltaleState* t, Glyph* normal, Glyph* pressed
) {
    const MonoKitInfo& i = *info_;
    Glyph* enabled = new Bevel(
	normal, &Bevel::rect, i.light(), i.flat(), i.dark(), i.thickness(),
	false, false
    );
    Glyph* disabled = new MonoKitForeground(enabled, i.gray_out());
    Glyph* active = new Bevel(
	pressed, &Bevel::rect, i.dark(), i.dull(), i.light(), i.thickness(),
	false, false
    );
    return new ChoiceItem(
	t, disabled, enabled, enabled, active, enabled,
	active, active, active, active, disabled
    );
}

/*
 * Added because the above doesn't look very good with monochrome because it
 * makes the left and top bevel disappear.  This accepts a third glyph so the
 * disabling looks right.
 */

ChoiceItem* MonoKitImpl::make_button(
    TelltaleState* t, Glyph* normal, Glyph* pressed, Glyph* disabled
) {
    const MonoKitInfo& i = *info_;
    Glyph* enabled = new Bevel(
	normal, &Bevel::rect, i.light(), i.flat(), i.dark(), i.thickness(),
	false, false
    );
    Glyph* unenabled = new Bevel(
	disabled, &Bevel::rect, i.light(), i.flat(), i.dark(), i.thickness(),
	false, false
    );
    Glyph* active = new Bevel(
	pressed, &Bevel::rect, i.dark(), i.dull(), i.light(), i.thickness(),
	false, false
    );
    return new ChoiceItem(
	t, unenabled, enabled, enabled, active, enabled,
	active, active, active, active, unenabled
    );
}

ChoiceItem* MonoKitImpl::make_toggle(
    TelltaleState* t, Beveler b, float scale
) {
    const MonoKitInfo& i = *info_;
    Coord box_size = kit_->font()->width('m') * scale;
    Glyph* box = layout_->fixed_span(nil, box_size, box_size);
    Glyph* toggle_out = new Bevel(
	box, b, i.light(), i.flat(), i.dark(), i.thickness(), false, false
    );
    Glyph* toggle_in = new Bevel(
	box, b, i.dark(), i.dull(), i.light(), i.thickness(), false, false
    );
    return new ChoiceItem(
	t, new MonoKitForeground(toggle_out, i.gray_out()),
	toggle_out, toggle_out, toggle_in, toggle_in,
	toggle_in, toggle_in, toggle_in, toggle_in,
	new MonoKitForeground(toggle_in, i.gray_out())
    );
}

ChoiceItem* MonoKitImpl::make_mover(TelltaleState* t, Beveler b) {
    const MonoKitInfo& i = *info_;
    Coord m = 2.0;
    Glyph* enabled = layout_->margin(
	new Bevel(nil, b, i.light(), i.flat(), i.dark(), i.thickness()), m
    );
    Glyph* active = layout_->margin(
	new Bevel(nil, b, i.dark(), i.flat(), i.light(), i.thickness()), m
    );
    Glyph* disabled = new MonoKitForeground(enabled, i.gray_out());
    return new ChoiceItem(
	t, disabled, enabled, enabled, active, active,
	enabled, active, active, active, disabled
    );
}

Slider* MonoKitImpl::make_slider(Slider* slider) {
    const MonoKitInfo& i = *info_;
    slider->normal_thumb(
	new Bevel(
	    nil, &Bevel::rect, i.light(), i.flat(), i.dark(), i.thickness()
	)
    );
    return slider;
}

/* class MonoKitInfo */

MonoKitInfo::MonoKitInfo(Style* s) {
    style_ = s;
    Resource::ref(style_);
    load();
}

MonoKitInfo::~MonoKitInfo() {
    unload();
    Resource::unref(style_);
}

void MonoKitInfo::load() {
    Style& s = *style_;
    s.find_attribute("frameThickness", thickness_);
    s.find_attribute("toggleScale", toggle_scale_);
    s.find_attribute("radioScale", radio_scale_);
    s.find_attribute("moverSize", mover_size_);
    s.find_attribute("sliderSize", slider_size_);

    String v("#000000");
    if (!s.find_attribute("background", v)) {
	s.find_attribute("Background", v);
    }
    const Color* c = Color::lookup(Session::instance()->default_display(), v);
    if (c == nil) {
	c = new Color(0.0, 0.0, 0.0, 1.0);
    }
    flat_ = c;
    gray_out_ = new Color(*c, 0.5);

    v = "#ffffff";
    if (!s.find_attribute("foreground", v)) {
	s.find_attribute("Foreground", v);
    }
    c = Color::lookup(Session::instance()->default_display(), v);
    if (c == nil) {
	c = new Color(1.0, 1.0, 1.0, 1.0);
    }
    dull_ = c;
    light_ = new Color(*c, 0.5, Color::Xor);
    dark_ = c;

    /* Set the flat style attribute explicitly.  FieldEditors depend on it. */
    Color* black = new Color(0.0, 0.0, 0.0, 1.0);
    Resource::ref(black);
    if (!flat_->distinguished(black)) {
        s.attribute("flat", "black");
    } else {
        s.attribute("flat", "white");
    }
    Resource::unref(black);

    Resource::ref(flat_);
    Resource::ref(light_);
    Resource::ref(dull_);
    Resource::ref(dark_);
    Resource::ref(gray_out_);
}

void MonoKitInfo::unload() {
    Resource::unref(flat_);
    Resource::unref(light_);
    Resource::unref(dull_);
    Resource::unref(dark_);
    Resource::unref(gray_out_);
}
