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
 * MFKit -- object for creating common UI Motif-ish objects
 */

#include <IV-look/bevel.h>
#include <IV-look/choice.h>
#include <IV-look/mf_kit.h>
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
#include <OS/list.h>
#include <OS/string.h>

static PropertyData kit_props[] = {
    { "*flat", "#aaaaaa" },
    { "*frameThickness", "2.0" },
    { "*moverSize", "20.0" },
    { "*radioScale", "0.7" },
    { "*sliderSize", "20.0" },
    { "*toggleScale", "0.5" },
    { "*FileChooser*filter", "off" },
    { "*FieldEditor*beveled", "on" },
    { "*FieldEditor*background", "#e0e0e0" },
    { "*FieldEditor*flat", "#e0e0e0" },
    { "*Panner*minimumThumbSize", "18.0" },
    { "*ScrollBar*minimumThumbSize", "28.0" },
    nil
};

class MFKitInfo : public Resource {
public:
    MFKitInfo(Style*);
    virtual ~MFKitInfo();

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

inline Style* MFKitInfo::style() const { return style_; }

inline Coord MFKitInfo::thickness() const { return thickness_; }
inline float MFKitInfo::toggle_scale() const { return toggle_scale_; }
inline float MFKitInfo::radio_scale() const { return radio_scale_; }
inline Coord MFKitInfo::mover_size() const { return mover_size_; }
inline Coord MFKitInfo::slider_size() const { return slider_size_; }

inline const Color* MFKitInfo::flat() const { return flat_; }
inline const Color* MFKitInfo::light() const { return light_; }
inline const Color* MFKitInfo::dull() const { return dull_; }
inline const Color* MFKitInfo::dark() const { return dark_; }
inline const Color* MFKitInfo::gray_out() const { return gray_out_; }

class MFKitInfo;

declarePtrList(MFKitInfoList,MFKitInfo)
implementPtrList(MFKitInfoList,MFKitInfo)

class MFKitFrame : public BevelFrame {
public:
    MFKitFrame(
	Glyph*, TelltaleState*, MFKitInfo*, Coord,
	boolean target, boolean choosable
    );
    virtual ~MFKitFrame();

    const MFKitInfo& info() const;

    virtual void draw(Canvas*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);

    virtual void draw_frame(Canvas*, const Allocation&, Coord) const;
private:
    TelltaleState* state_;
    MFKitInfo* info_;
    boolean target_;
    boolean choosable_;
};

const MFKitInfo& MFKitFrame::info() const { return *info_; }

class MFKitMenuItem : public MonoGlyph {
public:
    MFKitMenuItem(Glyph*, TelltaleState*);
    virtual ~MFKitMenuItem();

    virtual void draw(Canvas*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);
private:
    TelltaleState* state_;
};

class MFKitForeground : public MonoGlyph {
public:
    MFKitForeground(Glyph*, const Color*);
    virtual ~MFKitForeground();

    virtual void draw(Canvas*, const Allocation&) const;
private:
    const Color* foreground_;
};

class MFKitImpl {
private:
    friend class MFKit;

    MFKit* kit_;
    LayoutKit* layout_;
    MFKitInfo* info_;
    MFKitInfoList info_list_;

    MFKitImpl(MFKit*);
    ~MFKitImpl();

    boolean match(const Style&, const Style&, const char*);
    Glyph* make_menu_item(TelltaleState*, Glyph* check, Glyph*);
    ChoiceItem* make_button(TelltaleState*, Glyph* normal, Glyph* pressed);
    ChoiceItem* make_toggle(TelltaleState*, Beveler, float scale);
    ChoiceItem* make_mover(TelltaleState*, Beveler);
    Slider* make_slider(Slider*);
};

MFKit::MFKit() {
    impl_ = new MFKitImpl(this);
    Style* s = Session::instance()->style();
    for (PropertyData* p = kit_props; p->path != nil; p++) {
	s->attribute(p->path, p->value, -10);
    }
}

MFKit::~MFKit() {
    delete impl_;
}

const char* MFKit::gui() const { return "Motif"; }

static const char* style_attributes[] = {
    "flat", "frameThickness", "moverSize", "sliderSize", nil
};

void MFKit::style_changed(Style* style) {
    MFKitImpl& i = *impl_;
    for (ListItr(MFKitInfoList) s(i.info_list_); s.more(); s.next()) {
	MFKitInfo* info = s.cur();
	if (info->style() == style) {
	    i.info_ = info;
	    return;
	}
    }
    const Style& s1 = *style;
    for (ListItr(MFKitInfoList) t(i.info_list_); t.more(); t.next()) {
	MFKitInfo* info = t.cur();
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
    i.info_ = new MFKitInfo(style);
    Resource::ref(i.info_);
    i.info_list_.append(i.info_);
}

MonoGlyph* MFKit::outset_frame(Glyph* g) const {
    const MFKitInfo& i = *impl_->info_;
    return new Bevel(
	g, &Bevel::rect, i.light(), i.flat(), i.dark(), i.thickness()
    );
}

MonoGlyph* MFKit::inset_frame(Glyph* g) const {
    const MFKitInfo& i = *impl_->info_;
    return new Bevel(
	g, &Bevel::rect, i.dark(), i.flat(), i.light(), i.thickness()
    );
}

MonoGlyph* MFKit::bright_inset_frame(Glyph* g) const {
    const MFKitInfo& i = *impl_->info_;
    return new Bevel(
	g, &Bevel::rect, i.dark(), i.light(), i.light(), 1.0,
	0.0, 0.0, false, false
    );
}

Glyph* MFKit::menubar_look() const {
    MFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return outset_frame(layout.r_margin(layout.hbox(), 0.0, fil, 0.0));
}

Glyph* MFKit::pulldown_look() const {
    MFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return outset_frame(layout.vbox());
}

Glyph* MFKit::menubar_item_look(Glyph* g, TelltaleState* t) const {
    MFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    MFKitInfo* info = i.info_;
    Glyph* item = layout.h_margin(g, 2.0);
    return new MFKitFrame(item, t, info, info->thickness(), true, true);
}

Glyph* MFKit::menu_item_look(Glyph* g, TelltaleState* t) const {
    MFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    MFKitInfo* info = i.info_;
    return new MFKitFrame(g, t, info, info->thickness(), true, true);
}

Glyph* MFKit::check_menu_item_look(Glyph* g, TelltaleState* t) const {
    MFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    const MFKitInfo& info = *i.info_;
    Coord box_size = font()->width('m') * info.toggle_scale();
    Glyph* dot = new Bevel(
	layout.center(layout.fixed_span(nil, box_size, box_size), 0.0, 0.0),
	&Bevel::rect, info.dark(), info.dull(), info.light(), info.thickness(),
	false, false
    );
    return i.make_menu_item(t, dot, g);
}

Glyph* MFKit::radio_menu_item_look(Glyph* g, TelltaleState* t) const {
    MFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    const MFKitInfo& info = *i.info_;
    Coord box_size = font()->width('m') * info.radio_scale();
    Glyph* diamond = new Bevel(
	layout.center(layout.fixed_span(nil, box_size, box_size), 0.0, 0.0),
	&Bevel::diamond, info.dark(), info.dull(), info.light(),
	info.thickness(), false, false
    );
    return i.make_menu_item(t, diamond, g);
}

Glyph* MFKit::menu_item_separator_look() const {
    MFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return layout.v_margin(
	layout.v_fixed_span(
	    layout.h_margin(inset_frame(nil), 3.0), 3.0
	),
	2.0
    );
}

Glyph* MFKit::push_button_look(Glyph* g, TelltaleState* t) const {
    MFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    Coord margin = 5.0;
    Coord offset = 0.5 * i.info_->thickness();
    return i.make_button(
	t,
	layout.margin(g, margin),
	layout.margin(g, margin + offset, margin - offset)
    );
}

Glyph* MFKit::default_button_look(Glyph* g, TelltaleState* t) const {
    MFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return inset_frame(layout.margin(push_button_look(g, t), 3.0));
}

Glyph* MFKit::palette_button_look(Glyph* g, TelltaleState* t) const {
    return push_button_look(g, t);
}

Glyph* MFKit::check_box_look(Glyph* g, TelltaleState* t) const {
    MFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return layout.hbox(
	layout.vcenter(
	    i.make_toggle(t, &Bevel::rect, i.info_->toggle_scale())
	),
	layout.hspace(5.0),
	layout.vcenter(g)
    );
}

Glyph* MFKit::radio_button_look(Glyph* g, TelltaleState* t) const {
    MFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return layout.hbox(
	layout.vcenter(
	    i.make_toggle(t, &Bevel::diamond, i.info_->radio_scale())
	),
	layout.hspace(5.0),
	layout.vcenter(g)
    );
}

Glyph* MFKit::slider_look(DimensionName d, Adjustable* a) const {
    MFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    const MFKitInfo& info = *i.info_;
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
	break;
    default:
	return nil;
    }
    return new Bevel(
	g, &Bevel::rect,
	info.dark(), info.dull(), info.light(), info.thickness()
    );
}

Glyph* MFKit::scroll_bar_look(DimensionName d, Adjustable* a) const {
    MFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    const MFKitInfo& info = *i.info_;
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

Glyph* MFKit::panner_look(Adjustable* x, Adjustable* y) const {
    MFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return i.make_slider(new XYSlider(style(), x, y));
}

Glyph* MFKit::enlarger_look(TelltaleState*) const {
    return nil;
}

Glyph* MFKit::reducer_look(TelltaleState*) const {
    return nil;
}

Glyph* MFKit::up_mover_look(TelltaleState* t) const {
    return impl_->make_mover(t, &Bevel::up_arrow);
}

Glyph* MFKit::down_mover_look(TelltaleState* t) const {
    return impl_->make_mover(t, &Bevel::down_arrow);
}

Glyph* MFKit::left_mover_look(TelltaleState* t) const {
    return impl_->make_mover(t, &Bevel::left_arrow);
}

Glyph* MFKit::right_mover_look(TelltaleState* t) const {
    return impl_->make_mover(t, &Bevel::right_arrow);
}

/* class MFKitFrame */

MFKitFrame::MFKitFrame(
    Glyph* g, TelltaleState* t, MFKitInfo* i, Coord thickness,
    boolean target, boolean choosable
) : BevelFrame(g, thickness) {
    state_ = t;
    Resource::ref(state_);
    info_ = i;
    Resource::ref(info_);
    target_ = target;
    choosable_ = choosable;
}

MFKitFrame::~MFKitFrame() {
    Resource::unref(state_);
    Resource::unref(info_);
}

void MFKitFrame::draw(Canvas* c, const Allocation& a) const {
    BevelFrame::draw(c, a);
    if (!state_->test(TelltaleState::is_enabled)) {
	c->fill_rect(
	    a.left(), a.bottom(), a.right(), a.top(), info_->gray_out()
	);
    }
}

void MFKitFrame::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
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

void MFKitFrame::draw_frame(Canvas* c, const Allocation& a, Coord t) const {
    const MFKitInfo& i = info();
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

/* class MFKitMenuItem */

MFKitMenuItem::MFKitMenuItem(Glyph* g, TelltaleState* s) : MonoGlyph(g) {
    Resource::ref(s);
    state_ = s;
}

MFKitMenuItem::~MFKitMenuItem() {
    Resource::unref(state_);
}

void MFKitMenuItem::draw(Canvas* c, const Allocation& a) const {
    if (state_->test(TelltaleState::is_chosen)) {
	MonoGlyph::draw(c, a);
    }
}

void MFKitMenuItem::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    if (state_->test(TelltaleState::is_chosen)) {
	MonoGlyph::pick(c, a, depth, h);
    }
}

/* class MFKitForeground */

MFKitForeground::MFKitForeground(Glyph* g, const Color* c) : MonoGlyph(g) {
    foreground_ = c;
    Resource::ref(foreground_);
}

MFKitForeground::~MFKitForeground() {
    Resource::unref(foreground_);
}

void MFKitForeground::draw(Canvas* c, const Allocation& a) const {
    MonoGlyph::draw(c, a);
    c->fill_rect(a.left(), a.bottom(), a.right(), a.top(), foreground_);
}

/* class MFKitImpl */

MFKitImpl::MFKitImpl(MFKit* k) {
    kit_ = k;
    layout_ = LayoutKit::instance();
}

MFKitImpl::~MFKitImpl() {
    for (ListItr(MFKitInfoList) i(info_list_); i.more(); i.next()) {
	MFKitInfo* info = i.cur();
	Resource::unref(info);
    }
}

boolean MFKitImpl::match(const Style& s1, const Style& s2, const char* n) {
    String v1, v2;
    boolean b1 = s1.find_attribute(n, v1);
    boolean b2 = s2.find_attribute(n, v2);
    return !(b1 || b2) || (b1 && b2 && v1 == v2);
}

Glyph* MFKitImpl::make_menu_item(
    TelltaleState* t, Glyph* check, Glyph* g
) {
    const LayoutKit& layout = *layout_;
    return new MFKitFrame(
	layout.hbox(
	    new MFKitMenuItem(layout.vcenter(check), t),
	    layout.hspace(5),
	    layout.vcenter(g)
	),
	t, info_, info_->thickness(), true, false
    );
}

ChoiceItem* MFKitImpl::make_button(
    TelltaleState* t, Glyph* normal, Glyph* pressed
) {
    const MFKitInfo& i = *info_;
    Glyph* enabled = new Bevel(
	normal, &Bevel::rect, i.light(), i.flat(), i.dark(), i.thickness(),
	false, false
    );
    Glyph* disabled = new MFKitForeground(enabled, i.gray_out());
    Glyph* active = new Bevel(
	pressed, &Bevel::rect, i.dark(), i.flat(), i.light(), i.thickness(),
	false, false
    );
    return new ChoiceItem(
	t, disabled, enabled, enabled, active, enabled,
	active, active, active, active, disabled
    );
}

ChoiceItem* MFKitImpl::make_toggle(TelltaleState* t, Beveler b, float scale) {
    const MFKitInfo& i = *info_;
    Coord box_size = kit_->font()->width('m') * scale;
    Glyph* box = layout_->fixed_span(nil, box_size, box_size);
    Glyph* toggle_out = new Bevel(
	box, b, i.light(), i.flat(), i.dark(), i.thickness(), false, false
    );
    Glyph* toggle_in = new Bevel(
	box, b, i.dark(), i.dull(), i.light(), i.thickness(), false, false
    );
    return new ChoiceItem(
	t, new MFKitForeground(toggle_out, i.gray_out()),
	toggle_out, toggle_out, toggle_in, toggle_in,
	toggle_in, toggle_in, toggle_in, toggle_in,
	new MFKitForeground(toggle_in, i.gray_out())
    );
}

ChoiceItem* MFKitImpl::make_mover(TelltaleState* t, Beveler b) {
    const MFKitInfo& i = *info_;
    Coord m = 2.0;
    Glyph* enabled = layout_->margin(
	new Bevel(nil, b, i.light(), i.flat(), i.dark(), i.thickness()), m
    );
    Glyph* active = layout_->margin(
	new Bevel(nil, b, i.dark(), i.flat(), i.light(), i.thickness()), m
    );
    Glyph* disabled = new MFKitForeground(enabled, i.gray_out());
    return new ChoiceItem(
	t, disabled, enabled, enabled, active, active,
	enabled, active, active, active, disabled
    );
}

Slider* MFKitImpl::make_slider(Slider* slider) {
    const MFKitInfo& i = *info_;
    slider->normal_thumb(
	new Bevel(
	    nil, &Bevel::rect, i.light(), i.flat(), i.dark(), i.thickness()
	)
    );
    return slider;
}

/* class MFKitInfo */

MFKitInfo::MFKitInfo(Style* s) {
    style_ = s;
    Resource::ref(style_);
    load();
}

MFKitInfo::~MFKitInfo() {
    unload();
    Resource::unref(style_);
}

void MFKitInfo::load() {
    Style& s = *style_;
    s.find_attribute("frameThickness", thickness_);
    s.find_attribute("toggleScale", toggle_scale_);
    s.find_attribute("radioScale", radio_scale_);
    s.find_attribute("moverSize", mover_size_);
    s.find_attribute("sliderSize", slider_size_);

    String v;
    s.find_attribute("flat", v);
    const Color* c = Color::lookup(Session::instance()->default_display(), v);
    if (c == nil) {
	c = new Color(0.7, 0.7, 0.7, 1.0);
    }
    flat_ = c;
    light_ = c->brightness(0.5);
    dull_ = c->brightness(-0.2);
    dark_ = c->brightness(-0.4);
    gray_out_ = new Color(*c, 0.2);
    Resource::ref(flat_);
    Resource::ref(light_);
    Resource::ref(dull_);
    Resource::ref(dark_);
    Resource::ref(gray_out_);
}

void MFKitInfo::unload() {
    Resource::unref(flat_);
    Resource::unref(light_);
    Resource::unref(dull_);
    Resource::unref(dark_);
    Resource::unref(gray_out_);
}
