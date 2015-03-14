/*
 * Copyright (c) 1991, 1992 Stanford University
 * Copyright (c) 1991, 1992 Silicon Graphics, Inc.
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
 * SMFKit -- object for creating common SGI Motif controls
 */

#include <IV-look/bevel.h>
#include <IV-look/slider.h>
#include <IV-look/smf_kit.h>
#include <InterViews/adjust.h>
#include <InterViews/bitmap.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/font.h>
#include <InterViews/hit.h>
#include <InterViews/layout.h>
#include <InterViews/observe.h>
#include <InterViews/session.h>
#include <InterViews/shadow.h>
#include <InterViews/stencil.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <OS/list.h>
#include <OS/string.h>
#include <math.h>

static PropertyData kit_props[] = {
    { "*checkScale", "0.7" },
    { "*flat", "#aaaaaa" },
    { "*frameThickness", "2.0" },
    { "*moverSize", "20.0" },
    { "*radioScale", "0.9" },
    { "*sliderSize", "20.0" },
    { "*FileChooser*filter", "off" },
    { "*FieldEditor*beveled", "on" },
    { "*FieldEditor*background", "#b88d8d" },
    { "*FieldEditor*flat", "#b88d8d" },
    { "*MenuBar*font", "*-helvetica-bold-o-normal--14-140-*" },
    { "*MenuItem*font", "*-helvetica-bold-o-normal--14-140-*" },
    { "*PaletteButton*minimumWidth", "72.0" },
    { "*Panner*minimumThumbSize", "18.0" },
    { "*PushButton*minimumWidth", "72.0" },
    { "*ScrollBar*minimumThumbSize", "28.0" },
    { "*ScrollBar*thumbRidges", "3" },
    { "*Slider*thumbRidges", "1" },
    nil
};

#define button_border 4
#define arrow_border 6

static const int black = 0;
static const int very_dark_gray = 1;
static const int dark_gray = 2;
static const int medium_gray = 3;
static const int light_gray = 4;
static const int very_light_gray = 5;
static const int white = 6;
static const int gray_out = 7;
static const int shadow = 8;
static const int yellow = 9;
static const int light_yellow = 10;
static const int dark_yellow = 11;
static const int medium_yellow = 12;
static const int num_colors = 13;

static const unsigned int checkmark_width = 32;
static const unsigned int checkmark_height = 12;

static char checkmark_bits[] = {
    0x00, 0x00, 0x3e, 0x00, 0x00, 0xc0, 0x0f, 0x00,
    0x04, 0xf0, 0x03, 0x00, 0x1e, 0xf8, 0x00, 0x00,
    0x3f, 0x3e, 0x00, 0x00, 0xbf, 0x1f, 0x00, 0x00,
    0xf8, 0x07, 0x00, 0x00, 0xf0, 0x03, 0x00, 0x00,
    0xf0, 0x01, 0x00, 0x00, 0xe0, 0x00, 0x00, 0x00,
    0x60, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

static const unsigned int shadow1_width = 32;
static const unsigned int shadow1_height = 13;

static char shadow1_bits[] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0xc0, 0x00, 0x00, 0x00, 0xe0, 0x00, 0x00,
    0x07, 0x38, 0x00, 0x00, 0x0f, 0x1c, 0x00, 0x00,
    0x08, 0x06, 0x00, 0x00, 0x10, 0x03, 0x00, 0x00,
    0x90, 0x01, 0x00, 0x00, 0xe0, 0x00, 0x00, 0x00,
    0x60, 0x00, 0x00, 0x00
};

static const unsigned int shadow2_width = 32;
static const unsigned int shadow2_height = 5;

static char shadow2_bits[] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x00,
    0x00, 0x00, 0x3c, 0x00, 0x00, 0x00, 0x0f, 0x00,
    0x00, 0x00, 0x03, 0x00
};

class SMFKitInfo : public Resource {
public:
    SMFKitInfo(Style*);
    virtual ~SMFKitInfo();

    Style* style() const;

    Coord thickness() const;
    float check_scale() const;
    float radio_scale() const;
    Coord mover_size() const;
    Coord slider_size() const;

    const Color* color(int) const;

    Stencil* checkmark() const;
    Stencil* shadow1() const;
    Stencil* shadow2() const;
private:
    Style* style_;

    Coord thickness_;
    float check_scale_;
    float radio_scale_;
    Coord mover_size_;
    Coord slider_size_;
    const Color* color_[num_colors];
    Stencil* checkmark_;
    Stencil* shadow1_;
    Stencil* shadow2_;

    void load();
    void unload();
    void update();
};

inline Style* SMFKitInfo::style() const { return style_; }

inline Coord SMFKitInfo::thickness() const { return thickness_; }
inline float SMFKitInfo::check_scale() const { return check_scale_; }
inline float SMFKitInfo::radio_scale() const { return radio_scale_; }
inline Coord SMFKitInfo::mover_size() const { return mover_size_; }
inline Coord SMFKitInfo::slider_size() const { return slider_size_; }
inline const Color* SMFKitInfo::color(int c) const { return color_[c]; }
inline Stencil* SMFKitInfo::checkmark() const { return checkmark_; }
inline Stencil* SMFKitInfo::shadow1() const { return shadow1_; }
inline Stencil* SMFKitInfo::shadow2() const { return shadow2_; }

declarePtrList(SMFKitInfoList,SMFKitInfo)
implementPtrList(SMFKitInfoList,SMFKitInfo)

static inline TelltaleFlags flags(TelltaleState* t) {
    return t->flags() & TelltaleState::is_enabled_visible_active_chosen;
}

class SMFKitFrame : public BevelFrame {
public:
    SMFKitFrame(
	Glyph*, TelltaleState*, SMFKitInfo*, Coord thickness,
	float xalign = 0.5, float yalign = 0.5,
	boolean hmargin = true, boolean vmargin = true
    );
    virtual ~SMFKitFrame();

    TelltaleState* state() const;
    const SMFKitInfo& info() const;

    virtual void draw(Canvas*, const Allocation&) const;
    virtual void draw_frame(Canvas*, const Allocation&, Coord) const;
private:
    TelltaleState* state_;
    SMFKitInfo* info_;
};

inline TelltaleState* SMFKitFrame::state() const { return state_; }

inline const SMFKitInfo& SMFKitFrame::info() const { return *info_; }

class SMFKitButtonFrame : public SMFKitFrame {
public:
    SMFKitButtonFrame(
	Glyph*, TelltaleState*, SMFKitInfo*, Coord thickness = button_border,
	float xalign = 0.5, float yalign = 0.5
    );
    virtual ~SMFKitButtonFrame();

    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);
    virtual void draw_frame(Canvas*, const Allocation&, Coord) const;
};

class SMFKitPushButtonFrame : public SMFKitButtonFrame {
public:
    SMFKitPushButtonFrame(
	Glyph*, TelltaleState*, SMFKitInfo*, Style*,
	Coord thickness = button_border,
	float xalign = 0.5, float yalign = 0.5
    );
    virtual ~SMFKitPushButtonFrame();

    virtual void request(Requisition&) const;
private:
    Coord minwidth_;
};

class SMFKitMenuItemFrame : public SMFKitButtonFrame {
public:
    SMFKitMenuItemFrame(Glyph*, TelltaleState*, SMFKitInfo*);
    virtual ~SMFKitMenuItemFrame();

    virtual void draw_frame(Canvas*, const Allocation&, Coord) const;
};

class SMFKitGlyph : public Glyph {
public:
    SMFKitGlyph(SMFKitInfo*);
    virtual ~SMFKitGlyph();

    const SMFKitInfo& info() const;

    virtual void allocate(Canvas*, const Allocation&, Extension&);
private:
    SMFKitInfo* info_;
};

inline const SMFKitInfo& SMFKitGlyph::info() const { return *info_; }

class SMFKitCheckmark : public SMFKitGlyph {
public:
    SMFKitCheckmark(TelltaleState*, SMFKitInfo*, const Font*);
    virtual ~SMFKitCheckmark();

    virtual void request(Requisition&) const;
    virtual void draw(Canvas*, const Allocation&) const;
private:
    TelltaleState* state_;
    const Font* font_;
};

class SMFKitIndicator : public SMFKitGlyph {
public:
    SMFKitIndicator(TelltaleState*, SMFKitInfo*);
    virtual ~SMFKitIndicator();

    virtual void request(Requisition&) const;
    virtual void draw(Canvas*, const Allocation&) const;
private:
    TelltaleState* state_;
};

class SMFKitRadioFlag : public SMFKitGlyph {
public:
    SMFKitRadioFlag(TelltaleState*, SMFKitInfo*, const Font*);
    virtual ~SMFKitRadioFlag();

    TelltaleState* state() const;

    virtual void request(Requisition&) const;
    virtual void draw(Canvas*, const Allocation&) const;
private:
    TelltaleState* state_;
    const Font* font_;
};

inline TelltaleState* SMFKitRadioFlag::state() const { return state_; }

class SMFKitRadioItem : public SMFKitRadioFlag {
public:
    SMFKitRadioItem(TelltaleState*, SMFKitInfo*, const Font*);
    virtual ~SMFKitRadioItem();

    virtual void draw(Canvas*, const Allocation&) const;
};

class SMFKitDefaultArrow : public SMFKitGlyph {
public:
    SMFKitDefaultArrow(const Font*, SMFKitInfo*);
    virtual ~SMFKitDefaultArrow();

    virtual void request(Requisition&) const;
    virtual void draw(Canvas*, const Allocation&) const;
private:
    const Font* font_;
};

class SMFKitThumb : public SMFKitGlyph {
public:
    SMFKitThumb(DimensionName, SMFKitInfo*, long ridges, const TelltaleFlags);
    virtual ~SMFKitThumb();

    virtual void draw(Canvas*, const Allocation&) const;
private:
    DimensionName dimension_;
    long ridges_;
    TelltaleFlags flags_;
};

class SMFKitImpl {
public:
    static void shade(
	Canvas*, const Allocation&, const SMFKitInfo&,
	const int* colors, int ncolors, Coord*
    );
private:
    friend class SMFKit;

    SMFKit* kit_;
    LayoutKit* layout_;
    SMFKitInfo* info_;
    SMFKitInfoList info_list_;

    SMFKitImpl(SMFKit*);
    ~SMFKitImpl();

    boolean match(const Style&, const Style&, const char*);
    void make_thumb(Slider*, DimensionName, SMFKitInfo*);
};

SMFKit::SMFKit() {
    impl_ = new SMFKitImpl(this);
    Style* s = Session::instance()->style();
    for (PropertyData* p = kit_props; p->path != nil; p++) {
	s->attribute(p->path, p->value, -10);
    }
}

SMFKit::~SMFKit() {
    delete impl_;
}

const char* SMFKit::gui() const { return "SGIMotif"; }

static const char* style_attributes[] = {
    "flat", "frameThickness", "moverSize", "sliderSize", nil
};

void SMFKit::style_changed(Style* style) {
    SMFKitImpl& i = *impl_;
    for (ListItr(SMFKitInfoList) s(i.info_list_); s.more(); s.next()) {
	SMFKitInfo* info = s.cur();
	if (info->style() == style) {
	    i.info_ = info;
	    return;
	}
    }
    const Style& s1 = *style;
    for (ListItr(SMFKitInfoList) t(i.info_list_); t.more(); t.next()) {
	SMFKitInfo* info = t.cur();
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
    i.info_ = new SMFKitInfo(style);
    Resource::ref(i.info_);
    i.info_list_.append(i.info_);
}

MonoGlyph* SMFKit::outset_frame(Glyph* g) const {
    SMFKitInfo* i = impl_->info_;
    return new SMFKitFrame(
	g, new TelltaleState(TelltaleState::is_enabled_active),
	i, i->thickness()
    );
}

MonoGlyph* SMFKit::inset_frame(Glyph* g) const {
    SMFKitInfo* i = impl_->info_;
    return new SMFKitFrame(
	g, new TelltaleState(TelltaleState::is_enabled_chosen),
	i, i->thickness()
    );
}

MonoGlyph* SMFKit::bright_inset_frame(Glyph* g) const {
    SMFKitInfo* i = impl_->info_;
    return new SMFKitFrame(
	g, new TelltaleState(TelltaleState::is_enabled_visible_active),
	i, 0.5 * i->thickness(), 0.0, 0.0, false, false
    );
}

Glyph* SMFKit::menubar_look() const {
    SMFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return outset_frame(layout.r_margin(layout.hbox(), 0.0, fil, 0.0));
}

Glyph* SMFKit::pulldown_look() const {
    SMFKitImpl& k = *impl_;
    const LayoutKit& layout = *k.layout_;
    const SMFKitInfo& i = *k.info_;
    return layout.t_margin(
	new Shadow(
	    outset_frame(layout.vbox()), 6, -6, i.color(shadow), true
	), 4
    );
}

Glyph* SMFKit::pullright_look() const {
    SMFKitImpl& k = *impl_;
    const LayoutKit& layout = *k.layout_;
    const SMFKitInfo& i = *k.info_;
    return new Shadow(
	outset_frame(layout.vbox()), 6, -6, i.color(shadow), true
    );
}

Glyph* SMFKit::menubar_item_look(Glyph* g, TelltaleState* t) const {
    SMFKitImpl& k = *impl_;
    const LayoutKit& layout = *k.layout_;
    return new SMFKitMenuItemFrame(layout.h_margin(g, 4.0), t, k.info_);
}

Glyph* SMFKit::menu_item_look(Glyph* g, TelltaleState* t) const {
    SMFKitImpl& k = *impl_;
    return new SMFKitMenuItemFrame(g, t, k.info_);
}

Glyph* SMFKit::check_menu_item_look(Glyph* g, TelltaleState* t) const {
    SMFKitImpl& k = *impl_;
    const LayoutKit& layout = *k.layout_;
    SMFKitInfo* i = k.info_;
    return new SMFKitMenuItemFrame(
	layout.hbox(new SMFKitCheckmark(t, i, font()), layout.hspace(6.0), g),
	t, i
    );
}

Glyph* SMFKit::radio_menu_item_look(Glyph* g, TelltaleState* t) const {
    SMFKitImpl& k = *impl_;
    const LayoutKit& layout = *k.layout_;
    SMFKitInfo* i = k.info_;
    return new SMFKitMenuItemFrame(
	layout.hbox(new SMFKitRadioItem(t, i, font()), layout.hspace(2.0), g),
	t, i
    );
}

Glyph* SMFKit::menu_item_separator_look() const {
    SMFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return layout.v_margin(
	layout.v_fixed_span(
	    layout.h_margin(inset_frame(nil), 3.0), 3.0
	),
	2.0
    );
}

Glyph* SMFKit::push_button_look(Glyph* g, TelltaleState* t) const {
    SMFKitImpl& i = *impl_;
    return new SMFKitPushButtonFrame(g, t, i.info_, style());
}

Glyph* SMFKit::default_button_look(Glyph* g, TelltaleState* t) const {
    SMFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    Glyph* arrow = new SMFKitDefaultArrow(font(), i.info_);
    return push_button_look(layout.hbox(g, layout.hspace(3.0), arrow), t);
}

Glyph* SMFKit::check_box_look(Glyph* g, TelltaleState* t) const {
    SMFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return layout.hbox(
	layout.vcenter(
	    new SMFKitButtonFrame(
		new SMFKitCheckmark(t, i.info_, font()),
		t, i.info_
	    )
	),
	layout.hspace(6.0),
	layout.vcenter(g)
    );
}

Glyph* SMFKit::palette_button_look(Glyph* g, TelltaleState* t) const {
    SMFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    return new SMFKitPushButtonFrame(
	layout.hbox(
	    layout.hspace(3.0),
	    new SMFKitIndicator(t, i.info_),
	    layout.hspace(6.0),
	    g
	),
	t, i.info_, style(), button_border, 0.0, 0.5
    );
}

Glyph* SMFKit::radio_button_look(Glyph* g, TelltaleState* t) const {
    SMFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    SMFKitInfo* info = i.info_;
    return layout.hbox(
	layout.vcenter(
	    new SMFKitRadioFlag(t, info, font())
	),
	layout.hspace(6.0),
	layout.vcenter(g)
    );
}

Glyph* SMFKit::slider_look(DimensionName d, Adjustable* a) const {
    SMFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    SMFKitInfo* info = i.info_;
    Glyph* g;
    Slider* slider;
    switch (d) {
    case Dimension_X:
	slider = new XSlider(style(), a);
	g = layout.v_fixed_span(slider, info->slider_size());
	break;
    case Dimension_Y:
	slider = new YSlider(style(), a);
	g = layout.h_fixed_span(slider, info->slider_size());
	break;
    default:
	return nil;
    }
    i.make_thumb(slider, d, info);
    return new SMFKitFrame(
	g, new TelltaleState(TelltaleState::is_enabled),
	info, info->thickness()
    );
}

Glyph* SMFKit::scroll_bar_look(DimensionName d, Adjustable* a) const {
    SMFKitImpl& i = *impl_;
    const LayoutKit& layout = *i.layout_;
    SMFKitInfo* info = i.info_;
    Glyph* g, * box, * mover1, * mover2;
    Slider* slider;
    Coord xspan, yspan;
    boolean hmargin, vmargin;
    switch (d) {
    case Dimension_X:
	xspan = info->mover_size();
	yspan = info->slider_size();
	box = layout.hbox();
	g = layout.v_fixed_span(box, yspan);
	mover1 = left_mover(a);
	slider = new XSlider(style(), a);
	mover2 = right_mover(a);
	hmargin = false;
	vmargin = true;
	break;
    case Dimension_Y:
	xspan = info->slider_size();
	yspan = info->mover_size();
	box = layout.vbox();
	g = layout.h_fixed_span(box, xspan);
	mover1 = up_mover(a);
	slider = new YSlider(style(), a);
	mover2 = down_mover(a);
	hmargin = true;
	vmargin = false;
	break;
    default:
	return nil;
    }
    TelltaleState* t = new TelltaleState(TelltaleState::is_enabled);
    box->append(layout.fixed_span(mover1, xspan, yspan));
    i.make_thumb(slider, d, info);
    box->append(
	new SMFKitFrame(
	    slider, t, info, info->thickness(), 0.0, 0.0, hmargin, vmargin
	)
    );
    box->append(layout.fixed_span(mover2, xspan, yspan));
    return g;
}

Glyph* SMFKit::panner_look(Adjustable* x, Adjustable* y) const {
    SMFKitImpl& k = *impl_;
    SMFKitInfo* i = k.info_;
    Slider* s = new XYSlider(style(), x, y);
    k.make_thumb(s, Dimension_Undefined, i);
    return s;
}

Glyph* SMFKit::enlarger_look(TelltaleState*) const {
    /* unimplemented */
    return nil;
}

Glyph* SMFKit::reducer_look(TelltaleState*) const {
    /* unimplemented */
    return nil;
}

Glyph* SMFKit::up_mover_look(TelltaleState* t) const {
    SMFKitImpl& i = *impl_;
    return new SMFKitButtonFrame(
	new UpArrow(i.info_->color(dark_gray)), t, i.info_, arrow_border
    );
}

Glyph* SMFKit::down_mover_look(TelltaleState* t) const {
    SMFKitImpl& i = *impl_;
    return new SMFKitButtonFrame(
	new DownArrow(i.info_->color(dark_gray)), t, i.info_, arrow_border
    );
}

Glyph* SMFKit::left_mover_look(TelltaleState* t) const {
    SMFKitImpl& i = *impl_;
    return new SMFKitButtonFrame(
	new LeftArrow(i.info_->color(dark_gray)), t, i.info_, arrow_border
    );
}

Glyph* SMFKit::right_mover_look(TelltaleState* t) const {
    SMFKitImpl& i = *impl_;
    return new SMFKitButtonFrame(
	new RightArrow(i.info_->color(dark_gray)), t, i.info_, arrow_border
    );
}

/* class SMFKitImpl */

SMFKitImpl::SMFKitImpl(SMFKit* k) {
    kit_ = k;
    layout_ = LayoutKit::instance();
}

SMFKitImpl::~SMFKitImpl() {
    for (ListItr(SMFKitInfoList) i(info_list_); i.more(); i.next()) {
	SMFKitInfo* info = i.cur();
	Resource::unref(info);
    }
}

boolean SMFKitImpl::match(const Style& s1, const Style& s2, const char* n) {
    String name(n);
    String v1, v2;
    boolean b1 = s1.find_attribute(n, v1);
    boolean b2 = s2.find_attribute(n, v2);
    return (!b1 && !b2) || (b1 && b2 && v1 == v2);
}

void SMFKitImpl::make_thumb(Slider* s, DimensionName d, SMFKitInfo* i) {
    long r = 0;
    kit_->style()->find_attribute("thumbRidges", r);
    s->normal_thumb(new SMFKitThumb(d, i, r, TelltaleState::is_enabled));
    s->visible_thumb(new SMFKitThumb(d, i, r, TelltaleState::is_visible));
    s->old_thumb(
	new SMFKitFrame(
	    nil, new TelltaleState(TelltaleState::is_enabled_active_chosen),
	    i, i->thickness()
	)
    );
}

void SMFKitImpl::shade(
    Canvas* c, const Allocation& a, const SMFKitInfo& info,
    const int* colors, int ncolors, Coord* t
) {
    Coord x0 = a.left(), y0 = a.bottom(), x1 = a.right(), y1 = a.top();
    int nbands = (ncolors - 1) >> 1;
    int b = nbands - 1;
    int n = ncolors - 1;
    for (int i = 0; i < b; i++) {
	Coord t1 = t[i];
	Bevel::rect(
	    c, info.color(colors[i]), nil, info.color(colors[n-i]),
	    t1, x0, y0, x1, y1
	);
	x0 += t1; y0 += t1;
	x1 -= t1; y1 -= t1;
    }
    Bevel::rect(
	c, info.color(colors[b]), info.color(colors[nbands]),
	info.color(colors[nbands+1]), t[b], x0, y0, x1, y1
    );
}

/* class SMFKitInfo */

SMFKitInfo::SMFKitInfo(Style* style) {
    style_ = style;
    Resource::ref(style_);
    load();
}

SMFKitInfo::~SMFKitInfo() {
    unload();
    Resource::unref(style_);
}

void SMFKitInfo::load() {
    Display* d = Session::instance()->default_display();
    const Style& s = *style_;
    s.find_attribute("frameThickness", thickness_);
    s.find_attribute("checkScale", check_scale_);
    s.find_attribute("radioScale", radio_scale_);
    s.find_attribute("moverSize", mover_size_);
    s.find_attribute("sliderSize", slider_size_);

    String v;
    s.find_attribute("flat", v);
    const Color* c = Color::lookup(d, v);
    if (c == nil) {
	c = new Color(0.7, 0.7, 0.7, 1.0);
    }
    color_[black] = c->brightness(-0.85);
    color_[very_dark_gray] = c->brightness(-0.66);
    color_[dark_gray] = c->brightness(-0.5);
    color_[medium_gray] = c->brightness(-0.33);
    color_[light_gray] = c;
    color_[very_light_gray] = c->brightness(0.45);
    color_[white] = c->brightness(0.7);
    color_[gray_out] = new Color(*c, 0.2);
    color_[shadow] = new Color(*color_[black], 0.5);
    color_[yellow] = new Color(1.0, 1.0, 0.0, 1.0);
    color_[light_yellow] = new Color(1.0, 1.0, 0.875, 1.0);
    color_[medium_yellow] = color_[yellow]->brightness(-0.3);
    color_[dark_yellow] = color_[yellow]->brightness(-0.5);
    for (int i = 0; i < num_colors; i++) {
	Resource::ref(color_[i]);
    }

    v = "red3";
    s.find_attribute("checkmarkColor", v);
    const Color* checkmark_color = Color::lookup(d, v);
    if (checkmark_color == nil) {
	checkmark_color = new Color(0.9, 0.0, 0.0, 1.0);
    }
    checkmark_ = new Stencil(
	new Bitmap(checkmark_bits, checkmark_width, checkmark_height, 0, 13),
	checkmark_color
    );
    shadow1_ = new Stencil(
	new Bitmap(shadow1_bits, shadow1_width, shadow1_height, 0, 13),
	color_[dark_gray]
    );
    /*
     * Originally was light_gray, but that only looks reasonable
     * on certain backgrounds.
     */
    shadow2_ = new Stencil(
	new Bitmap(shadow2_bits, shadow2_width, shadow2_height, 0, 13),
	color_[dark_gray]
    );
    Resource::ref(checkmark_);
    Resource::ref(shadow1_);
    Resource::ref(shadow2_);
}

void SMFKitInfo::unload() {
    for (int i = 0; i < num_colors; i++) {
	Resource::unref(color_[i]);
    }
    Resource::unref(checkmark_);
    Resource::unref(shadow1_);
    Resource::unref(shadow2_);
}

void SMFKitInfo::update() {
    unload();
    load();
}

/* class SMFKitFrame */

SMFKitFrame::SMFKitFrame(
    Glyph* g, TelltaleState* t, SMFKitInfo* info, Coord thickness,
    float xalign, float yalign, boolean hmargin, boolean vmargin
) : BevelFrame(g, thickness, xalign, yalign, hmargin, vmargin) {
    state_ = t;
    Resource::ref(state_);
    info_ = info;
    Resource::ref(info_);
}

SMFKitFrame::~SMFKitFrame() {
    Resource::unref(state_);
    Resource::unref(info_);
}

void SMFKitFrame::draw(Canvas* c, const Allocation& a) const {
    BevelFrame::draw(c, a);
    if (!state()->test(TelltaleState::is_enabled)) {
	const SMFKitInfo& i = info();
	Coord t = i.thickness();
	c->fill_rect(
	    a.left() + t, a.bottom() + t, a.right() - t, a.top() - t,
	    i.color(gray_out)
	);
    }
}

static int inset_colors[] = {
    dark_gray, very_dark_gray, light_gray, very_light_gray, white
};

static int bright_inset_colors[] = {
    dark_gray, very_light_gray, white
};

static int outset_colors[] = {
    black, white, light_gray, medium_gray, black
};

static int trough_colors[] = {
    dark_gray, very_light_gray, light_gray, dark_gray, very_dark_gray
};

static int visible_trough_colors[] = {
    medium_gray, white, very_light_gray, medium_gray, dark_gray
};

static int old_thumb_colors[] = {
    very_dark_gray, dark_gray, medium_gray, light_gray, very_light_gray
};

static int* frame_colors[] = {
    /* 0 */ nil,
    /* is_enabled */ trough_colors,
    /* is_visible */ nil,
    /* is_enabled_visible */ visible_trough_colors,
    /* is_active */ nil,
    /* is_enabled_active */ outset_colors,
    /* is_visible_active */ nil,
    /* is_enabled_visible_active */ bright_inset_colors,
    /* is_chosen */ nil,
    /* is_enabled_chosen */ inset_colors,
    /* is_visible_chosen */ nil,
    /* is_enabled_visible_chosen */ nil,
    /* is_active_chosen */ nil,
    /* is_enabled_active_chosen */ old_thumb_colors,
    /* is_visible_active_chosen */ nil,
    /* is_enabled_visible_active_chosen */ nil
};

void SMFKitFrame::draw_frame(
    Canvas* canvas, const Allocation& a, Coord thickness
) const {
    const SMFKitInfo& i = info();
    int* colors = frame_colors[flags(state())];
    if (colors == nil) {
	canvas->fill_rect(
	    a.left(), a.bottom(), a.right(), a.top(), i.color(light_gray)
	);
    } else {
	int ncolors;
	Coord t[2];
	if (colors == bright_inset_colors) {
	    ncolors = 3;
	    t[0] = thickness;
	} else {
	    ncolors = 5;
	    Coord tt = 0.5 * thickness;
	    t[0] = tt;
	    t[1] = tt;
	}
	SMFKitImpl::shade(canvas, a, i, colors, ncolors, t);
    }
}

/* class SMFKitButtonFrame */

SMFKitButtonFrame::SMFKitButtonFrame(
    Glyph* g, TelltaleState* t, SMFKitInfo* info, Coord thickness,
    float xalign, float yalign
) : SMFKitFrame(g, t, info, thickness, xalign, yalign, true, true) { }

SMFKitButtonFrame::~SMFKitButtonFrame() { }

void SMFKitButtonFrame::pick(Canvas*, const Allocation& a, int depth, Hit& h) {
    Coord x = h.left();
    Coord y = h.bottom();
    if (x >= a.left() && x < a.right() && y >= a.bottom() && y < a.top()) {
	h.target(depth, this, 0);
    }
}

static int disabled_button_colors[] = {
    medium_gray, light_gray, light_gray, light_gray,
    light_gray, light_gray, dark_gray
};

static int enabled_button_colors[] = {
    dark_gray, white, very_light_gray, light_gray,
    medium_gray, dark_gray, very_dark_gray
};

static int visible_button_colors[] = {
    dark_gray, white, white, very_light_gray,
    light_gray, dark_gray, very_dark_gray
};

static int active_button_colors[] = {
    very_dark_gray, dark_gray, very_light_gray, light_gray,
    light_gray, black, white
};

static int visible_active_button_colors[] = {
    very_dark_gray, dark_gray, white, very_light_gray,
    light_gray, black, white
};

static int* button_colors[] = {
    /* 0 */ disabled_button_colors,
    /* is_enabled */ enabled_button_colors,
    /* is_visible */ disabled_button_colors,
    /* is_enabled_visible */ visible_button_colors,
    /* is_active */ disabled_button_colors,
    /* is_enabled_active */ active_button_colors,
    /* is_visible_active */ disabled_button_colors,
    /* is_enabled_visible_active */ visible_active_button_colors,
    /* is_chosen */ disabled_button_colors,
    /* is_enabled_chosen */ enabled_button_colors,
    /* is_visible_chosen */ disabled_button_colors,
    /* is_enabled_visible_chosen */ visible_button_colors,
    /* is_active_chosen */ disabled_button_colors,
    /* is_enabled_active_chosen */ active_button_colors,
    /* is_visible_active_chosen */ disabled_button_colors,
    /* is_enabled_visible_active_chosen */ visible_active_button_colors
};

void SMFKitButtonFrame::draw_frame(
    Canvas* c, const Allocation& a, Coord
) const {
    Coord t1 = c->to_pixels_coord(1.0);
    Coord t[3];
    t[0] = t1; t[1] = t1; t[2] = t1 + t1;
    SMFKitImpl::shade(c, a, info(), button_colors[flags(state())], 7, t);
}

/* class SMFKitPushButtonFrame */

SMFKitPushButtonFrame::SMFKitPushButtonFrame(
    Glyph* g, TelltaleState* t, SMFKitInfo* info, Style* s, Coord thickness,
    float xalign, float yalign
) : SMFKitButtonFrame(g, t, info, thickness, xalign, yalign) {
    minwidth_ = 72.0;
    s->find_attribute("minimumWidth", minwidth_);
}

SMFKitPushButtonFrame::~SMFKitPushButtonFrame() { }

void SMFKitPushButtonFrame::request(Requisition& req) const {
    SMFKitButtonFrame::request(req);
    Requirement& rx = req.x_requirement();
    Coord width = rx.natural() + button_border;
    if (width < minwidth_) {
	width = minwidth_;
    }
    rx.natural(width);
    Requirement& ry = req.y_requirement();
    ry.natural(ry.natural() + button_border);
}

/* class SMFKitMenuItemFrame */

SMFKitMenuItemFrame::SMFKitMenuItemFrame(
    Glyph* g, TelltaleState* t, SMFKitInfo* info
) : SMFKitButtonFrame(g, t, info, info->thickness()) { }

SMFKitMenuItemFrame::~SMFKitMenuItemFrame() { }

void SMFKitMenuItemFrame::draw_frame(
    Canvas* c, const Allocation& a, Coord thickness
) const {
    const SMFKitInfo& i = info();
    TelltaleState* t = state();
    if (t->test(TelltaleState::is_active) ||
	t->test(TelltaleState::is_running)
    ) {
	int* colors = outset_colors;
	Coord t[2];
	Coord tt = 0.5 * thickness;
	t[0] = tt;
	t[1] = tt;
	SMFKitImpl::shade(c, a, i, colors, 5, t);
    } else {
	c->fill_rect(
	    a.left(), a.bottom(), a.right(), a.top(), i.color(light_gray)
	);
    }
}

/* class SMFKitGlyph */

SMFKitGlyph::SMFKitGlyph(SMFKitInfo* i) {
    info_ = i;
    Resource::ref(info_);
}

SMFKitGlyph::~SMFKitGlyph() {
    Resource::unref(info_);
}

void SMFKitGlyph::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    ext.merge(c, a);
}

/* class SMFKitCheckmark */

SMFKitCheckmark::SMFKitCheckmark(
    TelltaleState* t, SMFKitInfo* info, const Font* f
) : SMFKitGlyph(info) {
    state_ = t;
    Resource::ref(state_);
    font_ = f;
    Resource::ref(font_);
}

SMFKitCheckmark::~SMFKitCheckmark() {
    Resource::unref(state_);
    Resource::unref(font_);
}

void SMFKitCheckmark::request(Requisition& req) const {
    FontBoundingBox b;
    font_->font_bbox(b);
    Coord size = (b.font_ascent() + b.font_descent()) * info().check_scale();
    req.x_requirement().natural(size);
    req.y_requirement().natural(size);
}

void SMFKitCheckmark::draw(Canvas* c, const Allocation& a) const {
    if (state_->test(TelltaleState::is_chosen)) {
	info().shadow1()->draw(c, a);
	info().shadow2()->draw(c, a);
	info().checkmark()->draw(c, a);
    }
}

/* class SMFKitIndicator */

SMFKitIndicator::SMFKitIndicator(
    TelltaleState* t, SMFKitInfo* i
) : SMFKitGlyph(i) {
    state_ = t;
    Resource::ref(state_);
}

SMFKitIndicator::~SMFKitIndicator() {
    Resource::unref(state_);
}

void SMFKitIndicator::request(Requisition& req) const {
    req.x_requirement().natural(6.0);
}

static int disabled_indicator_colors[] = {
    dark_gray, medium_gray, white
};

static int disabled_chosen_indicator_colors[] = {
    dark_gray, light_yellow, white
};

static int enabled_indicator_colors[] = {
    black, dark_yellow, white
};

static int active_indicator_colors[] = {
    black, yellow, white
};

static int visible_indicator_colors[] = {
    black, medium_yellow, white
};

static int* indicator_colors[] = {
    /* 0 */ disabled_indicator_colors,
    /* is_enabled */ enabled_indicator_colors,
    /* is_visible */ disabled_indicator_colors,
    /* is_enabled_visible */ visible_indicator_colors,
    /* is_active */ disabled_indicator_colors,
    /* is_enabled_active */ enabled_indicator_colors,
    /* is_visible_active */ disabled_indicator_colors,
    /* is_enabled_visible_active */ visible_indicator_colors,
    /* is_chosen */ disabled_chosen_indicator_colors,
    /* is_enabled_chosen */ active_indicator_colors,
    /* is_visible_chosen */ disabled_chosen_indicator_colors,
    /* is_enabled_visible_chosen */ active_indicator_colors,
    /* is_active_chosen */ disabled_chosen_indicator_colors,
    /* is_enabled_active_chosen */ active_indicator_colors,
    /* is_visible_active_chosen */ disabled_chosen_indicator_colors,
    /* is_enabled_visible_active_chosen */ active_indicator_colors
};

void SMFKitIndicator::draw(Canvas* canvas, const Allocation& a) const {
    const SMFKitInfo& i = info();
    int* c = indicator_colors[flags(state_)];
    Coord t1 = canvas->to_pixels_coord(1.0);
    Coord t2 = t1 + t1;
    Bevel::rect(
	canvas, i.color(c[0]), i.color(c[1]), i.color(c[2]), t1,
	a.left(), a.bottom() + t2, a.right(), a.top() - t2
    );
}

/* class SMFKitRadioFlag */

SMFKitRadioFlag::SMFKitRadioFlag(
    TelltaleState* t, SMFKitInfo* i, const Font* f
) : SMFKitGlyph(i) {
    state_ = t;
    Resource::ref(state_);
    font_ = f;
    Resource::ref(font_);
}

SMFKitRadioFlag::~SMFKitRadioFlag() {
    Resource::unref(state_);
    Resource::unref(font_);
}

void SMFKitRadioFlag::request(Requisition& req) const {
    FontBoundingBox b;
    font_->char_bbox('M', b);
    Coord size = b.width() * info().radio_scale();
    req.x_requirement().natural(size);
    req.y_requirement().natural(size);
}

static int outset_radio_colors[] = {
    very_light_gray, light_gray, dark_gray
};

static int visible_outset_radio_colors[] = {
    white, very_light_gray, medium_gray
};

static int inset_radio_colors[] = {
    dark_gray, light_gray, very_light_gray
};

static int visible_inset_radio_colors[] = {
    medium_gray, very_light_gray, white
};
    
static int* radio_colors[] = {
    /* 0 */ outset_radio_colors,
    /* is_enabled */ outset_radio_colors,
    /* is_visible */ outset_radio_colors,
    /* is_enabled_visible */ visible_outset_radio_colors,
    /* is_active */ outset_radio_colors,
    /* is_enabled_active */ inset_radio_colors,
    /* is_visible_active */ outset_radio_colors,
    /* is_enabled_visible_active */ visible_inset_radio_colors,
    /* is_chosen */ inset_radio_colors,
    /* is_enabled_chosen */ inset_radio_colors,
    /* is_visible_chosen */ inset_radio_colors,
    /* is_enabled_visible_chosen */ visible_inset_radio_colors,
    /* is_active_chosen */ inset_radio_colors,
    /* is_enabled_active_chosen */ inset_radio_colors,
    /* is_visible_active_chosen */ inset_radio_colors,
    /* is_enabled_visible_active_chosen */ visible_inset_radio_colors
};

void SMFKitRadioFlag::draw(Canvas* c, const Allocation& a) const {
    const SMFKitInfo& i = info();
    int* colors = radio_colors[flags(state_)];
    Bevel::diamond(
	c, i.color(colors[0]), i.color(colors[1]), i.color(colors[2]),
	i.thickness(), a.left(), a.bottom(), a.right(), a.top()
    );
}

/* class SMFKitRadioItem */

SMFKitRadioItem::SMFKitRadioItem(
    TelltaleState* t, SMFKitInfo* i, const Font* f
) : SMFKitRadioFlag(t, i, f) { }

SMFKitRadioItem::~SMFKitRadioItem() { }

void SMFKitRadioItem::draw(Canvas* c, const Allocation& a) const {
    if (state()->test(TelltaleState::is_chosen)) {
	SMFKitRadioFlag::draw(c, a);
    }
}

/* class SMFKitThumb */

SMFKitThumb::SMFKitThumb(
    DimensionName d, SMFKitInfo* info, long ridges, const TelltaleFlags s
) : SMFKitGlyph(info) {
    dimension_ = d;
    ridges_ = ridges;
    flags_ = s;
}

SMFKitThumb::~SMFKitThumb() { }

static int thumb_colors[] = {
    very_light_gray, very_light_gray, light_gray, medium_gray, dark_gray
};

static int visible_thumb_colors[] = {
    white, white, very_light_gray, light_gray, dark_gray
};

void SMFKitThumb::draw(Canvas* canvas, const Allocation& a) const {
    const SMFKitInfo& i = info();
    int* colors;
    if ((flags_ & TelltaleState::is_visible) != 0) {
	colors = visible_thumb_colors;
    } else {
	colors = thumb_colors;
    }
    const Color* c[6];
    for (int c_i = 0; c_i < 5; c_i++) {
	c[c_i] = i.color(colors[c_i]);
    }
    Coord x0 = a.left(), y0 = a.bottom(), x1 = a.right(), y1 = a.top();
    Coord p = canvas->to_pixels_coord(1.0);
    Coord p2 = p + p, p3 = p2 + p, p4 = p3 + p, p5 = p4 + p;
    const Color* dark = i.color(very_dark_gray);
    switch (dimension_) {
    case Dimension_X:
	canvas->fill_rect(x0, y0, x0 + p, y1, dark);
	canvas->fill_rect(x1 - p, y0, x1, y1, dark);
	x0 += p;
	x1 -= p;
	break;
    case Dimension_Y:
	canvas->fill_rect(x0, y1 - p, x1, y1, dark);
	canvas->fill_rect(x0, y0, x1, y0 + p, dark);
	y0 += p;
	y1 -= p;
    }
    Bevel::rect(canvas, c[0], nil, c[4], p, x0, y0, x1, y1);
    Bevel::rect(canvas, c[1], c[2], c[3], p, x0 + p, y0 + p, x1 - p, y1 - p);

    Coord mid;
    Coord left[6], bottom[6], right[6], top[6];
    switch (dimension_) {
    case Dimension_X:
	mid = canvas->to_pixels_coord(0.5 * (x0 + x1));
	y0 += p; y1 -= p;
	c[0] = i.color(very_dark_gray);
	c[1] = i.color(white);
	left[0] = mid + p4; bottom[0] = y0; right[0] = mid + p5; top[0] = y1;
	left[1] = mid + p3; bottom[1] = y0; right[1] = mid + p4; top[1] = y1;
	left[2] = mid; bottom[2] = y0; right[2] = mid + p; top[2] = y1;
	left[3] = mid - p; bottom[3] = y0; right[3] = mid; top[3] = y1;
	left[4] = mid - p4; bottom[4] = y0; right[4] = mid - p3; top[4] = y1;
	left[5] = mid - p5; bottom[5] = y0; right[5] = mid - p4; top[5] = y1;
	break;
    case Dimension_Y:
	mid = canvas->to_pixels_coord(0.5 * (y0 + y1));
	x0 += p; x1 -= p;
	c[0] = i.color(white);
	c[1] = i.color(very_dark_gray);
	left[0] = x0; bottom[0] = mid + p4; right[0] = x1; top[0] = mid + p5;
	left[1] = x0; bottom[1] = mid + p3; right[1] = x1; top[1] = mid + p4;
	left[2] = x0; bottom[2] = mid; right[2] = x1; top[2] = mid + p;
	left[3] = x0; bottom[3] = mid - p; right[3] = x1; top[3] = mid;
	left[4] = x0; bottom[4] = mid - p4; right[4] = x1; top[4] = mid - p3;
	left[5] = x0; bottom[5] = mid - p5; right[5] = x1; top[5] = mid - p4;
	break;
    default:
	return;
    }
    c[2] = c[0];
    c[3] = c[1];
    c[4] = c[0];
    c[5] = c[1];
    for (long r = 3 - ridges_; r < 3 + ridges_; r++) {
	canvas->fill_rect(left[r], bottom[r], right[r], top[r], c[r]);
    }
}

/* class SMFKitDefaultArrow */

SMFKitDefaultArrow::SMFKitDefaultArrow(
    const Font* f, SMFKitInfo* i
) : SMFKitGlyph(i) {
    font_ = f;
    Resource::ref(f);
}

SMFKitDefaultArrow::~SMFKitDefaultArrow() {
    Resource::unref(font_);
}

void SMFKitDefaultArrow::request(Requisition& req) const {
    FontBoundingBox b;
    font_->string_bbox("M", 1, b);
    Requirement& rx = req.requirement(Dimension_X);
    rx.natural(1.25 * b.width());
    rx.stretch(0);
    rx.shrink(0);
    rx.alignment(0);
    Requirement& ry = req.requirement(Dimension_Y);
    Coord h = b.font_ascent() + b.font_descent();
    ry.natural(h);
    ry.stretch(0);
    ry.shrink(0);
    ry.alignment(h == 0 ? 0.0 : b.font_descent() / h);
}

void SMFKitDefaultArrow::draw(Canvas* c, const Allocation& a) const {
    const SMFKitInfo& i = info();
    FontBoundingBox b;
    font_->string_bbox("M", 1, b);
    Coord left = a.left();
    Coord right = a.right();
    Coord x0 = left + 0.4 * (right - left);
    Coord bottom = a.y();
    Coord top = bottom + 0.9 * b.ascent();
    Coord y0 = 0.5 * (bottom + top);
    const Color* bl = i.color(black);

    c->new_path();
    c->move_to(left, y0);
    c->line_to(x0, top);
    c->line_to(x0, bottom);
    c->close_path();
    c->fill(bl);

    Coord t = i.thickness();
    y0 -= 0.5 * t;
    Coord y1 = y0 + t;
    c->fill_rect(x0, y0, right, y1, bl);
    c->fill_rect(right - t, y1, right, top, bl);
}
