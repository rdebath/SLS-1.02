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
 * WidgetKit -- object for creating common UI objects
 */

#ifdef motif_kit
#include <IV-look/mf_kit.h>
#endif
#ifdef bw_kit
#include <IV-look/mono_kit.h>
#endif
#ifdef openlook_kit
#include <IV-look/ol_kit.h>
#endif
#ifdef sgi_motif_kit
#include <IV-look/smf_kit.h>
#endif
#include <IV-look/bevel.h>
#include <InterViews/background.h>
#include <InterViews/bitmap.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/cursor.h>
#include <InterViews/display.h>
#include <InterViews/font.h>
#include <InterViews/label.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/Bitmaps/hand.bm>
#include <InterViews/Bitmaps/handMask.bm>
#include <InterViews/Bitmaps/lfast.bm>
#include <InterViews/Bitmaps/lfastMask.bm>
#include <InterViews/Bitmaps/lufast.bm>
#include <InterViews/Bitmaps/lufastMask.bm>
#include <InterViews/Bitmaps/ufast.bm>
#include <InterViews/Bitmaps/ufastMask.bm>
#include <InterViews/Bitmaps/rufast.bm>
#include <InterViews/Bitmaps/rufastMask.bm>
#include <InterViews/Bitmaps/rfast.bm>
#include <InterViews/Bitmaps/rfastMask.bm>
#include <InterViews/Bitmaps/rdfast.bm>
#include <InterViews/Bitmaps/rdfastMask.bm>
#include <InterViews/Bitmaps/dfast.bm>
#include <InterViews/Bitmaps/dfastMask.bm>
#include <InterViews/Bitmaps/ldfast.bm>
#include <InterViews/Bitmaps/ldfastMask.bm>
#include <OS/list.h>
#include <OS/string.h>
#include <OS/ustring.h>
#include <stdio.h>

declareActionCallback(Session)
implementActionCallback(Session)

declarePtrList(WidgetKitStyleList,Style)
implementPtrList(WidgetKitStyleList,Style)

class WidgetKitImpl {
private:
    friend class WidgetKit;

    static WidgetKit* kit_;

    static WidgetKit* make_kit();

    Style* style_;
    boolean style_changed_;
    const Font* font_;
    const Color* foreground_;
    const Color* background_;
    WidgetKitStyleList styles_;

    Cursor* hand_cursor_;
    Cursor* lfast_cursor_;
    Cursor* lufast_cursor_;
    Cursor* ufast_cursor_;
    Cursor* rufast_cursor_;
    Cursor* rfast_cursor_;
    Cursor* rdfast_cursor_;
    Cursor* dfast_cursor_;
    Cursor* ldfast_cursor_;

    boolean initialized_label_styles_;
    UniqueString* chiseled_label_style_;
    UniqueString* raised_label_style_;

    WidgetKitImpl();
    ~WidgetKitImpl();

    void style(Style*);
    Style* style();
    void update_style_info();
    void report_error(
	Session*, const char* op, const String& name, const char* value
    );
    WidgetKitImpl* updated();

    TelltaleState* begin_style(const char*, const char*, TelltaleFlags);
    TelltaleState* begin_menubar_item_style();
    TelltaleState* begin_menu_item_style();
    TelltaleState* begin_check_menu_item_style();
    TelltaleState* begin_radio_menu_item_style(TelltaleGroup*);
    TelltaleState* begin_menu_item_separator_style();
    MenuItem* end_menu_item_style(Glyph*, TelltaleState*);
    TelltaleState* begin_push_button_style();
    TelltaleState* begin_default_button_style();
    TelltaleState* begin_check_box_style();
    TelltaleState* begin_palette_button_style();
    TelltaleState* begin_radio_button_style(TelltaleGroup*);
    Button* end_button_style(Glyph*, TelltaleState*, Action*);

    Glyph* build_fancy_label(Glyph* above, Glyph* below, Glyph* g);
    ColorIntensity label_shading();
};

WidgetKit* WidgetKitImpl::kit_;

WidgetKit::WidgetKit() {
    impl_ = new WidgetKitImpl;
    WidgetKitImpl::kit_ = this;
}

WidgetKit::~WidgetKit() {
    delete impl_;
}

WidgetKit* WidgetKit::instance() {
    WidgetKit* k = WidgetKitImpl::kit_;
    if (k == nil) {
	k = WidgetKitImpl::make_kit();
	k->style_changed(k->impl_->style_);
    }
    return k;
}

const char* WidgetKit::gui() const { return "unknown"; }

void WidgetKit::style(Style* s) { impl_->style(s); style_changed(s); }
Style* WidgetKit::style() const { return impl_->style_; }

const Font* WidgetKit::font() const {
    return impl_->updated()->font_;
}

const Color* WidgetKit::foreground() const {
    return impl_->updated()->foreground_;
}

const Color* WidgetKit::background() const {
    return impl_->updated()->background_;
}

void WidgetKit::begin_style(const String& str) {
    push_style(new Style(str, style()));
}

void WidgetKit::begin_style(const char* str) { begin_style(String(str)); }

void WidgetKit::begin_style(const String& str_name, const String& str_alias) {
    Style* s = new Style(str_name, style());
    s->alias(str_alias);
    push_style(s);
}

void WidgetKit::begin_style(const char* s, const char* a) {
    begin_style(String(s), String(a));
}

void WidgetKit::alias(const String& str) { style()->alias(str); }
void WidgetKit::alias(const char* str) { style()->alias(str); }
void WidgetKit::end_style() { pop_style(); }

void WidgetKit::push_style() {
    WidgetKitImpl& k = *impl_;
    k.styles_.prepend(k.style_);
    Resource::ref(k.style_);
}

void WidgetKit::push_style(Style* s) {
    push_style();
    style(s);
}

void WidgetKit::pop_style() {
    WidgetKitImpl& k = *impl_;
    long n = k.styles_.count();
    if (n != 0) {
	Style* s = k.styles_.item(0);
	k.styles_.remove(0);
	k.style(s);
	Resource::unref(s);
	style_changed(s);
    }
}

void WidgetKit::style_changed(Style*) { }

#if defined(__STDC__) || defined(__ANSI_CPP__)
#define __concat(first,second) first##second
#else
#define __concat(first,second) first/**/second
#endif

#define concat(first,second) __concat(first,second)

#define WidgetKit_cursor_function(c) \
Cursor* WidgetKit::concat(c,_cursor)() const { \
    WidgetKitImpl& k = *impl_; \
    if (k.concat(c,_cursor_) == nil) { \
	k.concat(c,_cursor_) = new Cursor( \
	    new Bitmap( \
		concat(c,_bits), concat(c,_width), concat(c,_height), \
		concat(c,_x_hot), concat(c,_y_hot) \
	    ), \
	    new Bitmap( \
		concat(c,_mask_bits), \
		concat(c,_mask_width), concat(c,_mask_height) \
	    ) \
	); \
    } \
    return k.concat(c,_cursor_); \
}

WidgetKit_cursor_function(hand)
WidgetKit_cursor_function(lfast)
WidgetKit_cursor_function(lufast)
WidgetKit_cursor_function(ufast)
WidgetKit_cursor_function(rufast)
WidgetKit_cursor_function(rfast)
WidgetKit_cursor_function(rdfast)
WidgetKit_cursor_function(dfast)
WidgetKit_cursor_function(ldfast)

Glyph* WidgetKit::label(const char* str) const {
    return new Label(str, font(), foreground());
}

Glyph* WidgetKit::label(const String& str) const {
    return new Label(str, font(), foreground());
}

Glyph* WidgetKit::chiseled_label(const char* str) const {
    return chiseled_label(String(str));
}

Glyph* WidgetKit::chiseled_label(const String& str) const {
    WidgetKitImpl& k = *impl_;
    const Font* f = font();
    ColorIntensity shading = k.label_shading();
    return k.build_fancy_label(
	nil,
	new Label(str, f, new Color(shading, shading, shading, 1.0)),
	new Label(str, f, foreground())
    );
}

Glyph* WidgetKit::raised_label(const char* str) const {
    return raised_label(String(str));
}

Glyph* WidgetKit::raised_label(const String& str) const {
    WidgetKitImpl& k = *impl_;
    const Font* f = font();
    ColorIntensity shading = k.label_shading();
    return k.build_fancy_label(
	new Label(str, f, new Color(shading, shading, shading, 1.0)),
	nil,
	new Label(str, f, foreground())
    );
}

Glyph* WidgetKit::fancy_label(const char* str) const {
    return fancy_label(String(str));
}

Glyph* WidgetKit::fancy_label(const String& str) const {
    WidgetKitImpl& k = *impl_;
    String v;
    if (k.style()->find_attribute("labelStyle", v)) {
	UniqueString u(v);
	if (!k.initialized_label_styles_) {
	    k.chiseled_label_style_ = new UniqueString("chiseled");
	    k.raised_label_style_ = new UniqueString("raised");
	    k.initialized_label_styles_ = true;
	}
	if (u == *k.chiseled_label_style_) {
	    return chiseled_label(str);
	}
	if (u == *k.raised_label_style_) {
	    return raised_label(str);
	}
    }
    return label(str);
}

Menu* WidgetKit::menubar() const {
    WidgetKit* k = (WidgetKit*)this;
    k->begin_style("MenuBar", "Menu");
    Menu* m = new Menu(menubar_look(), style(), 0.0, 0.0, 0.0, 1.0);
    k->end_style();
    return m;
}

Menu* WidgetKit::pulldown() const {
    WidgetKit* k = (WidgetKit*)this;
    k->begin_style("Pulldown", "Menu");
    Menu* m = new Menu(pulldown_look(), style(), 1.0, 1.0, 0.0, 1.0);
    k->end_style();
    return m;
}

Menu* WidgetKit::pullright() const {
    WidgetKit* k = (WidgetKit*)this;
    k->begin_style("Pullright", "Menu");
    Menu* m = new Menu(pullright_look(), style(), 1.0, 1.0, 0.0, 1.0);
    k->end_style();
    return m;
}

Glyph* WidgetKit::pullright_look() const {
    return pulldown_look();
}

MenuItem* WidgetKit::menubar_item(const char* s) const {
    TelltaleState* t = impl_->begin_menubar_item_style();
    return impl_->end_menu_item_style(menubar_item_look(label(s), t), t);
}

MenuItem* WidgetKit::menubar_item(const String& s) const {
    TelltaleState* t = impl_->begin_menubar_item_style();
    return impl_->end_menu_item_style(menubar_item_look(label(s), t), t);
}

MenuItem* WidgetKit::menubar_item(Glyph* g) const {
    TelltaleState* t = impl_->begin_menubar_item_style();
    return impl_->end_menu_item_style(menubar_item_look(g, t), t);
}

MenuItem* WidgetKit::menu_item(const char* s) const {
    TelltaleState* t = impl_->begin_menu_item_style();
    return impl_->end_menu_item_style(menu_item_look(label(s), t), t);
}

MenuItem* WidgetKit::menu_item(const String& s) const {
    TelltaleState* t = impl_->begin_menu_item_style();
    return impl_->end_menu_item_style(menu_item_look(label(s), t), t);
}

MenuItem* WidgetKit::menu_item(Glyph* g) const {
    TelltaleState* t = impl_->begin_menu_item_style();
    return impl_->end_menu_item_style(menu_item_look(g, t), t);
}

MenuItem* WidgetKit::check_menu_item(const char* s) const {
    TelltaleState* t = impl_->begin_check_menu_item_style();
    return impl_->end_menu_item_style(check_menu_item_look(label(s), t), t);
}

MenuItem* WidgetKit::check_menu_item(const String& s) const {
    TelltaleState* t = impl_->begin_check_menu_item_style();
    return impl_->end_menu_item_style(check_menu_item_look(label(s), t), t);
}

MenuItem* WidgetKit::check_menu_item(Glyph* g) const {
    TelltaleState* t = impl_->begin_check_menu_item_style();
    return impl_->end_menu_item_style(check_menu_item_look(g, t), t);
}

MenuItem* WidgetKit::radio_menu_item(TelltaleGroup* g, const char* s) const {
    TelltaleState* t = impl_->begin_radio_menu_item_style(g);
    return impl_->end_menu_item_style(radio_menu_item_look(label(s), t), t);
}

MenuItem* WidgetKit::radio_menu_item(TelltaleGroup* g, const String& s) const {
    TelltaleState* t = impl_->begin_radio_menu_item_style(g);
    return impl_->end_menu_item_style(radio_menu_item_look(label(s), t), t);
}

MenuItem* WidgetKit::radio_menu_item(TelltaleGroup* group, Glyph* g) const {
    TelltaleState* t = impl_->begin_radio_menu_item_style(group);
    return impl_->end_menu_item_style(radio_menu_item_look(g, t), t);
}

MenuItem* WidgetKit::menu_item_separator() const {
    TelltaleState* t = impl_->begin_menu_item_separator_style();
    return impl_->end_menu_item_style(menu_item_separator_look(), t);
}

Button* WidgetKit::push_button(const char* s, Action* a) const {
    TelltaleState* t = impl_->begin_push_button_style();
    return impl_->end_button_style(push_button_look(label(s), t), t, a);
}

Button* WidgetKit::push_button(const String& s, Action* a) const {
    TelltaleState* t = impl_->begin_push_button_style();
    return impl_->end_button_style(push_button_look(label(s), t), t, a);
}

Button* WidgetKit::push_button(Glyph* g, Action* a) const {
    TelltaleState* t = impl_->begin_push_button_style();
    return impl_->end_button_style(push_button_look(g, t), t, a);
}

Button* WidgetKit::default_button(const char* s, Action* a) const {
    TelltaleState* t = impl_->begin_default_button_style();
    return impl_->end_button_style(default_button_look(label(s), t), t, a);
}

Button* WidgetKit::default_button(const String& s, Action* a) const {
    TelltaleState* t = impl_->begin_default_button_style();
    return impl_->end_button_style(default_button_look(label(s), t), t, a);
}

Button* WidgetKit::default_button(Glyph* g, Action* a) const {
    TelltaleState* t = impl_->begin_default_button_style();
    return impl_->end_button_style(default_button_look(g, t), t, a);
}

Button* WidgetKit::check_box(const char* s, Action* a) const {
    TelltaleState* t = impl_->begin_check_box_style();
    return impl_->end_button_style(check_box_look(label(s), t), t, a);
}

Button* WidgetKit::check_box(const String& s, Action* a) const {
    TelltaleState* t = impl_->begin_check_box_style();
    return impl_->end_button_style(check_box_look(label(s), t), t, a);
}

Button* WidgetKit::check_box(Glyph* g, Action* a) const {
    TelltaleState* t = impl_->begin_check_box_style();
    return impl_->end_button_style(check_box_look(g, t), t, a);
}

Button* WidgetKit::palette_button(const char* s, Action* a) const {
    TelltaleState* t = impl_->begin_palette_button_style();
    return impl_->end_button_style(palette_button_look(label(s), t), t, a);
}

Button* WidgetKit::palette_button(const String& s, Action* a) const {
    TelltaleState* t = impl_->begin_palette_button_style();
    return impl_->end_button_style(palette_button_look(label(s), t), t, a);
}

Button* WidgetKit::palette_button(Glyph* g, Action* a) const {
    TelltaleState* t = impl_->begin_palette_button_style();
    return impl_->end_button_style(palette_button_look(g, t), t, a);
}

Button* WidgetKit::radio_button(
    TelltaleGroup* g, const char* s, Action* a
) const {
    TelltaleState* t = impl_->begin_radio_button_style(g);
    return impl_->end_button_style(radio_button_look(label(s), t), t, a);
}

Button* WidgetKit::radio_button(
    TelltaleGroup* g, const String& s, Action* a
) const {
    TelltaleState* t = impl_->begin_radio_button_style(g);
    return impl_->end_button_style(radio_button_look(label(s), t), t, a);
}

Button* WidgetKit::radio_button(
    TelltaleGroup* group, Glyph* g, Action* a
) const {
    TelltaleState* t = impl_->begin_radio_button_style(group);
    return impl_->end_button_style(radio_button_look(g, t), t, a);
}

Action* WidgetKit::quit() const {
    return new ActionCallback(Session)(Session::instance(), &Session::quit);
}

Glyph* WidgetKit::hslider(Adjustable* a) const {
    WidgetKit* k = (WidgetKit*)this;
    k->begin_style("HSlider", "Slider");
    Glyph* g = slider_look(Dimension_X, a);
    k->end_style();
    return g;
}

Glyph* WidgetKit::hscroll_bar(Adjustable* a) const {
    WidgetKit* k = (WidgetKit*)this;
    k->begin_style("HScrollBar", "ScrollBar");
    Glyph* g = scroll_bar_look(Dimension_X, a);
    k->end_style();
    return g;
}

Glyph* WidgetKit::vslider(Adjustable* a) const {
    WidgetKit* k = (WidgetKit*)this;
    k->begin_style("VSlider", "Slider");
    Glyph* g = slider_look(Dimension_Y, a);
    k->end_style();
    return g;
}

Glyph* WidgetKit::vscroll_bar(Adjustable* a) const {
    WidgetKit* k = (WidgetKit*)this;
    k->begin_style("VScrollBar", "ScrollBar");
    Glyph* g = scroll_bar_look(Dimension_Y, a);
    k->end_style();
    return g;
}

Glyph* WidgetKit::panner(Adjustable* x, Adjustable* y) const {
    WidgetKit* k = (WidgetKit*)this;
    k->begin_style("Panner");
    Glyph* g = panner_look(x, y);
    k->end_style();
    return g;
}

Stepper* WidgetKit::enlarger(Adjustable*) const {
    /* unimplemented */
    return nil;
}

Stepper* WidgetKit::reducer(Adjustable*) const {
    /* unimplemented */
    return nil;
}

Stepper* WidgetKit::up_mover(Adjustable* a) const {
    TelltaleState* t = impl_->begin_style(
	"UpMover", "Button", TelltaleState::is_enabled
    );
    Stepper* s = new ForwardScroller(
	up_mover_look(t), style(), t, a, Dimension_Y
    );
    ((WidgetKit*)this)->end_style();
    return s;
}

Stepper* WidgetKit::down_mover(Adjustable* a) const {
    TelltaleState* t = impl_->begin_style(
	"DownMover", "Button", TelltaleState::is_enabled
    );
    Stepper* s = new BackwardScroller(
	down_mover_look(t), style(), t, a, Dimension_Y
    );
    ((WidgetKit*)this)->end_style();
    return s;
}

Stepper* WidgetKit::left_mover(Adjustable* a) const {
    TelltaleState* t = impl_->begin_style(
	"LeftMover", "Button", TelltaleState::is_enabled
    );
    Stepper* s = new BackwardScroller(
	left_mover_look(t), style(), t, a, Dimension_X
    );
    ((WidgetKit*)this)->end_style();
    return s;
}

Stepper* WidgetKit::right_mover(Adjustable* a) const {
    TelltaleState* t = impl_->begin_style(
	"RightMover", "Button", TelltaleState::is_enabled
    );
    Stepper* s = new ForwardScroller(
	right_mover_look(t), style(), t, a, Dimension_X
    );
    ((WidgetKit*)this)->end_style();
    return s;
}

WidgetKitImpl::WidgetKitImpl() {
    style_ = nil;
    style_changed_ = false;
    font_ = nil;
    foreground_ = nil;
    background_ = nil;
    style(Session::instance()->style());

    hand_cursor_ = nil;
    lfast_cursor_ = nil;
    lufast_cursor_ = nil;
    ufast_cursor_ = nil;
    rufast_cursor_ = nil;
    rfast_cursor_ = nil;
    rdfast_cursor_ = nil;
    dfast_cursor_ = nil;
    ldfast_cursor_ = nil;

    initialized_label_styles_ = false;
    chiseled_label_style_ = nil;
    raised_label_style_ = nil;
}

WidgetKitImpl::~WidgetKitImpl() {
    Resource::unref(style_);
    Resource::unref(font_);
    Resource::unref(foreground_);
    Resource::unref(background_);
    delete hand_cursor_;
    delete lfast_cursor_;
    delete lufast_cursor_;
    delete ufast_cursor_;
    delete rufast_cursor_;
    delete rfast_cursor_;
    delete rdfast_cursor_;
    delete dfast_cursor_;
}

void WidgetKitImpl::style(Style* s) {
    Resource::ref(s);
    Resource::unref(style_);
    style_ = s;
    style_changed_ = true;
}

Style* WidgetKitImpl::style() { return style_; }

void WidgetKitImpl::update_style_info() {
    Style* s = style_;
    Session* session = Session::instance();
    Display* d = session->default_display();
    String v;
    if (s->find_attribute("font", v) || s->find_attribute("Font", v)) {
	const Font* f = Font::lookup(v);
	if (f == nil) {
	    if (font_ == nil) {
		const char* default_font = "fixed";
		report_error(session, "open font", v, default_font);
		font_ = Font::lookup(default_font);
		Resource::ref(font_);
	    }
	} else {
	    Resource::ref(f);
	    Resource::unref(font_);
	    font_ = f;
	}
    }
    if (s->find_attribute("foreground", v) ||
	s->find_attribute("Foreground", v)
    ) {
	const Color* c = Color::lookup(d, v);
	if (c == nil) {
	    if (foreground_ == nil) {
		const char* default_foreground = "#000000";
		report_error(session, "find color", v, default_foreground);
		foreground_ = Color::lookup(d, default_foreground);
		Resource::ref(foreground_);
	    }
	} else {
	    Resource::ref(c);
	    Resource::unref(foreground_);
	    foreground_ = c;
	}
    }
    if (s->find_attribute("background", v) ||
	s->find_attribute("Background", v)
    ) {
	const Color* c = Color::lookup(d, v);
	if (c == nil) {
	    if (background_ == nil) {
		const char* default_background = "#ffffff";
		report_error(session, "find color", v, default_background);
		background_ = Color::lookup(d, default_background);
		Resource::ref(background_);
	    }
	} else {
	    Resource::ref(c);
	    Resource::unref(background_);
	    background_ = c;
	}
    }
    style_changed_ = false;
}

void WidgetKitImpl::report_error(
    Session* s, const char* op, const String& name, const char* value
) {
    fprintf(
	stderr, "%s: unable to %s \"%.*s\", using \"%s\"\n",
	s->name(), op, name.length(), name.string(), value
    );
}

WidgetKitImpl* WidgetKitImpl::updated() {
    if (style_changed_) {
	update_style_info();
    }
    return this;
}

WidgetKit* WidgetKitImpl::make_kit() {
    String gui;
    if (Session::instance()->style()->find_attribute("gui", gui)) {
#ifdef bw_kit
	if (gui == "monochrome") {
	    return new MonoKit;
	}
#endif
#if defined(motif_kit)
	if (gui == "Motif" || gui == "motif") {
	    return new MFKit;
	}
#endif
#ifdef openlook_kit
	if (gui == "OpenLook" || gui == "openlook") {
	    return new OLKit;
	}
#endif
#ifdef sgi_motif_kit
	if (gui == "SGIMotif" || gui == "sgimotif") {
	    return new SMFKit;
	}
#endif
    }

#ifdef bw_kit
    const Color* c1 = new Color(0.0, 0.0, 0.0, 1.0);
    Resource::ref(c1);
    const Color* c2 = new Color(1.0, 1.0, 1.0, 1.0);
    Resource::ref(c2);
    const Color* c3 = new Color(0.5, 0.5, 0.5, 1.0);
    Resource::ref(c3);
    if (!c3->distinguished(c1) || !c3->distinguished(c2)) {
	return new MonoKit;
    }
    Resource::unref(c1);
    Resource::unref(c2);
    Resource::unref(c3);
#endif

#ifdef default_kit
    return new default_kit;
#else

#ifdef sgi_motif_kit
    return new SMFKit;
#endif
#ifdef motif_kit
    return new MFKit;
#endif
#ifdef openlook_kit
    return new OLKit;
#endif
#ifdef bw_kit
    return new MonoKit;
#endif

#endif
}

TelltaleState* WidgetKitImpl::begin_style(
    const char* s1, const char* s2, TelltaleFlags f
) {
    if (s2 == nil) {
	kit_->begin_style(s1);
    } else {
	kit_->begin_style(s1, s2);
    }
    return new TelltaleState(f);
}

TelltaleState* WidgetKitImpl::begin_menubar_item_style() {
    return begin_style("MenuBar", "Menu", TelltaleState::is_enabled);
}

TelltaleState* WidgetKitImpl::begin_menu_item_style() {
    return begin_style("MenuItem", nil, TelltaleState::is_enabled);
}

TelltaleState* WidgetKitImpl::begin_check_menu_item_style() {
    return begin_style(
	"ToggleButton", "MenuItem",
	TelltaleState::is_enabled | TelltaleState::is_toggle
    );
}

TelltaleState* WidgetKitImpl::begin_radio_menu_item_style(TelltaleGroup* g) {
    TelltaleState* t = begin_style(
	"RadioButton", "MenuItem",
	TelltaleState::is_enabled | TelltaleState::is_choosable
    );
    t->join(g);
    return t;
}

TelltaleState* WidgetKitImpl::begin_menu_item_separator_style() {
    return begin_style("MenuSeparator", "MenuItem", 0);
}

MenuItem* WidgetKitImpl::end_menu_item_style(Glyph* g, TelltaleState* t) {
    MenuItem* i = new MenuItem(g, t);
    kit_->end_style();
    return i;
}

TelltaleState* WidgetKitImpl::begin_push_button_style() {
    return begin_style("PushButton", "Button", TelltaleState::is_enabled);
}

TelltaleState* WidgetKitImpl::begin_default_button_style() {
    Style* s = new Style("DefaultButton", style());
    s->alias("PushButton");
    s->alias("Button");
    kit_->push_style(s);
    return new TelltaleState(TelltaleState::is_enabled);
}

TelltaleState* WidgetKitImpl::begin_check_box_style() {
    return begin_style(
	"ToggleButton", "Button",
	TelltaleState::is_enabled | TelltaleState::is_toggle
    );
}

TelltaleState* WidgetKitImpl::begin_palette_button_style() {
    return begin_style(
	"PaletteButton", "Button",
	TelltaleState::is_enabled | TelltaleState::is_toggle
    );
}

TelltaleState* WidgetKitImpl::begin_radio_button_style(TelltaleGroup* g) {
    TelltaleState* t = begin_style(
	"RadioButton", "Button",
	TelltaleState::is_enabled | TelltaleState::is_choosable
    );
    t->join(g);
    return t;
}

Button* WidgetKitImpl::end_button_style(
    Glyph* g, TelltaleState* t, Action* a
) {
    Button* b = new Button(g, style(), t, a);
    kit_->end_style();
    return b;
}

class WidgetKitOverlay : public Glyph {
public:
    WidgetKitOverlay(Glyph* g1, Glyph* g2, Glyph* g3);
    virtual ~WidgetKitOverlay();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);
private:
    Glyph* first_;
    Glyph* second_;
    Glyph* third_;
};

WidgetKitOverlay::WidgetKitOverlay(Glyph* g1, Glyph* g2, Glyph* g3) {
    first_ = g1;
    Resource::ref(g1);
    second_ = g2;
    Resource::ref(g2);
    third_ = g3;
    Resource::ref(g3);
}

WidgetKitOverlay::~WidgetKitOverlay() {
    Resource::unref(first_);
    Resource::unref(second_);
    Resource::unref(third_);
}

void WidgetKitOverlay::request(Requisition& req) const {
    third_->request(req);
}

void WidgetKitOverlay::allocate(
    Canvas* c, const Allocation& a, Extension& ext
) {
    first_->allocate(c, a, ext);
    second_->allocate(c, a, ext);
    third_->allocate(c, a, ext);
}

void WidgetKitOverlay::draw(Canvas* c, const Allocation& a) const {
    first_->draw(c, a);
    second_->draw(c, a);
    third_->draw(c, a);
}

void WidgetKitOverlay::pick(
    Canvas* c, const Allocation& a, int depth, Hit& h
) {
    third_->pick(c, a, depth, h);
}

Glyph* WidgetKitImpl::build_fancy_label(Glyph* above, Glyph* below, Glyph* g) {
    LayoutKit& layout = *LayoutKit::instance();
    Coord offset = 1.0;
    Glyph* g1, * g2, * g3;
    if (above != nil) {
	g1 = layout.margin(above, 0.0, offset, offset, 0.0);
	g2 = layout.margin(above, 0.0, offset, 0.0, 0.0);
	g3 = layout.margin(g, offset, 0.0, 0.0, offset);
    } else {
	g1 = layout.margin(below, offset, 0.0, 0.0, offset);
	g2 = layout.margin(below, 0.0, 0.0, 0.0, offset);
	g3 = layout.margin(g, 0.0, offset, offset, 0.0);
    }
    return new WidgetKitOverlay(g1, g2, g3);
}

ColorIntensity WidgetKitImpl::label_shading() {
    ColorIntensity shading = 0.9;
    style()->find_attribute("labelShading", shading);
    return shading;
}
