/*
 * Copyright (c) 1990, 1991 Stanford University
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

#include <IV-look/dialogs.h>
#include <IV-look/kit.h>
#include <IV-look/menu.h>
#include <InterViews/action.h>
#include <InterViews/adjust.h>
#include <InterViews/background.h>
#include <InterViews/color.h>
#include <InterViews/composition.h>
#include <InterViews/display.h>
#include <InterViews/font.h>
#include <InterViews/layout.h>
#include <InterViews/monoglyph.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/texcomp.h>
#include <InterViews/tformsetter.h>
#include <InterViews/transformer.h>
#include <InterViews/window.h>
#include <OS/string.h>
#include <stdio.h>

static const Coord tool_scale = 12.0;

struct CommandInfo;

class App {
public:
    App();
    ~App();

    int run(int, char**);

    void open();
    void save();

    void cut();
    void copy();
    void paste();

    void black();
    void red();
    void green();
    void blue();

    void do_nothing();
    void quit();
private:
    WidgetKit* kit_;
    LayoutKit* layout_;
    Style* style_;
    Style* title_style_;
    Menu* file_;
    Composition* body_;
    Glyph* sep_;
    Coord size_;
    ApplicationWindow* main_;
    FileChooser* dialog_;

    Menu* menubar();
    Menu* make_menu(Menu*, CommandInfo*, int = 0);
    MenuItem* make_submenu(MenuItem*, Menu*);
    MenuItem* make_item(MenuItem*, Action*);
    void add(const char* label, Glyph*);
};

App::App() {
    main_ = nil;
    dialog_ = nil;
}

App::~App() {
    Resource::unref(dialog_);
}

declareActionCallback(App)
implementActionCallback(App)

struct CommandInfo {
    const char* str;
    ActionMemberFunction(App) func;
    CommandInfo* submenu;
    int options;
};

CommandInfo filemenu[] = {
    { "File Chooser ...", &App::open },
    { "Save", &App::save },
    { "", nil },
    { "Quit", &App::quit },
    { nil }
};

CommandInfo editmenu[] = {
    { "Cut", &App::cut },
    { "Copy", &App::copy },
    { "Paste", &App::paste },
    { nil }
};

CommandInfo times_menu[] = {
    { "Normal", &App::do_nothing },
    { "Bold", &App::do_nothing },
    { "Italic", &App::do_nothing },
    { nil }
};

CommandInfo helv_menu[] = {
    { "Normal", &App::do_nothing },
    { "Bold", &App::do_nothing },
    { "Oblique", &App::do_nothing },
    { nil }
};

CommandInfo fontmenu[] = {
    { "Times", nil, times_menu, 2 },
    { "Helvetica", nil, helv_menu, 2 },
    { nil }
};

CommandInfo colormenu[] = {
    { "Black", &App::black },
    { "Red", &App::red },
    { "Green", &App::green },
    { "Blue", &App::blue },
    { nil }
};

class BoundedValue : public Adjustable {
protected:
    BoundedValue();
public:
    BoundedValue(Coord lower, Coord upper);
    virtual ~BoundedValue();

    virtual void lower_bound(Coord);
    virtual void upper_bound(Coord);
    virtual void current_value(Coord);
    virtual void scroll_incr(Coord);
    virtual void page_incr(Coord);

    virtual Coord lower(DimensionName) const;
    virtual Coord upper(DimensionName) const;
    virtual Coord length(DimensionName) const;
    virtual Coord cur_lower(DimensionName) const;
    virtual Coord cur_upper(DimensionName) const;
    virtual Coord cur_length(DimensionName) const;

    virtual void scroll_to(DimensionName, Coord position);
    virtual void scroll_forward(DimensionName);
    virtual void scroll_backward(DimensionName);
    virtual void page_forward(DimensionName);
    virtual void page_backward(DimensionName);
private:
    Coord curvalue_;
    Coord lower_;
    Coord span_;
    Coord scroll_incr_;
    Coord page_incr_;
};

class Valuator : public MonoGlyph, public Observer {
public:
    Valuator(BoundedValue*, Style*);
    virtual ~Valuator();

    virtual InputHandler* focusable() const;

    virtual void update(Observable*);
    virtual void disconnect(Observable*);
private:
    BoundedValue* bvalue_;
    FieldEditor* editor_;

    void accept_editor(FieldEditor*);
    void cancel_editor(FieldEditor*);
};

static OptionDesc options[] = {
    { "-titlefg", "Idemo*title*foreground", OptionValueNext },
    { "-chiseled", "Idemo*labelStyle", OptionValueImplicit, "chiseled" },
    { "-flattext", "Idemo*labelStyle", OptionValueImplicit, "normal" },
    { "-raised", "Idemo*labelStyle", OptionValueImplicit, "raised" },
    { "-rot", "Idemo*rotation", OptionValueNext },
    { "-rotate", "Idemo*rotation", OptionValueNext },
    { nil }
};

int main(int argc, char** argv) {
    App* a = new App;
    return a->run(argc, argv);
}

int App::run(int argc, char** argv) {
    Session* session = new Session("Idemo", argc, argv, options);
    kit_ = WidgetKit::instance();
    layout_ = LayoutKit::instance();
    WidgetKit& kit = *kit_;
    const LayoutKit& layout = *layout_;
    style_ = kit_->style();
    title_style_ = new Style(style_);
    Resource::ref(title_style_);
    title_style_->alias("title");
    const Font* f = kit_->font();
    size_ = tool_scale * f->width('m');
    body_ = new LRComposition(
	layout.vbox(), new TeXCompositor(10), nil,
	size_ + 1.0 + size_ + 1.0 + size_ + 2.0
    );
    Glyph* space = layout.spaces(2, 0.5, f, kit_->foreground());
    Glyph* begin_par = layout.vstrut(0, 0, 0, fil, 0);
    Glyph* end_par = layout.strut(f, 0, fil, 0);
    Glyph* begin_line = layout.vstrut(0, 0, 0, fil, 0);
    Glyph* end_line = layout.strut(f, 0, fil, 0);
    Glyph* vfil = layout.vglue();
    Glyph* vspace1 = layout.vspace(1.0);
    Glyph* vspace2 = layout.vspace(2.0);
    Glyph* vspace4 = layout.vspace(4.0);
    sep_ = layout.discretionary(
	0,
	space,
	end_line,
	layout.discretionary(0, vfil, vfil, nil, nil),
	begin_line
    );
    sep_ = layout.vcenter(sep_, 1.0);
    Style* s = style_;

    Transformer t;
    float rotation;
    s->find_attribute("rotation", rotation);
    t.rotate(rotation);
    Action* action = new ActionCallback(App)(this, &App::do_nothing);
    add(
	"Labels",
	layout.vbox(
	    layout.center(kit.label("Normal text"), 0.5, 1.0),
	    vspace2,
	    layout.hcenter(
		new TransformSetter(kit.label("Rotated text"), t)
	    )
	)
    );
    body_->append(sep_);

    Button* disabled = kit.push_button("Disabled", nil);
    disabled->state()->set(TelltaleState::is_enabled, false);
    add(
	"Push buttons",
	layout.vbox(
	    layout.hcenter(kit.push_button("Do nothing", action)),
	    vspace2,
	    layout.hcenter(disabled),
	    vspace2,
	    layout.hcenter(kit.push_button("Quit", kit_->quit()))
	)
    );
    body_->append(sep_);

    Button* column_c = kit.check_box("Column C", nil);
    column_c->state()->set(TelltaleState::is_chosen, true);
    column_c->state()->set(TelltaleState::is_enabled, false);
    add(
	"Check boxes",
	layout.vbox(
	    kit.check_box("Column A", action),
	    vspace2,
	    kit.check_box("Column B", nil),
	    vspace2,
	    column_c
	)
    );
    body_->append(sep_);

    TelltaleGroup* group = new TelltaleGroup;
    add(
	"Radio buttons",
	layout.vbox(
	    kit.radio_button(group, "Able", action),
	    vspace4,
	    kit_->radio_button(group, "Baker", nil),
	    vspace4,
	    kit_->radio_button(group, "Charlie", nil)
	)
    );
    body_->append(sep_);

    add(
	"Palette buttons",
	layout.vbox(
	    layout.vcenter(kit.palette_button("One", nil), 1.0),
	    vspace2,
	    kit.palette_button("Two", nil),
	    vspace2,
	    kit.palette_button("Three", nil)
	)
    );
    body_->append(sep_);

    BoundedValue* bv1 = new BoundedValue(0.0, 100.0);
    bv1->current_value(50.0);
    bv1->scroll_incr(5.0);
    bv1->page_incr(20.0);
    BoundedValue* bv2 = new BoundedValue(0.0, 100.0);
    bv2->current_value(50.0);
    bv2->scroll_incr(5.0);
    bv2->page_incr(10.0);
    Valuator* v1 = new Valuator(bv1, s);
    Valuator* v2 = new Valuator(bv2, s);
    InputHandler* vgroup = new InputHandler(
	layout.vbox(v1, vspace4, v2, layout.vspace(15.0)),
	s
    );
    vgroup->append_input_handler(v1->focusable());
    vgroup->append_input_handler(v2->focusable());
    add("Field editors", vgroup);
    body_->append(sep_);

    add(
	"Panner",
	layout.fixed_span(kit.panner(bv1, bv2), size_ - 10.0, size_ - 10.0)
    );
    body_->append(sep_);

    add(
	"HScrollBar",
	layout.vcenter(layout.h_fixed_span(kit.hscroll_bar(bv1), size_ - 8.0))
    );
    body_->append(sep_);

    add(
	"VScrollBar",
	layout.hcenter(layout.v_fixed_span(kit.vscroll_bar(bv2), size_ - 8.0))
    );

    body_->append(
	layout.vcenter(
	    layout.discretionary(
		PenaltyGood,
		end_par,
		end_par,
		layout.discretionary(0, vfil, vfil, nil, nil),
		begin_par
	    ),
	    1.0
	)
    );

    body_->repair();

    main_ = new ApplicationWindow(
	new Background(
	    layout.vcenter(
		layout.vbox(
		    layout.vcenter(menubar(), 1.0),
		    body_
		)
	    ),
	    kit.background()
	)
    );
    return session->run_window(main_);
}

declareFileChooserCallback(App)
implementFileChooserCallback(App)

void App::open() {
    if (dialog_ == nil) {
	dialog_ = DialogKit::instance()->file_chooser(".", style_);
	Resource::ref(dialog_);
    }
    if (dialog_->post_for(main_)) {
	const String* name = dialog_->selected();
	if (name != nil) {
	    printf("%.*s\n", name->length(), name->string());
	    fflush(stdout);
	}
    }
}

void App::save() { printf("save\n"); }

void App::cut() { printf("cut\n"); }
void App::copy() { printf("copy\n"); }
void App::paste() { printf("paste\n"); }
void App::black() { printf("black\n"); }
void App::red() { printf("red\n"); }
void App::green() { printf("green\n"); }
void App::blue() { printf("blue\n"); }

void App::do_nothing() { printf("do nothing\n"); }

void App::quit() {
    delete main_;
    kit_->quit()->execute();
}

Menu* App::menubar() {
    WidgetKit& kit = *kit_;
    Menu* m = kit.menubar();
    Menu* file = make_menu(kit.pulldown(), filemenu);
    m->append_item(make_submenu(kit.menubar_item("Dialog"), file));
    m->append_item(
	make_submenu(
	    kit.menubar_item("Edit"), make_menu(kit.pulldown(), editmenu, 1)
	)
    );
    m->append_item(
	make_submenu(
	    kit.menubar_item("Font"), make_menu(kit.pulldown(), fontmenu, 2)
	)
    );

    /* disable save */
    file->item(1)->state()->set(TelltaleState::is_enabled, false);

    return m;
}

Menu* App::make_menu(Menu* m, CommandInfo* info, int options) {
    WidgetKit& kit = *kit_;
    TelltaleGroup* group = nil;
    for (CommandInfo* i = info; i->str != nil; i++) {
        if (i->str[0] == '\0') {
            m->append_item(kit.menu_item_separator());
        } else {
	    Glyph* g = layout_->r_margin(kit.label(i->str), 0.0, fil, 0.0);
	    MenuItem* item;
	    switch (options) {
	    case 1:
		item = kit.check_menu_item(g);
		break;
	    case 2:
		if (group == nil) {
		    group = new TelltaleGroup;
		}
		item = kit.radio_menu_item(group, g);
		break;
	    default:
		item = kit.menu_item(g);
		break;
	    }
	    if (i->func == nil) {
		if (i->submenu != nil) {
		    make_submenu(
			item,
			make_menu(kit.pullright(), i->submenu, i->options)
		    );
		}
	    } else {
		make_item(item, new ActionCallback(App)(this, i->func));
	    }
	    m->append_item(item);
        }
    }
    return m;
}

MenuItem* App::make_submenu(MenuItem* i, Menu* s) {
    i->menu(s);
    return i;
}

MenuItem* App::make_item(MenuItem* i, Action* a) {
    i->action(a);
    return i;
}

void App::add(const char* title, Glyph* glyph) {
    const LayoutKit& layout = *layout_;
    Glyph* g = layout.margin(
	glyph,
	0.0, fil, 0.0, 0.0, fil, 0.0,
	0.0, 0.25*fil, 0.0, 0.0, 0.75*fil, 0.0
    );
    if (title != nil) {
	WidgetKit& kit = *kit_;
	kit.push_style();
	kit.style(title_style_);
	g = layout.overlay(
	    layout.center(g),
	    layout.center(
		layout.margin(
		    kit.fancy_label(title),
		    0.0, fil, 0.0, 0.0, fil, 0.0,
		    0.0, 0.90*fil, 0.0, 0.0, 0.10*fil, 0.0
		)
	    )
	);
	kit.pop_style();
    }
    body_->append(
	layout.center(layout.fixed_span(g, size_, size_), 0.0, 1.0)
    );
}

/* class BoundedValue */

BoundedValue::BoundedValue() {
    scroll_incr_ = 0.0;
    page_incr_ = 0.0;
}

BoundedValue::BoundedValue(Coord lower, Coord upper) {
    lower_ = lower;
    span_ = upper - lower;
    scroll_incr_ = span_ * 0.04;
    page_incr_ = span_ * 0.4;
    curvalue_ = (lower + upper) * 0.5;
}

BoundedValue::~BoundedValue() { }

void BoundedValue::lower_bound(Coord c) { lower_ = c; }
void BoundedValue::upper_bound(Coord c) { span_ = c - lower_; }

void BoundedValue::current_value(Coord value) {
    curvalue_ = value;
    constrain(Dimension_X, curvalue_);
    notify(Dimension_X);
    notify(Dimension_Y);
}

void BoundedValue::scroll_incr(Coord c) { scroll_incr_ = c; }
void BoundedValue::page_incr(Coord c) { page_incr_ = c; }

Coord BoundedValue::lower(DimensionName) const { return lower_; }
Coord BoundedValue::upper(DimensionName) const { return lower_ + span_; }
Coord BoundedValue::length(DimensionName) const { return span_; }
Coord BoundedValue::cur_lower(DimensionName) const { return curvalue_; }
Coord BoundedValue::cur_upper(DimensionName) const { return curvalue_; }
Coord BoundedValue::cur_length(DimensionName) const { return 0; }

void BoundedValue::scroll_to(DimensionName d, Coord position) {
    Coord p = position;
    constrain(d, p);
    if (p != curvalue_) {
	curvalue_ = p;
	notify(Dimension_X);
	notify(Dimension_Y);
    }
}

void BoundedValue::scroll_forward(DimensionName d) {
    scroll_to(d, curvalue_ + scroll_incr_);
}

void BoundedValue::scroll_backward(DimensionName d) {
    scroll_to(d, curvalue_ - scroll_incr_);
}

void BoundedValue::page_forward(DimensionName d) {
    scroll_to(d, curvalue_ + page_incr_);
}

void BoundedValue::page_backward(DimensionName d) {
    scroll_to(d, curvalue_ - page_incr_);
}

/* class Valuator */

declareFieldEditorCallback(Valuator)
implementFieldEditorCallback(Valuator)

Valuator::Valuator(BoundedValue* bv, Style* style) : MonoGlyph(nil) {
    Style* s = new Style(style);
    s->alias("Valuator");
    bvalue_ = bv;
    bv->attach(Dimension_X, this);
    editor_ = DialogKit::instance()->field_editor(
	"100.00", s,
	new FieldEditorCallback(Valuator)(
	    this, &Valuator::accept_editor, &Valuator::cancel_editor
	)
    );
    body(editor_);
    update(bv->observable(Dimension_X));
}

Valuator::~Valuator() {
    if (bvalue_ != nil) {
	bvalue_->detach(Dimension_X, this);
    }
}

InputHandler* Valuator::focusable() const {
    return editor_;
}

void Valuator::update(Observable*) {
    Coord v = bvalue_->cur_lower(Dimension_X);
    char buf[20];
    sprintf(buf, "%.2f", v);
    editor_->field(buf);
}

void Valuator::disconnect(Observable*) {
    bvalue_ = nil;
}

void Valuator::accept_editor(FieldEditor*) {
    Coord v;
    const String& value = *editor_->text();
    if (value.convert(v)) {
	bvalue_->current_value(v);
    }
}

void Valuator::cancel_editor(FieldEditor*) {
    update(bvalue_->observable(Dimension_X));
}
