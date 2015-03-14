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
 * FieldEditor -- simple editor for text fields
 */

#include <Dispatch/dispatcher.h>
#include <Dispatch/iocallback.h>
#include <IV-look/field.h>
#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/canvas.h>
#include <InterViews/display.h>
#include <InterViews/font.h>
#include <InterViews/event.h>
#include <InterViews/hit.h>
#include <InterViews/layout.h>
#include <InterViews/printer.h>
#include <InterViews/selection.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <IV-2_6/InterViews/button.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/sensor.h>
#include <IV-2_6/InterViews/streditor.h>
#include <IV-2_6/InterViews/textdisplay.h>
#include <OS/math.h>
#include <OS/string.h>

class FieldStringEditor : public StringEditor {
public:
    FieldStringEditor(
	ButtonState* bs, const char* sample, WidgetKit*, Style*
    );
    virtual ~FieldStringEditor();

    virtual void print(Printer*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);

    void press(const Event&);
    boolean keystroke(const Event&);
    void cursor_on();
    void cursor_off();
    void focus_in();
    void focus_out();
    void cut(SelectionManager*);
    void paste(SelectionManager*);
protected:
    virtual void Reconfig();
private:
    WidgetKit* kit_;
    Style* style_;
    int start_;
    int index_;

    void do_select(Event&);
    void do_grab_scroll(Event&);
    void do_rate_scroll(Event&);
};

declareSelectionCallback(FieldStringEditor)
implementSelectionCallback(FieldStringEditor)

FieldStringEditor::FieldStringEditor(
    ButtonState* bs, const char* sample, WidgetKit* kit, Style* style
) : StringEditor(bs, sample) {
    kit_ = kit;
    style_ = style;
    Resource::ref(style);
    delete input;
    input = nil;
}

FieldStringEditor::~FieldStringEditor() {
    Resource::unref(style_);
}

void FieldStringEditor::print(Printer* p, const Allocation& a) const {
    const Font* f = output->GetFont();
    const Color* fg = output->GetFgColor();
    FontBoundingBox b;
    f->font_bbox(b);
    Coord x = a.left(), y = a.bottom() + b.font_descent();
    FieldStringEditor* e = (FieldStringEditor*)this;
    for (const char* s = e->Text(); *s != '\0'; s++) {
	Coord w = f->width(*s);
	p->character(f, *s, w, fg, x, y);
	x += w;
    }
}

void FieldStringEditor::pick(
    Canvas*, const Allocation& a, int depth, Hit& h
) {
    const Event* ep = h.event();
    if (ep != nil && h.left() < a.right() && h.right() >= a.left() &&
	h.bottom() < a.top() && h.top() >= a.bottom()
    ) {
	h.target(depth, this, 0);
    }
}

void FieldStringEditor::press(const Event& event) {
    Event e;
    display->Draw(output, canvas);
    switch (event.pointer_button()) {
    case Event::left:
	do_select(e);
	break;
    case Event::middle:
	do_grab_scroll(e);
	break;
    case Event::right:
	do_rate_scroll(e);
	break;
    }
}

void FieldStringEditor::do_select(Event& e) {
    int origin = display->Left(0, 0);
    int width = display->Width();
    Poll(e);
    start_ = display->LineIndex(0, e.x);
    do {
	if (e.x < 0) {
	    origin = Math::min(0, origin - e.x);
	} else if (e.x > xmax) {
	    origin = Math::max(
		xmax - width, origin - (e.x - xmax)
	    );
	}
	display->Scroll(0, origin, ymax);
	index_ = display->LineIndex(0, e.x);
	DoSelect(start_, index_);
	Poll(e);
    } while (e.leftmouse);
    SelectionManager* s = e.display()->primary_selection();
    s->own(
	new SelectionCallback(FieldStringEditor)(this, &FieldStringEditor::cut)
    );
}

void FieldStringEditor::do_grab_scroll(Event& e) {
    Window* w = canvas->window();
    Cursor* c = w->cursor();
    w->cursor(kit_->hand_cursor());
    int origin = display->Left(0, 0);
    int width = display->Width();
    Poll(e);
    int x = e.x;
    do {
	origin += e.x - x;
	origin = Math::min(
	    0, Math::max(Math::min(0, xmax - width), origin)
	);
	display->Scroll(0, origin, ymax);
	x = e.x;
	Poll(e);
    } while (e.middlemouse);
    w->cursor(c);
}

void FieldStringEditor::do_rate_scroll(Event& e) {
    Window* w = canvas->window();
    Cursor* c = w->cursor();
    WidgetKit& kit = *kit_;
    Cursor* left = kit.lfast_cursor();
    Cursor* right = kit.rfast_cursor();
    int origin = display->Left(0, 0);
    int width = display->Width();
    Poll(e);
    int x = e.x;
    do {
	origin += x - e.x;
	origin = Math::min(
	    0, Math::max(Math::min(0, xmax - width), origin)
	);
	display->Scroll(0, origin, ymax);
	if (e.x - x < 0) {
	    w->cursor(left);
	} else {
	    w->cursor(right);
	}
	Poll(e);
    } while (e.rightmouse);
    w->cursor(c);
}

boolean FieldStringEditor::keystroke(const Event& e) {
    char c;
    return e.mapkey(&c, 1) != 0 && HandleChar(c) && c == '\t';
}

void FieldStringEditor::cursor_on() {
    if (canvas != nil) {
	display->CaretStyle(BarCaret);
    }
}

void FieldStringEditor::cursor_off() {
    if (canvas != nil) {
	display->CaretStyle(NoCaret);
    }
}

void FieldStringEditor::focus_in() { }
void FieldStringEditor::focus_out() { }

void FieldStringEditor::cut(SelectionManager* s) {
    s->put_value(Text() + start_, index_ - start_);
}

void FieldStringEditor::paste(SelectionManager*) {
    /* unimplemented */
}

void FieldStringEditor::Reconfig() {
    kit_->push_style();
    kit_->style(style_);
    Painter* p = new Painter(output);
    p->SetColors(kit_->foreground(), kit_->background());
    p->SetFont(kit_->font());
    Resource::unref(output);
    output = p;
    StringEditor::Reconfig();
    kit_->pop_style();
}

class FieldButton : public ButtonState {
public:
    FieldButton(FieldEditor*, FieldEditorAction*);
    virtual ~FieldButton();

    virtual void Notify();
private:
    FieldEditor* editor_;
    FieldEditorAction* action_;
};

class FieldEditorImpl {
private:
    friend class FieldEditor;

    WidgetKit* kit_;
    FieldStringEditor* editor_;
    FieldButton* bs_;
    String text_;
    boolean cursor_is_on_;
    IOHandler* blink_handler_;
    long flash_rate_;

    void build(FieldEditor*, const char*, FieldEditorAction*);
    void blink_cursor(long, long);
    void stop_blinking();
};

declareIOCallback(FieldEditorImpl)
implementIOCallback(FieldEditorImpl)

FieldEditor::FieldEditor(
    const String& sample, WidgetKit* kit, Style* s, FieldEditorAction* action
) : InputHandler(nil, s) {
    impl_ = new FieldEditorImpl;
    impl_->kit_ = kit;
    NullTerminatedString ns(sample);
    impl_->build(this, ns.string(), action);
}

FieldEditor::~FieldEditor() {
    FieldEditorImpl* i = impl_;
    i->stop_blinking();
    Resource::unref(i->editor_);
    Resource::unref(i->bs_);
    delete i->blink_handler_;
    delete i;
}

void FieldEditor::undraw() {
    FieldEditorImpl& f = *impl_;
    f.stop_blinking();
    InputHandler::undraw();
}

void FieldEditor::press(const Event& e) {
    impl_->editor_->press(e);
}

void FieldEditor::drag(const Event&) { }
void FieldEditor::release(const Event&) { }

void FieldEditor::keystroke(const Event& e) {
    FieldEditorImpl& f = *impl_;
    if (f.editor_->keystroke(e)) {
	select(text()->length());
	next_focus();
    }
}

InputHandler* FieldEditor::focus_in() {
    FieldEditorImpl& f = *impl_;
    f.blink_cursor(0, 0);
    f.editor_->focus_in();
    return InputHandler::focus_in();
}

void FieldEditor::focus_out() {
    FieldEditorImpl& f = *impl_;
    f.stop_blinking();
    f.editor_->cursor_off();
    f.editor_->focus_out();
    InputHandler::focus_out();
}

void FieldEditor::field(const char* str) {
    impl_->editor_->Message(str);
}

void FieldEditor::field(const String& s) {
    NullTerminatedString ns(s);
    impl_->editor_->Message(ns.string());
}

void FieldEditor::select(int pos) {
    impl_->editor_->Select(pos);
}

void FieldEditor::select(int l, int r) {
    impl_->editor_->Select(l, r);
}

void FieldEditor::edit() {
    impl_->editor_->Edit();
}

void FieldEditor::edit(const char* str, int left, int right) {
    impl_->editor_->Edit(str, left, right);
}

void FieldEditor::edit(const String& str, int left, int right) {
    NullTerminatedString ns(str);
    impl_->editor_->Edit(ns.string(), left, right);
}

const String* FieldEditor::text() const {
    impl_->text_ = String(impl_->editor_->Text());
    return &impl_->text_;
}

/** class FieldEditorImpl **/

void FieldEditorImpl::build(
    FieldEditor* e, const char* str, FieldEditorAction* a
) {
    WidgetKit& kit = *kit_;
    kit.begin_style("FieldEditor");
    Style* s  = kit.style();
    bs_ = new FieldButton(e, a);
    editor_ = new FieldStringEditor(bs_, str, kit_, s);
    Glyph* g = editor_;
    if (s->value_is_on("beveled")) {
	g = kit.inset_frame(
	    new Background(
		LayoutKit::instance()->h_margin(editor_, 2.0),
		kit.background()
	    )
	);
    }
    e->body(g);
    cursor_is_on_ = false;
    blink_handler_ = new IOCallback(FieldEditorImpl)(
	this, &FieldEditorImpl::blink_cursor
    );
    float sec = 0.5;
    s->find_attribute("cursorFlashRate", sec);
    flash_rate_ = long(sec * 1000000);
    kit.end_style();
}

void FieldEditorImpl::blink_cursor(long, long) {
    if (cursor_is_on_) {
	editor_->cursor_off();
	cursor_is_on_ = false;
    } else {
	editor_->cursor_on();
	cursor_is_on_ = true;
    }
    if (flash_rate_ > 10) {
	Dispatcher::instance().startTimer(0, flash_rate_, blink_handler_);
    }
}

void FieldEditorImpl::stop_blinking() {
    Dispatcher::instance().stopTimer(blink_handler_);
    editor_->cursor_off();
    cursor_is_on_ = false;
}

/** class FieldButton **/

FieldButton::FieldButton(FieldEditor* editor, FieldEditorAction* action) {
    editor_ = editor;
    action_ = action;
    Resource::ref(action_);
}

FieldButton::~FieldButton() {
    Resource::unref(action_);
}

/*
 * We need to reset the button state's value so that we'll get
 * notified again.  If we call SetValue, then it will call
 * Notify again!  Alas, we must access the protected value member (sigh).
 */

void FieldButton::Notify() {
    int v;
    GetValue(v);
    value = 0;
    if (action_ != nil) {
	switch (v) {
	case '\r':
	    action_->accept(editor_);
	    break;
	case /*  ^G */ '\007':
	case /* Esc */ '\033':
	    action_->cancel(editor_);
	    break;
	default:
	    break;
	}
    }
}

/** class FieldEditorAction **/

FieldEditorAction::FieldEditorAction() { }
FieldEditorAction::~FieldEditorAction() { }
void FieldEditorAction::accept(FieldEditor*) { }
void FieldEditorAction::cancel(FieldEditor*) { }
