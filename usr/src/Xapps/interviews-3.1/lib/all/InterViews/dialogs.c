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
 * DialogKit -- object for creating common dialog boxes
 */

#include <IV-look/dialogs.h>
#include <IV-look/kit.h>
#ifdef motif_kit
#include <IV-look/mf_dialogs.h>
#endif
#ifdef openlook_kit
#include <IV-look/ol_dialogs.h>
#endif
#include <InterViews/display.h>
#include <InterViews/event.h>
#include <InterViews/handler.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <OS/string.h>

class DialogKitImpl {
private:
    friend class DialogKit;

    static DialogKit* instance_;

    static DialogKit* make_kit();

    WidgetKit* kit_;
};

DialogKit* DialogKitImpl::instance_;

/*
 * We need to create a widget kit to make sure it is built
 * before trying to build any dialog objects, and thus avoiding
 * any circularities regarding style dependencies and the creation
 * of the initial style settings by the widget kit.
 */

DialogKit::DialogKit() {
    impl_ = new DialogKitImpl;
    impl_->kit_ = WidgetKit::instance();
}

DialogKit::~DialogKit() {
    delete impl_;
}

DialogKit* DialogKit::instance() {
    if (DialogKitImpl::instance_ == nil) {
	DialogKitImpl::instance_ = DialogKitImpl::make_kit();
    }
    return DialogKitImpl::instance_;
}

DialogKit* DialogKitImpl::make_kit() {
#ifdef openlook_kit
    String gui;
    if (Session::instance()->style()->find_attribute("gui", gui)) {
	if (gui == "OpenLook" || gui == "openlook") {
	    return new OLDialogKit;
	}
    }
#endif
    return new MFDialogKit;
}

WidgetKit* DialogKit::widget_kit() const {
    return impl_->kit_;
}

FieldEditor* DialogKit::field_editor(
    const char* sample, Style* style, FieldEditorAction* a
) const {
    return make_field_editor(String(sample), widget_kit(), style, a);
}

FieldEditor* DialogKit::field_editor(
    const String& sample, Style* style, FieldEditorAction* a
) const {
    return make_field_editor(sample, widget_kit(), style, a);
}

FieldEditor* DialogKit::make_field_editor(
    const String&, WidgetKit*, Style*, FieldEditorAction*
) const {
    /* DialogKit subclasses should define this */
    return nil;
}

FileChooser* DialogKit::file_chooser(
    const char* dir, Style* style, FileChooserAction* a
) const {
    return make_file_chooser(String(dir), widget_kit(), style, a);
}

FileChooser* DialogKit::file_chooser(
    const String& dir, Style* style, FileChooserAction* a
) const {
    return make_file_chooser(dir, widget_kit(), style, a);
}

FileChooser* DialogKit::make_file_chooser(
    const String&, WidgetKit*, Style*, FileChooserAction*
) const {
    /* DialogKit subclasses should define this */
    return nil;
}

/* class DialogHandler -- helper for class Dialog */

class DialogHandler : public Handler {
public:
    DialogHandler(Dialog*);
    virtual ~DialogHandler();

    virtual boolean event(Event&);
private:
    Dialog* dialog_;
};

DialogHandler::DialogHandler(Dialog* d) { dialog_ = d; }
DialogHandler::~DialogHandler() { }

boolean DialogHandler::event(Event&) {
    dialog_->dismiss(false);
    return true;
}

/* class Dialog */

Dialog::Dialog(Glyph* g, Style* s) : InputHandler(g, s) { }
Dialog::~Dialog() { }

boolean Dialog::post_for_aligned(Window* w, float x_align, float y_align) {
    TransientWindow* t = new TransientWindow(this);
    t->style(new Style(style()));
    t->transient_for(w);
    t->wm_delete(new DialogHandler(this));
    t->place(w->left() + 0.5 * w->width(), w->bottom() + 0.5 * w->height());
    t->align(x_align, y_align);
    t->map();
    boolean b = run();
    t->unmap();
    t->display()->sync();
    delete t;
    return b;
}

boolean Dialog::post_at_aligned(
    Coord x, Coord y, float x_align, float y_align
) {
    TransientWindow* t = new TransientWindow(this);
    t->style(new Style(style()));
    t->wm_delete(new DialogHandler(this));
    t->place(x, y);
    t->align(x_align, y_align);
    t->map();
    boolean b = run();
    t->unmap();
    t->display()->sync();
    delete t;
    return b;
}

boolean Dialog::run() {
    Session* s = Session::instance();
    Event e;
    done_ = false;
    for (;;) {
	s->read(e);
	if (e.grabber() != nil || inside(e)) {
	    e.handle();
	} else if (e.type() == Event::key) {
	    keystroke(e);
	}
	if (done_) {
	    break;
	}
	if (s->done()) {
	    accepted_ = false;
	    break;
	}
    }
    return accepted_;
}

void Dialog::dismiss(boolean accept) {
    accepted_ = accept;
    done_ = true;
}
