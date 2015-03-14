/*
 * Copyright (c) 1987-1992 Stanford University
 * Copyright (c) 1991-1992 Silicon Graphics, Inc.
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

#include "dialogs.h"
#include <IV-look/dialogs.h>
#include <IV-look/kit.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <OS/string.h>
#include <string.h>

Chooser::Chooser(const char* name, const char* prompt) {
    Style* s = new Style(Session::instance()->style());
    s->attribute("name", name);
    s->attribute("subcaption", prompt);
    chooser_ = DialogKit::instance()->file_chooser(".", s, nil);
    Resource::ref(chooser_);
    style_ = s;
    Resource::ref(style_);
}

Chooser::~Chooser() {
    Resource::unref(chooser_);
    Resource::unref(style_);
}

const char* Chooser::post(Window* window, const char* ext) {
    if (ext != nil) {
        static char filter[100];
        strcpy(filter, "*.");
        strcat(filter, ext);
	String v;
	if (!style_->find_attribute("filterPattern", v) || v != filter) {
	    style_->attribute("filterPattern", filter);
	    chooser_->reread();
	}
    }
    if (chooser_->post_for(window)) {
	const String* s = chooser_->selected();
	if (s != nil) {
	    return s->string();
	}
    }
    return nil;
}

declareActionCallback(Asker)
implementActionCallback(Asker)

#define AskerCallback(f) ActionCallback(Asker)(this, &Asker::f)

declareFieldEditorCallback(Asker)
implementFieldEditorCallback(Asker)

Asker::Asker(const char* name, const char* prompt) {
    WidgetKit& kit = *WidgetKit::instance();
    const LayoutKit& layout = *LayoutKit::instance();
    Style* s = new Style(kit.style());
    s->attribute("name", name);
    editor_ = DialogKit::instance()->field_editor(
	"", s, new FieldEditorCallback(Asker)(
	    this, &Asker::accept_editor, &Asker::cancel_editor
	)
    );
    Glyph* sep = layout.vglue(11, fil, 11);
    dialog_ = new Dialog(
	kit.inset_frame(
	    layout.margin(
		layout.vbox(
		    layout.hbox(kit.label(prompt), layout.hglue()),
		    sep,
		    editor_,
		    sep,
		    layout.hbox(
			layout.hglue(100, fil, 100),
			kit.default_button("OK", new AskerCallback(accept)),
			layout.hglue(9, 0, 9),
			kit.push_button("Cancel", new AskerCallback(cancel))
		    )
		),
		12, 12
	    )
        ),
	s
    );
    dialog_->append_input_handler(editor_);
    dialog_->next_focus();
    Resource::ref(dialog_);
}

Asker::~Asker() {
    Resource::unref(dialog_);
}

const char* Asker::post(Window* window, const char* initial) {
    if (initial != nil) {
	editor_->field(initial);
    }
    if (dialog_->post_for(window)) {
	const String* s = editor_->text();
	if (s != nil) {
	    return s->string();
	}
    }
    return nil;
}

void Asker::accept_editor(FieldEditor*) {
    dialog_->dismiss(true);
}

void Asker::cancel_editor(FieldEditor*) {
    dialog_->dismiss(false);
}

void Asker::accept() { dialog_->dismiss(true); }
void Asker::cancel() { dialog_->dismiss(false); }

declareActionCallback(Confirmer)
implementActionCallback(Confirmer)

#define ConfirmerCallback(f) ActionCallback(Confirmer)(this, &Confirmer::f)

Confirmer::Confirmer(const char* name, const char* prompt) {
    WidgetKit& kit = *WidgetKit::instance();
    const LayoutKit& layout = *LayoutKit::instance();
    Style* s = new Style(kit.style());
    s->attribute("name", name);
    Glyph* sep = layout.hspace(8);
    dialog_ = new Dialog(
	kit.outset_frame(
	    layout.margin(
		layout.vbox(
		    layout.hbox(kit.label(prompt), layout.hglue()),
		    layout.vglue(16, 20, 8),
		    layout.hbox(
			layout.hglue(30, fil, 0),
			kit.push_button(
			    "Cancel", new ConfirmerCallback(cancel)
			),
			sep,
			kit.push_button(
			    "No", new ConfirmerCallback(no)
			),
			sep,
			kit.push_button(
			    "Yes", new ConfirmerCallback(yes)
			)
		    )
		),
		12, 12
	    )
	),
	s
    );
    Resource::ref(dialog_);
}

Confirmer::~Confirmer() {
    Resource::unref(dialog_);
}

int Confirmer::post(Window* window) {
    if (dialog_->post_for(window)) {
	return confirm_ ? 1 : 2;
    }
    return 3;
}

void Confirmer::yes() { confirm_ = true; dialog_->dismiss(true); }
void Confirmer::no() { confirm_ = false; dialog_->dismiss(true); }
void Confirmer::cancel() { dialog_->dismiss(false); }

declareActionCallback(Reporter)
implementActionCallback(Reporter)

#define ReporterCallback(f) ActionCallback(Reporter)(this, &Reporter::f)

Reporter::Reporter(const char* name, const char* prompt) {
    WidgetKit& kit = *WidgetKit::instance();
    const LayoutKit& layout = *LayoutKit::instance();
    Style* s = new Style(kit.style());
    s->attribute("name", name);
    dialog_ = new Dialog(
	kit.outset_frame(
	    layout.margin(
		layout.vbox(
		    layout.hbox(kit.label(prompt), layout.hglue()),
		    layout.vglue(16, 20, 8),
		    layout.hbox(
			layout.hglue(36, fil, 0),
			kit.push_button("OK", new ReporterCallback(ok))
		    )
		),
		12, 12
	    )
	),
	s
    );
    Resource::ref(dialog_);
}

Reporter::~Reporter() {
    Resource::unref(dialog_);
}

void Reporter::post(Window* window) {
    dialog_->post_for(window);
}

void Reporter::ok() { dialog_->dismiss(true); }
