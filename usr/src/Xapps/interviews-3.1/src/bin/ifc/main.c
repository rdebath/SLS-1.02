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

/*
 * File chooser main program.
 */

#include <IV-look/dialogs.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <OS/string.h>
#include <stdio.h>
#include <stdlib.h>

static PropertyData props[] = {
    { "*font", "*times-medium-r-normal-*-140-*" },
    { "Ifc*iconName", "Ifc" },
    { "Ifc*title", "InterViews file chooser" },
    { "Ifc*rows", "12" },
    { "Ifc*separator", "\n" },
    { "Ifc*static", "false" },
    { nil }
};

static OptionDesc options[] = {
    { "-caption", "*caption", OptionValueNext },
    { "-nullsep", "*separator", OptionValueImplicit, "" },
    { "-open", "*open", OptionValueNext },
    { "-rows", "*rows", OptionValueNext },
    { "-separator", "*separator", OptionValueNext },
    { "-static", "*static", OptionValueImplicit, "true" },
    { "-subcaption", "*subcaption", OptionValueNext },
    { nil }
};

int main(int argc, char** argv) {
    Session* session = new Session("Ifc", argc, argv, options, props);
    Style* s = session->style();
    String sep;
    s->find_attribute("separator", sep);
    DialogKit& dialogs = *DialogKit::instance();
    FileChooser* fc = dialogs.file_chooser(((argc == 2) ? argv[1] : "."), s);
    ApplicationWindow* w = new ApplicationWindow(fc);
    w->style(s);
    w->map();
    int status = 1;
    for (;;) {
	if (!fc->run()) {
	    status = 1;
	    break;
	}
	const String* name = fc->selected();
	if (name != nil) {
	    if (sep == "") {
		printf("%.*s%c", name->length(), name->string(), '\0');
	    } else {
		printf(
		    "%.*s%.*s", name->length(), name->string(),
		    sep.length(), sep.string()
		);
	    }
	    fflush(stdout);
	    status = 0;
	}
	if (!s->value_is_on("static")) {
	    break;
	}
    }
    w->unmap();
    delete w;
    delete session;
    return status;
}
