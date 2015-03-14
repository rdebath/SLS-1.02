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
 *  alert - displays a message in a dialog box
 */

#include <Dispatch/dispatcher.h>
#include <Dispatch/iohandler.h>
#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/display.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <OS/string.h>
#include <stdio.h>

static PropertyData props[] = {
    { "Alert*font", "9x15" },
    { "Alert*delay", "12" },
    { "Alert*quitbutton", "OK, OK ..." },
    { "Alert*transient", "on" },
    { nil }
};

static OptionDesc options[] = {
    { "font=", "*font", OptionValueAfter },
    { "button=", "*quitbutton", OptionValueAfter },
    { "-delay", "*delay", OptionValueNext },
    { "-top", "*transient", OptionValueImplicit, "off" },
    { nil }
};

static Glyph* make_message(Glyph*, const WidgetKit&, const LayoutKit&);

class Timeout : public IOHandler {
public:
    Timeout(Session*);
    virtual ~Timeout();

    virtual void timerExpired(long sec, long usec);
private:
    Session* session_;
};

Timeout::Timeout(Session* s) { session_ = s; }
Timeout::~Timeout() { }

void Timeout::timerExpired(long, long) {
    session_->quit();
}

int main(int argc, char** argv) {
    Session* session = new Session("Alert", argc, argv, options, props);
    const WidgetKit& kit = *WidgetKit::instance();
    Style* style = kit.style();
    const LayoutKit& layout = *LayoutKit::instance();
    String button_label;
    style->find_attribute("quitbutton", button_label);
    float delay;
    style->find_attribute("delay", delay);
    /* convert from hours to seconds */
    delay *= 60*60;
    long delay_sec = long(delay);
    long delay_usec = long((delay - float(delay_sec)) * 1000000);

    Glyph* vspace = layout.vglue(18.0, fil, 16.0);
    Glyph* hspace = layout.hglue(36.0, fil, 34.0);
    Glyph* dialog = kit.outset_frame(
	layout.vbox(
	    vspace,
	    layout.hbox(
		hspace,
		layout.vbox(
		    make_message(layout.vbox(), kit, layout),
		    vspace,
		    layout.l_margin(
			kit.push_button(button_label, kit.quit()),
			0.0, fil, 0.0
		    )
		),
		hspace
	    ),
	    vspace
	)
    );

    Window* w;
    if (style->value_is_on("transient")) {
	TransientWindow* t = new TransientWindow(dialog);
	t->transient_for(t);
	w = t;
    } else {
	w = new ApplicationWindow(dialog);
    }
    Display* d = session->default_display();
    w->place(d->width() / 2, d->height() / 2);
    w->align(0.5, 0.5);
    d->ring_bell(0);
    Dispatcher::instance().startTimer(
	delay_sec, delay_usec, new Timeout(session)
    );
    session->run_window(w);
    delete w;
    delete session;
    return 0;
}

static Glyph* make_message(
    Glyph* container, const WidgetKit& kit, const LayoutKit& layout
) {
    char buffer[1024];
    /* guarantee null-termination */
    buffer[sizeof(buffer) - 1] = '\0';

    while (fgets(buffer, sizeof(buffer) - 1, stdin) != nil) {
	String s(buffer);
	int n = s.length();
	if (s[n - 1] == '\n') {
	    s.set_to_left(n - 1);
	}
	container->append(
	    layout.r_margin(kit.label(s), 0.0, fil, 0.0)
	);
    }
    return container;
}
