/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
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
 * Session - coordinate control flow and display management
 */

#include <Dispatch/dispatcher.h>
#include <Dispatch/iohandler.h>
#include <InterViews/cursor.h>
#include <InterViews/display.h>
#include <InterViews/event.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <OS/host.h>
#include <OS/file.h>
#include <OS/list.h>
#include <OS/string.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef sgi
#include <malloc.h>
#endif

/*
 * These attributes must be defined somewhere.
 */

static PropertyData defpropvalues[] = {
    { "*background", "#ffffff" },
    { "*foreground", "#000000" },
    { "*font", "fixed" },
    { nil }
};

/*
 * Predefined command-line options.
 */

static OptionDesc defoptions[] = {
    { "-background", "*background", OptionValueNext },
    { "-bg", "*background", OptionValueNext },
    { "-dbuf", "*double_buffered", OptionValueImplicit, "on" },
    { "-display", "*display", OptionValueNext },
    { "-dpi", "*dpi", OptionValueNext },
    { "-fg", "*foreground", OptionValueNext },
    { "-flat", "*flat", OptionValueNext },
    { "-fn", "*font", OptionValueNext },
    { "-font", "*font", OptionValueNext },
    { "-foreground", "*foreground", OptionValueNext },
    { "-geometry", "*geometry", OptionValueNext },
    { "-iconic", "*iconic", OptionValueImplicit, "on" },
    { "-monochrome", "*gui", OptionValueImplicit, "monochrome" },
    { "-motif", "*gui", OptionValueImplicit, "Motif" },
    { "-name", "*name", OptionValueNext },
    { "-nodbuf", "*double_buffered", OptionValueImplicit, "off" },
    { "-noshape", "*shaped_windows", OptionValueImplicit, "off" },
    { "-openlook", "*gui", OptionValueImplicit, "OpenLook" },
    { "-reverse", "*reverseVideo", OptionValueImplicit, "on" },
    { "-rv", "*reverseVideo", OptionValueImplicit, "on" },
    { "-shape", "*shaped_windows", OptionValueImplicit, "on" },
    { "-smotif", "*gui", OptionValueImplicit, "SGIMotif" },
    { "-synchronous", "*synchronous", OptionValueImplicit, "on" },
    { "+synchronous", "*synchronous", OptionValueImplicit, "off" },
    { "-title", "*title", OptionValueNext },
    { "-visual", "*visual", OptionValueNext },
    { "-visual_id", "*visual_id", OptionValueNext },
    { "-xrm", nil, OptionPropertyNext },
#ifdef sgi
    { "-malloc", "*malloc_debug", OptionValueImplicit, "on" },
#endif
    { nil }
};

/*
 * Session representation.
 */

declarePtrList(DisplayList,Display)
implementPtrList(DisplayList,Display)

class SessionRep {
private:
    friend class Session;

    SessionRep();
    ~SessionRep();
public:
    void handle_display_input(Display*);
private:
    int argc_;
    char** argv_;
    boolean done_;
    boolean readinput_;
    const char* classname_;
    String* name_;
    Style* style_;
    const PropertyData* props_;
    Display* default_;
    DisplayList* displays_;
    static Session* instance_;

    void init(
	const char*, int& argc, char** argv,
	const OptionDesc*, const PropertyData*
    );
    void parse_args(int& argc, char** argv, const OptionDesc*);
    boolean match(
	const String& arg, const OptionDesc& o, int& i, int argc, char** argv
    );
    void extract(
	const String& arg, const OptionDesc& o, int& i, int argc, char** argv,
	String& name, String& value
    );
    void bad_arg(const char* fmt, const String& arg);
    String next_arg(
	int& i, int argc, char** argv, const char* message, const String&
    );
    boolean find_arg(const String& name, String& value);

    void init_style(const char*, const PropertyData*);
    String* find_name();
    void load_props(Style*, const PropertyData*, int priority);
    void load_app_defaults(Style*, int priority);
    void load_environment(Style*, int priority);
    void load_path(Style*, const char*, const char*, int priority);
    void load_path(
	Style*, const char*, const char*, const char*, int priority
    );
    const char* home();

    void init_display();
    void connect(Display*);
    void set_style(Display*);
    boolean check(Event&);
};

Session* SessionRep::instance_;

/*
 * SessionIOHandler is a helper class to translate input ready for a
 * display fd into a call to SessionRep::handle_display_input(Display*);
 */

class SessionIOHandler : public IOHandler {
public:
    SessionIOHandler(SessionRep*, Display*);

    virtual int inputReady(int);
private:
    SessionRep* session_;
    Display* display_;
};

SessionIOHandler::SessionIOHandler(SessionRep* s, Display* d) {
    session_ = s;
    display_ = d;
}

int SessionIOHandler::inputReady(int) {
    session_->handle_display_input(display_);
    return 0;
}

Session::Session(
    const char* classname, int& argc, char** argv,
    const OptionDesc* opts, const PropertyData* initprops
) {
    SessionRep::instance_ = this;
    rep_ = new SessionRep();
    rep_->init(classname, argc, argv, opts, initprops);
}

Session::~Session() {
    delete rep_;
}

Session* Session::instance() { return SessionRep::instance_; }

const char* Session::name() const { return rep_->name_->string(); }
const char* Session::classname() const { return rep_->classname_; }
int Session::argc() const { return rep_->argc_; }
char** Session::argv() const { return rep_->argv_; }

Style* Session::style() const {
    SessionRep* s = rep_;
    if (s->default_ != nil) {
	return s->default_->style();
    }
    return s->style_;
}

void Session::default_display(Display* d) { rep_->default_ = d; }
Display* Session::default_display() const { return rep_->default_; }

Display* Session::connect(const String& display_name) {
    Display* d = Display::open(display_name);
    if (d != nil) {
	rep_->connect(d);
    }
    return d;
}

Display* Session::connect(const char* display_name) {
    Display* d = Display::open(display_name);
    if (d != nil) {
	rep_->connect(d);
    }
    return d;
}
    
void Session::disconnect(Display* d) {
    DisplayList& list = *(rep_->displays_);
    long n = list.count();
    for (long i = 0; i < n; i++) {
	if (list.item(i) == d) {
	    Dispatcher::instance().unlink(d->fd());
	    d->close();
	    list.remove(i);
	    break;
	}
    }
}

void SessionRep::handle_display_input(Display* d) {
    if (d->closed()) {
	done_ = true;
	return;
    }
    if (readinput_) {
	/*
	 * I know this loop would better be expressed as a while loop,
	 * but my silly compiler doesn't understand it in that form.
	 */
	Event e;
	for (;;) {
	    if (!d->get(e)) {
		break;
	    }
	    e.handle();
	}
    }
}

/*
 * Main session loop: read events and handle them.
 */

int Session::run() {
    Event e;
    boolean& done = rep_->done_;
    done = false;
    do {
	read(e);
	e.handle();
    } while (!done);
    return 0;
}

/*
 * Map window and run.
 */

int Session::run_window(Window* w) {
    w->map();
    return run();
}

/*
 * Exit the main session loop.
 */

void Session::quit() {
    rep_->done_ = true;
}

/*
 * Return loop status.
 */

boolean Session::done() const {
    return rep_->done_;
}

/*
 * Check if an event is pending on any display.
 */

boolean Session::pending() const {
    Event e;
    boolean b = rep_->check(e);
    if (b) {
	e.unread();
    }
    return b;
}

/*
 * Read an event.  Could be from any open display.
 * The redundant-looking test of rep_->done_ is necessary
 * because check might change the value of done_.
 */

void Session::read(Event& e) {
    boolean save = rep_->readinput_;
    rep_->readinput_ = false;
    while (!rep_->done_ && !rep_->check(e) && !rep_->done_) {
	Dispatcher::instance().dispatch();
    }
    rep_->readinput_ = save;
}

/*
 * Read an event as above, but time out after a given (sec, usec) delay.
 * Return true if an event was read, false if the time-out expired.
 */

boolean Session::read(long sec, long usec, Event& e) {
    long sec_left = sec;
    long usec_left = usec;
    boolean save = rep_->readinput_;
    rep_->readinput_ = false;
    while (!rep_->done_ && !rep_->check(e) && !rep_->done_) {
	if (!(sec_left > 0 || usec_left > 0)) {
	    rep_->readinput_ = save;
	    return false;
	}
	Dispatcher::instance().dispatch(sec_left, usec_left);
    }
    rep_->readinput_ = save;
    return true;
}

/*
 * Check for a pending event, returning it if there is one.
 */

boolean SessionRep::check(Event& e) {
    DisplayList& list = *displays_;
    long n = list.count();
    for (long i = 0; i < n; i++) {
	Display* d = list.item(i);
	if (d->get(e)) {
	    return true;
	}
    }
    return false;
}

/*
 * Put an event back from whence it came.
 */

void Session::unread(Event& e) {
    e.unread();
}

/*
 * Poll an event (implies the event already has an associated display).
 */

void Session::poll(Event& e) {
    e.poll();
}

SessionRep::SessionRep() {
    done_ = false;
    readinput_ = true;
    displays_ = new DisplayList;
}

SessionRep::~SessionRep() {
    delete name_;
    Resource::unref(style_);
    for (ListItr(DisplayList) i(*displays_); i.more(); i.next()) {
	Display* d = i.cur();
	delete d;
    }
    delete displays_;
    delete argv_;
}

void SessionRep::init(
    const char* name, int& argc, char** argv,
    const OptionDesc* opts, const PropertyData* initprops
) {
    argc_ = argc;
    argv_ = new char*[argc + 1];
    for (int i = 0; i < argc; i++) {
	argv_[i] = argv[i];
    }
    argv_[argc_] = nil;

    init_style(name, initprops);
    if (opts != nil) {
	parse_args(argc, argv, opts);
    }
    parse_args(argc, argv, defoptions);
    init_display();

    Cursor::init();

#ifdef sgi
    if (style_->value_is_on("malloc_debug")) {
	mallopt(M_DEBUG, 1);
    }
#endif
}

/*
 * Parse the argument list, setting any properties that are specified
 * by the option list.  Matching arguments are removed (in-place)
 * from the argument list.
 */

void SessionRep::parse_args(int& argc, char** argv, const OptionDesc* opts) {
    int i;
    int newargc = 1;
    char* newargv[1024];
    newargv[0] = argv[0];
    for (i = 1; i < argc; i++) {
	boolean matched = false;
	String arg(argv[i]);
	for (const OptionDesc* o = &opts[0]; o->name != nil; o++) {
	    if (match(arg, *o, i, argc, argv)) {
		matched = true;
		break;
	    }
	}
	if (!matched) {
	    newargv[newargc] = argv[i];
	    ++newargc;
	}
    }
    if (newargc < argc) {
	for (i = 1; i < newargc; i++) {
	    argv[i] = newargv[i];
	}
	argc = newargc;
	argv[argc] = nil;
    }
}

/*
 * See if the given argument matches the option description.
 */

boolean SessionRep::match(
    const String& arg, const OptionDesc& o, int& i, int argc, char** argv
) {
    String opt(o.name);
    if (arg != opt) {
	if (o.style == OptionValueAfter) {
	    int n = opt.length();
	    if (opt == arg.left(n)) {
		style_->attribute(String(o.path), arg.right(n));
		return true;
	    }
	}
	return false;
    }
    String name, value;
    extract(arg, o, i, argc, argv, name, value);
    style_->attribute(name, value);
    return true;
}

/*
 * Extract an attribute <name, value> from a given argument.
 */

void SessionRep::extract(
    const String& arg, const OptionDesc& o, int& i, int argc, char** argv,
    String& name, String& value
) {
    int colon;
    switch (o.style) {
    case OptionPropertyNext:
	value = next_arg(i, argc, argv, "missing property after '%s'", arg);
	colon = value.index(':');
	if (colon < 0) {
	    bad_arg("missing ':' in '%s'", value);
	} else {
	    name = value.left(colon);
	    value = value.right(colon+1);
	}
	break;
    case OptionValueNext:
	name = o.path;
	value = next_arg(i, argc, argv, "missing value after '%s'", arg);
	break;
    case OptionValueImplicit:
	name = o.path;
	value = o.value;
	break;
    case OptionValueIsArg:
	name = o.path;
	value = arg;
	break;
    case OptionValueAfter:
	bad_arg("missing value in '%s'", arg);
	break;
    }
}

/*
 * Report that an argument is bad and exit.  A caller of this function
 * may assume that it does not return.
 *
 * We also assume that arg is null-terminated (because it came
 * from argv).
 */

void SessionRep::bad_arg(const char* fmt, const String& arg) {
    fflush(stdout);
    fprintf(stderr, fmt, arg.string());
    putc('\n', stderr);
    exit(1);
}

/*
 * Make sure there is another argument--if not generate an error.
 */

String SessionRep::next_arg(
    int& i, int argc, char** argv, const char* message, const String& arg
) {
    ++i;
    if (i == argc) {
	bad_arg(message, arg);
    }
    return String(argv[i]);
}

/*
 * Find the value for a specific argument.
 */

boolean SessionRep::find_arg(const String& arg, String& value) {
    int last = argc_ - 1;
    for (int i = 1; i < last; i++) {
	if (arg == argv_[i]) {
	    value = String(argv_[i+1]);
	    return true;
	}
    }
    return false;
}

/*
 * Initialize style information for the session.
 */

void SessionRep::init_style(const char* name, const PropertyData* props) {
    classname_ = name;
    name_ = find_name();
    style_ = new Style(*name_);
    Resource::ref(style_);
    style_->alias(classname_);
    props_ = props;
}

/*
 * Set the style information for the given display.
 * First, copy the current style (just the parsed command-line arguments),
 * then add the environment style, display defaults, app defaults,
 * session properties, and default properties.
 */

void SessionRep::set_style(Display* d) {
    Style* s = new Style(*style_);
    load_props(s, defpropvalues, -5);
    load_path(s, IV_LIBALL, "/app-defaults/InterViews", -5);
    load_props(s, props_, -5);
    load_app_defaults(s, -5);
    String str;
    if (d->defaults(str)) {
	s->load_list(str, -5);
    } else {
	load_path(s, home(), "/.Xdefaults", -5);
    }
    load_environment(s, -5);
    d->style(s);
}

void SessionRep::load_props(
    Style* s, const PropertyData* props, int priority
) {
    if (props != nil) {
	for (const PropertyData* p = &props[0]; p->path != nil; p++) {
	    s->attribute(String(p->path), String(p->value), priority);
	}
    }
}

void SessionRep::load_app_defaults(Style* s, int priority) {
    load_path(s, X_LIBDIR, "/app-defaults/", classname_, priority);
    load_path(s, IV_LIBALL, "/app-defaults/", classname_, priority);
    const char* xres = getenv("XAPPLRESDIR");
    if (xres != nil) {
	load_path(s, xres, "/", classname_, priority);
    } else {
	load_path(s, home(), "/", classname_, priority);
    }
}

void SessionRep::load_environment(Style* s, int priority) {
    const char* xenv = getenv("XENVIRONMENT");
    if (xenv != nil) {
	s->load_file(String(xenv), priority);
    } else {
	load_path(s, ".Xdefaults-", Host::name(), priority);
    }
}

void SessionRep::load_path(
    Style* s, const char* head, const char* tail, int priority
) {
    String h(head);
    String t(tail);
    char* buf = new char[h.length() + t.length() + 1];
    sprintf(buf, "%s%s", h.string(), t.string());
    s->load_file(String(buf), priority);
    delete buf;
}

const char* SessionRep::home() {
    const char* home = getenv("HOME");
    if (home == nil) {
	home = ".";
    }
    return home;
}

void SessionRep::load_path(
    Style* s, const char* head, const char* middle, const char* tail,
    int priority
) {
    String h(head);
    String m(middle);
    String t(tail);
    char* buf = new char[h.length() + m.length() + t.length() + 1];
    sprintf(buf, "%s%s%s", h.string(), m.string(), t.string());
    s->load_file(String(buf), priority);
    delete buf;
}

/*
 * Use ICCCM rules to find an application's instance name
 * from the command line or an environment variable.
 */

String* SessionRep::find_name() {
    String name;
    if (find_arg(String("-name"), name)) {
	return new String(name);
    }

    const char* res_name = getenv("RESOURCE_NAME");
    if (res_name != nil) {
	return new String(res_name);
    }

    if (argc_ > 0) {
	String s(argv_[0]);
	int slash = s.rindex('/');
	if (slash >= 0) {
	    s = s.right(slash + 1);
	}
	return new String(s);
    }

    return new String("noname");
}

/*
 * Open the default display and initialize its style information.
 */

void SessionRep::init_display() {
    String name;
    if (style_->find_attribute(String("display"), name)) {
	default_ = Display::open(name);
    } else {
	default_ = Display::open();
    }
    if (default_ == nil) {
	if (name.length() > 0) {
	    fprintf(
		stderr, "can't open display %.*s\n",
		name.length(), name.string()
	    );
	} else {
	    fprintf(stderr, "can't open DISPLAY\n");
	}
	exit(1);
    }
    connect(default_);
}

void SessionRep::connect(Display* d) {
    set_style(d);
    Dispatcher::instance().link(
	d->fd(), Dispatcher::ReadMask, new SessionIOHandler(this, d)
    );
    displays_->append(d);
}
