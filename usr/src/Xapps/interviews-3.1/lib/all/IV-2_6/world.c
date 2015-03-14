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
 * A world is the root scene for a display.
 */

#include <InterViews/display.h>
#include <InterViews/color.h>
#include <InterViews/font.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <IV-2_6/InterViews/sensor.h>
#include <IV-2_6/InterViews/world.h>
#include <OS/string.h>

double inch, inches, cm, mm, point, points;

World* World::current_;

World::World() {
    session_ = nil;
    display_ = nil;
}

World::World(
    const char* classname, int& argc, char** argv,
    const OptionDesc* opts, const PropertyData* initprops
) {
    session_ = new Session(classname, argc, argv, opts, initprops);
    display_ = session_->default_display();
    make_current();
    Sensor::init();
}

World::~World() {
    delete session_;
}

Session* World::session() const { return session_; }
Display* World::display() const { return display_; }
const char* World::name() const { return session_->name(); }
const char* World::classname() const { return session_->classname(); }
int World::argc() const { return session_->argc(); }
char** World::argv() const { return session_->argv(); }

Style* World::style() const {
    return display_->style();
}

const char* World::property_value(const char* s) const {
    String v;
    if (style()->find_attribute(String(s), v)) {
	return v.string();
    }
    return nil;
}

boolean World::property_is_on(const char* s) const {
    return style()->value_is_on(String(s));
}

const Font* World::font() const {
    const Font* f = nil;
    Style* s = style();
    String v;
    if (s->find_attribute("font", v) || s->find_attribute("Font", v)) {
	f = Font::lookup(v);
    }
    if (f == nil) {
	f = Font::lookup("fixed");
    }
    return f;
}

const Color* World::foreground() const {
    const Color* c = nil;
    Style* s = style();
    String v;
    if (s->find_attribute("foreground", v) ||
	s->find_attribute("Foreground", v)
    ) {
	c = Color::lookup(display(), v);
    }
    if (c == nil) {
	c = new Color(0.0, 0.0, 0.0, 1.0);
    }
    return c;
}

const Color* World::background() const {
    const Color* c = nil;
    Style* s = style();
    String v;
    if (s->find_attribute("background", v) ||
	s->find_attribute("Background", v)
    ) {
	c = Color::lookup(display(), v);
    }
    if (c == nil) {
	c = new Color(1.0, 1.0, 1.0, 1.0);
    }
    return c;
}

boolean World::shaped_windows() const {
    return style()->value_is_on("shaped_windows");
}

boolean World::double_buffered() const {
    return style()->value_is_on("double_buffered");
}

void World::flush() { display_->flush(); }
void World::sync() { display_->sync(); }
Coord World::width() const { return display_->width(); }
Coord World::height() const { return display_->height(); }
unsigned int World::pwidth() const { return display_->pwidth(); }
unsigned int World::pheight() const { return display_->pheight(); }

void World::run() { session_->run(); }
void World::quit() { session_->quit(); }
boolean World::done() const { return session_->done(); }

boolean World::pending() const { return session_->pending(); }
void World::read(Event& e) { session_->read(e); }
boolean World::read(long sec, long usec, Event& e) {
    return session_->read(sec, usec, e);
}
void World::unread(Event& e) { session_->unread(e); }
void World::poll(Event& e) { session_->poll(e); }

void World::RingBell(int i) { display_->ring_bell(i); }
void World::SetKeyClick(int i) { display_->set_key_click(i); }
void World::SetAutoRepeat(boolean b) { display_->set_auto_repeat(b); }
void World::SetFeedback(int t, int s) { display_->set_pointer_feedback(t, s); }

void World::SetScreen(int s) { display_->set_screen(s); }

World* World::current() {
    if (current_ == nil) {
	World* w = new World;
	w->session_ = Session::instance();
	w->display_ = w->session_->default_display();
	w->make_current();
	Sensor::init();
    }
    return current_;
}

void World::make_current() {
    current_ = this;
    point = double(display_->pwidth()) / display_->a_width();
    points = point;
    inch = 72.27 * point;
    inches = inch;
    cm = inch / 2.54;
    mm = inch / 25.4;
}
