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
 * X11-dependent event-reading code
 */

#include "wtable.h"
#include <InterViews/display.h>
#include <InterViews/event.h>
#include <InterViews/handler.h>
#include <InterViews/session.h>
#include <InterViews/window.h>
#include <IV-X11/Xlib.h>
#include <IV-X11/Xutil.h>
#include <IV-X11/xcanvas.h>
#include <IV-X11/xdisplay.h>
#include <IV-X11/xdrag.h>
#include <IV-X11/xevent.h>
#include <IV-X11/xwindow.h>
#include <X11/keysymdef.h>
#include <string.h>

Event::Event() {
    if (sizeof(EventRep) <= sizeof(free_store_)) {
	rep_ = (EventRep*)free_store_;
    } else {
	rep_ = new EventRep;
    }
    EventRep& e = *rep_;
    e.display_ = nil;
    e.window_ = nil;
    e.xevent_.type = LASTEvent;
    e.pointer_x_ = 0;
    e.pointer_y_ = 0;
    e.clear();

    /* backward compatibility */
    target = nil;
    timestamp = 0;
    eventType = undefined;
    x = 0;
    y = 0;
    control = false;
    meta = false;
    shift = false;
    shiftlock = false;
    leftmouse = false;
    middlemouse = false;
    rightmouse = false;
    button = 0;
    len = 0;
    keystring = keydata;
    w = nil;
    wx = 0;
    wy = 0;
}

Event::Event(const Event& e) {
    if (sizeof(EventRep) <= sizeof(free_store_)) {
	rep_ = (EventRep*)free_store_;
    } else {
	rep_ = new EventRep;
    }
    *this = e;
}

Event::~Event() {
    if (rep_ != (EventRep*)free_store_) {
	delete rep_;
    }
}

Event& Event::operator =(const Event& e) {
    copy_rep(e);
    target = e.target;
    timestamp = e.timestamp;
    eventType = e.eventType;
    x = e.x;
    y = e.y;
    control = e.control;
    meta = e.meta;
    shift = e.shift;
    shiftlock = e.shiftlock;
    leftmouse = e.leftmouse;
    middlemouse = e.middlemouse;
    rightmouse = e.rightmouse;
    button = e.button;
    len = e.len;
    if (e.keystring == e.keydata) {
	keystring = keydata;
	strncpy(keydata, e.keydata, e.len);
    } else {
	keystring = e.keystring;
    }
    w = e.w;
    wx = e.wx;
    wy = e.wy;
    return *this;
}

void Event::copy_rep(const Event& e) {
    *rep_ = *e.rep_;
}

void Event::display(Display* d) { rep()->display_ = d; }
Display* Event::display() const { return rep()->display_; }
void Event::window(Window* w) { rep()->window_ = w; }
Window* Event::window() const { return rep()->window_; }

boolean Event::pending() const {
    Event e;
    if (rep()->display_->get(e)) {
	rep()->display_->put(e);
	return true;
    }
    return false;
}

void Event::read() {
    Session::instance()->read(*this);
}

boolean Event::read(long s, long u) {
    return Session::instance()->read(s, u, *this);
}

void Event::unread() { rep()->display_->put(*this); }

void Event::poll() {
    EventRep& e = *rep();
    if (e.display_ == nil) {
	if (e.window_ == nil) {
	    e.display_ = Session::instance()->default_display();
	} else {
	    e.display_ = e.window_->display();
	}
    }
    DisplayRep& d = *(e.display_->rep());
    XMotionEvent& m = e.xevent_.xmotion;
    if (e.window_ == nil) {
	m.window = d.root_;
    } else {
	m.window = e.window_->rep()->xwindow_;
    }
    XQueryPointer(
	d.display_, m.window, &m.root, &m.subwindow,
	&m.x_root, &m.y_root, &m.x, &m.y, &m.state
    );
    m.type = MotionNotify;
    e.clear();
}

Handler* Event::handler() const {
    Handler* h = nil;
    Window* w = rep()->window_;
    if (w != nil) {
	h = w->target(*this);
    }
    return h;
}

void Event::handle() {
    Handler* h = nil;
    if (rep()->xevent_.type != KeyPress) {
	h = grabber();
    }
    if (h == nil) {
	h = handler();
    }
    if (h != nil) {
	boolean b = Resource::defer(true);
	h->ref();
	h->event(*this);
	h->unref();
	Resource::flush();
	Resource::defer(b);
    }
}

void Event::grab(Handler* h) const {
    EventRep& e = *rep();
    e.display_->grab(e.window_, h);
}

void Event::ungrab(Handler* h) const { rep()->display_->ungrab(h); }
Handler* Event::grabber() const { return rep()->display_->grabber(); }

boolean Event::is_grabbing(Handler* h) const {
    return rep()->display_->is_grabbing(h);
}

EventType Event::type() const {
    switch (rep()->xevent_.type) {
    case MotionNotify:
    case EnterNotify:
    case LeaveNotify:
	return motion;
    case ButtonPress:
	return down;
    case ButtonRelease:
	return up;
    case KeyPress:
	return key;
    default:
	return other_event;
    }
}

unsigned long Event::time() const {
    XEvent& xe = rep()->xevent_;
    switch (xe.type) {
    case MotionNotify:
    case EnterNotify:
    case LeaveNotify:
	return xe.xmotion.time;
    case ButtonPress:
    case ButtonRelease:
	return xe.xbutton.time;
    case KeyPress:
	return xe.xkey.time;
    default:
	return CurrentTime;
    }
}

Coord Event::pointer_x() const {
    EventRep& e = *rep();
    e.locate();
    return e.pointer_x_;
}

Coord Event::pointer_y() const {
    EventRep& e = *rep();
    e.locate();
    return e.pointer_y_;
}

Coord Event::pointer_root_x() const {
    EventRep& e = *rep();
    e.locate();
    return e.pointer_root_x_;
}

Coord Event::pointer_root_y() const {
    EventRep& e = *rep();
    e.locate();
    return e.pointer_root_y_;
}

EventButton Event::pointer_button() const {
    XEvent& xe = rep()->xevent_;
    switch (xe.type) {
    case ButtonPress:
    case ButtonRelease:
	switch (xe.xbutton.button) {
	case Button1:
	    return left;
	case Button2:
	    return middle;
	case Button3:
	    return right;
	default:
	    return other_button;
	}
    default:
	return none;
    }
}

unsigned int Event::keymask() const {
    XEvent& xe = rep()->xevent_;
    switch (xe.type) {
    case MotionNotify:
	return xe.xmotion.state;
    case ButtonPress:
    case ButtonRelease:
	return xe.xbutton.state;
    case KeyPress:
	return xe.xkey.state;
    case EnterNotify:
    case LeaveNotify:
	return xe.xcrossing.state;
    default:
	/* is this really correct? */
	return 0;
    }
}

static boolean check_key(const Event* e, unsigned long mask) {
    return (e->keymask() & mask) != 0;
}

boolean Event::control_is_down() const { return check_key(this, ControlMask); }
boolean Event::meta_is_down() const { return check_key(this, Mod1Mask); }
boolean Event::shift_is_down() const { return check_key(this, ShiftMask); }
boolean Event::capslock_is_down() const { return check_key(this, LockMask); }
boolean Event::left_is_down() const { return check_key(this, Button1Mask); }
boolean Event::middle_is_down() const { return check_key(this, Button2Mask); }
boolean Event::right_is_down() const { return check_key(this, Button3Mask); }

unsigned char Event::keycode() const {
    XEvent& xe = rep()->xevent_;
    if (xe.type == KeyPress) {
	return xe.xkey.keycode;
    }
    return 0;
}

unsigned long Event::keysym() const {
    XEvent& xe = rep()->xevent_;
    if (xe.type == KeyPress) {
	return XLookupKeysym(&xe.xkey, 0);
    }
    return XK_VoidSymbol;
}

unsigned int Event::mapkey(char* buf, unsigned int len) const {
    unsigned int n = 0;
    XEvent& xe = rep()->xevent_;
    if (xe.type == KeyPress) {
	n = XLookupString(&xe.xkey, buf, len, nil, nil);
	/*
	 * R5 internationalization might make this superfluous.
	 */
	if (meta_is_down()) {
	    for (unsigned int i = 0; i < n; i++) {
		buf[i] |= 0200;
	    }
	}
    }
    return n;
}

/** class EventRep **/

void EventRep::clear() {
    location_valid_ = false;
}

void EventRep::locate() {
    if (!location_valid_ && window_ != nil) {
	PixelCoord x, y, root_x = 0, root_y = 0;
	boolean has_root_location = true;
	XEvent& xe = xevent_;
	switch (xe.type) {
	case MotionNotify:
	    x = xe.xmotion.x;
	    y = xe.xmotion.y;
	    root_x = xe.xmotion.x_root;
	    root_y = xe.xmotion.y_root;
	    break;
	case ButtonPress:
	case ButtonRelease:
	    x = xe.xbutton.x;
	    y = xe.xbutton.y;
	    root_x = xe.xbutton.x_root;
	    root_y = xe.xbutton.y_root;
	    break;
	case KeyPress:
	    x = xe.xkey.x;
	    y = xe.xkey.y;
	    root_x = xe.xkey.x_root;
	    root_y = xe.xkey.y_root;
	    break;
	case EnterNotify:
	case LeaveNotify:
	    x = xe.xcrossing.x;
	    y = xe.xcrossing.y;
	    root_x = xe.xcrossing.x_root;
	    root_y = xe.xcrossing.y_root;
	    break;
	case ClientMessage: /* drag & drop */
	    if (!XDrag::isDrag(xe)) {
		has_pointer_location_ = false;
		return;
	    }
	    XDrag::locate(xe, x, y);
	    has_root_location = false;
	    break;
	default:
	    has_pointer_location_ = false;
	    return;
	}
	has_pointer_location_ = true;
	pointer_x_ = display_->to_coord(x);
	pointer_y_ = display_->to_coord(window_->canvas()->pheight() - y);
	pointer_root_x_ = display_->to_coord(root_x);
	pointer_root_y_ = display_->to_coord(display_->pheight() - root_y);
	location_valid_ = true;

	/*
	 * As a side effect, the pointer location tells us the root-relative
	 * location of the window.
	 */
	if (has_root_location) {
	    window_->rep()->move(window_, root_x - x, root_y - y);
	}
    }
}

boolean EventRep::has_pointer_location() {
    locate();
    return has_pointer_location_;
}

void EventRep::acknowledge_motion() {
    XMotionEvent& m = xevent_.xmotion;
    XQueryPointer(
	display_->rep()->display_, m.window,
	&m.root, &m.subwindow, &m.x_root, &m.y_root, &m.x, &m.y, &m.state
    );
}
