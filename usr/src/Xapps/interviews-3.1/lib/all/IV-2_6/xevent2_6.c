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

#include <InterViews/display.h>
#include <InterViews/event.h>
#include <IV-2_6/InterViews/sensor.h>
#include <IV-2_6/InterViews/world.h>
#include <IV-X11/Xlib.h>
#include <IV-X11/Xutil.h>
#include <IV-X11/xdisplay.h>
#include <IV-X11/xevent.h>
#include <string.h>

Mask motionmask = PointerMotionMask;
Mask keymask = KeyPressMask;
Mask entermask = EnterWindowMask;
Mask leavemask = LeaveWindowMask;
Mask focusmask = FocusChangeMask;
Mask substructmask = SubstructureRedirectMask;
Mask upmask = ButtonReleaseMask|OwnerGrabButtonMask;
Mask downmask = ButtonPressMask|OwnerGrabButtonMask;
Mask initmask = PointerMotionHintMask;

boolean Sensor::Caught(const Event& e) const {
    XEvent& xe = e.rep()->xevent_;
    switch (xe.type) {
    case MotionNotify:
	return (mask & motionmask) != 0;
    case FocusIn:
    case FocusOut:
	return (mask & focusmask) != 0;
    case KeyPress:
    case ButtonPress:
	return ButtonIsSet(down, e.button);
    case ButtonRelease:
	return ButtonIsSet(up, e.button);
    case EnterNotify:
	return (mask & entermask) != 0 &&
	    e.rep()->xevent_.xcrossing.detail != NotifyInferior;
    case LeaveNotify:
	return (mask & leavemask) != 0 &&
	    e.rep()->xevent_.xcrossing.detail != NotifyInferior;
    }
    return false;
}

void Event::GetInfo() {
    EventRep& e = *rep();
    w = World::current();
    y = 0;
    XEvent& xe = e.xevent_;
    switch (xe.type) {
    case MotionNotify:
	GetMotionInfo();
	break;
    case KeyPress:
	GetKeyInfo();
	break;
    case ButtonPress:
	GetButtonInfo(DownEvent);
	break;
    case ButtonRelease:
	GetButtonInfo(UpEvent);
	break;
    case FocusIn:
	eventType = FocusInEvent;
	break;
    case FocusOut:
	eventType = FocusOutEvent;
	break;
    case EnterNotify:
	GetCrossingInfo(EnterEvent);
	break;
    case LeaveNotify:
	GetCrossingInfo(LeaveEvent);
	break;
    }
}

void Event::GetMotionInfo() {
    rep()->acknowledge_motion();

    XMotionEvent& m = rep()->xevent_.xmotion;
    eventType = MotionEvent;
    timestamp = m.time;
    x = m.x;
    y = m.y;
    wx = m.x_root;
    wy = m.y_root;
    GetKeyState(m.state);
}

void Event::GetButtonInfo(EventType t) {
    XButtonEvent& b = rep()->xevent_.xbutton;
    eventType = t;
    timestamp = b.time;
    x = b.x;
    y = b.y;
    wx = b.x_root;
    wy = b.y_root;
    button = b.button - 1;
    len = 0;
    GetKeyState(b.state | (Button1Mask << button));
}

void Event::GetKeyInfo() {
    XKeyEvent& k = rep()->xevent_.xkey;
    char buf[4096];

    eventType = KeyEvent;
    timestamp = k.time;
    x = k.x;
    y = k.y;
    wx = k.x_root;
    wy = k.y_root;
    button = k.keycode;
    len = mapkey(buf, sizeof(buf));
    if (len != 0) {
	if (len < sizeof(keydata)) {
	    keystring = keydata;
	} else {
	    keystring = new char[len+1];
	}
	strncpy(keystring, buf, len);
	keystring[len] = '\0';
    } else {
	keystring = keydata;
	keydata[0] = '\0';
    }
    GetKeyState(k.state);
}

void Event::GetKeyState(unsigned state) {
    shift = (state & ShiftMask) != 0;
    control = (state & ControlMask) != 0;
    meta = (state & Mod1Mask) != 0;
    shiftlock = (state & LockMask) != 0;
    leftmouse = (state & Button1Mask) != 0;
    middlemouse = (state & Button2Mask) != 0;
    rightmouse = (state & Button3Mask) != 0;
}

void Event::GetCrossingInfo(EventType t) {
    XCrossingEvent& c = rep()->xevent_.xcrossing;
    eventType = t;
    if (c.detail != NotifyInferior) {
	timestamp = c.time;
	x = c.x;
	y = c.y;
	wx = c.x_root;
	wy = c.y_root;
	GetKeyState(c.state);
    }
}

void Event::GetAbsolute(_lib_iv2_6(Coord)& absx, _lib_iv2_6(Coord)& absy) {
    absx = wx;
    absy = rep()->display_->pheight() - wy;
}

void Event::GetAbsolute(
    World*& wd, _lib_iv2_6(Coord)& absx, _lib_iv2_6(Coord)& absy
) {
    wd = w;
    absx = wx;
    absy = rep()->display_->pheight() - wy;
}
