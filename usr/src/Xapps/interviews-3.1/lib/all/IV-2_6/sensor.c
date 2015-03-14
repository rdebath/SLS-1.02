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
 * Implementation of input interest handling.
 */

#include <IV-2_6/InterViews/sensor.h>

extern unsigned long
    motionmask, keymask, entermask, leavemask, focusmask,
    upmask, downmask, initmask;

Sensor::Sensor() {
    mask = initmask;
    for (register int i = 0; i < 8; i++) {
	down[i] = 0;
	up[i] = 0;
    }
    ref();
}

Sensor::Sensor(const Sensor& s) {
    *this = s;
    ref();
}

Sensor::Sensor(const Sensor* s) {
    *this = *s;
    ref();
}

Sensor::~Sensor() { }

Sensor& Sensor::operator =(register const Sensor& s) {
    mask = s.mask;
    for (register int i = 0; i < 8; i++) {
	down[i] = s.down[i];
	up[i] = s.up[i];
    }
    return *this;
}

Sensor* allEvents;
Sensor* onoffEvents;
Sensor* updownEvents;
Sensor* noEvents;

void Sensor::init() {
    allEvents = new Sensor;
    allEvents->Catch(MotionEvent);
    allEvents->Catch(DownEvent);
    allEvents->Catch(UpEvent);
    allEvents->Catch(KeyEvent);
    allEvents->Catch(EnterEvent);
    allEvents->Catch(LeaveEvent);
    onoffEvents = new Sensor;
    onoffEvents->Catch(EnterEvent);
    onoffEvents->Catch(LeaveEvent);
    updownEvents = new Sensor;
    updownEvents->Catch(UpEvent);
    updownEvents->Catch(DownEvent);
    noEvents = new Sensor;
}

void Sensor::Catch(EventType t) {
    register int i;

    switch (t) {
	case MotionEvent:
	    mask |= motionmask;
	    break;
	case DownEvent:
	    mask |= downmask;
	    SetMouseButtons(down);
	    break;
	case UpEvent:
	    mask |= upmask;
	    SetMouseButtons(up);
	    break;
	case KeyEvent:
	    mask |= keymask;
	    down[0] |= 0xfffffff8;
	    for (i = 1; i < 8; i++) {
		down[i] = 0xffffffff;
	    }
	    break;
	case EnterEvent:
	    mask |= entermask;
	    break;
	case LeaveEvent:
	    mask |= leavemask;
	    break;
	case FocusInEvent:
	case FocusOutEvent:
	    mask |= focusmask;
	    break;
    }
}

void Sensor::CatchButton(EventType t, int b) {
    switch (t) {
	case DownEvent:
	    mask |= downmask;
	    SetButton(down, b);
	    break;
	case UpEvent:
	    mask |= upmask;
	    SetButton(up, b);
	    break;
	case KeyEvent:
	    mask |= keymask;
	    SetButton(down, b);
	    break;
	default:
	    /* ignore */
	    break;
    }
}

void Sensor::Ignore(EventType t) {
    register int i;

    switch (t) {
	case MotionEvent:
	    mask &= ~motionmask;
	    break;
	case DownEvent:
	    ClearMouseButtons(down);
	    if (!MouseButtons(up)) {
		mask &= ~downmask;
	    }
	    break;
	case UpEvent:
	    ClearMouseButtons(up);
	    if (!MouseButtons(down)) {
		mask &= ~upmask;
	    }
	    break;
	case KeyEvent:
	    down[0] &= ~0xfffffff8;
	    for (i = 1; i < 8; i++) {
		down[i] = 0;
	    }
	    mask &= ~keymask;
	    break;
	case EnterEvent:
	    mask &= ~entermask;
	    break;
	case LeaveEvent:
	    mask &= ~leavemask;
	    break;
	case FocusInEvent:
	case FocusOutEvent:
	    mask &= ~focusmask;
	    break;
    }
}

void Sensor::IgnoreButton(EventType t, int b) {
    register int i;

    switch (t) {
	case DownEvent:
	    ClearButton(down, b);
	    if (!MouseButtons(down) && !MouseButtons(up)) {
		mask &= ~downmask;
	    }
	    break;
	case UpEvent:
	    ClearButton(up, b);
	    if (!MouseButtons(up) && !MouseButtons(down)) {
		mask &= ~upmask;
	    }
	    break;
	case KeyEvent:
	    ClearButton(down, b);
	    if ((down[0] & 0xfffffff8) == 0) {
		mask &= ~keymask;
		for (i = 1; i < 8; i++) {
		    if (down[i] != 0) {
			mask |= keymask;
			break;
		    }
		}
	    }
	    break;
	default:
	    /* ignore */
	    break;
    }
}
