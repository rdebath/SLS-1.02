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
 * Button - clickable Action
 */

#include <IV-look/button.h>
#include <IV-look/telltale.h>
#include <InterViews/action.h>
#include <InterViews/event.h>

Button::Button(
    Glyph* g, Style* s, TelltaleState* state, Action* a
) : ActiveHandler(g, s) {
    state_ = state;
    Resource::ref(state_);
    state_->attach(this);
    action_ = a;
    Resource::ref(action_);
}

Button::~Button() {
    state_->detach(this);
    Resource::unref(state_);
    Resource::unref(action_);
}

void Button::state(TelltaleState* s) {
    if (s != state_) {
	Resource::ref(s);
	s->attach(this);
	state_->detach(this);
	Resource::unref(state_);
	state_ = s;
    }
}

TelltaleState* Button::state() const { return state_; }

void Button::action(Action* a) {
    if (a != action_) {
	Resource::ref(a);
	Resource::unref(action_);
	action_ = a;
    }
}

Action* Button::action() const { return action_; }

void Button::enter() {
    TelltaleState* s = state();
    if (s->test(TelltaleState::is_enabled)) {
	s->set(TelltaleState::is_visible, true);
    }
}

void Button::leave() {
    TelltaleState* s = state();
    if (s->test(TelltaleState::is_enabled)) {
	s->set(TelltaleState::is_visible, false);
    }
}

void Button::press(const Event&) {
    TelltaleState* s = state();
    if (s->test(TelltaleState::is_enabled)) {
	s->set(TelltaleState::is_active, true);
    }
}

void Button::release(const Event& e) {
    TelltaleState* s = state();
    if (s->test(TelltaleState::is_enabled)) {
	s->set(TelltaleState::is_active, false);
	if (inside(e)) {
	    boolean chosen = s->test(TelltaleState::is_chosen);
	    boolean act = !chosen;
	    if (s->test(TelltaleState::is_toggle)) {
		s->set(TelltaleState::is_chosen, act);
		act = true;
	    } else if (s->test(TelltaleState::is_choosable)) {
		s->set(TelltaleState::is_chosen, true);
	    }
	    if (act) {
		Action* a = action();
		if (a != nil) {
		    s->set(TelltaleState::is_running, true);
		    a->execute();
		    s->set(TelltaleState::is_running, false);
		}
	    }
	}
    }
}

void Button::update(Observable*) {
    redraw();
}
