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
 * Implementation of control classes
 */

#include <IV-2_6/InterViews/control.h>
#include <IV-2_6/InterViews/message.h>
#include <IV-2_6/InterViews/sensor.h>

/** class Control **/

Control::Control(Interactor* i) { Init(nil, i); }
Control::Control(const char* name, Interactor* i) { Init(name, i); }

void Control::Init(const char* name, Interactor* i) {
    if (name != nil) {
	SetInstance(name);
    }
    enabled_ = true;
    parent_ = nil;
    state_ = new ControlState;
    state_->Attach(this);
    input = new Sensor;
    input->Catch(EnterEvent);
    input->Catch(LeaveEvent);
    input->Catch(DownEvent);
    input->Catch(UpEvent);
    if (i != nil) {
	Insert(i);
    }
}

Control::~Control () {
    state_->Detach(this);
}

void Control::Handle(Event& e) {
    switch (e.eventType) {
    case DownEvent:
	Down();
	break;
    case UpEvent:
	Up();
	break;
    case EnterEvent:
	Enter();
	break;
    case LeaveEvent:
	Leave();
	break;
    }
}

void Control::Enable(boolean b) {
    if (b != enabled_) {
	enabled_ = b;
    }
}

/*
 * Hitting a control is equivalent to making the state active and
 * then selecting it.
 */

void Control::Down() {
    if (Enabled() && !state_->Active()) {
	state_->Activate();
	state_->NotifySelection(this);
    }
}

void Control::Enter() {
    if (Enabled() && state_->Active()) {
	state_->NotifySelection(this);
    }
}

void Control::Leave() {
    if (Enabled() && state_->Active()) {
	state_->NotifySelection(nil);
    }
}

void Control::Select() {
    Highlight(true);
    Open();
    Grab();
}

void Control::Unselect() {
    Close();
    Highlight(false);
}

void Control::Grab() {
    Event e;
    do {
	Read(e);
	e.target->Handle(e);
	if (e.target == this && e.eventType == LeaveEvent) {
	    Skip();
	    break;
	}
    } while (state_->Active());
}

/*
 * Skip all input events until we see something for another open control.
 */

void Control::Skip() {
    Event e;
    for (;;) {
	Read(e);
	if (e.eventType == EnterEvent && IsGrabbing(e.target)) {
	    UnRead(e);
	    break;
	} else if (e.eventType == UpEvent) {
	    Up();
	    break;
	}
    }
}

/*
 * Check whether an interactor (usually an event target) is a grabbing
 * control.  Trivially, the current control is grabbing.  Any other controls
 * attached to the control's state are also grabbing.  Similarly, any controls
 * attached to other control's state up the stack from the current one
 * are also considered to be grabbing.
 */

boolean Control::IsGrabbing(Interactor* i) {
    if (i == this) {
	return true;
    }
    for (ControlState* c = state_; c != nil; c = c->Next()) {
	if (c->IsView(i)) {
	    return true;
	}
    }
    for (c = state_->Prev(); c != nil; c = c->Prev()) {
	if (c->IsView(i)) {
	    return true;
	}
    }
    return false;
}

/*
 * On an up event, deactivate all the control states and then
 * call Do the current selection (if any).
 */

void Control::Up() {
    if (state_->Active()) {
	Control* target = state_->Selection();
	state_->Action(target);
	for (ControlState* c = state_; c != nil; c = c->Prev()) {
	    c->Deactivate();
	}
	if (target != nil) {
	    Busy();
	    target->Do();
	    Done();
	}
    }
}

void Control::Open() { }
void Control::Close() { }
void Control::Do() { }
void Control::Busy() { }
void Control::Done() { }

Control* Control::RootControl() {
    Control* root = this;
    Control* parent = ParentControl();

    while (parent != nil) {
	root = parent;
	parent = parent->ParentControl();
    }
    return root;
}

void Control::SetState(ControlState* s) {
    state_->Detach(this);
    state_ = s;
    s->Attach(this);
}

void Control::Reparent(Control* ctrl, Control* parent) {
    ctrl->parent_ = parent;
}

/** class ControlState **/

ControlState::ControlState(unsigned s) {
    status = s;
    selection = nil;
    action = nil;
    next = nil;
    prev = nil;
}

ControlState::~ControlState() { }

void ControlState::NotifySelection(Control* c) {
    if (selection != c) {
	if (selection != nil) {
	    selection->Unselect();
	}
	selection = c;
	if (selection != nil) {
	    selection->Select();
	}
    }
}

void ControlState::Push(ControlState* s) {
    next = s;
    s->prev = this;
}

void ControlState::Pop() {
    if (prev != nil) {
	prev->next = next;
	prev = nil;
    }
    Deactivate();
}

void ControlState::Deactivate() {
    Set(ControlActive, false);
    if (selection != nil) {
	selection->Unselect();
	selection = nil;
    }
}
