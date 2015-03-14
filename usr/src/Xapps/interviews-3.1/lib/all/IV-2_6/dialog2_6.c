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
 * Dialog class implementation.
 */

#include <InterViews/event.h>
#include <IV-2_6/InterViews/button.h>
#include <IV-2_6/InterViews/dialog.h>
#include <IV-2_6/InterViews/shape.h>
#include <IV-2_6/InterViews/world.h>

#include <IV-2_6/_enter.h>

Dialog::Dialog(ButtonState* b, Interactor* i, Alignment a) {
    Init(b, i, a);
}

Dialog::Dialog(const char* name, ButtonState* b, Interactor* i, Alignment a) {
    SetInstance(name);
    Init(b, i, a);
}

void Dialog::Init(ButtonState* b, Interactor* i, Alignment a) {
    SetClassName("Dialog");
    state = b;
    Resource::ref(state);
    align = a;
    if (i != nil) {
	Insert(i);
    }
}

Dialog::~Dialog() {
    Resource::unref(state);
}

boolean Dialog::Popup(Event& e, boolean placed) {
    World* w;
    IntCoord wx, wy;
    boolean accept;

    e.GetAbsolute(w, wx, wy);
    if (placed) {
	w->InsertTransient(this, e.target, wx, wy, align);
    } else {
	w->InsertTransient(this, e.target);
    }
    accept = Accept();
    w->Remove(this);
    return accept;
}

boolean Dialog::Accept() {
    Event e;
    int v;

    state->SetValue(0);
    v = 0;
    do {
	Read(e);
	e.target->Handle(e);
	state->GetValue(v);
    } while (v == 0 && e.target != nil);
    return v == 1 || e.target == nil;
}

int Dialog::Status() {
    int v;

    state->GetValue(v);
    return v;
}
