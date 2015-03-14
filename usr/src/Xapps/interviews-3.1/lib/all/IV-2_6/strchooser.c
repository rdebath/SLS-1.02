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
 * StringChooser implementation.
 */

#include <InterViews/event.h>
#include <IV-2_6/InterViews/button.h>
#include <IV-2_6/InterViews/strbrowser.h>
#include <IV-2_6/InterViews/strchooser.h>
#include <IV-2_6/InterViews/streditor.h>
#include <IV-2_6/InterViews/sensor.h>
#include <IV-2_6/InterViews/world.h>
#include <string.h>

#include <IV-2_6/_enter.h>

StringChooser::StringChooser(
    ButtonState* bs, int r, int c, const char* s, Alignment a
) : Dialog(bs, nil, a) { 
    Init(new StringEditor(bs, s), new StringBrowser(bs, r, c));
}

StringChooser::StringChooser(
    const char* name,
    ButtonState* bs, int r, int c, const char* s, Alignment a
) : Dialog(name, bs, nil, a) {
    Init(new StringEditor(bs, s), new StringBrowser(bs, r, c));
}

StringChooser::StringChooser(
    ButtonState* bs, Alignment a
) : Dialog(bs, nil, a) { }

StringChooser::~StringChooser() { }

void StringChooser::Init(StringEditor* se, StringBrowser* sb) {
    input = new Sensor;
    input->Catch(KeyEvent);
    _sedit = se;
    _browser = sb;
    _focus = _sedit;
}
    
void StringChooser::Select(int index) {
    if (index < 0) {
        _sedit->Select(strlen(_sedit->Text()));
    } else {
        _sedit->Select(index);
    }
}

void StringChooser::Select(int left, int right) {
    _sedit->Select(left, right);
}

void StringChooser::SelectMessage() {
    _sedit->Select(0, strlen(_sedit->Text()));
}

void StringChooser::Message(const char* msg) { _sedit->Message(msg); }
const char* StringChooser::Choice() { return _sedit->Text(); }

void StringChooser::Forward(Event& e) {
    IntCoord x = e.x, y = e.y;

    e.target->GetRelative(x, y, this);
    if (x >= 0 && y >= 0 && x <= xmax && y <= ymax) {
        e.target->Handle(e);
    }
}

boolean StringChooser::Accept() {
    Event e;
    int v = 0;
    _focus = _sedit;
    state->SetValue(v);

    World* world = GetWorld();
    HandleFocus();
    state->GetValue(v);
    while (!world->done() && (v == 0 || v == '\t')) {
        if (v == '\t') {
            UpdateEditor();
            UpdateBrowser();
            SwitchFocus();
            HandleFocus();

        } else {
            Read(e);
            if (e.target != _focus && CanFocus(e.target)) {
                SwitchFocus();
            }
            Forward(e);
        }
        state->GetValue(v);
    }

    boolean accepted = v == '\r';

    if (accepted) {
        UpdateEditor();
        UpdateBrowser();
    }
    return accepted;
}

void StringChooser::Handle(Event& e) {
    _focus->Handle(e);
}

void StringChooser::SwitchFocus() {
    if (_focus == _sedit) {
        _focus = _browser;
        Select();
        state->SetValue(0);

    } else if (_focus == _browser) {
        _focus = _sedit;
        _browser->UnselectAll();
        state->SetValue(0);
    }
}

boolean StringChooser::CanFocus(Interactor* i) {
    return i == _sedit || i == _browser;
}

void StringChooser::HandleFocus() {
    if (_focus == _sedit) {
        _sedit->Edit();
    } else if (_focus == _browser) {
        _browser->Browse();
    }
}

void StringChooser::UpdateEditor() {
    int index = _browser->Selection();

    if (index >= 0) {
        _sedit->Message(_browser->String(index));
        SelectMessage();
    }
}

void StringChooser::UpdateBrowser() {
    _browser->UnselectAll();
}
