/*
 * Copyright (c) 1990, 1991 Stanford University
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
 * ControlInfo implementation.
 */

#include <Unidraw/ctrlinfo.h>
#include <Unidraw/globals.h>
#include <Unidraw/uctrl.h>
#include <Unidraw/Components/text.h>

#include <IV-2_6/_enter.h>

#include <string.h>

/*****************************************************************************/

void ControlInfo::Init (const char* kl, const char* kc, void* o) {
    _label = nil;

    _keyLabel = strnew(kl);
    _keyCode = strnew(kc);

    _owner = o;
}

ControlInfo::ControlInfo (
    GraphicComp* label, const char* kl, const char* kc, void* o
) {
    Init(kl, kc, o);
    _label = label;
}

ControlInfo::ControlInfo (
    const char* lbl, const char* kl, const char* kc, void* o
) {
    Init(kl, kc, o);
    SetLabel(lbl);
}

ControlInfo* ControlInfo::Copy () {
    return new ControlInfo(
        (GraphicComp*) _label->Copy(), _keyLabel, _keyCode, _owner
    );
}

ControlInfo::~ControlInfo () {
    if (_label != nil) {
        delete _label;
    }
    delete _keyLabel;
    delete _keyCode;
}

void ControlInfo::SetLabel (GraphicComp* g) {
    if (_label != nil) {
        delete _label;
    }
    _label = g;
}

void ControlInfo::SetLabel (const char* s) {
    if (_label != nil) {
        delete _label;
    }        
    _label = new TextComp(new TextGraphic(s, stdgraphic));
}

void ControlInfo::SetKeyLabel (const char* kl) {
    delete _keyLabel;
    _keyLabel = strnew(kl);
}

void ControlInfo::SetKeyCode (const char* kc) {
    delete _keyCode;
    _keyCode = strnew(kc);
}
