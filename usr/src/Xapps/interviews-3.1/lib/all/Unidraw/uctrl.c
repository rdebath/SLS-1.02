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
 * UControl and UControlInteractor implementations.
 */

#include <Unidraw/classes.h>
#include <Unidraw/ctrlinfo.h>
#include <Unidraw/iterator.h>
#include <Unidraw/uctrl.h>
#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Graphic/picture.h>

#include <IV-2_6/InterViews/button.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

UControl::UControl (ControlInfo* info) : Control(nil) { Init(info); }

UControl::UControl (const char* name, ControlInfo* info) : Control(name, nil) {
    Init(info);
}

void UControl::Init (ControlInfo* info) { _info = info; }
void UControl::SetControlInfo (ControlInfo* c) { _info = c; }
ControlInfo* UControl::GetControlInfo () { return _info; }

/*****************************************************************************/

UControlInteractor::UControlInteractor (ControlInfo* info) {
    Init(info);
    _label = InitLabel(_info);
}

UControlInteractor::UControlInteractor () {
    Init(nil);
    _label = nil;
}

UControlInteractor::~UControlInteractor () { delete _picture; }

void UControlInteractor::Init (ControlInfo* info) {
    _info = info;
    _picture = new Picture;
    _highlighted = false;
}

void UControlInteractor::Redraw (Coord, Coord, Coord, Coord) {
    if (canvas != nil) {
        _picture->Draw(canvas);
    }
}

void UControlInteractor::Highlight (boolean on) {
    if (_highlighted != on) {
        Invert();
        _highlighted = on;
        Draw();
    }
}

static void InvertGraphic (Graphic* g) {
    Iterator i;

    PSColor* fg = g->GetFgColor();
    PSColor* bg = g->GetBgColor();
    g->SetColors(bg, fg);

    for (g->First(i); !g->Done(i); g->Next(i)) {
        InvertGraphic(g->GetGraphic(i));
    }
}

void UControlInteractor::Invert () { InvertGraphic(_picture); }
void UControlInteractor::SetControlInfo (ControlInfo* c) { _info = c; }
ControlInfo* UControlInteractor::GetControlInfo () { return _info; }

Graphic* UControlInteractor::InitLabel (ControlInfo* info) {
    GraphicComp* comp = info->GetLabel();
    GraphicView* view = (GraphicView*) comp->Create(COMPONENT_VIEW);
    comp->Attach(view);
    view->Update();

    Graphic* label = view->GetGraphic()->Copy();
    delete view;
    return label;
}
