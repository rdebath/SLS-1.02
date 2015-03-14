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
 * Implementation of UControl and UControlInteractor subclasses.
 */

#include <Unidraw/classes.h>
#include <Unidraw/ctrlinfo.h>
#include <Unidraw/globals.h>
#include <Unidraw/iterator.h>
#include <Unidraw/uctrls.h>
#include <Unidraw/Commands/command.h>
#include <Unidraw/Graphic/ulabel.h>
#include <Unidraw/Graphic/picture.h>
#include <Unidraw/Graphic/polygons.h>

#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/sensor.h>
#include <IV-2_6/InterViews/shape.h>
#include <IV-2_6/InterViews/world.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

static const int HPAD = 4;          // horizontal padding around control labels
static const int VPAD = 1;          // vertical padding around control labels
static const int SEP = 8;           // separation between label & equiv
static const int MINHT = 15;        // minimum height of control labels
static const int MINWD = 15;        // minimum width of control labels

/*****************************************************************************/

CommandControl::CommandControl (ControlInfo* info) : UControl(info) {
    Init(info);
}

CommandControl::CommandControl (
    const char* name, ControlInfo* info
) : UControl(name, info) { 
    Init(info);
}

void CommandControl::Init (ControlInfo* info) {
    Insert(new CommandInteractor(info));
}

void CommandControl::Do () {
    Command* cmd = (Command*) _info->GetOwner();

    if (cmd != nil) {
        Busy();

        if (cmd->Reversible()) {
            cmd = cmd->Copy();
            cmd->Execute();

            if (cmd->Reversible()) {
                cmd->Log();
            } else {
                delete cmd;
            }

        } else {
            cmd->Execute();

            if (cmd->Reversible()) {
                cmd = cmd->Copy();
                cmd->Log();
            }
        }

        Done();
    }
}

void CommandControl::Busy () {
    RootControl()->Highlight(true);
    World::current()->Sync();
}

void CommandControl::Done () {
    RootControl()->Highlight(false);
    World::current()->Sync();
}

/*****************************************************************************/

PanelControl::PanelControl (
    Interactor* i, ControlInfo* info, ControlState* state
) : UControl(info) {
    Init(i, state);
}

PanelControl::PanelControl (
    const char* name, Interactor* i, ControlInfo* info, ControlState* state
) : UControl(name, info) {
    Init(i, state);
}

void PanelControl::Init (Interactor* i, ControlState* state) {
    if (i != nil) {
        Insert(i);
    }

    if (state != nil) {
	SetState(state);
    }
}

void PanelControl::Do () { Down(); }
void PanelControl::Down () { State()->NotifySelection(this); }
void PanelControl::Enter () { }
void PanelControl::Leave () { }
void PanelControl::Select () { Highlight(true); }

/*****************************************************************************/

HPanelControl::HPanelControl (
    ControlInfo* info, ControlState* state
) : PanelControl(new PanelInteractor(info, Horizontal), info, state) { }

HPanelControl::HPanelControl (
    const char* name, ControlInfo* info, ControlState* state
) : PanelControl(name, new PanelInteractor(info, Horizontal), info, state) { }

/*****************************************************************************/

VPanelControl::VPanelControl (
    ControlInfo* info, ControlState* state
) : PanelControl(new PanelInteractor(info, Vertical), info, state) { }

VPanelControl::VPanelControl (
    const char* name, ControlInfo* info, ControlState* state
) : PanelControl(name, new PanelInteractor(info, Vertical), info, state) { }

/*****************************************************************************/

CommandInteractor::CommandInteractor (
    ControlInfo* info
) : UControlInteractor(info) { }

void CommandInteractor::Reconfig () {
    const char* kl = _info->GetKeyLabel();
    int x0, y0, x1, y1;

    _label->GetBox(x0, y0, x1, y1);
    shape->width = 2*HPAD + x1 - x0;
    shape->height = max(2*VPAD + y1 - y0, MINHT);

    if (*kl != '\0') {
	Font* f = stdgraphic->GetFont();
	shape->width += f->Width(kl) + SEP;
	shape->height = max(shape->height, f->Height() + 2*VPAD);
    }
    shape->Rigid(shape->width, hfil, 0, 0);
}

void CommandInteractor::Resize () {
    const char* keyLabel = _info->GetKeyLabel();
    Graphic* bg;
    boolean invert = false;

    Iterator i;
    _picture->First(i);

    if (_picture->Done(i)) {
        bg = new F_Rect(0, 0, xmax, ymax, stdgraphic);
        invert = _highlighted;

    } else {
        bg = new F_Rect(0, 0, xmax, ymax, _picture->GetGraphic(i));
        Graphic* newpict = new Picture(_picture);

        _picture->Remove(_label);
        delete _picture;
        _picture = newpict;
    }

    bg->SetPattern(psclear);
    _picture->Append(bg);
    _picture->Append(_label);

    if (*keyLabel == '\0') {
        bg->Align(Center, _label, Center);

    } else {
        bg->Align(CenterLeft, _label, CenterLeft);
        _label->Translate(HPAD, 0);

	Graphic* klbl = new ULabel(keyLabel, bg);
        klbl->SetFont(psstdfont);
	_picture->Append(klbl);
	bg->Align(CenterRight, klbl, CenterRight);
	klbl->Translate(-HPAD, 0);
    }    
    if (invert) {
        Invert();
    }
}

/*****************************************************************************/

PanelInteractor::PanelInteractor (
    ControlInfo* info, Orientation orient
) : UControlInteractor(info) {
    _orient = orient;
}

void PanelInteractor::Reconfig () {
    int x0, y0, x1, y1;
    const char* kl = _info->GetKeyLabel();

    _label->GetBox(x0, y0, x1, y1);
    shape->width = x1 - x0;
    shape->height = y1 - y0;

    if (*kl == '\0') {
        shape->width += 2*HPAD;
        shape->height += 2*VPAD;

    } else {
	PSFont* f = stdgraphic->GetFont();
	shape->width += 2 * f->Width(kl) + HPAD;
	shape->height += f->Height();
    }
    shape->height = max(shape->height, MINHT);

    if (_orient == Horizontal) {
        shape->Rigid(0, shape->width, 0, vfil);

    } else if (_orient == Vertical) {
        shape->Rigid(0, hfil, shape->height, 0);
    }        
}

void PanelInteractor::Resize () {
    const char* keyLabel = _info->GetKeyLabel();
    Graphic* bg;
    boolean invert = false;
    Iterator i;
    _picture->First(i);

    if (_picture->Done(i)) {
        bg = new F_Rect(0, 0, xmax, ymax, stdgraphic);
        invert = _highlighted;

    } else {
        bg = new F_Rect(0, 0, xmax, ymax, _picture->GetGraphic(i));
        Graphic* newpict = new Picture(_picture);
        
        _picture->Remove(_label);
        delete _picture;
        _picture = newpict;
    }

    bg->SetPattern(psclear);
    _picture->Append(bg);
    _picture->Append(_label);
    bg->Align(Center, _label, Center);

    if (*keyLabel != '\0') {
	Graphic* klbl = new ULabel(keyLabel, bg);
        klbl->SetFont(psstdfont);
	_picture->Append(klbl);
	_picture->Align(BottomRight, klbl, BottomRight);
    }    
    if (invert) {
        Invert();
    }
}
