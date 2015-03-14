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
 * Implementation of idraw-specific commands.
 */

#include "ided.h"
#include "idclasses.h"
#include "idcmds.h"
#include "idcomp.h"
#include "iddialogs.h"
#include "idvars.h"
#include "idversion.h"

#include <Unidraw/dialogs.h>
#include <Unidraw/catalog.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/globals.h>
#include <Unidraw/grid.h>
#include <Unidraw/iterator.h>
#include <Unidraw/statevars.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/transforms.h>

#undef FileChooser
#define FileChooser _lib_iv(FileChooser)

#include <stream.h>

/*****************************************************************************/

ClassId OpenCmd::GetClassId () { return OPEN_CMD; }

boolean OpenCmd::IsA (ClassId id) {
    return OPEN_CMD == id || ViewCompCmd::IsA(id);
}

OpenCmd::OpenCmd (ControlInfo* c, FileChooser* fc) : ViewCompCmd(c, fc) { }
OpenCmd::OpenCmd (Editor* ed, FileChooser* fc) : ViewCompCmd(ed, fc) { }

Command* OpenCmd::Copy () {
    Command* copy = new OpenCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void OpenCmd::Execute () {
    Editor* ed = GetEditor();
    Component* orig_comp = ed->GetComponent();
    ViewCompCmd::Execute();
    Component* new_comp = ed->GetComponent();

    if (new_comp != orig_comp) {
        IdrawComp* idcomp = (IdrawComp*) new_comp;
        Grid* grid = ed->GetViewer()->GetGrid();
        
        float xincr, yincr;
        idcomp->GetGridSpacing(xincr, yincr);
        grid->SetSpacing(xincr, yincr);
    }
}

/*****************************************************************************/

ClassId PreciseMoveCmd::GetClassId () { return PRECISEMOVE_CMD; }

boolean PreciseMoveCmd::IsA (ClassId id) {
    return PRECISEMOVE_CMD == id || Command::IsA(id);
}

PreciseMoveCmd::PreciseMoveCmd (ControlInfo* c) : Command(c) { _dialog = nil; }
PreciseMoveCmd::PreciseMoveCmd (Editor* ed) : Command(ed) { _dialog = nil; }
PreciseMoveCmd::~PreciseMoveCmd () { delete _dialog; }

Command* PreciseMoveCmd::Copy () {
    Command* copy = new PreciseMoveCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void PreciseMoveCmd::Execute () {
    float dx = 0.0, dy = 0.0;
    Editor* ed = GetEditor();

    if (_dialog == nil) {
	_dialog = new MoveDialog();
    }

    ed->InsertDialog(_dialog);
    boolean accepted = _dialog->Accept();
    ed->RemoveDialog(_dialog);

    if (accepted) {
	_dialog->GetValues(dx, dy);

	if (dx != 0.0 || dy != 0.0) {
	    MoveCmd* moveCmd = new MoveCmd(ed, dx, dy);
	    moveCmd->Execute();
	    moveCmd->Log();
	}
    }
}

boolean PreciseMoveCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId PreciseScaleCmd::GetClassId () { return PRECISESCALE_CMD; }

boolean PreciseScaleCmd::IsA (ClassId id) {
    return PRECISESCALE_CMD == id || Command::IsA(id);
}

PreciseScaleCmd::PreciseScaleCmd (ControlInfo* c) : Command(c) {_dialog = nil;}
PreciseScaleCmd::PreciseScaleCmd (Editor* ed) : Command(ed) { _dialog = nil; }
PreciseScaleCmd::~PreciseScaleCmd () { delete _dialog; }

Command* PreciseScaleCmd::Copy () {
    Command* copy = new PreciseScaleCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void PreciseScaleCmd::Execute () {
    float x = 0.0, y = 0.0;
    Editor* ed = GetEditor();

    if (_dialog == nil) {
	_dialog = new ScaleDialog();
    }

    ed->InsertDialog(_dialog);
    boolean accepted = _dialog->Accept();
    ed->RemoveDialog(_dialog);

    if (accepted) {
	_dialog->GetValues(x, y);
	if (x != 0.0 && y != 0.0) {
	    ScaleCmd* scaleCmd = new ScaleCmd(ed, x, y);
	    scaleCmd->Execute();
	    scaleCmd->Log();
	}
    }
}

boolean PreciseScaleCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId PreciseRotateCmd::GetClassId () { return PRECISEROTATE_CMD; }

boolean PreciseRotateCmd::IsA (ClassId id) {
    return PRECISEROTATE_CMD == id || Command::IsA(id);
}

PreciseRotateCmd::PreciseRotateCmd (ControlInfo* c) : Command(c) {_dialog=nil;}
PreciseRotateCmd::PreciseRotateCmd (Editor* ed) : Command(ed) { _dialog = nil;}
PreciseRotateCmd::~PreciseRotateCmd () { delete _dialog; }

Command* PreciseRotateCmd::Copy () {
    Command* copy = new PreciseRotateCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void PreciseRotateCmd::Execute () {
    float angle = 0.0;
    Editor* ed = GetEditor();

    if (_dialog == nil) {
	_dialog = new RotateDialog();
    }

    ed->InsertDialog(_dialog);
    boolean accepted = _dialog->Accept();
    ed->RemoveDialog(_dialog);

    if (accepted) {
	_dialog->GetValue(angle);
	if (angle != 0.0) {
	    RotateCmd* rotateCmd = new RotateCmd(ed, angle);
	    rotateCmd->Execute();
	    rotateCmd->Log();
	}
    }
}

boolean PreciseRotateCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId NewViewCmd::GetClassId () { return NEWVIEW_CMD; }

boolean NewViewCmd::IsA (ClassId id) {
    return NEWVIEW_CMD == id || Command::IsA(id);
}

NewViewCmd::NewViewCmd (ControlInfo* c) : Command(c) { }
NewViewCmd::NewViewCmd (Editor* ed) : Command(ed) { }

Command* NewViewCmd::Copy () {
    Command* copy = new NewViewCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void NewViewCmd::Execute () {
    Editor* ed = GetEditor();
    Editor* newEd = new IdrawEditor(GetGraphicComp());

    *newEd->GetState("ModifStatusVar") = *ed->GetState("ModifStatusVar");

    unidraw->Open(newEd);
}

boolean NewViewCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId ArrowCmd::GetClassId () { return ARROW_CMD; }
boolean ArrowCmd::IsA (ClassId id) {return ARROW_CMD==id || Command::IsA(id);}

ArrowCmd::ArrowCmd (ControlInfo* c, boolean head, boolean tail) : Command(c) {
    _head = head;
    _tail = tail;
}

ArrowCmd::ArrowCmd (Editor* ed, boolean head, boolean tail) : Command(ed) {
    _head = head;
    _tail = tail;
}

Command* ArrowCmd::Copy () {
    Command* copy = new ArrowCmd(CopyControlInfo(), Head(), Tail());
    InitCopy(copy);
    return copy;
}

void ArrowCmd::Execute () {
    ArrowVar* arrowVar	= (ArrowVar*) GetEditor()->GetState("ArrowVar");

    if (arrowVar != nil) {
        arrowVar->SetArrows(_head, _tail);
    }
    Command::Execute();
}

/*****************************************************************************/

ClassId AboutCmd::GetClassId () { return ABOUT_CMD; }
boolean AboutCmd::IsA (ClassId id) {return ABOUT_CMD==id || Command::IsA(id);}

AboutCmd::AboutCmd (ControlInfo* c) : Command(c) { }
AboutCmd::AboutCmd (Editor* ed) : Command(ed) { }

Command* AboutCmd::Copy () {
    Command* copy = new AboutCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void AboutCmd::Execute () {
    Editor* ed = GetEditor();
    AcknowledgeDialog dialog(VERSION);

    ed->InsertDialog(&dialog);
    dialog.Acknowledge();
    ed->RemoveDialog(&dialog);
}

boolean AboutCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId IGridSpacingCmd::GetClassId () { return IGRIDSPACING_CMD; }

boolean IGridSpacingCmd::IsA (ClassId id) {
    return IGRIDSPACING_CMD == id || GridSpacingCmd::IsA(id);
}

IGridSpacingCmd::IGridSpacingCmd (ControlInfo* c) : GridSpacingCmd(c) { }
IGridSpacingCmd::IGridSpacingCmd (Editor* ed) : GridSpacingCmd(ed) { }

Command* IGridSpacingCmd::Copy () {
    Command* copy = new IGridSpacingCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void IGridSpacingCmd::Execute () {
    GridSpacingCmd::Execute();
    float xincr, yincr;
    _dialog->GetValues(xincr, yincr);

    IdrawComp* idcomp = (IdrawComp*) GetEditor()->GetComponent()->GetRoot();
    idcomp->SetGridSpacing(xincr, yincr);
}
