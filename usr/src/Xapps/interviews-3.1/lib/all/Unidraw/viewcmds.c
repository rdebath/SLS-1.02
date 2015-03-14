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
 * Implementation of view commands.
 */

#include <Unidraw/classes.h>
#include <Unidraw/dialogs.h>
#include <Unidraw/editor.h>
#include <Unidraw/grid.h>
#include <Unidraw/iterator.h>
#include <Unidraw/statevars.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/catcmds.h>
#include <Unidraw/Commands/viewcmds.h>

#include <Unidraw/Components/component.h>

/*****************************************************************************/

ClassId NormSizeCmd::GetClassId () { return NORMSIZE_CMD; }

boolean NormSizeCmd::IsA (ClassId id) {
    return NORMSIZE_CMD == id || Command::IsA(id);
}

NormSizeCmd::NormSizeCmd (ControlInfo* c) : Command(c) { }
NormSizeCmd::NormSizeCmd (Editor* ed) : Command(ed) { }

Command* NormSizeCmd::Copy () {
    Command* copy = new NormSizeCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void NormSizeCmd::Execute () {
    Viewer* viewer;
    
    for (int i = 0; (viewer = GetEditor()->GetViewer(i)) != nil; ++i) {
        viewer->SetMagnification(1.0);
    }
}

boolean NormSizeCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId RedToFitCmd::GetClassId () { return REDTOFIT_CMD; }

boolean RedToFitCmd::IsA (ClassId id) {
    return REDTOFIT_CMD == id || Command::IsA(id);
}

RedToFitCmd::RedToFitCmd (ControlInfo* c) : Command(c) { }
RedToFitCmd::RedToFitCmd (Editor* ed) : Command(ed) { }

Command* RedToFitCmd::Copy () {
    Command* copy = new RedToFitCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void RedToFitCmd::Execute () {
    Viewer* viewer;
    
    for (int i = 0; (viewer = GetEditor()->GetViewer(i)) != nil; ++i) {
        viewer->ReduceToFit();
    }
}

boolean RedToFitCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId CenterCmd::GetClassId () { return CENTER_CMD; }

boolean CenterCmd::IsA (ClassId id) {
    return CENTER_CMD == id || Command::IsA(id);
}

CenterCmd::CenterCmd (ControlInfo* c) : Command(c) { }
CenterCmd::CenterCmd (Editor* ed) : Command(ed) { }

Command* CenterCmd::Copy () {
    Command* copy = new CenterCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void CenterCmd::Execute () {
    Viewer* viewer;
    
    for (int i = 0; (viewer = GetEditor()->GetViewer(i)) != nil; ++i) {
        viewer->CenterOp();
    }
}

boolean CenterCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId GridCmd::GetClassId () { return GRID_CMD; }

boolean GridCmd::IsA (ClassId id) {
    return GRID_CMD == id || Command::IsA(id);
}

GridCmd::GridCmd (ControlInfo* c) : Command(c) { }
GridCmd::GridCmd (Editor* ed) : Command(ed) { }

Command* GridCmd::Copy () {
    Command* copy = new GridCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void GridCmd::Execute () {
    Viewer* viewer;

    for (int i = 0; (viewer = GetEditor()->GetViewer(i)) != nil; ++i) {
	Grid* grid = viewer->GetGrid();

	if (grid != nil) grid->Visibility(!grid->IsVisible());
	viewer->Draw();
    }
}

boolean GridCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId GridSpacingCmd::GetClassId () { return GRIDSPACING_CMD; }

boolean GridSpacingCmd::IsA (ClassId id) {
    return GRIDSPACING_CMD == id || Command::IsA(id);
}

GridSpacingCmd::GridSpacingCmd (ControlInfo* c) : Command(c) { _dialog = nil; }
GridSpacingCmd::GridSpacingCmd (Editor* ed) : Command(ed) { _dialog = nil; }
GridSpacingCmd::~GridSpacingCmd () { delete _dialog; }

Command* GridSpacingCmd::Copy () {
    Command* copy = new GridSpacingCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void GridSpacingCmd::Execute () {
    float xincr, yincr;
    Editor* ed = GetEditor();

    if (_dialog == nil) {
        _dialog = new GridDialog();
    }

    ed->InsertDialog(_dialog);
    boolean accepted = _dialog->Accept();
    ed->RemoveDialog(_dialog);

    if (accepted) {
        _dialog->GetValues(xincr, yincr);

        if (xincr != 0.0 && yincr != 0.0) {
	    Viewer* viewer;

	    for (int i = 0; (viewer = ed->GetViewer(i)) != nil; ++i) {
		viewer->GetGrid()->SetSpacing(xincr, yincr);
		viewer->Draw();
	    }
        }
    }
}

boolean GridSpacingCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId GravityCmd::GetClassId () { return GRAVITY_CMD; }

boolean GravityCmd::IsA (ClassId id) {
    return GRAVITY_CMD == id || Command::IsA(id);
}

GravityCmd::GravityCmd (ControlInfo* c) : Command(c) { }
GravityCmd::GravityCmd (Editor* ed) : Command(ed) { }

Command* GravityCmd::Copy () {
    Command* copy = new GravityCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void GravityCmd::Execute () {
    GravityVar* gravity = (GravityVar*) GetEditor()->GetState("GravityVar");

    if (gravity != nil) {
        gravity->Activate(!gravity->IsActive());
    }
}

boolean GravityCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId OrientationCmd::GetClassId () { return ORIENTATION_CMD; }

boolean OrientationCmd::IsA (ClassId id) {
    return ORIENTATION_CMD == id || Command::IsA(id);
}

OrientationCmd::OrientationCmd (ControlInfo* c) : Command(c) { }
OrientationCmd::OrientationCmd (Editor* ed) : Command(ed) { }

Command* OrientationCmd::Copy () {
    Command* copy = new OrientationCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void OrientationCmd::Execute () {
    Viewer* viewer;

    for (int i = 0; (viewer = GetEditor()->GetViewer(i)) != nil; ++i) {
	Orientation o = viewer->GetOrientation();
	Orientation newOrient = 
	    (o == Normal || o == Portrait || o == Vertical) ? Rotated : Normal;

	viewer->SetOrientation(newOrient);
    }
}

boolean OrientationCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId CloseEditorCmd::GetClassId () { return CLOSEEDITOR_CMD; }

boolean CloseEditorCmd::IsA (ClassId id) {
    return CLOSEEDITOR_CMD == id || Command::IsA(id);
}

CloseEditorCmd::CloseEditorCmd (ControlInfo* c) : Command(c) { }
CloseEditorCmd::CloseEditorCmd (Editor* ed) : Command(ed) { }

Command* CloseEditorCmd::Copy () {
    Command* copy = new CloseEditorCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

static boolean FoundAnyExcept (Editor* ed) {
    Component* comp = ed->GetComponent()->GetRoot();
    Iterator i;

    for (unidraw->First(i); !unidraw->Done(i); unidraw->Next(i)) {
        Editor* test_ed = unidraw->GetEditor(i);

        if (test_ed != ed) {
            Component* test_comp = test_ed->GetComponent();

            if (test_comp != nil && test_comp->GetRoot() == comp) {
                return true;
            }
        }
    }
    return false;
}

void CloseEditorCmd::Execute () {
    Editor* ed = GetEditor();
    Iterator i;
    unidraw->First(i);
    unidraw->Next(i);

    if (!unidraw->Done(i)) {
	ModifStatusVar* mv = (ModifStatusVar*) ed->GetState("ModifStatusVar");

	if (mv != nil && mv->GetModifStatus() && !FoundAnyExcept(ed)) {
            ConfirmDialog dialog("Save changes?");

	    ed->InsertDialog(&dialog);
	    char resp = dialog.Confirm();
	    ed->RemoveDialog(&dialog);

	    if (resp == '\007') {
		return;

            } else if (resp == 'y') {
		SaveCompCmd saveComp(ed);
		saveComp.Execute();

                if (mv->GetModifStatus()) {
                    return;                         // save dialog was aborted
                }
	    }
	}
        unidraw->Close(ed);
    }
}

boolean CloseEditorCmd::Reversible () { return false; }
