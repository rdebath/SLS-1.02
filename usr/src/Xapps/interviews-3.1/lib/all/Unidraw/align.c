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
 * Alignment command implmentation.
 */

#include <Unidraw/classes.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/grid.h>
#include <Unidraw/iterator.h>
#include <Unidraw/selection.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/align.h>
#include <Unidraw/Commands/datas.h>
#include <Unidraw/Commands/transforms.h>

#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>

#include <Unidraw/Graphic/graphic.h>

#include <InterViews/transformer.h>

#include <stream.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

ClassId AlignCmd::GetClassId () { return ALIGN_CMD; }

boolean AlignCmd::IsA (ClassId id) {
    return ALIGN_CMD == id || Command::IsA(id);
}

AlignCmd::AlignCmd (ControlInfo* c, Alignment a1, Alignment a2) : Command(c) {
    _align1 = a1;
    _align2 = a2;
}

AlignCmd::AlignCmd (Editor* ed, Alignment a1, Alignment a2) : Command(ed) {
    _align1 = a1;
    _align2 = a2;
}

Command* AlignCmd::Copy () {
    Command* copy = new AlignCmd(CopyControlInfo(), _align1, _align2);
    InitCopy(copy);
    return copy;
}

GraphicComp* AlignCmd::GetReference (GraphicComp* grcomp) {
    Clipboard* cb = GetClipboard();
    Iterator i;

    cb->SetComp(grcomp, i);
    cb->Prev(i);

    if (cb->GetComp(i) == nil) {
        cb->First(i);
    }
    return cb->GetComp(i);
}

void AlignCmd::GetAlignment (Alignment& a1, Alignment& a2) {
    a1 = _align1;
    a2 = _align2;
}

void AlignCmd::Read (istream& in) {
    Command::Read(in);
    unsigned int align1, align2;
    in >> align1 >> align2;
    _align1 = align1;
    _align2 = align2;
}

void AlignCmd::Write (ostream& out) {
    Command::Write(out);
    out << _align1 << " " << _align2 << " ";
}

/*****************************************************************************/

ClassId AlignToGridCmd::GetClassId () { return ALIGNTOGRID_CMD; }

boolean AlignToGridCmd::IsA (ClassId id) {
    return ALIGNTOGRID_CMD == id || Command::IsA(id);
}

AlignToGridCmd::AlignToGridCmd (ControlInfo* c) : Command(c) { }
AlignToGridCmd::AlignToGridCmd (Editor* ed) : Command(ed) { }

Command* AlignToGridCmd::Copy () {
    Command* copy = new AlignToGridCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void AlignToGridCmd::Execute () {
    Selection* s = _editor->GetSelection();

    if (!s->IsEmpty()) {
        Clipboard* cb = GetClipboard();
        Iterator i;

        if (cb == nil) {
            for (s->First(i); !s->Done(i); s->Next(i)) {
                s->GetView(i)->Interpret(this);
            }

            SetClipboard(cb = new Clipboard);
            cb->Init(s);

        } else {
            for (cb->First(i); !cb->Done(i); cb->Next(i)) {
                Move(cb->GetComp(i));
            }
        }
        unidraw->Update();
    }
}

void AlignToGridCmd::Unexecute () {
    Clipboard* cb = GetClipboard();

    if (cb != nil) {
        Iterator i;

        for (cb->Last(i); !cb->Done(i); cb->Prev(i)) {
            Unmove(cb->GetComp(i));
        }
        unidraw->Update();
    }
}

void AlignToGridCmd::Align (GraphicView* gv, float refx, float refy) {
    MoveData* md = (MoveData*) Recall(gv->GetGraphicComp());

    if (md == nil) {
        Viewer* v = gv->GetViewer();
        Grid* grid = (v == nil) ? nil : v->GetGrid();

        if (grid == nil) {
            return;
        }

        Graphic* g = gv->GetGraphic();
        Transformer t;
        g->Parent()->TotalTransformation(t);
        t.Invert();

        Coord cx = round(refx);
        Coord cy = round(refy);

        grid->Constrain(cx, cy);

        float dx, dy, trefx, trefy;

        t.Transform(float(cx), float(cy), dx, dy);
        t.Transform(refx, refy, trefx, trefy);

        dx -= trefx;
        dy -= trefy;
        Store(gv->GetGraphicComp(), new MoveData(dx, dy));

    }
    Move(gv->GetGraphicComp());
}
    
void AlignToGridCmd::Unalign (GraphicView* gv) { Unmove(gv->GetGraphicComp());}

void AlignToGridCmd::Move (GraphicComp* gc) {
    MoveData* md = (MoveData*) Recall(gc);

    if (md != nil) {
        MoveCmd movement(GetEditor(), md->_dx, md->_dy);
        gc->Interpret(&movement);
    }
}

void AlignToGridCmd::Unmove (GraphicComp* gc) {
    MoveData* md = (MoveData*) Recall(gc);

    if (md != nil) {
        MoveCmd movement(GetEditor(), md->_dx, md->_dy);
        gc->Uninterpret(&movement);
    }
}
