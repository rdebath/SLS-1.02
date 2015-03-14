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
 * Implementation of Selection class.
 */

#include <Unidraw/globals.h>
#include <Unidraw/iterator.h>
#include <Unidraw/selection.h>
#include <Unidraw/ulist.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Graphic/picture.h>

#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/rubband.h>

#include <IV-2_6/_enter.h>

#include <stream.h>

/*****************************************************************************/

GraphicView* Selection::View (UList* r) { return (GraphicView*) (*r)(); }
UList* Selection::Elem (Iterator i) { return (UList*) i.GetValue(); }

Selection::Selection (Selection* s) {
    _ulist = new UList;
    _count = 0;

    if (s != nil) {
        Iterator i;

        for (s->First(i); !s->Done(i); s->Next(i)) {
	    Append(GetView(i));
	}
    }
}

Selection::~Selection () { delete _ulist; }

void Selection::Show (Viewer* viewer) {
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        GraphicView* view = GetView(i);

        if (viewer == nil || view->GetViewer() == viewer) {
            view->RedrawHandles();
        }
    }
}

void Selection::Update (Viewer* viewer) {
    Iterator i;
    
    for (First(i); !Done(i); Next(i)) {
        GraphicView* view = GetView(i);

        if (viewer == nil || view->GetViewer() == viewer) {
            view->DrawHandles();
        }
    }
}

void Selection::Hide (Viewer* viewer) {
    Iterator i;
    
    for (First(i); !Done(i); Next(i)) {
        GraphicView* view = GetView(i);

        if (viewer == nil || view->GetViewer() == viewer) {
            view->EraseHandles();
        }
    }
}

void Selection::Init (Viewer* viewer) {
    Iterator i;
    
    for (First(i); !Done(i); Next(i)) {
        GraphicView* view = GetView(i);

        if (viewer == nil || view->GetViewer() == viewer) {
            view->InitHandles();
        }
    }
}

void Selection::Clear (Viewer* viewer) {
    Iterator i;
    First(i);

    while (!Done(i)) {
        GraphicView* view = GetView(i);

        if (viewer == nil || view->GetViewer() == viewer) {
            view->EraseHandles();
        }
        Remove(i);
    }
}

void Selection::Append (GraphicView* v) {
    _ulist->Append(new UList(v));
    ++_count;
}

void Selection::Prepend (GraphicView* v) {
    _ulist->Prepend(new UList(v));
    ++_count;
}

void Selection::InsertAfter (Iterator i, GraphicView* v) {
    Elem(i)->Prepend(new UList(v));
    ++_count;
}

void Selection::InsertBefore (Iterator i, GraphicView* v) {
    Elem(i)->Append(new UList(v));
    ++_count;
}

void Selection::Remove (Iterator& i) {
    UList* doomed = Elem(i);

    Next(i);
    _ulist->Remove(doomed);
    delete doomed;
    --_count;
}	
    
void Selection::Remove (GraphicView* p) {
    UList* temp;

    if ((temp = _ulist->Find(p)) != nil) {
	_ulist->Remove(temp);
        delete temp;
	--_count;
    }
}

GraphicView* Selection::GetView (Iterator i) { return View(Elem(i)); }

void Selection::SetView (GraphicView* gv, Iterator& i) {
    i.SetValue(_ulist->Find(gv));
}

void Selection::First (Iterator& i) { i.SetValue(_ulist->First()); }
void Selection::Last (Iterator& i) { i.SetValue(_ulist->Last()); }
void Selection::Next (Iterator& i) { i.SetValue(Elem(i)->Next()); }
void Selection::Prev (Iterator& i) { i.SetValue(Elem(i)->Prev()); }
boolean Selection::Done (Iterator i) { return Elem(i) == _ulist->End(); }
int Selection::Number () { return _count; }

boolean Selection::Includes (GraphicView* e) {
    return _ulist->Find(e) != nil;
}

void Selection::Sort (GraphicView* views) {
/*
 * Note: this doesn't work if there are views in the selection from more than
 * one GraphicViews.
 */
    Iterator i;
    UList* cur;
    UList* newList = new UList;

    views->First(i);

    while (!views->Done(i) && !_ulist->IsEmpty()) {
        cur = _ulist->First();

        while (cur != _ulist->End()) {
            if (views->GetView(i) == View(cur)) {
                _ulist->Remove(cur);
                newList->Append(cur);
                break;
            } else {
                cur = cur->Next();
            }
        }
        views->Next(i);
    }
    if (!_ulist->IsEmpty()) {
        cerr << "warning: selection contained spurious element(s)\n";
    }
    delete _ulist;
    _ulist = newList;
}

void Selection::Merge (Selection* s) {
    Iterator i;
    GraphicView* gv;

    for (s->First(i); !s->Done(i); s->Next(i)) {
        gv = GetView(i);
	if (!Includes(gv)) {
	    Append(gv);
	}
    }
    Update();
}

void Selection::Exclusive (Selection* s) {
    Iterator i;
    GraphicView* gv;

    for (s->First(i); !s->Done(i); s->Next(i)) {
        gv = GetView(i);

	if (Includes(gv)) {
	    Remove(gv);
            gv->EraseHandles();
	} else {
	    Append(gv);
	}
    }
    Update();
}

void Selection::GetBox (Coord& l, Coord& b, Coord& r, Coord& t) {
    Iterator i;
    Graphic* gr;
    BoxObj btotal, bgraphic;

    First(i);
    gr = GetView(i)->GetGraphic();
    gr->GetBox(btotal);
    
    for (Next(i); !Done(i); Next(i)) {
	gr = GetView(i)->GetGraphic();
	gr->GetBox(bgraphic);
	btotal = btotal + bgraphic;
    }
    l = btotal._left;
    b = btotal._bottom;
    r = btotal._right;
    t = btotal._top;
}

boolean Selection::IsEmpty () { return _ulist->IsEmpty(); }
