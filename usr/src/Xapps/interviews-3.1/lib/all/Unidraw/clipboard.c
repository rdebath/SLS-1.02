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
 * Implementation of Clipboard class.
 */

#include <Unidraw/clipboard.h>
#include <Unidraw/globals.h>
#include <Unidraw/iterator.h>
#include <Unidraw/selection.h>
#include <Unidraw/ulist.h>
#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

Clipboard::Clipboard (GraphicComp* comp) {
    _comps = new UList;
    if (comp != nil) {
        _comps->Append(new UList(comp));
    }
}

Clipboard* Clipboard::Copy () {
    Clipboard* cbnew = new Clipboard;
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        cbnew->Append(GetComp(i));
    }
    return cbnew;
}

Clipboard* Clipboard::DeepCopy () {
    Clipboard* cbnew = new Clipboard;
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        cbnew->Append((GraphicComp*) GetComp(i)->Copy());
    }
    return cbnew;
}

void Clipboard::Init (Selection* s) {
    Iterator i;

    Clear();
    for (s->First(i); !s->Done(i); s->Next(i)) {
        Append(s->GetView(i)->GetGraphicComp());
    }
}

void Clipboard::CopyInit (Selection* s) {
    Iterator i;

    Clear();
    for (s->First(i); !s->Done(i); s->Next(i)) {
        Append((GraphicComp*) s->GetView(i)->GetGraphicComp()->Copy());
    }
}

void Clipboard::Clear () {
    delete _comps;
    _comps = new UList;
}

void Clipboard::DeleteComps () {
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        GraphicComp* comp = GetComp(i);
        delete comp;
    }
}

UList* Clipboard::Elem (Iterator i) { return (UList*) i.GetValue(); }
void Clipboard::First (Iterator& i) { i.SetValue(_comps->First()); }
void Clipboard::Last (Iterator& i) { i.SetValue(_comps->Last()); }
void Clipboard::Next (Iterator& i) { i.SetValue(Elem(i)->Next()); }
void Clipboard::Prev (Iterator& i) { i.SetValue(Elem(i)->Prev()); }
boolean Clipboard::Done (Iterator i) { return Elem(i) == _comps->End(); }
GraphicComp* Clipboard::Comp (UList* r) { return (GraphicComp*) (*r)(); }
GraphicComp* Clipboard::GetComp (Iterator i) { return Comp(Elem(i)); }

void Clipboard::SetComp (GraphicComp* gc, Iterator& i) {
    i.SetValue(_comps->Find(gc));
}

boolean Clipboard::Includes (GraphicComp* gc) {
    Iterator i;
    
    for (First(i); !Done(i); Next(i)) {
        if (GetComp(i) == gc) {
            return true;
        }
    }
    return false;
}

void Clipboard::Append (GraphicComp* comp) { _comps->Append(new UList(comp));}
void Clipboard::Prepend (GraphicComp* comp) {_comps->Prepend(new UList(comp));}

void Clipboard::InsertBefore (Iterator i, GraphicComp* comp) {
    Elem(i)->Append(new UList(comp));
}

void Clipboard::InsertAfter (Iterator i, GraphicComp* comp) {
    Elem(i)->Prepend(new UList(comp));
}

void Clipboard::Remove (Iterator& i) {
    UList* doomed = Elem(i);

    Next(i);
    _comps->Remove(doomed);
    delete doomed;
}

void Clipboard::Remove (GraphicComp* comp) { _comps->Delete(comp); }
Clipboard::~Clipboard () { delete _comps; }
boolean Clipboard::IsEmpty () { return _comps->IsEmpty(); }
