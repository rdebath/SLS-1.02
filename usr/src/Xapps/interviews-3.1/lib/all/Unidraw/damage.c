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
 * Damage class implementation.
 * (Enhanced by Ian Daniel <ian@research.canon.oz.au>, May, 1991.)
 */

#include <Unidraw/globals.h>
#include <Unidraw/iterator.h>
#include <Unidraw/ulist.h>

#include <Unidraw/Graphic/damage.h>
#include <Unidraw/Graphic/geomobjs.h>
#include <Unidraw/Graphic/graphic.h>

#include <InterViews/canvas.h>
#include <IV-2_6/InterViews/painter.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

Damage::Damage (Canvas* c, Painter* p, Graphic* g) {
    _areas = new UList;
    _additions = new UList;
    _canvas = c;
    _output = p;
    Ref(_output);
    _graphic = g;
}

Damage::~Damage () {
    Unref(_output);
    DeleteAreas();
    delete _additions;
}

int Damage::Area (BoxObj& b) {
    return (b._right - b._left) * (b._top - b._bottom);
}

void Damage::DrawAreas () {
    BoxObj visible(0, 0, _canvas->Width() - 1, _canvas->Height() - 1);
    BoxObj b, *a;
    Iterator i;

    for (FirstArea(i); !Done(i); Next(i)) {
        a = GetArea(i);
        b = *a - visible;
	_output->ClearRect(_canvas, b._left, b._bottom, b._right, b._top);
	_graphic->DrawClipped(_canvas, b._left, b._bottom, b._right, b._top);
    }
}    

void Damage::DrawAdditions () {
    Coord xmax = _canvas->Width() - 1;
    Coord ymax = _canvas->Height() - 1;
    Iterator i;

    for (FirstAddition(i); !Done(i); Next(i)) {
	GetAddition(i)->Draw(_canvas, 0, 0, xmax, ymax);
    }
}

void Damage::Merge (BoxObj& newb) {
    BoxObj* a1, *a2;
    int newArea, area1, area2, diff1, diff2, diff3, maximum;
    Iterator i;

    FirstArea(i);
    a1 = GetArea(i);
    Next(i);
    a2 = GetArea(i);
    BoxObj merge1(*a1 + newb);
    BoxObj merge2(*a2 + newb);
    BoxObj merge3(*a1 + *a2);

    newArea = Area(newb);
    area1 = Area(*a1);
    area2 = Area(*a2);
    diff1 = area1 + newArea - Area(merge1);
    diff2 = area2 + newArea - Area(merge2);
    diff3 = area1 + area2 - Area(merge3);
    maximum = max(max(diff1, diff2), diff3);

    if (maximum == diff1) {
	if (a2->Intersects(merge1)) {
	    *a1 = merge1 + *a2;
            DeleteArea(a2);
	} else {
	    *a1 = merge1;
	}

    } else if (maximum == diff2) {
	if (a1->Intersects(merge2)) {
	    *a2 = merge2 + *a1;
            DeleteArea(a1);
	} else {
	    *a2 = merge2;
	}

    } else {
	if (newb.Intersects(merge3)) {
	    *a1 = merge3 + newb;
            DeleteArea(a2);
	} else {
	    *a1 = merge3;
	    *a2 = newb;
	}
    }
}

void Damage::Added (Graphic* g) { 
    _additions->Append(new UList(g));
}

boolean Damage::Incurred () {
    return !_areas->IsEmpty() || !_additions->IsEmpty();
}

void Damage::Incur (Graphic* g) {
    BoxObj box;

    g->GetBox(box);
    Incur(box);
}

void Damage::Incur (Coord l, Coord b, Coord r, Coord t) {
    BoxObj box(l, b, r, t);
    Incur(box);
}

void Damage::Incur (BoxObj& newb) {
    BoxObj* b;
    Iterator i;

    if (_areas->IsEmpty()) {
	_areas->Prepend(new UList(new BoxObj(&newb)));

    } else if (_areas->First() == _areas->Last()) {
        FirstArea(i);
        b = GetArea(i);
	if (newb.Intersects(*b)) {
	    if (!newb.Within(*b)) {
		*b = *b + newb;
	    }
	} else {
	    _areas->Prepend(new UList(new BoxObj(&newb)));
	}
    } else {
	Merge(newb);
    }
}

void Damage::Repair () {
    DrawAreas();
    DrawAdditions();
    Reset();
}

void Damage::Reset () {
    DeleteAreas();
    _areas = new UList;
    delete _additions;
    _additions = new UList;
}

void Damage::SetCanvas (Canvas* c) {
    _canvas = c;
}

void Damage::SetPainter (Painter* p) {
    Unref(_output);
    _output = p;
    Ref(_output);
}

void Damage::SetGraphic (Graphic* gr) {
    _graphic = gr;
}

Canvas* Damage::GetCanvas () {
    return _canvas;
}

Painter* Damage::GetPainter () {
    return _output;
}

Graphic* Damage::GetGraphic () {
    return _graphic;
}

BoxObj* Damage::GetArea (Iterator i) { 
    UList* area = Elem(i); 
    return (BoxObj*) (*area) (); 
}

Graphic* Damage::GetAddition (Iterator i) { 
    UList* addition = Elem(i); 
    return (Graphic*) (*addition) (); 
}

void Damage::FirstArea (Iterator& i) { 
    i.SetValue(_areas->First()); 
}

void Damage::FirstAddition (Iterator& i) { 
    i.SetValue(_additions->First()); 
}

void Damage::Next (Iterator& i) { 
    i.SetValue(Elem(i)->Next()); 
}

boolean Damage::Done (Iterator i) { 
    UList* u = Elem(i);
    return u == _areas->End() || u == _additions->End(); 
}

UList* Damage::Elem (Iterator i) {
    return (UList*) i.GetValue(); 
}

void Damage::DeleteArea (BoxObj* area) {
    delete area;
    _areas->Delete(area);
}

void Damage::DeleteAreas () {
    Iterator i;

    for (FirstArea(i); !Done(i); Next(i)) {
        delete GetArea(i);
    }
    delete _areas;
}
