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
 * Picture class implementation.  A Picture is a Graphic that contains other 
 * Graphics.
 */

#include <Unidraw/iterator.h>
#include <Unidraw/ulist.h>
#include <Unidraw/Graphic/picture.h>

#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

Picture::Picture (Graphic* gr) : FullGraphic(gr) {
    _extent = nil;
    _kids = new UList();
}

Picture::~Picture () {
    while (!_kids->IsEmpty()) {
	UList* cur = _kids->First();
	_kids->Remove(cur);
        Graphic* g = graphic(cur);
	delete g;
	delete cur;
    }
    delete _kids;
    uncacheExtent();
}

void Picture::First (Iterator& i) { i.SetValue(_kids->First()); }
void Picture::Last (Iterator& i) { i.SetValue(_kids->Last()); }
void Picture::Next (Iterator& i) { i.SetValue(Elem(i)->Next()); }
void Picture::Prev (Iterator& i) { i.SetValue(Elem(i)->Prev()); }
boolean Picture::Done (Iterator i) { return Elem(i) == _kids->End(); }
boolean Picture::IsEmpty () { return _kids->IsEmpty(); }
Graphic* Picture::GetGraphic (Iterator i) { return graphic(Elem(i)); }

void Picture::SetGraphic (Graphic* g, Iterator& i) {
    i.SetValue(_kids->Find(g));
}

void Picture::Append (Graphic* g0, Graphic* g1, Graphic* g2, Graphic* g3) {
    invalidateCachesGraphic(g0);
    _kids->Append(new UList(g0));
    setParent(g0, this);
    if (g1 != nil) {
	invalidateCachesGraphic(g1);
	_kids->Append(new UList(g1));
	setParent(g1, this);
    }
    if (g2 != nil) {
	invalidateCachesGraphic(g2);
	_kids->Append(new UList(g2));
	setParent(g2, this);
    }
    if (g3 != nil) {
	invalidateCachesGraphic(g3);
	_kids->Append(new UList(g3));
	setParent(g3, this);
    }
    uncacheExtent();
    uncacheParents();
}

void Picture::Prepend (Graphic* g0, Graphic* g1, Graphic* g2, Graphic* g3) {
    if (g3 != nil) {
	invalidateCachesGraphic(g3);
	_kids->Prepend(new UList(g3));
	setParent(g3, this);
    }
    if (g2 != nil) {
	invalidateCachesGraphic(g2);
	_kids->Prepend(new UList(g2));
	setParent(g2, this);
    }
    if (g1 != nil) {
	invalidateCachesGraphic(g1);
	_kids->Prepend(new UList(g1));
	setParent(g1, this);
    }
    invalidateCachesGraphic(g0);
    _kids->Prepend(new UList(g0));
    setParent(g0, this);
    uncacheExtent();
    uncacheParents();
}

void Picture::InsertBefore (Iterator i, Graphic* g) {
    invalidateCachesGraphic(g);
    Elem(i)->Append(new UList(g));
    setParent(g, this);
    uncacheExtent();
    uncacheParents();
}

void Picture::InsertAfter (Iterator i, Graphic* g) {
    invalidateCachesGraphic(g);
    Elem(i)->Prepend(new UList(g));
    setParent(g, this);
    uncacheExtent();
    uncacheParents();
}

void Picture::Remove (Graphic* g) {
    unsetParent(g);
    _kids->Delete(g);
    uncacheExtent();
    uncacheParents();
}

void Picture::Remove (Iterator& i) {
    UList* doomed = Elem(i);
    Graphic* g = graphic(doomed);

    Next(i);
    unsetParent(g);
    _kids->Remove(doomed);
    delete doomed;
    uncacheExtent();
    uncacheParents();
}

Graphic* Picture::FirstGraphicContaining (PointObj& pt) {
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
	Graphic* subgr = GetGraphic(i);

	if (subgr->Contains(pt)) {
	    return subgr;
	}
    }
    return nil;
}

Graphic* Picture::LastGraphicContaining (PointObj& pt) {
    Iterator i;

    for (Last(i); !Done(i); Prev(i)) {
	Graphic* subgr = GetGraphic(i);

	if (subgr->Contains(pt)) {
	    return subgr;
	}
    }
    return nil;
}

Graphic* Picture::FirstGraphicIntersecting (BoxObj& b) {
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
	Graphic* subgr = GetGraphic(i);

	if (subgr->Intersects(b)) {
	    return subgr;
	}
    }
    return nil;
}

Graphic* Picture::LastGraphicIntersecting (BoxObj& b) {
    Iterator i;

    for (Last(i); !Done(i); Prev(i)) {
	Graphic* subgr = GetGraphic(i);

	if (subgr->Intersects(b)) {
	    return subgr;
	}
    }
    return nil;
}

Graphic* Picture::FirstGraphicWithin (BoxObj& userb) {
    Iterator i;
    BoxObj b;

    for (First(i); !Done(i); Next(i)) {
	Graphic* subgr = GetGraphic(i);
	subgr->GetBox(b);

	if (b.Within(userb)) {
	    return subgr;
	}
    }
    return nil;
}

Graphic* Picture::LastGraphicWithin (BoxObj& userb) {
    Iterator i;
    BoxObj b;

    for (Last(i); !Done(i); Prev(i)) {
	Graphic* subgr = GetGraphic(i);
	subgr->GetBox(b);

	if (b.Within(userb)) {
	    return subgr;
	}
    }
    return nil;
}

void Picture::Bequeath () {
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
	Graphic* gr = GetGraphic(i);
	concatGraphic(gr, gr, this, gr);
    }
    FullGraphic null;
    *((Graphic*) this) = null;
}

Graphic* Picture::Copy () {
    Iterator i;
    Picture* newPicture = new Picture(this);
    
    for (First(i); !Done(i); Next(i)) {
        newPicture->Append(GetGraphic(i)->Copy());
    }
    return newPicture;
}

Graphic* Picture::graphic(UList* r) { return (Graphic*) (*r)(); }
UList* Picture::Elem (Iterator i) { return (UList*) i.GetValue(); }

void Picture::draw (Canvas* c, Graphic* gs) {
    Iterator i;
    FullGraphic gstemp;
    Transformer ttemp;

    gstemp.SetTransformer(&ttemp);

    for (First(i); !Done(i); Next(i)) {
	Graphic* gr = GetGraphic(i);
	concatGraphic(gr, gr, gs, &gstemp);
	drawGraphic(gr, c, &gstemp);
    }
    gstemp.SetTransformer(nil);	/* to avoid deleting ttemp explicitly */
}

void Picture::drawClipped (
    Canvas* c, Coord l, Coord b, Coord r, Coord t, Graphic* gs
) {
    BoxObj box, clipBox(l, b, r, t);
    
    getBox(box, gs);

    if (clipBox.Intersects(box)) {
        Iterator i;
        FullGraphic gstemp;
        Transformer ttemp;
	gstemp.SetTransformer(&ttemp);

        for (First(i); !Done(i); Next(i)) {
	    Graphic* gr = GetGraphic(i);
	    concatGraphic(gr, gr, gs, &gstemp);
	    drawClippedGraphic(gr, c, l, b, r, t, &gstemp);
	}
	gstemp.SetTransformer(nil); /* to avoid deleting ttemp explicitly */
    }
}

void Picture::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    Extent e;
    float right, top, dummy1, dummy2;

    if (extentCached()) {
	getCachedExtent(e._left, e._bottom, e._cx, e._cy, e._tol);

    } else {
	if (IsEmpty()) {
	    l = b = cx = cy = tol = 0.0;
	    return;

	} else {
            Iterator i;
            FullGraphic gstemp;
            Transformer ttemp;
            Extent te;
    
            gstemp.SetTransformer(&ttemp);
            First(i);
	    Graphic* gr = GetGraphic(i);
	    concatGSGraphic(gr, gr, gs, &gstemp);
            concatTransformerGraphic(gr, nil, gr->GetTransformer(), &ttemp);
	    getExtentGraphic(gr, e._left,e._bottom,e._cx,e._cy,e._tol,&gstemp);

            for (Next(i); !Done(i); Next(i)) {
		gr = GetGraphic(i);
		concatGSGraphic(gr, gr, gs, &gstemp);
                concatTransformerGraphic(gr,nil, gr->GetTransformer(), &ttemp);
		getExtentGraphic(
                    gr, te._left, te._bottom, te._cx, te._cy, te._tol, &gstemp
                );
		e.Merge(te);
	    }
	    cacheExtent(e._left, e._bottom, e._cx, e._cy, e._tol);
            gstemp.SetTransformer(nil); // to avoid deleting ttemp explicitly
	}
    }
    right = 2*e._cx - e._left;
    top = 2*e._cy - e._bottom;
    transformRect(e._left, e._bottom, right, top, l, b, dummy1, dummy2, gs);
    transform(e._cx, e._cy, cx, cy, gs);
    tol = e._tol;
}

boolean Picture::contains (PointObj& po, Graphic* gs) {
    if (!IsEmpty()) {
        Iterator i;
        FullGraphic gstemp;
        Transformer ttemp;
        BoxObj b;

	getBox(b, gs);

	if (b.Contains(po)) {
	    gstemp.SetTransformer(&ttemp);

            for (First(i); !Done(i); Next(i)) {
                Graphic* gr = GetGraphic(i);
		concatGraphic(gr, gr, gs, &gstemp);

		if (containsGraphic(gr, po, &gstemp)) {
		    gstemp.SetTransformer(nil);
		    return true;
		}
	    }
	    gstemp.SetTransformer(nil); /* to avoid deleting ttemp explicitly*/
	}
    }
    return false;
}

boolean Picture::intersects (BoxObj& userb, Graphic* gs) {
    if (!IsEmpty()) {
        Iterator i;
        FullGraphic gstemp;
        Transformer ttemp;
        BoxObj b;

	getBox(b, gs);

	if (b.Intersects(userb)) {
	    gstemp.SetTransformer(&ttemp);

            for (First(i); !Done(i); Next(i)) {
		Graphic* gr = GetGraphic(i);
		concatGraphic(gr, gr, gs, &gstemp);

		if (intersectsGraphic(gr, userb, &gstemp)) {
		    gstemp.SetTransformer(nil);
		    return true;
		}
	    }
	    gstemp.SetTransformer(nil); /* to avoid deleting ttemp explicitly*/
	}
    }
    return false;
}

void Picture::getCachedExtent (
    float& l, float& b, float& cx, float& cy, float& tol
) {
    l = _extent->_left;
    b = _extent->_bottom;
    cx = _extent->_cx;
    cy = _extent->_cy;
    tol = _extent->_tol;
}

boolean Picture::extentCached () { return _caching && _extent != nil; }

void Picture::cacheExtent (float l, float b, float cx, float cy, float tol) {
    if (_caching) {
	uncacheExtent();
	_extent = new Extent(l, b, cx, cy, tol);
    }
}

void Picture::uncacheExtent () {
    delete _extent; 
    _extent = nil;
}

void Picture::uncacheChildren () {
    register UList* i;
    Graphic* subgr;

    for (i = _kids->First(); i != _kids->End(); i = i->Next()) {
	subgr = graphic(i);
	uncacheExtentGraphic(subgr);
	uncacheChildrenGraphic(subgr);
    }
}
