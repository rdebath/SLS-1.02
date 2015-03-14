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
 * Implementation of UStencil, an object derived from Graphic.
 */

#include <Unidraw/globals.h>
#include <Unidraw/Graphic/ustencil.h>

#include <InterViews/bitmap.h>
#include <IV-2_6/InterViews/painter.h>
#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

UStencil::UStencil (Bitmap* i, Bitmap* m, Graphic* gr) : Graphic(gr) {
    _image = i;
    _mask = m;
    Ref(_image);
    Ref(_mask);
}

UStencil::~UStencil () {
    Unref(_image);
    Unref(_mask);
}

void UStencil::GetOriginal (Bitmap*& i, Bitmap*& m) {
    i = _image;
    m = _mask;
}

extern PSPattern* pssolid;                      // hack (see header file)
PSPattern* UStencil::GetPattern () { return pssolid; }

Graphic* UStencil::Copy () {
    Bitmap* image_cpy = new Bitmap(*_image);
    Bitmap* mask_cpy =
        (_mask == nil) ? nil :
        (_mask == _image) ? image_cpy :
        new Bitmap(*_mask);

    return new UStencil(image_cpy, mask_cpy, this);
}

void UStencil::draw (Canvas *c, Graphic* gs) {
    update(gs);
    _p->Stencil(c, _image->Left(), _image->Bottom(), _image, _mask);
}

void UStencil::getExtent (
    float& x0, float& y0, float& cx, float& cy, float& tol, Graphic* gs
) {
    Bitmap* bitmap = (_mask == nil) ? _image : _mask;
    float w = float(bitmap->Width());
    float h = float(bitmap->Height());

    if (gs->GetTransformer() == nil) {
	x0 = y0 = 0;
	cx = w/2;
	cy = h/2;
    } else {
	transformRect(0, 0, w, h, x0, y0, cx, cy, gs);
	cx = (cx + x0)/2;
	cy = (cy + y0)/2;
    }
    tol = 0;
}

boolean UStencil::contains (PointObj& po, Graphic* gs) {
    Bitmap* bitmap = (_mask == nil) ? _image : _mask;
    PointObj pt (&po);

    invTransform(pt._x, pt._y, gs);
    BoxObj b (0, 0, bitmap->Width(), bitmap->Height());
    return b.Contains(pt);
}

boolean UStencil::intersects (BoxObj& userb, Graphic* gs) {
    Transformer* t = gs->GetTransformer();
    Bitmap* bitmap = (_mask == nil) ? _image : _mask;
    Coord xmax = bitmap->Width();
    Coord ymax = bitmap->Height();
    Coord tx0, ty0, tx1, ty1;
    
    if (t != nil && t->Rotated()) {
	Coord x[4], tx[5];
	Coord y[4], ty[5];
    
	x[0] = x[3] = y[0] = y[1] = 0;
	x[2] = x[1] = xmax;
	y[2] = y[3] = ymax;
	transformList(x, y, 4, tx, ty, gs);
	tx[4] = tx[0];
	ty[4] = ty[0];
	FillPolygonObj fp (tx, ty, 5);
	return fp.Intersects(userb);
    
    } else if (t != nil) {
	t->Transform(0, 0, tx0, ty0);
	t->Transform(xmax, ymax, tx1, ty1);
	BoxObj b1 (tx0, ty0, tx1, ty1);
	return b1.Intersects(userb);

    } else {
	BoxObj b2 (0, 0, xmax, ymax);
	return b2.Intersects(userb);
    }
}
