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
 * UPage implementation.
 */

#include <Unidraw/globals.h>
#include <Unidraw/upage.h>

#include <Unidraw/Graphic/graphic.h>

#include <IV-2_6/InterViews/painter.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

class PageGraphic : public Graphic {
public:
    PageGraphic(float, float, Graphic* = nil);
    virtual ~PageGraphic();

    void GetOriginal(float&, float&);

    virtual void SetBrush(PSBrush*);
    virtual PSBrush* GetBrush();

    virtual Graphic* Copy();
protected:
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
private:
    float _width, _height;
    PSBrush* _br;
};

PageGraphic::PageGraphic (
    float width, float height, Graphic* gr
) : Graphic(gr) {
    _br = nil;
    if (gr != nil) {
        PageGraphic::SetBrush(gr->GetBrush());
    }
    _width = width;
    _height = height;
}

PageGraphic::~PageGraphic () { Unref(_br); }

void PageGraphic::GetOriginal (float& width, float& height) {
    width = _width;
    height = _height;
}

void PageGraphic::SetBrush (PSBrush* br) {
    if (_br != br) {
        Ref(br);
        Unref(_br);
        _br = br;
	invalidateCaches();
    }
}

PSBrush* PageGraphic::GetBrush () { return _br; }
Graphic* PageGraphic::Copy () { return new PageGraphic(_width, _height, this);}

void PageGraphic::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    float width, dummy1, dummy2;

    width = float(gs->GetBrush()->Width());
    tol = (width > 1) ? width/2 : 0;
    transformRect(0, 0, _width-1, _height-1, l, b, dummy1, dummy2, gs);
    transform((_width-1)/2, (_height-1)/2, cx, cy, gs);
}

void PageGraphic::draw (Canvas* c, Graphic* gs) {
    if (!gs->GetBrush()->None()) {
	update(gs);
	_p->Rect(c, 0, 0, round(_width)-1, round(_height)-1);
    }
}

/*****************************************************************************/

UPage::UPage (float width, float height) {
    _graphic = new PageGraphic(width, height, stdgraphic);
}

UPage::UPage (Graphic* g) { _graphic = g; }
UPage::~UPage () { delete _graphic; }
Graphic* UPage::GetGraphic () { return _graphic; }
