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
 * GraphicBlock implementation.
 */

#include <Unidraw/Graphic/graphic.h>
#include <Unidraw/Graphic/grblock.h>

#include <IV-look/kit.h>
#include <InterViews/bitmap.h>
#include <InterViews/canvas.h>
#include <InterViews/cursor.h>
#include <InterViews/event.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/perspective.h>
#include <IV-2_6/InterViews/scene.h>
#include <IV-2_6/InterViews/shape.h>

#include <IV-2_6/_enter.h>

#include <math.h>
#include <stdlib.h>

/*****************************************************************************/

GraphicBlock::GraphicBlock (
    const char* name, Graphic* gr, Coord pad, Alignment a, Zooming z
) {
    SetClassName("GraphicBlock");
    SetInstance(name);
    _graphic = gr;
    _pad = pad;
    _align = a;
    _zooming = z;
    perspective = new Perspective;
    _highlighted = false;
    Init();
}

GraphicBlock::GraphicBlock (Graphic* gr, Coord pad, Alignment a, Zooming z) {
    SetClassName("GraphicBlock");
    _graphic = gr;
    _pad = pad;
    _align = a;
    _zooming = z;
    perspective = new Perspective;
    _highlighted = false;
    Init();
}

void GraphicBlock::Init () {
    register Perspective* p = perspective;
    Coord left, bottom, right, top;

    _mag = 1;
    if (_graphic == nil) {
        _x0 = _y0 = 0;
        p->width = p->height = 1;
    } else {
        GetGraphicBox(left, bottom, right, top);
        _x0 = left;
        _y0 = bottom;
        p->width = shape->width = right - left + 2*_pad;
        p->height = shape->height = top - bottom + 2*_pad;
    }
    if (canvas == nil) {
        p->curwidth = p->width;
        p->curheight = p->height;
    } else {
        p->curwidth = xmax + 1;
        p->curheight = ymax + 1;
    }
    Align();
}

void GraphicBlock::Align () {
    register Perspective* p = perspective;
    Coord l, b, dummy1, dummy2;

    if (_graphic == nil) {
        return;
    }
    switch (_align) {
	case BottomLeft:
	case CenterLeft:
	case TopLeft:
	    p->curx = 0;
	    break;
	case BottomCenter:
	case Center:
	case TopCenter:
	    p->curx = (p->width - p->curwidth)/2;
	    break;
	case BottomRight:
	case CenterRight:
	case TopRight:
	    p->curx = p->width - p->curwidth;
	    break;
    }

    switch (_align) {
	case BottomLeft:
	case BottomCenter:
	case BottomRight:
	    p->cury = 0;
	    break;	
	case CenterLeft:
	case Center:
	case CenterRight:
	    p->cury = (p->height - p->curheight)/2;
	    break;
	case TopLeft:
	case TopCenter:
	case TopRight:
	    p->cury = p->height - p->curheight;
	    break;
    }
    GetGraphicBox(l, b, dummy1, dummy2);
    l = _pad - l - p->curx;
    b = _pad - b - p->cury;
    _graphic->Translate(l, b);
    _x0 += l;
    _y0 += b;
}

void GraphicBlock::Fix () {
    register Perspective* p = perspective;
    Coord l, b, dummy1, dummy2;

    if (_graphic == nil) {
        return;
    }
    GetGraphicBox(l, b, dummy1, dummy2);
    l = _pad - l;
    b = _pad - b;

    switch (_align) {
	case BottomLeft:
	case Left:
	case TopLeft:
	    p->curx = l;
	    break;
	case Bottom:
	case Center:
	case Top:
	    p->curx -= (xmax + 1 - p->curwidth)/2;
	    break;
	case BottomRight:
	case Right:
	case TopRight:
	    p->curx -= (xmax + 1 - p->curwidth);
	    break;
    }

    switch (_align) {
	case BottomLeft:
	case Bottom:
	case BottomRight:
	    p->cury = b;
	    break;
	case Left:
	case Center:
	case Right:
	    p->cury -= (ymax + 1 - p->curheight)/2;
	    break;
	case TopLeft:
	case Top:
	case TopRight:
	    p->cury -= (ymax + 1 - p->curheight);
	    break;
    }
    l -= p->curx;
    b -= p->cury;
    _graphic->Translate(l, b);
    _x0 += l;
    _y0 += b;
}

float GraphicBlock::LimitMagnification (float desired) {
    return desired;			// no limit by default
}

void GraphicBlock::Resize () {
    register Perspective* p = perspective;
    
    Fix();
    p->curwidth = xmax + 1;
    p->curheight = ymax + 1;
    p->sx = p->curwidth/8;
    p->sy = p->curheight/8;
    p->lx = p->curwidth/2;
    p->ly = p->curheight/2;
    p->Update();
}

void GraphicBlock::UpdatePerspective () {
    register Perspective* p = perspective;
    Coord left, bottom, right, top;

    if (_graphic != nil) {
        GetGraphicBox(left, bottom, right, top);
        if (_x0 != left) {
            p->curx += _x0 - left;
            _x0 = left;
        }
        if (_y0 != bottom) {
            p->cury += _y0 - bottom;
            _y0 = bottom;
        }
        p->width = right - left + 2*_pad;
        p->height = top - bottom + 2*_pad;
    }
    p->Update();
}

void GraphicBlock::Update () {
    UpdatePerspective();
    Draw();
}

void GraphicBlock::Draw () {
    if (canvas != nil) {
	output->ClearRect(canvas, 0, 0, xmax, ymax);
	if (_graphic != nil) {
	    _graphic->Draw(canvas, 0, 0, xmax, ymax);
	}
    }
}

void GraphicBlock::Redraw (Coord l, Coord b, Coord r, Coord t) {
    if (canvas != nil) {
	output->ClearRect(canvas, l, b, r, t);
	if (_graphic != nil) {
	    _graphic->DrawClipped(canvas, l, b, r, t);
	}
    }
}

void GraphicBlock::Normalize (Perspective& np) {
    register Perspective* p = perspective;
    float hfactor, vfactor;

    if (p->width != np.width) {
	hfactor = float(p->width) / float(np.width);
	np.x0 = round(hfactor * float(np.x0));
	np.width = p->width;
	np.curx = round(hfactor * float(np.curx));
	np.curwidth = round(hfactor * float(np.curwidth));
	np.sx = round(hfactor * float(np.sx));
    }
    if (p->height != np.height) {
	vfactor = float(p->height) / float(np.height);
	np.y0 = round(vfactor * float(np.y0));
	np.height = p->height;
	np.cury = round(vfactor * float(np.cury));
	np.curheight = round(vfactor * float(np.curheight));
	np.sy = round(vfactor * float(np.sy));
    }
}

float GraphicBlock::NearestPow2 (float factor) {
    double mant;
    int pow2;

    mant = frexp(factor, &pow2);
    if (mant < 0.95) {
	--pow2;
    }
    return ldexp(1.0, pow2);
}

float GraphicBlock::ScaleFactor (Perspective& np) {
    register Perspective* p = perspective;
    float factor = 1;
    Coord dx, dy;

    dx = abs(p->curwidth - np.curwidth);
    dy = abs(p->curheight - np.curheight);
    if (dx < dy) {
	factor = float(p->curwidth) / float(np.curwidth);
    } else {
	factor = float(p->curheight) / float(np.curheight);
    }
    if (_zooming == Binary) {
	factor = NearestPow2(factor);
    }
    return factor;
}

void GraphicBlock::GetGraphicBox (Coord& l, Coord& b, Coord& r, Coord& t) {
    _graphic->GetBox(l, b, r, t);
}

void GraphicBlock::Zoom (Perspective& np) {
    register Perspective* p = perspective;
    Coord cx, cy, halfw, halfh, dx, dy;
    float factor = ScaleFactor(np);

    factor = LimitMagnification(_mag * factor)/_mag;
    if (_graphic != nil && factor != 1.0) {
	cx = np.curx + np.curwidth/2;
	cy = np.cury + np.curheight/2;
	halfw = p->curwidth/2;
	halfh = p->curheight/2;
	dx = (p->curx + halfw) - cx;
	dy = (p->cury + halfh) - cy;
	_graphic->Translate(dx, dy);
	_graphic->Scale(factor, factor, float(halfw), float(halfh));

	_x0 = round((_x0 + dx - halfw)*factor + halfw);
	_y0 = round((_y0 + dy - halfh)*factor + halfh);
	p->width = round(p->width * factor);
	p->height = round(p->height * factor);
	p->curx = round(float(cx) * factor) - halfw;
	p->cury = round(float(cy) * factor) - halfh;
    }
    _mag *= factor;
}

void GraphicBlock::Scroll (Perspective& np) {
    register Perspective* p = perspective;
    Coord dx, dy;

    if (_graphic != nil) {
        dx = p->curx - np.curx;
        dy = p->cury - np.cury;
        _graphic->Translate(dx, dy);
        _x0 += dx;
        _y0 += dy;
        p->curx = np.curx;
        p->cury = np.cury;
    }
}

void GraphicBlock::Adjust (Perspective& np) {
    register Perspective* p = perspective;
    Perspective ptmp;
    
    if (canvas == nil) {
        *p = np;
    } else if (_graphic != nil && *p != np) {
	Normalize(np);
	ptmp = *p;
	if (np.curwidth != canvas->Width() || np.curheight!=canvas->Height()) {
	    Zoom(np);
	} else {
	    Scroll(np);
	}
	p->Update();
	if (ptmp != *p) {
	    Draw();
	}
    }
}

GraphicBlock::~GraphicBlock () { Unref(perspective); }

void GraphicBlock::Highlight (boolean on) {
    if (_highlighted != on) {
        _highlighted = on;

        const Color* fg = output->GetFgColor();
        const Color* bg = output->GetBgColor();
        output->SetColors(bg, fg);

        if (_graphic != nil) {
            PSColor* fg = _graphic->GetFgColor();
            PSColor* bg = _graphic->GetBgColor();
            _graphic->SetColors(bg, fg);
        }
        Draw();
    }
}

Graphic* GraphicBlock::GetGraphic () { return _graphic; }
float GraphicBlock::GetMagnification () { return _mag; }

void GraphicBlock::SetGraphic (Graphic* g) {
    _graphic = g;
    Init();
}

void GraphicBlock::SetMagnification (float m) {
    register Perspective* p = perspective;
    float factor;
    Coord cx, cy, halfw, halfh;

    if (_zooming == Binary) {
	m = NearestPow2(m);
    }
    factor = LimitMagnification(m)/_mag;

    if (_graphic != nil && factor != 1.0) {
	halfw = p->curwidth/2;
	halfh = p->curheight/2;
	cx = p->curx + halfw;
	cy = p->cury + halfh;
	_graphic->Scale(factor, factor, float(halfw), float(halfh));

	_x0 = round((_x0 - halfw)*factor + halfw);
	_y0 = round((_y0 - halfh)*factor + halfh);
	p->width = round(p->width * factor);
	p->height = round(p->height * factor);
	p->curx = round(float(cx) * factor) - halfw;
	p->cury = round(float(cy) * factor) - halfh;

	p->Update();
	Draw();
    }
    _mag *= factor;
}

void GraphicBlock::GrabScroll (Event& e) {
    Cursor* origCursor = GetCursor();
    SetCursor(WidgetKit::instance()->hand_cursor());

    int y = e.y;
    int x = e.x;
    Perspective s = *GetPerspective();

    do {
        s.curx += (x - e.x);
        s.cury += (y - e.y);
        Adjust(s);
        y = e.y;
        x = e.x;
        Poll(e);
    } while (e.middlemouse);

    SetCursor(origCursor);
}

void GraphicBlock::RateScroll (Event& e) {
    Cursor* origCursor = GetCursor();
    WidgetKit& kit = *WidgetKit::instance();

    int y = e.y;
    int x = e.x;

    do {
        Perspective s = *GetPerspective();
        int dx = x - e.x;
        int dy = y - e.y;

        if (dx == 0 && dy == 0) {
            SetCursor(origCursor);
        } else {
            double angle = atan2(dy, dx)*180/M_PI;

            if (angle < -157.5) {
                SetCursor(kit.rfast_cursor());
            } else if (angle < -112.5) {
                SetCursor(kit.rufast_cursor());
            } else if (angle < -67.5) {
                SetCursor(kit.ufast_cursor());
            } else if (angle < -22.5) {
                SetCursor(kit.lufast_cursor());
            } else if (angle < 22.5) {
                SetCursor(kit.lfast_cursor());
            } else if (angle < 67.5) {
                SetCursor(kit.ldfast_cursor());
            } else if (angle < 112.5) {
                SetCursor(kit.dfast_cursor());
            } else if (angle < 157.5) {
                SetCursor(kit.rdfast_cursor());
            } else {
                SetCursor(kit.rfast_cursor());
            }
        }

        s.curx -= dx;
        s.cury -= dy;
        s.curx = min(max(s.x0, s.curx), s.x0 + s.width - s.curwidth);
        s.cury = min(max(s.y0, s.cury), s.y0 + s.height - s.curheight);

        Adjust(s);
        Poll(e);
    } while (e.rightmouse);

    SetCursor(origCursor);
}
