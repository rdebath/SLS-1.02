/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * Banner implementation.
 */

#include <InterViews/font.h>
#include <IV-2_6/InterViews/banner.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/shape.h>
#include <OS/math.h>

static const int pad = 2*pixels;	/* space around banner text */

Banner::Banner(char* lt, char* m, char* rt) {
    Init(lt, m, rt);
}

Banner::Banner(const char* name, char* lt, char* m, char* rt) {
    SetInstance(name);
    Init(lt, m, rt);
}

void Banner::Init(char* lt, char* m, char* rt) {
    SetClassName("Banner");
    left = lt;
    middle = m;
    right = rt;
    highlight = false;
    inverse = nil;
}

void Banner::Reconfig() {
    int w;

    const Font* f = output->GetFont();
    lw = left == nil ? 0 : f->Width(left);
    mw = middle == nil ? 0 : f->Width(middle);
    rw = right == nil ? 0 : f->Width(right);
    if (mw > 0) {
	w = mw + 2*Math::max(lw, rw);
    } else {
	w = lw + rw;
    }
    shape->width = 2*pad + w + f->Width("    ");
    shape->height = f->Height() + 2*pad;
    shape->Rigid(0, hfil, 0, 0);
    Unref(inverse);
    inverse = new Painter(output);
    inverse->Reference();
    inverse->SetColors(output->GetBgColor(), output->GetFgColor());
}

Banner::~Banner() {
    Unref(inverse);
}

void Banner::Redraw(IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2) {
    Painter* p = highlight ? inverse : output;
    p->ClearRect(canvas, x1, y1, x2, y2);
    if (right != nil && rx <= x2) {
	p->MoveTo(rx, pad);
	p->Text(canvas, right);
    }
    if (middle != nil && mx + mw >= x1 && mx <= x2) {
	p->MoveTo(mx, pad);
	p->Text(canvas, middle);
    }
    if (left != nil && lx + lw >= x1) {
	p->MoveTo(lx, pad);
	p->Text(canvas, left);
    }
}

void Banner::Resize() {
    lx = pad;
    mx = (xmax - mw) / 2;
    rx = xmax - rw + 1 - pad;
}

void Banner::Update() {
    if (canvas != nil) {
	Reconfig();
	Resize();
	Draw();
    }
}
