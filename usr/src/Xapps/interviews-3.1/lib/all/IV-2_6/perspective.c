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
 * Perspective definition.
 */

#include <IV-2_6/InterViews/perspective.h>
#include <IV-2_6/InterViews/interactor.h>

class ViewList {
public:
    Interactor* view;
    ViewList* next;

    ViewList (Interactor* i) { view = i; next = nil; }
};

Perspective::Perspective() {
    views = nil;
    x0 = 0; y0 = 0;
    width = 0; height = 0;
    curx = 0; cury = 0;
    curwidth = 0; curheight = 0;
    sx = 0; sy = 0;
    lx = 0; ly = 0;
    Reference();
}

Perspective::Perspective(Perspective& p) {
    views = nil;
    x0 = p.x0;
    y0 = p.y0;
    width = p.width;
    height = p.height;
    curx = p.curx;
    cury = p.cury;
    curwidth = p.curwidth;
    curheight = p.curheight;
    sx = p.sx;
    sy = p.sy;
    lx = p.lx;
    ly = p.ly;
    Reference();
}

Perspective::~Perspective() {
    register ViewList* e;
    register ViewList* next;

    for (e = views; e != nil; e = next) {
	next = e->next;
	delete e;
    }
}

void Perspective::Init(
    IntCoord ix0, IntCoord iy0, IntCoord iwidth, IntCoord iheight
) {
    x0 = ix0; y0 = iy0;
    width = iwidth; height = iheight;
    curx = x0; cury = y0;
}

void Perspective::Attach(Interactor* i) {
    register ViewList* e;

    e = new ViewList(i);
    e->next = views;
    views = e;
    Reference();
}

void Perspective::Detach(Interactor* i) {
    register ViewList* e;
    register ViewList* prev;

    prev = nil;
    for (e = views; e != nil; e = e->next) {
	if (e->view == i) {
	    if (prev == nil) {
		views = e->next;
	    } else {
		prev->next = e->next;
	    }
	    e->view = nil;
	    e->next = nil;
	    delete e;
	    Unreference();
	    break;
	}
	prev = e;
    }
}

void Perspective::Update() {
    register ViewList* e;

    for (e = views; e != nil; e = e->next) {
	e->view->Update();
    }
}

boolean Perspective::operator ==(Perspective& p) {
    return
	x0 == p.x0 && y0 == p.y0 &&
	width == p.width && height == p.height &&
	curx == p.curx && cury == p.cury &&
	curwidth == p.curwidth && curheight == p.curheight &&
	sx == p.sx && sy == p.sy &&
	lx == p.lx && ly == p.ly;
}

boolean Perspective::operator !=(Perspective& p) {
    return
	x0 != p.x0 || y0 != p.y0 ||
	width != p.width || height != p.height ||
	curx != p.curx || cury != p.cury ||
	curwidth != p.curwidth || curheight != p.curheight ||
	sx != p.sx || sy != p.sy ||
	lx != p.lx || ly != p.ly;
}

Perspective& Perspective::operator =(Perspective& p) {
    x0 = p.x0;
    y0 = p.y0;
    width = p.width;
    height = p.height;
    curx = p.curx;
    cury = p.cury;
    curwidth = p.curwidth;
    curheight = p.curheight;
    sx = p.sx;
    sy = p.sy;
    lx = p.lx;
    ly = p.ly;
    return *this;
}
