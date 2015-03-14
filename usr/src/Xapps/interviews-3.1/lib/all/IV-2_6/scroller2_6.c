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
 * Scrolling implementation.
 */

#include <InterViews/pattern.h>
#include <IV-2_6/InterViews/scroller.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/perspective.h>
#include <IV-2_6/InterViews/rubrect.h>
#include <IV-2_6/InterViews/sensor.h>
#include <IV-2_6/InterViews/shape.h>
#include <OS/math.h>

#include <IV-2_6/_enter.h>

static const int inset = 1;	/* space between scroller canvas and bar */

static inline int DefaultSize() { return Math::round(0.20*inch); }

Scroller::Scroller(Interactor* i, int n) {
    interactor = i;
    size = n;
    Init();
}

Scroller::Scroller(const char* name, Interactor* i, int n) {
    SetInstance(name);
    interactor = i;
    size = n;
    Init();
}

void Scroller::Init() {
    view = interactor->GetPerspective();
    view->Attach(this);
    shown = new Perspective;
    shape->Rigid();
    input = new Sensor;
    input->Catch(DownEvent);
    input->Catch(UpEvent);
    input->Catch(MotionEvent);
}

Scroller::~Scroller() {
    view->Detach(this);
    Resource::unref(shown);
}

void Scroller::MakeBackground() {
    Painter* bg = new Painter(output);
    bg->ref();
    Resource::unref(output);
    output = bg;
    static Pattern* pat;
    if (pat == nil) {
	pat = new Pattern(Pattern::lightgray);
	pat->Reference();
    }
    output->SetPattern(pat);
}

void Scroller::Resize() {
    *shown = *view;
}

inline void Scroller::Background(
    IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2
) {
    output->FillRect(canvas, x1, y1, x2, y2);
}

HScroller::HScroller(Interactor* i, int n) : Scroller(i, n) {
    Init();
}

HScroller::HScroller(
    const char* name, Interactor* i, int n
) : Scroller(name, i, n) {
    Init();
}

HScroller::~HScroller() { }

void HScroller::Init() {
    SetClassName("HScroller");
}

void HScroller::Reconfig() {
    if (size == 0) {
	shape->height = DefaultSize();
    } else {
	shape->height = size;
    }
    shape->hstretch = hfil;
    shape->hshrink = 0;
    MakeBackground();
    syncScroll = AttributeIsSet("syncScroll");
}

VScroller::VScroller(Interactor* i, int n) : Scroller(i, n) {
    Init();
}

VScroller::VScroller(
    const char* name, Interactor* i, int n
) : Scroller(name, i, n) {
    Init();
}

VScroller::~VScroller() { }

void VScroller::Init() {
    SetClassName("VScroller");
}

void VScroller::Reconfig() {
    if (size == 0) {
	shape->width = DefaultSize();
    } else {
	shape->width = size;
    }
    shape->vstretch = vfil;
    shape->vshrink = 0;
    MakeBackground();
    syncScroll = AttributeIsSet("syncScroll");
}

void HScroller::GetBarInfo(
    register Perspective* s, IntCoord& left, int& width
) {
    IntCoord maxwidth = xmax + 1;

    if (s->width == 0) {
        scale = 1.0;
        left = -1;
        width = maxwidth + 2;
    } else {
	scale = double(maxwidth) / double(s->width);
	if (Math::equal(scale, double(0.0), double(1e-6))) {
	    scale = 1.0;
	}
        left = Math::round(double(s->curx - s->x0) * scale);
        width = Math::max(Math::round(double(s->curwidth) * scale), 5);
    }
}

void VScroller::GetBarInfo(
    register Perspective* s, IntCoord& bot, int& height
) {
    IntCoord maxheight = ymax + 1;

    if (s->height == 0) {
        scale = 1.0;
        bot = -1;
        height = maxheight + 2;
    } else {
	scale = double(maxheight) / double(s->height);
	if (Math::equal(scale, double(0.0), double(1e-6))) {
	    scale = 1.0;
	}
        bot = Math::round(double(s->cury - s->y0) * scale);
        height = Math::max(Math::round(double(s->curheight) * scale), 5);
    }
}

inline void HScroller::Bar(IntCoord x, int width) {
    output->ClearRect(canvas, x, inset+1, x+width-1, ymax-inset-1);
}

inline void VScroller::Bar(IntCoord y, int height) {
    output->ClearRect(canvas, inset+1, y, xmax-inset-1, y+height-1);
}

inline void HScroller::Outline(IntCoord x, int width) {
    output->Rect(canvas, x, inset, x+width-1, ymax-inset);
}

inline void VScroller::Outline(IntCoord y, int height) {
    output->Rect(canvas, inset, y, xmax-inset, y+height-1);
}

inline void HScroller::Border(IntCoord x) {
    output->Line(canvas, x, inset, x, ymax-inset);
}

inline void VScroller::Border(IntCoord y) {
    output->Line(canvas, inset, y, xmax-inset, y);
}

inline void HScroller::Sides(IntCoord x1, IntCoord x2) {
    output->Line(canvas, x1, inset, x2, inset);
    output->Line(canvas, x1, ymax-inset, x2, ymax-inset);
}

inline void VScroller::Sides(IntCoord y1, IntCoord y2) {
    output->Line(canvas, inset, y1, inset, y2);
    output->Line(canvas, xmax-inset, y1, xmax-inset, y2);
}

void HScroller::Redraw(IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2) {
    IntCoord left;
    int width;

    Background(x1, y1, x2, y2);
    GetBarInfo(shown, left, width);
    Bar(left, width);
    Outline(left, width);
}

void VScroller::Redraw(IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2) {
    IntCoord bot;
    int height;

    Background(x1, y1, x2, y2);
    GetBarInfo(shown, bot, height);
    Bar(bot, height);
    Outline(bot, height);
}

void HScroller::Handle(Event& e) {
    if (e.eventType == DownEvent) {
        Perspective s = *view;
        boolean syncing =
            (syncScroll && !e.control) || (!syncScroll && e.control);

	IntCoord nx = Slide(e);
	if (!syncing) {
	    s.curx = nx;
	    interactor->Adjust(s);
	}
    }
}

void VScroller::Handle(Event& e) {
    if (e.eventType == DownEvent) {
        Perspective s = *view;
        boolean syncing =
            (syncScroll && !e.control) || (!syncScroll && e.control);

	IntCoord ny = Slide(e);
	if (!syncing) {
	    s.cury = ny;
	    interactor->Adjust(s);
	}
    }
}

IntCoord HScroller::Slide(register Event& e) {
    IntCoord x1, y1, x2, y2;
    IntCoord oldx, minx, maxx;
    int width, w;
    Perspective s;

    s = *view;
    GetBarInfo(shown, oldx, width);
    if (e.x < oldx) {
        x1 = Math::max(0, e.x - width/2);
    } else if (e.x > oldx + width) {
        x1 = Math::min(e.x - width/2, xmax - width + 1);
    } else {
        x1 = oldx;
    }
    x2 = x1 + width - 1;
    minx = Math::min(oldx, 0);
    maxx = Math::max(xmax + 1, oldx + width) - width;
    w = e.x - x1;

    boolean syncing = (syncScroll && !e.control) || (!syncScroll && e.control);
    SlidingRect r(output, canvas, x1+1, inset+1, x2-1, ymax-inset-1, e.x, 0);
    r.Draw();

    for (;;) {
        switch (e.eventType) {
        case UpEvent:
        case DownEvent:
        case MotionEvent:
	    if (e.target != this) {
		e.target->GetRelative(e.x, e.y, this);
	    }
            r.Track(Math::max(minx + w, Math::min(maxx + w, e.x)), 0);

            if (syncing) {
                r.Erase();
                r.GetCurrent(x1, y1, x2, y2);
                s.curx = shown->x0 + Math::round(double(x1-1) / scale);
                interactor->Adjust(s);
            }
            break;
        }
        if (e.eventType == UpEvent) {
            break;
        }
	Read(e);
    }

    r.GetCurrent(x1, y1, x2, y2);
    r.Erase();
    return shown->x0 + Math::round(double(x1-1) / scale);
}

IntCoord VScroller::Slide(register Event& e) {
    IntCoord x1, y1, x2, y2;
    IntCoord oldy, miny, maxy;
    int height, h;
    Perspective s;

    s = *view;
    GetBarInfo(shown, oldy, height);
    if (e.y < oldy) {
        y1 = Math::max(0, e.y - height/2);
    } else if (e.y > oldy + height) {
        y1 = Math::min(e.y - height/2, ymax - height + 1);
    } else {
        y1 = oldy;
    }
    y2 = y1 + height - 1;
    miny = Math::min(oldy, 0);
    maxy = Math::max(ymax + 1, oldy + height) - height;
    h = e.y - y1;

    boolean syncing = (syncScroll && !e.control) || (!syncScroll && e.control);
    SlidingRect r(output, canvas, inset+1, y1+1, xmax-inset-1, y2-1, 0, e.y );
    r.Draw();

    for (;;) {
        switch (e.eventType) {
        case UpEvent:
        case DownEvent:
        case MotionEvent:
	    if (e.target != this) {
		e.target->GetRelative(e.x, e.y, this);
	    }
            r.Track(0, Math::max(miny + h, Math::min(maxy + h, e.y)));

            if (syncing) {
                r.Erase();
                r.GetCurrent(x1, y1, x2, y2);
                s.cury = shown->y0 + Math::round(double(y1-1) / scale);
                interactor->Adjust(s);
            }
            break;
        }
        if (e.eventType == UpEvent) {
            break;
        }
	Read(e);
    }

    r.GetCurrent(x1, y1, x2, y2);
    r.Erase();
    return shown->y0 + Math::round(double(y1-1) / scale);
}

void HScroller::Update() {
    IntCoord oldleft, oldright, newleft, newright;
    int oldwidth, newwidth;
    Perspective* p;

    if (canvas == nil) {
	return;
    }
    p = view;
    GetBarInfo(shown, oldleft, oldwidth);
    GetBarInfo(p, newleft, newwidth);
    if (oldleft != newleft || oldwidth != newwidth) {
	oldright = oldleft+oldwidth-1;
	newright = newleft+newwidth-1;
	if (oldright >= newleft && newright >= oldleft) {
	    if (oldright > newright) {
		Background(newright+1, inset, oldright, ymax-inset);
		Border(newright);
	    } else if (oldright < newright) {
		Bar(oldright, newright-oldright);
		Sides(oldright, newright);
		Border(newright);
	    }
	    if (oldleft > newleft) {
		Bar(newleft+1, oldleft-newleft);
		Sides(newleft, oldleft);
		Border(newleft);
	    } else if (oldleft < newleft) {
		Background(oldleft, inset, newleft-1, ymax-inset);
		Border(newleft);
	    }
	} else {
	    Background(oldleft, inset, oldright, ymax-inset);
	    Bar(newleft, newwidth);
	    Outline(newleft, newwidth);
	}
    }
    *shown = *p;
}

void VScroller::Update() {
    IntCoord oldbottom, oldtop, newbottom, newtop;
    int oldheight, newheight;
    Perspective* p;

    if (canvas == nil) {
	return;
    }
    p = view;
    GetBarInfo(shown, oldbottom, oldheight);
    GetBarInfo(p, newbottom, newheight);
    if (oldbottom != newbottom || oldheight != newheight) {
	oldtop = oldbottom+oldheight-1;
	newtop = newbottom+newheight-1;
	if (oldtop >= newbottom && newtop >= oldbottom) {
	    if (oldtop > newtop) {
		Background(inset, newtop+1, xmax-inset, oldtop);
		Border(newtop);
	    } else if (oldtop < newtop) {
		Bar(oldtop, newtop-oldtop);
		Sides(oldtop, newtop);
		Border(newtop);
	    }
	    if (oldbottom > newbottom) {
		Bar(newbottom+1, oldbottom-newbottom);
		Sides(newbottom, oldbottom);
		Border(newbottom);
	    } else if (oldbottom < newbottom) {
		Background(inset, oldbottom, xmax-inset, newbottom-1);
		Border(newbottom);
	    }
	} else {
	    Background(inset, oldbottom, xmax-inset, oldtop);
	    Bar(newbottom, newheight);
	    Outline(newbottom, newheight);
	}
    }
    *shown = *p;
}
