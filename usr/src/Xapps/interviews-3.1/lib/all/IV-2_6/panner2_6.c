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
 * Panner implementation.
 */

#include <InterViews/pattern.h>
#include <IV-2_6/InterViews/adjuster.h>
#include <IV-2_6/InterViews/border.h>
#include <IV-2_6/InterViews/panner.h>
#include <IV-2_6/InterViews/box.h>
#include <IV-2_6/InterViews/glue.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/perspective.h>
#include <IV-2_6/InterViews/rubrect.h>
#include <IV-2_6/InterViews/sensor.h>
#include <IV-2_6/InterViews/shape.h>
#include <OS/math.h>
#include <string.h>

#include <IV-2_6/_enter.h>

Panner::Panner (Interactor* i, int size) {
    Init(i, size);
}

Panner::Panner (const char* name, Interactor* i, int size) {
    SetInstance(name);
    Init(i, size);
}

Panner::~Panner() { }

/* 0.3 second delay for auto-repeat */
static int DELAY = 3;

void Panner::Init (Interactor* i, int n) {
    SetClassName("Panner");
    size = n;
    adjusters = new HBox(
        new HGlue,
	new VBox(
            new VGlue,
            new UpMover(i, DELAY),
	    new HBox(
                new HGlue,
		new LeftMover(i, DELAY),
		new HGlue,
		new RightMover(i, DELAY),
                new HGlue
	    ),
            new DownMover(i, DELAY),
            new VGlue
	),
        new HGlue,
	new VBox(
            new VGlue(2),
	    new Enlarger(i),
            new VGlue(4),
	    new Reducer(i),
            new VGlue(2)
	),
        new HGlue
    );
    slider = new Slider(i);
    Insert(
	new VBox(adjusters, new HBorder, slider)
    );
}

void Panner::Reconfig () {
    MonoScene::Reconfig();
    Shape a = *adjusters->GetShape();
    if (a.vstretch != 0 || a.vshrink != a.height / 3) {
        if (size != 0) {
            a.width = size;
            a.hshrink = a.hstretch = 0;
        }
        a.vstretch = 0;
        a.vshrink = a.height/3;
        adjusters->Reshape(a);
    }
    Shape* s = slider->GetShape();
    if (s->width != a.width) {
        slider->Reshape(a);
    }
}

static const int MIN_SLIDER_HT = 20;
enum MoveType { MOVE_HORIZ, MOVE_VERT, MOVE_UNDEF };

Slider::Slider (Interactor* i) {
    Init(i);
}

Slider::Slider (const char* name, Interactor* i) {
    SetInstance(name);
    Init(i);
}

void Slider::Init (Interactor* i) {
    SetClassName("Slider");
    interactor = i;
    view = i->GetPerspective();
    view->Attach(this);
    shown = new Perspective;
    constrained = false;
    moveType = MOVE_UNDEF;
    *shown = *view;
    shape->vstretch = shape->vshrink = 0;
    prevl = prevb = prevr = prevt = 0;
    input = new Sensor(updownEvents);
}

void Slider::Reconfig () {
    Painter* p = new Painter(output);
    p->Reference();
    Unref(output);
    output = p;

    const char* attrib = GetAttribute("syncScroll");
    syncScroll = attrib != nil &&
        (strcmp(attrib, "true") == 0 || strcmp(attrib, "on") == 0);
}

void Slider::Reshape (Shape& ns) {
    if (shown->width == 0) {
	*shape = ns;
    } else {
	shape->width = (canvas == nil) ? ns.width : xmax + 1;
	float aspect = float(shown->height) / float(shown->width);
	int h = Math::round(aspect * float(shape->width));
	if (h != shape->height) {
	    shape->height = h;
	    Scene* p = Parent();
	    if (p != nil) {
		p->Change(this);
	    }
	}
    }
}

void Slider::Draw () {
    if (canvas != nil) {
	output->SetPattern(new Pattern(Pattern::lightgray));
	output->FillRect(canvas, 0, 0, xmax, ymax);
	output->SetPattern(new Pattern(Pattern::clear));
	output->FillRect(canvas, left, bottom, right, top);
	output->SetPattern(new Pattern(Pattern::solid));
	output->Rect(canvas, left, bottom, right, top);
	output->Line(canvas, left+1, bottom-1, right+1, bottom-1);
	output->Line(canvas, right+1, bottom-1, right+1, top-1);

	prevl = left; prevb = bottom;
	prevr = right; prevt = top;
    }
}

void Slider::Redraw (
    IntCoord left, IntCoord bottom, IntCoord right, IntCoord top
) {
    output->Clip(canvas, left, bottom, right, top);
    Draw();
    output->NoClip();
}

inline IntCoord Slider::ViewX (IntCoord x) {
    return Math::round(float(x) * float(shown->width) / float(xmax));
}

inline IntCoord Slider::ViewY (IntCoord y) {
    return Math::round(float(y) * float(shown->height) / float(ymax));
}

inline IntCoord Slider::SliderX (IntCoord x) {
    return Math::round(float(x) * float(xmax) / float(shown->width));
}

inline IntCoord Slider::SliderY (IntCoord y) {
    return Math::round(float(y) * float(ymax) / float(shown->height));
}

void Slider::Move (IntCoord dx, IntCoord dy) {
    shown->curx += dx;
    shown->cury += dy;
}

boolean Slider::Inside (Event& e) {
    return e.x > left && e.x < right && e.y > bottom && e.y < top;
}

void Slider::CalcLimits (Event& e) {
    llim = e.x - Math::max(0, left);
    blim = e.y - Math::max(0, bottom);
    rlim = e.x + Math::max(0, xmax - right);
    tlim = e.y + Math::max(0, ymax - top);
    constrained = e.shift;
    moveType = MOVE_UNDEF;
    origx = e.x;
    origy = e.y;
}

static int CONSTRAIN_THRESH = 2;    
    // difference between x and y movement needed to decide which direction
    // is constrained

void Slider::Constrain (Event& e) {
    IntCoord dx, dy;

    if (constrained && moveType == MOVE_UNDEF) {
	dx = Math::abs(e.x - origx);
	dy = Math::abs(e.y - origy);
	if (Math::abs(dx - dy) < CONSTRAIN_THRESH) {
	    e.x = origx;
	    e.y = origy;
	} else if (dx > dy) {
	    moveType = MOVE_HORIZ;
	} else {
	    moveType = MOVE_VERT;
	}
    }

    if (!constrained) {
	e.x = Math::min(Math::max(e.x, llim), rlim);
	e.y = Math::min(Math::max(e.y, blim), tlim);

    } else if (moveType == MOVE_HORIZ) {
	e.x = Math::min(Math::max(e.x, llim), rlim);
	e.y = origy;

    } else if (moveType == MOVE_VERT) {
	e.x = origx;
	e.y = Math::min(Math::max(e.y, blim), tlim);

    }
}

void Slider::Slide (Event& e) {
    IntCoord newleft, newbot, dummy;
    boolean control = e.control;

    Listen(allEvents);
    SlidingRect r(output, canvas, left, bottom, right, top, e.x, e.y);
    CalcLimits(e);
    do {
	switch (e.eventType) {
	    case MotionEvent:
		e.target->GetRelative(e.x, e.y, this);
		Constrain(e);
		r.Track(e.x, e.y);

                if ((syncScroll && !control) || (!syncScroll && control)) {
                    r.Erase();
                    r.GetCurrent(newleft, newbot, dummy, dummy);
                    Move(ViewX(newleft - left), ViewY(newbot - bottom));
                    interactor->Adjust(*shown);
                }

		break;
	    default:
		break;
	}
	Read(e);
    } while (e.eventType != UpEvent);

    r.GetCurrent(newleft, newbot, dummy, dummy);
    Move(ViewX(newleft - left), ViewY(newbot - bottom));
    Listen(input);
}

void Slider::Jump (Event& e) {
    register Perspective* s = shown;
    IntCoord dx, dy;
    
    if (e.button == RIGHTMOUSE) {
	dx = ViewX(e.x) - s->curx - s->curwidth/2;
	dy = ViewY(e.y) - s->cury - s->curheight/2;
    } else {
	if (e.button == LEFTMOUSE) {
	    dx = s->sx;
	    dy = s->sy;
	} else {
	    dx = s->lx;
	    dy = s->ly;
	}

	if (e.x < left) {
	    dx = -dx;
	} else if (e.x < right) {
	    dx = 0;
	}
	if (e.y < bottom) {
	    dy = -dy;
	} else if (e.y < top) {
	    dy = 0;
	}
    }
    dx = Math::min(
	Math::max(s->x0 - s->curx, dx),
	s->x0 + s->width - s->curx - s->curwidth
    );
    dy = Math::min(
	Math::max(s->y0 - s->cury, dy),
	s->y0 + s->height - s->cury - s->curheight
    );
    Move(dx, dy);
}	

void Slider::Handle (Event& e) {
    if (e.eventType == DownEvent) {
	if (Inside(e)) {
	    Slide(e);
	} else {
	    Jump(e);
	}
	interactor->Adjust(*shown);
    }
}
    
static const int MIN_SIZE = 2;

void Slider::SizeKnob () {
    register Perspective* s = shown;
    
    if (canvas != nil) {
	left = SliderX(s->curx - s->x0);
	bottom = SliderY(s->cury - s->y0);
	right = left + Math::max(SliderX(s->curwidth), MIN_SIZE);
	top = bottom + Math::max(SliderY(s->curheight), MIN_SIZE);
    }
}    

void Slider::Update () {
    register Perspective* p = shown;
    int h, oldwidth, oldheight;
    float aspect;
    Scene* s;
    Shape ns;

    oldwidth = p->width;
    oldheight = p->height;
    *p = *view;
    aspect = float(p->height) / float(p->width);

    SizeKnob();
    if (p->width != oldwidth || p->height != oldheight) {
	h = Math::round(aspect * float(shape->width));
	if (h == shape->height) {
	    Draw();
	} else {
	    shape->height = h;
	    if ((s = Parent()) != nil) {
		s->Change(this);
	    }
	}
    } else if (
	prevl != left || prevb != bottom || prevr != right || prevt != top
    ) {
	Draw();
    }
}

void Slider::Resize () {
    int w = xmax + 1;
    if (shape->width != w) {
	Shape ns = *shape;
	ns.width = w;
	Reshape(ns);
    }
    SizeKnob();
}

Slider::~Slider () {
    view->Detach(this);
    Unref(shown);
}
