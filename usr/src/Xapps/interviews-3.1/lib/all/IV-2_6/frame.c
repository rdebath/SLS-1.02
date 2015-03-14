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
 * A frame surrounds another interactor, providing borders, title banners, etc.
 */

#include <InterViews/canvas.h>
#include <InterViews/pattern.h>
#include <IV-2_6/InterViews/banner.h>
#include <IV-2_6/InterViews/border.h>
#include <IV-2_6/InterViews/frame.h>
#include <IV-2_6/InterViews/box.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/sensor.h>
#include <IV-2_6/InterViews/shape.h>

Frame::Frame(Interactor* i, int w) {
    Init(i, w, w, w, w);
}

Frame::Frame(const char* name, Interactor* i, int w) {
    SetInstance(name);
    Init(i, w, w, w, w);
}

Frame::Frame(Interactor* i, int l, int b, int r, int t) {
    Init(i, l, b, r, t);
}

Frame::Frame(const char* name, Interactor* i, int l, int b, int r, int t) {
    SetInstance(name);
    Init(i, l, b, r, t);
}

Frame::~Frame() { }

void Frame::Init(Interactor* i, int l, int b, int r, int t) {
    SetClassName("Frame");
    left = l;
    bottom = b;
    right = r;
    top = t;
    if (i != nil) {
	Insert(i);
    }
}

void Frame::Reconfig() {
    MonoScene::Reconfig();
    shape->width += left + right;
    shape->height += bottom + top;
}

void Frame::Resize() {
    canvas->SetBackground(output->GetBgColor());
    Place(interior(), left, bottom, xmax - right, ymax - top);
}

void Frame::Redraw(IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2) {
    register IntCoord r = xmax - right;
    register IntCoord t = ymax - top;

    if (x1 < left) {
        output->FillRect(canvas, 0, 0, left-1, t);
    }
    if (y1 < bottom) {
        output->FillRect(canvas, left, 0, xmax, bottom-1);
    }
    if (x2 > r) {
        output->FillRect(canvas, r+1, bottom, xmax, ymax);
    }
    if (y2 > t) {
        output->FillRect(canvas, 0, t+1, r, ymax);
    }
}

/*
 * A ShowFrame highlights/unhighlights when it gets enter/leave events.
 * These frames are generally only applicable to window manager or
 * similar functionality; they are not recommended for general use.
 */

ShowFrame::~ShowFrame() { }

void ShowFrame::Init() {
    input = onoffEvents;
    input->Reference();
}

void ShowFrame::Handle(Event& e) {
    if (e.eventType == EnterEvent) {
        InsideFrame(true);
    } else if (e.eventType == LeaveEvent) {
        InsideFrame(false);
    } else {
	HandleInput(e);
    }
}

void ShowFrame::HandleInput(Event& e) {
    interior()->Handle(e);
}

void ShowFrame::InsideFrame(boolean) {
    /* default is to do nothing */
}

/*
 * A title frame is a frame around a box containing
 * a banner, border, and the component.
 */

TitleFrame::TitleFrame(Banner* b, Interactor* i, int w) : ShowFrame(nil, w) {
    Init(b, i);
}

TitleFrame::TitleFrame(
    const char* name, Banner* b, Interactor* i, int w
) : ShowFrame(name, nil, w) {
    Init(b, i);
}

TitleFrame::~TitleFrame() { }

void TitleFrame::Init(Banner* b, Interactor* i) {
    SetClassName("TitleFrame");
    banner = b;
    if (i != nil) {
	Insert(i);
    }
}

Interactor* TitleFrame::Wrap(Interactor* i) {
    Scene* p = banner->Parent();
    if (p != nil) {
	p->Remove(banner);
    }
    return new VBox(banner, new HBorder, i);
}

void TitleFrame::InsideFrame(boolean b) {
    banner->highlight = b;
    banner->Draw();
}

/*
 * A border frame draws an outline using a solid pattern when
 * it contains the input focus and using a gray pattern otherwise.
 */

BorderFrame::BorderFrame(Interactor* i, int w) : ShowFrame(i, w) {
    Init();
}

BorderFrame::BorderFrame(
    const char* name, Interactor* i, int w
) : ShowFrame(name, i, w) {
    Init();
}

BorderFrame::~BorderFrame() { }

void BorderFrame::Init() {
    SetClassName("BorderFrame");
    normal = false;
}

void BorderFrame::InsideFrame(boolean b) {
    normal = b;
    Redraw(0, 0, xmax, ymax);
}

void BorderFrame::Redraw(IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2) {
    if (normal) {
	Frame::Redraw(x1, y1, x2, y2);
    } else {
	const Pattern* save = output->GetPattern();
	Resource::ref(save);
	output->SetPattern(new Pattern(Pattern::gray));
	Frame::Redraw(x1, y1, x2, y2);
	output->SetPattern(save);
	Resource::unref(save);
    }
}

/*
 * A shadow frame is a frame with a drop shadow.
 */

ShadowFrame::ShadowFrame(Interactor* i, int h, int v) {
    Init(i, h, v);
}

ShadowFrame::ShadowFrame(
    const char* name, Interactor* i, int h, int v
) : Frame(name) {
    Init(i, h, v);
}

ShadowFrame::~ShadowFrame() { }

void ShadowFrame::Init(Interactor* i, int h, int v) {
    if (h > 0) {
	bottom += h;
    } else {
	top += -h;
    }
    if (v > 0) {
	right += v;
    } else {
	left += -v;
    }
    if (i != nil) {
	Insert(i);
    }
}

void ShadowFrame::Redraw(IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2) {
    register IntCoord r = xmax - right;
    register IntCoord t = ymax - top;
    register IntCoord v = bottom + top - 2;
    register IntCoord h = left + right - 2;

    /* borders */
    if (x1 < left) {
        output->FillRect(canvas, left-1, bottom-1, left-1, t);
    }
    if (y1 < bottom) {
        output->FillRect(canvas, left, bottom-1, r+1, bottom-1);
    }
    if (x2 > r) {
        output->FillRect(canvas, r+1, bottom, r+1, t+1);
    }
    if (y2 > t) {
        output->FillRect(canvas, left-1, t+1, r, t+1);
    }

    /* shadows */
    if (left > 1 && x1 < left-1) {
        output->FillRect(canvas, 0, v, left-2, ymax-v);
    }
    if (bottom > 1 && y1 < bottom-1) {
        output->FillRect(canvas, h, 0, xmax-h, bottom-2);
    }
    if (right > 1 && x2 > r+1) {
        output->FillRect(canvas, r+2, v, xmax, ymax-v);
    }
    if (top > 1 && y2 > t+1) {
        output->FillRect(canvas, h, t+2, xmax-h, ymax);
    }

    /* corner */
    if (left > 1 && bottom > 1 && x1 < left-1 && y1 < bottom-1) {
        output->FillRect(canvas, 0, 0, h - 1, v - 1);
    } else if (left > 1 && top > 1 && x1 < left-1 && y2 > t+1) {
        output->FillRect(canvas, 0, ymax - v + 1, h - 1, ymax);
    } else if (right > 1 && bottom > 1 && x2 > r+1 && y1 < bottom-1) {
        output->FillRect(canvas, xmax - h + 1, 0, xmax, v - 1);
    } else if (right > 1 && top > 1 && x1 > r+1 && y2 > t+1) {
        output->FillRect(canvas, xmax - h + 1, ymax - v + 1, xmax, ymax);
    }
}

/*
 * A margin frame surrounds its component with horizontal and vertical
 * glue.
 */

MarginFrame::MarginFrame(Interactor* i, int margin) : Frame(i, 0) {
    Init(margin, 0, 0, margin, 0, 0);
}

MarginFrame::MarginFrame(
    const char* name, Interactor* i, int margin
) : Frame(name, i, 0) {
    Init(margin, 0, 0, margin, 0, 0);
}

MarginFrame::MarginFrame(
    Interactor* i, int margin, int shrink, int stretch
) : Frame(i, 0) {
    Init(margin, shrink, stretch, margin, shrink, stretch);
}

MarginFrame::MarginFrame(
    const char* name, Interactor* i, int margin, int shrink, int stretch
) : Frame(name, i, 0) {
    Init(margin, shrink, stretch, margin, shrink, stretch);
}

MarginFrame::MarginFrame(
    Interactor* i, int hmargin, int vmargin
) : Frame(i, 0) {
    Init(hmargin, 0, 0, vmargin, 0, 0);
}

MarginFrame::MarginFrame(
    const char* name, Interactor* i, int hmargin, int vmargin
) : Frame(name, i, 0) {
    Init(hmargin, 0, 0, vmargin, 0, 0);
}

MarginFrame::MarginFrame(
    Interactor* i,
    int hmargin, int hshrink, int hstretch,
    int vmargin, int vshrink, int vstretch
) : Frame(i, 0) {
    Init(hmargin, hshrink, hstretch, vmargin, vshrink, vstretch);
}

MarginFrame::MarginFrame(
    const char* name, Interactor* i,
    int hmargin, int hshrink, int hstretch,
    int vmargin, int vshrink, int vstretch
) : Frame(name, i, 0) {
    Init(hmargin, hshrink, hstretch, vmargin, vshrink, vstretch);
}

MarginFrame::~MarginFrame() { }

void MarginFrame::Init(int h, int hshr, int hstr, int v, int vshr, int vstr) {
    SetClassName("MarginFrame");
    hmargin = h * 2;
    hshrink = hshr * 2;
    hstretch = hstr * 2;
    vmargin = v * 2;
    vshrink = vshr * 2;
    vstretch = vstr * 2;
}

void MarginFrame::Reconfig() {
    Frame::Reconfig();
    shape->width += hmargin;
    shape->height += vmargin;
    shape->hshrink += hshrink;
    shape->hstretch += hstretch;
    shape->vshrink += vshrink;
    shape->vstretch += vstretch;
}

void MarginFrame::Resize() {
    canvas->SetBackground(output->GetBgColor());

    IntCoord hextra = (xmax+1) - shape->width;
    IntCoord h = hmargin;
    if (hextra > 0 && shape->hstretch != 0) {
        h += int(float(hstretch) / float(shape->hstretch) * float(hextra));
    } else if (hextra < 0 && shape->hshrink != 0) {
        h += int(float(hshrink) / float(shape->hshrink) * float(hextra));
    }

    IntCoord vextra = (ymax+1) - shape->height;
    IntCoord v = vmargin;
    if (vextra > 0 && shape->vstretch != 0) {
        v += int(float(vstretch) / float(shape->vstretch) * float(vextra));
    } else if (vextra < 0 && shape->vshrink != 0) {
        v += int(float(vshrink) / float(shape->vshrink) * float(vextra));
    }

    Place(interior(), h/2, v/2, xmax-h/2, ymax-v/2);
}

void MarginFrame::Redraw(IntCoord, IntCoord, IntCoord, IntCoord) { }
