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
 * Implementation of the base class of interaction.
 */

#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/display.h>
#include <InterViews/event.h>
#include <InterViews/font.h>
#include <InterViews/handler.h>
#include <InterViews/style.h>
#include <IV-2_6/InterViews/ihandler.h>
#include <IV-2_6/InterViews/interactor.h>
#include <IV-2_6/InterViews/iwindow.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/scene.h>
#include <IV-2_6/InterViews/sensor.h>
#include <IV-2_6/InterViews/shape.h>
#include <IV-2_6/InterViews/world.h>
#include <OS/string.h>

#include <IV-2_6/_enter.h>

Interactor::Interactor() {
    Init();
}

Interactor::Interactor(const char* name) {
    Init();
    SetInstance(name);
}

void Interactor::Init() {
    shape = new Shape;
    canvas = nil;
    window = nil;
    perspective = nil;
    xmax = 0;
    ymax = 0;
    input = nil;
    output = nil;
    classname = nil;
    instance = nil;
    style = new Style;
    Resource::ref(style);
    parent = nil;
    cursensor = nil;
    world = nil;
    insert_window = nil;
    managed_window = nil;
    cursor_ = nil;
    canvas_type_ = CanvasInputOutput;
    handler_ = new InteractorHandler(this);
    Resource::ref(handler_);
    ref();
}

Interactor::~Interactor() {
    Resource::unref(input);
    Resource::unref(output);
    delete window;
    delete shape;
    Resource::unref(style);
    Resource::unref(handler_);
}

World* Interactor::GetWorld() const { return world; }
void Interactor::Flush() { GetWorld()->Flush(); }
void Interactor::Sync() { GetWorld()->Sync(); }

/*
 * The implementation of Check must closely match the implementation
 * of Read because we want Check returning true to imply that a subsequent
 * Read will not block.
 */

boolean Interactor::Check() {
    Event e;
    e.display(world->display());
    e.target = nil;
    do {
	if (!e.pending()) {
	    return false;
	}
	e.read();
    } while (e.handler() == nil || e.target == nil);
    e.unread();
    return true;
}

class InteractorHelper : public Interactor {
public:
    InteractorHelper();
    virtual ~InteractorHelper();

    static InteractorHelper* instance(Handler*);

    virtual void Handle(Event&);
private:
    static InteractorHelper* instance_;
    static Handler* handler_;
};

InteractorHelper* InteractorHelper::instance_;
Handler* InteractorHelper::handler_;

InteractorHelper::InteractorHelper() { }
InteractorHelper::~InteractorHelper() { }

InteractorHelper* InteractorHelper::instance(Handler* h) {
    if (instance_ == nil) {
	instance_ = new InteractorHelper;
    }
    handler_ = h;
    return instance_;
}

void InteractorHelper::Handle(Event& e) {
    Handler* h = handler_;
    Resource::ref(h);
    h->event(e);
    Resource::unref(h);
}

void Interactor::Read(Event& e) {
    e.display(world->display());
    e.target = nil;
    Handler* h = nil;
    while (!world->done()) {
	e.read();
	h = e.handler();
	Resource::ref(h);
	if (e.target != nil) {
	    Resource::unref(h);
	    break;
	}
	if (h != nil && !e.is_grabbing(h)) {
	    e.GetInfo();
	    e.target = InteractorHelper::instance(h);
	    break;
	}
	Resource::unref(h);
    }
}

boolean Interactor::Read(long sec, long usec, Event& e) {
    e.display(world->display());
    e.target = nil;
    while (!world->done() && e.read(sec, usec)) {
	Handler* h = e.handler();
	if (e.target != nil) {
	    return true;
	}
	if (h != nil && !e.is_grabbing(h)) {
	    e.GetInfo();
	    e.target = InteractorHelper::instance(h);
	    return true;
	}
    }
    return false;
}

void Interactor::UnRead(Event& e) { e.unread(); }

void Interactor::Run() {
    Event e;
    do {
	Read(e);
	if (world->done()) {
	    break;
	}
	e.target->Handle(e);
    } while (e.target != nil);
}

void Interactor::QuitRunning(Event& e) {
    e.target = nil;
    if (world != nil) {
	world->quit();
    }
}

/*
 * Set (x,y) for a given alignment of a shape within a canvas.
 */

void Interactor::Align(
    Alignment a, int width, int height, IntCoord& l, IntCoord& b
) const {
    switch (a) {
	case Left:
	case TopLeft:
	case CenterLeft:
	case BottomLeft:
	    l = 0;
	    break;
	case TopCenter:
	case Center:
	case BottomCenter:
	case HorizCenter:
	    l = (xmax + 1 - width) / 2;
	    break;
	case Right:
	case TopRight:
	case CenterRight:
	case BottomRight:
	    l = xmax + 1 - width;
	    break;
	case Bottom:
	case Top:
	case VertCenter:
	    /* leave unchanged */
	    break;
    }
    switch (a) {
	case Bottom:
	case BottomLeft:
	case BottomCenter:
	case BottomRight:
	    b = 0;
	    break;
	case CenterLeft:
	case Center:
	case CenterRight:
	case VertCenter:
	    b = (ymax + 1 - height) / 2;
	    break;
	case Top:
	case TopLeft:
	case TopCenter:
	case TopRight:
	    b = ymax + 1 - height;
	    break;
	case Left:
	case Right:
	case HorizCenter:
	    /* leave unchanged */
	    break;
    }
}

void Interactor::Adjust(Perspective&) {
    /* default is to ignore */
}

void Interactor::Update() {
    /* default is to ignore */
}

/*
 * Start a configuration traversal of an interactor hierarchy,
 * assigning parents, output painters, and calling Reconfig
 * for each interactor.
 *
 * This routine only works correctly when "s" is a world.
 * In any other case, it correctly assigns parents but does not
 * set up the property path for attribute lookup.  It should go up
 * from "s" to the world, then come down pushing property directories
 * in the same manner as DoConfig.  Instead of passing rootReversed
 * to DoConfig, it should compute "reverseVideo" for "s" and
 * pass whether it is on or not.
 */

void Interactor::Config(Scene* s) {
    if (parent != s) {
	if (parent != nil) {
	    parent->Remove(this);
	}
	parent = s;
	/* cast to workaround DEC C++ compiler bug */
	world = ((Interactor*)s)->world;
	DoConfig(false);
    }
}

void Interactor::Config(World* w) {
    if (parent != nil) {
	parent->Remove(this);
	parent = nil;
    }
    world = w;
    Resource::unref(output);
    output = nil;
    DoConfig(false);
}

/*
 * Configure an interactor.  This implies first configuring
 * all of its children, then calling the Reconfig virtual.
 * We automatically setup the interactor's painter and check
 * for any local properties before calling Reconfig.
 *
 * DoConfig is passed a flag indicating whether this interactor's
 * parent painter has had its foreground and background colors reversed.
 * We copy this into a local that DefaultConfig will modify
 * if a reverseVideo attribute is specified that swaps the colors.
 * A swap occurs if either reversed is false and reverseVideo:on is found
 * or reversed is true and reverseVideo:off is found.
 */

void Interactor::DoConfig(boolean parentReversed) {
    boolean reversed = parentReversed;
    if (parent != nil) {
	/* cast to workaround DEC C++ compiler bug */
	output = ((Interactor*)parent)->output;
    }
    DefaultConfig(reversed);
    Resource::ref(output);

    Interactor* children[100];
    Interactor** a;
    int n;
    GetComponents(children, sizeof(children) / sizeof(Interactor*), a, n);
    if (n > 0) {
	for (Interactor** ii = a; ii < &a[n]; ii++) {
	    Interactor* i = *ii;
	    i->parent = (Scene*)this;
	    i->world = world;
	    i->DoConfig(reversed);
	}
	if (a != children) {
	    delete a;
	}
    }

    Reconfig();
}

/*
 * Setup an interactor's painter.  If the style hasn't changed from
 * its parent, then just use the parent's painter.
 */

void Interactor::DefaultConfig(boolean& reversed) {
    if (parent == nil) {
	world->display()->style()->append(style);
    } else {
	/* cast to workaround DEC C++ compiler bug */
	((Interactor*)parent)->style->append(style);
    }

    const Font* f = nil;
    const Color* fg = nil;
    const Color* bg = nil;
    Display* d = world->display();

    String v;
    if (style->find_attribute("font", v) || style->find_attribute("Font", v)) {
	f = Font::lookup(v);
    }
    if (style->find_attribute("foreground", v) ||
	style->find_attribute("Foreground", v)
    ) {
	fg = Color::lookup(d, v);
    }
    if (style->find_attribute("background", v) ||
	style->find_attribute("Background", v)
    ) {
	bg = Color::lookup(d, v);
    }
    if (reversed) {
	const Color* c = fg;
	fg = bg;
	bg = c;
    }

    boolean swap_colors = false;
    String rv;
    if (style->find_attribute("reverseVideo", rv)) {
	if (rv.case_insensitive_equal("on")) {
	    if (!reversed) {
		swap_colors = true;
		reversed = true;
	    }
	} else if (rv.case_insensitive_equal("off")) {
	    if (reversed) {
		swap_colors = true;
		reversed = false;
	    }
	} else {
	    /* error message? */
	}
    }

    if (output == nil) {
	output = new Painter;
    } else if (!swap_colors && f == output->GetFont() &&
	fg == output->GetFgColor() && bg == output->GetBgColor()
    ) {
	return;
    } else {
	output = new Painter(output);
    }

    if (f != nil) {
	output->SetFont(f);
    }

    if (swap_colors) {
	const Color* c = fg;
	fg = bg;
	bg = c;
    }
    output->SetColors(fg, bg);
}

/*
 * Retrieve an attribute value using the interactor's style.
 */

const char* Interactor::GetAttribute(const char* name) const {
    String v;
    if (style->find_attribute(name, v) || (
	    style->parent() == nil &&
	    World::current()->display()->style()->find_attribute(name, v)
	)
    ) {
	return v.string();
    }
    return nil;
}

/*
 * Short-hand for testing if an attribute is "on" or "true".
 */

boolean Interactor::AttributeIsSet(const char* name) const {
    String v;
    return (
	style->value_is_on(name) || (
	    style->parent() == nil && !style->find_attribute(name, v) &&
	    World::current()->display()->style()->value_is_on(name)
	)
    );
}

/*
 * Given (x,y) relative to this, translate them to (x',y') that are
 * relative to the interactor "rel".  If "rel" is nil, then make
 * (x',y') relative to the screen.
 */

void Interactor::GetRelative(IntCoord& x, IntCoord& y, Interactor* rel) const {
    /* make (x,y) screen-relative */
    IntCoord left, bottom;
    GetPosition(left, bottom);
    x += left;
    y += bottom;

    if (rel != nil) {
	/* subtract position of other interactor */
	IntCoord rel_left, rel_bottom;
	rel->GetPosition(rel_left, rel_bottom);
	x -= rel_left;
	y -= rel_bottom;
    }
}

/*
 * Return coordinates relative to the world.  The given world
 * isn't relevant; it only makes sense for backward compatibility
 * (for the days when World was a subclass of Scene).
 */

void Interactor::GetRelative(IntCoord& x, IntCoord& y, World*) const {
    IntCoord left, bottom;
    GetPosition(left, bottom);
    x += left;
    y += bottom;
}

void Interactor::Orphan() {
    delete window;
    window = nil;
    canvas = nil;
}

void Interactor::GetComponents(Interactor**, int, Interactor**&, int& n) {
    n = 0;
}

void Interactor::Draw() {
    if (canvas != nil && canvas->status() != Canvas::unmapped) {
	Redraw(0, 0, xmax, ymax);
    }
}

void Interactor::Activate() { }
void Interactor::Deactivate() { }
void Interactor::Handle(Event&) { }
void Interactor::Highlight(boolean) { }
void Interactor::Reconfig() { }
void Interactor::Redraw(IntCoord, IntCoord, IntCoord, IntCoord) { }
void Interactor::Resize() { }

/*
 * Default is to redraw each area separately.
 */

void Interactor::RedrawList(
    int n, IntCoord left[], IntCoord bottom[], IntCoord right[], IntCoord top[]
) {
    register int i;

    for (i = 0; i < n; i++) {
	Redraw(left[i], bottom[i], right[i], top[i]);
    }
}

/*
 * Default is to accept a new shape completely and
 * notify the parent (if set).
 */

void Interactor::Reshape(Shape& ns) {
    *shape = ns;
    if (parent != nil) {
	parent->Change(this);
    }
}

void Interactor::SetClassName(const char* s) {
    if (s != nil) {
	style->alias(String(s));
    }
    classname = s;
}

const char* Interactor::GetClassName() const { return classname; }

void Interactor::SetInstance(const char* s) {
    if (s != nil) {
	style->name(String(s));
    }
    instance = s;
}

const char* Interactor::GetInstance() const { return instance; }

void Interactor::SetCursor(Cursor* c) {
    cursor_ = c;
    if (canvas != nil) {
	window->cursor(c);
    }
}

Cursor* Interactor::GetCursor() const { return cursor_; }
Canvas* Interactor::GetCanvas() const { return canvas; }

/*
 * Set the type of canvas for save under, backing store, etc.
 * This operation has no effect until the next time the canvas
 * is explicitly mapped.
 */

void Interactor::SetCanvasType(CanvasType ct) {
    canvas_type_ = ct;
}

CanvasType Interactor::GetCanvasType() const { return canvas_type_; }

ManagedWindow* Interactor::GetTopLevelWindow() const { return managed_window; }
