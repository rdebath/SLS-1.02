/*
 * Copyright (c) 1992 Redwood Design Automation
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the name of
 * Redwood Design Automation may not be used in any advertising or publicity
 * relating to the software without the specific, prior written permission of
 * Redwood Design Automation.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * IN NO EVENT SHALL REDWOOD DESIGN AUTOMATION BE LIABLE FOR ANY SPECIAL,
 * INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT
 * ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF LIABILITY,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */

#include <InterViews/cursor.h>
#include <InterViews/event.h>
#include <InterViews/handler.h>
#include <InterViews/hit.h>
#include <InterViews/window.h>
#include <InterViews/drag.h>
#include <InterViews/background.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <IV-look/kit.h>
#include <IV-X11/xcursor.h>
#include <IV-X11/xdisplay.h>
#include <IV-X11/xevent.h>
#include <IV-X11/xwindow.h>
#include <IV-X11/xdrag.h>
#include <X11/Xatom.h>
#include <OS/host.h>
#include <OS/types.h>
#include <string.h>
#include <strstream.h>

// how is this done portably? it is used to generate a name unique to
// this process.
extern "C" {
    pid_t getpid();
}

/* class DragBox */

class DragBox : public Background {
public:
    DragBox(Glyph* body, const Color* color);

    virtual void everDrawn(boolean drawn);
    virtual boolean everDrawn();

    virtual void draw(Canvas* canvas, const Allocation& allocation) const;
protected:
    boolean everDrawn_;
};

DragBox::DragBox(Glyph* body, const Color* color) : Background(body, color) {
    everDrawn_ = false;
}

void DragBox::everDrawn(boolean drawn) {
    everDrawn_ = drawn;
}

boolean DragBox::everDrawn() {
    return everDrawn_;
}

void DragBox::draw(Canvas* canvas, const Allocation& allocation) const {
    Background::draw(canvas, allocation);
    DragBox* self = (DragBox*) this;
    self->everDrawn_ = true;
}

/* class DragAtoms */

class DragAtoms {
public:
    DragAtoms();

    boolean enter(const XEvent&);
    boolean motion(const XEvent&);
    boolean leave(const XEvent&);
    boolean drop(const XEvent&);

    boolean enter(const Event&);
    boolean motion(const Event&);
    boolean leave(const Event&);
    boolean drop(const Event&);

    Atom enter(XDisplay*);
    Atom motion(XDisplay*);
    Atom leave(XDisplay*);
    Atom drop(XDisplay*);
    Atom drag(XDisplay*);
protected:
    void cache(XDisplay*);

    XDisplay* display_;
    Atom enter_;
    Atom motion_;
    Atom leave_;
    Atom drop_;
    Atom drag_;
};

static const char* dragName = "IV_DRAG";
static const char* enterName = "IV_ENTER";
static const char* motionName = "IV_MOTION";
static const char* leaveName = "IV_LEAVE";
static const char* dropName = "IV_DROP";
static int dropUid = 0;
static DragAtoms dragAtoms;

static void setDragProperty(
    XDisplay* display, XEvent& xevent, XWindow destination,
    Atom messageType, int x, int y, const char* value = 0,
    int length = 0
) {
    Atom property = None;
    if (length != 0) {
	ostrstream name;
	name << dragName << "_" << Host::name() << "_" << getpid() << "_"  <<
	    dropUid++ << ends;
	char* buffer = name.str();
	property = XInternAtom(display, buffer, False);
	delete buffer;

	XChangeProperty(
	    display, destination, property, XA_STRING, 8,
	    PropModePrepend, (unsigned char*)value, length
	);
    }

    xevent.xclient.type = ClientMessage;
    xevent.xclient.window = destination;
    xevent.xclient.display = display;
    xevent.xclient.message_type = messageType;
    xevent.xclient.format = 32;

    xevent.xclient.data.l[0] = x;
    xevent.xclient.data.l[1] = y;

    xevent.xclient.data.l[2] = destination;
    xevent.xclient.data.l[3] = property;
    xevent.xclient.data.l[4] = length;
}

static void getDragProperty(XEvent& xevent, char*& buffer, int& length) {
    length = 0;
    buffer = 0;

    long len = xevent.xclient.data.l[4];
    if (! len) {
	return;
    }
    XDisplay* display = xevent.xclient.display;
    XWindow xwindow = xevent.xclient.data.l[2];
    Atom property = xevent.xclient.data.l[3];

    Atom actual_type;
    int actual_format;
    unsigned long nitems;
    unsigned long bytes_after;
    if (
	XGetWindowProperty(
	    display, xwindow, property, 0, len, True,
	    AnyPropertyType, &actual_type, &actual_format,
	    &nitems, &bytes_after, (unsigned char**) &buffer
	) == Success
    ) {
	length = int(len);
    }
}

static boolean understandsDragging(XDisplay* xdisplay, XWindow xwindow) {
    if (xwindow == None) {
	return false;
    }

    Atom actual_type;
    int actual_format;
    unsigned long nitems;
    unsigned long bytes_after;
    unsigned char* buffer = nil;
    if (
	XGetWindowProperty(
	    xdisplay, xwindow, dragAtoms.drag(xdisplay), 0, 0,
	    False, AnyPropertyType, &actual_type, &actual_format, &nitems,
	    &bytes_after, (unsigned char**)&buffer
	) != Success
    ) {
	return false;
    }
    if (buffer) {
	XFree((caddr_t)buffer);
    }

    return actual_type != None;
}

static XWindow translate(XDisplay* display, XWindow root, int& x, int& y) {
    XWindow dest = None;
    XWindow prev = root;
    XWindow child;
    int nx, ny;
    XTranslateCoordinates(display, root, root, x, y, &nx, &ny, &child);
    while (child != None) {
	dest = child;
	XTranslateCoordinates(display, prev, dest, x, y, &nx, &ny, &child);
	x = nx;
	y = ny;
	prev = dest;
    }
    return understandsDragging(display, dest) ? dest : None;
}

static XWindow translate(
    XDisplay* display, XWindow root, XWindow under, int& x, int& y
) {
    XWindow parent;
    XWindow *children;
    unsigned int kids;
    Status status;
    status = XQueryTree(display, root, &root, &parent, &children, &kids);
    if (status == 0) {
	return None;
    }
    for (int i = kids - 1; i >= 0 && children[i] != under ; --i);
    for (--i; i >= 0; --i) {
	XWindowAttributes attributes;
	XGetWindowAttributes(display, children[i], &attributes);
	if (attributes.map_state == IsViewable &&
	    attributes.x <= x && attributes.x + attributes.width >= x &&
	    attributes.y <= y && attributes.y + attributes.width >= y
	) {
	    break;
	}
    }
    if (i < 0) {
	return None;
    }

    XWindow dest = None;
    XWindow prev = root;
    XWindow child = children[i];
    XFree((caddr_t) children);
    int nx, ny;
    while (child != None) {
	dest = child;
	XTranslateCoordinates(display, prev, dest, x, y, &nx, &ny, &child);
	x = nx;
	y = ny;
	prev = dest;
    }
    return understandsDragging(display, dest) ? dest : None;
}

/* class XDrag */

boolean XDrag::isDrag(const XEvent& xevent) {
    return (
	dragAtoms.enter(xevent) || dragAtoms.motion(xevent) ||
	dragAtoms.leave(xevent) || dragAtoms.drop(xevent)
    );
}

void XDrag::locate(const XEvent& xevent, int& x, int& y) {
    x = int(xevent.xclient.data.l[0]);
    y = int(xevent.xclient.data.l[1]);
}

/* class DragAtoms */

DragAtoms::DragAtoms() {
    display_ = nil;
    enter_ = nil;
    motion_ = nil;
    leave_ = nil;
    drop_ = nil;
    drag_ = nil;
}

void DragAtoms::cache(XDisplay* display) {
    if (display_ == display) {
	return;
    }

    display_ = display;
    enter_  = XInternAtom(display, enterName, False);
    motion_ = XInternAtom(display, motionName, False);
    leave_  = XInternAtom(display, leaveName, False);
    drop_   = XInternAtom(display, dropName, False);
    drag_   = XInternAtom(display, dragName, False);
}

boolean DragAtoms::enter(const XEvent& xevent) {
    if (xevent.type != ClientMessage) {
	return false;
    }
    cache(xevent.xclient.display);
    return enter_ && xevent.xclient.message_type == enter_;
}

boolean DragAtoms::motion(const XEvent& xevent) {
    if (xevent.type != ClientMessage) {
	return false;
    }
    cache(xevent.xclient.display);
    return motion_ && xevent.xclient.message_type == motion_;
}

boolean DragAtoms::leave(const XEvent& xevent) {
    if (xevent.type != ClientMessage) {
	return false;
    }
    cache(xevent.xclient.display);
    return leave_ && xevent.xclient.message_type == leave_;
}

boolean DragAtoms::drop(const XEvent& xevent) {
    if (xevent.type != ClientMessage) {
	return false;
    }
    cache(xevent.xclient.display);
    return drop_ && xevent.xclient.message_type == drop_;
}

boolean DragAtoms::enter(const Event& event) {
    return enter(event.rep()->xevent_);
}

boolean DragAtoms::motion(const Event& event) {
    return motion(event.rep()->xevent_);
}

boolean DragAtoms::leave(const Event& event) {
    return leave(event.rep()->xevent_);
}

boolean DragAtoms::drop(const Event& event) {
    return drop(event.rep()->xevent_);
}

Atom DragAtoms::enter(XDisplay* display) {
    cache(display);
    return enter_;
}

Atom DragAtoms::motion(XDisplay* display) {
    cache(display);
    return motion_;
}

Atom DragAtoms::leave(XDisplay* display) {
    cache(display);
    return leave_;
}

Atom DragAtoms::drop(XDisplay* display) {
    cache(display);
    return drop_;
}

Atom DragAtoms::drag(XDisplay* display) {
    cache(display);
    return drag_;
}

/* class DragMethod */

class DragMethod {
public:
    virtual XWindow setup(XDisplay*, Event&, Drag*) = 0;
    virtual boolean moveWindow(XDisplay*, XWindow, int x, int y) = 0;
    virtual void cleanup(XDisplay*, XWindow) = 0;
};

/* class DragMethodCursor */

class DragMethodCursor : public DragMethod {
public:
    virtual XWindow setup(XDisplay*, Event&, Drag*);
    virtual boolean moveWindow(XDisplay*, XWindow, int x, int y);
    virtual void cleanup(XDisplay*, XWindow);
};

/* class DragMethodWindow */

class DragMethodWindow : public DragMethod {
public:
    virtual XWindow setup(XDisplay*, Event&, Drag*);
    virtual boolean moveWindow(XDisplay*, XWindow, int x, int y);
    virtual void cleanup(XDisplay*, XWindow);

    Window* dragWindow_;
    int dx_;
    int dy_;
    int wlx_;
    int wly_;
};

/* class DragRep */

class DragRep {
public:
    DragRep(Drag* drag);
    ~DragRep();

    boolean event(Event& event);

    Drag* drag_;
    boolean dragable_;
    Handler* target_;
    Coord x_;
    Coord y_;
    DragMethod* method_;
    DragMethodCursor methodCursor_;
    DragMethodWindow methodWindow_;
};

/* class DragHandler */

class DragHandler : public Handler {
public:
    DragHandler(DragRep* dragRep);

    virtual boolean event(Event& event);
protected:
    DragRep* dragRep_;
};

/* class Drag */

Drag::Drag(Glyph* glyph) : MonoGlyph(glyph) {
    rep_ = new DragRep(this);
}

Drag::~Drag() {
    delete rep_;
    rep_ = 0;
}

void Drag::dragable(boolean dragable) {
    rep_->dragable_ = dragable;
}

boolean Drag::dragable() const {
    return rep_->dragable_;
}

void Drag::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    rep_->x_ = a.left();
    rep_->y_ = a.top();
    MonoGlyph::allocate(c, a, ext);
}

void Drag::pick(Canvas* c, const Allocation& a, int depth, Hit& hit) {
    const Event* event = hit.event();
    if (event != nil && hit.left() <= a.right() &&
	hit.right() >= a.left() && hit.bottom() <= hit.top() &&
	hit.top() >= a.bottom() && caught(*event)
    ) {
	hit.target(depth, this, 0, rep_->target_);
    }
    MonoGlyph::pick(c, a, depth + 1, hit);
}

void Drag::dragType(char*& value, int& length) {
    value = 0;
    length = 0;
}

void Drag::dragOffset(Event& event, int& dx, int& dy) {
    dx = int(event.pointer_x() - rep_->x_ + 1);
    dy = int(rep_->y_ - event.pointer_y() + 1);
}

boolean Drag::caught(const Event& event) const {
    return (
	rep_->dragable_ && (event.type() == Event::down) &&
	event.pointer_button() == Event::middle
    );
}

boolean Drag::commit(const Event& event) const {
    return event.type() == Event::up;
}

boolean Drag::abort(const Event& event) const {
    return event.type() == Event::down;
}

DragRep* Drag::rep() const {
    return rep_;
}

/* class DragMethodCursor */

XWindow DragMethodCursor::setup(XDisplay* display, Event& event, Drag*) {
    int x, y;
    XWindow root;
    XWindow child;
    int wx, wy;
    unsigned int keys;
    XQueryPointer(
	display, event.rep()->window_->rep()->xwindow_, &root, &child,
	&x, &y, &wx, &wy, &keys
    );

    XWindow window;
    XSetWindowAttributes at;
    at.override_redirect = true;
    window = XCreateWindow(
	display, root, 0, 0, event.display()->rep()->pwidth_,
	event.display()->rep()->pheight_, 0, (int) CopyFromParent, InputOnly,
	CopyFromParent, CWOverrideRedirect, &at
    );
    XMapWindow(display, window);

    return window;
}

boolean DragMethodCursor::moveWindow(XDisplay*, XWindow, int, int) {
    return false;
}

void DragMethodCursor::cleanup(XDisplay* display, XWindow window) {
    XDestroyWindow(display, window);
}

/* class DragMethodWindow */

XWindow DragMethodWindow::setup(XDisplay* display, Event& event, Drag* drag) {
    drag->dragOffset(event, dx_, dy_);

    WidgetKit* kit = WidgetKit::instance();
    DragBox* dragBox = new DragBox(drag->dragGlyph(), kit->background());
    dragWindow_ = new PopupWindow(dragBox);
    Style* dragStyle = new Style;
    dragStyle->attribute("saveUnder", "true");
    dragWindow_->style(dragStyle);

    int x, y;
    XWindow root;
    XWindow child;
    int wx, wy;
    unsigned int keys;

    // What we really want is for the window to appear with the drag glyph
    // already drawn (it looks really bad if that is not done).
    //
    // This is how it seems like we should be able to do it:
    //   1) bind the interviews window to an X window
    //   2) repair the interviews window
    //   3) flush the X display connection
    //   4) sync with the X display
    //   5) map the X window
    //
    // But we can't bind then map the interviews window, so the following is a
    // hack that seems to work.
    XSynchronize(display, true);
    XQueryPointer(
	display, event.rep()->window_->rep()->xwindow_, &root, &child,
	&x, &y, &wx, &wy, &keys
    );
    Requisition requisition;
    dragBox->request(requisition);
    dragWindow_->place(
	event.display()->to_coord(x - dx_),
	event.display()->height() - event.display()->to_coord(y - dy_) -
	requisition.requirement(Dimension_Y).natural()
    );
    dragWindow_->map();
    XWindow window = dragWindow_->rep()->xwindow_;
    Event dummyEvent;
    dummyEvent.display(event.display());
    while (! dragBox->everDrawn()) {
	if (dummyEvent.read(0, 10) && drag->commit(dummyEvent)) {
	    cleanup(display, window);
	    XSynchronize(display, false);
	    return None;
	}
    }
    XSynchronize(display, false);

    XQueryPointer(display, window, &root, &child, &x, &y, &wx, &wy, &keys);
    int wlx_ = x - dx_;
    int wly_ = y - dy_;
    XMoveWindow(display, window, wlx_, wly_);
    return window;
}

boolean DragMethodWindow::moveWindow(
    XDisplay* display, XWindow window, int x, int y
) {
    // check to see if motion was due to pointer moving or window moving.
    if ((wlx_ == (x - dx_)) && (wly_ == (y - dy_))) {
	return false;
    }
    wlx_ = x - dx_;
    wly_ = y - dy_;
    XMoveWindow(display, window, wlx_, wly_);
    return true;
}

void DragMethodWindow::cleanup(XDisplay*, XWindow) {
    dragWindow_->unmap();
    delete dragWindow_;
}

/* class DragRep */

DragRep::DragRep(Drag* drag) : drag_(drag), dragable_(true) {
    target_ = new DragHandler(this);
    Resource::ref(target_);
}

DragRep::~DragRep() {
    Resource::unref(target_);
    target_ = nil;
    drag_ = 0;
}

boolean DragRep::event(Event& event) {
    Resource::ref(drag_);
    if (drag_->dragGlyph()) {
	method_ = &methodWindow_;
    } else {
	method_ = &methodCursor_;
    }

    Display* disp = event.display();
    XDisplay* display = disp->rep()->display_;

    XWindow window = method_->setup(display, event, drag_);
    if (window == None) {
	Resource::unref(drag_);
	return true;
    }

    int x, y;
    XWindow root;
    XWindow child;
    int wx, wy;
    unsigned int keys;
    XQueryPointer(display, window, &root, &child, &x, &y, &wx, &wy, &keys);

    XCursor cursor = None;
    Cursor* c = drag_->dragCursor();
    if (c) {
	cursor = c->rep()->xid(disp, disp->rep()->default_visual_);
    }

    Event dragEvent;
    dragEvent.display(disp);
    if (
	XGrabPointer(
	    display, window, False, (unsigned int) (ButtonMotionMask |
	    ButtonPressMask | ButtonReleaseMask), GrabModeAsync, GrabModeAsync,
	    None, cursor, CurrentTime
	) != GrabSuccess
    ) {
	// if we can't grab the pointer then we can't track the pointer properly
	// so just give up.
	return true;
    }

    XWindow last = None;
    boolean aborted = false;
    int lx = x;
    int ly = y;

    // send a drag enter event to the window that the pointer is over.
    last = translate(display, root, window, lx, ly);
    if (last != None) {
	char* value;
	int length;
	drag_->dragType(value, length);
	XEvent xevent;
	setDragProperty(
	    display, xevent, last, dragAtoms.enter(display), lx, ly,
	    value, length
	);
	XSendEvent(display, last, False, NoEventMask, &xevent);
    }

    lx = 0;
    ly = 0;
    do {
	dragEvent.read();
	if (dragEvent.type() == Event::motion) {
//          cout << "d: " << dragEvent.rep()->xevent_.xmotion.serial << endl;
// there seems to be a bug in 3.1 where this is not always done! -denis
// the effect is that the motion event coordinates are out of order and
// the window jumps around the screen to old locations. -dens
	    dragEvent.rep()->acknowledge_motion();
	    x = dragEvent.rep()->xevent_.xmotion.x_root;
	    y = dragEvent.rep()->xevent_.xmotion.y_root;

	    // check to see if motion was due to pointer or window.
	    method_->moveWindow(display, window, x, y);

	    // translate ignores the drag window.
	    int zx = x;
	    int zy = y;
	    XWindow zone = translate(display, root, window, zx, zy);
	    if ((last != None) && (last != zone)) {
		// send drag leave to last window that was dragged over.
		XEvent xevent;
		setDragProperty(
		    display, xevent, last, dragAtoms.leave(display), zx, zy
		);
		XSendEvent(display, last, False, NoEventMask, &xevent);
	    }
	    if (zone != None) {
		// send drag motion (or enter) to window that pointer is
		// being moved over.
		XEvent xevent;
		char* value;
		int length;
		drag_->dragType(value, length);
		Atom messageType = (last != zone) ?
		    dragAtoms.enter(display) : dragAtoms.motion(display);
		setDragProperty(
		    display, xevent, zone, messageType, zx, zy, value, length
		);
		XSendEvent(display, zone, False, NoEventMask, &xevent);
	    }
	    last = zone;
	    lx = zx;
	    ly = zy;
	} else if (drag_->abort(dragEvent)) {
	    if (last != None) {
		// send drag abort to window that pointer is over.
		XEvent xevent;
		setDragProperty(
		    display, xevent, last, dragAtoms.leave(display), lx, ly
		);
		XSendEvent(display, last, False, NoEventMask, &xevent);
	    }
	    aborted = true;
	} else if (dragAtoms.enter(dragEvent) || dragAtoms.motion(dragEvent) ||
	    dragAtoms.leave(dragEvent) || dragAtoms.drop(dragEvent)
	) {
	    // deliver drag events to a drag zone in this session.
	    dragEvent.handle();
	}
    } while (! (drag_->commit(dragEvent) || aborted));

    XUngrabPointer(display, CurrentTime);
    method_->cleanup(display, window);

    if (!(aborted || last == None)) {
	char* value;
	int length;
	drag_->dragData(value, length);
	XEvent xevent;
	setDragProperty(
	    display, xevent, last, dragAtoms.drop(display), lx, ly,
	    value, length
	);
	XSendEvent(display, last, False, NoEventMask, &xevent);
    }

    Resource::unref(drag_);
    return true;
}

/* class DragHandler */

DragHandler::DragHandler(DragRep* dragRep) : dragRep_(dragRep) { }

boolean DragHandler::event(Event& event) {
    return dragRep_->event(event);
}

/* class DragZoneRep */

class DragZoneRep {
public:
    DragZoneRep(DragZone* dragZone);
    ~DragZoneRep();

    boolean caught(const Event&) const;
    boolean event(Event&);

    DragZone* dragZone_;
    boolean sensitive_ : 1;
    boolean grabbing_ : 1;
    Handler* target_;
    Extension extension_;
};

/* class DragZoneHandler */

class DragZoneHandler : public Handler {
public:
    DragZoneHandler(DragZoneRep* dragZoneRep);

    virtual boolean event(Event&);
protected:
    DragZoneRep* dragZoneRep_;
};

/* class DragZone */

DragZone::DragZone(Glyph* glyph) : MonoGlyph(glyph) {
    rep_ = new DragZoneRep(this);
}

DragZone::~DragZone() {
    delete rep_;
    rep_ = 0;
}

void DragZone::sensitive(boolean sensitive) {
    rep_->sensitive_ = sensitive;
}

boolean DragZone::sensitive() const {
    return rep_->sensitive_;
}

void DragZone::enter(Event&, const char*, int) { }
void DragZone::motion(Event&) { }
void DragZone::leave(Event&) { }

void DragZone::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    MonoGlyph::allocate(c, a, ext);
    rep_->extension_ = ext;
}

void DragZone::pick(Canvas* c, const Allocation& a, int depth, Hit& hit) {
    const Event* event = hit.event();
    if (event != nil && hit.left() <= a.right() &&
	hit.right() >= a.left() && hit.bottom() <= hit.top() &&
	hit.top() >= a.bottom() && rep_->caught(*event)
    ) {
	hit.target(depth, this, 0, rep_->target_);
    }
    MonoGlyph::pick(c, a, depth + 1, hit);
}

DragZoneRep* DragZone::rep() const {
    return rep_;
}

/* class DragZoneRep */

DragZoneRep::DragZoneRep(DragZone* dragZone) : dragZone_(dragZone) {
    target_ = new DragZoneHandler(this);
    Resource::ref(target_);
    sensitive_ = true;
    grabbing_ = false;
}

DragZoneRep::~DragZoneRep() {
    Resource::unref(target_);
    target_ = nil;
}

boolean DragZoneRep::caught(const Event& event) const {
    if (! sensitive_) {
	return false;
    }
    return (
	dragAtoms.enter(event) || dragAtoms.motion(event) ||
	dragAtoms.leave(event) || dragAtoms.drop(event)
    );
}

boolean DragZoneRep::event(Event& event) {
    XEvent& xevent = event.rep()->xevent_;

    if (dragAtoms.enter(event)) {
	if (!grabbing_) {
	    event.grab(target_);
	    grabbing_ = true;
	}
	char* type;
	int length;
	getDragProperty(xevent, type, length);
	dragZone_->enter(event, type, length);
	if (type != nil) {
	    XFree(type);
	}
	return true;
    }

    if (dragAtoms.motion(event)) {
	char* type;
	int length;
	getDragProperty(xevent, type, length);
	Coord x = event.pointer_x();
	Coord y = event.pointer_y();
	boolean in = (
	    extension_.left() <= x && x <= extension_.right() &&
	    extension_.bottom() <= y && y <= extension_.top()
	);
	if (in && !grabbing_) {
	    event.grab(target_);
	    grabbing_ = true;
	    dragZone_->enter(event, type, length);
	}
	if (in) {
	    dragZone_->motion(event);
	}
	if (!in && grabbing_) {
	    dragZone_->leave(event);
	    event.ungrab(target_);
	    grabbing_ = false;
	    event.handle();
	}
	if (type) {
	    XFree(type);
	}
	return true;
    }

    if (dragAtoms.leave(event)) {
	if (grabbing_) {
	    event.ungrab(target_);
	    grabbing_ = false;
	}
	dragZone_->leave(event);
	return true;
    }

    if (! dragAtoms.drop(event)) {
	return true;
    }
    if (grabbing_) {
	event.ungrab(target_);
	grabbing_ = false;
    }

    char* buffer;
    int length;
    getDragProperty(xevent, buffer, length);
    dragZone_->drop(event, buffer, length);
    if (buffer != nil) {
	XFree(buffer);
    }

    return true;
}

/* class DragZoneHandler */

DragZoneHandler::DragZoneHandler(DragZoneRep* dragZoneRep) :
    dragZoneRep_(dragZoneRep) {
}

boolean DragZoneHandler::event(Event& event) {
    return dragZoneRep_->event(event);
}

/* class DragZoneSinkHandler */

class DragZoneSinkHandler : public Handler {
public:
    DragZoneSinkHandler(DragZoneSink* dragZoneSink);

    virtual boolean event(Event& event);
protected:
    DragZoneSink* dragZoneSink_;
};

DragZoneSinkHandler::DragZoneSinkHandler(DragZoneSink* dragZoneSink) :
    dragZoneSink_(dragZoneSink) {
}

boolean DragZoneSinkHandler::event(Event& event) {
    return dragZoneSink_->event(event);
}

/* class DragZoneSink */

DragZoneSink::DragZoneSink(Glyph* glyph) : DragZone(glyph) {
    dragPublished_ = false;
    target_ = new DragZoneSinkHandler(this);
    Resource::ref(target_);
}

DragZoneSink::~DragZoneSink() {
    Resource::unref(target_);
}

void DragZoneSink::drop(Event&, const char*, int) { }

void DragZoneSink::draw(Canvas* canvas, const Allocation& allocation) const {
    DragZone::draw(canvas, allocation);
    // publish that this X window as capable of understanding drag messages.
    if (! dragPublished_) {
	Window* window = canvas->window();
	if (window) {
	    WindowRep* rep = window->rep();
	    XDisplay* xdisplay = rep->dpy();
	    XWindow xwindow = rep->xwindow_;
	    XChangeProperty(
		xdisplay, xwindow, dragAtoms.drag(xdisplay), XA_STRING, 8,
		PropModePrepend, 0, 0
	    );
	    DragZoneSink* self = (DragZoneSink*) this;
	    self->dragPublished_ = true;
	}
    }
}

void DragZoneSink::pick(Canvas* c, const Allocation& a, int depth, Hit& hit) {
    MonoGlyph::pick(c, a, depth, hit);
    const Event* event = hit.event();
    if (event != nil &&
	(
	    dragAtoms.enter(*event) || dragAtoms.motion(*event) ||
	    dragAtoms.leave(*event) || dragAtoms.drop(*event)
	)
    ) {
	hit.target(depth, this, 0, target_);
    }
}

boolean DragZoneSink::event(Event& event) {
    if (dragAtoms.enter(event) || dragAtoms.motion(event) ||
	dragAtoms.drop(event)) {
	char* type;
	int length;
	XEvent& xevent = event.rep()->xevent_;
	getDragProperty(xevent, type, length);
	if (type != nil) {
	    XFree(type);
	}
    }
    return true;
}
