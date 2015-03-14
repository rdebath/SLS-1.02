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
 * X11-dependent window and display implementation
 */

#include "wtable.h"
#include <InterViews/bitmap.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/cursor.h>
#include <InterViews/display.h>
#include <InterViews/event.h>
#include <InterViews/handler.h>
#include <InterViews/glyph.h>
#include <InterViews/hit.h>
#include <InterViews/selection.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <IV-X11/Xlib.h>
#include <IV-X11/Xutil.h>
#include <IV-X11/Xext.h>
#include <IV-X11/xbitmap.h>
#include <IV-X11/xcanvas.h>
#include <IV-X11/xcursor.h>
#include <IV-X11/xdisplay.h>
#include <IV-X11/xevent.h>
#include <IV-X11/xselection.h>
#include <IV-X11/xwindow.h>
#include <OS/host.h>
#include <OS/list.h>
#include <OS/math.h>
#include <OS/string.h>
#include <OS/table.h>
#include <X11/Xatom.h>
#include <sys/ioctl.h>
#if defined(sun) && OSMajorVersion >= 5
#include <stropts.h>
#include <sys/conf.h>
#endif

/* no standard place for this */
extern "C" {
    extern int ioctl(int, int, ...);
}

implementPtrList(WindowVisualList,WindowVisual)

declarePtrList(WindowCursorStack,Cursor)
implementPtrList(WindowCursorStack,Cursor)

Window::Window(Glyph* g) {
    WindowRep* w = new WindowRep;
    rep_ = w;
    w->glyph_ = g;
    w->glyph_->ref();
    w->style_ = nil;
    w->display_ = nil;
    w->visual_ = nil;
    w->left_ = 0;
    w->bottom_ = 0;
    w->focus_in_ = nil;
    w->focus_out_ = nil;
    w->wm_delete_ = nil;
    w->xwindow_ = WindowRep::unbound;
    w->xattrmask_ = 0;
    w->xclass_ = InputOutput;
    w->clear_mapping_info();
    w->cursor_ = defaultCursor;
    w->cursor_stack_ = new WindowCursorStack;
    w->toplevel_ = this;
    w->canvas_ = new Canvas;
    w->canvas_->rep()->window_ = this;
}

/*
 * Construct a window with a given representation object,
 * thereby overriding the default representation.
 */

Window::Window(WindowRep* w) {
    rep_ = w;
}

/*
 * The virtual call to unbind cannot be overridden by a subclass
 * because we are calling it from the destructor.
 */

Window::~Window() {
    Window::unbind();
    WindowRep& w = *rep();
    Resource::unref_deferred(w.glyph_);
    Resource::unref_deferred(w.style_);
    Resource::unref_deferred(w.focus_in_);
    Resource::unref_deferred(w.focus_out_);
    Resource::unref_deferred(w.wm_delete_);
    delete w.canvas_;
    delete w.cursor_stack_;
    delete rep_;
    rep_ = nil;
}

Glyph* Window::glyph() const {
    WindowRep& w = *rep();
    return w.glyph_;
}

void Window::style(Style* s) {
    WindowRep& w = *rep();
    Resource::ref(s);
    Resource::unref(w.style_);
    w.style_ = s;
    w.check_binding(this);
}

Style* Window::style() const {
    WindowRep& w = *rep();
    return w.style_;
}

void Window::display(Display* d) {
    WindowRep& w = *rep();
    w.check_binding(this);
    w.display_ = d;
    w.canvas_->rep()->display_ = d;
}

Display* Window::display() const {
    WindowRep& w = *rep();
    return w.display_;
}

Canvas* Window::canvas() const {
    WindowRep& w = *rep();
    return w.canvas_;
}

void Window::cursor(Cursor* c) {
    WindowRep& w = *rep();
    w.check_binding(this);
    w.cursor_ = c;
    XWindow xw = w.xwindow_;
    if (xw != WindowRep::unbound) {
	XDisplay* dpy = w.dpy();
	if (c == nil) {
	    XUndefineCursor(dpy, xw);
	} else {
	    XDefineCursor(dpy, xw, c->rep()->xid(w.display_, w.visual_));
	}
	XFlush(dpy);
    }
}

Cursor* Window::cursor() const { return rep()->cursor_; }

void Window::push_cursor() {
    WindowRep& w = *rep();
    w.cursor_stack_->prepend(w.cursor_);
}

void Window::pop_cursor() {
    WindowRep& w = *rep();
    WindowCursorStack& s = *w.cursor_stack_;
    if (s.count() > 0) {
	cursor(s.item(0));
	s.remove(0);
    }
}

void Window::place(Coord left, Coord bottom) {
    WindowRep& w = *rep();
    if (!w.placed_ || !Math::equal(left, w.left_, float(1e-3)) ||
	!Math::equal(bottom, w.bottom_, float(1e-3))
    ) {
	w.check_binding(this);
	w.placed_ = true;
	w.left_ = left;
	w.bottom_ = bottom;
    }
}

void Window::pplace(IntCoord pleft, IntCoord pbottom) {
    WindowRep& w = *rep();
    w.check_binding(this);
    w.placed_ = true;
    Display& d = *w.display_;
    w.left_ = d.to_coord(pleft);
    w.bottom_ = d.to_coord(pbottom);
}

void Window::align(float x, float y) {
    WindowRep& w = *rep();
    if (!w.aligned_ || !Math::equal(x, w.xalign_, float(1e-3)) ||
	!Math::equal(y, w.yalign_, float(1e-3))
    ) {
	w.check_binding(this);
	w.aligned_ = true;
	w.xalign_ = x;
	w.yalign_ = y;
    }
}

Coord Window::left() const {
    WindowRep& w = *rep();
    Display* d = w.display_;
    if (d == nil) {
	return w.left_;
    }
    w.check_position(this);
    return d->to_coord(w.xpos_);
}

Coord Window::bottom() const {
    WindowRep& w = *rep();
    Display* d = w.display_;
    if (d == nil) {
	return w.bottom_;
    }
    w.check_position(this);
    return d->height() - d->to_coord(w.ypos_) - height();
}

Coord Window::width() const { return rep()->canvas_->width(); }
Coord Window::height() const { return rep()->canvas_->height(); }

void Window::map() {
    WindowRep& w = *rep();
    if (w.map_pending_ || is_mapped()) {
	return;
    }
    if (bound()) {
	w.unmapped_ = false;
	w.display_->rep()->wtable_->insert(w.xwindow_, this);
    } else {
	if (w.display_ == nil) {
	    display(Session::instance()->default_display());
	}
	if (w.style_ == nil) {
	    style(new Style(w.display_->style()));
	}
	configure();
	default_geometry();
	compute_geometry();
	bind();
	set_props();
    }
    do_map();
}

void Window::configure() { }

void Window::default_geometry() {
    WindowRep& w = *rep();
    const Display& d = *w.display_;
    w.glyph_->request(w.shape_);
    Coord width = w.shape_.requirement(Dimension_X).natural();
    Coord height = w.shape_.requirement(Dimension_Y).natural();
    w.canvas_->size(width, height);
    w.xpos_ = d.to_pixels(w.left_);
    w.ypos_ = d.pheight() - d.to_pixels(w.bottom_) - w.canvas_->pheight();
    if (w.aligned_) {
	w.xpos_ -= d.to_pixels(w.xalign_ * width);
	w.ypos_ += d.to_pixels(w.yalign_ * height);
    }
}

void Window::compute_geometry() { }

/*
 * Bind the current window description to its X counterpart, creating
 * the X window if necessary.
 */

void Window::bind() {
    WindowRep& w = *rep();
    w.do_bind(this, w.display_->rep()->root_, w.xpos_, w.ypos_);
}

/*
 * Unbind the window from its X counterpart, removing the window
 * from the xid -> window table and making sure it is not on
 * the damage list.  X will destroy subwindows automatically,
 * so there is no need to destroy the window if it has a parent.
 * In fact, it will cause an error if the windows are destroyed
 * out of order (parent destroyed before children).
 *
 * If the window is top-level, then we must undraw the associated glyph.
 * It is important that we remove the window from the lookup table
 * before calling undraw so that an interior glyph with a subwindow
 * can tell detect the top-level window is being unbound.
 */

void Window::unbind() {
    WindowRep& w = *rep();
    Display* d = w.display_;
    if (d != nil && w.xwindow_ != WindowRep::unbound) {
	DisplayRep& r = *d->rep();
	r.wtable_->remove(w.xwindow_);
	r.remove(this);
	if (w.toplevel_ == this) {
	    w.glyph_->undraw();
	    XDestroyWindow(r.display_, w.xwindow_);
	}
    }
    w.xwindow_ = WindowRep::unbound;
    w.clear_mapping_info();
    CanvasRep& c = *w.canvas_->rep();
    c.unbind();
    c.clear_damage();
}

boolean Window::bound() const {
    WindowRep& w = *rep();
    if (w.xwindow_ != WindowRep::unbound) {
	DisplayRep& d = *w.display_->rep();
	if (w.find(w.xtoplevel_, d.wtable_) == w.toplevel_) {
	    return true;
	}
    }
    return false;
}

void Window::set_attributes() {
    WindowRep& w = *rep();

    if (w.visual_ == nil) {
	w.visual_ = WindowVisual::find_visual(w.display_, w.style_);
    }

    w.xattrmask_ |= CWBackPixmap;
    w.xattrs_.background_pixmap = None;

    /*
     * It is necessary to set the border pixel to avoid trying
     * to use the parent's border.  The problem is the parent
     * might have a different visual.  Of course, none of this really
     * matters because we always use a border width of zero!
     */
    w.xattrmask_ |= CWBorderPixel;
    w.xattrs_.border_pixel = 0;

    if (w.style_->value_is_on("backingStore")) {
	w.xattrmask_ |= CWBackingStore;
	w.xattrs_.backing_store = WhenMapped;
    }

    if (w.style_->value_is_on("saveUnder")) {
	w.xattrmask_ |= CWSaveUnder;
	w.xattrs_.save_under = true;
    }

    w.xattrmask_ |= CWEventMask;
    w.xattrs_.event_mask = (
	KeyPressMask | KeyReleaseMask |
	ButtonPressMask | ButtonReleaseMask |
	PointerMotionMask | PointerMotionHintMask |
	EnterWindowMask | LeaveWindowMask |
	ExposureMask |
	StructureNotifyMask |
	FocusChangeMask |
	OwnerGrabButtonMask
    );

    /*
     * These events are caught at the top-level and not propagated
     * out to the root window (meaning the window manager).
     */
    w.xattrmask_ |= CWDontPropagate;
    w.xattrs_.do_not_propagate_mask = (
	KeyPressMask | KeyReleaseMask |
	ButtonPressMask | ButtonReleaseMask | PointerMotionMask
    );

    w.xattrmask_ |= CWColormap;
    w.xattrs_.colormap = w.visual_->colormap();

    if (w.cursor_ != nil) {
	w.xattrmask_ |= CWCursor;
	w.xattrs_.cursor = w.cursor_->rep()->xid(w.display_, w.visual_);
    }
}

void Window::set_props() { }

void Window::do_map() {
    WindowRep& w = *rep();
    DisplayRep& d = *w.display_->rep();
    XDisplay* dpy = d.display_;
    XColormap cmap = w.visual_->colormap();
    if ((w.xattrmask_ & CWOverrideRedirect) != 0 &&
	w.xattrs_.override_redirect && cmap != d.default_visual_->colormap()
    ) {
	XInstallColormap(dpy, cmap);
    }
    XMapRaised(dpy, w.xwindow_);
    w.map_pending_ = true;
}

void Window::unmap() {
    WindowRep& w = *rep();
    if (w.map_pending_ || is_mapped()) {
	DisplayRep& d = *w.display_->rep();
	w.glyph_->undraw();
	XUnmapWindow(w.dpy(), w.xwindow_);
	d.wtable_->remove(w.xwindow_);
	d.remove(this);
	w.canvas_->rep()->clear_damage();
	w.unmapped_ = true;
	w.wm_mapped_ = false;
    }
}

boolean Window::is_mapped() const {
    WindowRep& w = *rep();
    return w.wm_mapped_;
}

/*
 * Look at an event that has been received for this window.
 * Here we handle map/expose/configure events.  We also directly
 * dispatch focus and delete events.
 */

void Window::receive(const Event& e) {
    WindowRep& w = *rep();
    XEvent& xe = e.rep()->xevent_;
    Handler* handler = nil;
    SelectionManager* s;
    switch (xe.type) {
    case MapNotify:
	w.map_notify(this, xe.xmap);
	break;
    case UnmapNotify:
	w.unmap_notify(this, xe.xunmap);
	break;
    case Expose:
	w.expose(this, xe.xexpose);
	break;
    case ConfigureNotify:
	w.configure_notify(this, xe.xconfigure);
	break;
    case MotionNotify:
	/* allow next pointer motion */
	e.rep()->acknowledge_motion();
	break;
    case FocusIn:
	handler = w.focus_in_;
	break;
    case FocusOut:
	handler = w.focus_out_;
	break;
    case ClientMessage:
	if (xe.xclient.message_type == w.wm_protocols_atom() &&
	    xe.xclient.data.l[0] == w.wm_delete_atom()
	) {
	    handler = w.wm_delete_;
	    if (handler == nil) {
		Session::instance()->quit();
	    }
	}
	break;
    case SelectionRequest:
	s = w.display_->primary_selection();
	s->rep()->request(s, xe.xselectionrequest);
	break;
    case SelectionNotify:
	s = w.display_->primary_selection();
	s->rep()->notify(s, xe.xselection);
	break;
    }
    if (handler != nil) {
	Event writable_e(e);
	handler->event(writable_e);
    }
}

/*
 * Search for a handler for the given event.
 * For events that have no associated pointer location, return nil.
 * Otherwise, use pick on the glyph to find a handler.
 */

Handler* Window::target(const Event& e) const {
    EventRep& r = *e.rep();
    XEvent& xe = r.xevent_;
    if (xe.type == LeaveNotify || !r.has_pointer_location()) {
	return nil;
    }

    WindowRep& w = *rep();
    Hit hit(&e);
    w.glyph_->pick(w.canvas_, w.allocation_, 0, hit);

    Handler* h = hit.handler();
    if (h != nil &&
	(e.grabber() == nil || e.type() == Event::key || e.is_grabbing(h))
    ) {
	return h;
    }

    return nil;
}

/*
 * Grab control of the display pointer and use the given cursor
 * when it is outside the window.
 */

void Window::grab_pointer(Cursor* c) const {
    WindowRep& w = *rep();
    XGrabPointer(
	w.dpy(), w.xwindow_, True,
	(unsigned int)(
	    w.xattrs_.event_mask & (PointerMotionMask | PointerMotionHintMask)
	),
	/* pointer_mode */ GrabModeAsync, /* keyboard_mode */ GrabModeAsync,
	/* confine_to */ None,
	/* cursor */ c == nil ? None : c->rep()->xid(w.display_, w.visual_),
	CurrentTime
    );
}

/*
 * Release control of the display pointer.
 */

void Window::ungrab_pointer() const {
    WindowRep& w = *rep();
    XUngrabPointer(w.dpy(), CurrentTime);
}

/*
 * Redraw the damaged part of the window's canvas, if any.
 */

void Window::repair() {
    WindowRep& w = *rep();
    CanvasRep& c = *w.canvas_->rep();
    if (c.start_repair()) {
	w.glyph_->draw(w.canvas_, w.allocation_);
	c.finish_repair();
    }
}

void Window::raise() {
    WindowRep& w = *rep();
    XRaiseWindow(w.dpy(), w.xwindow_);
}

void Window::lower() {
    WindowRep& w = *rep();
    XLowerWindow(w.dpy(), w.xwindow_);
}

void Window::move(Coord left, Coord bottom) {
    WindowRep& w = *rep();
    Display& d = *w.display_;
    XMoveWindow(
	d.rep()->display_, w.xwindow_,
	d.to_pixels(left),
	d.pheight() - d.to_pixels(bottom) - w.canvas_->pheight()
    );
}

void Window::resize() {
    WindowRep& w = *rep();
    CanvasRep& c = *w.canvas_->rep();
    XResizeWindow(w.dpy(), w.xwindow_, c.pwidth_, c.pheight_);
    w.needs_resize_ = true;
}

/** class ManagedWindow **/

ManagedWindow::ManagedWindow(Glyph* g) : Window(g) {
    ManagedWindowRep* w = new ManagedWindowRep;
    rep_ = w;
    w->group_leader_ = nil;
    w->transient_for_ = nil;
    w->icon_ = nil;
    w->icon_bitmap_ = nil;
    w->icon_mask_ = nil;
}

ManagedWindow::~ManagedWindow() {
    delete rep_;
}

ManagedWindow* ManagedWindow::icon() const { return rep()->icon_; }
Bitmap* ManagedWindow::icon_bitmap() const { return rep()->icon_bitmap_; }
Bitmap* ManagedWindow::icon_mask() const { return rep()->icon_mask_; }

void ManagedWindow::icon(ManagedWindow* i) {
    ManagedWindowRep& w = *rep();
    w.icon_ = i;
    w.do_set(this, &ManagedWindowRep::set_icon);
}

void ManagedWindow::icon_bitmap(Bitmap* b) {
    ManagedWindowRep& w = *rep();
    w.icon_bitmap_ = b;
    w.do_set(this, &ManagedWindowRep::set_icon_bitmap);
}

void ManagedWindow::icon_mask(Bitmap* b) {
    ManagedWindowRep& w = *rep();
    w.icon_mask_ = b;
    w.do_set(this, &ManagedWindowRep::set_icon_mask);
}

void ManagedWindow::iconify() {
    WindowRep& w = *Window::rep();
    XWindow xw = w.xwindow_;
    if (xw != WindowRep::unbound) {
	XEvent xe;
	static Atom a = None;
	DisplayRep& r = *w.canvas_->rep()->display_->rep();
	XDisplay* dpy = r.display_;

	if (a == None) {
	    a = XInternAtom(dpy, "WM_CHANGE_STATE", False);
	}
	xe.type = ClientMessage;
	xe.xclient.type = ClientMessage;
	xe.xclient.display = dpy;
	xe.xclient.window = xw;
	xe.xclient.message_type = a;
	xe.xclient.format = 32;
	xe.xclient.data.l[0] = IconicState;
	XSendEvent(
	    dpy, r.root_, False,
	    SubstructureRedirectMask | SubstructureNotifyMask, &xe
	);
    }
}

void ManagedWindow::deiconify() {
    WindowRep& w = *Window::rep();
    XWindow xw = w.xwindow_;
    if (xw != WindowRep::unbound) {
	XMapWindow(w.dpy(), xw);
    }
}

void ManagedWindow::resize() {
    default_geometry();
    ManagedWindowRep& w = *rep();
    w.wm_normal_hints(this);
    Window::resize();
}

void ManagedWindow::focus_event(Handler* in, Handler* out) {
    WindowRep& w = *Window::rep();
    Resource::ref(in);
    Resource::ref(out);
    Resource::unref(w.focus_in_);
    Resource::unref(w.focus_out_);
    w.focus_in_ = in;
    w.focus_out_ = out;
}

void ManagedWindow::wm_delete(Handler* h) {
    WindowRep& w = *Window::rep();
    Resource::ref(h);
    Resource::unref(w.wm_delete_);
    w.wm_delete_ = h;
}

void ManagedWindow::compute_geometry() {
    WindowRep& wr = *Window::rep();
    CanvasRep& c = *wr.canvas_->rep();
    Display& d = *wr.display_;
    if (c.pwidth_ <= 0) {
	c.width_ = 72;
	c.pwidth_ = d.to_pixels(c.width_);
    }
    if (c.pheight_ <= 0) {
	c.height_ = 72;
	c.pheight_ = d.to_pixels(c.height_);
    }
}

void ManagedWindow::set_props() {
    ManagedWindowRep& w = *rep();
    w.wm_normal_hints(this);
    w.wm_name(this);
    w.wm_class(this);
    w.wm_protocols(this);
    w.wm_colormap_windows(this);
    w.wm_hints(this);
}

/** class ApplicationWindow **/

ApplicationWindow::ApplicationWindow(Glyph* g) : ManagedWindow(g) { }
ApplicationWindow::~ApplicationWindow() { }

void ApplicationWindow::compute_geometry() {
    WindowRep& wr = *Window::rep();
    CanvasRep& c = *wr.canvas_->rep();
    Display& d = *wr.display_;
    unsigned int spec = 0;
    String v;
    if (wr.style_ != nil && wr.style_->find_attribute("geometry", v)) {
	NullTerminatedString g(v);
	unsigned int xw, xh;
	spec = XParseGeometry(g.string(), &wr.xpos_, &wr.ypos_, &xw, &xh);
	const unsigned int userplace = XValue | YValue;
	if ((spec & userplace) == userplace) {
	    wr.placed_ = true;
	}
	if ((spec & WidthValue) != 0) {
	    c.pwidth_ = PixelCoord(xw);
	    c.width_ = d.to_coord(c.pwidth_);
	}
	if ((spec & HeightValue) != 0) {
	    c.pheight_ = PixelCoord(xh);
	    c.height_ = d.to_coord(c.pheight_);
	}
	if ((spec & XValue) != 0 && (spec & XNegative) != 0) {
	    wr.xpos_ = d.pwidth() + wr.xpos_ - c.pwidth_;
	}
	if ((spec & YValue) != 0 && (spec & YNegative) != 0) {
	    wr.ypos_ = d.pheight() + wr.ypos_ - c.pheight_;
	}
    }
    ManagedWindow::compute_geometry();
}

void ApplicationWindow::set_props() {
    WindowRep& w = *Window::rep();
    Session& s = *Session::instance();
    Display* d = w.display_;
    if (d == nil) {
	d = s.default_display();
    }
    XSetCommand(d->rep()->display_, w.xwindow_, s.argv(), s.argc());
    ManagedWindow::set_props();
}

/** class TopLevelWindow **/

TopLevelWindow::TopLevelWindow(Glyph* g) : ManagedWindow(g) { }
TopLevelWindow::~TopLevelWindow() { }

void TopLevelWindow::group_leader(Window* primary) {
    ManagedWindowRep& w = *rep();
    w.group_leader_ = primary;
    w.do_set(this, &ManagedWindowRep::set_group_leader);
}

Window* TopLevelWindow::group_leader() const { return rep()->group_leader_; }

void TopLevelWindow::set_props() {
    ManagedWindow::set_props();
}

/** class TransientWindow **/

TransientWindow::TransientWindow(Glyph* g) : TopLevelWindow(g) { }
TransientWindow::~TransientWindow() { }

void TransientWindow::transient_for(Window* primary) {
    ManagedWindowRep& w = *rep();
    w.transient_for_ = primary;
    w.do_set(this, &ManagedWindowRep::set_transient_for);
}

Window* TransientWindow::transient_for() const {
    return rep()->transient_for_;
}

/*
 * Don't do the normal geometry property lookup, etc. for transients.
 */

void TransientWindow::configure() {
    Window::configure();
}

void TransientWindow::set_attributes() {
    Style& s = *style();
    s.alias("TransientWindow");
    TopLevelWindow::set_attributes();
}

/** class PopupWindow **/

PopupWindow::PopupWindow(Glyph* g) : Window(g) { }
PopupWindow::~PopupWindow() { }

void PopupWindow::set_attributes() {
    Style& s = *style();
    s.alias("PopupWindow");
    Window::set_attributes();
    WindowRep& w = *rep();
    w.xattrmask_ |= CWOverrideRedirect;
    w.xattrs_.override_redirect = True;
}

/** class IconWindow **/

IconWindow::IconWindow(Glyph* g) : ManagedWindow(g) { }
IconWindow::~IconWindow() { }

void IconWindow::do_map() { }

/** class WindowRep **/

/*
 * Hopefully, these atoms will have the same values on all displays.
 */

XDisplay* WindowRep::dpy() { return display_->rep()->display_; }

Atom WindowRep::wm_delete_atom_ = None;

Atom WindowRep::wm_delete_atom() {
    if (wm_delete_atom_ == None) {
	wm_delete_atom_ = XInternAtom(dpy(), "WM_DELETE_WINDOW", False);
    }
    return wm_delete_atom_;
}

Atom WindowRep::wm_protocols_atom_ = None;

Atom WindowRep::wm_protocols_atom() {
    if (wm_protocols_atom_ == None) {
	wm_protocols_atom_ = XInternAtom(dpy(), "WM_PROTOCOLS", False);
    }
    return wm_protocols_atom_;
}

void WindowRep::clear_mapping_info() {
    xtoplevel_ = WindowRep::unbound;
    needs_resize_ = false;
    resized_ = false;
    placed_ = false;
    aligned_ = false;
    moved_ = false;
    unmapped_ = false;
    wm_mapped_ = false;
    map_pending_ = false;
}

void WindowRep::map_notify(Window*, XMapEvent&) {
    needs_resize_ = true;
    wm_mapped_ = true;
    map_pending_ = false;
    /* for backward compatibility */
    canvas_->rep()->status_ = Canvas::mapped;
}

/*
 * We can only see an unmap if it is generated external (e.g.,
 * by a window manager).  Application unmaps will unbind the window,
 * thus removing it from the xid->window table.
 */

void WindowRep::unmap_notify(Window*, XUnmapEvent&) {
    glyph_->undraw();
    wm_mapped_ = false;
    /* for backward compatibility */
    canvas_->rep()->status_ = Canvas::unmapped;
}

/*
 * Handle an expose event.  Because window managers generate a variety
 * of event sequences in response to maps, We defer the first resize
 * until when a window is first exposed.
 */

void WindowRep::expose(Window* w, XExposeEvent& xe) {
    unsigned int pw = canvas_->pwidth();
    unsigned int ph = canvas_->pheight();
    if (needs_resize_) {
	needs_resize_ = false;
	resize(w, pw, ph);
    } else {
	Display* d = display_;
	Coord l = d->to_coord(xe.x);
	Coord r = l + d->to_coord(xe.width);
	Coord t = d->to_coord(ph - xe.y);
	Coord b = t - d->to_coord(xe.height);
	canvas_->redraw(l, b, r, t);
    }
}

/*
 * Handle an X ConfigureNotify event.  If the window has been configured
 * once already, then only resize it if the new size is different.
 * If it hasn't been configured once, then note the size and we'll take
 * care of it when the first expose event is handled.
 */

void WindowRep::configure_notify(Window* w, XConfigureEvent& xe) {
    moved_ = true;
    if (resized_) {
	if (xe.width != canvas_->pwidth() || xe.height != canvas_->pheight()) {
	    resize(w, xe.width, xe.height);
	}
    } else {
	canvas_->psize(xe.width, xe.height);
	needs_resize_ = true;
    }
}

/*
 * Note that a window has moved.
 */

void WindowRep::move(Window*, int x, int y) {
    xpos_ = x;
    ypos_ = y;
    moved_ = false;
}

/*
 * Resize a window, allocating the associated glyph and
 * damaging the new area.
 */

void WindowRep::resize(Window* w, unsigned int xwidth, unsigned int xheight) {
    canvas_->psize(xwidth, xheight);
    canvas_->damage_all();
    const Requirement& rx = shape_.requirement(Dimension_X);
    const Requirement& ry = shape_.requirement(Dimension_Y);
    Coord xsize = canvas_->width();
    Coord ysize = canvas_->height();
    Coord ox = xsize * rx.alignment();
    Coord oy = ysize * ry.alignment();
    allocation_.allot(Dimension_X, Allotment(ox, xsize, ox / xsize));
    allocation_.allot(Dimension_Y, Allotment(oy, ysize, oy / ysize));
    Extension ext;
    ext.clear();
    init_renderer(w);
    if (resized_) {
	glyph_->undraw();
    }
    glyph_->allocate(canvas_, allocation_, ext);
    resized_ = true;
}

void WindowRep::check_position(const Window*) {
    if (moved_) {
	DisplayRep& d = *display_->rep();
	int x, y;
	XWindow xw;
	XTranslateCoordinates(
	    d.display_, xwindow_, d.root_, 0, 0, &x, &y, &xw
	);
	xpos_ = x;
	ypos_ = y;
	moved_ = false;
    }
}

void WindowRep::check_binding(Window* w) {
    if (unmapped_) {
	w->unbind();
    }
}

void WindowRep::do_bind(Window* w, XWindow parent, int left, int top) {
    CanvasRep& c = *canvas_->rep();
    DisplayRep& d = *display_->rep();
    XDisplay* dpy = d.display_;
    WindowTable& t = *d.wtable_;
    if (xwindow_ != WindowRep::unbound) {
	t.remove(xwindow_);
    }
    w->set_attributes();
    xwindow_ = XCreateWindow(
	dpy, parent, left, top, canvas_->pwidth(), canvas_->pheight(),
	/* border width */ 0, visual_->depth(), xclass_,
	visual_->visual(), xattrmask_, &xattrs_
    );
    c.xdrawable_ = xwindow_;
    t.insert(xwindow_, w);
    xtoplevel_ = toplevel_->rep()->xwindow_;
}

void WindowRep::init_renderer(Window* w) {
    CanvasRep& c = *w->canvas()->rep();
    c.unbind();
    c.bind(style_->value_is_on("double_buffered"));
}

Window* WindowRep::find(XWindow xw, WindowTable* t) {
    Window* window;
    if (t->find(window, xw)) {
	WindowRep& w = *window->rep();
	Window* toplevel;
	if (t->find(toplevel, w.xtoplevel_) && toplevel == w.toplevel_) {
	    return window;
	}
    }
    return nil;
}

/* class ManagedWindowRep */

void ManagedWindowRep::do_set(Window* window, HintFunction f) {
    WindowRep& w = *window->rep();
    ManagedWindowHintInfo info;
    info.xwindow_ = w.xwindow_;
    if (info.xwindow_ != WindowRep::unbound) {
	info.style_ = w.style_;
	info.dpy_ = w.dpy();
	info.hints_ = XGetWMHints(info.dpy_, info.xwindow_);
	if (info.hints_ == nil) {
	    info.hints_ = XAllocWMHints();
	}
	info.pwidth_ = w.canvas_->pwidth();
	info.pheight_ = w.canvas_->pheight();
	info.display_ = w.display_;
	if ((this->*f)(info)) {
	    XSetWMHints(info.dpy_, info.xwindow_, info.hints_);
	}
	XFree((char*)info.hints_);
    }
}

boolean ManagedWindowRep::set_name(ManagedWindowHintInfo& info) {
    if (info.style_ != nil) {
	Style& s = *info.style_;
	String v;
	if (s.find_attribute("name", v) || s.find_attribute("title", v)) {
	    NullTerminatedString ns(v);
	    XStoreName(info.dpy_, info.xwindow_, ns.string());
	}
    }
    return false;
}

boolean ManagedWindowRep::set_geometry(ManagedWindowHintInfo&) {
    /* unimplemented: should configure mapped windows */
    return false;
}

boolean ManagedWindowRep::set_group_leader(ManagedWindowHintInfo& info) {
    if (group_leader_ == nil) {
	info.hints_->flags &= ~WindowGroupHint;
	info.hints_->window_group = None;
	return true;
    } else {
	XWindow g = group_leader_->rep()->xwindow_;
	if (g != WindowRep::unbound) {
	    info.hints_->flags |= WindowGroupHint;
	    info.hints_->window_group = g;
	    return true;
	}
    }
    return false;
}

boolean ManagedWindowRep::set_transient_for(ManagedWindowHintInfo& info) {
    if (transient_for_ != nil) {
	XDrawable td = transient_for_->rep()->xwindow_;
	if (td != WindowRep::unbound) {
	    XSetTransientForHint(info.dpy_, info.xwindow_, td);
	    return true;
	}
    }
    return false;
}

boolean ManagedWindowRep::set_icon(ManagedWindowHintInfo& info) {
    if (icon_ == nil) {
	info.hints_->flags &= ~IconWindowHint;
	info.hints_->icon_window = None;
	return true;
    } else {
	XWindow i = icon_->Window::rep()->xwindow_;
	if (i != WindowRep::unbound) {
	    info.hints_->flags |= IconWindowHint;
	    info.hints_->icon_window = i;
	    return true;
	}
    }
    return false;
}

boolean ManagedWindowRep::set_icon_name(ManagedWindowHintInfo& info) {
    if (info.style_ != nil) {
	Style& s = *info.style_;
	String v;
	if (s.find_attribute("iconName", v) || s.find_attribute("name", v)) {
	    NullTerminatedString ns(v);
	    XSetIconName(info.dpy_, info.xwindow_, ns.string());
	}
    }
    return false;
}

boolean ManagedWindowRep::set_icon_geometry(ManagedWindowHintInfo& info) {
    info.hints_->flags &= ~IconPositionHint;
    String g;
    if (!info.style_->find_attribute("iconGeometry", g)) {
	if (icon_ == nil) {
	    return false;
	}
	Style* s = icon_->style();
	if (s == nil || !s->find_attribute("geometry", g)) {
	    return false;
	}
    }
    NullTerminatedString ns(g);
    int x = 0, y = 0;
    unsigned int w = info.pwidth_;
    unsigned int h = info.pheight_;
    if (icon_bitmap_ != nil) {
	w = icon_bitmap_->pwidth();
	h = icon_bitmap_->pheight();
    }
    if (icon_ != nil) {
	w = icon_->canvas()->pwidth();
	h = icon_->canvas()->pheight();
    }
    unsigned int p = XParseGeometry(ns.string(), &x, &y, &w, &h);
    Display& d = *info.display_;
    if ((p & XNegative) != 0) {
	x = d.pwidth() + x - w;
    }
    if ((p & YNegative) != 0) {
	y = d.pheight() + y - h;
    }
    if ((p & (XValue|YValue)) != 0) {
	info.hints_->flags |= IconPositionHint;
	info.hints_->icon_x = x;
	info.hints_->icon_y = y;
	return true;
    }
    return false;
}

boolean ManagedWindowRep::set_icon_bitmap(ManagedWindowHintInfo& info) {
    if (icon_bitmap_ == nil) {
	info.hints_->flags &= ~IconPixmapHint;
	info.hints_->icon_pixmap = None;
    } else {
	info.hints_->flags |= IconPixmapHint;
	info.hints_->icon_pixmap = icon_bitmap_->rep()->pixmap_;
    }
    return true;
}

boolean ManagedWindowRep::set_icon_mask(ManagedWindowHintInfo& info) {
    if (icon_mask_ == nil) {
	info.hints_->flags &= ~IconMaskHint;
	info.hints_->icon_mask = None;
    } else {
	info.hints_->flags |= IconMaskHint;
	info.hints_->icon_mask = icon_mask_->rep()->pixmap_;
    }
    return true;
}

boolean ManagedWindowRep::set_all(ManagedWindowHintInfo& info) {
    Style* s = info.style_;
    XWMHints& h = *info.hints_;
    h.flags = InputHint;
    h.input = True;
    h.flags |= StateHint;
    h.initial_state =
	(s != nil && s->value_is_on("iconic")) ? IconicState : NormalState;
    set_name(info);
    set_geometry(info);
    set_group_leader(info);
    set_transient_for(info);
    set_icon_name(info);
    set_icon_geometry(info);
    set_icon(info);
    set_icon_bitmap(info);
    set_icon_mask(info);
    return true;
}

void ManagedWindowRep::wm_normal_hints(Window* window) {
    WindowRep& w = *window->rep();
    const Display& d = *w.display_;
    unsigned int cpwidth = w.canvas_->pwidth();
    unsigned int cpheight = w.canvas_->pheight();
    XSizeHints sizehints;
    if (w.placed_) {
	sizehints.flags = USPosition | USSize;
    } else {
	sizehints.flags = PSize | PBaseSize;
    }
    /* obsolete as of R4, but kept for backward compatibility */
    sizehints.x = w.xpos_;
    sizehints.y = w.ypos_;
    sizehints.width = cpwidth;
    sizehints.height = cpheight;

    sizehints.base_width = cpwidth;
    sizehints.base_height = cpheight;

    const Coord smallest = d.to_coord(2);
    const Coord x_largest = d.width();
    const Coord y_largest = d.height();
    Requirement& rx = w.shape_.requirement(Dimension_X);
    Requirement& ry = w.shape_.requirement(Dimension_Y);

    Coord min_width = Math::min(
	x_largest, Math::max(smallest, rx.natural() - rx.shrink())
    );
    Coord min_height = Math::min(
	y_largest, Math::max(smallest, ry.natural() - ry.shrink())
    );
    sizehints.flags |= PMinSize;
    sizehints.min_width = d.to_pixels(min_width);
    sizehints.min_height = d.to_pixels(min_height);

    Coord max_width = Math::max(smallest, rx.natural() + rx.stretch());
    Coord max_height = Math::max(smallest, ry.natural() + ry.stretch());
    if (max_width < x_largest || max_height < y_largest) {
	sizehints.flags |= PMaxSize;
	sizehints.max_width = d.to_pixels(Math::min(max_width, x_largest));
	sizehints.max_height = d.to_pixels(Math::min(max_height, y_largest));
    }

    /* PResizeInc: width_inc, height_inc */
    /* PAspect: {min_aspect,max_aspect}.{x,y} */

    XSetNormalHints(w.dpy(), w.xwindow_, &sizehints);
}

void ManagedWindowRep::wm_name(Window* window) {
    WindowRep& w = *window->rep();
    Style* s = w.style_;
    String v;
    if (!s->find_attribute("name", v) && !s->find_attribute("title", v)) {
	s->attribute("name", Session::instance()->name());
    }
    String hostname(Host::name());
    XChangeProperty(
	w.dpy(), w.xwindow_, XA_WM_CLIENT_MACHINE,
	XA_STRING, 8, PropModeReplace,
	(unsigned char*)(hostname.string()), hostname.length()
    );
}

void ManagedWindowRep::wm_class(Window* window) {
    WindowRep& w = *window->rep();
    Style* s = w.style_;
    XClassHint classhint;
    String v("Noname");
    if (!s->find_attribute("name", v)) {
	s->find_attribute("title", v);
    }
    NullTerminatedString ns(v);
    classhint.res_name = (char*)ns.string();
    classhint.res_class = (char*)(Session::instance()->classname());
    XSetClassHint(w.dpy(), w.xwindow_, &classhint);
}

void ManagedWindowRep::wm_protocols(Window* window) {
    WindowRep& w = *window->rep();
    Atom a = w.wm_delete_atom();
    XSetWMProtocols(w.dpy(), w.xwindow_, &a, 1);
}

void ManagedWindowRep::wm_colormap_windows(Window*) {
    /* we do not currently manipulate colormaps */
}

void ManagedWindowRep::wm_hints(Window* window) {
    do_set(window, &ManagedWindowRep::set_all);
}

/* class WindowVisual */

declareTable(ColorTable,unsigned long,XColor)
implementTable(ColorTable,unsigned long,XColor)

class RGBTableEntry {
public:
    unsigned short red_;
    unsigned short green_;
    unsigned short blue_;

    unsigned long hash() const;
    boolean operator ==(const RGBTableEntry&) const;
    boolean operator !=(const RGBTableEntry&) const;
};

inline unsigned long key_to_hash(const RGBTableEntry& k) { return k.hash(); }

unsigned long RGBTableEntry::hash() const {
    return (red_ >> 7) ^ (green_ >> 7) ^ (blue_ >> 7);
}

boolean RGBTableEntry::operator ==(const RGBTableEntry& rgb) const {
    return red_ == rgb.red_ && green_ == rgb.green_ && blue_ == rgb.blue_;
}

boolean RGBTableEntry::operator !=(const RGBTableEntry& rgb) const {
    return red_ != rgb.red_ || green_ != rgb.green_ || blue_ != rgb.blue_;
}

declareTable(RGBTable,RGBTableEntry,XColor)
implementTable(RGBTable,RGBTableEntry,XColor)

WindowVisual::WindowVisual(const WindowVisualInfo& info) {
    WindowVisualInfo& i = info_;
    i = info;
    if (i.visual_ == nil) {
	i.visual_ = DefaultVisual(i.display_, i.screen_);
	i.cmap_ = DefaultColormap(i.display_, i.screen_);
    } else {
	i.cmap_ = XCreateColormap(
	    i.display_, RootWindow(i.display_, i.screen_), i.visual_, AllocNone
	);
    }
    init_color_tables();
}

WindowVisual::~WindowVisual() {
    delete ctable_;
    delete rgbtable_;
    delete [] localmap_;
}

WindowVisual* WindowVisual::find_visual(Display* d, Style* s) {
    WindowVisualInfo info;
    DisplayRep& dr = *d->rep();
    XDisplay* dpy = dr.display_;
    info.display_ = dpy;
    info.screen_ = dr.screen_;
    info.depth_ = DefaultDepth(dpy, dr.screen_);
    WindowVisualList& wvlist = dr.visuals_;
    info.visual_ = wvlist.count() == 0 ? nil : dr.default_visual_->visual();
    info.overlay_.id_ = 0x0;
    String v;
    int layer;
    if (s->find_attribute("visual_id", v)) {
	long id;
	if (v.convert(id)) {
	    XVisualInfo xinfo;
	    xinfo.visualid = id;
	    find_visual_by_info(xinfo, VisualIDMask, info);
	}
    } else if (s->find_attribute("visual", v)) {
	find_visual_by_class_name(v, info);
    } else if (s->find_attribute("overlay", v) && find_layer(v, layer)) {
	for (ListItr(WindowVisualList) i(wvlist); i.more(); i.next()) {
	    WindowVisual* wv = i.cur();
	    WindowOverlayInfo& ov = wv->info_.overlay_;
	    if (ov.id_ != 0x0 && (layer == 0 || ov.layer_ == layer)) {
		return wv;
	    }
	}
	find_overlay(layer, info);
    }
    WindowVisual* wv;
    for (ListItr(WindowVisualList) i(wvlist); i.more(); i.next()) {
	wv = i.cur();
	if (wv->visual() == info.visual_) {
	    return wv;
	}
    }
    wv = new WindowVisual(info);
    wvlist.append(wv);
    return wv;
}

/*
 * Lookup a visual by class name.
 */

struct VisualTable {
    char* class_name;
    int class_tag;
};

static VisualTable visual_classes[] = {
    { "TrueColor", TrueColor },
    { "PseudoColor", PseudoColor },
    { "StaticGray", StaticGray },
    { "GrayScale", GrayScale },
    { "StaticColor", StaticColor },
    { "DirectColor", DirectColor },
    { nil, -1 }
};

void WindowVisual::find_visual_by_class_name(
    const String& name, WindowVisualInfo& info
) {
    for (VisualTable* v = &visual_classes[0]; v->class_name != nil; v++) {
	if (name == v->class_name) {
	    XVisualInfo xinfo;
	    xinfo.c_class = v->class_tag;
	    find_visual_by_info(xinfo, VisualClassMask, info);
	    return;
	}
    }
}

boolean WindowVisual::find_layer(const String& v, int& layer) {
    if (v.convert(layer)) {
	return true;
    }
    if (v.case_insensitive_equal("true") || v.case_insensitive_equal("yes")) {
	layer = 0;
	return true;
    }
    return false;
}

/*
 * Look for an overlay visual.  There is no X standard way to do this,
 * so we rely on the convention that the SERVER_OVERLAY_VISUALS property
 * on the root window contains a list of overlay visuals.  Each visual
 * has 4 pieces of information: the visual id, the type of transparency,
 * the pixel or mask for transparency, and the overlay layer.  Layers
 * are numbered from top-to-bottom.
 */

void WindowVisual::find_overlay(int layer, WindowVisualInfo& info) {
    XDisplay* dpy = info.display_;
    Atom actual_type;
    int actual_format;
    unsigned long nitems, bytes_after;
    WindowOverlayInfo* overlay_visuals = nil;

    if (
	XGetWindowProperty(
	    dpy, RootWindow(dpy, info.screen_),
	    XInternAtom(dpy, "SERVER_OVERLAY_VISUALS", False),
	    /* offset */ 0L, /* length */ (long)1000000, /* delete */ False,
	    AnyPropertyType, &actual_type, &actual_format, &nitems,
	    &bytes_after, (unsigned char**)&overlay_visuals
	) != Success
    ) {
	return;
    }
    if (actual_type != None && actual_format == 32 && nitems >= 4) {
	unsigned long noverlays = nitems >> 2;
	for (unsigned long i = 0; i < noverlays; i++) {
	    if (layer == 0 || overlay_visuals[i].layer_ == layer) {
		/*
		 * Alas, we must query the visual info to find out
		 * the depth of the overlay.
		 */
		XVisualInfo xinfo;
		xinfo.visualid = overlay_visuals[i].id_;
		find_visual_by_info(xinfo, VisualIDMask, info);
		info.overlay_ = overlay_visuals[i];
		break;
	    }
	}
    }
    if (overlay_visuals != nil) {
	XFree((char*)overlay_visuals);
    }
}

/*
 * Look for a visual matching the given xinfo.
 */

void WindowVisual::find_visual_by_info(
    XVisualInfo& xinfo, long mask, WindowVisualInfo& info
) {
    xinfo.screen = info.screen_;
    int nvisuals = 0;
    XVisualInfo* visuals = XGetVisualInfo(
	info.display_, VisualScreenMask | mask, &xinfo, &nvisuals
    );
    if (visuals != nil) {
	if (nvisuals > 0) {
	    info.depth_ = visuals[0].depth;
	    info.visual_ = visuals[0].visual;
	}
	XFree((char*)visuals);
    }
}

/*
 * Set up appropriate color mapping tables for the visual.
 * For TrueColor, we don't need an rgb->pixel table because we can
 * compute the pixel value directly.  The pixel->rgb table (ctable_)
 * is used by Raster::peek.
 *
 * The table sizes are 512 = 2 (hash tables work best half full) times
 * 256 (most non-TrueColor systems are 8-bit).
 */

void WindowVisual::init_color_tables() {
    ctable_ = new ColorTable(512);
    localmap_ = nil;
    localmapsize_ = 0;
    Visual& v = *info_.visual_;
    switch (v.c_class) {
    case TrueColor:
	rgbtable_ = nil;
	set_shift(v.red_mask, red_, red_shift_);
	set_shift(v.green_mask, green_, green_shift_);
	set_shift(v.blue_mask, blue_, blue_shift_);
	break;
    default:
	rgbtable_ = new RGBTable(512);
	if (v.c_class == PseudoColor && v.map_entries < 16) {
	    XColor xc;
	    find_color(0, 0, 0, xc);
	    find_color(0xffff, 0xffff, 0xffff, xc);
	}
	break;
    }
}

void WindowVisual::set_shift(
    unsigned long mask, unsigned long& v, unsigned long& shift
) {
    shift = 0;
    v = mask;
    while ((v & 0x1) == 0) {
	shift += 1;
	v >>= 1;
    }
}

/*
 * Compute a reasonable pixel for xor'ing.  Note that this should be done
 * after the visual is selected to handle the DirectColor case correctly.
 */

inline unsigned int WindowVisual::MSB(unsigned long i) {
    return (i ^ (i>>1)) & i;
}

unsigned long WindowVisual::xor(const Style& s) const {
    unsigned long p;
    String custom;
    if (s.find_attribute("RubberbandPixel", custom)) {
	long n = 1;
	custom.convert(n);
	p = n;
    } else if (info_.visual_->c_class == DirectColor) {
        p = (
            MSB(info_.visual_->red_mask) |
	    MSB(info_.visual_->green_mask) |
            MSB(info_.visual_->blue_mask)
        );
    } else {
	XDisplay* dpy = info_.display_;
	int s = info_.screen_;
	p = BlackPixel(dpy, s) ^ WhitePixel(dpy, s);
    }
    return p;
}

/*
 * Find the X color information for a given pixel value.
 * If it is already in the color table, just retrieve.
 * Otherwise, we have to query X.
 */

void WindowVisual::find_color(unsigned long pixel, XColor& xc) {
    if (!ctable_->find(xc, pixel)) {
	xc.pixel = pixel;
	XQueryColor(info_.display_, info_.cmap_, &xc);
	ctable_->insert(pixel, xc);
    }
}

double WindowVisual::distance(
    unsigned short r, unsigned short g, unsigned short b, const XColor& xc
) {
    double scale = 1.0 / double(0x10000);
    double rr = (double(r) - double(xc.red)) * scale;
    double gg = (double(g) - double(xc.green)) * scale;
    double bb = (double(b) - double(xc.blue)) * scale;
    return rr*rr + gg*gg + bb*bb;
}

inline unsigned long WindowVisual::rescale(
    unsigned long value, unsigned long in_scale, unsigned long out_scale
) {
    return (value * out_scale + in_scale/2) / in_scale;
}

/*
 * Find the X color information for a specified rgb.
 * For a TrueColor visual, this is easy (computed directly from rgb values).
 * Otherwise, we have to do an XAllocColor if we haven't seen the rgb
 * combination before.  If XAllocColor fails, then we read the colormap and
 * try to find the best match.  Note this may cause havoc if the colormap
 * entries are read/write.
 */

void WindowVisual::find_color(
    unsigned short red, unsigned short green, unsigned short blue, XColor& xc
) {
    unsigned long r, g, b;
    switch (info_.visual_->c_class) {
    case TrueColor:
	r = rescale(red, 0xffff, red_);
	g = rescale(green, 0xffff, green_);
	b = rescale(blue, 0xffff, blue_);
	xc.pixel = (
	    (r << red_shift_) | (g << green_shift_) | (b << blue_shift_)
	);
	xc.red = (unsigned short)rescale(r, red_, 0xffff);
	xc.green = (unsigned short)rescale(g, green_, 0xffff);
	xc.blue = (unsigned short)rescale(b, blue_, 0xffff);
	break;
    default:
	RGBTableEntry rgb;
	rgb.red_ = red;
	rgb.green_ = green;
	rgb.blue_ = blue;
	if (!rgbtable_->find(xc, rgb)) {
	    if (localmapsize_ == 0) {
		xc.red = red;
		xc.green = green;
		xc.blue = blue;
		if (!XAllocColor(info_.display_, info_.cmap_, &xc)) {
		    localmapsize_ = Math::min(info_.visual_->map_entries, 256);
		    localmap_ = new XColor[localmapsize_];
		    for (unsigned long p = 0; p < localmapsize_; p++) {
			localmap_[p].pixel = p;
		    }
		    XQueryColors(
			info_.display_, info_.cmap_, localmap_, localmapsize_
		    );
		}
	    }
	    if (localmapsize_ != 0) {
		unsigned long best = 0;
		double best_match = 0.0;
		boolean matched = false;
		for (unsigned long p = 0; p < localmapsize_; p++) {
		    const WindowOverlayInfo& ov = info_.overlay_;
		    if (ov.id_ == 0x0 || ov.transparent_ != p) {
			double d = distance(red, green, blue, localmap_[p]);
			if (!matched || d < best_match) {
			    best = p;
			    best_match = d;
			    matched = true;
			}
		    }
		}
		xc = localmap_[best];
	    }
	    rgbtable_->insert(rgb, xc);
	}
    }
}

/* class Display */

declarePtrList(DamageList,Window)
implementPtrList(DamageList,Window)

class GrabInfo {
private:
    friend class Display;
    friend class DisplayRep;

    Window* window_;
    Handler* handler_;
};

declareList(GrabList,GrabInfo)
implementList(GrabList,GrabInfo)

declarePtrList(SelectionList,SelectionManager)
implementPtrList(SelectionList,SelectionManager)

implementTable(WindowTable,XWindow,Window*)

Display::Display(DisplayRep* d) {
    rep_ = d;
}

Display* Display::open(const String& s) {
    NullTerminatedString ns(s);
    return open(ns.string());
}

Display* Display::open() {
    return open(nil);
}

Display* Display::open(const char* device) {
    XDisplay* dpy = XOpenDisplay(device);
    if (dpy == nil) {
	return nil;
    }
    DisplayRep* d = new DisplayRep;
    d->display_ = dpy;
    d->screen_ = DefaultScreen(d->display_);
    d->style_ = nil;
    d->grabbers_ = new GrabList;
    d->damaged_ = new DamageList;
    d->selections_ = new SelectionList;
    d->wtable_ = new WindowTable(256);
    return new Display(d);
}

void Display::close() {
    DisplayRep* d = rep();
    XCloseDisplay(d->display_);
}

Display::~Display() {
    DisplayRep* d = rep();
    Resource::unref_deferred(d->style_);
    delete d->damaged_;
    for (ListItr(SelectionList) i(*d->selections_); i.more(); i.next()) {
	SelectionManager* s = i.cur();
	delete s;
    }
    delete d->selections_;
    delete d->grabbers_;
    delete d->wtable_;
    delete d;
}

int Display::fd() const { return ConnectionNumber(rep()->display_); }
Coord Display::width() const { return rep()->width_; }
Coord Display::height() const { return rep()->height_; }
PixelCoord Display::pwidth() const { return rep()->pwidth_; }
PixelCoord Display::pheight() const { return rep()->pheight_; }

/*
 * Convert millimeters to points.  We use 72.0 pts/in and 25.4 mm/in.
 */

static inline double mm_to_points(double mm) {
    return (72.0 / 25.4) * mm;
}

Coord Display::a_width() const {
    DisplayRep& d = *rep();
    return Coord(mm_to_points(double(DisplayWidthMM(d.display_, d.screen_))));
}

Coord Display::a_height() const {
    DisplayRep& d = *rep();
    return Coord(mm_to_points(double(DisplayHeightMM(d.display_, d.screen_))));
}

boolean Display::defaults(String& s) const {
    const char* list = XResourceManagerString(rep_->display_);
    if (list != nil) {
	s = list;
	return true;
    }
    return false;
}

void Display::style(Style* s) {
    DisplayRep& d = *rep();
    Resource::ref(s);
    Resource::unref(d.style_);
    d.style_ = s;
    set_screen(d.screen_);
    if (s->value_is_on("synchronous")) {
	XSynchronize(d.display_, True);
    }
}
    
Style* Display::style() const { return rep()->style_; }

void Display::set_screen(int s) {
    DisplayRep& d = *rep();
    XDisplay* dpy = d.display_;
    if (s < 0 || s >= ScreenCount(dpy)) {
	return;
    }
    d.screen_ = s;
    d.root_ = RootWindow(dpy, s);
    d.default_visual_ = WindowVisual::find_visual(this, d.style_);
    d.pwidth_ = DisplayWidth(dpy, s);
    d.pheight_ = DisplayHeight(dpy, s);
    d.set_dpi(pixel_);
    point_ = 1 / pixel_;
    d.width_ = to_coord(d.pwidth_);
    d.height_ = to_coord(d.pheight_);
}

void Display::repair() {
    DamageList& list = *rep()->damaged_;
    for (ListItr(DamageList) i(list); i.more(); i.next()) {
	i.cur()->repair();
    }
    list.remove_all();
}

void Display::flush() {
    XFlush(rep()->display_);
}

void Display::sync() {
    XSync(rep()->display_, 0);
}

void Display::ring_bell(int v) {
    XDisplay* dpy = rep()->display_;
    if (v > 100) {
	XBell(dpy, 100);
    } else if (v >= 0) {
	XBell(dpy, v);
    }
}

void Display::set_key_click(int v) {
    XKeyboardControl k;
    k.key_click_percent = v;
    XChangeKeyboardControl(rep()->display_, KBKeyClickPercent, &k);
}

void Display::set_auto_repeat(boolean b) {
    XDisplay* dpy = rep()->display_;
    if (b) {
	XAutoRepeatOn(dpy);
    } else {
	XAutoRepeatOff(dpy);
    }
}

void Display::set_pointer_feedback(int t, int s) {
    XChangePointerControl(rep()->display_, True, True, s, 1, t);
}

void Display::move_pointer(Coord x, Coord y) {
    DisplayRep& d = *rep();
    XWarpPointer(
	d.display_, None, d.root_, 0, 0, 0, 0,
	to_pixels(x), pheight() - to_pixels(y)
    );
}

SelectionManager* Display::primary_selection() {
    return find_selection("PRIMARY");
}

SelectionManager* Display::secondary_selection() {
    return find_selection("SECONDARY");
}

SelectionManager* Display::clipboard_selection() {
    return find_selection("CLIPBOARD");
}

SelectionManager* Display::find_selection(const char* name) {
    return find_selection(String(name));
}

SelectionManager* Display::find_selection(const String& name) {
    SelectionManager* s;
    SelectionList& list = *rep()->selections_;
    for (ListItr(SelectionList) i(list); i.more(); i.next()) {
	s = i.cur();
	if (*s->rep()->name_ == name) {
	    return s;
	}
    }
    s = new SelectionManager(this, name);
    list.append(s);
    return s;
}

/*
 * Compute size of a pixel in printer points.  If the "dpi" attribute
 * is specified, then we use it as dots per inch and convert to points.
 * Otherwise we use font metrics, not the (alleged) screen size,
 * because applications really care about how things measure
 * with respect to text.  The default assumes that fonts are designed
 * for 75 dots/inch and printer points are 72.0 pts/inch.
 */

void DisplayRep::set_dpi(Coord& pixel) {
    String s;
    if (style_->find_attribute("dpi", s)) {
	long dpi;
	if (s.convert(dpi) && dpi != 0) {
	    pixel = 72.0 / float(dpi);
	}
    } else {
	pixel = 72.0 / 75.0;
    }
}

/*
 * Read the next event if one is pending.  Otherwise, return false.
 * Window::receive will be called on the target window for the event,
 * if the window is known and is valid.  Because we don't keep track
 * of subwindows, it is possible to get an event for a subwindow after
 * the main window has been unmapped.  We must ignore such events.
 */

boolean Display::get(Event& event) {
    DisplayRep* d = rep();
    EventRep& e = *(event.rep());
    e.display_ = this;
    XDisplay* dpy = d->display_;
    XEvent& xe = e.xevent_;
    if (d->damaged_->count() != 0 && QLength(dpy) == 0) {
	repair();
    }
    if (!XPending(dpy)) {
	return false;
    }
    XNextEvent(dpy, &xe);
    e.clear();
    e.window_ = WindowRep::find(xe.xany.window, d->wtable_);
    if (e.window_ != nil) {
	e.window_->receive(event);
    }
    return true;
}

void Display::put(const Event& e) {
    XPutBackEvent(rep()->display_, &e.rep()->xevent_);
}

/*
 * Check to see if the display connection just shut down.
 */

#if !defined(FIONREAD) && defined(sun) && OSMajorVersion >= 5
#define FIONREAD I_NREAD
#endif

boolean Display::closed() {
    XDisplay* dpy = rep()->display_;
    if (XEventsQueued(dpy, QueuedAfterReading) == 0) {
	/* need to detect whether partial event or connection closed */
	int fd = ConnectionNumber(dpy);
	int pending = 0;
	if (ioctl(fd, FIONREAD, (char*)&pending) < 0 || pending == 0) {
	    return true;
	}
    }
    return false;
}

/*
 * Add a handler to the grabber list.  The handler is ref'd to ensure
 * that it is not deallocated while on the list.
 */

void Display::grab(Window* w, Handler* h) {
    GrabInfo g;
    g.window_ = w;
    Resource::ref(h);
    g.handler_ = h;
    rep()->grabbers_->prepend(g);
}

/*
 * Remove a handler from the grabber list.
 * This function has no effect if the handler is not presently on the list.
 * If the handler is on the list, it is unref'd.
 */

void Display::ungrab(Handler* h, boolean all) {
    for (ListUpdater(GrabList) i(*rep()->grabbers_); i.more(); i.next()) {
	const GrabInfo& g = i.cur_ref();
        if (g.handler_ == h) {
            i.remove_cur();
            Resource::unref(h);
	    if (!all) {
		break;
	    }
        }
    }
}
/*
 * Return the most recent grabber, or nil if the list is empty.
 */

Handler* Display::grabber() const {
    GrabList& g = *rep()->grabbers_;
    return (g.count() == 0) ? nil : g.item(0).handler_;
}

/*
 * Check whether a given handler is on the grabber list.
 */

boolean Display::is_grabbing(Handler* h) const {
    for (ListItr(GrabList) i(*rep()->grabbers_); i.more(); i.next()) {
	const GrabInfo& g = i.cur_ref();
        if (g.handler_ == h) {
            return true;
        }
    }
    return false;
}

/*
 * Add a window to the damage list.
 */

void DisplayRep::needs_repair(Window* w) {
    damaged_->append(w);
}

/*
 * Remove a window from relevant display tables.
 */

void DisplayRep::remove(Window* w) {
    for (ListUpdater(DamageList) i(*damaged_); i.more(); i.next()) {
	if (i.cur() == w) {
	    i.remove_cur();
	    break;
	}
    }

    /*
     * No easy way to delete multiple items during a single
     * list traversal.  Sigh.
     */
    boolean done;
    do {
	done = true;
	for (ListUpdater(GrabList) i(*grabbers_); i.more(); i.next()) {
	    const GrabInfo& g = i.cur_ref();
	    if (g.window_ == w) {
		i.remove_cur();
		done = false;
		break;
	    }
	}
    } while (!done);
}
