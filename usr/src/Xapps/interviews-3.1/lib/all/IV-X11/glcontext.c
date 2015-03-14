/*
 * Copyright (c) 1991 Stanford University
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
 * GLContext -- glyph for 3D drawing
 */

#ifdef GL

#include "wtable.h"
#include <InterViews/canvas.h>
#include <InterViews/display.h>
#include <InterViews/glcontext.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <IV-X11/Xlib.h>
#include <IV-X11/xcanvas.h>
#include <IV-X11/xdisplay.h>
#include <IV-X11/xwindow.h>

#undef String
#include <gl/gl.h>

/*
 * Copied from <gl/glws.h> to avoid including Xlib.h directly.
 */

#define GLX_NORMAL      0x1000
#define GLX_RGB         0x0001
#define GLX_DOUBLE      0x0002
#define GLX_VISUAL      0x0007
#define GLX_COLORMAP    0x0008
#define GLX_WINDOW      0x0009

class GLContextImpl {
private:
    friend GLContext;

    Canvas* canvas_;
    Allocation allocation_;
    GLWindow* window_;

    void make_gl_window(Canvas*, const Allocation&);
};

class GLWindow : public Window {
public:
    GLWindow(Glyph*, Window* parent);
    virtual ~GLWindow();

    virtual void place(Coord x, Coord y);
    virtual void bind();
    virtual void repair();
private:
    Window* parent_;
    GLXconfig default_[3];
    GLXconfig* config_;
    Visual* visual_;
    unsigned int depth_;
    XColormap colormap_;
    boolean double_buffered_;

    static void config(GLXconfig&, int buffer, int mode, int arg);
    void setup_glx_config();
    void create_window();
};

GLContext::GLContext(Glyph* g) : MonoGlyph(g) {
    GLContextImpl* gl = new GLContextImpl;
    impl_ = gl;
    gl->canvas_ = nil;
    gl->window_ = nil;
}

GLContext::~GLContext() {
    delete impl_->window_;
    delete impl_;
}

void GLContext::draw(Canvas* c, const Allocation& a) const {
    GLContextImpl* gl = impl_;
    if (gl->canvas_ != c || !a.equals(gl->allocation_, 1e-2)) {
	delete gl->window_;
	Window* pw = c->window();
	GLWindow* w = new GLWindow(body(), pw);
	gl->window_ = w;
	const Allotment& ax = a.x_allotment();
	const Allotment& ay = a.y_allotment();
	w->canvas()->size(ax.span(), ay.span());
	w->place(ax.origin(), ay.origin());
	w->bind();
	gl->canvas_ = c;
	gl->allocation_ = a;
	return;
    }
    Extension ext;
    ext.clear();
    ext.set(c, a);
    if (c->damaged(ext)) {
	Window* w = gl->window_;
	c->damage_area(ext);
	Coord x0 = a.left(), y0 = a.bottom();
	w->canvas()->damage(
	    ext.left() - x0, ext.bottom() - y0,
	    ext.right() - x0, ext.top() - y0
	);
	w->repair();
    }
}

GLWindow::GLWindow(Glyph* g, Window* parent) : Window(g) {
    parent_ = parent;
    Display* d = parent->display();
    display(d);
    Style* s = new Style(d->style());
    s->alias("GLWindow");
    double_buffered_ = s->value_is_on("double_buffered");
    s->attribute("double_buffered", "off");
    style(s);
}

GLWindow::~GLWindow() {
    WindowRep* w = rep();
    GLXunlink(w->display_->rep()->display_, w->xwindow_);
}

void GLWindow::place(Coord x, Coord y) {
    Window::place(x, y);
    WindowRep& w = *rep();
    Display& d = *w.display_;
    w.xpos_ = d.to_pixels(w.left_);
    w.ypos_ = (
	parent_->canvas()->pheight() - d.to_pixels(w.bottom_) -
	canvas()->pheight()
    );
}

void GLWindow::bind() {
    setup_glx_config();
    create_window();
}

void GLWindow::setup_glx_config() {
    config(default_[0], GLX_NORMAL, GLX_RGB, TRUE);
    config(default_[1], GLX_NORMAL, GLX_DOUBLE, double_buffered_);
    config(default_[2], 0, 0, 0);

    DisplayRep& d = *display()->rep();
    config_ = GLXgetconfig(d.display_, d.screen_, default_);
    WindowVisual* wv = WindowVisual::find_visual(display(), style());
    visual_ = wv->visual();
    depth_ = wv->depth();
    for (GLXconfig* cf = config_; cf->buffer != 0; cf++) {
	if (cf->buffer == GLX_NORMAL && cf->mode == GLX_COLORMAP) {
	    colormap_ = (long)cf->arg;
	}
	if (cf->buffer == GLX_NORMAL && cf->mode == GLX_VISUAL) {
	    XVisualInfo vinfo;
	    vinfo.screen = d.screen_;
	    vinfo.visualid = (long)cf->arg;
	    int nv = 0;
	    XVisualInfo* visuals = XGetVisualInfo(
		d.display_, VisualScreenMask | VisualIDMask, &vinfo, &nv
	    );
	    if (visuals != nil) {
		if (nv > 0) {
		    visual_ = visuals[0].visual;
		    depth_ = visuals[0].depth;
		}
		XFree((char*)visuals);
	    }
	}
    }
}

void GLWindow::create_window() {
    WindowRep* w = rep();
    Canvas* c = w->canvas_;
    DisplayRep* d = w->display_->rep();
    XDisplay* dpy = d->display_;
    WindowTable* t = d->wtable_;
    XWindow xw = w->xwindow_;
    if (xw != WindowRep::unbound) {
	t->remove(xw);
    }
    Window::set_attributes();
    w->xattrmask_ &= ~CWDontPropagate;
    w->xattrs_.do_not_propagate_mask = 0;
    w->xattrmask_ |= CWColormap;
    w->xattrs_.colormap = colormap_;
    w->xattrs_.event_mask = ExposureMask | StructureNotifyMask;
    xw = XCreateWindow(
	dpy, parent_->rep()->xwindow_,
	w->xpos_, w->ypos_, c->pwidth(), c->pheight(),
	/* border width */ 0, depth_, w->xclass_,
	visual_, w->xattrmask_, &w->xattrs_
    );
    c->rep()->xdrawable_ = xw;
    t->insert(xw, this);
    w->xwindow_ = xw;
    w->xtoplevel_ = w->toplevel_->rep()->xwindow_;

    for (GLXconfig* cf = config_; cf->buffer != 0; cf++) {
	if (cf->buffer == GLX_NORMAL && cf->mode == GLX_WINDOW) {
	    cf->arg = (int)xw;
	    break;
	}
    }
    GLXlink(dpy, config_);
    XMapRaised(d->display_, xw);
}

void GLWindow::repair() {
    WindowRep* w = rep();
    CanvasRep* c = w->canvas_->rep();
    if (c->damaged_) {
	GLXwinset(w->display_->rep()->display_, w->xwindow_);
	w->glyph_->draw(w->canvas_, w->allocation_);
	if (double_buffered_) {
	    swapbuffers();
	}
	c->clear_damage();
    }
}

void GLWindow::config(GLXconfig& cf, int buffer, int mode, int arg) {
    cf.buffer = buffer;
    cf.mode = mode;
    cf.arg = arg;
}

#endif
