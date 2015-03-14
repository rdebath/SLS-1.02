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

#include <InterViews/color.h>
#include <InterViews/display.h>
#include <InterViews/session.h>
#include <InterViews/window.h>
#include <IV-X11/xcolor.h>
#include <IV-X11/xdisplay.h>
#include <IV-X11/xwindow.h>
#include <OS/list.h>
#include <OS/math.h>
#include <OS/string.h>
#include <OS/ustring.h>
#include <OS/table2.h>

declarePtrList(ColorRepList,ColorRep)
implementPtrList(ColorRepList,ColorRep)

inline unsigned long key_to_hash(String& s) { return s.hash(); }

declareTable2(NameToColor,Display*,UniqueString,const Color*)
implementTable2(NameToColor,Display*,UniqueString,const Color*)

class ColorImpl {
private:
    friend class Color;

    ColorIntensity red;
    ColorIntensity green;
    ColorIntensity blue;
    float alpha;
    ColorOp op;
    ColorRepList replist;
    UniqueString ctable_name;
    Display* ctable_display;

    static NameToColor* ctable_;
};

NameToColor* ColorImpl::ctable_;

Color::Color(
    ColorIntensity r, ColorIntensity g, ColorIntensity b,
    float alpha, ColorOp op
) {
    ColorImpl* c = new ColorImpl;
    impl_ = c;
    c->red = r;
    c->green = g;
    c->blue = b;
    c->alpha = alpha;
    c->op = op;
    c->ctable_display = nil;
}

Color::Color(const Color& color, float alpha, ColorOp op) {
    ColorImpl* c = new ColorImpl;
    impl_ = c;
    c->red = color.impl_->red;
    c->green = color.impl_->green;
    c->blue = color.impl_->blue;
    c->alpha = alpha;
    c->op = op;
    c->ctable_display = nil;
}

Color::~Color() {
    ColorImpl* c = impl_;
    if (c->ctable_display != nil) {
	c->ctable_->remove(c->ctable_display, c->ctable_name);
    }
    for (ListItr(ColorRepList) i(c->replist); i.more(); i.next()) {
	destroy(i.cur());
    }
    delete c;
}

/*
 * Shorthand for find + constructor.
 */

const Color* Color::lookup(Display* d, const String& s) {
    NameToColor* t = ColorImpl::ctable_;
    if (t == nil) {
	t = new NameToColor(128);
	ColorImpl::ctable_ = t;
    }
    UniqueString u(s);
    const Color* c;
    if (t->find(c, d, u)) {
	return c;
    }
    ColorIntensity r, g, b;
    if (find(d, u, r, g, b)) {
	c = new Color(r, g, b);
	t->insert(d, u, c);
	c->impl_->ctable_display = d;
	c->impl_->ctable_name = u;
	return c;
    }
    return nil;
}

const Color* Color::lookup(Display* d, const char* name) {
    return lookup(d, String(name));
}

boolean Color::distinguished(const Color* c) const {
    return distinguished(Session::instance()->default_display(), c);
}

void Color::intensities(
    ColorIntensity& r, ColorIntensity& g, ColorIntensity& b
) const {
    intensities(Session::instance()->default_display(), r, g, b);
}

float Color::alpha() const {
    return impl_->alpha;
}

ColorRep* Color::rep(WindowVisual* wv) const {
    for (ListItr(ColorRepList) i(impl_->replist); i.more(); i.next()) {
	ColorRep* c = i.cur();
	if (c->visual_ == wv) {
	    return c;
	}
    }
    ColorImpl* c = impl_;
    ColorRep* r = create(wv, c->red, c->green, c->blue, c->alpha, c->op);
    impl_->replist.append(r);
    return r;
}

void Color::remove(WindowVisual* wv) const {
    for (ListUpdater(ColorRepList) i(impl_->replist); i.more(); i.next()) {
	ColorRep* c = i.cur();
	if (c->visual_ == wv) {
	    i.remove_cur();
	    break;
	}
    }
}

static char stipple_data[][4] = {
    {'\000','\000','\000','\000'},
    {'\001','\000','\000','\000'},
    {'\001','\000','\004','\000'},
    {'\005','\000','\004','\000'},
    {'\005','\000','\005','\000'},
    {'\005','\002','\005','\000'},
    {'\005','\002','\005','\010'},
    {'\005','\012','\005','\010'},
    {'\005','\012','\005','\012'},
    {'\007','\012','\005','\012'},
    {'\007','\012','\015','\012'},
    {'\017','\012','\015','\012'},
    {'\017','\012','\017','\012'},
    {'\017','\013','\017','\012'},
    {'\017','\013','\017','\016'},
    {'\017','\017','\017','\016'}
};

static Pixmap stipple[16];

static Pixmap make_stipple(WindowVisual* wv, float alpha) {
    int index = Math::max(0, Math::min(int(alpha * 16), 15));
    if (stipple[index] == None) {
        stipple[index] = XCreateBitmapFromData(
            wv->display(), RootWindow(wv->display(), wv->screen()),
	    stipple_data[index], 4, 4
        );
    }
    return stipple[index];
}

ColorRep* Color::create(
    WindowVisual* wv, ColorIntensity r, ColorIntensity g, ColorIntensity b,
    float alpha, ColorOp op
) const {
    unsigned short red = (unsigned short)Math::round(r * float(0xffff));
    unsigned short green = (unsigned short)Math::round(g * float(0xffff));
    unsigned short blue = (unsigned short)Math::round(b * float(0xffff));
    register ColorRep* c = new ColorRep;
    wv->find_color(red, green, blue, c->xcolor_);
    c->visual_ = wv;
    switch (op) {
    case Copy:
	c->op_ = GXcopy;
	c->masking_ = false;
	break;
    case Xor:
	c->op_ = GXxor;
	c->masking_ = false;
	break;
    case Invisible:
	c->op_ = GXnoop;
	c->masking_ = false;
	break;
    }
    c->stipple_ = ((alpha > 0.9999 && alpha < 1.0001) ?
	None : make_stipple(wv, alpha)
    );
    return c;
}

boolean Color::find(
    const Display* display, const String& name,
    ColorIntensity& r, ColorIntensity& g, ColorIntensity& b
) {
    NullTerminatedString ns(name);
    XColor xc;
    DisplayRep& d = *display->rep();
    if (XParseColor(
	d.display_, d.default_visual_->colormap(), ns.string(), &xc)
    ) {
	r = float(xc.red) / float(0xffff);
	g = float(xc.green) / float(0xffff);
	b = float(xc.blue) / float(0xffff);
	return true;
    }
    return false;
}

boolean Color::find(
    const Display* display, const char* name,
    ColorIntensity& r, ColorIntensity& g, ColorIntensity& b
) {
    return find(display, String(name), r, g, b);
}

void Color::destroy(ColorRep* r) {
    delete r;
}

boolean Color::distinguished(Display* d, const Color* color) const {
    WindowVisual* wv = d->rep()->default_visual_;
    XColor& xc = rep(wv)->xcolor_;
    ColorRep* c = color->rep(wv);
    XColor& color_xc = c->xcolor_;
    return (
	xc.red != color_xc.red ||
	xc.green != color_xc.green ||
	xc.blue != color_xc.blue
    );
}

void Color::intensities(
    Display* d, ColorIntensity& r, ColorIntensity& g, ColorIntensity& b
) const {
    WindowVisual* wv = d->rep()->default_visual_;
    XColor& xc = rep(wv)->xcolor_;
    r = float(xc.red) / float(0xffff);
    g = float(xc.green) / float(0xffff);
    b = float(xc.blue) / float(0xffff);
}

const Color* Color::brightness(float adjust) const {
    ColorIntensity r, g, b;
    intensities(r, g, b);
    if (adjust >= 0) {
	r += (1 - r) * adjust;
	g += (1 - g) * adjust;
	b += (1 - b) * adjust;
    } else {
	float f = adjust + 1.0;
	r *= f;
	g *= f;
	b *= f;
    }
    return new Color(r, g, b);
}

/* anachronisms */

Color::Color(int ir, int ig, int ib) {
    ColorImpl* c = new ColorImpl;
    impl_ = c;
    c->red = float(ir) / float(0xffff);
    c->green = float(ig) / float(0xffff);
    c->blue = float(ib) / float(0xffff);
    c->alpha = 1.0;
    c->op = Copy;
}

void Color::Intensities(int& ir, int& ig, int& ib) const {
    ColorIntensity r, g, b;
    intensities(Session::instance()->default_display(), r, g, b);
    ir = Math::round(float(0xffff) * r);
    ig = Math::round(float(0xffff) * g);
    ib = Math::round(float(0xffff) * b);
}

int Color::PixelValue() const {
    Display& d = *Session::instance()->default_display();
    WindowVisual* wv = d.rep()->default_visual_;
    return rep(wv)->xcolor_.pixel;
}
