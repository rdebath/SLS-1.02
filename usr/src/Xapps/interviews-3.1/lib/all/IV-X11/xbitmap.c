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
 * X11-dependent bitmap code
 */

#include <InterViews/bitmap.h>
#include <InterViews/display.h>
#include <InterViews/font.h>
#include <InterViews/session.h>
#include <IV-X11/xbitmap.h>
#include <IV-X11/xfont.h>
#include <IV-X11/xdisplay.h>
#include <IV-X11/Xlib.h>
#include <IV-X11/Xutil.h>
#include <OS/math.h>
#include <X11/Xatom.h>

Bitmap::Bitmap() {
    rep_ = nil;
}

Bitmap* Bitmap::open(const char* filename) {
    Display* d = Session::instance()->default_display();
    DisplayRep* r = d->rep();
    unsigned int w, h;
    Pixmap p;
    int x0, y0;
    if (
	XReadBitmapFile(
	    r->display_, r->root_, filename, &w, &h, &p, &x0, &y0
	) != BitmapSuccess
    ) {
	return nil;
    }
    Bitmap* bm = new Bitmap;
    BitmapRep* b = new BitmapRep;
    bm->rep_ = b;
    b->display_ = d;
    b->pwidth_ = w;
    b->pheight_ = h;
    b->width_ = d->to_coord(w);
    b->height_ = d->to_coord(h);
    b->pixmap_ = p;
    if (x0 == -1 && y0 == -1) {
	b->left_ = 0;
	b->right_ = d->to_coord(w);
	b->bottom_ = 0;
	b->top_ = d->to_coord(h);
    } else {
	b->left_ = d->to_coord(-x0);
	b->right_ = d->to_coord(w - x0);
	b->bottom_ = d->to_coord(y0 - h);
	b->top_ = d->to_coord(y0);
    }
    return bm;
}

Bitmap::Bitmap(
    const void* data, unsigned int w, unsigned int h, int x0, int y0
) {
    Display* d = Session::instance()->default_display();
    DisplayRep* r = d->rep();
    BitmapRep* b = new BitmapRep;
    rep_ = b;
    b->display_ = d;
    b->pwidth_ = w;
    b->pheight_ = h;
    b->width_ = d->to_coord(w);
    b->height_ = d->to_coord(h);
    if (x0 == -1 && y0 == -1) {
	b->left_ = 0;
	b->right_ = d->to_coord(w);
	b->bottom_ = 0;
	b->top_ = d->to_coord(h);
    } else {
	b->left_ = d->to_coord(-x0);
	b->right_ = d->to_coord(w - x0);
	b->bottom_ = d->to_coord(y0 - h);
	b->top_ = d->to_coord(y0);
    }
    if (data != nil) {
	b->pixmap_ = XCreateBitmapFromData(
	    r->display_, r->root_, (char*)data, w, h
	);
    } else {
	b->pixmap_ = XCreatePixmap(r->display_, r->root_, w, h, 1);
	GC gc = XCreateGC(r->display_, b->pixmap_, 0, nil);
	XSetForeground(r->display_, gc, 0);
	XFillRectangle(r->display_, b->pixmap_, gc, 0, 0, w, h);
	XFreeGC(r->display_, gc);
    }
}    

Bitmap::Bitmap(const Font* f, long c, float scale) {
    Display* d = Session::instance()->default_display();
    DisplayRep* r = d->rep();
    XDisplay* dpy = r->display_;
    BitmapRep* b = new BitmapRep;
    rep_ = b;
    int w, h;
    int x, y;
    XFontStruct* info = f->rep(d)->font_;
    if (
        c >= info->min_char_or_byte2 && c <= info->max_char_or_byte2 &&
        info->per_char != nil
    ) {
        long i = c - info->min_char_or_byte2;
        w = info->per_char[i].rbearing - info->per_char[i].lbearing;
        h = info->per_char[i].ascent + info->per_char[i].descent;
        x = info->per_char[i].lbearing;
        y = info->per_char[i].ascent;
    } else {
        w = info->max_bounds.rbearing - info->min_bounds.lbearing;
        h = info->max_bounds.ascent + info->max_bounds.descent;
        x = info->min_bounds.lbearing;
        y = info->max_bounds.ascent;
    }

    int pw = Math::max(1, int(w * scale));
    int ph = Math::max(1, int(h * scale));
    w = Math::max(1, w);
    h = Math::max(1, h);

    Pixmap map = XCreatePixmap(dpy, r->root_, w, h, 1);
    GC gc = XCreateGC(dpy, map, 0, nil);
    XSetFont(dpy, gc, info->fid);
    XSetForeground(dpy, gc, 0);
    XFillRectangle(dpy, map, gc, 0, 0, pw, ph);
    XSetForeground(dpy, gc, 1);
    char ch = char(c);
    XDrawString(dpy, map, gc, -x, y, &ch, 1);
    if (scale != 1.0) {
        XImage* source = XGetImage(dpy, map, 0, 0, w, h, 0x01, ZPixmap);
        XFreePixmap(dpy, map);
        map = XCreatePixmap(dpy, r->root_, pw, ph, 1);
        XFillRectangle(dpy, map, gc, 0, 0, pw, ph);
        XImage* dest = XGetImage(dpy, map, 0, 0, pw, ph, 0x01, ZPixmap);
        for (int dy = 0; dy < ph; ++dy) {
            int sy = int(float(dy) / scale);
            for (int dx = 0; dx < pw; ++dx) {
                int sx = int(float(dx) / scale);
                XPutPixel(
		    dest, dx, ph - 1 - dy,
		    XGetPixel(source, sx, h - 1 - sy)
		);
            }
        }
        XPutImage(dpy, map, gc, dest, 0, 0, 0, 0, pw, ph);
        XDestroyImage(source);
        XDestroyImage(dest);
    }
    XFreeGC(dpy, gc);

    b->display_ = d;
    b->pixmap_ = map;
    b->pwidth_ = pw;
    b->pheight_ = ph;
    b->width_ = d->to_coord(pw);
    b->height_ = d->to_coord(ph);
    b->left_ = d->to_coord(x) * scale;
    b->right_ = d->to_coord(x + w) * scale;
    b->top_ = d->to_coord(y) * scale;
    b->bottom_ = d->to_coord(y - h) * scale;
}    

Bitmap::Bitmap(const Bitmap& bm) {
    rep_ = new BitmapRep(bm.rep_, BitmapRep::copy);
    flush();
}

Bitmap::~Bitmap() {
    delete rep_;
}

Coord Bitmap::width() const { return rep()->width_; }
Coord Bitmap::height() const { return rep()->height_; }
unsigned int Bitmap::pwidth() const { return rep()->pwidth_; }
unsigned int Bitmap::pheight() const { return rep()->pheight_; }

Coord Bitmap::left_bearing() const { return -rep()->left_; }
Coord Bitmap::right_bearing() const { return rep()->right_; }
Coord Bitmap::ascent() const { return rep()->top_; }
Coord Bitmap::descent() const { return -rep()->bottom_; }

void Bitmap::poke(boolean set, int x, int y) {
    BitmapRep* b = rep();
    b->fill();
    unsigned long p = set ? 1 : 0;
    XPutPixel(rep()->image_, x, b->pheight_ - 1 - y, p);
    b->modified_ = true;
}

boolean Bitmap::peek(int x, int y) const {
    BitmapRep* b = rep();
    b->fill();
    unsigned long pixel = XGetPixel(b->image_, x, b->pheight_ - 1 - y);
    return pixel != 0;
}

void Bitmap::flush() const { rep()->flush(); }

/* class BitmapRep */

BitmapRep::BitmapRep() {
    image_ = nil;
    modified_ = false;
}

/*
 * The second parameter requests a simple transformation, but
 * we don't support that anymore.  It got broken somewhere along the line
 * and no one complained, so presumably no one will mind if it is ignored
 * completely.  The only exception is inverse, which is straightforward.
 */

BitmapRep::BitmapRep(BitmapRep* b, unsigned int t) {
    display_ = b->display_;
    DisplayRep* r = display_->rep();
    image_ = nil;
    modified_ = true;
    left_ = b->left_;
    right_ = b->right_;
    bottom_ = b->bottom_;
    top_ = b->top_;
    width_ = b->width_;
    height_ = b->height_;
    pwidth_ = b->pwidth_;
    pheight_ = b->pheight_;
    pixmap_ = XCreatePixmap(r->display_, r->root_, pwidth_, pheight_, 1);

    b->fill();
    fill();
    for (register int bx = 0; bx < pwidth_; bx++) {
	for (register int by = 0; by < pheight_; by++) {
            unsigned long pixel;
	    if (bx >= 0 && bx < b->pwidth_ && by >= 0 && by < b->pheight_) {
		pixel = XGetPixel(b->image_, bx, b->pheight_ - by - 1);
	    } else {
		pixel = 0;
	    }
	    if (t == inv) {
		pixel = !pixel;
	    }
	    XPutPixel(image_, bx, pheight_ - by - 1, pixel);
	}
    }
}

BitmapRep::~BitmapRep() {
    if (image_ != nil) {
	XDestroyImage(image_);
    }
    if (pixmap_ != nil) {
	XFreePixmap(display_->rep()->display_, pixmap_);
    }
}

void BitmapRep::fill() {
    if (image_ == nil) {
	image_ = XGetImage(
	    display_->rep()->display_, pixmap_,
	    0, 0, pwidth_, pheight_, 0x01, ZPixmap
	);
    }
}

void BitmapRep::flush() {
    if (modified_) {
	modified_ = false;
	XDisplay* dpy = display_->rep()->display_;
	GC gc = XCreateGC(dpy, pixmap_, 0, nil);
	XPutImage(dpy, pixmap_, gc, image_, 0, 0, 0, 0, pwidth_, pheight_);
	XFreeGC(dpy, gc);
    }
}
