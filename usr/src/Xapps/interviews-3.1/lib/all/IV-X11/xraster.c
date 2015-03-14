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
 * X11-dependent raster code
 */

#include <InterViews/canvas.h>
#include <InterViews/display.h>
#include <InterViews/raster.h>
#include <InterViews/session.h>
#include <IV-X11/Xlib.h>
#include <IV-X11/Xutil.h>
#include <IV-X11/xdisplay.h>
#include <IV-X11/xraster.h>

Raster::Raster(unsigned long w, unsigned long h) {
    RasterRep* r = new RasterRep;
    rep_ = r;
    Display* d = Session::instance()->default_display();
    r->display_ = d;
    r->modified_ = false;
    r->pwidth_ = (unsigned int)w;
    r->pheight_ = (unsigned int)h;
    r->width_ = d->to_coord(r->pwidth_);
    r->height_ = d->to_coord(r->pheight_);
    r->left_ = 0;
    r->bottom_ = 0;
    r->right_ = r->width_;
    r->top_ = r->height_;
    DisplayRep* dr = r->display_->rep();
    XDisplay* dpy = dr->display_;
    r->pixmap_ = XCreatePixmap(
	dpy, dr->root_, r->pwidth_, r->pheight_, dr->default_visual_->depth()
    );
    r->gc_ = XCreateGC(dpy, r->pixmap_, 0, nil);
    r->image_ = XGetImage(
	dpy, r->pixmap_, 0, 0, r->pwidth_, r->pheight_, AllPlanes, ZPixmap
    );
}

Raster::Raster(const Raster& raster) {
    RasterRep* r = new RasterRep;
    rep_ = r;
    raster.flush();
    RasterRep& rr = *(raster.rep());
    r->display_ = rr.display_;
    r->modified_ = true;
    r->width_ = rr.width_;
    r->height_ = rr.height_;
    r->left_ = rr.left_;
    r->bottom_ = rr.bottom_;
    r->right_ = rr.right_;
    r->top_ = rr.top_;
    r->pwidth_ = rr.pwidth_;
    r->pheight_ = rr.pheight_;
    DisplayRep* dr = r->display_->rep();
    XDisplay* dpy = dr->display_;
    r->pixmap_ = XCreatePixmap(
	dpy, dr->root_, r->pwidth_, r->pheight_, dr->default_visual_->depth()
    );
    r->gc_ = XCreateGC(dpy, r->pixmap_, 0, nil);
    XCopyArea(
	dpy, rr.pixmap_, r->pixmap_, r->gc_,
	0, 0, r->pwidth_, r->pheight_, 0, 0
    );
    r->image_ = XGetImage(
	dpy, r->pixmap_, 0, 0, r->pwidth_, r->pheight_, AllPlanes, ZPixmap
    );
}

Raster::Raster(RasterRep* r) { rep_ = r; }

Raster::~Raster() {
    RasterRep* r = rep();
    XDisplay* dpy = r->display_->rep()->display_;
    XFreePixmap(dpy, r->pixmap_);
    XFreeGC(dpy, r->gc_);
    XDestroyImage(r->image_);
    delete r;
}

Coord Raster::width() const { return rep()->width_; }
Coord Raster::height() const { return rep()->height_; }
unsigned long Raster::pwidth() const { return rep()->pwidth_; }
unsigned long Raster::pheight() const { return rep()->pheight_; }

Coord Raster::left_bearing() const { return -rep()->left_; }
Coord Raster::right_bearing() const { return rep()->right_; }
Coord Raster::ascent() const { return rep()->top_; }
Coord Raster::descent() const { return -rep()->bottom_; }

void Raster::peek(
    unsigned long x, unsigned long y,
    ColorIntensity& red, ColorIntensity& green, ColorIntensity& blue,
    float& alpha
) const {
    register RasterRep* r = rep();
    unsigned long pixel = XGetPixel(
	r->image_, (unsigned int)x, r->pheight_ - (unsigned int)y - 1
    );
    XColor xc;
    r->display_->rep()->default_visual_->find_color(pixel, xc);
    red = float(xc.red) / 0xffff;
    green = float(xc.green) / 0xffff;
    blue = float(xc.blue) / 0xffff;
    alpha = 1.0;
}

void Raster::poke(
    unsigned long x, unsigned long y,
    ColorIntensity red, ColorIntensity green, ColorIntensity blue, float
) {
    RasterRep* r = rep();
    unsigned short sr = (unsigned short)(red * 0xffff);
    unsigned short sg = (unsigned short)(green * 0xffff);
    unsigned short sb = (unsigned short)(blue * 0xffff);
    XColor xc;
    r->display_->rep()->default_visual_->find_color(sr, sg, sb, xc);
    XPutPixel(
	r->image_, (unsigned int)x, r->pheight_ - (unsigned int)y - 1, xc.pixel
    );
    r->modified_ = true;
}

void Raster::flush() const {
    RasterRep* r = rep();
    if (r->modified_) {
	XPutImage(
	    r->display_->rep()->display_, r->pixmap_, r->gc_, r->image_,
	    0, 0, 0, 0, r->pwidth_, r->pheight_
	);
	r->modified_ = false;
    }
}
