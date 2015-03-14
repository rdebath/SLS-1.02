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
 * Old X11-dependent bitmap code
 */

#include <InterViews/bitmap.h>
#include <InterViews/display.h>
#include <InterViews/font.h>
#include <InterViews/session.h>
#include <InterViews/transformer.h>
#include <IV-X11/xbitmap.h>
#include <IV-X11/xfont.h>
#include <IV-X11/xdisplay.h>
#include <IV-X11/Xlib.h>
#include <IV-X11/Xutil.h>
#include <OS/math.h>
#include <X11/Xatom.h>
#include <stdlib.h>

/* anachronisms */

static void DrawSourceTransformedImage(
    XImage* s, int sx0, int sy0,
    XImage* m, int mx0, int my0,
    XDrawable d, unsigned int height, int dx0, int dy0,
    boolean stencil, unsigned long fg, unsigned long bg,
    GC gc, const Transformer& matrix,
    int xmin, int ymin, int xmax, int ymax
) {
    XDisplay* dpy = Session::instance()->default_display()->rep()->display_;
    unsigned long lastdrawnpixel = fg;
    for (int xx = xmin; xx <= xmax; ++xx) {
        float lx, ly;
        float rx, ry;
        float tx, ty;
        matrix.Transform(float(xx), float(ymin), lx, ly);
        matrix.Transform(float(xx + 1), float(ymin), rx, ry);
        matrix.Transform(float(xx), float(ymax+1), tx, ty);
        float dx = (tx - lx) / float(ymax - ymin + 1);
        float dy = (ty - ly) / float(ymax - ymin + 1);
        int ilx = 0, ily = 0;
        int irx = 0, iry = 0;
        boolean lastmask = false, mask;
        unsigned long lastpixel = fg, pixel, source;
        for (int yy = ymin; yy <= ymax+1; ++yy) {
            mask = (
                yy <= ymax
                && (m == nil || XGetPixel(m, xx-mx0, m->height-1-(yy-my0)))
            );
            if (
                yy<sy0 || yy>=sy0+s->height || xx<sx0 || xx>=sx0+s->width
            ) {
                source = bg;
            } else {
                source = XGetPixel(s, xx-sx0, s->height-1-(yy-sy0));
            }
            if (stencil) {
                pixel = (source != 0) ? fg : bg;
            } else {
                pixel = source;
            }
            if (mask != lastmask || lastmask && pixel != lastpixel) {
                int iilx = Math::round(lx), iily = Math::round(ly);
                int iirx = Math::round(rx), iiry = Math::round(ry);
                if (lastmask) {
                    if (lastpixel != lastdrawnpixel) {
                        XSetForeground(dpy, gc, lastpixel);
                        lastdrawnpixel = lastpixel;
                    }
                    if (
                        (ilx==iilx || ily==iily) && (irx==ilx || iry==ily)
                    ) {
                        XFillRectangle(
                            dpy, d, gc,
                            Math::min(ilx, iirx) - dx0,
                            height - (Math::max(ily, iiry) - dy0),
                            Math::abs(ilx - iirx), Math::abs(ily - iiry)
                        );
                    } else {
                        XPoint v[4];
                        v[0].x = ilx-dx0; v[0].y = height - (ily-dy0);
                        v[1].x = iilx-dx0; v[1].y = height - (iily-dy0);
                        v[2].x = iirx-dx0; v[2].y = height - (iiry-dy0);
                        v[3].x = irx-dx0; v[3].y = height - (iry-dy0);
                        XFillPolygon(
                            dpy, d, gc, v, 4, Convex, CoordModeOrigin
                        );
                    }
                }
                ilx = iilx; ily = iily;
                irx = iirx; iry = iiry;
                lastpixel = pixel;
                lastmask = mask;
            }
            lx += dx; ly += dy;
            rx += dx; ry += dy;
        }
    }
    XSetForeground(dpy, gc, fg);
}

static void DrawDestinationTransformedImage(
    XImage* s, int sx0, int sy0,
    XImage* m, int mx0, int my0,
    XDrawable d, unsigned int height, int dx0, int dy0,
    boolean stencil, unsigned long fg, unsigned long bg,
    GC gc, const Transformer& matrix,
    int xmin, int ymin, int xmax, int ymax
) {
    XDisplay* dpy = Session::instance()->default_display()->rep()->display_;
    Transformer t(matrix);
    t.Invert();

    unsigned long lastdrawnpixel = fg;
    for (IntCoord xx = xmin; xx <= xmax; ++xx) {
        float fx, fy;
        float tx, ty;
        t.Transform(float(xx) + 0.5, float(ymin) + 0.5, fx, fy);
        t.Transform(float(xx) + 0.5, float(ymax) + 1.5, tx, ty);
        float dx = (tx - fx) / float(ymax - ymin + 1); 
        float dy = (ty - fy) / float(ymax - ymin + 1);
        IntCoord lasty = ymin;
        boolean lastmask = false, mask;
        unsigned long lastpixel = fg, pixel, source;
        for (IntCoord yy = ymin; yy <= ymax+1; ++yy) {
            int ix = Math::round(fx - 0.5), iy = Math::round(fy - 0.5);
            boolean insource = (
               ix >= sx0 && ix < sx0 + s->width
               && iy >= sy0 && iy < sy0 + s->height
            );
            boolean inmask = (
                m != nil && ix >= mx0 && ix < mx0 + m->width
                && iy >= my0 && iy < my0 + m->height
            );
            if (yy <= ymax) {
                if (m == nil) {
                    mask = insource;
                } else if (inmask) {
                    mask = XGetPixel(m, ix-mx0, m->height-1-(iy-my0)) != 0;
                } else {
                    mask = false;
                }
            } else {
                mask = false;
            }
            if (insource) {
                source = XGetPixel(s, ix-sx0, s->height-1-(iy-sy0));
            } else {
                source = bg;
            }
            if (stencil) {
                pixel = (source != 0) ? fg : bg;
            } else {
                pixel = source;
            }
            if (mask != lastmask || lastmask && pixel != lastpixel) {
                if (lastmask) {
                    if (lastpixel != lastdrawnpixel) {
                        XSetForeground(dpy, gc, lastpixel);
                        lastdrawnpixel = lastpixel;
                    }
                    XFillRectangle(
                        dpy, d, gc,
                        xx - dx0, height - (yy - dy0), 1, yy - lasty
                    );
                }
                lastmask = mask;
                lastpixel = pixel;
                lasty = yy;
            }
            fx += dx;
            fy += dy;
        }
    }
    XSetForeground(dpy, gc, fg);
}

void DrawTransformedImage(
    XImage* s, int sx0, int sy0,
    XImage* m, int mx0, int my0,
    XDrawable d, unsigned int height, int dx0, int dy0,
    boolean stencil, unsigned long fg, unsigned long bg,
    GC gc, const Transformer& matrix
) {
    int x1 = (m != nil) ? mx0 : sx0;
    int y1 = (m != nil) ? my0 : sy0;
    int x2 = (m != nil) ? mx0 : sx0;
    int y2 = (m != nil) ? my0 + m->height : sy0 + s->height;
    int x3 = (m != nil) ? mx0 + m->width : sx0 + s->width;
    int y3 = (m != nil) ? my0 + m->height : sy0 + s->height;
    int x4 = (m != nil) ? mx0 + m->width : sx0 + s->width;
    int y4 = (m != nil) ? my0 : sy0;

    int sxmin = Math::min(x1, x2, x3, x4);
    int sxmax = Math::max(x1, x2, x3, x4) - 1;
    int symin = Math::min(y1, y2, y3, y4);
    int symax = Math::max(y1, y2, y3, y4) - 1;

    matrix.Transform(x1, y1);
    matrix.Transform(x2, y2);
    matrix.Transform(x3, y3);
    matrix.Transform(x4, y4);

    int dxmin = Math::min(x1, x2, x3, x4);
    int dxmax = Math::max(x1, x2, x3, x4) - 1;
    int dymin = Math::min(y1, y2, y3, y4);
    int dymax = Math::max(y1, y2, y3, y4) - 1;

    int swidth = sxmax - sxmin + 1;
    int sheight = symax - symin + 1;
    int dwidth = dxmax - dxmin + 1;
    int dheight = dymax - dymin + 1;

    boolean rect = (x1 == x2 || y1 == y2) && (x1 == x4 || y1 == y4);
    boolean alwaysdest = dwidth < 2 * swidth;
    boolean alwayssource = dwidth * dheight > 3 * swidth * sheight;
    boolean dest = alwaysdest || (!alwayssource && !rect);
    if (dest) {
        if (dheight > 0) {
            DrawDestinationTransformedImage(
                s, sx0, sy0, m, mx0, my0, d, height, dx0, dy0,
                stencil, fg, bg, gc, matrix,
                dxmin, dymin, dxmax, dymax
            );
        }
    } else {
        if (sheight > 0) {
            DrawSourceTransformedImage(
                s, sx0, sy0, m, mx0, my0, d, height, dx0, dy0,
                stencil, fg, bg, gc, matrix,
                sxmin, symin, sxmax, symax
            );
        }
    }
}

int Bitmap::Left() const { return rep()->display_->to_pixels(left_bearing()); }
int Bitmap::Right() const {
    return rep()->display_->to_pixels(right_bearing());
}
int Bitmap::Bottom() const { return rep()->display_->to_pixels(descent()); }
int Bitmap::Top() const { return rep()->display_->to_pixels(ascent()); }

void Bitmap::Transform(const Transformer* t) {
    Display* d = Session::instance()->default_display();
    DisplayRep* r = d->rep();
    XDisplay* dpy = r->display_;
    BitmapRep* b = rep();
    BitmapRep* nb = new BitmapRep;

    Coord x1, y1, x2, y2, x3, y3, x4, y4;

    t->transform(b->left_, b->bottom_, x1, y1);
    t->transform(b->left_, b->top_, x2, y2);
    t->transform(b->right_, b->top_, x3, y3);
    t->transform(b->right_, b->bottom_, x4, y4);

    Coord xmax = Math::max(x1, x2, x3, x4);
    Coord xmin = Math::min(x1, x2, x3, x4);
    Coord ymax = Math::max(y1, y2, y3, y4);
    Coord ymin = Math::min(y1, y2, y3, y4);

    nb->width_ = xmax - xmin;
    nb->height_ = ymax - ymin;
    nb->left_ = xmin;
    nb->bottom_ = ymin;
    nb->right_ = xmax;
    nb->top_ = ymax;
    nb->pwidth_ = Math::max(
	1, d->to_pixels(nb->right_) - d->to_pixels(nb->left_)
    );
    nb->pheight_ = Math::max(
	1, d->to_pixels(nb->top_) - d->to_pixels(nb->bottom_)
    );

    nb->pixmap_ = XCreatePixmap(dpy, r->root_, nb->pwidth_, nb->pheight_, 1);
    GC gc = XCreateGC(dpy, nb->pixmap_, 0, nil);
    XSetForeground(dpy, gc, 0);
    XFillRectangle(dpy, nb->pixmap_, gc, 0, 0, nb->pwidth_, nb->pheight_);
    XSetForeground(dpy, gc, 1);
    b->fill();
    DrawTransformedImage(
        b->image_, -d->to_pixels(-b->left_), -d->to_pixels(-b->bottom_),
        b->image_, -d->to_pixels(-b->left_), -d->to_pixels(-b->bottom_),
        nb->pixmap_, nb->pheight_,
	-d->to_pixels(-nb->left_), -d->to_pixels(-nb->bottom_),
        true, 1, 0, gc, *t
    );
    XFreeGC(dpy, gc);
    delete b;
    rep_ = nb;
    rep_->modified_ = true;
}

void Bitmap::FlipHorizontal() {
    BitmapRep* b = new BitmapRep(rep_, BitmapRep::fliph);
    delete rep_;
    rep_ = b;
}

void Bitmap::FlipVertical() {
    BitmapRep* b = new BitmapRep(rep_, BitmapRep::flipv);
    delete rep_;
    rep_ = b;
}

void Bitmap::Rotate90() {
    BitmapRep* b = new BitmapRep(rep_, BitmapRep::rot90);
    delete rep_;
    rep_ = b;
}

void Bitmap::Rotate180() {
    BitmapRep* b = new BitmapRep(rep_, BitmapRep::rot180);
    delete rep_;
    rep_ = b;
}

void Bitmap::Rotate270() {
    BitmapRep* b = new BitmapRep(rep_, BitmapRep::rot270);
    delete rep_;
    rep_ = b;
}

void Bitmap::Invert() {
    BitmapRep* b = new BitmapRep(rep_, BitmapRep::inv);
    delete rep_;
    rep_ = b;
}

void Bitmap::Scale(float sx, float sy) {
    Transformer t;
    t.Scale(sx, sy);
    Transform(&t);
}

void Bitmap::Rotate(float angle) {
    Transformer t;
    t.Rotate(angle);
    Transform(&t);
}
