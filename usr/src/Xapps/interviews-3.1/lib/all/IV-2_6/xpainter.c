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
 * X11-dependent painter code
 */

#include "../IV-X11/wtable.h"
#include <InterViews/bitmap.h>
#include <InterViews/brush.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/display.h>
#include <InterViews/event.h>
#include <InterViews/font.h>
#include <InterViews/pattern.h>
#include <InterViews/raster.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/transformer.h>
#include <IV-2_6/InterViews/iwindow.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-X11/Xlib.h>
#include <IV-X11/xbitmap.h>
#include <IV-X11/xbrush.h>
#include <IV-X11/xcanvas.h>
#include <IV-X11/xcolor.h>
#include <IV-X11/xdisplay.h>
#include <IV-X11/xevent.h>
#include <IV-X11/xfont.h>
#include <IV-X11/xpainter.h>
#include <IV-X11/xpattern.h>
#include <IV-X11/xraster.h>
#include <OS/list.h>
#include <OS/math.h>
#include <OS/string.h>
#include <OS/table2.h>

PainterRep::PainterRep() {
    display = Session::instance()->default_display();
    DisplayRep* d = display->rep();
    fillgc = XCreateGC(d->display_, d->root_, 0, nil);
    dashgc = XCreateGC(d->display_, d->root_, 0, nil);
    fillbg = true;
    overwrite = false;
    xor = false;
    clipped = false;
}

PainterRep::~PainterRep() {
    XDisplay* dpy = display->rep()->display_;
    XFreeGC(dpy, fillgc);
    XFreeGC(dpy, dashgc);
}

void PainterRep::PrepareFill(const Pattern* p) {
    PatternRep& pr = *p->rep();
    XDisplay* dpy = display->rep()->display_;
    if (pr.pixmap_ == nil) {
        XSetFillStyle(dpy, fillgc, FillSolid);
    } else if (fillbg) {
        XSetStipple(dpy, fillgc, pr.pixmap_);
        XSetFillStyle(dpy, fillgc, FillOpaqueStippled);
    } else {
        XSetStipple(dpy, fillgc, pr.pixmap_);
        XSetFillStyle(dpy, fillgc, FillStippled);
    }
}

void PainterRep::PrepareDash(const Brush* b) {
    BrushRep& br = *(b->rep(display));
    XDisplay* dpy = display->rep()->display_;
    if (br.dash_list_ == nil) {
	XSetLineAttributes(
	    dpy, dashgc, br.width_, LineSolid, CapButt, JoinMiter
	);
    } else {
	XSetLineAttributes(
	    dpy, dashgc, br.width_, LineOnOffDash, CapButt, JoinMiter
	);
	XSetDashes(dpy, dashgc, 0, br.dash_list_, br.dash_count_);
    }
}

/*
 * Short-hand for allocating a vector of X points.
 * The idea is to use a static array if possible; otherwise
 * allocate/deallocate off the heap.
 */

static const int XPointListSize = 200;
static XPoint xpoints[XPointListSize];

static XPoint* AllocPts(int n) {
    return (n <= XPointListSize) ? xpoints : new XPoint[n];
}

static void FreePts(XPoint* v) {
    if (v != xpoints) {
	delete v;
    }
}

Painter::Painter() {
    rep = new PainterRep;
    Init();
}

Painter::Painter(Painter* copy) {
    rep = new PainterRep;
    XDisplay* dpy = rep->display->rep()->display_;
    rep->fillbg = copy->rep->fillbg;
    rep->overwrite = copy->rep->overwrite;
    Copy(copy);
    if (copy->rep->xor) {
	Begin_xor();
    }
    rep->xor = copy->rep->xor;
    if (rep->overwrite) {
	XSetSubwindowMode(dpy, rep->fillgc, IncludeInferiors);
	XSetSubwindowMode(dpy, rep->dashgc, IncludeInferiors);
    }
}

Painter::~Painter() {
    Resource::unref(matrix);
    Resource::unref(font);
    Resource::unref(br);
    Resource::unref(foreground);
    Resource::unref(background);
    Resource::unref(pattern);
    delete rep;
}

void Painter::FillBg(boolean b) {
    if (rep->fillbg != b) {
        if (rep->xor) {
            End_xor();
        }
        rep->fillbg = b;
        if (pattern != nil) {
            rep->PrepareFill(pattern);
        }
        if (br != nil) {
            rep->PrepareDash(br);
        }
    }
}

boolean Painter::BgFilled() const { return rep->fillbg; }

void Painter::SetColors(const Color* f, const Color* b) {
    if (rep->xor) {
	End_xor();
    }

    /*
     * Make sure to reference both new colors before unreferencing the old
     * in case the call is swapping colors.
     */
    if (foreground != f) {
	Resource::ref(f);
    }
    if (background != b) {
	Resource::ref(b);
    }

    XDisplay* dpy = rep->display->rep()->display_;
    if (f != nil && foreground != f) {
	Resource::unref(foreground);
	foreground = f;
	unsigned long pixel = foreground->PixelValue();
        XSetForeground(dpy, rep->fillgc, pixel);
        XSetForeground(dpy, rep->dashgc, pixel);
    }

    if (b != nil && background != b) {
	Resource::unref(background);
	background = b;
	unsigned long pixel = background->PixelValue();
	XSetBackground(dpy, rep->fillgc, pixel);
	XSetBackground(dpy, rep->dashgc, pixel);
    }
}

void Painter::SetPattern(const Pattern* pat) {
    if (rep->xor) {
	End_xor();
    }
    if (pattern != pat) {
	Resource::ref(pat);
	Resource::unref(pattern);
	pattern = pat;
        if (pattern != nil) {
            rep->PrepareFill(pattern);
        }
    }
}

void Painter::SetBrush(const Brush* b) {
    if (rep->xor) {
	End_xor();
    }
    if (br != b) {
	Resource::ref(b);
	Resource::unref(br);
	br = b;
	if (br != nil) {
            rep->PrepareDash(br);
	}
    }
}

void Painter::SetFont(const Font* f) {
    if (font != f) {
	Resource::ref(f);
	Resource::unref(font);
        font = f;
        if (font != nil) {
	    Display* d = rep->display;
            XSetFont(
		d->rep()->display_, rep->fillgc, font->rep(d)->font_->fid
	    );
        }
    }
}

void Painter::Clip(
    Canvas* c, IntCoord left, IntCoord bottom, IntCoord right, IntCoord top
) {
    XRectangle& r = rep->xclip[0];
    IntCoord x, y;
    XDisplay* d = rep->display->rep()->display_;

    if (left > right) {
	x = right; r.width = left - right + 1;
    } else {
	x = left; r.width = right - left + 1;
    }
    if (bottom > top) {
	y = bottom; r.height = bottom - top + 1;
    } else {
	y = top; r.height = top - bottom + 1;
    }
    r.x = x;
    r.y = c->pheight() - 1 - y;
    if (r.x == 0 && r.y == 0 &&
	r.width == c->pwidth() && r.height == c->pheight()
    ) {
	/* clipping to entire canvas is equivalent to no clipping at all */
	NoClip();
    } else {
	rep->clipped = true;
	XSetClipRectangles(d, rep->fillgc, 0, 0, rep->xclip, 1, Unsorted);
	XSetClipRectangles(d, rep->dashgc, 0, 0, rep->xclip, 1, Unsorted);
    }
}

void Painter::NoClip() {
    XDisplay* dpy = rep->display->rep()->display_;
    rep->clipped = false;
    XSetClipMask(dpy, rep->fillgc, None);
    XSetClipMask(dpy, rep->dashgc, None);
}

void Painter::SetOverwrite(boolean children) {
    if (rep->overwrite != children) {
	XDisplay* dpy = rep->display->rep()->display_;
	rep->overwrite = children;
	XSetSubwindowMode(
	    dpy, rep->fillgc, children ? IncludeInferiors : ClipByChildren
	);
	XSetSubwindowMode(
	    dpy, rep->dashgc, children ? IncludeInferiors : ClipByChildren
	);
    }
}

void Painter::SetPlaneMask(int m) {
    XDisplay* dpy = rep->display->rep()->display_;
    XSetPlaneMask(dpy, rep->fillgc, m);
    XSetPlaneMask(dpy, rep->dashgc, m);
}

void Painter::Map(
    Canvas* c, IntCoord x, IntCoord y, IntCoord& mx, IntCoord& my
) {
    if (matrix == nil) {
	mx = x; my = y;
    } else {
	matrix->Transform(x, y, mx, my);
    }
    mx += xoff;
    my = c->pheight() - 1 - (my + yoff);
}

void Painter::MapList(
    Canvas* c, IntCoord x[], IntCoord y[], int n, IntCoord mx[], IntCoord my[]
) {
    IntCoord* xp, * yp, * mxp, * myp;
    IntCoord* lim;

    xp = x; yp = y;
    mxp = mx; myp = my;
    lim = &x[n];
    if (matrix == nil) {
	for (; xp < lim; xp++, yp++, mxp++, myp++) {
	    *mxp = *xp + xoff;
	    *myp = c->pheight() - 1 - (*yp + yoff);
	}
    } else {
	for (; xp < lim; xp++, yp++, mxp++, myp++) {
	    matrix->Transform(*xp, *yp, *mxp, *myp);
	    *mxp += xoff;
	    *myp = c->pheight() - 1 - (*myp + yoff);
	}
    }
}

void Painter::MapList(
    Canvas* c, float x[], float y[], int n, IntCoord mx[], IntCoord my[]
) {
    register float* xp, * yp;
    register IntCoord* mxp, * myp;
    float tmpx, tmpy, * lim;

    xp = x; yp = y;
    mxp = mx; myp = my;
    lim = &x[n];
    if (matrix == nil) {
	for (; xp < lim; xp++, yp++, mxp++, myp++) {
	    *mxp = Math::round(*xp + xoff);
	    *myp = Math::round(c->pheight() - 1 - (*yp + yoff));
	}
    } else {
	for (; xp < lim; xp++, yp++, mxp++, myp++) {
	    matrix->Transform(*xp, *yp, tmpx, tmpy);
	    *mxp = Math::round(tmpx + xoff);
	    *myp = Math::round(c->pheight() - 1 - (tmpy + yoff));
	}
    }
}

void Painter::Begin_xor() {
    if (!rep->xor) {
	rep->xor = true;
	DisplayRep& d = *rep->display->rep();
	XDisplay* dpy = d.display_;
	unsigned long xor_pixel = d.default_visual_->xor(*d.style_);
	XSetFunction(dpy, rep->fillgc, GXxor);
	XSetForeground(dpy, rep->fillgc, xor_pixel);
	XSetFillStyle(dpy, rep->fillgc, FillSolid);
	XSetFunction(dpy, rep->dashgc, GXxor);
	XSetForeground(dpy, rep->dashgc, xor_pixel);
	XSetFillStyle(dpy, rep->dashgc, FillSolid);
    }
}

void Painter::End_xor() {
    if (rep->xor) {
	rep->xor = false;
	XDisplay* dpy = rep->display->rep()->display_;
	XSetFunction(dpy, rep->fillgc, GXcopy);
	unsigned long pixel = foreground->PixelValue();
	XSetForeground(dpy, rep->fillgc, pixel);
        if (pattern != nil) {
            rep->PrepareFill(pattern);
        }
	XSetFunction(dpy, rep->dashgc, GXcopy);
	XSetForeground(dpy, rep->dashgc, pixel);
        if (br != nil) {
            rep->PrepareDash(br);
        }
    }
}

inline unsigned char _txkey (int i) {
    if (i >= 0) {
        return (
            i<32 ? i : i<160 ? 24 + (i>>2) : i<672 ? 54 + (i>>4) : 127
        );
    } else {
        return (
            i>-32 ? i : i>-160 ? -24 - (i>>2) : i>-672 ? -54 - (i>>4) : -127
        );
    }
}

static int TxKey(Transformer* t, int x, int y) {
    if (t == nil) {
        return 0;
    } else {
	float x1, y1, x2, y2, x3, y3;
	t->matrix(x1, y1, x2, y2, x3, y3);
	x1 = x1 - 1.0;
	y2 = y2 - 1.0;
        return (
              (_txkey((int)(x1*x)) << 24)
            + (_txkey((int)(y1*y)) << 16)
            + (_txkey((int)(x2*x)) << 8)
            + (_txkey((int)(y2*y)))
        );
    }
}

/*
 * Per-display shared painter information.
 */

class Bitmap;

declareTable2(BitmapTable,XFont,int,Bitmap*)
implementTable2(BitmapTable,XFont,int,Bitmap*)

declareTable2(RasterTable,const Raster*,int,RasterRep*)
implementTable2(RasterTable,const Raster*,int,RasterRep*)

class PainterDpyInfoList;

class PainterDpyInfo {
public:
    Display* display_;
    BitmapTable* btable_;
    BitmapTable* txtable_;
    RasterTable* tx_rasters_;
    static PainterDpyInfoList* info_list_;

    enum { TxFontsDefault, TxFontsOff, TxFontsOn, TxFontsCache } txfonts;
    enum {
	TxImagesDefault, TxImagesAuto, TxImagesDest, TxImagesSource
    } tximages;

    static PainterDpyInfo* find(Display*);

    int tx_key(const Transformer&, Coord x, Coord y);

    Bitmap* get_char_bitmap(const Font*, int c, int k, const Transformer&);
    RasterRep* tx_raster(const Raster*, const Transformer&);
};

PainterDpyInfoList* PainterDpyInfo::info_list_;

declarePtrList(PainterDpyInfoList,PainterDpyInfo);
implementPtrList(PainterDpyInfoList,PainterDpyInfo);

PainterDpyInfo* PainterDpyInfo::find(Display* d) {
    if (info_list_ == nil) {
	info_list_ = new PainterDpyInfoList(3);
    }
    for (ListItr(PainterDpyInfoList) i(*info_list_); i.more(); i.next()) {
	PainterDpyInfo* p = i.cur();
	if (p->display_ == d) {
	    return p;
	}
    }
    PainterDpyInfo* p = new PainterDpyInfo;
    p->display_ = d;
    p->btable_ = nil;
    p->txtable_ = nil;
    p->tx_rasters_ = nil;
    p->txfonts = PainterDpyInfo::TxFontsDefault;
    p->tximages = PainterDpyInfo::TxImagesDefault;

    String v;
    if (d->style()->find_attribute("TransformFonts", v)) {
	if (v.case_insensitive_equal("on")) {
	    p->txfonts = PainterDpyInfo::TxFontsOn;
	} else if (v.case_insensitive_equal("off")) {
	    p->txfonts = PainterDpyInfo::TxFontsOff;
	} else if (v.case_insensitive_equal("cache")) {
	    p->txfonts = PainterDpyInfo::TxFontsCache;
	}
    }
    if (d->style()->find_attribute("TransformImages", v)) {
	if (v.case_insensitive_equal("auto")) {
	    p->tximages = PainterDpyInfo::TxImagesAuto;
	} else if (v.case_insensitive_equal("off")) {
	    p->tximages = PainterDpyInfo::TxImagesDest;
	} else if (v.case_insensitive_equal("source")) {
	    p->tximages = PainterDpyInfo::TxImagesSource;
	}
    }
    info_list_->append(p);
    return p;
}

int PainterDpyInfo::tx_key(const Transformer& tx, Coord x, Coord y) {
    Coord x1, y1, x2, y2, x3, y3;
    tx.transform(0, 0, x1, y1);
    tx.transform(0, y, x2, y2);
    tx.transform(x, 0, x3, y3);
    int k1 = _txkey(int(x2 - x1)) & 0xff;
    int k2 = _txkey(int(y2 - y1 - y)) & 0xff;
    int k3 = _txkey(int(x3 - x1 - x)) & 0xff;
    int k4 = _txkey(int(y3 - y1)) & 0xff;
    return (k1 << 24) + (k2 << 16) + (k3 << 8) + k4;
}

Bitmap* PainterDpyInfo::get_char_bitmap(
    const Font* f, int c, int k, const Transformer& t
) {
    if (btable_ == nil) {
        btable_ = new BitmapTable(256);
        txtable_ = new BitmapTable(1024);
    }
    Bitmap* basic;
    XFont fid = f->rep(display_)->font_->fid;
    if (!btable_->find(basic, fid, c)) {
        basic = new Bitmap(f, c);
	Resource::ref(basic);
        btable_->insert(fid, c, basic);
    }
    Bitmap* tx;
    Pixmap mapid = basic->rep()->pixmap_;
    if (!txtable_->find(tx, mapid, k)) {
        tx = new Bitmap(*basic);
	Resource::ref(tx);
        tx->Transform(&t);
        txtable_->insert(mapid, k, tx);
    }
    return tx;
}

RasterRep* PainterDpyInfo::tx_raster(
    const Raster* r, const Transformer& tx
) {
    int key = tx_key(tx, r->width(), r->height());
    if (key == 0) {
        return r->rep();
    } else {
        if (tx_rasters_ == nil) {
            tx_rasters_ = new RasterTable(1024);
        }
        RasterRep* rep;
        if (!tx_rasters_->find(rep, r, key)) {
            Display* d = r->rep()->display_;
            rep = new RasterRep;

            Transformer v(tx);

            Coord x0, y0;
            v.transform(0, 0, x0, y0);
            v.translate(-x0, -y0);

            Coord x1, x2, x3, x4;
            Coord y1, y2, y3, y4;
            v.transform(-r->left_bearing(), -r->descent(), x1, y1);
            v.transform(-r->left_bearing(), r->ascent(), x2, y2);
            v.transform(r->right_bearing(), r->ascent(), x3, y3);
            v.transform(r->right_bearing(), -r->descent(), x4, y4);

            Coord xmax = Math::max(x1, x2, x3, x4);
            Coord xmin = Math::min(x1, x2, x3, x4);
            Coord ymax = Math::max(y1, y2, y3, y4);
            Coord ymin = Math::min(y1, y2, y3, y4);

            int width = d->to_pixels(xmax) - d->to_pixels(xmin);
            int height = d->to_pixels(ymax) - d->to_pixels(ymin);
            if (width <= 0) {
                width = 1;
            }
            if (height <= 0) {
                height = 1;
            }

	    DisplayRep& dr = *d->rep();
            XDisplay* dpy = dr.display_;
            RasterRep* srep = r->rep();

            XImage* source = XGetImage(
                dpy, srep->pixmap_,
                0, 0, srep->pwidth_, srep->pheight_, AllPlanes, ZPixmap
            );

            Pixmap map = XCreatePixmap(
                dpy, dr.root_, width, height, dr.default_visual_->depth()
            );
            GC xgc = XCreateGC(dpy, map, 0, nil);
            XSetForeground(dpy, xgc, 0);
            XFillRectangle(dpy, map, xgc, 0, 0, width, height);
            XImage* dest = XGetImage(
                dpy, map, 0, 0, width, height, AllPlanes, ZPixmap
            );

            int dx0 = d->to_pixels(-xmin);
            int dy0 = d->to_pixels(-ymin);
            int sx0 = d->to_pixels(r->left_bearing());
            int sy0 = d->to_pixels(r->descent());
            for (int dy = 0; dy < height; ++dy) {
                Coord tx1, ty1, tx2, ty2;
                v.inverse_transform(- dx0, dy - dy0, tx1, ty1);
                v.inverse_transform(width - dx0, dy - dy0, tx2, ty2);
                float delta_x = (tx2 - tx1) / width;
                float delta_y = (ty2 - ty1) / width;
                int sx, sy;
                for (int dx = 0; dx < width; ++dx) {
                    sx = int(tx1) + sx0;
                    sy = int(ty1) + sy0;
                    if (
                        sx >= 0 && sx < srep->pwidth_ &&
                        sy >= 0 && sy < srep->pheight_
                    ) {
                        XPutPixel(
                            dest, dx, height - 1 - dy,
                            XGetPixel(source, sx, srep->pheight_ - 1 - sy)
                        );
                    }
                    tx1 = tx1 + delta_x;
                    ty1 = ty1 + delta_y;
                }
            }

            XPutImage(dpy, map, xgc, dest, 0, 0, 0, 0, width, height);
            XFreeGC(dpy, xgc);
            XDestroyImage(source);
            XDestroyImage(dest);

            rep->display_ = d;
            rep->pixmap_ = map;
            rep->pwidth_ = width;
            rep->pheight_ = height;
            rep->width_ = xmax - xmin;
            rep->height_ = ymax - ymin;
            rep->left_ = xmin;
            rep->right_ = xmax;
            rep->bottom_ = ymin;
            rep->top_ = ymax;
            tx_rasters_->insert(r, key, rep);
        }
        return rep;
    }
}

void Painter::Stencil(
    Canvas* c, IntCoord x, IntCoord y, Bitmap* bitmap, Bitmap* mask
) {
    if (c == nil) {
	return;
    }
    Display* d = rep->display;
    XDisplay* dpy = d->rep()->display_;
    XDrawable xid = c->rep()->xdrawable_;
    if (xid == CanvasRep::unbound) {
	return;
    }
    if (rep->xor) {
        End_xor();
    }
    int tx = TxKey(matrix, bitmap->pwidth(), bitmap->pheight());
    if (tx == 0) {
        IntCoord dx, dy;
	IntCoord dl = x - d->to_pixels(bitmap->left_bearing());
	IntCoord dt = y + d->to_pixels(bitmap->ascent());
	Map(c, dl, dt - 1, dx, dy);
        if (mask == nil) {
            XCopyPlane(
                dpy, bitmap->rep()->pixmap_, xid,
                rep->fillgc, 0, 0, bitmap->pwidth(), bitmap->pheight(),
		dx, dy, 1
            );
        } else if (mask == bitmap) {
            XSetForeground(dpy, rep->fillgc, 0);
            XSetBackground(dpy, rep->fillgc, AllPlanes);
            XSetFunction(dpy, rep->fillgc, GXand);
            XCopyPlane(
                dpy, bitmap->rep()->pixmap_, xid,
                rep->fillgc, 0, 0, bitmap->pwidth(), bitmap->pheight(),
		dx, dy, 1
            );
            XSetForeground(dpy, rep->fillgc, foreground->PixelValue());
            XSetBackground(dpy, rep->fillgc, 0);
            XSetFunction(dpy, rep->fillgc, GXxor);
            XCopyPlane(
                dpy, bitmap->rep()->pixmap_, xid,
                rep->fillgc, 0, 0, bitmap->pwidth(), bitmap->pheight(),
		dx, dy, 1
            );
            XSetBackground(dpy, rep->fillgc, background->PixelValue());
            XSetFunction(dpy, rep->fillgc, GXcopy);
        } else {
            IntCoord mx, my;
	    IntCoord ml = x - d->to_pixels(mask->left_bearing());
	    IntCoord mt = y + d->to_pixels(mask->ascent());
	    Map(c, ml, mt - 1, mx, my);
            GC gc = XCreateGC(dpy, d->rep()->root_, 0, nil);
            XSetForeground(dpy, gc, foreground->PixelValue());
            XSetBackground(dpy, gc, background->PixelValue());
            XSetGraphicsExposures(dpy, gc, False);
            XSetClipOrigin(dpy, gc, mx, my);
            XSetClipMask(dpy, gc, mask->rep()->pixmap_);
            XCopyPlane(
                dpy, bitmap->rep()->pixmap_, xid,
                gc, 0, 0, bitmap->pwidth(), bitmap->pheight(), dx, dy, 1
            );
            XFreeGC(dpy, gc);
        }
    } else {
	bitmap->rep()->fill();
        if (mask != nil) {
	    mask->rep()->fill();
            DrawTransformedImage(
                bitmap->rep()->image_,
		x - d->to_pixels(bitmap->left_bearing()),
		y - d->to_pixels(bitmap->descent()),
                mask->rep()->image_,
		x - d->to_pixels(mask->left_bearing()),
		y - d->to_pixels(mask->descent()),
                xid, c->pheight(), -xoff, -yoff,
                true, foreground->PixelValue(), background->PixelValue(),
		rep->fillgc, *matrix
            );
        } else {
            DrawTransformedImage(
                bitmap->rep()->image_,
		x - d->to_pixels(bitmap->left_bearing()),
		y - d->to_pixels(bitmap->descent()),
                nil, 0, 0,
                xid, c->pheight(), -xoff, -yoff,
                true, foreground->PixelValue(), background->PixelValue(),
		rep->fillgc, *matrix
            );
        }
    }
}

void Painter::RasterRect(Canvas* c, IntCoord x, IntCoord y, Raster* r) {
    if (c == nil) {
	return;
    }
    Display* d = r->rep()->display_;
    XDisplay* dpy = d->rep()->display_;
    XDrawable xid = c->rep()->xdrawable_;
    if (xid == CanvasRep::unbound) {
	return;
    }
    r->flush();
    PainterDpyInfo& p = *PainterDpyInfo::find(rep->display);
    RasterRep* info = (matrix == nil) ? r->rep() : p.tx_raster(r, *matrix);

    IntCoord rw = (unsigned int)r->pwidth();
    IntCoord rh = (unsigned int)r->pheight();
    IntCoord x1, y1, x2, y2, x3, y3, x4, y4;
    Map(c, x, y, x1, y1);
    Map(c, x, y + rh, x2, y2);
    Map(c, x + rw, y + rh, x3, y3);
    Map(c, x + rw, y, x4, y4);
    IntCoord xmin = Math::min(x1, x2, x3, x4);
    IntCoord ymin = Math::min(y1, y2, y3, y4);

    XPoint clip_area[4];
    clip_area[0].x = x1; clip_area[0].y = y1;
    clip_area[1].x = x2; clip_area[1].y = y2;
    clip_area[2].x = x3; clip_area[2].y = y3;
    clip_area[3].x = x4; clip_area[3].y = y4;
    Region rg = XPolygonRegion(clip_area, 4, EvenOddRule);
    if (rep->clipped) {
	Region tmp = XCreateRegion();
	XUnionRectWithRegion(&rep->xclip[0], tmp, tmp);
	XIntersectRegion(rg, tmp, rg);
	XDestroyRegion(tmp);
    }

    XSetRegion(dpy, rep->fillgc, rg);
    XSetGraphicsExposures(dpy, rep->fillgc, False);
    XCopyArea(
	dpy, info->pixmap_, xid, rep->fillgc,
	0, 0, info->pwidth_, info->pheight_, xmin, ymin
    );
    XSetGraphicsExposures(dpy, rep->fillgc, True);
    XDestroyRegion(rg);

    if (rep->clipped) {
	XSetClipRectangles(dpy, rep->fillgc, 0, 0, rep->xclip, 1, Unsorted);
    } else {
	NoClip();
    }
}

void Painter::Text(Canvas* c, const char* s, int len, IntCoord x, IntCoord y) {
    if (c == nil) {
	return;
    }
    XDisplay* d = rep->display->rep()->display_;
    XDrawable xid = c->rep()->xdrawable_;
    if (xid == CanvasRep::unbound) {
	return;
    }
    IntCoord x0, y0;
    IntCoord ybase = y + font->Baseline() + 1;
    IntCoord ytop = y + font->Height();
    int txstring = TxKey(matrix, font->Width(s, len), font->Height());

    if (style & Reversed) {
        SetColors(GetBgColor(), GetFgColor());
    }
    if (txstring == 0) {
        Map(c, x, ybase - 1, x0, y0);
        if (rep->fillbg) {
            XDrawImageString(
                d, xid, rep->fillgc, x0, y0, s, len
            );
        } else {
            XDrawString(
                d, xid, rep->fillgc, x0, y0, s, len
            );
        }
        if (style & Boldface) {
            XDrawString(
                d, xid, rep->fillgc, x0-1, y0, s, len
            );
        }
    } else {
        IntCoord curx = x;
        float fx0, fy0;
        Transformer notrans(*matrix);
        notrans.Transform(0.0, 0.0, fx0, fy0);
        notrans.Translate(-fx0, -fy0);
        int txchar = TxKey(matrix, font->Width("M"), font->Height());
        Bitmap* bits;
	PainterDpyInfo& p = *PainterDpyInfo::find(rep->display);
	Transformer* oldmatrix;
        for (int i = 0; i < len; ++i) {
            IntCoord nextx = curx + font->Width(s+i, 1);
            if (rep->fillbg) {
                ClearRect(c, curx, y, nextx, ytop);
            }
            switch (p.txfonts) {
            case PainterDpyInfo::TxFontsOff:
                Map(c, curx, ybase - 1, x0, y0);
                XDrawString(
                    d, xid, rep->fillgc, x0, y0, s+i, 1
                );
                if (style & Boldface) {
                    XDrawString(
                        d, xid, rep->fillgc, x0-1, y0, s+i, 1
                    );
                }
                break;
            case PainterDpyInfo::TxFontsOn:
                bits = new Bitmap(font, s[i]);
                Stencil(c, curx, ybase, bits, bits);
                if (style & Boldface) {
                    Stencil(c, curx-1, ybase, bits, bits);
                }
                break;
            case PainterDpyInfo::TxFontsCache:
            case PainterDpyInfo::TxFontsDefault:
                bits = p.get_char_bitmap(font, s[i], txchar, notrans);
                oldmatrix = matrix;
                matrix = nil;
                oldmatrix->Transform(curx, ybase, x0, y0);
                Stencil(c, x0, y0, bits, bits);
                if (style & Boldface) {
                    oldmatrix->Transform(curx+1, ybase, x0, y0);
                    Stencil(c, x0, y0, bits, bits);
                }
                matrix = oldmatrix;
                break;
            }
            curx = nextx;
        }
    }
    if (style & Outlined) {
        /* unimplemented */
    }
    if (style & Underlined) {
        Line(c, x, ybase, x + font->Width(s, len) - 1, ybase);
    }
    if (style & Reversed) {
        SetColors(GetBgColor(), GetFgColor());
    }
}

void Painter::Point(Canvas* c, IntCoord x, IntCoord y) {
    if (c == nil) {
	return;
    }
    CanvasRep* cr = c->rep();
    if (cr->xdrawable_ == CanvasRep::unbound) {
	return;
    }
    IntCoord mx, my;
    Map(c, x, y, mx, my);
    XDrawPoint(cr->dpy(), cr->xdrawable_, rep->fillgc, mx, my);
}

void Painter::MultiPoint(Canvas* c, IntCoord x[], IntCoord y[], int n) {
    if (c == nil) {
	return;
    }
    CanvasRep* cr = c->rep();
    if (cr->xdrawable_ == CanvasRep::unbound) {
	return;
    }
    register XPoint* v = AllocPts(n);
    for (register int i = 0; i < n; i++) {
	Map(c, x[i], y[i], v[i].x, v[i].y);
    }
    XDrawPoints(cr->dpy(), cr->xdrawable_, rep->fillgc, v, n, CoordModeOrigin);
    FreePts(v);
}

void Painter::Line(
    Canvas* c, IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2
) {
    if (c == nil) {
	return;
    }
    CanvasRep* cr = c->rep();
    if (cr->xdrawable_ == CanvasRep::unbound) {
	return;
    }
    IntCoord mx1, my1, mx2, my2;
    Map(c, x1, y1, mx1, my1);
    Map(c, x2, y2, mx2, my2);
    XDrawLine(cr->dpy(), cr->xdrawable_, rep->dashgc, mx1, my1, mx2, my2);
}

void Painter::Rect(
    Canvas* c, IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2
) {
    if (c == nil) {
	return;
    }
    CanvasRep* cr = c->rep();
    if (cr->xdrawable_ == CanvasRep::unbound) {
	return;
    }
    if (matrix != nil && matrix->Rotated() && !matrix->Rotated90()) {
	IntCoord x[4], y[4];

	x[0] = x[3] = x1;
	x[1] = x[2] = x2;
	y[0] = y[1] = y1;
	y[2] = y[3] = y2;
	Polygon(c, x, y, 4);
    } else {
	IntCoord left, bottom, right, top, tmp;
	int w, h;

	Map(c, x1, y1, left, bottom);
	Map(c, x2, y2, right, top);
	if (left > right) {
	    tmp = left; left = right; right = tmp;
	}
	if (top > bottom) {
	    tmp = bottom; bottom = top; top = tmp;
	}
	w = right - left;
	h = bottom - top;
	XDrawRectangle(
	    cr->dpy(), cr->xdrawable_, rep->dashgc, left, top, w, h
        );
    }
}

void Painter::FillRect(
    Canvas* c, IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2
) {
    if (c == nil) {
	return;
    }
    CanvasRep* cr = c->rep();
    if (cr->xdrawable_ == CanvasRep::unbound) {
	return;
    }
    if (matrix != nil && matrix->Rotated() && !matrix->Rotated90()) {
	IntCoord x[4], y[4];

	x[0] = x[3] = x1;
	x[1] = x[2] = x2;
	y[0] = y[1] = y1;
	y[2] = y[3] = y2;
	FillPolygon(c, x, y, 4);
    } else {
	IntCoord left, bottom, right, top, tmp;
	Map(c, x1, y1, left, bottom);
	Map(c, x2, y2, right, top);
	if (left > right) {
	    tmp = left; left = right; right = tmp;
	}
	if (top > bottom) {
	    tmp = bottom; bottom = top; top = tmp;
	}
	unsigned int w = right - left + 1;
	unsigned int h = bottom - top + 1;
	XFillRectangle(
	    cr->dpy(), cr->xdrawable_, rep->fillgc, left, top, w, h
        );
    }
}

void Painter::ClearRect(
    Canvas* c, IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2
) {
    if (c == nil) {
	return;
    }
    CanvasRep* cr = c->rep();
    if (cr->xdrawable_ == CanvasRep::unbound) {
	return;
    }
    XDisplay* dpy = cr->dpy();
    XSetForeground(dpy, rep->fillgc, background->PixelValue());
    XSetFillStyle(dpy, rep->fillgc, FillSolid);
    FillRect(c, x1, y1, x2, y2);
    XSetForeground(dpy, rep->fillgc, foreground->PixelValue());
    rep->PrepareFill(pattern);
}

void Painter::Circle(Canvas* c, IntCoord x, IntCoord y, int r) {
    if (c == nil) {
	return;
    }
    CanvasRep* cr = c->rep();
    if (cr->xdrawable_ == CanvasRep::unbound) {
	return;
    }
    if (matrix != nil && (matrix->Stretched() || matrix->Rotated())) {
	Ellipse(c, x, y, r, r);
    } else {
	IntCoord left, top, right, bottom, tmp;
	Map(c, x-r, y+r, left, top);
        Map(c, x+r, y-r, right, bottom);
	if (left > right) {
	    tmp = left; left = right; right = tmp;
	}
	if (top > bottom) {
	    tmp = bottom; bottom = top; top = tmp;
	}
	XDrawArc(
	    cr->dpy(), cr->xdrawable_, rep->dashgc,
            left, top, right-left, bottom-top, 0, 360*64
	);
    }
}

void Painter::FillCircle(Canvas* c, IntCoord x, IntCoord y, int r) {
    if (c == nil) {
	return;
    }
    CanvasRep* cr = c->rep();
    if (cr->xdrawable_ == CanvasRep::unbound) {
	return;
    }
    if (matrix != nil && (matrix->Stretched() || matrix->Rotated())) {
	FillEllipse(c, x, y, r, r);
    } else {
	IntCoord left, top, right, bottom, tmp;
	Map(c, x-r, y+r, left, top);
        Map(c, x+r, y-r, right, bottom);
	if (left > right) {
	    tmp = left; left = right; right = tmp;
	}
	if (top > bottom) {
	    tmp = bottom; bottom = top; top = tmp;
	}
	XFillArc(
	    cr->dpy(), cr->xdrawable_, rep->fillgc,
            left, top, right-left, bottom-top, 0, 360*64
	);
    }
}

void Painter::MultiLine(Canvas* c, IntCoord x[], IntCoord y[], int n) {
    if (c == nil) {
	return;
    }
    CanvasRep* cr = c->rep();
    if (cr->xdrawable_ == CanvasRep::unbound) {
	return;
    }
    register XPoint* v = AllocPts(n);
    for (register int i = 0; i < n; i++) {
	Map(c, x[i], y[i], v[i].x, v[i].y);
    }
    XDrawLines(cr->dpy(), cr->xdrawable_, rep->dashgc, v, n, CoordModeOrigin);
    FreePts(v);
}

void Painter::MultiLineNoMap(Canvas* c, IntCoord x[], IntCoord y[], int n) {
    if (c == nil) {
	return;
    }
    CanvasRep* cr = c->rep();
    if (cr->xdrawable_ == CanvasRep::unbound) {
	return;
    }
    register XPoint* v = AllocPts(n);
    for (register int i = 0; i < n; i++) {
	v[i].x = x[i];
	v[i].y = y[i];
    }
    XDrawLines(cr->dpy(), cr->xdrawable_, rep->dashgc, v, n, CoordModeOrigin);
    FreePts(v);
}

void Painter::Polygon(Canvas* c, IntCoord x[], IntCoord y[], int n) {
    if (c == nil) {
	return;
    }
    CanvasRep* cr = c->rep();
    if (cr->xdrawable_ == CanvasRep::unbound) {
	return;
    }
    register XPoint* v = AllocPts(n+1);
    for (register int i = 0; i < n; i++) {
	Map(c, x[i], y[i], v[i].x, v[i].y);
    }
    if (x[i-1] != x[0] || y[i-1] != y[0]) {
	v[i] = v[0];
	++i;
    }
    XDrawLines(cr->dpy(), cr->xdrawable_, rep->dashgc, v, i, CoordModeOrigin);
    FreePts(v);
}

void Painter::FillPolygonNoMap(Canvas* c, IntCoord x[], IntCoord y[], int n) {
    if (c == nil) {
	return;
    }
    CanvasRep* cr = c->rep();
    if (cr->xdrawable_ == CanvasRep::unbound) {
	return;
    }
    register XPoint* v = AllocPts(n);
    for (register int i = 0; i < n; i++) {
	v[i].x = x[i];
	v[i].y = y[i];
    }
    XFillPolygon(
	cr->dpy(), cr->xdrawable_, rep->fillgc, v, n, Complex, CoordModeOrigin
    );
    FreePts(v);
}

void Painter::FillPolygon(Canvas* c, IntCoord x[], IntCoord y[], int n) {
    if (c == nil) {
	return;
    }
    CanvasRep* cr = c->rep();
    if (cr->xdrawable_ == CanvasRep::unbound) {
	return;
    }
    register XPoint* v = AllocPts(n+1);
    for (register int i = 0; i < n; i++) {
	Map(c, x[i], y[i], v[i].x, v[i].y);
    }
    XFillPolygon(
	cr->dpy(), cr->xdrawable_, rep->fillgc, v, n, Complex, CoordModeOrigin
    );
    FreePts(v);
}

void Painter::Copy(
    Canvas* src, IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2,
    Canvas* dst, IntCoord x0, IntCoord y0
) {
    if (src == nil || dst == nil) {
	return;
    }
    XDisplay* dpy = dst->rep()->dpy();
    XDrawable src_xid = src->rep()->xdrawable_;
    XDrawable dst_xid = dst->rep()->xdrawable_;
    if (src_xid == CanvasRep::unbound || dst_xid == CanvasRep::unbound) {
	return;
    }
    IntCoord sx1, sy1, sx2, sy2, sx3, sy3, sx4, sy4, dx1, dy1;
    if (matrix == nil) {
	sx1 = x1; sy1 = y1;
	sx2 = x1; sy2 = y2;
	sx3 = x2; sy3 = y2;
	sx4 = x2; sy4 = y1;
	dx1 = x0; dy1 = y0;
    } else {
	Transformer t(*matrix);

	t.Transform(x1, y1, sx1, sy1);
	t.Transform(x1, y2, sx2, sy2);
	t.Transform(x2, y2, sx3, sy3);
	t.Transform(x2, y1, sx4, sy4);
	t.Transform(x0, y0, dx1, dy1);
    }

    int minx = Math::min(sx1, sx2, sx3, sx4);
    int maxx = Math::max(sx1, sx2, sx3, sx4);
    int miny = Math::min(sy1, sy2, sy3, sy4);
    int maxy = Math::max(sy1, sy2, sy3, sy4);

    int w = maxx - minx + 1;
    int h = maxy - miny + 1;
    int sx = minx + xoff;
    int sy = src->pheight() - 1 - (maxy + yoff);
    int dx = dx1 - (sx1 - minx) + xoff;
    int dy = dst->pheight() - 1 - (dy1 - (sy1 - maxy) + yoff);

    if ((sx1 == sx2 || sy1 == sy2) && (sx1 == sx4 || sy1 == sy4)) {
        if (src->status() == Canvas::offscreen) {
            XSetGraphicsExposures(dpy, rep->fillgc, False);
            XCopyArea(
                dpy, src_xid, dst_xid, rep->fillgc, sx, sy, w, h, dx, dy
            );
            XSetGraphicsExposures(dpy, rep->fillgc, True);
        } else {
            XCopyArea(
                dpy, src_xid, dst_xid, rep->fillgc, sx, sy, w, h, dx, dy
            );
            dst->rep()->wait_for_copy();
        }
    } else {
        GC copygc = XCreateGC(dpy, dst_xid, 0, nil);
        Pixmap mask;
        mask = XCreatePixmap(dpy, rep->display->rep()->root_, w, h, 1);
        GC maskgc = XCreateGC(dpy, mask, 0, nil);
        XSetForeground(dpy, maskgc, 0);
        XFillRectangle(dpy, mask, maskgc, 0, 0, w, h);
        XSetForeground(dpy, maskgc, 1);
        XPoint v[4];
        v[0].x = sx1 - minx; v[0].y = maxy - sy1;
        v[1].x = sx2 - minx; v[1].y = maxy - sy2;
        v[2].x = sx3 - minx; v[2].y = maxy - sy3;
        v[3].x = sx4 - minx; v[3].y = maxy - sy4;
        XFillPolygon(dpy, mask, maskgc, v, 4, Convex, CoordModeOrigin);
        XFreeGC(dpy, maskgc);
        XSetClipOrigin(dpy, copygc, dx, dy);
        XSetClipMask(dpy, copygc, mask);
        if (src->status() == Canvas::offscreen) {
            XSetGraphicsExposures(dpy, copygc, False);
            XCopyArea(
                dpy, src_xid, dst_xid, copygc, sx, sy, w, h, dx, dy
            );
            XSetGraphicsExposures(dpy, copygc, True);
        } else {
            XCopyArea(
                dpy, src_xid, dst_xid, copygc, sx, sy, w, h, dx, dy
            );
            dst->rep()->wait_for_copy();
        }
        XFreePixmap(dpy, mask);
        XFreeGC(dpy, copygc);
    }
}

/* anachronism */
void CanvasRep::wait_for_copy() {
    Event e;
    XEvent& xe = e.rep()->xevent_;
    DisplayRep* r = display_->rep();
    XDisplay* dpy = r->display_;
    WindowTable* wt = r->wtable_;
    Window* w;

    boolean keep_waiting = true;
    while (keep_waiting) {
	XWindowEvent(dpy, xdrawable_, ExposureMask, &xe);
	switch (xe.type) {
	case NoExpose:
	    keep_waiting = false;
	    break;
	case Expose:
	    if (wt->find(w, xe.xexpose.window)) {
		w->receive(e);
	    }
	    break;
	case GraphicsExpose:
	    if (wt->find(w, xe.xgraphicsexpose.drawable)) {
		w->receive(e);
	    }
	    if (xe.xgraphicsexpose.count == 0) {
		keep_waiting = false;
	    }
	    break;
	}
    }
}
