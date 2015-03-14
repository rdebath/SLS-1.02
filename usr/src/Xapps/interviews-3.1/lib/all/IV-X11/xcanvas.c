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

#include <InterViews/bitmap.h>
#include <InterViews/brush.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/display.h>
#include <InterViews/font.h>
#include <InterViews/raster.h>
#include <InterViews/style.h>
#include <InterViews/transformer.h>
#include <InterViews/window.h>
#include <IV-X11/Xlib.h>
#include <IV-X11/Xutil.h>
#include <IV-X11/xbitmap.h>
#include <IV-X11/xbrush.h>
#include <IV-X11/xcanvas.h>
#include <IV-X11/xcolor.h>
#include <IV-X11/xdisplay.h>
#include <IV-X11/xfont.h>
#include <IV-X11/xraster.h>
#include <OS/math.h>
#include <OS/list.h>
#include <OS/table2.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#ifdef __DECCXX
struct _XRegion;
#endif

declarePtrList(TransformerStack,Transformer)
implementPtrList(TransformerStack,Transformer)

declarePtrList(ClippingStack,_XRegion)
implementPtrList(ClippingStack,_XRegion)

/* class Canvas */

Canvas::Canvas() {
    CanvasRep* c = new CanvasRep;
    rep_ = c;

    TextRenderInfo* t = &CanvasRep::text_;
    PathRenderInfo* p = &CanvasRep::path_;
    if (t->text_ == nil) {
        t->text_ = new char[1000];
        t->cur_text_ = t->text_;
        t->items_ = new XTextItem[100];
    }
    if (p->point_ == nil) {
        p->point_ = new XPoint[10];
        p->cur_point_ = p->point_;
	p->end_point_ = p->point_ + 10;
    }
    c->drawbuffer_ = CanvasRep::unbound;
    c->copybuffer_ = CanvasRep::unbound;
    c->drawgc_ = nil;
    c->copygc_ = nil;
    c->brush_ = nil;
    c->brush_width_ = 0;
    c->dash_list_ = nil;
    c->dash_count_ = 0;
    c->color_ = nil;
    c->pixel_ = 0;
    c->stipple_ = 0;
    c->font_ = nil;
    c->text_twobyte_ = false;
    c->text_reencode_ = false;
    c->clipping_ = XCreateRegion();
    c->empty_ = XCreateRegion();
    c->transformers_ = new TransformerStack;
    c->clippers_ = new ClippingStack;

    Transformer* identity = new Transformer;
    c->transformers_->append(identity);
    c->transformed_ = false;

    c->display_ = nil;
    c->window_ = nil;
    c->xdrawable_ = CanvasRep::unbound;

    c->width_ = 0;
    c->height_ = 0;
    c->pwidth_ = 0;
    c->pheight_ = 0;

    c->damaged_ = false;
    c->on_damage_list_ = false;
    c->repairing_ = false;

    c->status_ = unmapped;
}

Canvas::~Canvas() {
    CanvasRep* c = rep();
    c->unbind();
    for (ListItr(TransformerStack) i(*c->transformers_); i.more(); i.next()) {
	Transformer* t = i.cur();
	delete t;
    }
    delete c->transformers_;
    XDestroyRegion(c->clipping_);
    XDestroyRegion(c->empty_);
    delete c->clippers_;
    delete c;
    rep_ = nil;
}

void Canvas::size(Coord width, Coord height) {
    CanvasRep* c = rep();
    c->width_ = width;
    c->height_ = height;
    Display* d = c->display_;
    if (d != nil) {
	c->pwidth_ = d->to_pixels(width);
	c->pheight_ = d->to_pixels(height);
    }
}

void Canvas::psize(PixelCoord pwidth, PixelCoord pheight) {
    CanvasRep& c = *rep();
    c.pwidth_ = pwidth;
    c.pheight_ = pheight;
    Display* d = c.display_;
    if (d != nil) {
	c.width_ = d->to_coord(pwidth);
	c.height_ = d->to_coord(pheight);
    }
}

Coord Canvas::width() const { return rep()->width_; }
Coord Canvas::height() const { return rep()->height_; }
PixelCoord Canvas::pwidth() const { return rep()->pwidth_; }
PixelCoord Canvas::pheight() const { return rep()->pheight_; }

PixelCoord Canvas::to_pixels(Coord p) const {
    return rep()->display_->to_pixels(p);
}

Coord Canvas::to_coord(PixelCoord p) const {
    return rep()->display_->to_coord(p);
}

Coord Canvas::to_pixels_coord(Coord p) const {
    const Display& d = *rep()->display_;
    return d.to_coord(d.to_pixels(p));
}

void Canvas::push_transform() {
    CanvasRep* c = rep();
    c->flush();
    TransformerStack& s = *c->transformers_;
    Transformer* m = new Transformer(*s.item(s.count() - 1));
    s.append(m);
}

void Canvas::pop_transform() {
    CanvasRep* c = rep();
    c->flush();
    TransformerStack& s = *c->transformers_;
    long i = s.count() - 1;
    if (i == 0) {
	/*
	 * We pushed the first matrix during initialization,
	 * so we must be underflowing the stack.  Should be an exception.
	 */
	return;
    }
    Transformer* m = s.item(i);
    delete m;
    s.remove(i);
    c->transformed_ = !c->matrix().identity();
}

void Canvas::transform(const Transformer& t) {
    CanvasRep* c = rep();
    c->flush();
    c->matrix().premultiply(t);
    c->transformed_ = !c->matrix().identity();
}

void Canvas::transformer(const Transformer& t) {
    CanvasRep* c = rep();
    c->flush();
    c->matrix() = t;
    c->transformed_ = !t.identity();
}

const Transformer& Canvas::transformer() const {
    CanvasRep* c = rep();
    return c->matrix();
}

/*
 * There doesn't appear to be any way to copy a region other than
 * to union it with an empty region.
 */

void Canvas::push_clipping() {
    CanvasRep* c = rep();
    c->flush();
    Region old_clip = c->clipping_;
    Region new_clip = XCreateRegion();
    XUnionRegion(old_clip, c->empty_, new_clip);
    c->clippers_->append(old_clip);
    c->clipping_ = new_clip;
}

void Canvas::pop_clipping() {
    CanvasRep* c = rep();
    c->flush();
    ClippingStack& s = *c->clippers_;
    long n = s.count();
    if (n == 0) {
	/* stack underflow--should raise exception */
	return;
    }

    Region clip = c->clipping_;
    XDestroyRegion(clip);
    clip = s.item(n - 1);
    s.remove(n - 1);
    c->clipping_ = clip;

    XDisplay* dpy = c->display_->rep()->display_;
    GC gc = c->drawgc_;
    if (XEmptyRegion(clip)) {
	XSetClipMask(dpy, gc, None);
    } else {
	XSetRegion(dpy, gc, clip);
    }
}

void Canvas::front_buffer() {
    CanvasRep& c = *rep();
    if (c.copybuffer_ != CanvasRep::unbound) {
	c.drawbuffer_ = c.copybuffer_;
    }
}

void Canvas::back_buffer() {
    CanvasRep& c = *rep();
    if (c.copybuffer_ != CanvasRep::unbound) {
	c.drawbuffer_ = c.xdrawable_;
    }
}

static boolean rectangular(
    int x0, int y0, int x1, int y1, int x2, int y2, int x3, int y3
) {
    return (
        (x0 == x1 && y1 == y2 && x2 == x3 && y3 == y0) ||
        (x0 == x3 && y3 == y2 && x2 == x1 && y1 == y0)
    );
}

static boolean xrect(const XPoint* p, unsigned int n) {
    return (
	n == 5 && p[0].x == p[4].x && p[0].y == p[4].y &&
	rectangular(
	    p[0].x, p[0].y, p[1].x, p[1].y, p[2].x, p[2].y, p[3].x, p[3].y
	)
    );
}

static const float smoothness = 10.0;

static boolean straight(
    const Transformer& tx,
    Coord x0, Coord y0, Coord x1, Coord y1,
    Coord x2, Coord y2, Coord x3, Coord y3
) {
    Coord tx0, tx1, tx2, tx3;
    Coord ty0, ty1, ty2, ty3;
    tx.transform(x0, y0, tx0, ty0);
    tx.transform(x1, y1, tx1, ty1);
    tx.transform(x2, y2, tx2, ty2);
    tx.transform(x3, y3, tx3, ty3);
    float f = (
        (tx1 + tx2) * (ty0 - ty3) + (ty1 + ty2) * (tx3 - tx0)
        + 2 * (tx0 * ty3 - ty0 * tx3)
    );
    return (f * f) < smoothness;
}

static inline Coord mid(Coord a, Coord b) {
    return (a + b) / 2;
}

static char _txkey (int i) {
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

static int tx_key(const Transformer& tx, Coord x, Coord y) {
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

void Canvas::new_path() {
    PathRenderInfo* p = &CanvasRep::path_;
    p->curx_ = 0;
    p->cury_ = 0;
    XPoint* xp = p->point_;
    xp->x = 0;
    xp->y = 0;
    p->cur_point_ = xp;
}

void Canvas::move_to(Coord x, Coord y) {
    CanvasRep* c = rep();
    PathRenderInfo* p = &CanvasRep::path_;
    p->curx_ = x;
    p->cury_ = y;
    Coord tx, ty;
    if (c->transformed_) {
	c->matrix().transform(x, y, tx, ty);
    } else {
	tx = x;
	ty = y;
    }
    Display* d = c->display_;
    XPoint* xp = p->point_;
    xp->x = d->to_pixels(tx);
    xp->y = c->pheight_ - d->to_pixels(ty);
    p->cur_point_ = xp + 1;
}

void Canvas::line_to(Coord x, Coord y) {
    CanvasRep* c = rep();
    PathRenderInfo* p = &CanvasRep::path_;
    p->curx_ = x;
    p->cury_ = y;
    Coord tx, ty;
    if (c->transformed_) {
	c->matrix().transform(x, y, tx, ty);
    } else {
	tx = x;
	ty = y;
    }
    if (p->cur_point_ == p->end_point_) {
	int old_size = p->cur_point_ - p->point_;
	int new_size = 2 * old_size;
        XPoint* new_path = new XPoint[new_size];
        for (int i = 0; i < old_size; i++) {
            new_path[i] = p->point_[i];
        }
        delete p->point_;
        p->point_ = new_path;
	p->cur_point_ = p->point_ + old_size;
	p->end_point_ = p->point_ + new_size;
    }
    Display* d = c->display_;
    XPoint* xp = p->cur_point_;
    xp->x = d->to_pixels(tx);
    xp->y = c->pheight_ - d->to_pixels(ty);
    p->cur_point_ = xp + 1;
}

void Canvas::curve_to(
    Coord x, Coord y, Coord x1, Coord y1, Coord x2, Coord y2
) {
    CanvasRep* c = rep();
    PathRenderInfo* p = &CanvasRep::path_;
    Coord px = p->curx_;
    Coord py = p->cury_;

    if (straight(c->matrix(), px, py, x1, y1, x2, y2, x, y)) {
        line_to(x, y);
    } else {
        Coord xx = mid(x1, x2);
        Coord yy = mid(y1, y2);
        Coord x11 = mid(px, x1);
        Coord y11 = mid(py, y1);
        Coord x22 = mid(x2, x);
        Coord y22 = mid(y2, y);
        Coord x12 = mid(x11, xx);
        Coord y12 = mid(y11, yy);
        Coord x21 = mid(xx, x22);
        Coord y21 = mid(yy, y22);
        Coord cx = mid(x12, x21);
        Coord cy = mid(y12, y21);

        curve_to(cx, cy, x11, y11, x12, y12);
        curve_to(x, y, x21, y21, x22, y22);
    }
}

void Canvas::close_path() {
    PathRenderInfo* p = &CanvasRep::path_;
    XPoint* startp = p->point_;
    XPoint* xp = p->cur_point_;
    xp->x = startp->x;
    xp->y = startp->y;
    p->cur_point_ = xp + 1;
}

void Canvas::stroke(const Color* color, const Brush* b) {
    CanvasRep* c = rep();
    PathRenderInfo* p = &CanvasRep::path_;
    int n = p->cur_point_ - p->point_;
    if (n < 2) {
	return;
    }
    c->flush();
    c->color(color);
    c->brush(b);
    XDisplay* dpy = c->display_->rep()->display_;
    XDrawable d = c->drawbuffer_;
    GC gc = c->drawgc_;
    XPoint* pt = p->point_;
    if (n == 2) {
        XDrawLine(dpy, d, gc, pt[0].x, pt[0].y, pt[1].x, pt[1].y);
    } else if (xrect(pt, n)) {
        int x = Math::min(pt[0].x, pt[2].x);
        int y = Math::min(pt[0].y, pt[2].y);
        int w = Math::abs(pt[0].x - pt[2].x);
        int h = Math::abs(pt[0].y - pt[2].y);
        XDrawRectangle(dpy, d, gc, x, y, w, h);
    } else {
        XDrawLines(dpy, d, gc, pt, n, CoordModeOrigin);
    }
}

void Canvas::line(
    Coord x1, Coord y1, Coord x2, Coord y2, const Color* c, const Brush* b
) {
    new_path();
    move_to(x1, y1);
    line_to(x2, y2);
    stroke(c, b);
}

static void rect_path(Canvas* c, Coord l, Coord b, Coord r, Coord t) {
    c->new_path();
    c->move_to(l, b);
    c->line_to(l, t);
    c->line_to(r, t);
    c->line_to(r, b);
    c->close_path();
}

void Canvas::rect(
    Coord l, Coord b, Coord r, Coord t, const Color* c, const Brush* br
) {
    rect_path(this, l, b, r, t);
    stroke(c, br);
}

void Canvas::fill(const Color* color) {
    CanvasRep* c = rep();
    PathRenderInfo* p = &CanvasRep::path_;
    int n = p->cur_point_ - p->point_;
    if (n <= 2) {
	return;
    }
    c->flush();
    c->color(color);
    XDisplay* dpy = c->display_->rep()->display_;
    XDrawable d = c->drawbuffer_;
    GC gc = c->drawgc_;
    XPoint* pt = p->point_;
    if (xrect(pt, n)) {
        int x = Math::min(pt[0].x, pt[2].x);
        int y = Math::min(pt[0].y, pt[2].y);
        int w = Math::abs(pt[0].x - pt[2].x);
        int h = Math::abs(pt[0].y - pt[2].y);
        XFillRectangle(dpy, d, gc, x, y, w, h);
    } else {
        XFillPolygon(dpy, d, gc, pt, n, Complex, CoordModeOrigin);
    }
}

void Canvas::fill_rect(Coord l, Coord b, Coord r, Coord t, const Color* c) {
    rect_path(this, l, b, r, t);
    fill(c);
}

void Canvas::clip() {
    CanvasRep* c = rep();
    c->flush();
    PathRenderInfo* p = &CanvasRep::path_;
    int n = p->cur_point_ - p->point_;
    if (n <= 2) {
	return;
    }
    Region clip;
    XPoint* pt = p->point_;
    if (xrect(pt, n)) {
	XRectangle xr;
	xr.x = Math::min(pt[0].x, pt[2].x);
	xr.y = Math::min(pt[0].y, pt[2].y);
	xr.width = Math::abs(pt[0].x - pt[2].x);
	xr.height = Math::abs(pt[0].y - pt[2].y);
	clip = XCreateRegion();
	XUnionRectWithRegion(&xr, c->empty_, clip);
    } else {
	clip = XPolygonRegion(pt, n, EvenOddRule);
    }

    if (!XEmptyRegion(c->clipping_)) {
	Region intersect = XCreateRegion();
	XIntersectRegion(c->clipping_, clip, intersect);
	XDestroyRegion(clip);
	clip = intersect;
    }
    XDestroyRegion(c->clipping_);
    c->clipping_ = clip;
    XSetRegion(c->dpy(), c->drawgc_, clip);
}

void Canvas::clip_rect(Coord l, Coord b, Coord r, Coord t) {
    rect_path(this, l, b, r, t);
    clip();
}

static const int adobe_to_iso8859[] = {
    0000, 0001, 0002, 0003, 0004, 0005, 0006, 0007,
    0010, 0011, 0012, 0013, 0014, 0015, 0016, 0017,
    0020, 0021, 0022, 0023, 0024, 0025, 0026, 0027,
    0030, 0031, 0032, 0033, 0034, 0035, 0036, 0037,
    0040, 0041, 0042, 0043, 0044, 0045, 0046, 0047,
    0050, 0051, 0052, 0053, 0054, 0055, 0056, 0057,
    0060, 0061, 0062, 0063, 0064, 0065, 0066, 0067,
    0070, 0071, 0072, 0073, 0074, 0075, 0076, 0077,

    0100, 0101, 0102, 0103, 0104, 0105, 0106, 0107,
    0110, 0111, 0112, 0113, 0114, 0115, 0116, 0117,
    0120, 0121, 0122, 0123, 0124, 0125, 0126, 0127,
    0130, 0131, 0132, 0133, 0134, 0135, 0136, 0137,
    0140, 0141, 0142, 0143, 0144, 0145, 0146, 0147,
    0150, 0151, 0152, 0153, 0154, 0155, 0156, 0157,
    0160, 0161, 0162, 0163, 0164, 0165, 0166, 0167,
    0170, 0171, 0172, 0173, 0174, 0175, 0176, 0177,

    0200, 0201, 0202, 0203, 0204, 0205, 0206, 0207,
    0210, 0211, 0212, 0213, 0214, 0215, 0216, 0217,
    0220, 0221, 0222, 0223, 0224, 0225, 0226, 0227,
    0230, 0231, 0232, 0233, 0234, 0235, 0236, 0237,
    0240, 0241, 0242, 0243, 0057, 0245, 0000, 0247,
    0244, 0264, 0042, 0253, 0000, 0000, 0000, 0000,
    0000, 0055, 0000, 0000, 0267, 0000, 0266, 0052,
    0000, 0000, 0042, 0273, 0137, 0000, 0000, 0277,

    0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000,
    0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000,
    0055, 0000, 0000, 0000, 0000, 0000, 0000, 0000,
    0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000,
    0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000,
    0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000,
    0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000,
    0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000
};

declareTable2(CharBitmapTable,const Font*,long,const Bitmap*)
implementTable2(CharBitmapTable,const Font*,long,const Bitmap*)

static CharBitmapTable* _char_bitmaps;

static const Bitmap* char_bitmap(Display* d, const Font* font, long c) {
    if (_char_bitmaps == nil) {
	_char_bitmaps = new CharBitmapTable(1024);
    }
    const Bitmap* b;
    if (!_char_bitmaps->find(b, font, c)) {
        b = new Bitmap(font, c, font->rep(d)->scale_);
        _char_bitmaps->insert(font, c, b);
    }
    return b;
}

void Canvas::character(
    const Font* f, long ch, Coord width, const Color* color, Coord x, Coord y
) {
    CanvasRep* c = rep();
    int int_ch = int(ch);
    boolean is_flush = !isprint(int_ch);
    if (f != nil && f != c->font_) {
	c->flush();
	c->font(f);
    }
    if (color != nil && color != c->color_) {
	c->flush();
	c->color(color);
    }
    Transformer& m = c->matrix();
    if (!c->font_is_scaled_ &&
        (!c->transformed_ || tx_key(m, width, width) == 0)
    ) {
	TextRenderInfo* tr = &CanvasRep::text_;
        if (is_flush || y != tr->cury_ ||
	    !Math::equal(x, tr->curx_, float(0.1))
	) {
            c->flush();
        }
	char* cp = tr->cur_text_;
        if (cp == tr->text_) {
            Coord tx = x;
            Coord ty = y;
            if (c->transformed_) {
                m.transform(tx, ty);
            }
            tr->canvas_ = c;
            tr->drawgc_ = c->drawgc_;
	    Display* d = c->display_;
            tr->x0_ = d->to_pixels(tx);
            tr->y0_ = c->pheight_ - d->to_pixels(ty);
            tr->width_ = 0;
        }
        tr->width_ += width;
        tr->curx_ = x + width;
        tr->cury_ = y;
        if (c->text_twobyte_) {
            *cp++ = char((ch & 0xff00) >> 8);
	    *cp++ = char(ch & 0xff);
	} else if (c->text_reencode_) {
	    *cp++ = adobe_to_iso8859[ch & 0xff];
        } else {
	    *cp++ = char(ch & 0xff);
        }
	tr->cur_text_ = cp;
        if (ch == ' ') {
            tr->spaces_ += 1;
            if (cp > tr->text_ + 1) {
		c->flush();
	    }
        }
        if (is_flush) {
            c->flush();
        }
    } else if (ch != ' ') {
        c->flush();
        stencil(char_bitmap(c->display_, f, ch), color, x, y);
    }
}

declareTable2(TxBitmapTable,const Bitmap*,int,BitmapRep*)
implementTable2(TxBitmapTable,const Bitmap*,int,BitmapRep*)

static TxBitmapTable* _tx_bitmaps;

static BitmapRep* tx_bitmap(const Bitmap* b, const Transformer& tx) {
    int key = tx_key(tx, b->width(), b->height());
    if (key == 0) {
        return b->rep();
    } else {
	if (_tx_bitmaps == nil) {
	    _tx_bitmaps = new TxBitmapTable(1024);
	}
        BitmapRep* rep;
        if (!_tx_bitmaps->find(rep, b, key)) {
	    Display* d = b->rep()->display_;
            rep = new BitmapRep;

            Transformer v(tx);

            Coord x0, y0;
            v.transform(0, 0, x0, y0);
            v.translate(-x0, -y0);

	    Coord x1, x2, x3, x4;
	    Coord y1, y2, y3, y4;

	    v.transform(-b->left_bearing(), -b->descent(), x1, y1);
	    v.transform(-b->left_bearing(), b->ascent(), x2, y2);
	    v.transform(b->right_bearing(), b->ascent(), x3, y3);
	    v.transform(b->right_bearing(), -b->descent(), x4, y4);

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

	    XDisplay* dpy = d->rep()->display_;
            BitmapRep* srep = b->rep();

            XImage* source = XGetImage(
                dpy, srep->pixmap_,
                0, 0, srep->pwidth_, srep->pheight_, 0x01, ZPixmap
            );

            Pixmap map = XCreatePixmap(dpy, d->rep()->root_, width, height, 1);
            GC xgc = XCreateGC(dpy, map, 0, nil);
            XSetForeground(dpy, xgc, 0);
            XFillRectangle(dpy, map, xgc, 0, 0, width, height);
            XImage* dest = XGetImage(
                dpy, map, 0, 0, width, height, 0x01, ZPixmap
            );

            int dx0 = d->to_pixels(-xmin);
            int dy0 = d->to_pixels(-ymin);
            int sx0 = d->to_pixels(b->left_bearing());
            int sy0 = d->to_pixels(b->descent());
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
                        sx >= 0 && sx < srep->pwidth_
                        && sy >= 0 && sy < srep->pheight_
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
            _tx_bitmaps->insert(b, key, rep);
        }
        return rep;
    }
}

void Canvas::stencil(
    const Bitmap* mask, const Color* color, Coord x, Coord y
) {
    CanvasRep& c = *rep();
    c.flush();

    XDisplay* dpy = c.dpy();
    XDrawable d = c.drawbuffer_;
    Transformer& m = c.matrix();
    mask->flush();
    BitmapRep* info = tx_bitmap(mask, m);
    Coord tx, ty;
    if (c.transformed_) {
	m.transform(x, y, tx, ty);
    } else {
	tx = x;
	ty = y;
    }
    Display* disp = c.display_;
    int pleft = disp->to_pixels(tx + info->left_);
    int ptop = c.pheight_ - disp->to_pixels(ty + info->top_);

    XGCValues gcv;
    unsigned long valuemask = 0;

    valuemask |= GCFunction;
    gcv.function = GXand;
    valuemask |= GCForeground;
    gcv.foreground = 0;
    valuemask |= GCBackground;
    gcv.background = AllPlanes;
    valuemask |= GCGraphicsExposures;
    gcv.graphics_exposures = False;

    GC xgc = XCreateGC(dpy, d, valuemask, &gcv);
    XCopyGC(dpy, c.drawgc_, GCClipMask, xgc);

    XCopyPlane(
	dpy, info->pixmap_, d, xgc,
	0, 0, info->pwidth_, info->pheight_, pleft, ptop, 1
    );

    gcv.function = GXxor;
    gcv.foreground = color->rep(c.window_->rep()->visual_)->xcolor_.pixel;
    gcv.background = 0;
    valuemask &= ~GCGraphicsExposures;
    XChangeGC(dpy, xgc, valuemask, &gcv);
    XCopyPlane(
	dpy, info->pixmap_, d, xgc,
	0, 0, info->pwidth_, info->pheight_, pleft, ptop, 1
    );
    XFreeGC(dpy, xgc);
}

declareTable2(TxRasterTable,const Raster*,int,RasterRep*)
implementTable2(TxRasterTable,const Raster*,int,RasterRep*)

static TxRasterTable* _tx_rasters;

static RasterRep* tx_raster(const Raster* r, const Transformer& tx) {
    int key = tx_key(tx, r->width(), r->height());
    if (key == 0) {
        return r->rep();
    } else {
	if (_tx_rasters == nil) {
	    _tx_rasters = new TxRasterTable(1024);
	}
        RasterRep* rep;
        if (!_tx_rasters->find(rep, r, key)) {
	    Display* d = r->rep()->display_;
	    DisplayRep& dr = *d->rep();
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
            _tx_rasters->insert(r, key, rep);
        }
        return rep;
    }
}

void Canvas::image(const Raster* image, Coord x, Coord y) {
    CanvasRep* c = rep();
    c->flush();

    XDisplay* dpy = c->dpy();
    GC gc = c->drawgc_;
    Transformer& m = c->matrix();

    image->flush();
    RasterRep* info = tx_raster(image, m);

    Coord tx, ty;
    if (c->transformed_) {
	m.transform(x, y, tx, ty);
    } else {
	tx = x;
	ty = y;
    }

    Display* d = c->display_;
    int pleft = d->to_pixels(tx + info->left_);
    int ptop = c->pheight_ - d->to_pixels(ty + info->top_);

    /*
     * We assume that graphics exposures are off in the gc.
     */
    XCopyArea(
	dpy, info->pixmap_, c->drawbuffer_, gc,
	0, 0, info->pwidth_, info->pheight_, pleft, ptop
    );
}

Window* Canvas::window() const { return rep()->window_; }

/*
 * Damage coordinates are always absolute with respect to the canvas,
 * so they are not transformed.
 */

void Canvas::damage(const Extension& ext) {
    damage(ext.left(), ext.bottom(), ext.right(), ext.top());
}

void Canvas::damage(Coord left, Coord bottom, Coord right, Coord top) {
    CanvasRep& c = *rep();
    CanvasDamage& damage = c.damage_;
    if (c.damaged_) {
	damage.left = Math::min(damage.left, left);
	damage.bottom = Math::min(damage.bottom, bottom);
	damage.right = Math::max(damage.right, right);
	damage.top = Math::max(damage.top, top);
    } else {
	damage.left = left;
	damage.bottom = bottom;
	damage.right = right;
	damage.top = top;
    }
    c.new_damage();
}

boolean Canvas::damaged(const Extension& ext) const {
    return damaged(ext.left(), ext.bottom(), ext.right(), ext.top());
}

boolean Canvas::damaged(
    Coord left, Coord bottom, Coord right, Coord top
) const {
    CanvasRep& c = *rep();
    CanvasDamage& damage = c.damage_;
    return (
	c.damaged_ &&
	left < damage.right && right > damage.left &&
	bottom < damage.top && top > damage.bottom
    );
}

void Canvas::damage_area(Extension& ext) {
    CanvasRep& c = *rep();
    CanvasDamage& damage = c.damage_;
    ext.set_xy(nil, damage.left, damage.bottom, damage.right, damage.top);
}

void Canvas::damage_all() {
    CanvasRep& c = *rep();
    CanvasDamage& damage = c.damage_;
    damage.left = 0;
    damage.bottom = 0;
    damage.right = c.width_;
    damage.top = c.height_;
    c.new_damage();
}

boolean Canvas::any_damage() const { return rep()->damaged_; }

void Canvas::restrict_damage(const Extension& ext) {
    restrict_damage(ext.left(), ext.bottom(), ext.right(), ext.top());
}

void Canvas::restrict_damage(
    Coord left, Coord bottom, Coord right, Coord top
) {
    CanvasRep& c = *rep();
    c.clear_damage();
    damage(left, bottom, right, top);
    if (c.repairing_) {
	c.start_repair();
    }
}

/*
 * Force a portion of the canvas to be redrawn.  This is typically caused
 * by an X expose event.  If there is no damage and we have a buffer
 * with a copy of the canvas, then we can just copy it to the draw buffer.
 * Otherwise, we just damage the area.
 */

void Canvas::redraw(Coord left, Coord bottom, Coord right, Coord top) {
    CanvasRep& c = *rep();
    if (!c.damaged_ && c.copybuffer_ != CanvasRep::unbound) {
	Display& d = *c.display_;
	int ptop = d.to_pixels(top);
	int x = d.to_pixels(left);
	int y = c.pheight_ - ptop;
	unsigned int w = d.to_pixels(right) - x;
	unsigned int h = ptop - d.to_pixels(bottom);
	XCopyArea(
	    c.dpy(), c.drawbuffer_, c.copybuffer_, c.copygc_,
	    x, y, w, h, x, y
	);
    } else {
	damage(left, bottom, right, top);
    }
}

void Canvas::repair() {
    CanvasRep& c = *rep();
    c.clear_damage();
}

/* class CanvasRep */

/*
 * We must (re)bind the drawbuffer to the canvas' drawable whenever
 * it changes size or is set to a new xid.  This allows us
 * to (re)allocate a backbuffer pixmap when double-buffering.
 * We set the canvas' xdrawable to the backbuffer.
 */

void CanvasRep::bind(boolean double_buffered) {
    CanvasRep& c = *this;
    XDisplay* dpy = c.display_->rep()->display_;
    XGCValues gcv;
    gcv.graphics_exposures = False;
    if (double_buffered) {
	c.drawbuffer_ = XCreatePixmap(
	    dpy, c.xdrawable_, c.pwidth_, c.pheight_,
	    c.window_->rep()->visual_->depth()
	);
	c.copybuffer_ = c.xdrawable_;
	c.copygc_ = XCreateGC(dpy, c.copybuffer_, GCGraphicsExposures, &gcv);
	c.xdrawable_ = c.drawbuffer_;
    } else {
	c.drawbuffer_ = c.xdrawable_;
	c.copybuffer_ = CanvasRep::unbound;
    }
    c.drawgc_ = XCreateGC(dpy, c.drawbuffer_, GCGraphicsExposures, &gcv);
}

/*
 * If double-buffering, free the backbuffer and set the canvas' xdrawable
 * back to the frontbuffer.
 */

void CanvasRep::unbind() {
    CanvasRep& c = *this;
    if (c.display_ != nil) {
	XDisplay* dpy = c.dpy();
	if (c.copybuffer_ != CanvasRep::unbound) {
	    XFreePixmap(dpy, c.drawbuffer_);
	    c.xdrawable_ = c.copybuffer_;
	    c.copybuffer_ = CanvasRep::unbound;
	    if (c.copygc_ != nil) {
		XFreeGC(dpy, c.copygc_);
		c.copygc_ = nil;
	    }
	}
	if (c.drawgc_ != nil) {
	    XFreeGC(dpy, c.drawgc_);
	    c.drawgc_ = nil;
	}
    }
    c.drawbuffer_ = CanvasRep::unbound;
    Resource::unref(c.brush_);
    Resource::unref(c.color_);
    Resource::unref(c.font_);
    c.brush_ = nil;
    c.color_ = nil;
    c.font_ = nil;
}

static inline void restrict(int& c, int a, int b) {
    if (c < a) {
	c = a;
    } else if (c > b) {
	c = b;
    }
}

boolean CanvasRep::start_repair() {
    CanvasRep& c = *this;
    if (!c.damaged_) {
	return false;
    }

    XRectangle& clip = c.clip_;
    CanvasDamage& damage = c.damage_;
    Display& d = *c.display_;
    int d_left = d.to_pixels(damage.left);
    int d_bottom = d.to_pixels(damage.bottom);
    int d_right = d.to_pixels(damage.right);
    int d_top = d.to_pixels(damage.top);
    restrict(d_left, 0, pwidth_);
    restrict(d_bottom, 0, pheight_);
    restrict(d_right, 0, pwidth_);
    restrict(d_top, 0, pheight_);
    clip.x = d_left;
    clip.y = c.pheight_ - d_top;
    clip.width = d_right - d_left;
    clip.height = d_top - d_bottom;
    XUnionRectWithRegion(&clip, c.empty_, c.clipping_);
    XSetClipRectangles(dpy(), c.drawgc_, 0, 0, &clip, 1, YXBanded);
    c.repairing_ = true;
    return true;
}

void CanvasRep::finish_repair() {
    CanvasRep& c = *this;
    c.flush();
    c.swapbuffers();
    c.damaged_ = false;
    c.on_damage_list_ = false;
    c.repairing_ = false;
}

void CanvasRep::flush() {
    TextRenderInfo* t = &CanvasRep::text_;
    if (t == nil) {
	return;
    }
    unsigned int nchars = t->cur_text_ - t->text_;
    if (nchars != 0) {
	CanvasRep& c = *this;
	XDisplay* dpy = c.dpy();
	XDrawable d = t->canvas_->drawbuffer_;
	GC gc = t->drawgc_;
        if (t->spaces_ == 0 || c.text_twobyte_) {
            if (c.text_twobyte_) {
                XDrawString16(
                    dpy, d, gc, t->x0_, t->y0_, (XChar2b*)t->text_, nchars/2
                );
            } else {
                XDrawString(dpy, d, gc, t->x0_, t->y0_, t->text_, nchars);
            }
        } else {
            int width = XTextWidth(c.xfont_, t->text_, nchars);
            int delta = c.display_->to_pixels(t->width_) - width;
            int item = 0;
            int count = 0;
            t->items_[item].chars = t->text_;
            t->items_[item].delta = 0;
            t->items_[item].font = None;
	    char* text_end = t->cur_text_;
	    for (char* cp = t->text_; cp < text_end; cp++) {
                if (*cp == ' ') {
                    int d = delta / (t->spaces_ - item);
                    delta -= d;
                    t->items_[item].nchars = count;
                    ++item;
                    t->items_[item].chars = cp;
                    t->items_[item].delta = d;
                    t->items_[item].font = None;
                    count = 0;
                }
                ++count;
            }
            t->items_[item].nchars = count;
            XDrawText(
                dpy, d, gc, t->x0_, t->y0_, t->items_, item + 1
            );
        }
        t->cur_text_ = t->text_;
        t->spaces_ = 0;
    }
}

void CanvasRep::swapbuffers() {
    CanvasRep& c = *this;
    if (c.copybuffer_ == CanvasRep::unbound) {
	/* not double-buffering */
	return;
    }
    XRectangle& clip = c.clip_;
    XCopyArea(
	c.dpy(), c.drawbuffer_, c.copybuffer_, c.copygc_,
	clip.x, clip.y, clip.width, clip.height, clip.x, clip.y
    );
}

XDisplay* CanvasRep::dpy() const { return display_->rep()->display_; }

Transformer& CanvasRep::matrix() const {
    TransformerStack& s = *transformers_;
    return *(s.item(s.count() - 1));
}

void CanvasRep::color(const Color* color) {
    CanvasRep& c = *this;
    if (color != nil && color != c.color_) {
	Resource::ref(color);
	Resource::unref(c.color_);
	c.color_ = color;
	XDisplay* dpy = c.dpy();
	WindowVisual* wv = c.window_->rep()->visual_;
	GC gc = c.drawgc_;
	ColorRep& cr = *color->rep(wv);
        c.pixel_ = cr.xcolor_.pixel;
        c.op_ = cr.op_;
        c.stipple_ = cr.stipple_;
	if (cr.masking_) {
            XSetForeground(dpy, gc, 1);
	} else if (c.op_ == GXxor) {
            XSetForeground(dpy, gc, wv->xor(c.window_->style()));
        } else {
            XSetForeground(dpy, gc, c.pixel_);
        }
        XSetFunction(dpy, gc, c.op_);
        if (c.stipple_ != 0) {
            XSetStipple(dpy, gc, c.stipple_);
            XSetFillStyle(dpy, gc, FillStippled);
        } else {
            XSetFillStyle(dpy, gc, FillSolid);
        }
    }
}

void CanvasRep::brush(const Brush* b) {
    CanvasRep* c = this;
    if (b != nil && b != c->brush_) {
	Resource::ref(b);
	Resource::unref(c->brush_);
	c->brush_ = b;
	XDisplay* dpy = c->dpy();
	GC gc = c->drawgc_;
	BrushRep* br = b->rep(c->display_);
        c->brush_width_ = br->width_;
        c->dash_list_ = br->dash_list_;
        c->dash_count_ = br->dash_count_;
        if (c->dash_list_ == nil) {
            XSetLineAttributes(
                dpy, gc, c->brush_width_, LineSolid, CapButt, JoinMiter
            );
        } else {
            XSetLineAttributes(
                dpy, gc, c->brush_width_, LineOnOffDash, CapButt, JoinMiter
            );
            XSetDashes(dpy, gc, 0, c->dash_list_, c->dash_count_);
        }
    }
}

void CanvasRep::font(const Font* f) {
    CanvasRep& c = *this;
    if (f != nil && f != c.font_) {
	Resource::ref(f);
	Resource::unref(c.font_);
	FontRep* fr = f->rep(c.display_);
	c.font_ = f;
	XFontStruct* xf = fr->font_;
        c.xfont_ = xf;
        c.text_twobyte_ = xf->min_byte1 > 0 || xf->max_byte1 > 0;
	const char* coding = f->Font::encoding();
	c.text_reencode_ = coding != nil && strcmp(coding, "ISO8859") == 0;
	if (fr->unscaled_) {
	    c.font_is_scaled_ = false;
	} else {
	    c.font_is_scaled_ = true;
	    float tolerance = 0.15;
	    Window* w = c.window_;
	    if (w != nil) {
		Style* s = w->style();
		if (s != nil) {
		    s->find_attribute("fontScaleTolerance", tolerance);
		}
	    }
	    c.font_is_scaled_ = (
		fr->scale_ < (1 - tolerance) || fr->scale_ > (1 + tolerance)
	    );
	}
        XSetFont(c.dpy(), c.drawgc_, xf->fid);
    }
}

void CanvasRep::new_damage() {
    Window* w = window_;
    damaged_ = true;
    if (!on_damage_list_ && w != nil && w->bound()) {
	on_damage_list_ = true;
	display_->rep()->needs_repair(w);
    }
}

void CanvasRep::clear_damage() {
    damaged_ = false;
    on_damage_list_ = false;
}

/*
 * These are at the end of the file to workaround a cfront 2.0 problem.
 */

TextRenderInfo CanvasRep::text_;
PathRenderInfo CanvasRep::path_;

/* Canvas anachronisms */
unsigned int Canvas::Width() const { return pwidth(); }
unsigned int Canvas::Height() const { return pheight(); }
CanvasLocation Canvas::status() const { return rep()->status_; }

#include <IV-X11/xwindow.h>

void Canvas::SetBackground(const Color* c) {
    Window* ww = window();
    if (ww != nil) {
	WindowRep& w = *ww->rep();
	XSetWindowBackground(
	    w.display_->rep()->display_, w.xwindow_,
	    c->rep(w.visual_)->xcolor_.pixel
	);
    }
}
