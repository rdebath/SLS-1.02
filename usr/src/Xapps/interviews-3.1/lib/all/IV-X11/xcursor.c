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
 * Input cursors.
 */

#include <InterViews/bitmap.h>
#include <InterViews/color.h>
#include <InterViews/cursor.h>
#include <InterViews/display.h>
#include <InterViews/font.h>
#include <InterViews/style.h>
#include <IV-X11/Xlib.h>
#include <IV-X11/xbitmap.h>
#include <IV-X11/xcolor.h>
#include <IV-X11/xcursor.h>
#include <IV-X11/xdisplay.h>
#include <IV-X11/xfont.h>
#include <IV-X11/xwindow.h>
#include <OS/string.h>
#include <X11/cursorfont.h>

static const CursorPattern textPat = {
    0x0000, 0x4400, 0x2800, 0x1000, 0x1000, 0x1000, 0x1000, 0x1000, 
    0x1000, 0x1000, 0x1000, 0x1000, 0x1000, 0x2800, 0x4400, 0x0000
};

static const CursorPattern textMask = {
    0x0000, 0xCC00, 0x7800, 0x3000, 0x3000, 0x3000, 0x3000, 0x3000, 
    0x3000, 0x3000, 0x3000, 0x3000, 0x3000, 0x7800, 0xCC00, 0x0000, 
};

static const CursorPattern noPat = {
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0
};

Cursor* defaultCursor;
Cursor* arrow;
Cursor* crosshairs;
Cursor* ltextCursor;
Cursor* rtextCursor;
Cursor* hourglass;
Cursor* upperleft;
Cursor* upperright;
Cursor* lowerleft;
Cursor* lowerright;
Cursor* noCursor;

/*
 * Define the builtin cursors.
 */

void Cursor::init() {
    arrow = new Cursor(XC_left_ptr);
    crosshairs = new Cursor(XC_crosshair);
    ltextCursor = new Cursor(4, 8, textPat, textMask);
    rtextCursor = new Cursor(0, 8, textPat, textMask);
    hourglass = new Cursor(XC_watch);
    upperleft = new Cursor(XC_ul_angle);
    upperright = new Cursor(XC_ur_angle);
    lowerleft = new Cursor(XC_ll_angle);
    lowerright = new Cursor(XC_lr_angle);
    noCursor = new Cursor(0, 0, noPat, noPat);
    defaultCursor = arrow;
}

/*
 * Create a cursor a specific pattern and mask.
 */

Cursor::Cursor(
    short xoff, short yoff, const int* p, const int* m,
    const Color* fg, const Color * bg
) {
    rep_ = new CursorRepData(xoff, yoff, p, m, fg, bg);
}

/*
 * Create a cursor from bitmaps.
 */

Cursor::Cursor(
    const Bitmap* pat, const Bitmap* mask, const Color* fg, const Color* bg
) {
    rep_ = new CursorRepBitmap(pat, mask, fg, bg);
}

/*
 * Create a cursor from a font.
 */

Cursor::Cursor(
    const Font* font, int pat, int mask, const Color* fg, const Color* bg
) {
    rep_ = new CursorRepFont(font, pat, mask, fg, bg);
}

/*
 * Create a cursor from the predefined cursor font.
 */

Cursor::Cursor(int n, const Color* fg, const Color* bg) {
    rep_ = new CursorRepXFont(n, fg, bg);
}

Cursor::~Cursor() {
    delete rep_;
}

/* class CursorRep */

CursorRep::CursorRep(const Color* fg, const Color* bg) {
    Resource::ref(fg);
    fg_ = fg;
    Resource::ref(bg);
    bg_ = bg;
    display_ = nil;
    xcursor_ = 0;
}

CursorRep::~CursorRep() {
    if (xcursor_ != 0) {
	XFreeCursor(display_->rep()->display_, xcursor_);
    }
    Resource::unref(fg_);
    Resource::unref(bg_);
}

XCursor CursorRep::xid(Display* d, WindowVisual* wv) {
    if (display_ != d) {
	if (xcursor_ != 0) {
	    XFreeCursor(display_->rep()->display_, xcursor_);
	}
	Style* s = d->style();
	if (fg_ == nil) {
	    fg_ = make_color(
		d, s, "pointerColor", "foreground", "Foreground",
		"#000000"
	    );
	}
	if (bg_ == nil) {
	    bg_ = make_color(
		d, s, "pointerColorBackground", "background", "Background",
		"#ffffff"
	    );
	}
	make_xcursor(d, wv);
	display_ = d;
    }
    return xcursor_;
}

const Color* CursorRep::make_color(
    Display* d, Style* s,
    const char* str1, const char* str2, const char* str3,
    const char* default_value
) {
    const Color* c = nil;
    String v;
    if (s->find_attribute(str1, v) || s->find_attribute(str2, v) ||
	s->find_attribute(str3, v)
    ) {
	c = Color::lookup(d, v);
    }
    if (c == nil) {
	c = Color::lookup(d, default_value);
    }
    Resource::ref(c);
    return c;
}

/* class CursorRepData */

CursorRepData::CursorRepData(
    short x_hot, short y_hot, const int* pat, const int* mask,
    const Color* fg, const Color* bg
) : CursorRep(fg, bg) {
    x_ = x_hot;
    y_ = y_hot;
    pat_ = pat;
    mask_ = mask;
}

CursorRepData::~CursorRepData() { }

void CursorRepData::make_xcursor(Display* d, WindowVisual* wv) {
    XDisplay* dpy = d->rep()->display_;
    XWindow root = d->rep()->root_;
    Pixmap p = make_cursor_pixmap(dpy, root, pat_);
    Pixmap m = make_cursor_pixmap(dpy, root, mask_);
    xcursor_ = XCreatePixmapCursor(
	dpy, p, m, &fg_->rep(wv)->xcolor_, &bg_->rep(wv)->xcolor_,
	x_, cursorHeight - 1 - y_
    );
    XFreePixmap(dpy, p);
    XFreePixmap(dpy, m);
}

/*
 * Create the pixmap for a cursor.  These are always 16x16, unlike
 * fill patterns, which are 32x32.
 */

Pixmap CursorRepData::make_cursor_pixmap(
    XDisplay* dpy, XWindow root, const int* scanline
) {
    Pixmap dst = XCreatePixmap(dpy, root, cursorWidth, cursorHeight, 1);
    GC g = XCreateGC(dpy, dst, 0, nil);
    XSetForeground(dpy, g, 0);
    XSetFillStyle(dpy, g, FillSolid);
    XFillRectangle(dpy, dst, g, 0, 0, cursorWidth, cursorHeight);
    XSetForeground(dpy, g, 1);

    register int i, j;
    register unsigned s1, s2;
    for (i = 0; i < cursorHeight; i++) {
	s1 = scanline[i];
	s2 = 1;
	for (j = 0; j < cursorWidth; j++) {
	    if ((s1 & s2) != 0) {
		XDrawPoint(dpy, dst, g, cursorWidth - 1 - j, i);
	    }
	    s2 <<= 1;
	}
    }
    XFreeGC(dpy, g);
    return dst;
}

/* class CursorRepBitmap */

CursorRepBitmap::CursorRepBitmap(
    const Bitmap* pat, const Bitmap* mask, const Color* fg, const Color* bg
) : CursorRep(fg, bg) {
    Resource::ref(pat);
    pat_ = pat;
    Resource::ref(mask);
    mask_ = mask;
}

CursorRepBitmap::~CursorRepBitmap() {
    Resource::unref(pat_);
    Resource::unref(mask_);
}

void CursorRepBitmap::make_xcursor(Display* d, WindowVisual* wv) {
    BitmapRep* b = pat_->rep();
    xcursor_ = XCreatePixmapCursor(
	d->rep()->display_,
	b->pixmap_, mask_->rep()->pixmap_,
	&fg_->rep(wv)->xcolor_, &bg_->rep(wv)->xcolor_,
	d->to_pixels(-b->left_), d->to_pixels(b->height_ - 1 + b->bottom_)
    );
}

/* class CursorRepFont */

CursorRepFont::CursorRepFont(
    const Font* font, int pat, int mask, const Color* fg, const Color* bg
) : CursorRep(fg, bg) {
    Resource::ref(font);
    font_ = font;
    pat_ = pat;
    mask_ = mask;
}

CursorRepFont::~CursorRepFont() {
    Resource::unref(font_);
}

void CursorRepFont::make_xcursor(Display* d, WindowVisual* wv) {
    XFontStruct* i = font_->rep(d)->font_;
    xcursor_ = XCreateGlyphCursor(
        d->rep()->display_, i->fid, i->fid, pat_, mask_,
	&fg_->rep(wv)->xcolor_, &bg_->rep(wv)->xcolor_
    );
}

/* class CursorRepXFont */

CursorRepXFont::CursorRepXFont(
    int code, const Color* fg, const Color* bg
) : CursorRep(fg, bg) {
    code_ = code;
}

CursorRepXFont::~CursorRepXFont() { }

void CursorRepXFont::make_xcursor(Display* d, WindowVisual* wv) {
    XDisplay* dpy = d->rep()->display_;
    xcursor_ = XCreateFontCursor(dpy, code_);
    XRecolorCursor(
	dpy, xcursor_, &fg_->rep(wv)->xcolor_, &bg_->rep(wv)->xcolor_
    );
}
