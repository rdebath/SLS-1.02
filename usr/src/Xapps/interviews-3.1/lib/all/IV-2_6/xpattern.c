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

#include <InterViews/display.h>
#include <InterViews/pattern.h>
#include <InterViews/session.h>
#include <IV-X11/Xlib.h>
#include <IV-X11/xdisplay.h>
#include <IV-X11/xpattern.h>

Pattern::Pattern() {
    init(nil, 0, 0);
}

Pattern::Pattern(const char* pattern, unsigned int w, unsigned int h) {
    init(pattern, w, h);
}

Pattern::Pattern(int pattern) {
    char p[4];

    p[0] = (pattern & 0xf000) >> 12;
    p[1] = (pattern & 0x0f00) >> 8;
    p[2] = (pattern & 0x00f0) >> 4;
    p[3] = (pattern & 0x000f);
    init(p, 4, 4);
}

Pattern::Pattern(const int* pattern) {
    char p[32];

    register char* pp = p;
    for (register int i = 0; i < 16; i++) {
	int scanline = pattern[i];
	*pp++ = (scanline & 0xff00) >> 8;
	*pp++ = scanline & 0x00ff;
    }
    init(p, 16, 16);
}

static boolean is_solid(
    unsigned char* pat, unsigned int width, unsigned int height
) {
    unsigned int nbits = width * height;
    unsigned int n_whole_bytes = nbits >> 3;
    unsigned char* last_byte = &pat[n_whole_bytes];
    for (register unsigned char* cp = pat; cp < last_byte; cp++) {
	if (*cp != 0xff) {
	    return false;
	}
    }
    nbits -= n_whole_bytes << 3;
    if (nbits == 0) {
	return true;
    }
    unsigned int mask = (1 << nbits) - 1;
    return (*last_byte & mask) == mask;
}

void Pattern::init(
    const char* pattern, unsigned int width, unsigned int height
) {
    rep_ = new PatternRep;
    rep_->display_ = Session::instance()->default_display();
    if (pattern != nil && !is_solid((unsigned char*)pattern, width, height)) {
	DisplayRep* r = rep_->display_->rep();
        rep_->pixmap_ = XCreateBitmapFromData(
            r->display_, r->root_, pattern, width, height
        );
    } else {
        rep_->pixmap_ = 0;
    }
}

Pattern::~Pattern() {
    PatternRep* p = rep_;
    if (p->pixmap_ != 0) {
	XFreePixmap(p->display_->rep()->display_, p->pixmap_);
    }
    delete p;
}
