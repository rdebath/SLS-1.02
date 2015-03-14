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

#include <InterViews/brush.h>
#include <InterViews/display.h>
#include <InterViews/session.h>
#include <IV-X11/xbrush.h>
#include <OS/list.h>

declarePtrList(BrushRepList,BrushRep)
implementPtrList(BrushRepList,BrushRep)

class BrushImpl {
private:
    friend class Brush;

    Coord width;
    char* dash_list;
    int dash_count;
    BrushRepList replist;
};

Brush::Brush(Coord w) { init(nil, 0, w); }
Brush::Brush(const int* p, int c, Coord w) { init(p, c, w); }

Brush::Brush(int pat, Coord w) {
    int dash[16];
    int count;

    calc_dashes(pat, dash, count);
    init(dash, count, w);
}

Brush::~Brush() {
    BrushRepList& list = impl_->replist;
    for (ListItr(BrushRepList) i(list); i.more(); i.next()) {
	BrushRep* r = i.cur();
	delete r;
    }
    delete impl_->dash_list;
    delete impl_;
}

void Brush::calc_dashes(int pat, int* dash, int& count) {
    unsigned int p = pat & 0xffff;

    if (p == 0 || p == 0xffff) {
        count = 0;
    } else {
	const unsigned int MSB = 1 << 15;
	while ((p & MSB) == 0) {
	    p <<= 1;
	}

	if (p == 0x5555 || p == 0xaaaa) {
	    dash[0] = 1;
	    dash[1] = 3;
	    count = 2;
	} else if (p == 0xaaaa) {
	    dash[0] = 1;
	    dash[1] = 1;
	    count = 2;
	} else if (p == 0xcccc) {
	    dash[0] = 2;
	    dash[1] = 2;
	    count = 2;
	} else if (p == 0xeeee) {
	    dash[0] = 3;
	    dash[1] = 1;
	    count = 2;
	} else {
	    unsigned int m = MSB;
	    int index = 0;
	    while (m != 0) {
		/* count the consecutive one bits */
		int length = 0;
		while (m != 0 && (p & m) != 0) {
		    ++length;
		    m >>= 1;
		}
		dash[index++] = length;

		/* count the consecutive zero bits */
		length = 0;	
		while (m != 0 && (p & m) == 0) {
		    ++length;
		    m >>= 1;
		}
		if (length > 0) {
		    dash[index++] = length;
		}
	    }
	    count = index;
	}
    }
}

void Brush::init(const int* pattern, int count, Coord w) {
    BrushImpl* b = new BrushImpl;
    impl_ = b;
    b->width = w;
    b->dash_count = count;
    if (count > 0) {
        b->dash_list = new char[count];
        for (int i = 0; i < count; ++i) {
            b->dash_list[i] = char(pattern[i]);
        }
    } else {
        b->dash_list = nil;
    }
}

BrushRep* Brush::rep(Display* d) const {
    BrushRep* r;
    BrushRepList& list = impl_->replist;
    for (ListItr(BrushRepList) i(list); i.more(); i.next()) {
	r = i.cur();
	if (r->display_ == d) {
	    return r;
	}
    }
    r = new BrushRep;
    r->display_ = d;
    r->dash_list_ = impl_->dash_list;
    r->dash_count_ = impl_->dash_count;
    r->width_ = d->to_pixels(impl_->width);
    list.append(r);
    return r;
}

Coord Brush::width() const { return impl_->width; }

/* anachronism */
unsigned int Brush::Width() const {
    return rep(Session::instance()->default_display())->width_;
}
