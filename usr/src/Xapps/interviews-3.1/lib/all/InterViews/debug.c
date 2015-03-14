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
 * DebugGlyph -- trace glyph calls
 */

#include <InterViews/canvas.h>
#include <InterViews/debug.h>
#include <InterViews/hit.h>
#include <stdio.h>

DebugGlyph::DebugGlyph(
    Glyph* g, const char* msg, unsigned int flags
) : MonoGlyph(g) {
    msg_ = msg;
    flags_ = flags;
}

DebugGlyph::~DebugGlyph() { }

void DebugGlyph::request(Requisition& r) const {
    MonoGlyph::request(r);
    if ((flags_ & trace_request) != 0) {
	heading("request ");
	print_requirement(r.requirement(Dimension_X));
	printf(", ");
	print_requirement(r.requirement(Dimension_Y));
	printf("\n");
    }
}

void DebugGlyph::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    if ((flags_ & trace_allocate) != 0) {
	heading("allocate ");
	print_allotment(a.allotment(Dimension_X));
	printf(", ");
	print_allotment(a.allotment(Dimension_Y));
	printf("\n");
    }
    MonoGlyph::allocate(c, a, ext);
}

void DebugGlyph::draw(Canvas* c, const Allocation& a) const {
    if ((flags_ & trace_draw) != 0) {
	heading("draw ");
	print_allotment(a.allotment(Dimension_X));
	printf(", ");
	print_allotment(a.allotment(Dimension_Y));
	if (c != nil) {
	    Extension e;
	    c->damage_area(e);
	    printf(
		" [%.2f,%.2f %.2f,%.2f]",
		e.left(), e.bottom(), e.right(), e.top()
	    );
	}
	printf("\n");
    }
    MonoGlyph::draw(c, a);
}

void DebugGlyph::print(Printer* p, const Allocation& a) const {
    if ((flags_ & trace_print) != 0) {
	heading("print ");
	print_allotment(a.allotment(Dimension_X));
	printf(", ");
	print_allotment(a.allotment(Dimension_Y));
	printf("\n");
    }
    MonoGlyph::print(p, a);
}

void DebugGlyph::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    if ((flags_ & trace_pick) != 0) {
	heading("pick ");
	printf(
	    "at (%.2f,%.2f,%.2f,%.2f) ",
	    h.left(), h.bottom(), h.right(), h.top()
	);
	print_allotment(a.allotment(Dimension_X));
	printf(", ");
	print_allotment(a.allotment(Dimension_Y));
	printf("\n");
    }
    MonoGlyph::pick(c, a, depth, h);
}

void DebugGlyph::undraw() {
    if ((flags_ & trace_undraw) != 0) {
	heading("undraw\n");
    }
}

void DebugGlyph::print_requirement(const Requirement& r) {
    printf(
	"%.2f(+%.2f,-%.2f) @ %.2f",
	r.natural(), r.stretch(), r.shrink(), r.alignment()
    );
}

void DebugGlyph::print_allotment(const Allotment& a) {
    printf(
	"%.2f,%.2f @ %.2f",
	a.begin(), a.end(), a.alignment()
    );
}

void DebugGlyph::heading(const char* s) const {
    printf("%s(0x%x) %s", msg_, body(), s);
}
