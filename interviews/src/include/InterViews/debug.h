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

#ifndef iv_debug_h
#define iv_debug_h

#include <InterViews/monoglyph.h>

#include <InterViews/_enter.h>

class DebugGlyph : public MonoGlyph {
public:
    enum {
	trace_none = 0x0,
	trace_request = 0x1,
	trace_allocate = 0x2,
	trace_draw = 0x4,
	trace_print = 0x8,
	trace_pick = 0x10,
	trace_undraw = 0x20,
	/* convenient shorthand */
	trace_request_allocate = 0x3,
	trace_request_draw = 0x5,
	trace_allocate_draw = 0x6,
	trace_request_allocate_draw = 0x7,
	trace_request_pick = 0x11,
	trace_allocate_pick = 0x12,
	trace_request_allocate_pick = 0x13,
	trace_draw_pick = 0x14,
	trace_request_draw_pick = 0x15,
	trace_request_undraw = 0x21,
	trace_allocate_undraw = 0x22,
	trace_request_allocate_undraw = 0x23,
	trace_draw_undraw = 0x24,
	trace_request_draw_undraw = 0x25,
	trace_allocate_draw_undraw = 0x26,
	trace_request_allocate_draw_undraw = 0x27,
	trace_pick_undraw = 0x30,
	trace_request_pick_undraw = 0x31,
	trace_allocate_pick_undraw = 0x32,
	trace_request_allocate_pick_undraw = 0x33,
	trace_draw_pick_undraw = 0x34,
	trace_request_draw_pick_undraw = 0x35,
	trace_allocate_draw_pick_undraw = 0x36,
	trace_request_allocate_draw_pick_undraw = 0x37,
	trace_all = 0x3f
    };

    DebugGlyph(Glyph* g, const char* msg, unsigned int trace_flags);
    virtual ~DebugGlyph();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void print(Printer*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);
    virtual void undraw();

    static void print_requirement(const Requirement&);
    static void print_allotment(const Allotment&);
private:
    const char* msg_;
    unsigned int flags_;

    void heading(const char*) const;
};

#include <InterViews/_leave.h>

#endif
