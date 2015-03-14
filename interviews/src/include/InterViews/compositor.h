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
 * Compositor - find breaks
 */

#ifndef iv_compositor_h
#define iv_compositor_h

#include <InterViews/enter-scope.h>
#include <InterViews/coord.h>

#include <InterViews/_enter.h>

static const int PenaltyBad = 10000;
static const int PenaltyGood = -10000;

typedef long CompositorIndex;

class Compositor {
public:
    Compositor();
    virtual ~Compositor();

    virtual CompositorIndex compose(
        Coord* natural, Coord* stretch, Coord* shrink,
        int* penalites, CompositorIndex component_count,
        Coord* spans, CompositorIndex span_count,
        CompositorIndex* breaks, CompositorIndex break_count
    );
};

#include <InterViews/_leave.h>

#endif
