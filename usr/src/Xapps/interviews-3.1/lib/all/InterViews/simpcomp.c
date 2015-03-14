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

#include <InterViews/glyph.h>
#include <InterViews/simplecomp.h>
#include <OS/math.h>

SimpleCompositor::SimpleCompositor() : Compositor() { }

SimpleCompositor::~SimpleCompositor() { }

CompositorIndex SimpleCompositor::compose(
    Coord* natural, Coord*, Coord*,
    int* penalties, CompositorIndex component_count,
    Coord* spans, CompositorIndex span_count,
    CompositorIndex* breaks, CompositorIndex break_count
) {
    Coord span = 0;
    boolean breakable = false;
    CompositorIndex count = 0;
    int penalty;
    for (CompositorIndex i = 0; i < component_count; ++i) {
        if (i == component_count - 1) {
            penalty = PenaltyGood;
            span = fil;
        } else {
            penalty = penalties[i];
            span += natural[i];
        }
        if (penalty < PenaltyBad) {
            breakable = true;
        }
        if (span > spans[Math::min(count, span_count-1)]) {
            while (breakable && penalty == PenaltyBad) {
                --i;
                penalty = penalties[i];
            }
            if (count < break_count) {
                breaks[count] = i;
            }
            span = 0;
            breakable = false;
            ++count;
        }
    }
    return Math::min(count, break_count);
}
