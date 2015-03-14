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

#include <InterViews/arraycomp.h>
#include <OS/math.h>

ArrayCompositor::ArrayCompositor(CompositorIndex count) : Compositor() {
    count_ = count;
}

ArrayCompositor::~ArrayCompositor() { }

CompositorIndex ArrayCompositor::compose(
    Coord*, Coord*, Coord*,
    int*, CompositorIndex component_count,
    Coord*, CompositorIndex,
    CompositorIndex* breaks, CompositorIndex break_count
) {
    CompositorIndex count = Math::min(
	(component_count - 1) / count_ + 1, break_count
    );
    for (CompositorIndex i = 0; i < count; ++i) {
        breaks[i] = Math::min((i+1) * count_ - 1, component_count-1);
    }
    return count;
}
