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

#include <InterViews/aggr.h>
#include <InterViews/align.h>
#include <InterViews/group.h>

Group::Group(Aggregate* aggregate, DimensionName dimension) {
    aggregate_ = aggregate;
    if (aggregate_ != nil) {
        aggregate_->ref();
    }
    dimension_ = dimension;
    layout_ = new Align(dimension);
    map_ = new GlyphIndex[100];
    count_ = 0;
}

Group::~Group() {
    if (aggregate_ != nil) {
        aggregate_->unref();
    }
    delete layout_;
}

void Group::map(GlyphIndex index) {
    map_[count_] = index;
    ++count_;
}

void Group::request(Requisition& requisition) const {
    if (aggregate_ != nil) {
        Requisition* r = new Requisition[count_];
        for (int i = 0; i < count_; ++i) {
            Glyph* g = aggregate_->component(map_[i]);
            if (g != nil) {
                g->request(r[i]);
            }
        }
        layout_->request(count_, r, requisition);
        delete r;
    }
}

void Group::allocate(Canvas*, const Allocation& allocation, Extension&) {
    if (aggregate_ != nil) {
        Requisition* requisitions = new Requisition[count_];
        int i;
        for (i = 0; i < count_; ++i) {
            Glyph* g = aggregate_->component(map_[i]);
            if (g != nil) {
                g->request(requisitions[i]);
            }
        }
        Allocation* allocations = new Allocation[count_];
        layout_->allocate(allocation, count_, requisitions, allocations);
        for (i = 0; i < count_; ++i) {
            aggregate_->allot(
                map_[i], dimension_, allocations[i].allotment(dimension_)
            );
        }
        delete allocations;
        delete requisitions;
    }
}
