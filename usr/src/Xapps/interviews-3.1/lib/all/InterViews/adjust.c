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
 * Adjustable - object to scroll and/or zoom
 */

#include <InterViews/adjust.h>
#include <InterViews/observe.h>
#include <OS/math.h>

struct AdjustableInfo {
private:
    friend class Adjustable;

    Observable* observable_;
    Coord small_;
    Coord large_;
};

struct AdjustableImpl {
private:
    friend class Adjustable;

    AdjustableInfo info_[CoordinateSpace::dimensions];
};

Adjustable::Adjustable() {
    impl_ = new AdjustableImpl;
    AdjustableImpl& a = *impl_;
    for (DimensionName d = 0; d < CoordinateSpace::dimensions; d++) {
	AdjustableInfo& i = a.info_[d];
	i.observable_ = new Observable;
	i.small_ = 1;
	i.large_ = 0;
    }
}

Adjustable::~Adjustable() {
    AdjustableImpl& a = *impl_;
    for (DimensionName d = 0; d < CoordinateSpace::dimensions; d++) {
	delete a.info_[d].observable_;
    }
    delete impl_;
}

Observable* Adjustable::observable(DimensionName d) const {
    return impl_->info_[d].observable_;
}

void Adjustable::attach(DimensionName d, Observer* o) {
    observable(d)->attach(o);
}

void Adjustable::detach(DimensionName d, Observer* o) {
    observable(d)->detach(o);
}

Coord Adjustable::lower(DimensionName) const { return 0; }
Coord Adjustable::upper(DimensionName d) const { return lower(d) + length(d); }
Coord Adjustable::length(DimensionName d) const { return upper(d) - lower(d); }
Coord Adjustable::cur_lower(DimensionName) const { return 0; }

Coord Adjustable::cur_upper(DimensionName d) const {
    return cur_lower(d) + cur_length(d);
}

Coord Adjustable::cur_length(DimensionName d) const {
    return cur_upper(d) - cur_lower(d);
}

void Adjustable::small_scroll(DimensionName d, Coord c) {
    impl_->info_[d].small_ = c;
}

Coord Adjustable::small_scroll(DimensionName d) const {
    return impl_->info_[d].small_;
}

void Adjustable::large_scroll(DimensionName d, Coord c) {
    impl_->info_[d].large_ = c;
}

Coord Adjustable::large_scroll(DimensionName d) const {
    Coord s = impl_->info_[d].large_;
    if (Math::equal(s, float(0), float(1e-4))) {
	s = cur_length(d) - 1;
    }
    return s;
}

void Adjustable::begin_adjustment(DimensionName) { }
void Adjustable::commit_adjustment(DimensionName) { }
void Adjustable::abort_adjustment(DimensionName) { }

void Adjustable::scroll_forward(DimensionName d) {
    scroll_to(d, cur_lower(d) + small_scroll(d));
}

void Adjustable::scroll_backward(DimensionName d) {
    scroll_to(d, cur_lower(d) - small_scroll(d));
}

void Adjustable::page_forward(DimensionName d) {
    scroll_to(d, cur_lower(d) + large_scroll(d));
}

void Adjustable::page_backward(DimensionName d) {
    scroll_to(d, cur_lower(d) - large_scroll(d));
}

void Adjustable::scroll_to(DimensionName, Coord) { }
void Adjustable::scale_to(DimensionName, Coord) { }
void Adjustable::zoom_to(float) { }

void Adjustable::constrain(DimensionName d, Coord& new_lower) const {
    Coord a = lower(d);
    Coord b = upper(d) - cur_length(d);
    if (new_lower < a) {
	new_lower = a;
    } else if (new_lower > b) {
	new_lower = b;
    }
}

void Adjustable::notify(DimensionName d) const {
    observable(d)->notify();
}

void Adjustable::notify_all() const {
    for (DimensionName d = 0; d < CoordinateSpace::dimensions; d++) {
	observable(d)->notify();
    }
}
