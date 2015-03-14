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
#include <InterViews/texcomp.h>
#include <OS/math.h>

static const int TOLERANCE = 100;
static const float BADGSR = 4.5;

class BreakSet {
public:
    BreakSet(BreakSet*);
    ~BreakSet();

    void add_break(CompositorIndex index, int demerits);
    void no_break(Coord natural, Coord stretch, Coord shrink);

    int demerits_;
    Coord natural_;
    Coord stretch_;
    Coord shrink_;
    CompositorIndex* breaks_;
    CompositorIndex size_;
    CompositorIndex count_;
    BreakSet* next_;
    BreakSet* prev_;
};

BreakSet::BreakSet(BreakSet* b) {
    natural_ = 0;
    stretch_ = 0;
    shrink_ = 0;
    if (b == nil) {
        size_ = 20;
        breaks_ = new CompositorIndex[size_];
        demerits_ = 0;
        count_ = 0;
        next_ = this;
        prev_ = this;
    } else {
        demerits_ = b->demerits_;
        size_ = b->size_;
        breaks_ = new CompositorIndex[size_];
        count_ = b->count_;
        for (CompositorIndex i = 0; i < count_; ++i) {
            breaks_[i] = b->breaks_[i];
        }
        next_ = b->next_;
        prev_ = b;
        prev_->next_ = this;
        next_->prev_ = this;
    }
}

BreakSet::~BreakSet() {
    prev_->next_ = next_;
    next_->prev_ = prev_;
    delete breaks_;
}

void BreakSet::add_break(CompositorIndex index, int demerits) {
    if (count_ == size_) {
        CompositorIndex size = size_ + 20;
        CompositorIndex* breaks = new CompositorIndex[size];
        for (CompositorIndex i = 0; i < count_; ++i) {
            breaks[i] = breaks_[i];
        }
        delete breaks_;
        breaks_ = breaks;
        size_ = size;
    }
    breaks_[count_] = index;
    ++count_;
    natural_ = 0;
    stretch_ = 0;
    shrink_ = 0;
    demerits_ += demerits;
}

inline void BreakSet::no_break(Coord natural, Coord stretch, Coord shrink) {
    natural_ += natural;
    stretch_ += stretch;
    shrink_ += shrink;
}

inline int demerits(int badness, int penalty, int linepenalty) {
    int lb = linepenalty + (badness > 0 ? badness : -badness);
    if (penalty > 0) {
        return lb * lb + penalty * penalty;
    } else {
        return lb * lb - penalty * penalty;
    }
}

inline int badness(Coord size, Coord natural, Coord stretch, Coord shrink) {
    int r;
    Coord gsr;
    if (size >= natural) {
        if (stretch == 0) {
            return PenaltyBad;
        } else {
            gsr = (size - natural)/stretch;
            if (gsr > BADGSR) {
                return PenaltyBad;
            } else {
                r = int(100 * gsr * gsr * gsr);
                return Math::min(r, PenaltyBad);
            }
        }
    } else if (shrink == 0 || size < natural - shrink) {
	return -PenaltyBad;
    } else {
	gsr = (size - natural)/shrink;
	if (gsr < -BADGSR) {
	    return -PenaltyBad;
	} else {
	    r = int(100 * gsr * gsr * gsr);
	    return Math::max(r, -PenaltyBad);
	}
    }
}

static void possible_break(
    CompositorIndex index, Coord* spans, CompositorIndex span_count,
    Coord natural, Coord stretch, Coord shrink, int penalty,
    int breakpenalty, BreakSet* breaks
) {
    BreakSet* best_break = nil;
    BreakSet* doomed;
    int least_demerits;
    BreakSet* b = breaks->next_;
    while (b != breaks) {
        Coord span = spans[Math::min(b->count_, span_count-1)];
        b->no_break(natural, stretch, shrink);
        int break_badness = badness(
            span, b->natural_, b->stretch_, b->shrink_
        );
        boolean only_break = b == breaks->next_ && b->next_ == breaks;
        if (penalty <= PenaltyGood) {
            int break_demerits = demerits(
                break_badness, penalty, breakpenalty
            );
            if (best_break == nil) {
                b->add_break(index, break_demerits);
                best_break = b;
                least_demerits = b->demerits_;
            } else if (b->demerits_ + break_demerits < least_demerits) {
                delete best_break;
                b->add_break(index, break_demerits);
                best_break = b;
                least_demerits = b->demerits_;
            } else {
                if (!only_break) {
                    doomed = b;
                    b = b->prev_;
                    delete doomed;
                }
            }
        } else if (break_badness < -TOLERANCE) {
            if (only_break) {
                int break_demerits = demerits(
                    break_badness, penalty, breakpenalty
                );
                b->add_break(index, break_demerits);
                best_break = b;
                least_demerits = b->demerits_;
            } else {
                doomed = b;
                b = b->prev_;
                delete doomed;
            }
        } else if (break_badness <= TOLERANCE) {
            int break_demerits = demerits(
                break_badness, penalty, breakpenalty
            );
            if (best_break == nil) {
                new BreakSet(b);
                b = b->next_;
                b->add_break(index, break_demerits);
                best_break = b;
                least_demerits = b->demerits_;
            } else if (b->demerits_ + break_demerits < least_demerits) {
                delete best_break;
                new BreakSet(b);
                b = b->next_;
                b->add_break(index, break_demerits);
                best_break = b;
                least_demerits = b->demerits_;
            }
        }
        b = b->next_;
    }
}

TeXCompositor::TeXCompositor(int penalty) : Compositor() {
    penalty_ = penalty;
}

TeXCompositor::~TeXCompositor() { }

CompositorIndex TeXCompositor::compose(
    Coord* natural, Coord* stretch, Coord* shrink,
    int* penalties, CompositorIndex component_count,
    Coord* spans, CompositorIndex span_count,
    CompositorIndex* breaks, CompositorIndex break_count
) {
    BreakSet* best_breaks = new BreakSet(nil);
    new BreakSet(best_breaks);
    Coord nat = 0;
    Coord str = 0;
    Coord shr = 0;
    int penalty;
    for (CompositorIndex i = 0; i < component_count; ++i) {
        nat += natural[i];
        str += stretch[i];
        shr += shrink[i];
        if (i == component_count - 1) {
            penalty = PenaltyGood;
        } else {
            penalty = penalties[i];
        }
        if (penalty < PenaltyBad) {
            possible_break(
                i, spans, span_count, nat, str, shr, penalty,
                penalty_, best_breaks
            );
            nat = 0;
            str = 0;
            shr = 0;
        }
    }

    CompositorIndex count = Math::min(break_count, best_breaks->next_->count_);
    for (CompositorIndex j = 0; j < count; ++j) {
        breaks[j] = best_breaks->next_->breaks_[j];
    }
    BreakSet* doomed = best_breaks->next_;
    delete doomed;
    delete best_breaks;
    return count;
}
