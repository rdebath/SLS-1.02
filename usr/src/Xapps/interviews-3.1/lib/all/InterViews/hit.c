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
 * Hit detection
 */

#include <InterViews/event.h>
#include <InterViews/hit.h>
#include <InterViews/transformer.h>
#include <OS/list.h>
#include <OS/math.h>
#include <OS/memory.h>

struct HitTarget {
    Glyph* glyph_;
    GlyphIndex index_;
    Handler* handler_;
};

static const int HitTargetList_fixed = 20;

struct HitTargetList {
    int avail_;
    int used_;
    HitTarget* targets_;
    HitTarget fixed_targets_[HitTargetList_fixed];
};

static const int HitList_fixed = 5;

struct HitList {
    int avail_;
    int used_;
    HitTargetList* lists_;
    HitTargetList fixed_lists_[HitList_fixed];
};

struct PossibleHitTarget {
    boolean picked_;
    GlyphIndex count_;
    int depth_;
    HitTarget item_;
};

static const int HitStack_fixed = 25;

struct HitStack {
    int avail_;
    int used_;
    PossibleHitTarget* possible_targets_;
    PossibleHitTarget fixed_possible_targets_[HitStack_fixed];
};

struct HitTargetArea {
    Coord left;
    Coord bottom;
    Coord right;
    Coord top;
};

static const int HitTargetAreaList_fixed = 25;

struct HitTargetAreaList {
    int avail_;
    int used_;
    HitTargetArea* areas_;
    HitTargetArea fixed_areas_[HitTargetAreaList_fixed];
};

class HitImpl {
private:
    friend class Hit;

    void init(Coord x1, Coord y1, Coord x2, Coord y2);
    void free();

    void add_item(
	boolean override,
	int depth, Glyph*, GlyphIndex, Handler*, GlyphIndex target
    );
    HitTarget& item(int depth, GlyphIndex);
    HitTargetArea& area();

    const Event* event_;
    Handler* default_handler_;
    int default_handler_depth_;
    HitList items_;
    HitStack picks_;
    HitTargetAreaList areas_;
};

void HitImpl::init(Coord x1, Coord y1, Coord x2, Coord y2) {
    event_ = nil;
    default_handler_ = nil;
    items_.avail_ = HitList_fixed;
    items_.used_ = 0;
    items_.lists_ = items_.fixed_lists_;
    picks_.avail_ = HitStack_fixed;
    picks_.used_ = 0;
    picks_.possible_targets_ = picks_.fixed_possible_targets_;
    areas_.avail_ = HitTargetAreaList_fixed;
    areas_.used_ = 1;
    areas_.areas_ = areas_.fixed_areas_;
    HitTargetArea& a = areas_.areas_[0];
    a.left = x1;
    a.bottom = y1;
    a.right = x2;
    a.top = y2;
}

void HitImpl::free() {
    for (int i = 0; i < items_.used_; i++) {
	HitTargetList& list = items_.lists_[i];
	if (list.targets_ != list.fixed_targets_) {
	    delete list.targets_;
	}
    }
    if (items_.lists_ != items_.fixed_lists_) {
	delete items_.lists_;
    }
    if (picks_.possible_targets_ != picks_.possible_targets_) {
	delete picks_.possible_targets_;
    }
    if (areas_.areas_ != areas_.fixed_areas_) {
	delete areas_.areas_;
    }
}

Hit::Hit(const Event* e) {
    init();
    Coord x = e->pointer_x();
    Coord y = e->pointer_y();
    impl_->init(x, y, x, y);
    impl_->event_ = e;
}

Hit::Hit(Coord x, Coord y) {
    init();
    impl_->init(x, y, x, y);
}

Hit::Hit(Coord left, Coord bottom, Coord right, Coord top) {
    init();
    impl_->init(left, bottom, right, top);
}

Hit::Hit(HitImpl* h) { impl_ = h; }

Hit::~Hit() {
    impl_->free();
    if (impl_ != (HitImpl*)free_store_) {
	delete impl_;
    }
}

void Hit::init() {
    if (sizeof(HitImpl) <= sizeof(free_store_)) {
	impl_ = (HitImpl*)free_store_;
    } else {
	impl_ = new HitImpl;
    }
}

const Event* Hit::event() const { return impl_->event_; }
Coord Hit::left() const { return impl_->area().left; }
Coord Hit::bottom() const { return impl_->area().bottom; }
Coord Hit::right() const { return impl_->area().right; }
Coord Hit::top() const { return impl_->area().top; }

void Hit::push_transform() {
    HitTargetAreaList& s = impl_->areas_;
    if (s.used_ >= s.avail_) {
	int new_avail = s.avail_ << 1;
	HitTargetArea* new_areas = new HitTargetArea[new_avail];
	Memory::copy(s.areas_, new_areas, s.used_ * sizeof(HitTargetArea));
	if (s.areas_ != s.fixed_areas_) {
	    delete s.areas_;
	}
	s.areas_ = new_areas;
	s.avail_ = new_avail;
    }
    s.areas_[s.used_] = s.areas_[s.used_ - 1];
    s.used_ += 1;
}

void Hit::transform(const Transformer& t) {
    HitTargetArea& a = impl_->area();
    Coord x1, y1, x2, y2, x3, y3, x4, y4;
    t.inverse_transform(a.left, a.bottom, x1, y1);
    t.inverse_transform(a.left, a.top, x2, y2);
    t.inverse_transform(a.right, a.top, x3, y3);
    t.inverse_transform(a.right, a.bottom, x4, y4);
    a.left = Math::min(x1, x2, x3, x4);
    a.bottom = Math::min(y1, y2, y3, y4);
    a.right = Math::max(x1, x2, x3, x4);
    a.top = Math::max(y1, y2, y3, y4);
}

void Hit::pop_transform() {
    HitTargetAreaList& s = impl_->areas_;
    if (s.used_ != 0) {
	s.used_ -= 1;
    }
}

void Hit::begin(int depth, Glyph* target, GlyphIndex index, Handler* h) {
    HitStack& s = impl_->picks_;
    if (s.used_ >= s.avail_) {
	int new_avail = s.avail_ << 1;
	PossibleHitTarget* new_possible = new PossibleHitTarget[new_avail];
	Memory::copy(
	    s.possible_targets_, new_possible,
	    s.used_ * sizeof(PossibleHitTarget)
	);
	if (s.possible_targets_ != s.fixed_possible_targets_) {
	    delete s.possible_targets_;
	}
	s.possible_targets_ = new_possible;
	s.avail_ = new_avail;
    }
    PossibleHitTarget& p = s.possible_targets_[s.used_];
    p.picked_ = false;
    p.count_ = impl_->items_.used_;
    p.depth_ = depth;
    p.item_.glyph_ = target;
    p.item_.index_ = index;
    p.item_.handler_ = h;
    s.used_ += 1;
}

void Hit::target(int depth, Glyph* target, GlyphIndex index, Handler* h) {
    HitStack& stk = impl_->picks_;
    long top = stk.used_ - 1;
    if (top >= 0) {
	stk.possible_targets_[top].picked_ = true;
    }
    HitList& i = impl_->items_;
    if (i.used_ >= i.avail_) {
	int new_avail = i.avail_ << 1;
	HitTargetList* new_lists = new HitTargetList[new_avail];
	for (int e = 0; e < i.used_; e++) {
	    HitTargetList& t_old = i.lists_[e];
	    HitTargetList& t_new = new_lists[e];
	    t_new.avail_ = t_old.avail_;
	    t_new.used_ = t_old.used_;
	    if (t_old.targets_ == t_old.fixed_targets_) {
		t_new.targets_ = t_new.fixed_targets_;
		Memory::copy(
		    t_old.fixed_targets_, t_new.fixed_targets_,
		    sizeof(t_new.fixed_targets_)
		);
	    } else {
		t_new.targets_ = t_old.targets_;
	    }
	}
	if (i.lists_ != i.fixed_lists_) {
	    delete i.lists_;
	}
	i.lists_ = new_lists;
	i.avail_ = new_avail;
    }
    HitTargetList& t = i.lists_[i.used_];
    t.avail_ = HitTargetList_fixed;
    t.used_ = -1;
    t.targets_ = t.fixed_targets_;
    for (HitTarget* ht = t.targets_; ht < &t.targets_[t.avail_]; ht++) {
	ht->glyph_ = nil;
    }
    i.used_ += 1;
    impl_->add_item(false, depth, target, index, h, 0);
}

void Hit::end() {
    HitStack& stk = impl_->picks_;
    int top = stk.used_ - 1;
    if (top >= 0) {
	const PossibleHitTarget& p = stk.possible_targets_[top];
	if (p.picked_) {
	    GlyphIndex new_targets = impl_->items_.used_ - p.count_;
	    for (int i = 0; i < new_targets; i++) {
		impl_->add_item(
		    false, p.depth_, p.item_.glyph_, p.item_.index_,
		    p.item_.handler_, i
		);
	    }
	    if (top > 0) {
		stk.possible_targets_[top - 1].picked_ = true;
	    }
	}
	stk.used_ = top;
    }
}

void Hit::remove(int depth, GlyphIndex target) {
    if (target < 0 || target >= impl_->items_.used_) {
	ListImpl_range_error(target);
    }
    HitTargetList& list = impl_->items_.lists_[target];
    for (int i = depth + 1; i <= list.used_; i++) {
	list.targets_[i - 1] = list.targets_[i];
    }
    list.used_ -= 1;
    if (list.targets_[depth].handler_ == impl_->default_handler_) {
	impl_->default_handler_depth_ = depth;
    }
}

void Hit::retarget(
    int depth, Glyph* g, GlyphIndex i, Handler* h, GlyphIndex target
) {
    impl_->add_item(true, depth, g, i, h, target);
}

boolean Hit::any() const { return impl_->items_.used_ != 0; }
int Hit::count() const { return impl_->items_.used_; }
int Hit::depth(GlyphIndex target) const {
    if (target < 0 || target >= impl_->items_.used_) {
	ListImpl_range_error(target);
    }
    return impl_->items_.lists_[target].used_;
}

Glyph* Hit::target(int depth, GlyphIndex target) const {
    return impl_->item(depth, target).glyph_;
}

GlyphIndex Hit::index(int depth, GlyphIndex target) const {
    return impl_->item(depth, target).index_;
}

Handler* Hit::handler() const {
    return impl_->default_handler_;
}

/*
 * Add an item to the current hit list.  Use the depth
 * to place it appropriately.
 */

void HitImpl::add_item(
    boolean override,
    int depth, Glyph* g, GlyphIndex i, Handler* h, GlyphIndex target
) {
    HitTarget& t = item(depth, target);
    if (override || t.glyph_ == nil) {
	t.glyph_ = g;
	t.index_ = i;
	t.handler_ = h;
    }
    if (h != nil &&
	(default_handler_ == nil || depth >= default_handler_depth_)
    ) {
	default_handler_ = h;
	default_handler_depth_ = depth;
    }
}

/*
 * The way we specify targets is the opposite of how we store them:
 * "item(depth, 0)" is the most recently targetted item at that depth.
 * So, we use "used_ - 1 - t" as the appropriate target index.
 */

HitTarget& HitImpl::item(int depth, GlyphIndex t) {
    if (t < 0 || t >= items_.used_) {
	ListImpl_range_error(t);
    }
    GlyphIndex target = items_.used_ - 1 - t;
    HitTargetList& list = items_.lists_[target];
    if (depth >= list.avail_) {
	int new_avail = depth + HitTargetList_fixed;
	HitTarget* new_targets = new HitTarget[new_avail];
	Memory::copy(
	    list.targets_, new_targets, (list.used_ + 1) * sizeof(HitTarget)
	);
	if (list.targets_ != list.fixed_targets_) {
	    delete list.targets_;
	}
	HitTarget* i;
	for (i = &new_targets[depth]; i < &new_targets[new_avail]; i++) {
	    i->glyph_ = nil;
	}
	list.avail_ = new_avail;
	list.targets_ = new_targets;
    }
    if (depth > list.used_) {
	list.used_ = depth;
    }
    return list.targets_[depth];
}

HitTargetArea& HitImpl::area() {
    HitTargetAreaList& s = areas_;
    return s.areas_[s.used_ - 1];
}
