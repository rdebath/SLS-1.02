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
 * AllocationInfo - common information stored for allocations
 */

#include <InterViews/alloctbl.h>
#include <InterViews/canvas.h>
#include <OS/list.h>
#include <OS/math.h>

/*
 * An allocation table is represented by a list of pointers to
 * allocation information objects.  The list is ordered by usage
 * from least recently used to most recently used.  Whenever a lookup
 * is done on the table that finds the information for a <canvas, allocation>
 * pair, the pointer to the information is removed from its current position
 * in the list and put at the end.  In the common case where the list
 * has a single element, moving the found element is unnecessary.
 */

declarePtrList(AllocationInfoList,AllocationInfo)
implementPtrList(AllocationInfoList,AllocationInfo)

class AllocationTableImpl {
private:
    friend class AllocationTable;

    AllocationTableImpl(GlyphIndex, long);

    static boolean equal(const Allocation&, const Allocation&);
    static boolean same_size(const Allotment&, const Allotment&);

    GlyphIndex count_;
    long maximum_allocations_;
    AllocationInfoList allocations_;
    static const float epsilon_;
};

const float AllocationTableImpl::epsilon_ = 1e-4;

AllocationTableImpl::AllocationTableImpl(
    GlyphIndex count, long maximum
) : count_(count), maximum_allocations_(maximum), allocations_(maximum) { }

inline boolean AllocationTableImpl::equal(
    const Allocation& a1, const Allocation& a2
) {
    return a1.equals(a2, epsilon_);
}

inline boolean AllocationTableImpl::same_size(
    const Allotment& a1, const Allotment& a2
) {
    return (
	Math::equal(a1.span(), a2.span(), epsilon_) &&
	Math::equal(a1.alignment(), a2.alignment(), epsilon_)
    );
}

AllocationTable::AllocationTable(GlyphIndex count, long maximum_allocations) {
    impl_ = new AllocationTableImpl(count, maximum_allocations);
}

AllocationTable::~AllocationTable() {
    flush();
    delete impl_;
}

/*
 * Find an element on the list that matches the given canvas and allocation.
 */

AllocationInfo* AllocationTable::find(Canvas* c, const Allocation& a) const {
    AllocationInfoList& list = impl_->allocations_;
    for (ListUpdater(AllocationInfoList) i(list); i.more(); i.next()) {
	AllocationInfo* info = i.cur();
	if (info->canvas_ == c &&
	    (c == nil || info->transformer_ == c->transformer()) &&
	    AllocationTableImpl::equal(info->allocation_, a)
	) {
	    if (list.count() > 1) {
		i.remove_cur();
		list.append(info);
	    }
	    return info;
	}
    }
    return nil;
}

/*
 * Find an element on the list with the same canvas and same size
 * allocation as given.  If found, set the difference in the position
 * of the new allocation's origin to dx and dy.
 */

AllocationInfo* AllocationTable::find_same_size(
    Canvas* c, const Allocation& a, Coord& dx, Coord& dy
) const {
    const Allotment& x = a.x_allotment();
    const Allotment& y = a.y_allotment();
    AllocationInfoList& list = impl_->allocations_;
    for (ListUpdater(AllocationInfoList) i(list); i.more(); i.next()) {
	AllocationInfo* info = i.cur();
	if (info->canvas_ == c &&
	    (c == nil || info->transformer_ == c->transformer())
	) {
	    Allotment& oldx = info->allocation_.x_allotment();
	    Allotment& oldy = info->allocation_.y_allotment();
	    if (AllocationTableImpl::same_size(x, oldx) &&
		AllocationTableImpl::same_size(y, oldy)
	    ) {
		dx = x.origin() - oldx.origin();
		dy = y.origin() - oldy.origin();
		oldx.origin(x.origin());
		oldy.origin(y.origin());
		if (list.count() > 1) {
		    i.remove_cur();
		    list.append(info);
		}
		return info;
	    }
        }
    }
    return nil;
}

/*
 * Allocate a new table entry for the given canvas and allocation.
 * If the table is full, then use the first (least recently used) entry.
 */

AllocationInfo* AllocationTable::allocate(Canvas* c, const Allocation& a) {
    AllocationInfo* info;
    AllocationInfoList& list = impl_->allocations_;
    if (list.count() < impl_->maximum_allocations_) {
	info = new AllocationInfo;
	if (impl_->count_ == 0) {
	    info->component_allocation_ = nil;
	} else {
	    info->component_allocation_ = new Allocation[impl_->count_];
	}
    } else {
	info = list.item(0);
	list.remove(0);
    }
    info->canvas_ = c;
    if (c == nil) {
	/*
	 * This case shouldn't happen, but some old code (doc)
	 * occasionally passes nil for the canvas during allocation.
	 */
	Transformer t;
	info->transformer_ = t;
    } else {
	info->transformer_ = c->transformer();
    }
    info->allocation_ = a;
    list.append(info);
    return info;
}

/*
 * Return the most recently used allocation information, if any.
 */

AllocationInfo* AllocationTable::most_recent() const {
    AllocationInfo* info = nil;
    AllocationInfoList& list = impl_->allocations_;
    long n = list.count();
    if (n != 0) {
	info = list.item(n - 1);
    }
    return info;
}

/*
 * Flush all the entries from the table.
 */

void AllocationTable::flush() {
    AllocationInfoList& list = impl_->allocations_;
    for (ListItr(AllocationInfoList) i(list); i.more(); i.next()) {
	AllocationInfo* info = i.cur();
	if (info->component_allocation_ != nil) {
	    delete [] info->component_allocation_;
	}
	delete info;
    }
    list.remove_all();
}
