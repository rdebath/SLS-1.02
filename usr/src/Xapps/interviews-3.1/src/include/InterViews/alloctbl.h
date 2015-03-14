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
 * AllocationTable - common information for <canvas, allocation> pairs
 */

#ifndef iv_alloctbl_h
#define iv_alloctbl_h

#include <InterViews/glyph.h>
#include <InterViews/transformer.h>

class AllocationTable;
class AllocationTableImpl;

class AllocationInfo {
public:
    Canvas* canvas() const;
    const Allocation& allocation() const;
    const Transformer& transformer() const;
    void extension(const Extension&);
    Extension& extension();
    const Extension& extension() const;
    Allocation* component_allocations();
private:
    friend class AllocationTable;

    Canvas* canvas_;
    Transformer transformer_;
    Allocation allocation_;
    Extension extension_;
    long num_components_;
    Allocation* component_allocation_;
};

inline Canvas* AllocationInfo::canvas() const { return canvas_; }

inline const Allocation& AllocationInfo::allocation() const {
    return allocation_;
}

inline const Transformer& AllocationInfo::transformer() const {
    return transformer_;
}

inline void AllocationInfo::extension(const Extension& e) { extension_ = e; }
inline Extension& AllocationInfo::extension() { return extension_; }

inline const Extension& AllocationInfo::extension() const {
    return extension_;
}

inline Allocation* AllocationInfo::component_allocations() {
    return component_allocation_;
}

class AllocationTable {
public:
    AllocationTable(GlyphIndex count = 1, long maximum_allocations = 5);
    virtual ~AllocationTable();

    virtual AllocationInfo* find(Canvas*, const Allocation&) const;
    virtual AllocationInfo* find_same_size(
	Canvas*, const Allocation&, Coord& dx, Coord& dy
    ) const;
    virtual AllocationInfo* allocate(Canvas*, const Allocation&);
    virtual AllocationInfo* most_recent() const;
    virtual void flush();
private:
    AllocationTableImpl* impl_;
};

#endif
