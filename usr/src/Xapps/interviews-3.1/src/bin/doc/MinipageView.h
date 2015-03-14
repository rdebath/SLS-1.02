/*
 * Copyright (c) 1991 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * MiniPageView
 */

#ifndef MiniPageView_h
#define MiniPageView_h

#include "TextView.h"

class Composition;
class Patch;
class InsertMarker;
class LRMarker;
class XYMarker;

class MinipageView : public TextView {
public:
    MinipageView (DocumentViewer*, ItemView*, TextItem*);

    virtual void item_changed (long index, long count);
    virtual void item_inserted (long index, long count);
    virtual void item_removed (long index, long count);
    virtual void item_replaced (long index, long count);

    virtual void update ();
    virtual boolean command (const char*);

    virtual void allocate(Canvas*, const Allocation&, Extension&);
protected:
    virtual ~MinipageView ();
    virtual void mark_selection ();
    virtual long index (Coord x, Coord y);

    Glyph* _lines;
    Composition* _characters;

    Patch* _text_patch;
    LRMarker* _select_marker;
    InsertMarker* _insert_marker;
    XYMarker* _active_marker;
};

#endif
