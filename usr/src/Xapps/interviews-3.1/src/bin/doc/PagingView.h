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
 * PagingView
 */

#ifndef PagingView_h
#define PagingView_h

#include "TextView.h"

class Composition;
class Patch;
class LRMarker;
class InsertMarker;

class PagingView : public TextView {
public:
    PagingView (DocumentViewer*, ItemView*, TextItem*, Glyph* pages);

    virtual void reshaped ();
    virtual void view_page (long page);
    virtual long page_containing (long dot);

    virtual void item_changed (long index, long count);
    virtual void item_inserted (long index, long count);
    virtual void item_removed (long index, long count);
    virtual void item_replaced (long index, long count);

    virtual void repair ();
    virtual void update ();
    virtual boolean command (const char*);
protected:
    virtual ~PagingView ();
    virtual void mark_selection ();
    virtual long index (Coord x, Coord y);

    virtual void mark_insertion ();
    virtual void mark_column (long column);

    Composition* _columns;
    Composition* _lines;
    Composition* _characters;

    Patch* _text_patch;
    LRMarker** _select_marker;
    InsertMarker* _insert_marker;

    long _current_page;
    long _column_count;
};

#endif
