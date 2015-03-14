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
 * TabularView
 */

#ifndef TabularView_h
#define TabularView_h

#include "ItemView.h"

class TabularItem;

class Aggregate;
class Patch;
class Sensor;
class XYMarker;

class TabularView : public ItemView {
public:
    TabularView (DocumentViewer*, ItemView*, TabularItem*);

    void rebuild ();
    void row_inserted (long index);
    void row_removed (long index);
    void column_inserted (long index);
    void column_removed (long index);
    void cell_replaced (long row, long column);
    void cell_changed (long row, long column);
    void row_separator_changed (long index);
    void column_separator_changed (long index);

    virtual void repair ();
    virtual void update ();
    virtual void activate (boolean);
    virtual boolean command (const char*);
protected:
    virtual ~TabularView ();

    virtual void mark_selection ();
    virtual void keystroke (Event&);
    virtual void select (Event&);

    void dot (long row, long column);
    void mark (long row, long column);
    long row_hit (Coord x, Coord y);
    long column_hit (Coord x, Coord y);

    TabularItem* _tabular;
    Aggregate* _cells;
    Glyph* _columns;
    Glyph* _rows;
    Glyph* _table;
    Patch* _patch;
    Patch* _row_patch;
    Patch* _column_patch;
    boolean _active; 
    XYMarker* _marker;
    long _row_dot;
    long _row_mark;
    long _column_dot;
    long _column_mark;
};

#endif
