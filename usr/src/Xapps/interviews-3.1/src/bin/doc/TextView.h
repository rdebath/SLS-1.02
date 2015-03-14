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
 * TextView
 */

#ifndef TextView_h
#define TextView_h

#include "ItemView.h"

class TextItem;

class TextView : public ItemView {
public:
    TextView (DocumentViewer*, ItemView*, TextItem*);

    virtual void item_changed (long index, long count);
    virtual void item_inserted (long index, long count);
    virtual void item_removed (long index, long count);
    virtual void item_replaced (long index, long count);

    virtual void repair ();
    virtual void update ();
    virtual void activate (boolean);
    virtual boolean command (const char*);

    virtual long dot ();
    virtual void dot (long);
    virtual long mark ();
    virtual void mark (long);
protected:
    virtual ~TextView ();
    virtual void mark_selection ();
    virtual void selection_changed (boolean compute_style);
    virtual long index (Coord x, Coord y);
    virtual boolean safe_to_edit (long dot, long mark);

    virtual boolean handle_char (long);
    virtual void keystroke (Event&);
    virtual void select (Event&);

    virtual boolean insert_text (long character);

    TextItem* _text;

    char* _encoded_keymap;
    char* _verbatim_keymap;
    char* _menubar;
    boolean _active;
    long _dot;
    long _mark;
    long _style;
    long _source;
};

#endif
