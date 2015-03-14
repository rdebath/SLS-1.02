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
 * Counter
 */

#ifndef CounterItem_h
#define CounterItem_h

#include "Item.h"

class CounterView;
class CounterViewInfo_List;

class CounterItem : public Item {
public:
    CounterItem (Document*, Item* parent, long style, long source);

    virtual void name (const char*);
    virtual const char* name ();

    virtual void read (istream&);
    virtual void write (ostream&);
    virtual void label (const char*);

    virtual Glyph* view (ItemView* parent, DocumentViewer*);

    virtual void attach (CounterView* view);
    virtual void detach (CounterView* view);
    virtual void notify ();
protected:
    virtual ~CounterItem ();

    char* _name;
    CounterViewInfo_List* _view;
};

#endif
