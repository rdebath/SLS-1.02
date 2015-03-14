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
 * FloatItem
 */

#ifndef FloatItem_h
#define FloatItem_h

#include "Item.h"

class TextItem;
class FloatView;
class FloatViewInfo_List;

class FloatItem : public Item {
public:
    FloatItem (Document*, Item* parent, long style, long source);

    virtual void style (long);
    virtual long style ();

    virtual void item (TextItem*);
    virtual TextItem* item ();

    virtual void context (const char* context);
    virtual const char* context ();

    virtual void read (istream&);
    virtual void write (ostream&);
    virtual void label (const char*);
    virtual void change (Item*);

    virtual Glyph* view (ItemView* parent, DocumentViewer*);

    virtual void attach (FloatView* view);
    virtual void detach (FloatView* view);
    virtual void notify ();
protected:
    virtual ~FloatItem ();

    TextItem* _item;
    char* _context;
    FloatViewInfo_List* _view;
};

#endif
