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
 * TextItem
 */

#ifndef TextItem_h
#define TextItem_h

#include "Item.h"

class TextView;
class TextInfo_List;
class TextComponentInfo_List;
class TextViewInfo_List;

class TextItem : public Item {
public:
    TextItem (
        Document*, Item* parent, long style, long source,
        const char* paramters, int size_hint = 0
    );

    virtual void style (long);
    virtual long style ();

    virtual void read (istream&);
    virtual void write (ostream&);
    virtual void label (const char*);
    virtual void change (Item*);

    virtual Glyph* view (ItemView* parent, DocumentViewer*);

    virtual void attach (TextView* view);
    virtual void detach (TextView* view);
    virtual void notify ();

    virtual void parameters (const char*);
    virtual const char* parameters ();
    virtual float format_width ();

    virtual long item_count ();
    virtual Item* item (long index);
    virtual long item_code (long index);
    virtual long item_style (long index);
    virtual long item_source (long index);

    virtual long insert (long index, long code, long style, long source, Item*);
    virtual long remove (long index, long count);
    virtual void replace (long index, long count, long style);
protected:
    virtual ~TextItem ();

    char* _parameters;
    TextInfo_List* _text;
    TextComponentInfo_List* _component;
    TextViewInfo_List* _view;
};

#endif
