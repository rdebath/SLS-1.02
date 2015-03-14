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
 * PSFigItem
 */

#ifndef PSFigItem_h
#define PSFigItem_h

#include "Item.h"

enum PSFigViewMode { PSDraft, PSFinal };

class Glyph;
class PSFigView;
class PSFigViewInfo_List;

class PSFigItem : public Item {
public:
    PSFigItem (Document*, Item* parent, long style, long source);

    virtual void read (istream&);
    virtual void write (ostream&);

    virtual Glyph* view (ItemView* parent, DocumentViewer*);

    virtual void attach (PSFigView* view);
    virtual void detach (PSFigView* view);
    virtual void notify ();

    virtual void parameters (const char*);
    virtual const char* parameters ();
    virtual void change_graphic ();
    virtual Glyph* graphic (PSFigViewMode, PSFigView*);
protected:
    virtual ~PSFigItem ();

    char* _parameters;
    char* _filename;
    char* _creator;
    float _left;
    float _bottom;
    float _right;
    float _top;
    float _width;
    float _height;
    float _hscale;
    float _vscale;
    float _hoffset;
    float _voffset;
    float _rotate;
    PSFigViewInfo_List* _view;
};

#endif
