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
 * ItemView
 */

#ifndef ItemView_h
#define ItemView_h

#include <InterViews/handler.h>
#include <InterViews/monoglyph.h>

class Item;
class DocumentViewer;
class Document;
class Listener;
class Printer;

class ItemView : public MonoGlyph, public Handler {
public:
    ItemView (DocumentViewer*, ItemView*);

    virtual boolean event (Event&);
    virtual void allocate (Canvas*, const Allocation&, Extension&);

    virtual boolean command (const char*);

    virtual void repair ();
    virtual void update ();
    virtual void activate (boolean);
protected:
    virtual ~ItemView ();

    virtual void mark_selection ();
    virtual void keystroke (Event&);
    virtual void select (Event&);
    virtual void manipulate (Event&);
    virtual void menu (Event&);

    DocumentViewer* _viewer;
    ItemView* _parent;
    Listener* _listener;
};

#endif
