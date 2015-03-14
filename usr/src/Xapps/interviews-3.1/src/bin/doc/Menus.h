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
 * Menus for doc
 */

#ifndef Menus_h
#define Menus_h

#include <InterViews/boolean.h>
#include <InterViews/monoglyph.h>

class DocMenuInfo_List;
class DocumentViewer;
class Menu;

class DocMenu : public MonoGlyph {
public:
    DocMenu (DocumentViewer*, const char* name, Menu*);
    virtual ~DocMenu ();

    virtual int highlight (const char* tag, boolean);
    virtual void choose (const char* tag, boolean);
    virtual void enable (const char* tag, boolean);
private:
    Menu* _menu;
    DocumentViewer* _viewer;
    DocMenuInfo_List* _info;
};

class DocMenubar : public DocMenu {
public:
    DocMenubar (DocumentViewer*, const char* name);
    virtual ~DocMenubar ();
};

class DocPopup : public DocMenu {
public:
    DocPopup (DocumentViewer*, const char* name);
    virtual ~DocPopup ();
};

#endif
