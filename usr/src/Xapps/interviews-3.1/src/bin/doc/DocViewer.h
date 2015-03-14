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
 * DocumentViewer
 */

#ifndef DocumentViewer_h
#define DocumentViewer_h

#include <InterViews/window.h>
#include <InterViews/handler.h>

class Application;
class CounterItem;
class Color;
class DocDeck;
class DocKeymap;
class DocMenubar;
class Patch;
class Document;
class FloatItem;
class Font;
class LabelItem;
class Item;
class ItemView;
class Page;
class PagenumberItem;
class PagingView;
class PSFigItem;
class RefItem;
class Telltale;
class TextItem;
class TabularItem;
class ViewerColorInfo_List;
class ViewerMenuInfo_List;
class ViewerKeymapInfo_List;
class ViewerPinnedInfo_List;
class ViewerFloatInfo_List;
class ViewerPageInfo_List;

class DocumentViewer : public ApplicationWindow, public Handler {
public:
    DocumentViewer (Application*, Document*);

    virtual Application* application ();
    virtual Document* document ();

    virtual Glyph* view (ItemView* parent, TextItem*);
    virtual Glyph* view (ItemView* parent, TabularItem*);
    virtual Glyph* view (ItemView* parent, PSFigItem*);
    virtual Glyph* view (ItemView* parent, RefItem*);
    virtual Glyph* view (ItemView* parent, PagenumberItem*);
    virtual Glyph* view (ItemView* parent, FloatItem*);
    virtual Glyph* view (ItemView* parent, LabelItem*);
    virtual Glyph* view (ItemView* parent, CounterItem*);

    virtual void float_inserted (Item*);
    virtual void float_removed (Item*);
    virtual void float_changed (Item*);
    virtual void float_adjusted(Item*, float x, float y, long page);
    
    virtual Coord top_margin (long page, Coord l, Coord b, Coord r, Coord t);
    virtual Coord bottom_margin (
	long page, Coord l, Coord b, Coord r, Coord t
    );

    virtual const char* current_page_label () const;
    virtual void page_to_view (long index);
    virtual void page_to (long);

    virtual boolean event(Event&);

    virtual boolean command (const char*);

    virtual void update ();

    virtual ItemView* focus ();
    virtual void focus (ItemView*);

    virtual void highlight (const char* tag, boolean);
    virtual void choose (const char* tag, boolean);
    virtual void enable (const char* tag, boolean);

    virtual void menubar (const char* name);
    virtual void keymap (const char* name);
    virtual void pin (const char* name);
    virtual void unpin (const char* name);

    virtual long insert_flash();
    virtual void highlight_colors (
        const char*, const Color*& overlay, const Color*& underlay
    );
protected:
    virtual ~DocumentViewer ();

    void manipulate (Event&);
    void menu (Event&);
    long float_index (Coord x, Coord y);

    Application* _application;
    Document* _document;
    ItemView* _focus;

    long _insert_flash;
    const Font* _icon_font;
    const Color* _page_bg;
    long _starting_page;
    long _current_page;
    boolean _reshaped;

    DocMenubar* _menubar;
    DocKeymap* _keymap;

    Patch* _top;
    Patch* _header_patch;
    Patch* _body_patch;
    Patch* _footer_patch;
    Patch* _page_patch;
    Telltale* _prev_page;
    Telltale* _next_page;
    PagingView* _view;
    Page* _page;
    DocDeck* _pages;
    ViewerColorInfo_List* _color_info;
    ViewerMenuInfo_List* _menu_info;
    ViewerKeymapInfo_List* _keymap_info;
    ViewerPinnedInfo_List* _pinned_info;
    ViewerFloatInfo_List* _float_info;
    ViewerPageInfo_List* _page_info;
};

#endif
