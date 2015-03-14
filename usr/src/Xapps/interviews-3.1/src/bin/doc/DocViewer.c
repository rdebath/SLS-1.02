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

#include "DocViewer.h"

#include "Application.h"
#include "Command.h"
#include "Document.h"
#include "ItemView.h"
#include "Keymap.h"
#include "Menus.h"
#include "NoPrint.h"
#include "PageBorder.h"
#include "PageButton.h"
#include "TextItem.h"
#include "PagingView.h"
#include "MinipageView.h"
#include "PSFigView.h"
#include "RefView.h"
#include "TabularView.h"
#include "PagenoView.h"
#include "CounterView.h"
#include "LabelView.h"
#include "FloatView.h"

#include "doc-composition.h"
#include "doc-deck.h"
#include "doc-listener.h"
#include "properties.h"

#include <IV-look/kit.h>
#include <InterViews/background.h>
#include <InterViews/border.h>
#include <InterViews/box.h>
#include <InterViews/color.h>
#include <InterViews/display.h>
#include <InterViews/font.h>
#include <InterViews/hit.h>
#include <InterViews/label.h>
#include <InterViews/layout.h>
#include <InterViews/patch.h>
#include <InterViews/printer.h>
#include <InterViews/session.h>
#include <InterViews/simplecomp.h>
#include <InterViews/style.h>
#include <InterViews/page.h>
#include <InterViews/shadow.h>
#include <InterViews/tformsetter.h>
#include <InterViews/window.h>
#include <OS/list.h>
#include <OS/math.h>
#include <OS/string.h>

#include <fstream.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

class ViewerMenuInfo {
public:
    char* _name;
    DocMenubar* _menubar;
};

class ViewerKeymapInfo {
public:
    char* _name;
    DocKeymap* _keymap;
};

class ViewerPinnedInfo {
public:
    char* _name;
    DocPopup* _menu;
    boolean _mapped;
    TransientWindow* _window;
};

class ViewerFloatInfo {
public:
    Item* _item;
    Glyph* _view;
    Coord _x;
    Coord _y;
    long _page;
    Coord _repel_left;
    Coord _repel_right;
    Coord _repel_top;
    Coord _repel_bottom;
};

class ViewerPageInfo {
public:
    Telltale* _telltale;
    char* _label;
};

class ViewerColorInfo {
public:
    char* _name;
    const Color* _overlay;
    const Color* _underlay;
};

declareList(ViewerMenuInfo_List,ViewerMenuInfo)
implementList(ViewerMenuInfo_List,ViewerMenuInfo)

declareList(ViewerKeymapInfo_List,ViewerKeymapInfo)
implementList(ViewerKeymapInfo_List,ViewerKeymapInfo)

declareList(ViewerPinnedInfo_List,ViewerPinnedInfo)
implementList(ViewerPinnedInfo_List,ViewerPinnedInfo)

declareList(ViewerFloatInfo_List,ViewerFloatInfo)
implementList(ViewerFloatInfo_List,ViewerFloatInfo)

declareList(ViewerPageInfo_List,ViewerPageInfo)
implementList(ViewerPageInfo_List,ViewerPageInfo)

declareList(ViewerColorInfo_List,ViewerColorInfo)
implementList(ViewerColorInfo_List,ViewerColorInfo)

DocumentViewer::DocumentViewer (
    Application* application, Document* document
) : ApplicationWindow(_top = new Patch(nil)), Handler() {
    _application = application;
    _document = document;
    _document->ref();

    WidgetKit& kit = *WidgetKit::instance();
    Style* style = kit.style();
    const LayoutKit& layout = *LayoutKit::instance();
    const Color* fg = kit.foreground();
    const Color* bg = kit.background();
    const Font* font = kit.font();

    _page_bg = bg;
    String page_bg_color;
    if (style->find_attribute(PAGE_BACKGROUND_COLOR, page_bg_color)) {
	const Color* c = Color::lookup(
	    Session::instance()->default_display(), page_bg_color
	);
	if (c != nil) {
	    _page_bg = c;
	}
    }
    Resource::ref(_page_bg);

    _insert_flash = 0;
    float flash_rate_seconds;
    if (style->find_attribute(INSERT_FLASH_RATE, flash_rate_seconds)) {
	_insert_flash = long(flash_rate_seconds * 1000000);
    }

    _icon_font = font;
    String icon_font_name;
    if (style->find_attribute(PAGE_ICON_FONT, icon_font_name)) {
	const Font* f = Font::lookup(icon_font_name);
	if (f != nil) {
	    _icon_font = f;
	}
    }
    Resource::ref(_icon_font);

    _prev_page = new PageButton(
	layout.hcenter(new Label("-", _icon_font, fg)), fg
    );
    _prev_page->ref();
    _next_page = new PageButton(
	layout.hcenter(new Label("+", _icon_font, fg)), fg
    );
    _next_page->ref();

    _menu_info = new ViewerMenuInfo_List();
    _keymap_info = new ViewerKeymapInfo_List();
    _pinned_info = new ViewerPinnedInfo_List();
    _page_info = new ViewerPageInfo_List();
    _float_info = new ViewerFloatInfo_List();
    _color_info = new ViewerColorInfo_List();

    _header_patch = new Patch(nil);
    _page_patch = new Patch(nil);
    _footer_patch = new Patch(
	layout.hbox_first_aligned(_prev_page, _next_page)
    );
    _body_patch = new Patch(
	layout.vbox_first_aligned(
	    layout.margin(
		new PageBorder(
		    layout.margin(_page_patch, 1, 5, 5, 1), fg, _page_bg
                ),
		0, 0, 0, 0, fil, 0, 5, fil, 0, 0, 0, 0
	    ),
	    _footer_patch
	)
    );

    float leftmargin = _document->document_metric("leftsidemargin");
    float rightmargin = _document->document_metric("rightsidemargin");
    float bottommargin = _document->document_metric("bottommargin");
    float topmargin = _document->document_metric("topmargin");

    Listener* listener = new Listener(
        new Background(
	    layout.vbox_first_aligned(
		layout.vcenter(_header_patch, 1.0),
		layout.margin(_body_patch, 5)
            ), bg
        ), this
    );
    listener->key(true);
    listener->button(true, Event::any);
    _top->body(listener);

    _pages = new DocDeck();

    _view = new PagingView(
	this, nil, _document->body(), layout.variable_span(_pages)
    );
    _view->item_inserted(0L, _document->body()->item_count());
    _view->update();

    _page = new Page(_view);
    _page_patch->body(
	layout.margin(_page, leftmargin, rightmargin, bottommargin, topmargin)
    );

    _starting_page = long(_document->document_metric("startingpage"));
    _current_page = -1;
    _menubar = nil;
    _keymap = nil;
    _focus = nil;
    focus(_view);
    _reshaped = true;
    _document->attach(this);
}

DocumentViewer::~DocumentViewer () {
    _top->body(nil);
    _document->detach(this);

    _prev_page->unref();
    _next_page->unref();
    if (_icon_font != nil) {
        _icon_font->unref();
    }
    while (_color_info->count() > 0) {
        ViewerColorInfo& info = _color_info->item_ref(0);
        delete info._name;
        if (info._overlay != nil) {
            info._overlay->unref();
        }
        if (info._underlay != nil) {
            info._underlay->unref();
        }
        _color_info->remove(0);
    }
    delete _color_info;
    while (_menu_info->count() > 0) {
        ViewerMenuInfo& m = _menu_info->item_ref(0);
        delete m._name;
        if (m._menubar != nil) {
            m._menubar->unref();
        }
        _menu_info->remove(0);
    }
    delete _menu_info;
    while (_keymap_info->count() > 0) {
        ViewerKeymapInfo& m = _keymap_info->item_ref(0);
        delete m._name;
        delete m._keymap;
        _keymap_info->remove(0);
    }
    delete _keymap_info;
    while (_pinned_info->count() > 0) {
        ViewerPinnedInfo& p = _pinned_info->item_ref(0);
        if (p._mapped) {
            p._window->unmap();
        }
        delete p._window;
        delete p._name;
        _pinned_info->remove(0);
    }
    delete _pinned_info;
    while (_float_info->count() > 0) {
        ViewerFloatInfo& info = _float_info->item_ref(0);
        info._item->unref();
        info._view->unref();
        _float_info->remove(0);
    }
    delete _float_info;
    while (_page_info->count() > 0) {
        ViewerPageInfo& info = _page_info->item_ref(0);
        info._telltale->unref();
        delete info._label;
        _page_info->remove(0);
    }
    delete _page_info;
    _document->unref();
}

Application* DocumentViewer::application () {
    return _application;
}

Document* DocumentViewer::document () {
    return _document;
}

Glyph* DocumentViewer::view (ItemView* parent, TextItem* text) {
    TextView* view = new MinipageView(this, parent, text);
    view->item_inserted(0L, text->item_count());
    view->update();
    return view;
}

Glyph* DocumentViewer::view (ItemView* parent, PSFigItem* psfig) {
    PSFigView* view = new PSFigView(this, parent, psfig);
    view->graphic_changed();
    view->update();
    return view;
}

Glyph* DocumentViewer::view (ItemView* parent, TabularItem* tabular) {
    TabularView* view = new TabularView(this, parent, tabular);
    view->rebuild();
    view->update();
    return view;
}

Glyph* DocumentViewer::view (ItemView* parent, RefItem* ref) {
    RefView* view = new RefView(this, parent, ref);
    view->update();
    return view;
}

Glyph* DocumentViewer::view (
    ItemView* parent, PagenumberItem* pagenumber
) {
    PagenumberView* view = new PagenumberView(this, parent, pagenumber);
    view->update();
    return view;
}

Glyph* DocumentViewer::view (ItemView* parent, FloatItem* f) {
    FloatView* view = new FloatView(this, parent, f);
    view->update();
    return view;
}

Glyph* DocumentViewer::view (ItemView* parent, CounterItem* counter) {
    CounterView* view = new CounterView(this, parent, counter);
    view->update();
    return view;
}

Glyph* DocumentViewer::view (ItemView* parent, LabelItem* label) {
    LabelView* view = new LabelView(this, parent, label);
    view->update();
    return view;
}

boolean DocumentViewer::event (Event& e) {
    switch (e.type()) {
    case Event::key:
        if (_keymap != nil && _keymap->map(e)) {
            break;
        }
        if (_focus != nil) {
            _focus->event(e);
        }
        break;
    case Event::down:
        if (e.pointer_button() == Event::middle) {
            manipulate(e);
        } else if (e.pointer_button() == Event::right) {
            menu(e);
        }
        break;
    }
    return true;
}

boolean DocumentViewer::command (const char* command) {
    if (strncmp(command, "viewer", 6) == 0) {
	Style* style = Session::instance()->style();
	String ps_ext_string = "ps";
	style->find_attribute(POSTSCRIPT_FILE_EXTENSION, ps_ext_string);
	NullTerminatedString ps_ext(ps_ext_string);
	String doc_ext_string = "doc";
	style->find_attribute(DOCUMENT_FILE_EXTENSION, doc_ext_string);
	NullTerminatedString doc_ext(doc_ext_string);
        const char* keyword = command + 7;
        highlight(keyword, true);
        if (strcmp(keyword, "print") == 0) {
            const char* name = _document->name();
            if (name != nil && strlen(name) > 0) {
                char buffer[256];
                strcpy(buffer, name);
                char* dot = strrchr(buffer, '.');
                if (dot != nil) {
                    *dot = '\0';
                }
                strcat(buffer, ".");
                strcat(buffer, ps_ext.string());
                ofstream out(buffer);
                Printer* ps = new Printer(&out);
                const Allocation& a = _page_patch->allocation();
                ps->prolog(name);
                ps->resize(a.left(), a.bottom(), a.right(), a.top());
                long current_page = _current_page;
                long count = _page_info->count();
                for (long i = 0; i < count; ++i) {
                    ps->page(_page_info->item_ref(i)._label);
                    page_to(i);
                    _page_patch->print(ps, a);
		    _page_patch->undraw();
                }
                page_to(current_page);
                ps->epilog();
                delete ps;
            } else {
                _application->report(this, "Please save file before printing");
            }
        } else if (strcmp(keyword, "view") == 0) {
            _application->open(new DocumentViewer(_application, _document));
        } else if (strcmp(keyword, "new") == 0) {
            Document* document = _application->read("");
            _application->open(new DocumentViewer(_application, document));
        } else if (strcmp(keyword, "open") == 0) {
            const char* file_name = _application->choose(
                this, "Choose file to open:", doc_ext.string()
            );
            if (file_name != nil) {
                Document* document = _application->read(file_name);
                if (document != nil) {
                    document->name(file_name);
                    _application->open(
                        new DocumentViewer(_application, document)
                    );
                } else {
                    _application->report(
                        this, "Bad file: possible version mismatch"
                    );
                }
            }
        } else if (strcmp(keyword, "close") == 0) {
            long confirm = _document->touched() ? NotConfirmed : Confirmed;
            if (confirm != Confirmed) {
                confirm = _application->confirm(
                    this, "File is modified: Save changes?"
                );
            }
            if (confirm == Confirmed && _document->touched()) {
                const char* file_name = _document->name();
                if (file_name == nil || strlen(file_name) == 0) {
                    file_name = _application->choose(
                        this,
                        "Choose file for save:",
			doc_ext.string()
                    );
                }
                if (file_name == nil || strlen(file_name) == 0) {
                    confirm = Cancelled;
                } else {
                    _document->name(file_name);
                    _application->write(_document, file_name);
                }
            }
            if (confirm != Cancelled) {
                _application->close(this);
            }
        } else if (strcmp(keyword, "save") == 0)  {
            const char* file_name = _document->name();
            if (file_name == nil || strlen(file_name) == 0) {
                file_name = _application->choose(
                    this,
                    "Choose file for save:",
		    doc_ext.string()
                );
            }
            if (file_name != nil && strlen(file_name) > 0) {
                _document->name(file_name);
                _application->write(_document, file_name);
            }
        } else if (strcmp(keyword, "saveas") == 0) {
            const char* file_name = _application->choose(
                this,
                "Choose file for save:",
		doc_ext.string()
            );
            if (file_name != nil && strlen(file_name) > 0) {
                _document->name(file_name);
                _application->write(_document, file_name);
            }
	} else if (strcmp(keyword, "help") == 0) {
	    _application->report(this, "Help not implemented");
	} else if (strcmp(keyword, "about") == 0) {
	    String vers;
	    style->find_attribute(VERSION, vers);
	    char about[100];
	    sprintf(about, "Doc version %.*s", vers.length(), vers.string());
	    _application->report(this, about);
        }
        highlight(keyword, false);
        return false;
    } else if (strncmp(command, "menubar", 7) == 0) {
        menubar(command + 8);
        return false;
    } else if (strncmp(command, "keymap", 6) == 0) {
        keymap(command + 7);
        return false;
    } else if (strcmp(command, "page forward") == 0) {
        page_to(_current_page + 1);
        return false;
    } else if (strcmp(command, "page backward") == 0) {
        page_to(_current_page - 1);
        return false;
    } else if (strncmp(command, "page", 4) == 0) {
	page_to(atoi(command + 4));
        return false;
    } else if (strncmp(command, "pin", 3) == 0) {
        pin(command + 4);
        return false;
    } else if (strncmp(command, "unpin", 5) == 0) {
        unpin(command + 6);
        return false;
    } else {
        return _application->command(command);
    }
}

ItemView* DocumentViewer::focus () {
    return _focus;
}

void DocumentViewer::focus (ItemView* view) {
    if (view != _focus) {
        if (_focus != nil) {
            _focus->activate(false);
        }
        _focus = view;
        if (_focus != nil) {
            _focus->activate(true);
        }
    }
}

void DocumentViewer::choose (const char* tag, boolean choose) {
    if (_menubar != nil) {
        _menubar->choose(tag, choose);
    }
    long count = _pinned_info->count();
    for (long i = 0; i < count; ++i) {
        ViewerPinnedInfo& m = _pinned_info->item_ref(i);
        m._menu->choose(tag, choose);
    }
}

void DocumentViewer::enable (const char* tag, boolean enable) {
    if (_menubar != nil) {
        _menubar->enable(tag, enable);
    }
    long count = _pinned_info->count();
    for (long i = 0; i < count; ++i) {
        ViewerPinnedInfo& m = _pinned_info->item_ref(i);
        m._menu->enable(tag, enable);
    }
}

void DocumentViewer::highlight (const char* tag, boolean highlight) {
    if (_menubar != nil) {
        _menubar->highlight(tag, highlight);
    }
    Session::instance()->default_display()->flush();
}

void DocumentViewer::pin (const char* name) {
    long count = _pinned_info->count();
    for (long i = 0; i < count; ++i) {
        ViewerPinnedInfo& m = _pinned_info->item_ref(i);
        if (strcmp(m._name, name) == 0) {
            break;
        }
    }
    if (i == count) {
        ViewerPinnedInfo p;
        p._name = strcpy(new char[strlen(name) + 1], name);
        p._menu = new DocPopup(this, name);
        p._window = new TransientWindow(p._menu);
        p._window->transient_for(this);
        p._mapped = false;
        _pinned_info->append(p);
    }
    ViewerPinnedInfo& p = _pinned_info->item_ref(i);
    if (p._mapped) {
        p._window->unmap();
    }
    Event e;
    e.display(Session::instance()->default_display());
    e.poll();
    IntCoord x = e.x;
    IntCoord y = e.y;
    e.GetAbsolute(x, y);
    p._window->align(0.1, 0.9);
    p._window->place(x, y);
    p._window->map();
    p._mapped = true;
}

void DocumentViewer::unpin (const char* name) {
    long count = _pinned_info->count();
    for (long i = 0; i < count; ++i) {
        ViewerPinnedInfo& m = _pinned_info->item_ref(i);
        if (strcmp(m._name, name) == 0) {
            break;
        }
    }
    if (i < count) {
        ViewerPinnedInfo& p = _pinned_info->item_ref(i);
        if (p._mapped) {
            p._window->unmap();
        }
        p._mapped = false;
    }
}

void DocumentViewer::menubar (const char* name) {
    choose(nil, false);
    long count = _menu_info->count();
    for (long i = 0; i < count; ++i) {
        ViewerMenuInfo& m = _menu_info->item_ref(i);
        if (strcmp(m._name, name) == 0) {
            break;
        }
    }
    if (i == count) {
        ViewerMenuInfo m;
        m._name = strcpy(new char[strlen(name) + 1], name);
        m._menubar = new DocMenubar(this, name);
        m._menubar->ref();
        _menu_info->append(m);
    }
    ViewerMenuInfo& m = _menu_info->item_ref(i);
    _menubar = m._menubar;
    _header_patch->redraw();
    _header_patch->body(m._menubar);
    _header_patch->reallocate();
    _header_patch->redraw();
}

void DocumentViewer::keymap (const char* name) {
    long count = _keymap_info->count();
    for (long i = 0; i < count; ++i) {
        ViewerKeymapInfo& info = _keymap_info->item_ref(i);
        if (strcmp(info._name, name) == 0) {
            break;
        }
    }
    if (i == count) {
        ViewerKeymapInfo info;
        info._name = strcpy(new char[strlen(name) + 1], name);
        info._keymap = new DocKeymap(this, name);
        _keymap_info->append(info);
    }
    ViewerKeymapInfo& info = _keymap_info->item_ref(i);
    _keymap = info._keymap;
}

long DocumentViewer::insert_flash () {
    return _insert_flash;
}

void DocumentViewer::highlight_colors (
    const char* name, const Color*& overlay, const Color*& underlay
) {
    long count = _color_info->count();
    for (long i = 0; i < count; ++i) {
        ViewerColorInfo& info = _color_info->item_ref(i);
        if (strcmp(info._name, name) == 0) {
            break;
        }
    }
    if (i == count) {
        ViewerColorInfo info;
        info._name = strcpy(new char[strlen(name) + 1], name);
	WidgetKit& kit = *WidgetKit::instance();
	Style* style = kit.style();
	Display* display = Session::instance()->default_display();

	const Color* highlight = nil;
	String hl;
	if (style->find_attribute(name, hl)) {
	    highlight = Color::lookup(display, hl);
	}
	if (highlight == nil) {
	    String default_hl;
	    if (style->find_attribute(HIGHLIGHT_COLOR, default_hl)) {
		highlight = Color::lookup(display, default_hl);
	    }
	    if (highlight == nil) {
		highlight = new Color(0.8, 0.8, 0.8, 1.0);
	    }
	}
	Resource::ref(highlight);

        const Color* fg = kit.foreground();
        const Color* bg = kit.background();
        if (highlight->distinguished(fg) && highlight->distinguished(bg)) {
            info._underlay = highlight;
            info._underlay->ref();
            info._overlay = nil;
        } else {
            info._underlay = nil;
            info._overlay = new Color(0.0, 0.0, 0.0, 1.0, Color::Xor);
            info._overlay->ref();
        }
        highlight->unref();
        _color_info->append(info);
    }
    ViewerColorInfo& info = _color_info->item_ref(i);
    overlay = info._overlay;
    underlay = info._underlay;
}

void DocumentViewer::float_inserted (Item* item) {
    ViewerFloatInfo info;
    info._item = item;
    info._item->ref();
    info._view = nil;
    info._x = 0;
    info._y = 0;
    info._page = -3;
    info._repel_right = _document->document_metric("floatrepelright");
    info._repel_left = _document->document_metric("floatrepelleft");
    info._repel_top = _document->document_metric("floatrepeltop");
    info._repel_bottom = _document->document_metric("floatrepelbottom");
    _float_info->append(info);
    _page->append(nil);
    _page->show(_float_info->count()-1, false);
    _reshaped = true;
}

void DocumentViewer::float_removed (Item* item) {
    long count = _float_info->count();
    for (long i = 0; i < count; ++i) {
        if (_float_info->item_ref(i)._item == item) {
            break;
        }
    }
    ViewerFloatInfo& info = _float_info->item_ref(i);
    info._item->unref();
    Resource::unref(info._view);
    _float_info->remove(i);
    _page->show(i, false);
    _page->remove(i);
    _reshaped = true;
}

void DocumentViewer::float_changed (Item* item) {
    long count = _float_info->count();
    for (long i = 0; i < count; ++i) {
        if (_float_info->item_ref(i)._item == item) {
            break;
        }
    }
    ViewerFloatInfo& info = _float_info->item_ref(i);
    _page->change(i);
    _body_patch->change(0);
    _body_patch->reallocate();
    _reshaped = true;
}

void DocumentViewer::float_adjusted (Item* item, float x, float y, long p) {
    Session::instance()->default_display()->flush();
    long count = _float_info->count();
    for (long i = 0; i < count; ++i) {
        if (_float_info->item_ref(i)._item == item) {
            break;
        }
    }
    ViewerFloatInfo& info = _float_info->item_ref(i);
    if (info._view == nil) {
	info._view = item->view(nil, this);
	info._view->ref();
    }
    if (info._page != p) {
        Glyph* g;
        if (p == -1) {
            const Color* fg = WidgetKit::instance()->foreground();
            const Color* bg = _page_bg;
            g = new NoPrint(
                new Shadow(
                    new Background(new Border(info._view, fg, 1), bg), 4, -4,
		    new Color(0.0, 0.0, 0.0, 0.5)
                )
            );
        } else {
            g = info._view;
        }
        _page->replace(i, g);
        boolean showing = (
            p == -2 && _current_page > 0
            || p == -1
            || p == _current_page
        );
        _page->show(i, showing);
    }
    if (info._x != x || info._y != y) {
        _page->move(i, x, y);
    }
    _page->change(i);
    _body_patch->change(0);
    _body_patch->reallocate();
    _reshaped = true;
    info._x = x;
    info._y = y;
    info._page = p;
}

long DocumentViewer::float_index (Coord x, Coord y) {
    Hit hit(x, y);
    _page_patch->repick(0, hit);
    if (hit.any()) {
        if (hit.target(0) == _page) {
            return hit.index(0);
        } else {
            return -1;
        }
    } else {
        return -1;
    }
}

void DocumentViewer::manipulate (Event& e) {
    long index = float_index(e.pointer_x(), e.pointer_y());
    if (index >= 0) {
        ViewerFloatInfo& info = _float_info->item_ref(index);
        Coord x = e.pointer_x() - info._x;
        Coord y = e.pointer_y() - info._y;
        boolean synchronous = !e.shift_is_down();
        do {
            _document->adjust_float(
                info._item,
                e.pointer_x() - x, e.pointer_y() - y, info._page
            );
            if (synchronous) {
                _document->notify();
            }
            e.read();
        } while (e.type() != Event::up);
        _document->notify();
    }
}

void DocumentViewer::menu (Event& e) {
    long index = float_index(e.pointer_x(), e.pointer_y());
    boolean shift = e.shift_is_down();
    if (index >= 0) {
        ViewerFloatInfo& info = _float_info->item_ref(index);
        long new_page;
        if (info._page == -1) {
            new_page = shift ? -2 : _current_page;
        } else {
            new_page = -1;
        }
        _document->adjust_float(info._item, info._x, info._y, new_page);
        _document->notify();
    }
}

const char* DocumentViewer::current_page_label () const {
    if (_current_page >= 0) {
        return _page_info->item_ref(_current_page)._label;
    } else {
        return "?";
    }
}

void DocumentViewer::page_to (long page) {
    long page_count = _pages->count()/2;
    if (page_count != _page_info->count()) {
	WidgetKit& kit = *WidgetKit::instance();
	const LayoutKit& layout = *LayoutKit::instance();
	Style* style = kit.style();
        const Color* fg = kit.foreground();
        char label[10];
        while (_page_info->count() < page_count) {
            ViewerPageInfo info;
            _document->format_counter(
                _page_info->count() + _starting_page + 1,
                _document->document_parameter("pagenumberformat"),
                label
            );
            info._label = strcpy(new char[strlen(label) + 1], label);
            info._telltale = new PageButton(
                layout.hcenter(new Label(info._label, _icon_font, fg)), fg
            );
	    info._telltale->state()->set(TelltaleState::is_enabled, true);
            info._telltale->ref();
            _page_info->append(info);
        }
        while (_page_info->count() > page_count) {
            long count = _page_info->count();
            ViewerPageInfo& info = _page_info->item_ref(count - 1);
            if (info._telltale != nil) {
                info._telltale->unref();
            }
            delete info._label;
            _page_info->remove(count - 1);
        }

        PolyGlyph* buttons = layout.hbox_first_aligned();
	buttons->append(nil);
        buttons->append(
            new Button(
		_prev_page, style, _prev_page->state(),
		new Command(this, "page backward")
	    )
        );
        buttons->append(
            new Button(
		_next_page, style, _next_page->state(),
		new Command(this, "page forward")
	    )
        );
        buttons->append(layout.hspace(5));

        Coord width = _document->document_metric("textwidth");
        LRComposition* comp = new LRComposition(
            layout.vbox_first_aligned(), new SimpleCompositor(), nil, width
        );
        for (long i = 0; i < page_count; ++i) {
            ViewerPageInfo& info = _page_info->item_ref(i);
            char command [20];
            sprintf(command, "page %d", i);
            comp->append(
                new Button(
		    info._telltale, style, info._telltale->state(),
		    new Command(this, command)
		)
            );
            comp->append(
		layout.discretionary(0, nil, nil, nil, nil)
            );
        }
        comp->repair();
        buttons->append(comp);
	buttons->append(layout.hglue());
        _footer_patch->redraw();
        _footer_patch->body(buttons);
	_body_patch->change(1);
	_top->change(1);
	_top->reallocate();
    }
    page = Math::max(0L, page);
    page = Math::min(page_count-1, page);
    if (page != _current_page) {
        _current_page = page;
        _pages->flip_to(_current_page * 2);
        _view->view_page(_current_page * 2);
        for (long i = 0; i < page_count; ++i) {
            ViewerPageInfo& info = _page_info->item_ref(i);
            info._telltale->choose(i == _current_page);
        }
        _prev_page->enable(_current_page > 0);
        _next_page->enable(_current_page < page_count - 1);
        long float_count = _float_info->count();
        for (long j = 0; j < float_count; ++j) {
            ViewerFloatInfo& info = _float_info->item_ref(j);
            boolean showing = (
                info._page == -2 && _current_page > 0
                || info._page == -1
                || info._page == _current_page
            );
            _page->show(j, showing);
        }
        _page_patch->reallocate();
        _page_patch->redraw();
    }
}

void DocumentViewer::page_to_view (long index) {
    page_to(_view->page_containing(index) / 2);
}

void DocumentViewer::update () {
    if (_reshaped) {
        _view->reshaped();
        _reshaped = false;
        page_to(_current_page);
    }
    _view->view_page(_current_page * 2);
    enable("save", _document->touched());
    enable("revert", _document->touched());
}

Coord DocumentViewer::top_margin (
    long page, Coord l, Coord b, Coord r, Coord t
) {
    Coord top = t;
    long count = _float_info->count();
    for (long i = 0; i < count; ++i) {
        ViewerFloatInfo& info = _float_info->item_ref(i);
        if (info._page == page/2 || info._page == -2 && page > 0) {
	    Allotment ax, ay;
            _page->allotment(i, Dimension_X, ax);
            _page->allotment(i, Dimension_Y, ay);
            Coord fl = ax.begin();
            Coord fr = ax.end();
            Coord fb = ay.begin();
            Coord ft = ay.end();
            if (fr + info._repel_right > l && fl - info._repel_left < r) {
                if ((fb + ft)/2 > (b + t)/2) {
                    top = Math::min(top, fb - info._repel_bottom);
                }
            }
        }
    }
    return t - top;
}

Coord DocumentViewer::bottom_margin (
    long page, Coord l, Coord b, Coord r, Coord t
) {
    Coord bottom = b;
    long count = _float_info->count();
    for (long i = 0; i < count; ++i) {
        ViewerFloatInfo& info = _float_info->item_ref(i);
        if (info._page == page/2 || info._page == -2 && page > 0) {
	    Allotment ax, ay;
            _page->allotment(i, Dimension_X, ax);
            _page->allotment(i, Dimension_Y, ay);
            Coord fl = ax.begin();
            Coord fr = ax.end();
            Coord fb = ay.begin();
            Coord ft = ay.end();
            if (fr + info._repel_right > l && fl - info._repel_left < r) {
                if ((fb + ft)/2 <= (b + t)/2) {
                    bottom = Math::max(bottom, ft + info._repel_top);
                }
            }
        }
    }
    return bottom - b;
}
