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

#include "Command.h"
#include "Document.h"
#include "DocViewer.h"
#include "Menus.h"
#include "TextItem.h"

#include "doc-target.h"

#include <IV-look/kit.h>
#include <IV-look/telltale.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/style.h>

#include <OS/list.h>
#include <OS/string.h>

#include <stdio.h>
#include <string.h>
#include <strstream.h>

class DocMenuInfo {
public:
    char* _tag;
    DocMenu* _menu;
};

declareList(DocMenuInfo_List,DocMenuInfo)
implementList(DocMenuInfo_List,DocMenuInfo)

static Glyph* make_label (Document* document, char* text) {
    istrstream in(text);
    TextItem* entry = new TextItem(nil, nil, 0, 0, "");
    entry->ref();
    document->read(in, entry, 0, 0);
    long count = entry->item_count();
    Glyph* label = new DocTarget(LayoutKit::instance()->hbox(count));
    for (long i = 0; i < count; ++i) {
	label->append(
	    document->character(entry->item_code(i), entry->item_style(i))
	);
    }
    entry->unref();
    return label;
}

DocMenu::DocMenu (
    DocumentViewer* viewer, const char* name, Menu* body
) : MonoGlyph(body) {
    _menu = body;
    _info = new DocMenuInfo_List();
    _viewer = viewer;
    WidgetKit& kit = *WidgetKit::instance();
    String v;
    if (!kit.style()->find_attribute(name, v)) {
	return;
    }

    const char* def = v.string();
    boolean done = false;
    while (!done) {
	boolean check_item = true;
        char line[256];
        char* p = line;
        char c;
        do {
            c = *def; if (c == '\n') c = '\0'; *p = c;
            if (*def == '\0') done = true;
            ++p; ++def;
        } while (c != '\0');
        p = line;
        while (*p == ' ' || *p == '\t') ++p;
	if (*p == '[') {
	    char* item_type = p + 1;
	    char* end_p = strchr(item_type, ']');
	    if (end_p != nil) {
		*end_p = '\0';
		check_item = (strcmp(item_type, "check") == 0);
		p = end_p + 1;
	    }
	}
        char* tag;
        if (*p == '<') {
            tag = p + 1;
            char* end_p = strchr(tag, '>');
	    if (end_p != nil) {
		*end_p = '\0';
		p = end_p + 1;
	    }
        } else {
            tag = "";
        }
        char* command;
        while (*p == ' ' || *p == '\t') ++p;
        if (*p == '(') {
            command = p + 1;
            char* end_p = strchr(command, ')');
	    if (end_p != nil) {
		*end_p = '\0';
		p = end_p + 1;
	    }
        } else {
            command = "";
        }
        while (*p == ' ' || *p == '\t') ++p;

        DocMenuInfo info;
        info._tag = strcpy(new char[strlen(tag) + 1], tag);
	info._menu = nil;
	MenuItem* item;
	if (p == nil || strlen(p) == 0) {
	    item = kit.menu_item_separator();
	} else {
	    Glyph* label = make_label(viewer->document(), p);
	    if (strncmp(command, "menu", 4) == 0) {
		Menu* pulldown = kit.pulldown();
		info._menu = new DocMenu(viewer, command+5, pulldown);
		item = kit.menubar_item(label);
		item->menu(pulldown);
	    } else {
		Command* cmd = nil;
		if (strlen(command) > 0) {
		    cmd = new Command(viewer, command);
		}
		if (check_item) {
		    item = kit.check_menu_item(label);
		    TelltaleState* s = item->state();
		    s->set(TelltaleState::is_toggle, false);
		    s->set(TelltaleState::is_choosable, true);
		} else {
		    item = kit.menu_item(label);
		}
		item->action(cmd);
	    }
	}
	_menu->append_item(item);
        _info->append(info);
    }
}

DocMenu::~DocMenu () {
    while (_info->count() > 0) {
        DocMenuInfo& info = _info->item_ref(0);
        delete info._tag;
        _info->remove(0);
    }
    delete _info;
}

int DocMenu::highlight (const char* tag, boolean highlight) {
    int tally = 0;
    long count = _info->count();
    for (long i = 0; i < count; i++) {
        DocMenuInfo& info = _info->item_ref(i);
        int subtally = 0;
        if (info._menu != nil) {
            subtally += info._menu->highlight(tag, highlight);
        }
        if (tag == nil || strcmp(tag, info._tag) == 0) {
            subtally += 1;
        }
        if (subtally > 0) {
            _menu->item(i)->state()->set(TelltaleState::is_active, highlight);
        }
        tally += subtally;
    }
    return tally;
}

void DocMenu::choose (const char* tag, boolean choose) {
    long count = _info->count();
    for (long i = 0; i < count; i++) {
        DocMenuInfo& info = _info->item_ref(i);
        if (tag == nil || strcmp(tag, info._tag) == 0) {
            _menu->item(i)->state()->set(TelltaleState::is_chosen, choose);
        }
        if (info._menu != nil) {
            info._menu->choose(tag, choose);
        }
    }
}

void DocMenu::enable (const char* tag, boolean enable) {
    long count = _info->count();
    for (long i = 0; i < count; i++) {
        DocMenuInfo& info = _info->item_ref(i);
        if (tag == nil || strcmp(tag, info._tag) == 0) {
            _menu->item(i)->state()->set(TelltaleState::is_enabled, enable);
        }
        if (info._menu != nil) {
            info._menu->enable(tag, enable);
        }
    }
}

DocMenubar::DocMenubar (
    DocumentViewer* viewer, const char* name
) : DocMenu(viewer, name, WidgetKit::instance()->menubar()) { }

DocMenubar::~DocMenubar () { }

DocPopup::DocPopup (
    DocumentViewer* viewer, const char* name
) : DocMenu(viewer, name, WidgetKit::instance()->pulldown()) { }

DocPopup::~DocPopup () { }
