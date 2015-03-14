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
 * Keymap for doc
 */

#include "Keymap.h"

#include "Document.h"
#include "DocViewer.h"
#include "ItemView.h"

#include <InterViews/event.h>
#include <IV-2_6/InterViews/world.h>
#include <OS/list.h>
#include <string.h>

class DocKeymapInfo {
public:
    long _key;
    char* _command;
};

declareList(DocKeymapInfo_List,DocKeymapInfo)
implementList(DocKeymapInfo_List,DocKeymapInfo)

DocKeymap::DocKeymap (DocumentViewer* viewer, const char* name) {
    _viewer = viewer;
    _info = new DocKeymapInfo_List();
    Document* document = viewer->document();
    const char* def = World::current()->property_value(name);
    boolean done = def == nil;
    while (!done) {
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
        char* key;
        if (*p == '<') {
            key = p + 1;
            p = strchr(key, '>');
            *p = '\0';
            ++p;
        } else {
            key = "";
        }
        char* command;
        while (*p == ' ' || *p == '\t') ++p;
        if (*p == '(') {
            command = p + 1;
            p = strchr(command, ')');
            *p = '\0';
            ++p;
        } else {
            command = "";
        }

        DocKeymapInfo info;
        info._key = document->parse_text(key);
        info._command = strcpy(new char[strlen(command) + 1], command);
        _info->append(info);
    }
}

DocKeymap::~DocKeymap () {
    while (_info->count() > 0) {
        DocKeymapInfo& info = _info->item_ref(0);
        delete info._command;
        _info->remove(0);
    }
    delete _info;
}

boolean DocKeymap::map (Event& e) {
    char s[1];
    if (
        _viewer != nil
        && e.type() == Event::key && e.mapkey(s, sizeof(s)) > 0
    ) {
        _viewer->ref();
        ItemView* view = _viewer->focus();
        long count = _info->count();
        int key = s[0];
        if (key < 0) {
            key += 256;
        }
        for (long i = 0; i < count; ++i) {
            DocKeymapInfo& info = _info->item_ref(i);
            if (info._key == key) {
                boolean pending_repair;
                if (view != nil) {
                    pending_repair = view->command(info._command);
                } else {
                    pending_repair = _viewer->command(info._command);
                }
                if (pending_repair && !e.pending()) {
                    view->repair();
                }
                break;
            }
        }
        _viewer->unref();
        return i < count;
    } else {
        return false;
    }
}
