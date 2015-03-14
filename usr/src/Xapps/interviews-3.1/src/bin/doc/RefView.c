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
 * RefView
 */

#include "RefView.h"

#include "Document.h"
#include "RefItem.h"

#include "doc-listener.h"

#include <InterViews/layout.h>
#include <InterViews/patch.h>

#include <string.h>

RefView::RefView (
    DocumentViewer* viewer, ItemView* parent, RefItem* ref
) : ItemView(viewer, parent) {
    _ref = ref;
    _ref->attach(this);
    _ref->ref();
    _patch = new Patch(nil);
    _listener->body(_patch);
}

RefView::~RefView () {
    _ref->detach(this);
    _ref->unref();
}

void RefView::update () {
    const char* text = _ref->text();
    long style = _ref->style();
    if (text != nil) {
        _patch->redraw();
        int l = strlen(text);
        Glyph* box = LayoutKit::instance()->hbox_first_aligned(l);
        for (int i = 0; i < l; ++i) {
            box->append(_ref->document()->character(text[i], style));
        }
        _patch->body(box);
        _patch->reallocate();
        _patch->redraw();
    }
}
