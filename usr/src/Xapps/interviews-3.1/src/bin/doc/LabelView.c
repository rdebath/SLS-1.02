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
 * LabelView
 */

#include "LabelView.h"

#include "Document.h"
#include "LabelItem.h"

#include "doc-listener.h"

#include <InterViews/patch.h>

#include <string.h>

LabelView::LabelView (
    DocumentViewer* viewer, ItemView* parent, LabelItem* label
) : ItemView(viewer, parent) {
    _label = label;
    _label->attach(this);
    _label->ref();
    _patch = new Patch(nil);
    _listener->body(_patch);
}

LabelView::~LabelView () {
    _label->detach(this);
    _label->unref();
}

void LabelView::update () {
    long style = _label->style();
    _patch->body(_label->document()->character(0, style));
    _patch->reallocate();
    _patch->redraw();
}
