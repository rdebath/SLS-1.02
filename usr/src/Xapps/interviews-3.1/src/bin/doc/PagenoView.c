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
 * PagenumberView
 */

#include "PagenoView.h"

#include "Document.h"
#include "DocViewer.h"
#include "PagenoItem.h"

#include "doc-listener.h"

#include <InterViews/layout.h>
#include <InterViews/patch.h>

#include <string.h>

PagenumberView::PagenumberView (
    DocumentViewer* viewer, ItemView* parent, PagenumberItem* pagenumber
) : ItemView(viewer, parent) {
    _pagenumber = pagenumber;
    _pagenumber->attach(this);
    _pagenumber->ref();
    const char* sample = _pagenumber->sample();
    int l = strlen(sample);
    long style = _pagenumber->style();
    Glyph* box = LayoutKit::instance()->hbox(l);
    for (int i = 0; i < l; ++i) {
        box->append(_pagenumber->document()->character(sample[i], style));
    }
    Requisition r;
    box->request(r);
    _width = r.requirement(Dimension_X).natural();
    _label = nil;
    _patch = new Patch(box);
    _listener->body(_patch);
}

PagenumberView::~PagenumberView () {
    _pagenumber->detach(this);
    _pagenumber->unref();
    delete _label;
}

void PagenumberView::draw (Canvas* c, const Allocation& allocation) const {
    const char* label = _viewer->current_page_label();
    if (_label == nil || strcmp(label, _label) != 0) {
	PagenumberView* p = (PagenumberView*)this;
        delete p->_label;
        p->_label = strcpy(new char[strlen(label)+1], label);
        int l = strlen(label);
        long style = p->_pagenumber->style();
	const LayoutKit& layout = *LayoutKit::instance();
        Glyph* box = layout.hbox(l);
        for (int i = 0; i < l; ++i) {
            box->append(_pagenumber->document()->character(label[i], style));
        }
	p->_patch->undraw();
        p->_patch->body(
            layout.h_fixed_span(
                layout.h_margin(box, 0, fil, fil, 0, fil, fil), p->_width
            )
        );
        p->_patch->reallocate();
    }
    ItemView::draw(c, allocation);
}

void PagenumberView::print (Printer* printer, const Allocation& a) const {
    const char* label = _viewer->current_page_label();
    if (_label == nil || strcmp(label, _label) != 0) {
	PagenumberView* p = (PagenumberView*)this;
        delete p->_label;
        p->_label = strcpy(new char[strlen(label)+1], label);
        int l = strlen(label);
        long style = p->_pagenumber->style();
	const LayoutKit& layout = *LayoutKit::instance();
        Glyph* box = layout.hbox(l);
        for (int i = 0; i < l; ++i) {
            box->append(_pagenumber->document()->character(label[i], style));
        }
	p->_patch->undraw();
        p->_patch->body(
            layout.h_fixed_span(
                layout.h_margin(box, 0, fil, fil, 0, fil, fil), p->_width
            )
        );
        p->_patch->reallocate();
    }
    ItemView::print(printer, a);
}
