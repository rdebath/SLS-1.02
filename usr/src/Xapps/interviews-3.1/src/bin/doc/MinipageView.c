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
 * MinipageView
 */

#include "MinipageView.h"

#include "TextItem.h"

#include "Document.h"
#include "DocViewer.h"
#include "InsertMarker.h"

#include "doc-composition.h"
#include "doc-listener.h"
#include "doc-target.h"
#include "properties.h"

#include <InterViews/hit.h>
#include <InterViews/layout.h>
#include <InterViews/lrmarker.h>
#include <InterViews/patch.h>
#include <InterViews/texcomp.h>
#include <InterViews/xymarker.h>
#include <OS/math.h>
#include <string.h>

MinipageView::MinipageView (
    DocumentViewer* viewer, ItemView* parent, TextItem* text
) : TextView(viewer, parent, text) {
    Document* document = _text->document();
    _lines = LayoutKit::instance()->vbox_first_aligned();
    Coord format_width = Coord(_text->format_width());
    if (format_width == 0) {
        format_width = fil;
    }
    _characters = new LRComposition(
        _lines,
        new TeXCompositor(int(document->document_metric("linepenalty"))),
        nil, format_width, _text->item_count()
    );
    _characters->append(nil);
    _characters->view(0, 0);
    _text_patch = new Patch(_characters);
    const Color* ol;
    const Color* ul;
    _viewer->highlight_colors(SELECT_HIGHLIGHT_COLOR, ol, ul);
    _select_marker = new LRMarker(_text_patch, ol, ul);
    _viewer->highlight_colors(INSERT_HIGHLIGHT_COLOR, ol, ul);
    _insert_marker = new InsertMarker(
        _select_marker, ol, ul, _viewer->insert_flash()
    );
    _viewer->highlight_colors(ACTIVE_HIGHLIGHT_COLOR, ol, ul);
    _active_marker = new XYMarker(_insert_marker, ol, ul);
    _listener->body(_active_marker);
    _listener->button(true, Event::left);
}

MinipageView::~MinipageView () { }

void MinipageView::update () {
    _characters->repair();
    _text_patch->reallocate();
    mark_selection();
}

void MinipageView::item_changed (long index, long count) {
    Document* document = _text->document();
    for (long i = index; i < index + count; ++i) {
        Item* item = _text->item(i);
        Glyph* g;
        if (item == nil) {
            g = document->character(_text->item_code(i), _text->item_style(i));
        } else {
            g = _characters->component(i);
        }
        g->ref();
        _characters->replace(i, nil);
        _characters->replace(i, g);
        g->unref();
    }
    TextView::item_changed(index, count);
}

void MinipageView::item_replaced (long index, long count) {
    Document* document = _text->document();
    for (long i = index; i < index + count; ++i) {
        Item* item = _text->item(i);
        Glyph* g;
        if (item == nil) {
            g = document->character(_text->item_code(i), _text->item_style(i));
        } else {
            g = new DocTarget(item->view(this, _viewer));
        }
        g->ref();
        _characters->replace(i, nil);
        _characters->replace(i, g);
        g->unref();
    }
    TextView::item_replaced(index, count);
}

void MinipageView::item_inserted (long index, long count) {
    Document* document = _text->document();
    for (long i = index; i < index + count; ++i) {
        Item* item = _text->item(i);
        Glyph* g;
        if (item == nil) {
            g = document->character(_text->item_code(i), _text->item_style(i));
        } else {
            g = new DocTarget(item->view(this, _viewer));
        }
        _characters->insert(i, g);
    }
    TextView::item_inserted(index, count);
}

void MinipageView::item_removed (long index, long count) {
    for (long i = index; i < index + count; ++i) {
        _characters->remove(index);
    }
    TextView::item_removed(index, count);
}

long MinipageView::index (Coord x, Coord y) {
    Hit hit(x, y);
    _text_patch->repick(0, hit);
    if (hit.any()) {
        GlyphIndex result = hit.index(0);
        if (hit.depth() > 0) {
            result += hit.index(1);
        }
        return result;
    } else {
        return -1;
    }
}

void MinipageView::mark_selection () {
    long dot = Math::min(_dot, _mark);
    long mark = Math::max(_dot, _mark);
    dot = Math::min(_text->item_count(), dot);
    mark = Math::min(_text->item_count(), mark);
    long dot_line = _characters->item(dot);
    long mark_line = _characters->item(mark);

    Allotment dot_x, dot_line_y, mark_x, mark_line_y;
    _characters->allotment(dot, Dimension_X, dot_x);
    _lines->allotment(dot_line, Dimension_Y, dot_line_y);
    _characters->allotment(mark, Dimension_X, mark_x);
    _lines->allotment(mark_line, Dimension_Y, mark_line_y);

    Coord x1 = dot_x.begin();
    Coord y1 = dot_line_y.begin();
    Coord h1 = dot_line_y.end() - y1;
    Coord x2 = mark_x.begin();
    Coord y2 = mark_line_y.begin();
    Coord h2 = mark_line_y.end() - y2;

    const Allocation& a = _text_patch->allocation();
    if (_active && _dot != _mark) {
        _select_marker->bound(a.left(), a.bottom(), a.right(), a.top());
        _select_marker->mark(x1, y1, h1, x2, y2, h2);
    } else {
        _select_marker->unmark();
    }
    if (_active && _dot == _mark) {
        _insert_marker->mark(x1, y1, 1, h1);
    } else {
        _insert_marker->unmark();
    }
    if (_active) {
        _active_marker->mark(a.left(), a.bottom(), a.right(), a.top());
    } else {
        _active_marker->unmark();
    }
}

boolean MinipageView::command (const char* command) {
    boolean done = false;
    if (strncmp(command, "go", 2) == 0) {
        long d = Math::min(_dot, _mark);
        long m = Math::max(_dot, _mark);
        if (strcmp(command+3, "forward_line") == 0) {
            dot(_characters->beginning_of(_characters->item(m)+2));
            done = true;
        } else if (strcmp(command+3, "backward_line") == 0) {
            dot(_characters->beginning_of(_characters->item(d)-2));
            done = true;
        }
    }
    if (done) {
        return true;
    } else {
        return TextView::command(command);
    }
}

void MinipageView::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    if (c == nil) {
        _characters->view(0, 0);
    } else {
        _characters->view(-1, _characters->count());
        _characters->repair();
        _text_patch->reallocate();
    }
    TextView::allocate(c, a, ext);
}
