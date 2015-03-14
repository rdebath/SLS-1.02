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
 * PagingView
 */

#include "PagingView.h"

#include "Command.h"
#include "Document.h"
#include "DocViewer.h"
#include "InsertMarker.h"
#include "TextItem.h"

#include "codes.h"
#include "doc-composition.h"
#include "doc-listener.h"
#include "doc-target.h"
#include "properties.h"

#include <InterViews/arraycomp.h>
#include <InterViews/background.h>
#include <InterViews/hit.h>
#include <InterViews/layout.h>
#include <InterViews/lrmarker.h>
#include <InterViews/patch.h>
#include <InterViews/simplecomp.h>
#include <InterViews/texcomp.h>
#include <IV-2_6/InterViews/world.h>
#include <OS/math.h>
#include <stdio.h>
#include <string.h>

PagingView::PagingView (
    DocumentViewer* viewer, ItemView* parent, TextItem* text, Glyph* pages
) : TextView(viewer, parent, text) {
    Document* document = _text->document();
    Coord textwidth = document->document_metric("textwidth");
    Coord textheight = document->document_metric("textheight");
    Coord gutter = document->document_metric("gutter");

    Coord format_width = Coord(_text->format_width());
    if (format_width == 0) {
        format_width = fil;
    }
    _column_count = long(document->document_metric("columns"));

    const LayoutKit& layout = *LayoutKit::instance();

    _columns = new LRComposition(
        pages,
        new ArrayCompositor(2 * _column_count),
        layout.hstrut(textwidth, 0, 0, 0, 0),
        textwidth, 20
    );
    _lines = new TBComposition(
        _columns,
        new TeXCompositor(int(document->document_metric("pagepenalty"))),
        layout.discretionary(
            0,
            layout.vstrut(0, textheight, gutter, 0, 0),
            layout.vstrut(0, textheight, 0, 0, 0), nil, nil
        ),
        textheight, _text->item_count() / 30
    );
    _lines->view(0, 0);
    _characters = new LRComposition(
        _lines,
        new TeXCompositor(int(document->document_metric("linepenalty"))),
        nil,
        format_width, _text->item_count()
    );
    _characters->append(_text->document()->character(newpage, 1));
    _characters->view(0, 0);
    _text_patch = new Patch(_characters);

    Glyph* g = _text_patch;
    _select_marker = new LRMarker*[_column_count];
    const Color* ol;
    const Color* ul;
    _viewer->highlight_colors(SELECT_HIGHLIGHT_COLOR, ol, ul);
    for (long c = 0; c < _column_count; ++c) {
        _select_marker[c] = new LRMarker(g, ol, ul);
        g = _select_marker[c];
    }

    _viewer->highlight_colors(INSERT_HIGHLIGHT_COLOR, ol, ul);
    _insert_marker = new InsertMarker(g, ol, ul, _viewer->insert_flash());

    _listener->body(_insert_marker);
    _listener->button(true, Event::left);
    _current_page = 0;
}

PagingView::~PagingView () { }

void PagingView::reshaped () {
    update();
}

long PagingView::page_containing (long index) {
    return _columns->item(_lines->item(_characters->item(index)));
}

void PagingView::view_page (long page) {
    _current_page = page;
    long first_column = _columns->beginning_of(page);
    long last_column = _columns->end_of(page);
    long first_line = _lines->beginning_of(first_column);
    long last_line = _lines->end_of(last_column);
    long first_char = _characters->beginning_of(first_line);
    long last_char = _characters->end_of(last_line);
    _lines->view(first_line, last_line);
    _characters->view(first_char - 1, last_char + 1);
    _text_patch->reallocate();
    mark_selection();
}

void PagingView::item_changed (long index, long count) {
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

void PagingView::item_replaced (long index, long count) {
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

void PagingView::item_inserted (long index, long count) {
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

void PagingView::item_removed (long index, long count) {
    for (long i = index; i < index + count; ++i) {
        _characters->remove(index);
    }
    TextView::item_removed(index, count);
}

long PagingView::index (Coord x, Coord y) {
    Hit hit(x, y);
    _text_patch->repick(0, hit);
    if (hit.any()) {
        long result = hit.index(0);
        if (hit.depth() > 0) {
            result += hit.index(1);
        }
        return result;
    } else {
        return -1;
    }
}

void PagingView::repair () {
    TextView::repair();
    _viewer->page_to_view(dot());
}

void PagingView::update () {
    _characters->repair();
    do {
        _text_patch->reallocate();
        long count = _columns->count();
        for (long i = 0; i < count; ++i) {
            long page = _columns->item(i);
	    Allotment ax, ay;
            _columns->allotment(i, Dimension_X, ax);
            _columns->allotment(i, Dimension_Y, ay);
            Coord l = ax.begin();
            Coord r = ax.end();
            Coord b = ay.begin();
            Coord t = ay.end();
            Coord top = _viewer->top_margin(page, l, b, r, t);
            Coord bottom = _viewer->bottom_margin(page, l, b, r, t);
            _lines->margin(i, top, bottom);
        }
    } while (!_lines->repair() && !_columns->repair());
    _text_patch->reallocate();
    mark_selection();
}

void PagingView::mark_selection () {
    for (long i = 0; i < _column_count; ++i) {
        mark_column(i);
    }
    mark_insertion();
}

void PagingView::mark_insertion () {
    long dot = Math::min(_text->item_count(), _dot);
    long dot_line = _characters->item(dot);
    long dot_column = _lines->item(dot_line);
    long dot_page = _columns->item(dot_column);

    if (_active && _dot == _mark && dot_page == _current_page) {
	Allotment dot_x, dot_line_y;
        _characters->allotment(dot, Dimension_X, dot_x);
        _lines->allotment(dot_line, Dimension_Y, dot_line_y);
	Coord x = dot_x.begin();
	Coord y = dot_line_y.begin();
	Coord h = dot_line_y.end() - y;
        Coord w = 1;
        _insert_marker->mark(x, y, w, h);
    } else {
        _insert_marker->unmark();
    }
}

void PagingView::mark_column (long i) {
    long column = _current_page * _column_count + 2 * i;
    LRMarker* marker = _select_marker[i];

    long dot = Math::min(_dot, _mark);
    long mark = Math::max(_dot, _mark);
    dot = Math::min(_text->item_count(), dot);
    mark = Math::min(_text->item_count(), mark);
    long dot_line = _characters->item(dot);
    long mark_line = _characters->item(mark);
    long dot_column = _lines->item(dot_line);
    long mark_column = _lines->item(mark_line);

    long topline = _lines->beginning_of(column);
    long bottomline = _lines->end_of(column);

    if (column < _columns->count() - 1) {
	Allotment column_x, bottom_y, top_y;
        _columns->allotment(column, Dimension_X, column_x);
        _lines->allotment(bottomline, Dimension_Y, bottom_y);
        _lines->allotment(topline, Dimension_Y, top_y);
	Coord left = column_x.begin();
	Coord bottom = bottom_y.begin();
	Coord right = column_x.end();
	Coord top = top_y.end();
        marker->bound(left, bottom, right, top);
    }
    if (
        !_active || _dot == _mark
        || mark_column < column || dot_column > column
    ) {
        marker->unmark();
    } else {
        Coord x1, y1, h1, x2, y2, h2;
        if (dot_column < column) {
	    Allotment column_x, topline_y;
            _columns->allotment(column, Dimension_X, column_x);
            _lines->allotment(topline, Dimension_Y, topline_y);
	    x1 = column_x.begin();
	    y1 = topline_y.begin();
	    h1 = topline_y.end() - y1;
        } else {
	    Allotment dot_x, dot_line_y;
            _characters->allotment(dot, Dimension_X, dot_x);
            _lines->allotment(dot_line, Dimension_Y, dot_line_y);
	    x1 = dot_x.begin();
	    y1 = dot_line_y.begin();
	    h1 = dot_line_y.end() - y1;
        }
        if (mark_column > column) {
	    Allotment column_x, bottomline_y;
            _columns->allotment(column, Dimension_X, column_x);
            _lines->allotment(bottomline, Dimension_Y, bottomline_y);
	    x2 = column_x.end();
	    y2 = bottomline_y.begin();
	    h2 = bottomline_y.end() - y2;
        } else {
	    Allotment mark_x, mark_line_y;
            _characters->allotment(mark, Dimension_X, mark_x);
            _lines->allotment(mark_line, Dimension_Y, mark_line_y);
	    x2 = mark_x.begin();
	    y2 = mark_line_y.begin();
	    h2 = mark_line_y.end() - y2;
        }
        marker->mark(x1, y1, h1, x2, y2, h2);
    }
}

boolean PagingView::command (const char* command) {
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
        } else if (strcmp(command+3, "forward_page") == 0) {
            long dot_column = _lines->item(_characters->item(d));
            dot(_characters->beginning_of(_lines->beginning_of(dot_column+2)));
            done = true;
        } else if (strcmp(command+3, "backward_page") == 0) {
            long dot_column = _lines->item(_characters->item(d));
            dot(_characters->beginning_of(_lines->beginning_of(dot_column-2)));
            done = true;
        }
    }
    if (done) {
        return true;
    } else {
        return TextView::command(command);
    }
}
