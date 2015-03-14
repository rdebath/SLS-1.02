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
 * TabularView
 */

#include "TabularView.h"

#include "Document.h"
#include "DocViewer.h"
#include "TabularItem.h"
#include "TextItem.h"

#include "doc-listener.h"
#include "doc-target.h"
#include "properties.h"

#include <InterViews/aggr.h>
#include <InterViews/group.h>
#include <InterViews/hit.h>
#include <InterViews/layout.h>
#include <InterViews/patch.h>
#include <InterViews/rule.h>
#include <IV-2_6/InterViews/world.h>
#include <InterViews/xymarker.h>
#include <OS/math.h>
#include <string.h>

const char*  CELL_MARGIN = "cellmargin";
const char* TABULAR_MARGIN = "tabularmargin";
const char* TABULAR_SEPARATOR_THICKNESS = "tabularseparatorthickness";

TabularView::TabularView (
    DocumentViewer* viewer, ItemView* parent, TabularItem* tabular
) : ItemView(viewer, parent) {
    _tabular = tabular;
    _tabular->ref();
    _tabular->attach(this);
    _cells = nil;
    _rows = nil;
    _row_patch = nil;
    _columns = nil;
    _column_patch = nil;
    _table = nil;
    _patch = new Patch(nil);
    const Color* overlay;
    const Color* underlay;
    _viewer->highlight_colors(SELECT_HIGHLIGHT_COLOR, overlay, underlay);
    _marker = new XYMarker(_patch, overlay, underlay);
    _listener->body(
        LayoutKit::instance()->margin(
            _marker, _viewer->document()->document_metric(TABULAR_MARGIN)
        )
    );
    _listener->button(true, Event::left);
    _active = false;
    dot(0, 0);
}

TabularView::~TabularView () {
    if (_active) {
        _viewer->focus(nil);
    }
    if (_cells != nil) {
        _cells->unref();
    }
    if (_rows != nil) {
        _rows->unref();
    }
    if (_row_patch != nil) {
        _row_patch->unref();
    }
    if (_columns != nil) {
        _columns->unref();
    }
    if (_column_patch != nil) {
        _column_patch->unref();
    }
    if (_table != nil) {
        _table->unref();
    }
    _tabular->detach(this);
    _tabular->unref();
}

void TabularView::dot (long row, long column) { 
    if (row >= 0L) {
        _row_dot = Math::min(Math::max(row, 0L), _tabular->row_count());
        _row_mark = _row_dot;
    }
    if (column >= 0) {
        _column_dot = Math::min(
	    Math::max(column, 0L), _tabular->column_count()
	);
        _column_mark = _column_dot;
    }
}

void TabularView::mark (long row, long column) {
    if (row >= 0L) {
        _row_mark = Math::min(Math::max(row, 0L), _tabular->row_count());
    }
    if (column >= 0L) {
        _column_mark = Math::min(
	    Math::max(column, 0L), _tabular->column_count()
	);
    }
}

void TabularView::repair () {
    _tabular->notify();
}

void TabularView::update () {
    mark_selection();
}

void TabularView::mark_selection () {
    if (_active) {
        long cd = Math::min(_column_dot, _column_mark);
        long cm = Math::max(_column_dot, _column_mark);
        long rd = Math::min(_row_dot, _row_mark);
        long rm = Math::max(_row_dot, _row_mark);
	Allotment cdx, cmx, rmy, rdy;
        _columns->allotment(cd*2, Dimension_X, cdx);
        _columns->allotment(cm*2, Dimension_X, cmx);
        _rows->allotment(rm*2, Dimension_Y, rmy);
        _rows->allotment(rd*2, Dimension_Y, rdy);
	Coord l = cdx.begin();
	Coord r = cmx.end();
	Coord b = rmy.begin();
	Coord t = rdy.end();
        _marker->mark(l, b, r, t);
    } else {
        _marker->unmark();
    }
}

void TabularView::rebuild () {
    const LayoutKit& layout = *LayoutKit::instance();
    float cell_margin = _viewer->document()->document_metric(CELL_MARGIN);
    float separator_thickness = _viewer->document()->document_metric(
        TABULAR_SEPARATOR_THICKNESS
    );
    long rows = _tabular->row_count();
    long columns = _tabular->column_count();
    if (_cells != nil) {
        _cells->unref();
    }
    _cells = new Aggregate(rows * columns);
    _cells->ref();
    if (_columns != nil) {
        _columns->unref();
    }
    _columns = layout.hbox_first_aligned();
    _columns->ref();
    if (_column_patch != nil) {
        _column_patch->unref();
    }
    _column_patch = new Patch(_columns);
    _column_patch->ref();
    if (_rows != nil) {
        _rows->unref();
    }
    _rows = layout.vbox_first_aligned();
    _rows->ref();
    if (_row_patch != nil) {
        _row_patch->unref();
    }
    _row_patch = new Patch(_rows);
    _row_patch->ref();
    if (_table != nil) {
        _table->unref();
    }
    _table = layout.overlay(
        layout.vcenter(_column_patch, 0.0),
        layout.vcenter(_row_patch, 0.0),
        _cells
    );
    _table->ref();
    long r, c;
    for (r = 0; r < rows; ++r) {
        for (c = 0; c < columns; ++c) {
            Item* item = _tabular->cell(r, c);
            Glyph* g;
            if (item != nil) {
                g = item->view(this, _viewer);
                switch (_tabular->column_alignment(c)) {
                case ColumnAlignLeft:
                    g = layout.hcenter(
                        layout.h_margin(
			    g, cell_margin, 0, 0, cell_margin, fil, 0
			),
                        0.0
                    );
                    break;
                case ColumnAlignCenter:
                    g = layout.hcenter(
                        layout.h_margin(
			    g, cell_margin, fil, 0, cell_margin, fil, 0
			),
                        0.5
                    );
                    break;
                case ColumnAlignRight:
                    g = layout.hcenter(
                        layout.h_margin(
			    g, cell_margin, fil, 0, cell_margin, 0, 0
			),
                        1.0
                    );
                    break;
                default:
                    break;
                }
            } else {
                g = nil;
            }
            _cells->insert(r * columns + c, g);
        }
    }
    const Color* fg = World::current()->foreground();
    for (c = 0; c < columns; ++c) {
        if (_tabular->column_separator(c) == ColumnSeparatorSingle) {
            _columns->append(new VRule(fg, separator_thickness));
        } else {
            _columns->append(nil);
        }
        Group* group = new Group(_cells, Dimension_X);
        for (r = 0; r < rows; ++r) {
            group->map(r * columns + c);
        }
        _columns->append(new DocTarget(group));
    }
    if (_tabular->column_separator(c) == ColumnSeparatorSingle) {
        _columns->append(new VRule(fg, separator_thickness));
    } else {
        _columns->append(nil);
    }
    for (r = 0; r < rows; ++r) {
        if (_tabular->row_separator(r) == RowSeparatorSingle) {
            _rows->append(new HRule(fg, separator_thickness));
        } else {
            _rows->append(nil);
        }
        Group* group = new Group(_cells, Dimension_Y);
        for (c = 0; c < columns; ++c) {
            group->map(r * columns + c);
        }
        _rows->append(group);
    }
    if (_tabular->row_separator(r) == RowSeparatorSingle) {
        _rows->append(new HRule(fg, separator_thickness));
    } else {
        _rows->append(nil);
    }
    _patch->body(_table);
    mark_selection();
}

void TabularView::cell_replaced (long, long) {
    rebuild();
}

void TabularView::cell_changed (long r, long c) {
    long columns = _tabular->column_count();
    _cells->change(r * columns + c);
    _rows->change(r * 2 + 1);
    _columns->change(c * 2 + 1);
    _table->change(0);
    _table->change(1);
    _table->change(2);
    _patch->reallocate();
    mark_selection();
}

void TabularView::row_inserted (long) {
    rebuild();
}

void TabularView::row_removed (long) {
    rebuild();
}

void TabularView::column_inserted (long) {
    rebuild();
}

void TabularView::column_removed (long) {
    rebuild();
}

void TabularView::row_separator_changed (long) {
    rebuild();
}

void TabularView::column_separator_changed (long) {
    rebuild();
}

void TabularView::activate (boolean active) {
    if (_active != active) {
        _active = active;
        if (_active) {
            World* w = World::current();
            w->flush();
            _viewer->menubar(w->property_value(DEFAULT_TABULAR_MENUBAR));
            _viewer->keymap(w->property_value(DEFAULT_TABULAR_KEYMAP));
            w->flush();
        }
        mark_selection();
    }
}

boolean TabularView::command (const char* command) {
    if (strcmp(command, "new row") == 0) {
        _tabular->insert_row(_row_dot, "<Cell>");
        dot(_row_dot, 0);
        mark(_row_dot + 1, _tabular->column_count());
        return true;
    } else if (strcmp(command, "new left column") == 0) {
        _tabular->insert_column(_column_dot, "<Cell>", ColumnAlignLeft);
        dot(0, _column_dot);
        mark(_tabular->row_count(), _column_dot + 1);
        return true;
    } else if (strcmp(command, "new center column") == 0) {
        _tabular->insert_column(_column_dot, "<Cell>", ColumnAlignCenter);
        dot(0, _column_dot);
        mark(_tabular->row_count(), _column_dot + 1);
        return true;
    } else if (strcmp(command, "new right column") == 0) {
        _tabular->insert_column(_column_dot, "<Cell>", ColumnAlignRight);
        dot(0, _column_dot);
        mark(_tabular->row_count(), _column_dot + 1);
        return true;
    } else if (strcmp(command, "hide column separator") == 0) {
        _tabular->change_column_separator(_column_dot, ColumnSeparatorOff);
        return true;
    } else if (strcmp(command, "show column separator") == 0) {
        _tabular->change_column_separator(_column_dot, ColumnSeparatorSingle);
        return true;
    } else if (strcmp(command, "hide row separator") == 0) {
        _tabular->change_row_separator(_row_dot, RowSeparatorOff);
        return true;
    } else if (strcmp(command, "show row separator") == 0) {
        _tabular->change_row_separator(_row_dot, RowSeparatorSingle);
        return true;
    } else if (strncmp(command, "go", 2) == 0) {
        const char* keyword = command + 3;
        if (strcmp(keyword, "forward_row") == 0) {
            dot(_row_dot+1, _column_dot);
        } else if (strcmp(keyword, "forward_column") == 0) {
            dot(_row_dot, _column_dot+1);
        } else if (strcmp(keyword, "backward_row") == 0) {
            dot(_row_dot-1, _column_dot);
        } else if (strcmp(keyword, "backward_column") == 0) {
            dot(_row_dot, _column_dot-1);
        }
        mark_selection();
        return false;
    } else {
        return ItemView::command(command);
    }
}

long TabularView::row_hit (Coord x, Coord y) {
    Hit hit(x, y);
    _row_patch->repick(0, hit);
    if (hit.any()) {
        GlyphIndex result = hit.index(0);
        if (hit.depth() > 0) {
            result += hit.index(1);
        }
        return result/2;
    } else {
        return -1;
    }
}

long TabularView::column_hit (Coord x, Coord y) {
    Hit hit(x, y);
    _column_patch->repick(0, hit);
    if (hit.any()) {
        GlyphIndex result = hit.index(0);
        if (hit.depth() > 0) {
            result += hit.index(1);
        }
        return result/2;
    } else {
        return -1;
    }
}

void TabularView::keystroke (Event&) {
    ;
}

void TabularView::select (Event& e) {
    if (!e.shift_is_down()) {
        dot(
            row_hit(e.pointer_x(), e.pointer_y()),
            column_hit(e.pointer_x(), e.pointer_y())
        );
    }
    do {
        if (!e.pending()) {
            mark(
                row_hit(e.pointer_x(), e.pointer_y()),
                column_hit(e.pointer_x(), e.pointer_y())
            );
            update();
        }
        e.read();
    } while (e.type() != Event::up);
}
