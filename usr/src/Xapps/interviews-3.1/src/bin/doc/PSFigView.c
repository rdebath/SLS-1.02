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
 * PSFigView
 */

#include "PSFigView.h"

#include "Application.h"
#include "DocViewer.h"
#include "PSFigItem.h"

#include "doc-listener.h"
#include "properties.h"

#include <InterViews/layout.h>
#include <InterViews/patch.h>
#include <IV-2_6/InterViews/world.h>
#include <InterViews/xymarker.h>

#include <string.h>

PSFigView::PSFigView (
    DocumentViewer* viewer, ItemView* parent, PSFigItem* figure
) : ItemView(viewer, parent) {
    _figure = figure;
    _figure->ref();
    _figure->attach(this);
    const char* init_mode = World::current()->property_value(INIT_PSFIG_MODE);
    if (init_mode != nil && strcmp(init_mode, "draft") == 0) {
        _mode = PSDraft;
    } else if (init_mode != nil && strcmp(init_mode, "final") == 0) {
        _mode = PSFinal;
    } else {
        _mode = PSDraft;
    }
    _patch = new Patch(nil);
    const Color* overlay;
    const Color* underlay;
    _viewer->highlight_colors(ACTIVE_HIGHLIGHT_COLOR, overlay, underlay);
    _active_marker = new XYMarker(_patch, overlay, underlay);
    _listener->body(_active_marker);
    _listener->button(true, Event::left);
    _active = false;
}

PSFigView::~PSFigView () {
    if (_active) {
        _viewer->focus(nil);
    }
    _figure->detach(this);
    _figure->unref();
}

void PSFigView::graphic_changed () {
    build();
}

void PSFigView::update () {
    _patch->redraw();
    _patch->reallocate();
    _patch->redraw();
    mark_selection();
}

void PSFigView::build () {
    _patch->body(
	LayoutKit::instance()->margin(_figure->graphic(_mode, this), 5, 5)
    );
}

void PSFigView::repair () {
    _figure->notify();
}

boolean PSFigView::command (const char* command) {
    if (strncmp(command, "graphic", 7) == 0) {
        const char* keyword = command+8;
        if (strcmp(keyword, "draft") == 0) {
            if (_mode != PSDraft) {
                _mode = PSDraft;
                build();
                update();
                return false;
            }
        } else if (strcmp(keyword, "final") == 0) {
            if (_mode != PSFinal) {
                _mode = PSFinal;
                build();
                update();
                return false;
            }
        } else if (strcmp(keyword, "reread") == 0) {
            _figure->change_graphic();
            return true;
        } else if (strcmp(keyword, "modify") == 0) {
            const char* old_params = _figure->parameters();
            const char* new_params = _viewer->application()->ask(
                _viewer, "Modify figure paramters:", old_params
            );
            if (new_params != nil) {
                _figure->parameters(new_params);
                return true;
            } else {
                return false;
            }
        }
    }
    return ItemView::command(command);
}

void PSFigView::mark_selection () {
    if (_active) {
        const Allocation& a = _patch->allocation();
        _active_marker->mark(a.left(), a.bottom(), a.right(), a.top());
    } else {
        _active_marker->unmark();
    }
}

void PSFigView::activate (boolean active) {
    if (_active != active) {
        _active = active;
        if (_active) {
            World* w = World::current();
            w->flush();
            _viewer->menubar(w->property_value(DEFAULT_PSFIG_MENUBAR));
            _viewer->keymap(w->property_value(DEFAULT_PSFIG_KEYMAP));
            w->flush();
        }
        mark_selection();
    }
}
