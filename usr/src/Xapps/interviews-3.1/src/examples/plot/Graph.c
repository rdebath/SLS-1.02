/*
 * Graph
 */

#include "Axis.h"
#include "Graph.h"
#include "Plot.h"

#include <stdlib.h>
#include <InterViews/background.h>
#include <InterViews/brush.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/hit.h>
#include <InterViews/event.h>
#include <InterViews/geometry.h>
#include <InterViews/patch.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/target.h>
#include <InterViews/xymarker.h>
#include <InterViews/layout.h>
#include <InterViews/window.h>
#include <IV-look/kit.h>
#include <OS/string.h>
#include <stream.h>

const Color* color (const char* name) {
    String v;
    Session* session = Session::instance();
    Style* style = session->style();
    WidgetKit& kit = *WidgetKit::instance();
    style->find_attribute(name, v);

    ColorIntensity r, g, b;
    if (
        v.string() != nil &&
        Color::find(session->default_display(), v, r, g, b)
    ) {
        return new Color(r, g, b);
    } else {
        return kit.foreground();
    }
}

static const int buf_size = 1000;

Graph::Graph (
    float w, float h, float x_begin, float x_end,
    float y_begin, float y_end, const Color* bg, const char* symbol
) : InputHandler(nil, Session::instance()->style()) {
    _init = false;
    _current = -1;
    _count = 0;
    _w = w;
    _h = h;
    _x_origin = x_begin;
    _y_origin = y_begin;
    _x_range = x_end - x_begin;
    _y_range = y_end - y_begin;
    _x = new Coord[buf_size];
    _y = new Coord[buf_size];

    _plot = new Plot(_count, _x, _y, color("plot_color"), new Brush(2));
    _ppatch = new Patch(_plot);
    _marker = new XYMarker(_ppatch, nil, color("mark_color"));
    _xaxis = new XAxis(x_begin, x_end);
    _xpatch = new Patch(_xaxis);
    _yaxis = new YAxis(y_begin, y_end);
    _ypatch = new Patch(_yaxis);
    _symbol = new String(symbol);

    LayoutKit* kit = LayoutKit::instance();
    body(
        new Background(
            kit->margin(
                kit->vbox(
                    kit->hbox(
                        _ypatch,
                        //_yaxis,
                        new Target(
                            _marker, TargetPrimitiveHit
                        )
                    ),
                    _xpatch
                    //_xaxis
                ), 35, 15, 25, 15
            ), bg
        )
    );
}

Graph::~Graph () {
    delete _x;
    delete _y;
    delete _symbol;
}

void Graph::request (Requisition& r) const {
    MonoGlyph::request(r);
    Requirement& rx = r.x_requirement();
    Requirement& ry = r.y_requirement();
    rx.stretch(fil);
    rx.shrink(0.0);
    rx.alignment(0.0);
    rx.natural(_w);
    ry.stretch(fil);
    ry.shrink(0.0);
    ry.alignment(1.0);
    ry.natural(_h);
}

void Graph::AddPt (Coord x, Coord y) {
    if (!_init) {
        _init = true;
        _x_origin = x;
        _y_origin = y;
        _xaxis->Range(_x_origin, _x_origin+_x_range);
        _yaxis->Range(_y_origin, _y_origin+_y_range);
        _xpatch->reallocate();
        _xpatch->redraw();
        _ypatch->reallocate();
        _ypatch->redraw();
    }
    if (_count >= buf_size) {
        cerr << "Too many data points.\n";
        exit(1);
    }
    Coord x_last = _x_origin + _x_range;
    Coord y_last = _y_origin + _y_range;
    int i;
    if (x > x_last || x < _x_origin) {
        Coord xorig[buf_size];
        for (i = 0; i < _count; i++) {
            xorig[i] = _x[i]*_x_range+_x_origin;
        }
        if (x < _x_origin) {
            _x_origin -= (x_last-x)*1.333;
            _x_range = x_last-_x_origin;
        } else {
            _x_range += (x-_x_origin)*.1333;
        }
        for (i = 0; i < _count; i++) {
            _x[i] = (xorig[i]-_x_origin)/_x_range;
        }
        _xaxis->Range(_x_origin, _x_origin+_x_range);
        _xpatch->reallocate();
        _xpatch->redraw();
    }                
    if (y > y_last || y < _y_origin) {
        Coord yorig[buf_size];
        for (i = 0; i < _count; i++) {
            yorig[i] = _y[i]*_y_range+_y_origin;
        }
        if (y < _y_origin) {
            _y_origin -= (y_last-y)*1.333;
            _y_range = y_last-_y_origin;
        } else {
            _y_range += (y-_y_origin)*.1333;
        }
        for (i = 0; i < _count; i++) {
            _y[i] = (yorig[i]-_y_origin)/_y_range;
        }
        _yaxis->Range(_y_origin, _y_origin+_y_range);
        _ypatch->reallocate();
        _ypatch->redraw();
    }                
    _x[_count] = (x-_x_origin)/_x_range;
    _y[_count] = (y-_y_origin)/_y_range;
    _count++;
    _ppatch->redraw();
}

void Graph::mark (int i) {
    _current = i;
    if (_current >= 0) {
        const Allocation& a = _ppatch->allocation();
        Coord l = a.left(); Coord r = a.right();
        Coord b = a.bottom(); Coord t = a.top();
        Coord x = l * (1 - _x[_current]) + r * _x[_current];
        Coord y = b * (1 - _y[_current]) + t * _y[_current];
        _marker->mark(x - 4, y - 4, x + 4, y + 4);
    } else {
        _marker->unmark();
    }
}

void Graph::allocate (Canvas* c, const Allocation& a, Extension& ext) {
    InputHandler::allocate(c, a, ext);
    mark(_current);
}

void Graph::press (const Event& e) {
    if (e.type() == Event::down) {
        Hit h(&e);
        _ppatch->repick(0, h);
        if (h.any()) {
            mark((int)h.index(0));
        } else {
            mark(-1);
        }
    }
}
