#include <InterViews/color.h>
#include <InterViews/font.h>
#include <InterViews/label.h>
#include <InterViews/layout.h>
#include <InterViews/page.h>
#include <InterViews/rule.h>
#include <IV-look/kit.h>
#include <stdio.h>

/*
 * Axis - position tick marks as on axis
 */

#include "Axis.h"

static float xinc = 40.0;
static float yinc = 20.0;
extern const Color* color(const char* name);

static Glyph** make_yticks (
    int count, float first, float last, const char* format,
    const Font* font, const Color* c
) {
    LayoutKit* lkit = LayoutKit::instance();
    char label[100];
    Glyph** tick = new Glyph* [count];
    float step = (count > 1) ? (last - first) / (count - 1) : 0;
    for (int i = 0; i < count; ++i) {
        sprintf(label, format, first + i*step);
        tick[i] = lkit->vcenter(
            lkit->hcenter(
                lkit->r_margin(
                    new Label(label, font, c), 3.0
                ), 1.0
            ), 0.5
        );
    }
    return tick;
}

static Glyph** make_xticks (
    int count, float first, float last, const char* format,
    const Font* font, const Color* c
) {
    LayoutKit* lkit = LayoutKit::instance();
    char label[100];
    Glyph** tick = new Glyph* [count];
    float step = (count > 1) ? (last - first) / (count - 1) : 0;
    for (int i = 0; i < count; ++i) {
        float amt = first + i*step;
        sprintf(label, format, amt);
        tick[i] = lkit->hcenter(
            lkit->vcenter(
            lkit->t_margin(new Label(label, font, c), 3.0), 1.0), 0.5
        );
    }
    return tick;
}

Axis::Axis (
    float first, float last
) : MonoGlyph(nil) {
    _page = nil;
    _first = first;
    _last = last;
}

Axis::~Axis () { }

void Axis::allocate (Canvas* c, const Allocation& a, Extension& ext) {
    _a = a;
    Range(_first, _last);
    _page->allocate(c,  a, ext);
}

YAxis::YAxis (float first, float last) : Axis(first, last) {
    Range(first, last);
}

YAxis::~YAxis () { }

void YAxis::request (Requisition& r) const {
    MonoGlyph::request(r);
    Requirement& rx = r.x_requirement();
    Requirement& ry = r.y_requirement();
    rx.stretch(0.0);
    rx.shrink(0.0);
    rx.alignment(0.0);
    ry.stretch(fil);
    ry.shrink(fil);
    ry.alignment(1.0);
}

void YAxis::Range(float first, float last) {
    Coord b, t;
    WidgetKit& kit = *WidgetKit::instance();

    b = _a.bottom(); t = _a.top();
    _first = first;
    _last = last;
    _page = new Page(new VRule(color("axis_color"), 1));
    int num = (int)((t - b)/yinc);
    num = (num > 2) ? num : 2;
    Glyph** tick = make_yticks(
        num, _first, _last, "%3.2f", kit.font(), color("tick_color")
    );
    for (int i = 0; i < num; i++) {
        _page->insert(i, tick[i]);
    }
    if (num == 1) {
        _page->move(0, 0, 0);
    } else if (num > 1) {
        Coord diff = _a.top()-_a.bottom();
        Coord dy = diff / (num - 1);
        for (int i = 0; i < num; ++i) {
            _page->move(i, 0, i * dy-diff);
        }
    }
    body(_page);
}

XAxis::XAxis (float first, float last) : Axis(first, last) {
    Range(first, last);
}

XAxis::~XAxis () { }

void XAxis::request (Requisition& r) const {
    MonoGlyph::request(r);
    Requirement& rx = r.x_requirement();
    Requirement& ry = r.y_requirement();
    rx.stretch(fil);
    rx.shrink(fil);
    rx.alignment(0.0);
    ry.stretch(0.0);
    ry.shrink(0.0);
    ry.alignment(0.0);
}

void XAxis::Range(float first, float last) {
    Coord l, r;
    WidgetKit& kit = *WidgetKit::instance();

    l = _a.left(); r = _a.right();
    _first = first;
    _last = last;
    _page = new Page(new HRule(color("axis_color"), 1));
    int num = (int)((r - l)/xinc);
    num = (num > 2) ? num : 2;
    Glyph** tick = make_xticks(
        num, _first, _last, "%3.2f", kit.font(), color("tick_color")
    );
    for (int i = 0; i < num; i++) {
        _page->insert(i, tick[i]);
    }
    if (num == 1) {
        _page->move(0, 0, 0);
    } else if (num > 1) {
        Coord dx = (_a.right() - _a.left()) / (num - 1);
        for (int i = 0; i < num; ++i) {
            _page->move(i, i * dx, 0);
        }
    }
    body(_page);
}

