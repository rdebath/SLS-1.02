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
 * PSFigItem
 */

#include "PSFigItem.h"

#include "Document.h"
#include "PSFigView.h"
#include "PPM_Image.h"
#include "PGM_Image.h"
#include "IdrawImage.h"
#include "DocViewer.h"

#include "doc-target.h"
#include "properties.h"

#include <IV-look/kit.h>
#include <InterViews/border.h>
#include <InterViews/label.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/tformsetter.h>
#include <InterViews/transformer.h>
#include <OS/list.h>
#include <OS/math.h>

#include <strstream.h>
#include <stdio.h>
#include <string.h>

static void read_header (
    FILE* file, char* creator,
    float& left, float& bottom, float& right, float& top
) {
    char line[1000];
    boolean done = false;
    boolean creator_known = false;
    boolean bb_known = false;
    int l, b, r, t;
    while (!done && fgets(line, 1000, file) != NULL) {
        if (sscanf(line, "%%%%Creator: %s", creator) == 1) {
            creator_known = true;
        } else if (
            sscanf(line, "%%%%BoundingBox: %d %d %d %d", &l, &b, &r, &t) == 4)
        {
            bb_known = true;
        } else if (strcmp(line, "%%EndProlog\n") == 0) {
            done = true;
        }
    }
    if (!creator_known) {
        strcpy(creator, "idraw");
    }
    if (bb_known) {
        left = float(l);
        bottom = float(b);
        right = float(r);
        top = float(t);
    } else {
        left = 0; 
        bottom = 0;
        right = 50;
        top = 50;
    }
}

class PSFigViewInfo {
public:
    PSFigView* _view;
    Glyph* _draft;
    Glyph* _final;
};

declareList(PSFigViewInfo_List,PSFigViewInfo)
implementList(PSFigViewInfo_List,PSFigViewInfo)

PSFigItem::PSFigItem (
    Document* document, Item* parent, long style, long source
) : Item(document, parent, style, source) {
    _parameters = new char[256];
    _parameters[0] = '\0';
    _filename = new char[256];
    _filename[0] = '\0';
    _creator = new char[100];
    _creator[0] = '\0';
    _width = 0;
    _height = 0;
    _hscale = 0;
    _vscale = 0;
    _hoffset = 0;
    _voffset = 0;
    _view = nil;
}

PSFigItem::~PSFigItem () {
    delete _parameters;
    delete _filename;
    delete _creator;
    if (_view != nil) {
        while (_view->count() > 0) {
            PSFigViewInfo& view = _view->item_ref(0);
	    Resource::unref(view._draft);
	    Resource::unref(view._final);
            _view->remove(0);
        }
        delete _view;
    }
}

void PSFigItem::read (istream& in) {
    if (_document != nil) {
        _document->read(in, this, _style, _source);
    }
}

void PSFigItem::write (ostream& out) {
    if (_document != nil) {
        _document->write(out, this, _style, _source);
    }
}

Glyph* PSFigItem::view (ItemView* parent, DocumentViewer* viewer) {
    return viewer->view(parent, this);
}

void PSFigItem::parameters (const char* params) {
    strcpy(_parameters, params);
    change_graphic();
}

const char* PSFigItem::parameters () {
    return _parameters;
}

void PSFigItem::change_graphic () {
    _width = 0;
    _height = 0;
    _hscale = 0;
    _vscale = 0;
    _hoffset = 0;
    _voffset = 0;
    _rotate = 0;
    istrstream scratch(_parameters);
    char keyword[100];
    char value[100];
    while (scratch.getline(keyword, 100, '=')) {
        if (strcmp(keyword, "figure") == 0) {
            scratch.getline(_filename, 100, ',');
        } else if (strcmp(keyword, "width") == 0) {
            scratch.getline(value, 100, ',');
            _width = _document->convert_metric(value);
        } else if (strcmp(keyword, "height") == 0) {
            scratch.getline(value, 100, ',');
            _height = _document->convert_metric(value);
        } else if (strcmp(keyword, "hoffset") == 0) {
            scratch.getline(value, 100, ',');
            _hoffset = _document->convert_metric(value);
        } else if (strcmp(keyword, "voffset") == 0) {
            scratch.getline(value, 100, ',');
            _voffset = _document->convert_metric(value);
        } else if (strcmp(keyword, "hscale") == 0) {
            scratch.getline(value, 100, ',');
            _hscale = _document->convert_metric(value);
        } else if (strcmp(keyword, "vscale") == 0) {
            scratch.getline(value, 100, ',');
            _vscale = _document->convert_metric(value);
        } else if (strcmp(keyword, "rotate") == 0) {
            scratch.getline(value, 100, ',');
            _rotate = _document->convert_metric(value);
        }
        if (scratch.peek() == ',') {
            scratch.ignore(1);
        }
    };
    FILE* file = fopen(_filename, "r");
    if (file != nil) {
        read_header(file, _creator, _left, _bottom, _right, _top);
        float x1, y1, x2, y2, x3, y3, x4, y4;
        Transformer t;
        t.rotate(_rotate);
        t.transform(_left, _bottom, x1, y1);
        t.transform(_left, _top, x2, y2);
        t.transform(_right, _top, x3, y3);
        t.transform(_right, _bottom, x4, y4);
        _left = Math::min(x1, x2, x3, x4);
        _right = Math::max(x1, x2, x3, x4);
        _bottom = Math::min(y1, y2, y3, y4);
        _top = Math::max(y1, y2, y3, y4);

        if (_width != 0) {
            _hscale = _width / (_right - _left);
        }
        if (_height != 0) {
            _vscale = _height / (_top - _bottom);
        }
        if (_hscale == 0 && _vscale != 0) {
            _hscale = _vscale;
        }
        if (_vscale == 0 && _hscale != 0) {
            _vscale = _hscale;
        }
        if (_hscale == 0) {
            _hscale = 1.0;
        }
        if (_vscale == 0) {
            _vscale = 1.0;
        }
        _width = (_right - _left) * _hscale;
        _height = (_top - _bottom) * _vscale;
    }
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            PSFigViewInfo& info = _view->item_ref(i);
	    Resource::unref(info._draft);
	    info._draft = nil;
	    Resource::unref(info._final);
	    info._final = nil;
            info._view->graphic_changed();
        }
    }
}

Glyph* PSFigItem::graphic (PSFigViewMode mode, PSFigView* view) {
    WidgetKit& kit = *WidgetKit::instance();
    const LayoutKit& layout = *LayoutKit::instance();
    Style* s = kit.style();
    const Font* font = kit.font();
    const Color* fg = kit.foreground();
    const Color* bg = kit.background();
    boolean idraw_font_metrics = s->value_is_on(IDRAW_FONT_METRICS);
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            PSFigViewInfo& info = _view->item_ref(i);
            if (info._view == view) {
                Glyph* g = (mode == PSDraft) ? info._draft : info._final;
                if (g == nil) {
                    if (mode == PSDraft) {
                        char params[256];
                        strcpy(params, _parameters);
                        Glyph* label = layout.vbox_first_aligned(
			    new Label(_creator, font, fg)
			);
                        char* l = strtok(params, ",");
                        while (l != nil) {
                            label->append(new Label(l, font, fg));
                            l = strtok(nil, ",");
                        }
                        g = new DocTarget(
			    new Border(
				layout.margin(
				    layout.center(label),
				    0, fil, 0, 0, fil, 0, 0, fil, 0, 0, fil, 0
				),
				fg
			    )
                        );
                    } else {
                        FILE* file = fopen(_filename, "r");
                        Transformer t;
                        if (file != nil) {
                            if (strcmp(_creator, "pgmtops") == 0) { 
                                g = new PGM_Image(file);
                            } else if (strcmp(_creator, "ppmtops") == 0) { 
                                g = new PPM_Image(file);
                            } else if (strcmp(_creator, "idraw") == 0) {
                                g = new IdrawImage(file, idraw_font_metrics);
                                t.translate(- _left, - _bottom);
                            }
                            fclose(file);
                        }
			t.rotate(_rotate);
			t.scale(_hscale, _vscale);
			g = layout.center(new TransformSetter(g, t));
                    }
		    g = layout.fixed_span(g, _width, _height);
		    Resource::ref(g);
                    if (mode == PSDraft) {
                        info._draft = g;
                    } else {
                        info._final = g;
                    }
                }
                return g;
            }
        }
    }
    return nil;
}

void PSFigItem::attach (PSFigView* view) {
    if (_view == nil) {
        _view = new PSFigViewInfo_List();
    }
    PSFigViewInfo info;
    info._view = view;
    info._draft = nil;
    info._final = nil;
    _view->append(info);
}

void PSFigItem::detach (PSFigView* view) {
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            PSFigViewInfo& info = _view->item_ref(i);
            if (info._view == view) {
		Resource::unref(info._draft);
		Resource::unref(info._final);
                _view->remove(i);
                break;
            }
        }
    }
}

void PSFigItem::notify () {
    if (_view != nil) {
        long count = _view->count();
        for (long i = 0; i < count; ++i) {
            PSFigViewInfo& info = _view->item_ref(i);
            info._view->update();
        }
    }
    Item::notify();
}
