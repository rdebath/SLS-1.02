/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * IdrawImage - read an idraw drawing from a file
 */

#include "IdrawImage.h"

#include "Figure.h"

#include <InterViews/brush.h>
#include <InterViews/character.h>
#include <InterViews/color.h>
#include <InterViews/font.h>
#include <InterViews/layout.h>
#include <InterViews/psfont.h>
#include <InterViews/tformsetter.h>
#include <InterViews/transformer.h>
#include <IV-2_6/InterViews/world.h>

#include <string.h>
#include <ctype.h>

static boolean _idraw_font_metrics;

char buffer[1000];

void skip (FILE* f) {
    while (fscanf(f, "%s ", buffer) && strcmp(buffer, "%I") != 0) { }
}

Brush* no_brush = (Brush*)-1;

class BrushInfo {
public:
    const Brush* _brush;
    int _width;
    int _pattern;
    BrushInfo* _next;
};

BrushInfo* _brushes;
int _brush_count;

const Brush* read_brush (FILE* f) {
    skip(f);
    fscanf(f, "%s %s", buffer, buffer);
    if (strcmp(buffer, "u") == 0) {
        return nil;
    } else if (strcmp(buffer, "n") == 0) {
        return no_brush;
    } else {
        int pattern;
        int width;
        sscanf(buffer, "%d", &pattern);
        fscanf(f, "%d", &width);

        BrushInfo* brush = _brushes;
        while (
            brush != nil
            && (brush->_width != width || brush->_pattern != pattern)
        ) {
            brush = brush->_next;
        }
        if (brush == nil) {
            brush = new BrushInfo;
            brush->_brush = new Brush(pattern, Coord(width));
            brush->_brush->ref();
            brush->_width = width;
            brush->_pattern = pattern;
            brush->_next = _brushes;
            _brushes = brush;
            _brush_count += 1;
        }
        return brush->_brush;
    }
}

class ColorInfo {
public:
    const Color* _color;
    float _r;
    float _g;
    float _b;
    ColorInfo* _next;
};

ColorInfo* _colors;
int _color_count;

const Color* read_color (FILE* f) {
    skip(f);
    fscanf(f, "%s %s", buffer, buffer);
    if (strcmp(buffer, "u") == 0) {
        return nil;
    } else {
        float r, g, b;
        fscanf(f, "%g %g %g", &r, &g, &b);

        ColorInfo* color = _colors;
        while (
            color != nil
            && (color->_r != r || color->_g != g || color->_b != b)
        ) {
            color = color->_next;
        }
        if (color == nil) {
            color = new ColorInfo;
            color->_color = new Color(r, g, b);
            color->_color->ref();
            color->_r = r;
            color->_g = g;
            color->_b = b;
            color->_next = _colors;
            _colors = color;
            _color_count += 1;
        }
        return color->_color;
    }
}

const Color* dither_color (const Color* f, const Color* b, float dither) {
    float fr, fg, fb;
    float br, bg, bb;
    f->intensities(fr, fg, fb);
    b->intensities(br, bg, bb);
    float red = (1 - dither) * fr + dither * br;
    float green = (1 - dither) * fg + dither * bg;
    float blue = (1 - dither) * fb + dither * bb;

    ColorInfo* color = _colors;
    while (
        color != nil
        && (color->_r != red || color->_g != green || color->_b != blue)
    ) {
        color = color->_next;
    }
    if (color == nil) {
        color = new ColorInfo;
        color->_color = new Color(red, green, blue);
        color->_color->ref();
        color->_r = red;
        color->_g = green;
        color->_b = blue;
        color->_next = _colors;
        _colors = color;
        _color_count += 1;
    }
    return color->_color;
}

class FontInfo {
public:
    const Font* _font;
    const char* _screenname;
    const char* _psname;
    float _pointsize;
    FontInfo* _next;
};

FontInfo* _fonts;
int _font_count;

const Font* read_font (FILE* f) {
    skip(f);
    fscanf(f, "%s %s", buffer, buffer);
    if (strcmp(buffer, "u") == 0) {
        return nil;
    } else {
        char psname[256];
        float pointsize;
        fscanf(f, "%s %g", psname, &pointsize);
        FontInfo* font = _fonts;
        while (font != nil && (strcmp(font->_screenname, buffer) != 0)) {
            font = font->_next;
        }
        if (font == nil) {
	    World* w = World::current();
            font = new FontInfo;
            if (_idraw_font_metrics) {
		if (Font::exists(w->display(), buffer)) {
                    font->_font = new Font(buffer);
                } else {
                    font->_font = w->font();
                }
            } else {
		if (PSFont::exists(psname)) {
                    font->_font = new PSFont(psname, pointsize, buffer, 1.0);
                } else {
		    font->_font = w->font();
                }
            }
            font->_font->ref();
            font->_screenname = strcpy(new char[strlen(buffer) + 1], buffer);
            font->_psname = strcpy(new char[strlen(psname) + 1], psname);
            font->_pointsize = pointsize;
            font->_next = _fonts;
            _fonts = font;
            _font_count += 1;
        }
        return font->_font;
    }
}

class Stipple : public Resource {
public:
    Stipple (float dither);

    float _dither;
protected:
    virtual ~Stipple ();
};

Stipple::Stipple (float dither) {
    _dither = dither;
}

Stipple::~Stipple () { }

class StippleInfo {
public:
    Stipple* _stipple;
    StippleInfo* _next;
};

StippleInfo* _stipples;
int _stipple_count;

Stipple* no_stipple = (Stipple*) -1;

Stipple* read_stipple (FILE* file) {
    skip(file);
    fscanf(file, "%s %s", buffer, buffer);
    if (strcmp(buffer, "u") == 0) {
        return nil;
    } else if (strcmp(buffer, "n") == 0) {
        return no_stipple;
    } else {
        float dither;
        sscanf(buffer, "%f", &dither);
        StippleInfo* stipple = _stipples;
        while (
            stipple != nil
            && stipple->_stipple->_dither != dither
        ) {
            stipple = stipple->_next;
        }
        if (stipple == nil) {
            stipple = new StippleInfo;
            stipple->_stipple = new Stipple(dither);
            stipple->_stipple->ref();
            stipple->_next = _stipples;
            _stipples = stipple;
            _stipple_count += 1;
        }
        return stipple->_stipple;
    }
}

void read_transformer (FILE* f, Transformer& t) {
    skip(f);
    fscanf(f, "%s %s", buffer, buffer);
    if (strcmp(buffer, "u") != 0) {
        float a00, a01, a10, a11, a20, a21;
        fscanf(
            f, "%f %f %f %f %f %f",
            &a00, &a01, &a10, &a11, &a20, &a21
        );
        Transformer tt(a00, a01, a10, a11, a20, a21);
        t = tt;
    }
}

struct figure {
    const char* name;
    boolean brush;
    boolean foreground;
    boolean background;
    boolean font;
    boolean pattern;
    boolean transformer;
    int coords;
    int skip;
};
    
figure early_version_figures[] = {
    {"Idraw", 1, 1, 1, 1, 1, 1, 0, 0},
    {"BSpl",  1, 1, 1, 0, 1, 1, -1, 0},
    {"Circ",  1, 1, 1, 0, 1, 1, 1, 0},
    {"CBSpl", 1, 1, 1, 0, 1, 1, -1, 0},
    {"Elli",  1, 1, 1, 0, 1, 1, 1, 0},
    {"Line",  1, 1, 1, 0, 1, 1, 2, 0},
    {"MLine", 1, 1, 1, 0, 1, 1, -1, 0},
    {"Pict",  1, 1, 1, 1, 1, 1, 0, 0},
    {"Poly",  1, 1, 1, 0, 1, 1, -1, 0},
    {"Rect",  1, 1, 1, 0, 1, 1, 2, 0},
    {"Text", 0, 1, 0, 1, 0, 1, 0, 0},
    {"eop",  0, 0, 0, 0, 0, 0, 0, 0},
    {nil, 0, 0, 0, 0, 0, 0, 0}
};

figure version_10_figures[] = {
    {"Idraw", 1, 1, 1, 1, 1, 1, 0, 0},
    {"BSpl",  1, 1, 1, 0, 1, 1, -1, 1},
    {"Circ",  1, 1, 1, 0, 1, 1, 1, 0},
    {"CBSpl", 1, 1, 1, 0, 1, 1, -1, 0},
    {"Elli",  1, 1, 1, 0, 1, 1, 1, 0},
    {"Line",  1, 1, 1, 0, 1, 1, 2, 1},
    {"MLine", 1, 1, 1, 0, 1, 1, -1, 1},
    {"Pict",  1, 1, 1, 1, 1, 1, 0, 0},
    {"Poly",  1, 1, 1, 0, 1, 1, -1, 0},
    {"Rect",  1, 1, 1, 0, 1, 1, 2, 0},
    {"Text", 0, 1, 0, 1, 0, 1, 0, 0},
    {"eop",  0, 0, 0, 0, 0, 0, 0, 0},
    {nil, 0, 0, 0, 0, 0, 0, 0, 0}
};

static int drawing_version;

figure* versions[] = {
    early_version_figures,
    early_version_figures,
    early_version_figures,
    early_version_figures,
    early_version_figures,
    early_version_figures,
    early_version_figures,
    early_version_figures,
    early_version_figures,
    early_version_figures,
    version_10_figures,
    nil
};

static figure* figures = versions[0];

int which (figure* figures, const char* name) {
    int i = 0;
    while (figures[i].name != nil && strcmp(figures[i].name, name) != 0) {
        ++i;
    }
    return i;
}

const float fixtextscale = 75.0 / 72.0;

Glyph* read_idraw_graphic (
    FILE* file, const Brush* pb, const Color* pfg, const Color* pbg,
    const Font* pf, Stipple* ps
) {
    skip(file);
    Transformer tx;
    Glyph* glyph = nil;
    const LayoutKit& layout = *LayoutKit::instance();
    if (fscanf(file, "%s", buffer) != EOF) {
        figure& fig = figures[which(figures, buffer)];

        if (strcmp(fig.name, "Idraw") == 0) {
            fscanf(file, "%d", &drawing_version);
            figures = versions[drawing_version];
        }
        const Brush* b = (fig.brush) ? read_brush(file) : nil;
        const Color* fg = (fig.foreground) ? read_color(file) : nil;
        const Color* bg = (fig.background) ? read_color(file) : nil;
        const Font* f = (fig.font) ? read_font(file) : nil;
        Stipple* s = (fig.pattern) ? read_stipple(file) : nil;
        if (fig.transformer) {
            read_transformer(file, tx);
        }

        if (pb) b = pb;
        if (pfg) fg = pfg;
        if (pbg) bg = pbg;
        if (pf) f = pf;
        if (ps) s = ps;

        if (fig.name == nil) {
            ; // error
        } else if (
            strcmp(fig.name, "Idraw") == 0 || strcmp(fig.name, "Pict") == 0
        ) {
            Glyph* pic = layout.overlay();
            Glyph* g;
            do {
                g = read_idraw_graphic(file, b, fg, bg, f, s);
                if (g != nil) {
                    pic->append(g);
                }
            } while (g != nil);
            glyph = pic;
        } else if (strcmp(fig.name, "eop") == 0) {
            glyph = nil;
        } else if (strcmp(fig.name, "Text") == 0) {
            skip(file);
            fscanf(file, "%s", buffer);
            getc(file);
            PolyGlyph* col = layout.vbox_first_aligned();
            PolyGlyph* line = layout.hbox_first_aligned();
	    FontBoundingBox bbox;
	    f->font_bbox(bbox);
            Coord lineheight = bbox.font_ascent() + bbox.font_descent();
            if (_idraw_font_metrics) {
                lineheight /= fixtextscale;
            }
            int c;
            while ((c = getc(file)) != ']') {
                if (c == '\n') {
                    line->append(layout.strut(f));
                    col->append(
			layout.v_fixed_span(line, lineheight)
		    );
                    line = layout.hbox();
                } else if (c == ' ') {
                    if (_idraw_font_metrics) {
                        line->append(
			    layout.shape_of(new Character(' ', f, fg))
			);
                    } else {
                        line->append(new Character(' ', f, fg));
                    }
                } else if (c != ')' && c != '(') {
                    if (c == '\\') {
                        c = getc(file);
			if (isdigit(c)) {
			    c -= '0';
			    c = (c * 8) + getc(file) - '0';
			    c = (c * 8) + getc(file) - '0';
			}
                    }
                    line->append(new Character(c, f, fg));
                }
            }
            Transformer fixtext;
            if (_idraw_font_metrics) {
                fixtext.scale(fixtextscale, fixtextscale);
            }
            fixtext.translate(0, bbox.font_descent() - lineheight);
            glyph = new TransformSetter(col, fixtext);
        } else {
            skip(file);
            int c = fig.coords;
            if (c == -1) { 
                fscanf(file, "%d", &c);
            }
            Coord xx, yy;
            Coord* x = new Coord[c];
            Coord* y = new Coord[c];
            for (int i = 0; i < c; ++i) {
                fscanf(file, "%g %g", &xx, &yy);
                x[i] = xx;
                y[i] = yy;
            }

            const Brush* brush = (b != no_brush) ? b : nil;
            const Color* stroke = fg;
            const Color* fill = (
                (s != no_stipple) ? dither_color(fg, bg, s->_dither) : nil
            );
            if (strcmp(fig.name, "Line") == 0) {
                glyph = new Line(brush, stroke, fill, x[0], y[0], x[1], y[1]);
            } else if (strcmp(fig.name, "BSpl") == 0) {
                glyph = new Open_BSpline(brush, stroke, fill, x, y, c);
            } else if (strcmp(fig.name, "CBSpl") == 0) {
                glyph = new Closed_BSpline(brush, stroke, fill, x, y, c);
            } else if (strcmp(fig.name, "MLine") == 0) {
                glyph = new Polyline(brush, stroke, fill, x, y, c);
            } else if (strcmp(fig.name, "Poly") == 0) {
                glyph = new Polygon(brush, stroke, fill, x, y, c);
            } else if (strcmp(fig.name, "Rect") == 0) {
                glyph = new Rectangle(brush, stroke, fill,x[0],y[0],x[1],y[1]);
            } else if (strcmp(fig.name, "Circ") == 0) {
                fscanf(file, "%f", &xx);
                glyph = new Circle(brush, stroke, fill, x[0], y[0], xx);
            } else if (strcmp(fig.name, "Elli") == 0) {
                fscanf(file, "%f %f", &xx, &yy);
                glyph = new Ellipse(brush, stroke, fill, x[0], y[0], xx, yy);
            } else {
                glyph = nil;
            }
            delete x;
            delete y;
        }
        for (int extra = fig.skip; extra > 0; --extra) {
            skip(file);
        }
    }
    if (glyph != nil && !tx.identity()) {
        glyph = new TransformSetter(glyph, tx);
    }
    return glyph;
}

IdrawImage::IdrawImage(FILE* file, boolean idraw_font_metrics) {
    _idraw_font_metrics = idraw_font_metrics;
    body(read_idraw_graphic(file, nil, nil, nil, nil, nil));
}

IdrawImage::~IdrawImage() { }
