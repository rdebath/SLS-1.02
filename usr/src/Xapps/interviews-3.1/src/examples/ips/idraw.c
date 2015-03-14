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
 * Read an idraw drawing from a file
 */

#include "figure.h"
#include "idraw.h"
#include <InterViews/box.h>
#include <InterViews/brush.h>
#include <InterViews/character.h>
#include <InterViews/color.h>
#include <InterViews/font.h>
#include <InterViews/place.h>
#include <InterViews/psfont.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/tformsetter.h>
#include <InterViews/transformer.h>
#include <OS/file.h>
#include <OS/list.h>
#include <OS/string.h>
#include <ctype.h>

class BrushInfo {
public:
    const Brush* brush_;
    int width_;
    int pattern_;
};

declareList(BrushInfoList,BrushInfo)
implementList(BrushInfoList,BrushInfo)

class Stipple : public Resource {
public:
    Stipple(float dither);

    float dither_;
protected:
    virtual ~Stipple ();
};

Stipple::Stipple (float dither) {
    dither_ = dither;
}

Stipple::~Stipple () { }

declarePtrList(StippleList,Stipple)
implementPtrList(StippleList,Stipple)

static Stipple* no_stipple = (Stipple*) -1;

struct FigureInfo {
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
    
static FigureInfo early_info[] = {
    { "Idraw", 1, 1, 1, 1, 1, 1, 0, 0 },
    { "BSpl",  1, 1, 1, 0, 1, 1, -1, 0 },
    { "Circ",  1, 1, 1, 0, 1, 1, 1, 0 },
    { "CBSpl", 1, 1, 1, 0, 1, 1, -1, 0 },
    { "Elli",  1, 1, 1, 0, 1, 1, 1, 0 },
    { "Line",  1, 1, 1, 0, 1, 1, 2, 0 },
    { "MLine", 1, 1, 1, 0, 1, 1, -1, 0 },
    { "Pict",  1, 1, 1, 1, 1, 1, 0, 0 },
    { "Poly",  1, 1, 1, 0, 1, 1, -1, 0 },
    { "Rect",  1, 1, 1, 0, 1, 1, 2, 0 },
    { "Text", 0, 1, 0, 1, 0, 1, 0, 0 },
    { "eop",  0, 0, 0, 0, 0, 0, 0, 0 },
    {nil, 0, 0, 0, 0, 0, 0, 0}
};

static FigureInfo version_10_info[] = {
    { "Idraw", 1, 1, 1, 1, 1, 1, 0, 0 },
    { "BSpl",  1, 1, 1, 0, 1, 1, -1, 1 },
    { "Circ",  1, 1, 1, 0, 1, 1, 1, 0 },
    { "CBSpl", 1, 1, 1, 0, 1, 1, -1, 0 },
    { "Elli",  1, 1, 1, 0, 1, 1, 1, 0 },
    { "Line",  1, 1, 1, 0, 1, 1, 2, 1 },
    { "MLine", 1, 1, 1, 0, 1, 1, -1, 1 },
    { "Pict",  1, 1, 1, 1, 1, 1, 0, 0 },
    { "Poly",  1, 1, 1, 0, 1, 1, -1, 0 },
    { "Rect",  1, 1, 1, 0, 1, 1, 2, 0 },
    { "Text", 0, 1, 0, 1, 0, 1, 0, 0 },
    { "eop",  0, 0, 0, 0, 0, 0, 0, 0 },
    { nil, 0, 0, 0, 0, 0, 0, 0, 0 }
};

class IdrawReaderImpl {
    friend class IdrawReader;

    InputFile* file_;
    int version_;
    FigureInfo* figinfo_;
    const char* start_;
    const char* end_;
    const char* cur_;
    BrushInfoList brushes_;
    StippleList stipples_;
    static Brush* no_brush;

    Glyph* load(
	const Brush* pb, const Color* pfg, const Color* pbg,
	const Font* pf, const Stipple* ps
    );
    boolean fill();
    boolean read(String&);
    boolean read(char&);
    boolean read(int&);
    boolean read(float&);
    const Brush* read_brush();
    const Color* read_color();
    const Font* read_font();
    const Stipple* read_stipple();
    void read_transformer(Transformer&);
    const Color* dither_color(const Color* fg, const Color* bg, float dither);
    void skip();
};

Brush* IdrawReaderImpl::no_brush = (Brush*)-1;

Glyph* IdrawReader::load(InputFile* f) {
    IdrawReaderImpl impl_;
    impl_.file_ = f;
    impl_.start_ = nil;
    impl_.end_ = nil;
    impl_.cur_ = nil;
    return impl_.load(nil, nil, nil, nil, nil);
}

Glyph* IdrawReaderImpl::load(
    const Brush* pb, const Color* pfg, const Color* pbg,
    const Font* pf, const Stipple* ps
) {
    const LayoutKit& layout = *LayoutKit::instance();
    skip();
    Transformer tx;
    Glyph* glyph = nil;
    String name;
    if (read(name)) {
	if (name == "Idraw") {
	    read(version_);
	    figinfo_ = version_ < 10 ? early_info : version_10_info;
	}
	FigureInfo* fig;
	for (fig = &figinfo_[0]; fig->name != nil; fig++) {
	    if (name == fig->name) {
		break;
	    }
	}
        const Brush* b = fig->brush ? read_brush() : nil;
        const Color* fg = fig->foreground ? read_color() : nil;
        const Color* bg = fig->background ? read_color() : nil;
        const Font* f = fig->font ? read_font() : nil;
        const Stipple* s = fig->pattern ? read_stipple() : nil;
        if (fig->transformer) {
            read_transformer(tx);
        }

        if (pb) {
	    b = pb;
	}
        if (pfg) {
	    fg = pfg;
	}
        if (pbg) {
	    bg = pbg;
	}
        if (pf) {
	    f = pf;
	}
        if (ps) {
	    s = ps;
	}

	String figname(fig->name);
        if (fig->name == nil) {
            ; // error
        } else if (figname == "Idraw" || figname == "Pict") {
            Glyph* pic = layout.overlay();
	    for (;;) {
                Glyph* g = load(b, fg, bg, f, s);
		if (g == nil) {
		    break;
		}
		pic->append(g);
            }
            glyph = pic;
        } else if (figname == "eop") {
            glyph = nil;
        } else if (figname == "Text") {
            skip();
	    String s;
	    read(s);
	    char ch;
	    read(ch);
            Glyph* col = layout.vbox();
            Glyph* line = layout.hbox();
	    FontBoundingBox bbox;
	    f->font_bbox(bbox);
            Coord lineheight = bbox.font_ascent() + bbox.font_descent();
	    while (read(ch) && ch != ']') {
                if (ch == '\n') {
                    line->append(layout.strut(f));
                    col->append(
			layout.fixed_span_dimension(
			    line, Dimension_Y, lineheight
			)
		    );
                    line = layout.hbox();
                } else if (ch == ' ') {
		    line->append(new Character(' ', f, fg));
                } else if (ch != ')' && ch != '(') {
                    if (ch == '\\') {
			read(ch);
			if (isdigit(ch)) {
			    ch -= '0';
			    ch *= 8;
			    char digit;
			    read(digit);
			    ch = (ch * 8) + digit - '0';
			    read(digit);
			    ch = (ch * 8) + digit - '0';
			}
                    }
                    line->append(new Character(ch, f, fg));
                }
            }
            Transformer fixtext;
            fixtext.translate(0, bbox.font_descent() - lineheight);
            glyph = new TransformSetter(col, fixtext);
        } else {
            skip();
            int c = fig->coords;
            if (c == -1) { 
		read(c);
            }
            Coord* x = new Coord[c];
            Coord* y = new Coord[c];
            float xx, yy;
            for (int i = 0; i < c; ++i) {
		read(xx);
		read(yy);
		x[i] = xx;
		y[i] = yy;
            }

            const Brush* brush = (b != no_brush) ? b : nil;
            const Color* stroke = fg;
            const Color* fill = (
                (s != no_stipple) ? dither_color(fg, bg, s->dither_) : nil
            );
            if (figname == "Line") {
                glyph = new Line(brush, stroke, fill, x[0], y[0], x[1], y[1]);
            } else if (figname == "BSpl") {
                glyph = new Open_BSpline(brush, stroke, fill, x, y, c);
            } else if (figname == "CBSpl") {
                glyph = new Closed_BSpline(brush, stroke, fill, x, y, c);
            } else if (figname == "MLine") {
                glyph = new Polyline(brush, stroke, fill, x, y, c);
            } else if (figname == "Poly") {
                glyph = new Polygon(brush, stroke, fill, x, y, c);
            } else if (figname == "Rect") {
                glyph = new Rectangle(
		    brush, stroke, fill, x[0], y[0], x[1], y[1]
		);
            } else if (figname == "Circ") {
		Coord radius;
		read(radius);
                glyph = new Circle(brush, stroke, fill, x[0], y[0], radius);
            } else if (figname == "Elli") {
		Coord r1, r2;
		read(r1);
		read(r2);
                glyph = new Ellipse(brush, stroke, fill, x[0], y[0], r1, r2);
            } else {
                glyph = nil;
            }
            delete x;
            delete y;
        }
        for (int extra = fig->skip; extra > 0; --extra) {
            skip();
        }
    }
    if (glyph != nil && !tx.identity()) {
	glyph = new TransformSetter(glyph, tx);
    }
    return glyph;
}

boolean IdrawReaderImpl::fill() {
    if (cur_ >= end_) {
	int n = file_->read(start_);
	if (n <= 0) {
	    return false;
	}
	cur_ = start_;
	end_ = start_ + n;
    }
    return true;
}

boolean IdrawReaderImpl::read(String& s) {
    if (!fill()) {
	return false;
    }
    const char* p1 = cur_;
    while (p1 < end_ && isspace(*p1)) {
	++p1;
    }
    const char* p2 = p1;
    while (p2 < end_ && !isspace(*p2)) {
	++p2;
    }
    cur_ = p2;
    s = String(p1, p2 - p1);
}

boolean IdrawReaderImpl::read(char& c) {
    if (!fill()) {
	return false;
    }
    c = *cur_++;
    return true;
}

boolean IdrawReaderImpl::read(int& i) {
    String s;
    return read(s) && s.convert(i);
}

boolean IdrawReaderImpl::read(float& f) {
    String s;
    return read(s) && s.convert(f);
}

void IdrawReaderImpl::skip() {
    String s;
    while (read(s) && s != "%I") { }
}

const Brush* IdrawReaderImpl::read_brush() {
    skip();
    String s;
    read(s);
    read(s);
    if (s == "u") {
        return nil;
    }
    if (s == "n") {
        return no_brush;
    }
    int pattern;
    int width;
    s.convert(pattern);
    read(width);

    for (ListItr(BrushInfoList) i(brushes_); i.more(); i.next()) {
	BrushInfo& b = i.cur_ref();
	if (b.width_ == width && b.pattern_ == pattern) {
	    return b.brush_;
	}
    }
    BrushInfo* b = new BrushInfo;
    b->brush_ = new Brush(pattern, Coord(width));
    Resource::ref(b->brush_);
    b->width_ = width;
    b->pattern_ = pattern;
    brushes_.append(*b);
    return b->brush_;
}

const Color* IdrawReaderImpl::read_color() {
    skip();
    String s;
    read(s);
    read(s);
    if (s == "u") {
	return nil;
    }
    float r, g, b;
    read(r);
    read(g);
    read(b);
    return new Color(r, g, b, 1.0);
}

const Color* IdrawReaderImpl::dither_color(
    const Color* f, const Color* b, float dither
) {
    float fr, fg, fb;
    float br, bg, bb;
    f->intensities(fr, fg, fb);
    b->intensities(br, bg, bb);
    float red = (1 - dither) * fr + dither * br;
    float green = (1 - dither) * fg + dither * bg;
    float blue = (1 - dither) * fb + dither * bb;
    return new Color(red, green, blue);
}

const Font* IdrawReaderImpl::read_font() {
    skip();
    String s;
    read(s);
    read(s);
    if (s == "u") {
	return nil;
    }
    String psname, ptsize;
    read(psname);
    NullTerminatedString psname_nt(psname);
    float pointsize;
    read(pointsize);
    if (PSFont::exists(psname_nt.string())) {
	NullTerminatedString s_nt(s);
	return new PSFont(psname_nt.string(), pointsize, s_nt.string(), 1.0);
    }
    String default_font("fixed");
    Session::instance()->style()->find_attribute("font", default_font);
    return Font::lookup(default_font);
}

const Stipple* IdrawReaderImpl::read_stipple() {
    skip();
    String s;
    read(s);
    read(s);
    if (s == "u") {
	return nil;
    }
    if (s == "n") {
        return no_stipple;
    }
    float dither;
    s.convert(dither);
    for (ListItr(StippleList) i(stipples_); i.more(); i.next()) {
	Stipple* st = i.cur();
	if (st->dither_ == dither) {
	    return st;
	}
    }
    Stipple* st = new Stipple(dither);
    Resource::ref(st);
    stipples_.append(st);
    return st;
}

void IdrawReaderImpl::read_transformer(Transformer& t) {
    skip();
    String s;
    read(s);
    read(s);
    if (s != "u") {
	float a[6];
	for (int i = 0; i < 6; i++) {
	    read(a[i]);
	}
        Transformer tt(a[0], a[1], a[2], a[3], a[4], a[5]);
        t = tt;
    }
}
