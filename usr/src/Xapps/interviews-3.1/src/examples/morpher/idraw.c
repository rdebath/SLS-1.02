/*
 * Read an idraw drawing from a file
 */

#include "figure.h"
#include "idraw.h"
#include <InterViews/brush.h>
#include <InterViews/color.h>
#include <InterViews/font.h>
#include <InterViews/psfont.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/tformsetter.h>
#include <InterViews/transformer.h>
#include <IV-look/kit.h>
#include <OS/file.h>
#include <OS/list.h>
#include <OS/memory.h>
#include <OS/string.h>
#include <stdlib.h>
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
static Brush* no_brush = (Brush*) -1;

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
    { nil, 0, 0, 0, 0, 0, 0, 0 }
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

    Graphic* load(
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
    Transformer* read_transformer();
    const Color* dither_color(const Color* fg, const Color* bg, float dither);
    void skip();
};

GraphicMaster* IdrawReader::load(InputFile* f) {
    IdrawReaderImpl impl_;
    impl_.file_ = f;
    impl_.start_ = nil;
    impl_.end_ = nil;
    impl_.cur_ = nil;
    return (GraphicMaster*) impl_.load(nil, nil, nil, nil, nil);
}

GraphicMaster* IdrawReader::load(const char* filename) {
    GraphicMaster* gm = nil;
    String s(filename);
    InputFile* input = InputFile::open(s);
    if (input != nil) {
        gm = load(input);
        delete input;
    }
    return gm;
}

static void Ref_Brush(const Brush* b) {
    if (b != no_brush) {
        Resource::ref(b);
    }
}
static void Unref_Brush(const Brush* b) {
    if (b != no_brush) {
        Resource::unref(b);
    }
}
static void Ref_Stipple(const Stipple* p) {
    if (p != no_stipple) {
        Resource::ref(p);
    }
}
static void Unref_Stipple(const Stipple* p) {
    if (p != no_stipple) {
        Resource::unref(p);
    }
}

Graphic* IdrawReaderImpl::load(
    const Brush* pb, const Color* pfg, const Color* pbg,
    const Font* pf, const Stipple* ps
) {
    Ref_Brush(pb);
    Ref_Stipple(ps);
    Resource::ref(pfg);
    Resource::ref(pbg);
    Resource::ref(pf);
    
    Session* session = Session::instance();
    Style* style = session->style();
    skip();
    Transformer* tx = nil;
    Graphic* glyph = nil;
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
            tx = read_transformer();
        }
        Ref_Brush(b);
        Ref_Stipple(s);
        Resource::ref(fg);
        Resource::ref(bg);
        Resource::ref(f);

        if (pb) {
            Unref_Brush(b);
	    b = pb;
            Ref_Brush(b);
	}
        if (pfg) {
            Resource::unref(fg);
	    fg = pfg;
            Resource::ref(fg);
	}
        if (pbg) {
            Resource::unref(bg);
	    bg = pbg;
            Resource::ref(bg);
	}
        if (pf) {
            Resource::unref(f);
	    f = pf;
            Resource::ref(f);
	}
        if (ps) {
            Unref_Stipple(s);
	    s = ps;
            Ref_Stipple(s);
	}

	String figname(fig->name);
        if (fig->name == nil) {
            ; // error
        } else if (figname == "Idraw" || figname == "Pict") {
            PolyGraphic* pg;
            if (figname == "Idraw") {
                pg = new GraphicMaster;
            } else {
                pg = new PolyGraphic;
            }
            pg->transformer(tx);
	    for (;;) {
                Graphic* g = load(b, fg, bg, f, s);
		if (g == nil) {
		    break;
		}
		pg->append_(g);
            }
            glyph = pg;

        } else if (figname == "eop") {
            glyph = nil;
        } else if (figname == "Text") {
            skip();
	    String s;
	    read(s);
	    char ch;
	    read(ch);
            
            int bufsize = 256;
            char* buf = new char[bufsize];
            int i = 0;
            while (read(ch) && ch != ']') {
                buf[i++] = ch;
                if (i == bufsize) {
                    bufsize = bufsize*2;
                    char* newbuf = new char[bufsize];
                    Memory::copy(buf, newbuf, i*sizeof(char));
                    delete buf;
                    buf = newbuf;
                }
            }
            buf[i] = '\0';
            glyph = new Text(f, fg, buf, tx);
            delete buf;
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
                glyph = new Line(
                    brush, stroke, fill, x[0], y[0], x[1], y[1], tx
                );
            } else if (figname == "BSpl") {
                glyph = new Open_BSpline(brush, stroke, fill, x, y, c, tx);
            } else if (figname == "CBSpl") {
                glyph = new Closed_BSpline(brush, stroke, fill, x, y, c, tx);
            } else if (figname == "MLine") {
                glyph = new Polyline(brush, stroke, fill, x, y, c, tx);
            } else if (figname == "Poly") {
                glyph = new Polygon(brush, stroke, fill, x, y, c, tx);
            } else if (figname == "Rect") {
                glyph = new Rectangle(
		    brush, stroke, fill, x[0], y[0], x[1], y[1], tx
		);
            } else if (figname == "Circ") {
		Coord radius;
		read(radius);
                glyph = new Circle(
                    brush, stroke, fill, x[0], y[0], radius, tx
                );
            } else if (figname == "Elli") {
		Coord r1, r2;
		read(r1);
		read(r2);
                glyph = new Ellipse(
                    brush, stroke, fill, x[0], y[0], r1, r2, tx
                );
            } else {
                glyph = nil;
            }
            delete x;
            delete y;
        }
        for (int extra = fig->skip; extra > 0; --extra) {
            skip();
        }
        Unref_Brush(b);
        Unref_Stipple(s);
        Resource::unref(fg);
        Resource::unref(bg);
        Resource::unref(f);
        Resource::unref(tx);

    }
    Unref_Brush(pb);
    Unref_Stipple(ps);
    Resource::unref(pfg);
    Resource::unref(pbg);
    Resource::unref(pf);

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
    return true;
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
	BrushInfo& br = i.cur_ref();
	if (br.width_ == width && br.pattern_ == pattern) {
	    return br.brush_;
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
    String psname;
    read(psname);
    NullTerminatedString psname_nt(psname);
    float pointsize;
    read(pointsize);
    if (PSFont::exists(psname_nt.string())) {
        NullTerminatedString s_nt(s);
        return new PSFont(psname_nt.string(), pointsize, s_nt.string(), 1.0)
;
    }
    return WidgetKit::instance()->font();
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
	const Stipple* st = i.cur();
	if (st->dither_ == dither) {
	    return st;
	}
    }
    Stipple* st = new Stipple(dither);
    Resource::ref(st);
    stipples_.append(st);
    return st;
}

Transformer* IdrawReaderImpl::read_transformer() {
    skip();
    String s;
    read(s);
    read(s);
    if (s != "u") {
	float a[6];
	for (int i = 0; i < 6; i++) {
	    read(a[i]);
	}
        return new Transformer(a[0], a[1], a[2], a[3], a[4], a[5]);
    }
    return nil;
}
