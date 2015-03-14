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
 * Printer - draw for PostScript printer
 */

#include <InterViews/bitmap.h>
#include <InterViews/brush.h>
#include <InterViews/color.h>
#include <InterViews/font.h>
#include <InterViews/printer.h>
#include <InterViews/raster.h>
#include <InterViews/transformer.h>
#include <OS/list.h>
#include <OS/math.h>
#include <stream.h>
#include <string.h>

static const float PAGE_WIDTH = 8.5 * 72;
static const float PAGE_HEIGHT = 11 * 72;
static const float epsilon = 0.01;

static const char* ps_prolog = "\
save 20 dict begin\n\
\n\
/sf {   % scale /fontName => -  (set current font)\n\
    {findfont} stopped {pop /Courier findfont} if\n\
    exch scalefont setfont\n\
} def\n\
\n\
/ws {\n\
    4 index 6 4 roll moveto sub\n\
    2 index stringwidth pop sub\n\
    exch div 0 8#40 4 3 roll\n\
    widthshow\n\
} def\n\
\n\
/as {\n\
    4 index 6 4 roll moveto sub\n\
    2 index stringwidth pop sub\n\
    exch div 0 3 2 roll\n\
    ashow\n\
} def\n\
\n\
";

static const char* ps_epilog = "\
end restore\n\
";

class PrinterInfo {
public:
    const Color* color_;
    const Brush* brush_;
    const Font* font_;
};

class PrinterInfoList;

class PrinterRep {
public:
    ostream* out_;
    int page_;
    PrinterInfoList* info_;

    float x_;
    float y_;
    Coord text_curx_;
    Coord text_cury_;
    int text_chars_;
    int text_spaces_;
};

declareList(PrinterInfoList,PrinterInfo)
implementList(PrinterInfoList,PrinterInfo)

static void do_color(ostream& out, const Color* color) {
    float r, g, b;
    color->intensities(r, g, b);
    out << r << " " << g << " " << b << " setrgbcolor\n";
}

static void do_brush(ostream& out, const Brush* brush) {
    Coord linewidth = brush->width();
    out << linewidth << " setlinewidth\n";
}

static void do_font(ostream& out, const Font* font) {
    out << font->size() << " /";
    const char* p = font->name();
    while (*p != '\0') {
        out << char(*p == ' ' ? '-' : *p);
        ++p;
    }
    out << " sf\n";
}

Printer::Printer(ostream* out) {
    PrinterRep* p = new PrinterRep;
    rep_ = p;
    p->out_ = out;
    p->page_ = 1;
    p->x_ = 0;
    p->y_ = 0;
    p->text_curx_ = 0;
    p->text_cury_ = 0;
    p->text_chars_ = 0;
    p->text_spaces_ = 0;

    PrinterInfo info;
    info.color_ = nil;
    info.brush_ = nil;
    info.font_ = nil;

    p->info_ = new PrinterInfoList();
    p->info_->append(info);
}

Printer::~Printer() {
    flush();
    delete rep_->info_;
    delete rep_;
}

PixelCoord Printer::to_pixels(Coord p) const { return p; }
Coord Printer::to_coord(PixelCoord p) const { return p; }
Coord Printer::to_pixels_coord(Coord p) const { return p; }

void Printer::resize(Coord left, Coord bottom, Coord right, Coord top) {
    PrinterRep* p = rep_;
    ostream& out = *p->out_;
    p->x_ = (float(left + right) - PAGE_WIDTH)/2;
    p->y_ = (float(top + bottom) - PAGE_HEIGHT)/2;
    flush();
    size(right - left, top - bottom);
    damage(left, bottom, right, top);
}

void Printer::prolog(const char* creator) {
    ostream& out = *rep_->out_;
    out << "%!PS-Adobe-2.0\n";
    out << "%%Creator: " << creator << "\n";
    out << "%%Pages: atend\n";
    out << "%%EndComments\n";
    out << ps_prolog;
    out << "%%EndProlog\n";
}

void Printer::epilog () {
    PrinterRep* p = rep_;
    ostream& out = *p->out_;
    flush();
    out << "showpage\n";
    out << "%%Trailer\n";
    out << ps_epilog;
    out << "%%Pages: " << p->page_ - 1 << "\n";
}

void Printer::comment(const char* text) {
    ostream& out = *rep_->out_;
    flush();
    out << "%% " << text << "\n";
}

void Printer::page(const char* label) {
    PrinterRep* p = rep_;
    flush();
    if (p->page_ > 1) {
        *p->out_ << "showpage\n";
    }
    *p->out_ << "%%Page: " << label << " " << p->page_ << "\n";
    *p->out_ << -(p->x_) << " " << -(p->y_) << " translate\n";
    PrinterInfo& info = p->info_->item_ref(p->info_->count() - 1);
    info.font_ = nil;
    info.color_ = nil;
    info.brush_ = nil;
    p->page_ += 1;
}

void Printer::push_transform() {
    Canvas::push_transform();
    PrinterRep* p = rep_;
    flush();
    long depth = p->info_->count();
    PrinterInfo& info = p->info_->item_ref(depth - 1);
    p->info_->insert(depth, info);
    *p->out_ << "gsave\n";
}

void Printer::pop_transform() {
    PrinterRep* p = rep_;
    flush();
    long depth = p->info_->count();
    p->info_->remove(depth - 1);
    *p->out_ << "grestore\n";
    Canvas::pop_transform();
}

void Printer::transform(const Transformer& t) {
    Canvas::transform(t);
    PrinterRep* p = rep_;
    flush();
    float a00, a01, a10, a11, a20, a21;
    t.matrix(a00, a01, a10, a11, a20, a21);
    *p->out_ << "[" << a00 << " " << a01;
    *p->out_ << " " << a10 << " " << a11;
    *p->out_ << " " << a20 << " " << a21 << "] concat\n";
}

/*
 * The current transform push/pop just saves and restores state,
 * so we can use it for clipping as well.
 */

void Printer::push_clipping() {
    PrinterRep* p = rep_;
    flush();
    long depth = p->info_->count();
    PrinterInfo& info = p->info_->item_ref(depth - 1);
    p->info_->insert(depth, info);
    *p->out_ << "gsave\n";
}

void Printer::pop_clipping() {
    PrinterRep* p = rep_;
    flush();
    long depth = p->info_->count();
    p->info_->remove(depth - 1);
    *p->out_ << "grestore\n";
}

void Printer::new_path() {
    ostream& out = *rep_->out_;
    flush();
    out << "newpath\n";
}

void Printer::move_to(Coord x, Coord y) {
    ostream& out = *rep_->out_;
    flush();
    out << x << " " << y << " moveto\n";
}

void Printer::line_to(Coord x, Coord y) {
    ostream& out = *rep_->out_;
    flush();
    out << x << " " << y << " lineto\n";
}

void Printer::curve_to(
    Coord x, Coord y, Coord x1, Coord y1, Coord x2, Coord y2
) {
    ostream& out = *rep_->out_;
    flush();
    out << x1 << " " << y1 << " " << x2 << " " << y2 << " ";
    out << x << " " << y << " curveto\n";
}

void Printer::close_path() {
    ostream& out = *rep_->out_;
    flush();
    out << "closepath\n";
}

void Printer::stroke(const Color* color, const Brush* brush) {
    PrinterRep* p = rep_;
    ostream& out = *p->out_;
    flush();
    PrinterInfo& info = p->info_->item_ref(p->info_->count() - 1);
    if (info.color_ != color) {
        do_color(out, color);
        info.color_ = color;
    }
    if (info.brush_ != brush) {
        do_brush(out, brush);
        info.brush_ = brush;
    }
    out << "gsave stroke grestore\n";
}

void Printer::fill(const Color* color) {
    PrinterRep* p = rep_;
    ostream& out = *p->out_;
    flush();
    PrinterInfo& info = p->info_->item_ref(p->info_->count() - 1);
    if (info.color_ != color) {
        do_color(out, color);
        info.color_ = color;
    }
    out << "gsave eofill grestore\n";
}

void Printer::clip() {
    ostream& out = *rep_->out_;
    flush();
    out << "eoclip\n";
}

void Printer::character(
    const Font* font, long c, Coord width, const Color* color, Coord x, Coord y
) {
    PrinterRep* p = rep_;
    ostream& out = *p->out_;
    PrinterInfo& info = p->info_->item_ref(p->info_->count() - 1);
    if (info.color_ != color) {
        flush();
        do_color(out, color);
        info.color_ = color;
    }
    if (!Math::equal(y, p->text_cury_, epsilon)) {
        flush();
    }
    if (!Math::equal(x, p->text_curx_, epsilon)) {
        flush();
    }
    if (info.font_ != font) {
        flush();
        do_font(out, font);
        info.font_ = font;
    }
    if (p->text_chars_ == 0) {
        out << x << " " << y << "(";
    }
    p->text_curx_ = x + width;
    p->text_cury_ = y;
    if (c == '\\' || c == ')' || c == '(') {
        out << "\\" << char(c);
    } else if (c > 127) {
#ifdef __GNUC__
	out.form("\\%03o", c);
#else
	out << "\\";
        int old_width = out.width(3);
        char old_fill = out.fill('0');
        out << oct << c << dec;
        out.width(old_width);
        out.fill(old_fill);
#endif
    } else {
        out << char(c);
    }
    p->text_chars_ += 1;
    if (c == ' ') {
        p->text_spaces_ += 1;
    }
}

void Printer::flush() {
    PrinterRep* p = rep_;
    ostream& out = *p->out_;
    if (p->text_chars_ > 0) {
        out << ") ";
        if (p->text_spaces_ > 0) {
            out << p->text_spaces_ << " " << p->text_curx_ << " ws\n";
        } else {
            out << p->text_chars_ << " " << p->text_curx_ << " as\n";
        }
        p->text_chars_ = 0;
        p->text_spaces_ = 0;
    }
}

void Printer::stencil(
    const Bitmap* mask, const Color* color, Coord x, Coord y
) {
    PrinterRep* p = rep_;
    ostream& out = *p->out_;
    flush();
    PrinterInfo& info = p->info_->item_ref(p->info_->count() - 1);
    if (info.color_ != color) {
        do_color(out, color);
        info.color_ = color;
    }
    unsigned long width = mask->pwidth();
    unsigned long height = mask->pheight();
    unsigned long bytes = (width-1)/8 + 1;
    Coord left = x - mask->left_bearing();
    Coord right = x + mask->right_bearing();
    Coord bottom = y - mask->descent();
    Coord top = y + mask->ascent();
    out << "gsave\n";
    out << "/picstr " << bytes << " string def\n";
    out << left << " " << bottom << "  translate\n";
    out << right - left << " " << top - bottom << " scale\n";
    out << width << " " << height << " true\n";
    out << "[" << width << " 0 0 " << height << " 0 0]\n";
    out << "{currentfile picstr readhexstring pop} imagemask\n";
#ifndef __GNUC__
    int old_width = out.width(1);
    out << hex;
#endif
    for (int iy = 0; iy < height; ++iy) {
        for (int ix = 0; ix < bytes; ++ix) {
            int byte = 0;
            for (int bit = 0; bit < 8; ++bit) {
                if (mask->peek(ix*8 + bit, iy)) {
                    byte |= 0x80 >> bit;
                }
            }
#ifdef __GNUC__
	    out.form("%02x", byte);
#else
            out << ((byte>>4) & 0x0f) <<  (byte & 0x0f);
#endif
        }
        out << "\n";
    }
#ifndef __GNUC__
    out << dec;
    out.width(old_width);
#endif
    out << "grestore\n";
}

void Printer::image(const Raster* raster, Coord x, Coord y) {
    PrinterRep* p = rep_;
    ostream& out = *p->out_;
    flush();
    unsigned long width = raster->pwidth();
    unsigned long height = raster->pheight();
    float left = float(x) - raster->left_bearing();
    float right = float(x) + raster->right_bearing();
    float bottom = float(y) - raster->descent();
    float top = float(y) + raster->ascent();
    out << "gsave\n";
    out << "/picstr " << width << " string def\n";
    out << left << " " << bottom << "  translate\n";
    out << right - left << " " << top - bottom << " scale\n";
    out << width << " " << height << " 8\n";
    out << "[" << width << " 0 0 " << height << " 0 0]\n";
    out << "{currentfile picstr readhexstring pop} image\n";
#ifndef __GNUC__
    int old_width = out.width(1);
    out << hex;
#endif
    for (int iy = 0; iy < height; ++iy) {
        for (int ix = 0; ix < width; ++ix) {
            float r, g, b, alpha;
            raster->peek(ix, iy, r, g, b, alpha);
            int byte = int(0xff * (r + g + b) / 3);
#ifdef __GNUC__
	    out.form("%02x", byte);
#else
            out << ((byte>>4) & 0x0f) <<  (byte & 0x0f);
#endif
        }
        out << "\n";
    }
#ifndef __GNUC__
    out << dec;
    out.width(old_width);
#endif
    out << "grestore\n";
}
