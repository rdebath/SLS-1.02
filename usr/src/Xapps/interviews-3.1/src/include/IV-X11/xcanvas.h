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

#ifndef iv_xcanvas_h
#define iv_xcanvas_h

#include <InterViews/canvas.h>
#include <IV-X11/Xlib.h>
#include <IV-X11/Xutil.h>

#include <InterViews/_enter.h>

class ClippingStack;
class Display;
class TransformerStack;

class CanvasDamage {
public:
    Coord left;
    Coord bottom;
    Coord right;
    Coord top;
};

class TextRenderInfo {
public:
    CanvasRep* canvas_;
    GC drawgc_;
    int x0_;
    int y0_;
    Coord width_;
    Coord curx_;
    Coord cury_;
    char* text_;
    char* cur_text_;
    int spaces_;
    XTextItem* items_;
};

class PathRenderInfo {
public:
    Coord curx_;
    Coord cury_;
    XPoint* point_;
    XPoint* cur_point_;
    XPoint* end_point_;
};

class CanvasRep {
public:
    Display* display_;
    Window* window_;
    XDrawable xdrawable_;
    Coord width_;
    Coord height_;
    PixelCoord pwidth_;
    PixelCoord pheight_;

    boolean damaged_ : 1;
    boolean on_damage_list_ : 1;
    boolean repairing_ : 1;
    CanvasDamage damage_;

    XDrawable drawbuffer_;
    XDrawable copybuffer_;
    XRectangle clip_;
    const Brush* brush_;
    const Color* color_;
    const Font* font_;
    Region clipping_;
    Region empty_;
    GC drawgc_;
    GC copygc_;
    int op_;
    Pixmap stipple_;
    unsigned long pixel_;
    int brush_width_;
    char* dash_list_;
    int dash_count_;
    XFontStruct* xfont_;
    boolean text_twobyte_;
    boolean text_reencode_;
    boolean font_is_scaled_;
    boolean transformed_;
    TransformerStack* transformers_;
    ClippingStack* clippers_;

    static TextRenderInfo text_;
    static PathRenderInfo path_;

    enum { unbound = 0 };

    XDisplay* dpy() const;
    Transformer& matrix() const;

    void flush();
    void swapbuffers();
    void brush(const Brush*);
    void color(const Color*);
    void font(const Font*);

    void new_damage();
    void clear_damage();
    boolean start_repair();
    void finish_repair();

    void bind(boolean double_buffered);
    void unbind();

    /* for backward compability */
public:
    CanvasLocation status_;

    void wait_for_copy();
};

#include <InterViews/_leave.h>

#endif
