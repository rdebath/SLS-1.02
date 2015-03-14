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

#ifndef iv_xwindow_h
#define iv_xwindow_h

#include <InterViews/boolean.h>
#include <InterViews/geometry.h>
#include <IV-X11/Xlib.h>
#include <IV-X11/Xutil.h>
#include <OS/list.h>

#include <InterViews/_enter.h>

class Bitmap;
class ColorTable;
class Cursor;
class Display;
class Glyph;
class Handler;
class ManagedWindow;
class ManagedWindowRep;
class RGBTable;
class String;
class Style;
class WindowCursorStack;
class WindowTable;
class XDisplay;

struct WindowOverlayInfo {
    VisualID id_;
    long type_;
    long transparent_;
    long layer_;
};

struct WindowVisualInfo {
    XDisplay* display_;
    int screen_;
    int depth_;
    Visual* visual_;
    XColormap cmap_;
    WindowOverlayInfo overlay_;
};

class WindowVisual {
public:
    WindowVisual(const WindowVisualInfo&);
    ~WindowVisual();

    static WindowVisual* find_visual(Display*, Style*);

    void init_color_tables();
    void find_color(unsigned long, XColor&);
    void find_color(
	unsigned short r, unsigned short g, unsigned short b, XColor&
    );

    unsigned long xor(const Style&) const;

    XDisplay* display() const;
    int screen() const;
    int depth() const;
    Visual* visual() const;
    XColormap colormap() const;
private:
    WindowVisualInfo info_;
    ColorTable* ctable_;
    RGBTable* rgbtable_;
    XColor* localmap_;
    unsigned int localmapsize_;
    unsigned long red_;
    unsigned long red_shift_;
    unsigned long green_;
    unsigned long green_shift_;
    unsigned long blue_;
    unsigned long blue_shift_;
    unsigned long white_;
    unsigned long xor_;

    static void find_visual_by_class_name(const String&, WindowVisualInfo&);
    static boolean find_layer(const String&, int& layer);
    static void find_overlay(int layer, WindowVisualInfo&);
    static void find_visual_by_info(
	XVisualInfo&, long mask, WindowVisualInfo&
    );

    void set_shift(unsigned long mask, unsigned long& v, unsigned long& shift);

    static unsigned int MSB(unsigned long);
    static double distance(
	unsigned short r, unsigned short g, unsigned short b, const XColor&
    );
    static unsigned long rescale(
	unsigned long value, unsigned long in_scale, unsigned long out_scale
    );
};

inline XDisplay* WindowVisual::display() const { return info_.display_; }
inline int WindowVisual::screen() const { return info_.screen_; }
inline int WindowVisual::depth() const { return info_.depth_; }
inline Visual* WindowVisual::visual() const { return info_.visual_; }
inline XColormap WindowVisual::colormap() const { return info_.cmap_; }

declarePtrList(WindowVisualList,WindowVisual)

class WindowRep {
public:
    Glyph* glyph_;
    Style* style_;
    Display* display_;
    WindowVisual* visual_;
    Canvas* canvas_;
    Requisition shape_;
    Allocation allocation_;
    Cursor* cursor_;
    WindowCursorStack* cursor_stack_;
    Coord left_;
    Coord bottom_;
    float xalign_;
    float yalign_;

    Handler* focus_in_;
    Handler* focus_out_;
    Handler* wm_delete_;

    XWindow xwindow_;
    XSetWindowAttributes xattrs_;
    unsigned long xattrmask_;
    unsigned int xclass_;
    int xpos_;
    int ypos_;

    Window* toplevel_;
    XWindow xtoplevel_;

    boolean placed_ : 1;
    boolean aligned_ : 1;
    boolean needs_resize_ : 1;
    boolean resized_ : 1;
    boolean moved_ : 1;
    boolean unmapped_ : 1;
    boolean wm_mapped_ : 1;
    boolean map_pending_ : 1;

    static Atom wm_delete_atom_;
    static Atom wm_protocols_atom_;

    enum { unbound = 0 };

    XDisplay* dpy();
    Atom wm_delete_atom();
    Atom wm_protocols_atom();
    void clear_mapping_info();
    void map_notify(Window*, XMapEvent&);
    void unmap_notify(Window*, XUnmapEvent&);
    void expose(Window*, XExposeEvent&);
    void configure_notify(Window*, XConfigureEvent&);
    void move(Window*, int x, int y);
    void resize(Window*, unsigned int w, unsigned int h);
    void check_position(const Window*);

    void check_binding(Window*);
    void do_bind(Window*, XWindow parent, int left, int top);
    void init_renderer(Window*);

    static Window* find(XWindow, WindowTable*);
};

class ManagedWindowHintInfo {
private:
    friend class ManagedWindowRep;

    Style* style_;
    XWMHints* hints_;
    XDisplay* dpy_;
    XWindow xwindow_;
    unsigned int pwidth_;
    unsigned int pheight_;
    Display* display_;
};

typedef boolean (ManagedWindowRep::*HintFunction)(ManagedWindowHintInfo&);

class ManagedWindowRep {
public:
    ManagedWindow* icon_;
    Bitmap* icon_bitmap_;
    Bitmap* icon_mask_;
    Window* group_leader_;
    Window* transient_for_;

    void do_set(Window*, HintFunction);

    boolean set_name(ManagedWindowHintInfo&);
    boolean set_geometry(ManagedWindowHintInfo&);
    boolean set_group_leader(ManagedWindowHintInfo&);
    boolean set_transient_for(ManagedWindowHintInfo&);
    boolean set_icon_name(ManagedWindowHintInfo&);
    boolean set_icon_geometry(ManagedWindowHintInfo&);
    boolean set_icon(ManagedWindowHintInfo&);
    boolean set_icon_bitmap(ManagedWindowHintInfo&);
    boolean set_icon_mask(ManagedWindowHintInfo&);
    boolean set_all(ManagedWindowHintInfo&);

    void wm_normal_hints(Window*);
    void wm_name(Window*);
    void wm_class(Window*);
    void wm_protocols(Window*);
    void wm_colormap_windows(Window*);
    void wm_hints(Window*);
};

#include <InterViews/_leave.h>

#endif
