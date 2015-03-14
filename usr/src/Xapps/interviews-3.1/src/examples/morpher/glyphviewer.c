#include <InterViews/Bitmaps/enlargeHit.bm>
#include <InterViews/Bitmaps/enlargeMask.bm>
#include <InterViews/Bitmaps/reducerHit.bm>
#include <InterViews/Bitmaps/reducerMask.bm>
#include <InterViews/bitmap.h>

#include <Dispatch/dispatcher.h>
#include <Dispatch/iocallback.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/cursor.h>
#include <InterViews/event.h>
#include <InterViews/handler.h>
#include <InterViews/hit.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/target.h>
#include <InterViews/transformer.h>
#include <InterViews/window.h>
#include <OS/string.h>
#include <IV-look/kit.h>
#include <math.h>
#include "figure.h"
#include "globals.h"
#include "glyphviewer.h"

static Cursor* zoom_in_cursor = nil;
static Cursor* zoom_out_cursor = nil;
static double zoom_factor = 400.0;

class GlyphGrabber : public Handler {
public:
    GlyphGrabber(GlyphViewer*, Allocation*);
    virtual boolean event(Event&);
protected:
    GlyphViewer* _gviewer;
    Allocation* _a;
};

GlyphGrabber::GlyphGrabber (GlyphViewer* gviewer, Allocation* a) {
    _gviewer = gviewer;
    _a = a;
}

boolean GlyphGrabber::event (Event& e) {
    Graphic* root = _gviewer->root();
    Tool& tool = _gviewer->tool();
    Canvas* c = e.window()->canvas();
    c->push_clipping();
    c->clip_rect(_a->left(), _a->bottom(), _a->right(), _a->top());
    boolean ok = root->grasp(e, tool);
    Event ev;
    Session* s = Session::instance();
    while(ok) {
        s->read(ev);
        ok = root->manipulating(ev, tool);
    }
    ok = root->effect(ev, tool);
    c->pop_clipping();
    return ok;
}    

declareIOCallback(GlyphViewer)
implementIOCallback(GlyphViewer)

GlyphViewer::GlyphViewer(
    float w, float h
) : InputHandler(
    new Target(_root = new GraphicMaster ,TargetAlwaysHit), 
    Session::instance()->style()
) {
    Color* mg = new Color(0.7, 0.7, 0.7, 1.0);
    _root->background(mg->brightness(-0.33));
    delete mg;
    _x = -1.0;
    _y = -1.0;
    _lx = -1.0;
    _ly = -1.0;
    _width = w;
    _height = h;
    _grabber = new GlyphGrabber(this, &_a);
    _grabber->ref();

    _zoom = _pan = _grab = false;
    initshape();
    initgraphic();
    initcursor();
    inittimer();
}

GlyphViewer::~GlyphViewer () {
    delete _timer;
    _grabber->unref();
}

void GlyphViewer::initcursor () {    
    if (zoom_in_cursor == nil) {
        Bitmap* zoom_in = new Bitmap(
            enlarger_hit_bits, enlarger_hit_width, enlarger_hit_height,
            enlarger_hit_x_hot, enlarger_hit_y_hot
        );
        Bitmap* zoom_in_mask = new Bitmap(
            enlarger_mask_bits, enlarger_mask_width, enlarger_mask_height,
            enlarger_mask_x_hot, enlarger_mask_y_hot
        );
        
        Bitmap* zoom_out = new Bitmap(
            reducer_hit_bits, reducer_hit_width, reducer_hit_height,
            reducer_hit_x_hot, reducer_hit_y_hot
        );
        Bitmap* zoom_out_mask = new Bitmap(
            reducer_mask_bits, reducer_mask_width, reducer_mask_height,
            reducer_mask_x_hot, reducer_mask_y_hot
        );
        
        zoom_in_cursor = new Cursor(zoom_in, zoom_in_mask);
        zoom_out_cursor = new Cursor(zoom_out, zoom_out_mask);
    } 
    _cursor = nil;
}

void GlyphViewer::inittimer () {
    float seconds = 1.0;
    _delay = long(seconds * 1000000);
    _timer = new IOCallback(GlyphViewer)(this, &GlyphViewer::tick);
}

Tool& GlyphViewer::tool () {
    return _tool;
}

void GlyphViewer::cur_tool (unsigned int t) {
    _tool.tool(t);
}


GraphicMaster* GlyphViewer::root () { return _root; }

void GlyphViewer::root (GraphicMaster* root) {
    if (_root != root) {
        Extension ext;
        Allocation a;
        root->transformer(nil);
        root->background(_root->background());
        _root->allocate(nil, a, ext);
        _root = root;
        body(new Target(_root,TargetAlwaysHit));
        initgraphic();
        Canvas* c = canvas();
        if (c != nil) {
            Requisition req;
            //_root->flush();
            _root->request(req);
            _root->allocate(c, _a, ext);
        }
    }
}

void GlyphViewer::press(const Event& e) {
    Window* w = canvas()->window();
    _cursor = w->cursor();
    _zoom = _pan = false;
    _x = e.pointer_x();
    _y = e.pointer_y();
    Dispatcher::instance().startTimer(0, _delay/40, _timer);
}

void GlyphViewer::initshape () {
    if (_width < -0.5 && _height < -0.5) {
        Coord l, b, r, t;
        _root->getbounds(l, b, r, t);
        _width = r - l;
        _height = t - b;
    }
    Allotment& ax = _a.x_allotment();
    Allotment& ay = _a.y_allotment();
    ax.span(_width);
    ay.span(_height);
}

void GlyphViewer::initgraphic () {
    Coord l, b, r, t;
    _root->getbounds(l, b, r, t);
    _root->translate(-l-(r-l)/2.0, -b-(t-b)/2.0);
    Canvas* c = canvas();
    if (c != nil) {
        c->damage_all();
    }
}

void GlyphViewer::damage (Coord l, Coord b, Coord r, Coord t) {
    Canvas* c = canvas();
    if (c != nil) {
        c->damage(l, b, r, t);
    }
}

void GlyphViewer::repair () {
    Canvas* c = canvas();
    if (c != nil) {
        c->window()->repair();
    }
}
    
void GlyphViewer::drag(const Event& e) {
    _lx = e.pointer_x();
    _ly = e.pointer_y();
    if (_zoom == false && _pan == false && _grab == false) {
        if (e.left_is_down()) {
            _pan = true;
            rate_scroll();
        } else if (e.middle_is_down()) {
            _grab = true;
            grab_scroll();
        } else if (e.right_is_down()) {
            _zoom = true;
            rate_zoom();
        }
    } else if (_grab == true) {
        grab_scroll();
    } else if (_zoom == true) {
        rate_zoom();
    } else if (_pan == true) {
        rate_scroll();
    }
}

void GlyphViewer::release(const Event&) {
    Window* w = canvas()->window();
    w->cursor(_cursor);
    _zoom = _pan = _grab = false;
    //_root->flush();
    Dispatcher::instance().stopTimer(_timer);
}

void GlyphViewer::tick(long, long) {
    if (_zoom == true) {
        rate_zoom();
    } else if (_grab == true) {
        grab_scroll();
    } else if (_pan == true) {
        rate_scroll();
    }
    Dispatcher::instance().startTimer(0, _delay/40, _timer);
}

void GlyphViewer::grab_scroll () {
    Window* w = canvas()->window();
    WidgetKit* kit = WidgetKit::instance();
    
    Coord dx = _lx - _x;
    Coord dy = _ly - _y;
    
    if (dx != 0.0 || dy != 0.0) {
        Coord l, b, r, t;
        w->cursor(kit->hand_cursor());
        _root->getbounds(l, b, r, t);
        canvas()->damage(l, b, r, t);
        _root->translate(dx, dy);
        _root->getbounds(l, b, r, t);
        canvas()->damage(l, b, r, t);
        _x += dx;
        _y += dy;
    }
}

void GlyphViewer::rate_scroll () {        
    Window* w = canvas()->window();
    WidgetKit* kit = WidgetKit::instance();
    
    Coord dx = _x - _lx;
    Coord dy = _y - _ly;
    
    if (dx != 0.0 && dy != 0.0) {
        double angle = atan2(dy, dx)*180/M_PI;
        
        if (angle < -157.5) {
            w->cursor(kit->rfast_cursor());
        } else if (angle < -112.5) {
            w->cursor(kit->rufast_cursor());
        } else if (angle < -67.5) {
            w->cursor(kit->ufast_cursor());
        } else if (angle < -22.5) {
            w->cursor(kit->lufast_cursor());
        } else if (angle < 22.5) {
            w->cursor(kit->lfast_cursor());
        } else if (angle < 67.5) {
            w->cursor(kit->ldfast_cursor());
        } else if (angle < 112.5) {
            w->cursor(kit->dfast_cursor());
        } else if (angle < 157.5) {
            w->cursor(kit->rdfast_cursor());
        } else {
            w->cursor(kit->rfast_cursor());
        }
    }
    
    Coord l, b, r, t;
    _root->getbounds(l, b, r, t);
    canvas()->damage(l, b, r, t);
    _root->translate(dx, dy);
    _root->getbounds(l, b, r, t);
    canvas()->damage(l, b, r, t);
}
    
void GlyphViewer::rate_zoom () {        
    Window* w = canvas()->window();
    
    Coord dx = 0.0;
    Coord dy = _y - _ly;
    
    if (dy != 0.0) {
        double factor;
        if (dy > 0.0) {
            w->cursor(zoom_out_cursor);
            factor = zoom_factor/(zoom_factor+dy);
            
        } else {
            w->cursor(zoom_in_cursor);
            factor = (zoom_factor-dy)/zoom_factor;
        }
        Coord l, b, r, t;
        _root->getbounds(l, b, r, t);
        canvas()->damage(l, b, r, t);
        _root->scale(factor, factor, _x, _y);
        _root->getbounds(l, b, r, t);
        canvas()->damage(l, b, r, t);
    }
}
    
void GlyphViewer::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    _a = a;
    InputHandler::allocate(c, a, ext);
    ext.merge(c, a);
}

void GlyphViewer::request(Requisition& req) const {
    Requirement& rx = req.x_requirement();
    rx.natural(_width);
    rx.stretch(fil);
    rx.shrink(_width);
    rx.alignment(0.0);
    
    Requirement& ry = req.y_requirement();
    ry.natural(_height);
    ry.stretch(fil);
    ry.shrink(0.0);
    ry.alignment(0.0);
}

void GlyphViewer::draw(Canvas* c, const Allocation& a) const {
    _root->draw(c, a);
}

void GlyphViewer::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    const Event* e = h.event();
    if (e->control_is_down()) {
        InputHandler::pick(c, a, depth, h);
    } else if (e->type() == Event::down) {
        h.begin(depth, this, 0, _grabber);
        MonoGlyph::pick(c, a, depth, h);
        h.end();
    }
}

