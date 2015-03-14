#include "morpher.h"
#include "figure.h"
#include "glypheditor.h"
#include "glyphviewer.h"
#include <InterViews/canvas.h>
#include <InterViews/brush.h>
#include <InterViews/color.h>
#include <InterViews/display.h>
#include <InterViews/transformer.h>
#include <InterViews/window.h>
#include <IV-X11/xcanvas.h>
#include <OS/list.h>
#include <Dispatch/dispatcher.h>
#include <Dispatch/iocallback.h>
#include <math.h>
#include <stdlib.h>

class MorphInc {
public:
    MorphInc();
    virtual ~MorphInc();

    void cleanup();

    float _alpha_stroke_src;
    float _alpha_fill_src;
    float _alpha_stroke_dest;
    float _alpha_fill_dest;
    boolean _same_stroke;
    boolean _same_fill;
    Color* _stroke_dest;
    Color* _fill_dest;
    Coord* _x;
    Coord* _y;
    Transformer _t;
};

MorphInc::MorphInc () {
    _x = nil;
    _y = nil;
    _stroke_dest = nil;
    _fill_dest = nil;
    cleanup();
}

MorphInc::~MorphInc () {
    cleanup();
}

void MorphInc::cleanup () {
    delete _x;
    delete _y;
    _x = nil;
    _y = nil;
    _alpha_stroke_src = 0.0;
    _alpha_fill_src = 0.0;
    _alpha_stroke_dest = 0.0;
    _alpha_fill_dest = 0.0;
    Resource::unref(_stroke_dest);
    Resource::unref(_fill_dest);
    _stroke_dest = nil;
    _fill_dest = nil;
    _same_stroke = false;
    _same_fill = false;
    Transformer t;
    *(&_t) = *(&t);
}

class MorphGraphic : public Graphic {
public:
    MorphGraphic(Graphic* src = nil, Graphic* dest = nil, long steps = 0);
    virtual ~MorphGraphic();

    void reinit(Graphic* src = nil, Graphic* dest = nil, long steps = 0);
    void forward(long = 1);
    void reverse(long = 1);
    boolean done();
    long total_steps();
    long counter();
    virtual void draw_gs (Canvas*, Graphic*);
    virtual Graphic& operator = (Graphic&);
protected:
    void expandLine(Coord*, Coord*, int, Coord*, Coord*, int, boolean);
    void expandSpline(Coord*, Coord*, int, Coord*, Coord*, int, boolean);
protected:
    Graphic* _src;
    Graphic* _dest;
    MorphInc _morph_inc;
    long _steps;
    long _count;
};

inline long MorphGraphic::total_steps () { return _steps; }
inline long MorphGraphic::counter () { return _count; }
inline boolean MorphGraphic::done () { 
    return _steps == _count || _count == 0; 
}

MorphGraphic::MorphGraphic(Graphic* src, Graphic* dest, long steps) {
    _src = nil;
    _dest = nil;
    reinit(src, dest, steps);
}

MorphGraphic::~MorphGraphic () {
    Resource::unref(_src);
    Resource::unref(_dest);
}

Graphic& MorphGraphic::operator = (Graphic& g) {
    Graphic::operator = (g);
    _curved = g.curved();
    _closed = g.closed();
    return *this;
}
        
void MorphGraphic::forward (long steps) {
    if (_count+steps <= _steps) {
        _count += steps;
    } else {
        return;
    }
    for (int i = 0; i < _ctrlpts; i++) {
        _x[i] += _morph_inc._x[i]*steps;
        _y[i] += _morph_inc._y[i]*steps;
    }
    recompute_shape();
    if (!_morph_inc._same_stroke && _stroke != nil) {
        stroke(
            new Color(
                *_stroke, _stroke->alpha()+_morph_inc._alpha_stroke_src*steps
            )
        );
    }
    if (!_morph_inc._same_fill && _fill != nil) {
        fill(
            new Color(*_fill, _fill->alpha()+_morph_inc._alpha_fill_src*steps)
        );
    }
    Color* stroke_dest = _morph_inc._stroke_dest;
    if (!_morph_inc._same_stroke && stroke_dest != nil) {
        _morph_inc._stroke_dest = new Color(
            *stroke_dest, stroke_dest->alpha()+
            _morph_inc._alpha_stroke_dest*steps
        );
        _morph_inc._stroke_dest->ref();
        stroke_dest->unref();
    }
    Color* fill_dest = _morph_inc._fill_dest;
    if (!_morph_inc._same_fill && fill_dest != nil) {
        _morph_inc._fill_dest = new Color(
            *fill_dest, fill_dest->alpha()+_morph_inc._alpha_fill_dest*steps
        );
        _morph_inc._fill_dest->ref();
        fill_dest->unref();
    }
        
    Coord a00 = 1.0, a01 = 0.0, a10= 0.0, a11 = 1.0, a20 = 0.0, a21 = 0.0;
    Coord b00 = 1.0, b01 = 0.0, b10= 0.0, b11 = 1.0, b20 = 0.0, b21 = 0.0;
    _t->matrix(a00, a01, a10, a11, a20, a21);
    _morph_inc._t.matrix(b00, b01, b10, b11, b20, b21);
    a00 += b00*steps;
    a01 += b01*steps;
    a10 += b10*steps;
    a11 += b11*steps;
    a20 += b20*steps;
    a21 += b21*steps;
    Transformer t(a00, a01, a10, a11, a20, a21);
    *_t = t;
}

void MorphGraphic::reverse (long steps) {
    if (_count-steps >= 0) {
        _count -= steps;
    } else {
        return;
    }
    for (int i = 0; i < _ctrlpts; i++) {
        _x[i] -= _morph_inc._x[i]*steps;
        _y[i] -= _morph_inc._y[i]*steps;
    }
    recompute_shape();
    if (!_morph_inc._same_stroke && _stroke != nil) {
        stroke(
            new Color(
                *_stroke, _stroke->alpha()-_morph_inc._alpha_stroke_src*steps
            )
        );
    }
    if (!_morph_inc._same_fill && _fill != nil) {
        fill(
            new Color(*_fill, _fill->alpha()-_morph_inc._alpha_fill_src*steps)
        );
    }
    Color* stroke_dest = _morph_inc._stroke_dest;
    if (!_morph_inc._same_stroke && stroke_dest != nil) {
        _morph_inc._stroke_dest = new Color(
            *stroke_dest, stroke_dest->alpha()-
            _morph_inc._alpha_stroke_dest*steps
        );
        _morph_inc._stroke_dest->ref();
        stroke_dest->unref();
    }
    Color* fill_dest = _morph_inc._fill_dest;
    if (!_morph_inc._same_fill && fill_dest != nil) {
        _morph_inc._fill_dest = new Color(
            *fill_dest, fill_dest->alpha()-_morph_inc._alpha_fill_dest*steps
        );
        _morph_inc._fill_dest->ref();
        fill_dest->unref();
    }
    Coord a00 = 1.0, a01 = 0.0, a10= 0.0, a11 = 1.0, a20 = 0.0, a21 = 0.0;
    Coord b00 = 1.0, b01 = 0.0, b10= 0.0, b11 = 1.0, b20 = 0.0, b21 = 0.0;
    _t->matrix(a00, a01, a10, a11, a20, a21);
    _morph_inc._t.matrix(b00, b01, b10, b11, b20, b21);
    a00 -= b00*steps;
    a01 -= b01*steps;
    a10 -= b10*steps;
    a11 -= b11*steps;
    a20 -= b20*steps;
    a21 -= b21*steps;
    Transformer t(a00, a01, a10, a11, a20, a21);
    *_t = t;
}

void MorphGraphic::reinit (Graphic* src, Graphic* dest, long steps) {
    Resource::ref(src);
    Resource::ref(dest);
    Resource::unref(_src);
    Resource::unref(_dest);
    _morph_inc.cleanup();
    _src = src;
    _dest = dest;
    _steps = steps;
    _count = 0;
    
    if (_src != nil && _dest != nil) {
        Coord* x0, *y0, *x1, *y1;
        
        *this = *src;

        const Color* dstroke = _dest->stroke();
        if (
            dstroke != nil && _stroke != nil && 
            _brush != nil && _dest->brush() != nil &&
            (_brush->width() == _dest->brush()->width()) && 
            (_closed == _dest->closed())
        ) {
            ColorIntensity rs, gs, bs;
            ColorIntensity rd, gd, bd;

            dstroke->intensities(rd, gd, bd);
            _stroke->intensities(rs, gs, bs);
            if (rd == rs && gd == gs && bd == bs) {
                _morph_inc._same_stroke = true;
            }
        }
        
        if (!_morph_inc._same_stroke) {
            if (dstroke != nil) {
                _morph_inc._stroke_dest = new Color(*dstroke, 0.0);
                _morph_inc._stroke_dest->ref();
                _morph_inc._alpha_stroke_dest = dstroke->alpha()/(float)steps;
            }
            if (_stroke != nil) {
                _morph_inc._alpha_stroke_src = -_stroke->alpha()/(float)steps;
            }
        }
        const Color* dfill = _dest->fill();
        if (dfill != nil && _fill != nil) {
            ColorIntensity rs, gs, bs;
            ColorIntensity rd, gd, bd;

            dfill->intensities(rd, gd, bd);
            _fill->intensities(rs, gs, bs);
            if (rd == rs && gd == gs && bd == bs) {
                _morph_inc._same_fill = true;
            }
        }

        if (!_morph_inc._same_fill) {
            if (dfill != nil) {
                _morph_inc._fill_dest = new Color(*dfill, 0.0);
                _morph_inc._fill_dest->ref();
                _morph_inc._alpha_fill_dest = dfill->alpha()/(float)steps;
            }
            
            if (_fill != nil) {
                _morph_inc._alpha_fill_src = -_fill->alpha()/(float)steps;
            }
        }
        
        int count0 = _src->ctrlpts(x0, y0);
        int count1 = _dest->ctrlpts(x1, y1);

        _curved = _src->curved();
        if (_dest->curved()) {
            _curved = true;
        }

        static Coord sx[1000];
        static Coord sy[1000];

        if (!_src->curved() && _dest->curved()) {
            sx[0] = x0[0];
            sy[0] = y0[0];
            int k = 1;
            for (int j = 0; j < count0; j++) {
                sx[k] = x0[j];
                sy[k++] = y0[j];
                sx[k] = x0[j];
                sy[k++] = y0[j];
                sx[k] = x0[j];
                sy[k++] = y0[j];
            }
            count0 = k;
            x0 = sx;
            y0 = sy;
        } else if (_src->curved() && !_dest->curved()) {
            sx[0] = x1[0];
            sy[0] = y1[0];
            int k = 1;
            for (int j = 0; j < count1; j++) {
                sx[k] = x1[j];
                sy[k++] = y1[j];
                sx[k] = x1[j];
                sy[k++] = y1[j];
                sx[k] = x1[j];
                sy[k++] = y1[j];
            }
            count1 = k;
            x1 = sx;
            y1 = sy;
        }
        int count = max(count0, count1);
        _morph_inc._x = new Coord[count];
        _morph_inc._y = new Coord[count];

        delete _x;
        delete _y;
        _x = new Coord[count];
        _y = new Coord[count];
        _buf_size = count;
        _ctrlpts = count;
        if (count0 > count1) {
            if (!_curved) {
                expandLine(
                    x1, y1, count1, _morph_inc._x, _morph_inc._y, count,
                    _dest->closed()
                );
            } else {
                expandSpline(
                    x1, y1, count1, _morph_inc._x, _morph_inc._y, count,
                    _dest->closed()
                );
            }
            x1 = _morph_inc._x;
            y1 = _morph_inc._y;
        } else {
            if (!_curved) {
                expandLine(
                    x0, y0, count0, _morph_inc._x, _morph_inc._y, count,
                    _src->closed()
                );
            } else {
                expandSpline(
                    x0, y0, count0, _morph_inc._x, _morph_inc._y, count,
                    _src->closed()
                );
            }
            x0 = _morph_inc._x;
            y0 = _morph_inc._y;
        }
        for (int i = 0; i < _ctrlpts; i++) {
            _x[i] = x0[i];
            _y[i] = y0[i];
            _morph_inc._x[i] = (x1[i]-x0[i])/(float)steps;
            _morph_inc._y[i] = (y1[i]-y0[i])/(float)steps;
        }
        Coord a00 = 1.0, a01 = 0.0, a10= 0.0, a11 = 1.0, a20 = 0.0, a21 = 0.0;
        Coord b00 = 1.0, b01 = 0.0, b10= 0.0, b11 = 1.0, b20 = 0.0, b21 = 0.0;
        if (_src->transformer() != nil) {
            _src->transformer()->matrix(a00, a01, a10, a11, a20, a21);
        }
        if (_dest->transformer() != nil) {
            _dest->transformer()->matrix(b00, b01, b10, b11, b20, b21);
        }
        a00 = (b00-a00)/(float)steps;
        a01 = (b01-a01)/(float)steps;
        a10 = (b10-a10)/(float)steps;
        a11 = (b11-a11)/(float)steps;
        a20 = (b20-a20)/(float)steps;
        a21 = (b21-a21)/(float)steps;
        Transformer t(a00, a01, a10, a11, a20, a21);
        _morph_inc._t = t;
        recompute_shape();
    }
}
    
void MorphGraphic::draw_gs (Canvas* c, Graphic* gs) {
    const Brush* brush = gs->brush();
    const Color* stroke = gs->stroke();
    const Color* fill = gs->fill();
    Transformer* tx = gs->transformer();
    if (tx != nil) {
        c->push_transform();
        c->transform(*tx);
    }
    c->new_path();
    c->move_to(_x[0], _y[0]);
    if (_curved) {
        for (int i = 1; i < _ctrlpts; i += 3) {
            c->curve_to(
                _x[i + 2], _y[i + 2],
                _x[i], _y[i],
                _x[i + 1], _y[i + 1]
            );
        }
    } else {
        for (int i = 1; i < _ctrlpts; ++i) {
            c->line_to(_x[i], _y[i]);
        }
    }

    if (_morph_inc._same_stroke) {
        if (_closed) {
            c->close_path();
        }
        if (brush != nil && stroke != nil) {
            c->stroke(stroke, brush);
        }
    } else {
        Color* dest_stroke = _morph_inc._stroke_dest;
        const Brush* dest_brush = _dest->brush();
        
        if (_closed && !_dest->closed()) {
            if (dest_brush != nil && dest_stroke != nil) {
                c->stroke(dest_stroke, dest_brush);
            }
            c->close_path();
            if (brush != nil && stroke != nil) {
                c->stroke(stroke, brush);
            }
        } else if (!_closed && _dest->closed()) {
            if (brush != nil && stroke != nil) {
                c->stroke(stroke, brush);
            }
            c->close_path();
            if (dest_brush != nil && dest_stroke != nil) {
                c->stroke(dest_stroke, dest_brush);
            }
        } else if (!_closed && !_dest->closed()) {
            if (brush != nil && stroke != nil) {
                c->stroke(stroke, brush);
            }
            if (dest_brush != nil && dest_stroke != nil) {
                c->stroke(dest_stroke, dest_brush);
            }
        } else {
            c->close_path();
            if (brush != nil && stroke != nil) {
                c->stroke(stroke, brush);
            }
            if (dest_brush != nil && dest_stroke != nil) {
                c->stroke(dest_stroke, dest_brush);
            }
        }
    }
    if (fill != nil) {
        c->fill(fill);
    }
    if (!_morph_inc._same_fill) {
        Color* dest_fill = _morph_inc._fill_dest;
        if (dest_fill != nil) {
            c->fill(dest_fill);
        }
    }
    if (tx != nil) {
        c->pop_transform();
    }
}

void MorphGraphic::expandLine (
    Coord* x0, Coord* y0, int count0, Coord* x1, Coord* y1, int count1,
    boolean closed
) {
    int slot;
    int diff = count1 - count0;
    if (closed) {
        slot = diff/count0+1;
    } else {
        slot = diff/(count0-1)+1;
    }
    int j = 0;
    int total = 0;
    for (int i = 0; i < count0; i++) {
        x1[j] = x0[i];
        y1[j++] = y0[i];
        for (int k = 0;total < diff && k < slot; k++) {
            float factor = (float)(k+1)/(float)(slot+1);
            if ((i+1) == count0) {
                x1[j] = (x0[0]-x0[i])*factor + x0[i];
                y1[j++] = (y0[0]-y0[i])*factor + y0[i];
            } else {
                x1[j] = (x0[i+1]-x0[i])*factor + x0[i];
                y1[j++] = (y0[i+1]-y0[i])*factor + y0[i];
            }
            total++;
        }
    }
}
        
void MorphGraphic::expandSpline (
    Coord* x0, Coord* y0, int count0, Coord* x1, Coord* y1, int count1,
    boolean closed
) {
    int slot;
    int diff = count1 - count0;
    if (closed) {
        slot = (int)(diff/count0/3.0)+1;
    } else {
        slot = (int)(diff/(count0-1)/3.0)+1;
    }
    int j = 0;
    int total = 0;
    for (int i = 0; i < count0; i++) {
        x1[j] = x0[i];
        y1[j++] = y0[i];
        float factor = 1.0/(float)(slot+1);
        for (int k = 0;total+3 <= diff && k < slot; k++) {
            float xfactor = (k+1)*factor;
            if ((i+1) == count0) {
                x1[j] = (x0[0]-x0[i])*xfactor+x0[i];
                y1[j] = (y0[0]-y0[i])*xfactor+y0[i];
            } else {
                x1[j] = (x0[i+1]-x0[i])*xfactor+x0[i];
                y1[j] = (y0[i+1]-y0[i])*xfactor+y0[i];
            }
            x1[j+1] = (x1[j]+x0[i])/2.0;
            y1[j+1] = (y1[j]+y0[i])/2.0;
            x1[j+2] = 2*x1[j]-x1[j+1];
            y1[j+2] = 2*y1[j]-y1[j+1];

            total += 3;
            j += 3;
        }
    }
    diff -= total;
    for (j = 0; j < diff; j++) {
        x1[j] = x0[i-1];
        y1[j] = y0[i-1];
    }
}
        
declarePtrList(GraphicList, Graphic);
implementPtrList(GraphicList, Graphic);
declarePtrList(MGraphicList, MorphGraphic);
implementPtrList(MGraphicList, MorphGraphic);
declarePtrList(MMGraphicList, MGraphicList);
implementPtrList(MMGraphicList, MGraphicList);
declareIOCallback(Morpher);
implementIOCallback(Morpher);

class MorpherImpl {
public:
    MorpherImpl(long size, long steps, GlyphEditor*, IOHandler*);
    virtual ~MorpherImpl();

    void init();
    void new_session();
    void clean_lists();
    void interpolate(Graphic*, Graphic*);

    GraphicList* _glist;
    MMGraphicList* _mmglist;
    GraphicMaster* _gmaster;

    GlyphEditor* _ged;
    IOHandler* _callback;

    unsigned int _mode;
    long _count;
    long _steps;
};
    
MorpherImpl::MorpherImpl (
    long size, long steps, GlyphEditor* ged, IOHandler* callback
) {
    _glist = new GraphicList(size);
    _mmglist = new MMGraphicList(8);
    _ged = ged;
    Resource::ref(_ged);
    _callback = callback;
    _mode = Morpher::p;
    _steps = steps;
    _count = 0;
    _gmaster = new GraphicMaster;
    _gmaster->ref();
}
 
MorpherImpl::~MorpherImpl () {
    Resource::unref(_ged);
    _gmaster->unref();
    delete _callback;
}

void MorpherImpl::clean_lists () {
    int i, j;
    for (i = 0; i < _glist->count(); i++) {
        _glist->item(i)->unref();
    }
    delete _glist;
    _glist = nil;
    for (i = 0; i < _mmglist->count(); i++) {
        MGraphicList* ml = _mmglist->item(i);
        for (j = 0; j < ml->count(); j++) {
            ml->item(j)->unref();
        }
        delete ml;
    }
    delete _mmglist;
    _mmglist = nil;
}

void MorpherImpl::interpolate (Graphic* src, Graphic* dest) {
    MGraphicList* cur = _mmglist->item(_mmglist->count()-1);
    int src_count = (int)src->count_();
    int dest_count = (int)dest->count_();
    if (src_count == 0 && dest_count == 0) {
        MorphGraphic* mgr = new MorphGraphic(src, dest, _steps);
        mgr->ref();
        cur->append(mgr);

    } else if (src_count > 0 && dest_count > 0) {
        int max_count = max(src_count, dest_count);
        for (int i = 0; i < max_count; i++) {
            int src_i = min(i, src_count-1);
            int dest_i = min(i, dest_count-1);
            interpolate(src->component_(src_i), dest->component_(dest_i));
        }
    } else if (src_count == 0) {
        for (int i = 0; i < dest_count; i++) {
            interpolate(src, dest->component_(i));
        }
    } else {
        for (int i = 0; i < src_count; i++) {
            interpolate(src->component_(i), dest);
        }
    }
}
    
void MorpherImpl::init () {
    long count = _glist->count();
    if (count > 1) {
        MGraphicList* mglist = new MGraphicList;
        _mmglist->append(mglist);
        interpolate(_glist->item(count-2), _glist->item(count-1));
        if (count == 2) {
            for (int i = 0; i < mglist->count(); i++) {
                _gmaster->append_(mglist->item(i));
            }
        }
    } 
}

void MorpherImpl::new_session () {
    _ged->viewer()->root(new GraphicMaster);
    _ged->viewer()->redraw();
    clean_lists();
    _glist = new GraphicList;
    _mmglist = new MMGraphicList;
    _gmaster->unref();
    _gmaster = new GraphicMaster;
    _gmaster->ref();
    _count = 0;
}

Morpher::Morpher (long size, long steps, GlyphEditor* ged) {
    _m_impl = new MorpherImpl(
        size, steps, ged, new IOCallback(Morpher)(this, &Morpher::tick)
    );
}
 
Morpher::~Morpher () {
    delete _m_impl;
}

void Morpher::init () {
    _m_impl->init();
}

void Morpher::new_session () {
    _m_impl->new_session();
}

void Morpher::execute (unsigned int code) {
    if (_m_impl->_mode != code) {
        _m_impl->_mode = code;
        
        switch(code) {
        case fr:
        case rp:
        case rps:
        case ff:
        case fp:
        case fps:
        case r:
            {
                Dispatcher::instance().startTimer(
                    0, 100000, _m_impl->_callback
                );
                if (code == r) {
                    append(_m_impl->_ged->viewer()->root());
                    init();
                } else {
                    _m_impl->_ged->viewer()->root(_m_impl->_gmaster);
                }
            }
            break;
        case cp:
            {
                _m_impl->_ged->viewer()->root(_m_impl->_gmaster);
                if (_m_impl->_count == _m_impl->_mmglist->count()) {
                    Coord l, b, r, t;
                    GlyphEditor* ged = _m_impl->_ged;
                    Graphic* mgr = ged->viewer()->root();
                    Window* w = ged->window();
                    Canvas* c = w->canvas();
                    mgr->getbounds(l, b, r, t);
                    c->damage(l, b, r, t);
                    rewind();
                }
                Dispatcher::instance().startTimer(
                    0, 100000, _m_impl->_callback
                );
            }
            break;
        case p:
            Dispatcher::instance().stopTimer(_m_impl->_callback);
            break;
        default:
            break;
        }
    }
}

void Morpher::rewind() {
    for (int i = 0; i < _m_impl->_mmglist->count(); i++) {
        MGraphicList* mglist = _m_impl->_mmglist->item(i);
        for (int j = 0; j < mglist->count(); j++) {
            MorphGraphic* mgr = mglist->item(j);
            mgr->reverse(mgr->counter());
        }
    }
    _m_impl->_count = 0;
}

void Morpher::tick (long, long) {
    if (_m_impl->_mode == r || _m_impl->_mmglist->count() == 0) {
        _m_impl->_ged->pause();
        return;
    }
    int code = _m_impl->_mode;
    long s_timer = 100000;
    long rate = s_timer/3;
    if (code == fr || code == ff) {
        rate = s_timer/10;
    }
    Coord l, b, r, t;
    GlyphEditor* ged = _m_impl->_ged;
    Graphic* root = ged->viewer()->root();
    MGraphicList* mglist = _m_impl->_mmglist->item(_m_impl->_count);
    long gcount = mglist->count();
    Window* w = ged->window();
    Canvas* c = w->canvas();
    switch(code) {
    case fr:
    case rp:
    case rps:
    case fp:
    case ff:
    case fps:
    case cp:
        {
            MorphGraphic* mgr = nil;
            root->getbounds(l, b, r, t);
            c->damage(l, b, r, t);
            for (int i = 0; i < gcount; i++) {
                mgr = mglist->item(i);
                if (code == fr || code == rp || code == rps) {
                    mgr->reverse();
                } else {
                    mgr->forward();
                }
            }
            root->getbounds(l, b, r, t);
            c->damage(l, b, r, t);
            CanvasRep& rep = *c->rep();
            CanvasDamage& cd = rep.damage_;
            rep.start_repair();
            root->drawclipped(c, cd.left, cd.bottom, cd.right, cd.top);
            rep.finish_repair();
            w->display()->flush();

            if (!mgr->done()) {
                if (code == fps || code == rps) {
                    _m_impl->_ged->pause();
                } else {                
                    Dispatcher::instance().startTimer(
                        0, rate, _m_impl->_callback
                    );
                }
            } else {
                boolean done = false;
                if (code == fr || code == rp || code == rps) {
                    _m_impl->_count--;
                    if (_m_impl->_count < 0) {
                        _m_impl->_count++;
                        done = true;
                    } 
                } else {
                    _m_impl->_count++;
                    if (_m_impl->_count == _m_impl->_mmglist->count()) {
                        _m_impl->_count--;
                        done = true;
                    } 
                }
                if (!done || code == cp) {
                    gcount = root->count_();
                    root->getbounds(l, b, r, t);
                    c->damage(l, b, r, t);
                    for (int i = 0; i < gcount; i++) {
                        root->remove_(0);
                    }
                    if (done && code == cp) {
                        mglist = _m_impl->_mmglist->item(0);
                        rewind();
                    } else {
                        mglist = _m_impl->_mmglist->item(_m_impl->_count);
                    }
                    gcount = mglist->count();
                    for (i = 0; i < gcount; i++) {
                        root->append_(mglist->item(i));
                    }
                    if (code == fps || code == rps) {
                        _m_impl->_ged->pause();
                    } else {
                        Dispatcher::instance().startTimer(
                            0, rate, _m_impl->_callback
                        );
                    }
                } else {
                    _m_impl->_ged->pause();
                }
            }
            break;
        }
    default:
        break;
    }
}
        
long Morpher::count () {
    return _m_impl->_glist->count();
}

Graphic* Morpher::item (long index) const {
    return _m_impl->_glist->item(index);
}

void Morpher::prepend (Graphic* g) {
    g->ref();
    _m_impl->_glist->prepend(g);
}

void Morpher::append (Graphic* g) {
    g->ref();
    _m_impl->_glist->append(g);
}

void Morpher::insert (long index, Graphic* g) {
    g->ref();
    _m_impl->_glist->insert(index, g);
}

void Morpher::remove (long index) {
    Graphic* g = item(index);
    if (g != nil) {
        g->unref();
        _m_impl->_glist->remove(index);
    }
}

void Morpher::remove_all () {
    long count = _m_impl->_glist->count();
    for(int i = 0; i < count; i++) {
        _m_impl->_glist->item(i)->unref();
    }
    _m_impl->_glist->remove_all();
}


