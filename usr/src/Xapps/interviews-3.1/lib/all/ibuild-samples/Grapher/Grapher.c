#include <Unidraw/Graphic/grblock.h> 
#include <Unidraw/Graphic/picture.h> 
#include <Unidraw/Components/text.h> 
#include <Unidraw/Graphic/ellipses.h> 
#include <Unidraw/iterator.h>
#include "Grapher.h"
#include "Plotter.h"
#include <stdio.h>
#include <IV-2_6/_enter.h>

static int pad = 30;

Grapher::Grapher(
    const char* name, int hmin, int hmax, int vmin, int vmax
) : Grapher_core(name) {
    int l, b, r, t;

    _hrange = hmax - hmin;
    _vrange = vmax - vmin;

    _vaxis->GetBox(l, b, r, t);
    _vinc = (float) t - b;

    _haxis->GetBox(l, b, r, t);
    _hinc = (float) r - l;

    _lorigx = (float) hmin;
    _lorigy = (float) vmin;

    InitHAxis();
    InitVAxis();

    _view->Reinit();
    _dot_picture->Remove(_dot);
    _hlabel_picture->Remove(_hlabel);
    _vlabel_picture->Remove(_vlabel);

    _haxis_picture->GetBox(l, b, r, t);
    _origx = (float) l;
    _origy = (float) t;
}

Grapher::~Grapher () {
    delete _dot;
    delete _hlabel;
    delete _vlabel;
}
    
void Grapher::InitHAxis () {
    for (int h = 1; h <= _hrange; h++) {
        char num[10];
        
        sprintf(num, "%d", (int)_lorigx+h);
        TextGraphic* text = new TextGraphic(num, _hlabel);
        _hlabel_picture->Append(text);
        text->Translate(_hinc*(h-1), 0.0);

        Graphic* haxis = _haxis->Copy();
        _haxis_picture->Append(haxis);
        haxis->Translate(_hinc*h, 0.0);
    }

}

void Grapher::InitVAxis () {
    for (int v = 1; v <= _vrange; v++) {
        char num[10];
        sprintf(num, "%d", (int)_lorigy+v);
        TextGraphic* text = new TextGraphic(num, _vlabel);
        _vlabel_picture->Append(text);
        text->Translate(0.0, _vinc*(v-1));
        _vaxis_picture->Align(Left, text, Right);

        Graphic* vaxis = _vaxis->Copy();
        _vaxis_picture->Append(vaxis);
        vaxis->Translate(0.0, _vinc*v);
    }
}

void Grapher::Append (float x, float y) {
    float cx, cy;
    float effx, effy;

    effx = _origx + (x - _lorigx) * _hinc;
    effy = _origy + (y - _lorigy) * _vinc;
    
    Graphic* dot = _dot->Copy();
    _dot_picture->Append(dot);
    dot->GetCenter(cx, cy);
    dot->Translate(effx-cx, effy-cy);
}

void Grapher::Resize () {
    float l, b, r, t;
    float xscale, yscale;

    _vaxis_picture->GetBounds(l, b, r, t);
    float w = r-l+pad;

    _haxis_picture->GetBounds(l, b, r, t);
    xscale = (xmax-w)/(r-l);
    
    _haxis_picture->Scale(xscale, 1.0, l, b);
    
    Retranslate(_hlabel_picture, xscale-1.0, 0.0, l, b);
    Retranslate(_dot_picture, xscale-1.0, 0.0, l, b);

    _haxis_picture->GetBounds(l, b, r, t);
    float h = t-b+pad;

    _vaxis_picture->GetBounds(l, b, r, t);
    yscale = (ymax-h)/(t-b);
    
    _vaxis_picture->Scale(1.0,yscale, l, b);
    Retranslate(_vlabel_picture, 0.0, yscale-1.0, l, b);
    Retranslate(_dot_picture, 0.0, yscale-1.0, l, b);

    _view->Reinit();
    Grapher_core::Resize();
}

void Grapher::Retranslate (Picture* p, float sx, float sy, float l, float b) {
    float ml, mb;
    Iterator i;
    for (p->First(i); !p->Done(i); p->Next(i)) {
        Graphic* kid = p->GetGraphic(i);
        kid->GetCenter(ml, mb);
        kid->Translate((ml-l)*sx, (mb-b)*sy);
    }
}

        
