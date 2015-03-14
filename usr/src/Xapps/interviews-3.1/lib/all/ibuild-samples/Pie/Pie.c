#include <Unidraw/Graphic/lines.h> 
#include <Unidraw/Components/text.h> 
#include <Unidraw/Components/grcomp.h> 
#include "Pie.h"
#include <stdio.h>
#include <IV-2_6/_enter.h>

Pie::Pie(const char* name) : Pie_core(name) {
    _accum = 0.0;
}

Pie::~Pie () {
    GraphicComp* text = GetGraphicComp(_textgr);
    GraphicComp* index = GetGraphicComp(_index);
    GraphicComp* seggr = GetGraphicComp(_seggr);

    delete text;
    delete index;
    delete seggr;
}

GraphicComp* Pie::GetGraphicComp(Graphic* gr) {
    return (GraphicComp*) gr->GetTag();
}

void Pie::Resize () {
    GraphicComp* grcomp = (GraphicComp*) GetComponent();
    GraphicComp* text = GetGraphicComp(_textgr);
    GraphicComp* index = GetGraphicComp(_index);
    GraphicComp* seggr = GetGraphicComp(_seggr);
    grcomp->Remove(text);
    grcomp->Remove(index);
    grcomp->Remove(seggr);
    grcomp->Notify();
    Pie_core::Resize();
}

void Pie::Append (float fraction, const char* text) {
    InitBorder(fraction);
    InitCircum(fraction);
    InitText(fraction , text);
    _accum += fraction;
    GraphicComp* grcomp = (GraphicComp*) GetComponent();
    grcomp->Notify();
}

void Pie::InitBorder (float fraction) {
    float l, b, r, t;
    fraction += _accum;
    GraphicComp* grcomp = (GraphicComp*) GetComponent();
    GraphicComp* linegr = (GraphicComp*) GetGraphicComp(_linegr)->Copy();
    grcomp->Append(linegr);
    _linegr->GetBounds(l, b, r, t);
    l = (l+r)/2;
    linegr->GetGraphic()->Rotate(-fraction*360.0, l, b);
}

void Pie::InitCircum (float fraction) {
    float l, b, r, t;
    float inc = 1.0/360.0;
    fraction += _accum;
    
    _linegr->GetBounds(l, b, r, t);
    l = (l+r)/2;
    GraphicComp* grcomp = (GraphicComp*) GetComponent();

    for (float i = _accum; i < fraction; i += inc) {
        GraphicComp* seggr = (GraphicComp*) GetGraphicComp(_seggr)->Copy();
        grcomp->Append(seggr);
        seggr->GetGraphic()->Rotate(-i*360.0, l, b);
    }
}

void Pie::InitText (float fraction, const char* text) {
    float l, b, r, t;
    char indexbuf[256];

    sprintf(indexbuf, " %.3f%%", fraction*100.0);
    fraction /= 2.0;
    fraction += _accum;
    
    _linegr->GetBounds(l, b, r, t);
    l = (l+r)/2;
    GraphicComp* grcomp = (GraphicComp*) GetComponent();

    TextGraphic* textgr = new TextGraphic(text, _textgr);
    TextGraphic* index = new TextGraphic(indexbuf, _index);
    TextComp* textcomp = new TextComp(textgr);
    TextComp* indexcomp = new TextComp(index);

    grcomp->Append(textcomp);
    grcomp->Append(indexcomp);

    textgr->Rotate(-fraction*360.0, l, b);
    textgr->GetCenter(l, b);
    textgr->Rotate(fraction*360.0, l, b);
    textgr->Align(CenterRight, index, CenterLeft);
}
    
