#include <Unidraw/iterator.h>
#include <Unidraw/Graphic/grblock.h>
#include <Unidraw/Graphic/lines.h>
#include <stdio.h>

#include <Unidraw/Graphic/polygons.h> 
#include <Unidraw/Graphic/picture.h> 
#include <Unidraw/Components/text.h> 
#include "Hist.h"
#include <IV-2_6/_enter.h>

Hist::Hist(const char* name, float max_value) : Hist_core(name) {
    _count = 0;
    _max_value = max_value;
    RemoveDummies();
    InitValueAxis();
}

Hist::~Hist () {
    delete _x_axis;
    delete _y_axis;
    delete _x_tick;
    delete _y_tick;
    delete _label;
    delete _value;
    delete _bar;
}

void Hist::RemoveDummies () {
    Iterator i;
    Graphic* g = _chart->GetGraphic();
    g->First(i);

    while (!g->Done(i)) {
        g->Remove(i);
    }
}

void Hist::InitValueAxis () {
    float x0, y0, x1, y1;
    _y_axis->GetBounds(x0, y0, x1, y1);

    for (int i = 0; i < int(_max_value); ++i) {
        char value_buf[100];
        sprintf(value_buf, "%d", i+1);
        float dy = (y1 - y0)/2 * i;

        Graphic* y_axis = _y_axis->Copy();
        Graphic* y_tick = _y_tick->Copy();
        Graphic* value = new TextGraphic(value_buf, _value);

        _chart->GetGraphic()->Append(y_axis, y_tick, value);

        y_axis->Translate(0., dy);
        y_tick->Translate(0., dy);
        y_tick->Align(VertCenter, value, VertCenter);
    }
}

void Hist::Include (const char* label_string, float value) {
    Graphic* x_axis = _x_axis->Copy();
    Graphic* x_tick = _x_tick->Copy();
    Graphic* label = new TextGraphic(label_string, _label);
    Graphic* bar = _bar->Copy();

    _chart->GetGraphic()->Append(x_axis, x_tick, label, bar);

    float x0, y0, x1, y1;
    _x_axis->GetBounds(x0, y0, x1, y1);
    float dx = (x1 - x0)/2 * _count;
    
    x_axis->Translate(dx, 0.);
    x_tick->Translate(dx, 0.);
    x_tick->Align(HorizCenter, label, HorizCenter);

    bar->Scale(1., value);
    x_tick->Align(TopCenter, bar, BottomCenter);

    ++_count;
    _chart->SetGraphic(_chart->GetGraphic());
}
