#include <Unidraw/Graphic/damage.h>
#include <Unidraw/Graphic/graphic.h>

#include "Dial.h"
#include <IV-2_6/_enter.h>

Dial::Dial(
    const char* name, Graphic* gr
) : Dial_core(name, gr) {
    _damage = new Damage;
}

Dial::~Dial () {
    delete _damage;
}

void Dial::Resize () {
    Dial_core::Resize();
    _damage->SetGraphic(_graphic);
    _damage->SetPainter(output);
    _damage->SetCanvas(canvas);

    float l, b, r, t, cx, cy;
    _graphic->GetCenter(cx, cy);
    _graphic->GetBounds(l, b, r, t);

    float sx = double(xmax + 1) / double(r - l + 1.);
    float sy = double(ymax + 1) / double(t - b + 1.);
    
    _graphic->Scale(sx, sy, cx, cy);
    UpdatePerspective();
}

void Dial::Damaged(Graphic* gr) {
    _damage->Incur(gr);
}

void Dial::Repair () {
    _damage->Repair();
}
