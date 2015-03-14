#include <Unidraw/Graphic/picture.h> 
#include <Unidraw/Graphic/ellipses.h> 
#include <Unidraw/Graphic/grblock.h> 
#include <Unidraw/Graphic/geomobjs.h>
#include <InterViews/event.h>
#include <InterViews/canvas.h>
#include "Meter.h"
#include <IV-2_6/_enter.h>

static int inc = 5;

Meter::Meter(const char* name) : Meter_core(name) {
    _upButton->Remove(_up_pressed);
    _downButton->Remove(_down_pressed);
    _lpressed = false;
    _rpressed = false;
    _angle = 90;
}

void Meter::Handle(Event& e) {
    if (e.eventType == DownEvent && !_lpressed && !_rpressed) {
        PointObj point(e.x, e.y);
        if (_upButton->FirstGraphicContaining(point) != nil) {
            _rpressed = true;
            PressRight();
            RotateRight();
        } else if (_downButton->FirstGraphicContaining(point) != nil) {
            _lpressed = true;
            PressLeft();
            RotateLeft();
        }
    } else if (e.eventType == UpEvent) {
        if (_lpressed) {
            _lpressed = false;
            UnpressLeft();
        } else if (_rpressed) {
            _rpressed = false;
            UnpressRight();
        }
    }
}

void Meter::PressLeft () {
    Canvas* canvas = _canvas->GetCanvas();
    _down_unpressed->Erase(canvas);
    _downButton->Remove(_down_unpressed);
    _downButton->Append(_down_pressed);
    _downButton->Draw(canvas);
}

void Meter::PressRight () {
    Canvas* canvas = _canvas->GetCanvas();
    _up_unpressed->Erase(canvas);
    _upButton->Remove(_up_unpressed);
    _upButton->Append(_up_pressed);
    _upButton->Draw(canvas);
}

void Meter::UnpressLeft () {
    Canvas* canvas = _canvas->GetCanvas();
    _down_pressed->Erase(canvas);
    _downButton->Remove(_down_pressed);
    _downButton->Append(_down_unpressed);
    _downButton->Draw(canvas);
}

void Meter::UnpressRight () {
    Canvas* canvas = _canvas->GetCanvas();
    _up_pressed->Erase(canvas);
    _upButton->Remove(_up_pressed);
    _upButton->Append(_up_unpressed);
    _upButton->Draw(canvas);
}

void Meter::RotateLeft () {
    Canvas* canvas = _canvas->GetCanvas();
    float cx, cy;
    _orig->GetCenter(cx, cy);

    if (_angle+inc <= 180) {
        _angle += inc;
        _needle->Erase(canvas);
        _needle->Rotate((float)inc, cx, cy);
        _needle->Draw(canvas);
    }
}
        
void Meter::RotateRight () {
    Canvas* canvas = _canvas->GetCanvas();
    float cx, cy;
    _orig->GetCenter(cx, cy);

    if (_angle-inc >= 0) {
        _angle -= inc;
        _needle->Erase(canvas);
        _needle->Rotate(-(float)inc, cx, cy);
        _needle->Draw(canvas);
    }
}
        
