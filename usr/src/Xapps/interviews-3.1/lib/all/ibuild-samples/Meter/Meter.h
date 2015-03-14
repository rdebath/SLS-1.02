#ifndef Meter_h
#define Meter_h

#include "Meter-core.h"

class Meter : public Meter_core {
public:
    Meter(const char*);
    virtual void Handle(Event& e);
private:
    void PressLeft();
    void UnpressLeft();
    void PressRight();
    void UnpressRight();
    void RotateLeft();
    void RotateRight();
private:
    boolean _lpressed;
    boolean _rpressed;
    int _angle;
};

#endif
