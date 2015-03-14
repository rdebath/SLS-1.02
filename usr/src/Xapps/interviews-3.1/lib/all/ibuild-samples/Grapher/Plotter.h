#ifndef Plotter_h
#define Plotter_h

#include "Plotter-core.h"

class Plotter : public Plotter_core {
public:
    Plotter(const char*, Graphic*);
    void Reinit();
};

#endif
