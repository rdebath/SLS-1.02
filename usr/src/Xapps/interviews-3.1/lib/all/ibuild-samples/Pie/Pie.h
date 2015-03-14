#ifndef Pie_h
#define Pie_h

#include "Pie-core.h"

class Graphic;
class GraphicComp;

class Pie : public Pie_core {
public:
    Pie(const char*);
    virtual ~Pie();

    void Append(float, const char*);
    virtual void Resize();
private:
    void InitBorder(float);
    void InitCircum(float);
    void InitText(float, const char*);
    GraphicComp* GetGraphicComp(Graphic*);
private:
    float _accum;
};

#endif
