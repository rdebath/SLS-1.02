#ifndef Grapher_h
#define Grapher_h

#include "Grapher-core.h"
class Picture;
class Graphic;

class Grapher : public Grapher_core {
public:
    Grapher(const char*, int hmin, int hmax, int vmin, int vmax);
    virtual ~Grapher();
    void Append(float, float);
protected:
    virtual void Resize();
private:
    void InitHAxis();
    void InitVAxis();
    void Retranslate(Picture*, float sx, float sy, float l, float b);
private:
    float _origx, _origy;
    float _hinc, _vinc;
    float _lorigx, _lorigy;
    int _hrange, _vrange;
};

#endif
