/*
 * Plot - simple graph glyph
 */

#ifndef plot_h
#define plot_h

#include <InterViews/glyph.h>

class Brush;
class Color;

class Plot : public Glyph {
public:
    Plot(int& count, float* x, float* y, const Color*, const Brush*);

    void setpts(int&, float*, float*);

    virtual void request (Requisition& r) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);

protected:
    virtual ~Plot();

    int* _count;
    float* _x;
    float* _y;

    const Color* _color;
    const Brush* _brush;
};

inline void Plot::setpts (int& count, float* x, float* y) {
    _count = &count;
    _x = x;
    _y = y;
}

#endif
