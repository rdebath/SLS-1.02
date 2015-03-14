/*
 * Axis - position tick marks as on axis
 */

#ifndef axis_h
#define axis_h

#include <InterViews/monoglyph.h>

class Page;

class Axis : public MonoGlyph {
public:
    virtual void Range(float first, float last) = 0;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
protected:
    Axis(float first, float last);
    virtual ~Axis();

    Allocation _a;
    Page* _page;
    float _first;
    float _last;
};

class YAxis : public Axis {
public:
    YAxis(float first, float last);
    virtual void request (Requisition& r) const;
    virtual void Range(float first, float last);

protected:
    virtual ~YAxis();
};

class XAxis : public Axis {
public:
    XAxis(float first, float last);
    virtual void request (Requisition& r) const;
    virtual void Range(float first, float last);

protected:
    virtual ~XAxis();
};

#endif
