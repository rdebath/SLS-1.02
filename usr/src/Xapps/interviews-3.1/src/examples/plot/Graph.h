/*
 * Graph.h
 */

#ifndef graph_h
#define graph_h

#include <InterViews/input.h>

class Axis;
class Color;
class Patch;
class Plot;
class String;
class XYMarker;

class Graph : public InputHandler {
public:
    Graph(
        float w, float h, float x_begin, float x_end, 
        float y_begin, float y_end, const Color*, const char* symbol
    );
    void AddPt(Coord, Coord);
    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void press (const Event&);
protected:
    virtual ~Graph();
    virtual void mark (int);
protected:
    boolean _init;
    int _current;
    int _count;
    Coord* _x;
    Coord* _y;
    float _w, _h;
    float _x_range, _y_range;
    float _x_origin, _y_origin;
    Plot* _plot;
    XYMarker* _marker;
    Axis* _xaxis;
    Patch* _xpatch;
    Axis* _yaxis;
    Patch* _ypatch;
    String* _symbol;
    Patch* _ppatch;
};

#endif
