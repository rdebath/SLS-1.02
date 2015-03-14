/*
 * InputHandler - glyph that handles input
 */

#ifndef ihandler_h
#define ihandler_h

#include <InterViews/event.h>
#include <InterViews/input.h>
#include "figure.h"

class Cursor;
class IOHandler;
class GlyphGrabber;

class GlyphViewer : public InputHandler {
public:
    GlyphViewer(float w = -1.0, float h = -1.0);
    virtual ~GlyphViewer();

    virtual void press(const Event&);
    virtual void drag(const Event&);
    virtual void release(const Event&);

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);

    virtual GraphicMaster* root();
    virtual void root(GraphicMaster*);

    virtual Tool& tool();
    virtual void cur_tool(unsigned int);

    void damage(Coord, Coord, Coord, Coord);
    void repair();
protected:
    void rate_scroll();
    void grab_scroll();
    void rate_zoom();
    void initshape();
    void initgraphic();
    void initcursor();
    void inittimer();
protected:
    Coord _x, _y;
    Coord _lx, _ly;

    float _width, _height;
    Allocation _a;

    boolean _zoom, _pan, _grab;
    Cursor* _cursor;
    long _delay;
    IOHandler* _timer;
    GraphicMaster* _root;
    Tool _tool;
    GlyphGrabber* _grabber;
private:
    void tick(long, long);
};

#endif
