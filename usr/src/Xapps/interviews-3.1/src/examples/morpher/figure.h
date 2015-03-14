/*
 * planar figures
 */

#ifndef figure_h
#define figure_h

#include <InterViews/event.h>
#include <InterViews/glyph.h>
#include <InterViews/transformer.h>
#include "globals.h"

class BoxObjList;
class Brush;
class Color;
class Font;
class GraphicList;
class PolyGlyph;
class String;
class ToolState;
class Transformer;

const int buf_size = 10;

class Tool {
public:
    enum {
        nop, select, move, scale, stretch, rotate, alter, create
    };

    Tool(unsigned int = Tool::nop);
    virtual ~Tool();

    virtual unsigned int tool();
    virtual void tool(unsigned int);
    
    virtual ToolState& toolstate();
    virtual void toolstate(ToolState*);
    virtual void reset();
protected:
    unsigned int _cur_tool;
    ToolState* _toolstate;
};

class Graphic : public Glyph {
public:
    Graphic(Graphic* gr = nil);
    virtual ~Graphic ();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void drawit(Canvas*);
    virtual void drawclipped(Canvas*, Coord, Coord, Coord, Coord);

    virtual boolean grasp(const Event&, Tool&);
    virtual boolean manipulating(const Event&, Tool&);
    virtual boolean effect(const Event&, Tool&);

    virtual Glyph* clone() const;
    virtual void flush();

    virtual Transformer* transformer();
    virtual void transformer(Transformer*);
    void eqv_transformer(Transformer&);

    virtual void brush(const Brush*);
    virtual const Brush* brush();
    virtual void stroke(const Color*);
    virtual const Color* stroke();
    virtual void fill(const Color*);
    virtual const Color* fill();
    virtual void font(const Font*);
    virtual const Font* font();
    virtual void closed(boolean);
    virtual boolean closed();
    virtual void curved(boolean);
    virtual boolean curved();
    virtual int ctrlpts(Coord*&, Coord*&) const;
    virtual void ctrlpts(Coord*, Coord*, int);
    virtual Graphic* parent();
    virtual void parent(Graphic*);
    virtual Graphic* root();

    void translate(Coord dx, Coord dy);
    void scale(Coord sx, Coord sy, Coord ctrx = 0.0, Coord ctry = 0.0);
    void rotate(float angle, Coord ctrx = 0.0, Coord ctry = 0.0);
    void align(Alignment, Graphic*, Alignment);

    virtual void recompute_shape();
    virtual void getbounds(Coord&, Coord&, Coord&, Coord&);
    virtual void getcenter(Coord&, Coord&);    
    virtual boolean contains(PointObj&);
    virtual boolean intersects(BoxObj&);

    virtual void undraw();
    virtual void append_(Graphic*);
    virtual void prepend_(Graphic*);
    virtual void insert_(GlyphIndex, Graphic*);
    virtual void remove_(GlyphIndex);
    virtual void replace_(GlyphIndex, Graphic*);
    virtual void change_(GlyphIndex);

    virtual GlyphIndex count_() const;
    virtual Graphic* component_(GlyphIndex) const;
    virtual void modified_(GlyphIndex);

    virtual Graphic* first_containing(PointObj&);
    virtual Graphic* last_containing(PointObj&);

    virtual Graphic* first_intersecting(BoxObj&);
    virtual Graphic* last_intersecting(BoxObj&);

    virtual Graphic* first_within(BoxObj&);
    virtual Graphic* last_within(BoxObj&);

    virtual Graphic& operator = (Graphic&);

    void get_original(const Coord*&, const Coord*&);
    void add_point (Coord x, Coord y);
    void add_curve (Coord x, Coord y, Coord x1, Coord y1, Coord x2, Coord y2);
    void Bspline_move_to (
        Coord x, Coord y, Coord x1, Coord y1, Coord x2, Coord y2
    );
    void Bspline_curve_to (
        Coord x, Coord y, Coord x1, Coord y1, Coord x2, Coord y2
    );
protected:
    Graphic (
        const Brush* brush, const Color* stroke, const Color* fill,
        const Font* font, boolean closed, boolean curved, int coords, 
        Transformer*
    );

    virtual void draw_gs(Canvas*, Graphic*);
    virtual void drawclipped_gs(
        Canvas*, Coord, Coord, Coord, Coord, Graphic*
    );
    virtual void getextent_gs(
        Coord&, Coord&, Coord&, Coord& ,float& ,Graphic* gs
    );
    virtual boolean contains_gs(PointObj&, Graphic* gs);
    virtual boolean intersects_gs(BoxObj&, Graphic* gs);
    virtual void getbounds_gs(Coord&, Coord&, Coord&, Coord&, Graphic* gs);

    virtual void total_gs (Graphic& gs);
    void parentXform(Transformer& t);
    
    virtual void concat_gs(Graphic* a, Graphic* b, Graphic* dest);
    virtual void concatXform(
        Transformer* a, Transformer* b, Transformer* dest
    );
    virtual void concat(Graphic* a, Graphic* b, Graphic* dest);

/*   Helpers   */

    virtual boolean contains_(Graphic*, PointObj&, Graphic* gs);
    virtual boolean intersects_(Graphic*, BoxObj&, Graphic* gs);
    virtual void getbounds_(
        Graphic*, Coord&, Coord&, Coord&, Coord&, Graphic* gs
    );
    void total_gs_(Graphic*, Graphic& gs);
    void concatgs_(Graphic*, Graphic*, Graphic*, Graphic*);
    void concatXform_(Graphic*, Transformer*, Transformer*, Transformer*);
    void concat_(Graphic*, Graphic*, Graphic*, Graphic*);
    void getextent_(Graphic*, Coord&, Coord&, Coord&, Coord&, float&,Graphic*);

    void draw_(Graphic*, Canvas*, Graphic*);
    void drawclipped_(Graphic*, Canvas*, Coord, Coord, Coord, Coord, Graphic*);
    void transform_(Coord, Coord, Coord&, Coord&, Graphic*);
protected:
    const Brush* _brush;
    const Color* _stroke;
    const Color* _fill;
    const Font* _font;
    Transformer*  _t;

    boolean _closed;
    boolean _curved;
    int _ctrlpts;
    int _buf_size;
    Coord* _x;
    Coord* _y;

    Coord _xmin;
    Coord _xmax;
    Coord _ymin;
    Coord _ymax;
    Graphic* _parent;
};

class PolyGraphic : public Graphic {
public:
    PolyGraphic(Graphic* = nil) ;
    virtual ~PolyGraphic();
    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);

    virtual void undraw();

    virtual void append_(Graphic*);
    virtual void prepend_(Graphic*);
    virtual void insert_(GlyphIndex, Graphic*);
    virtual void remove_(GlyphIndex);
    virtual void replace_(GlyphIndex, Graphic*);
    virtual void change_(GlyphIndex);

    virtual GlyphIndex count_() const;
    virtual Graphic* component_(GlyphIndex) const;

    virtual void modified_(GlyphIndex);
    virtual void flush();
    virtual Glyph* clone() const;

    virtual Graphic* first_containing(PointObj&);
    virtual Graphic* last_containing(PointObj&);

    virtual Graphic* first_intersecting(BoxObj&);
    virtual Graphic* last_intersecting(BoxObj&);

    virtual Graphic* first_within(BoxObj&);
    virtual Graphic* last_within(BoxObj&);
protected:
    virtual void draw_gs(Canvas*, Graphic*);
    virtual void drawclipped_gs(
        Canvas*, Coord, Coord, Coord, Coord, Graphic*
    );
    virtual boolean contains_gs(PointObj&, Graphic*);
    virtual boolean intersects_gs(BoxObj&, Graphic*);
    virtual void getextent_gs(
        Coord&, Coord&, Coord&, Coord&, float&, Graphic* gs
    );

protected:
    PolyGlyph* _body;
};

class GraphicMaster : public PolyGraphic {
public:
    GraphicMaster(Graphic* = nil, const Color* bg = nil);
    virtual ~GraphicMaster();

    void background(const Color*);
    const Color* background();

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual boolean grasp(const Event&, Tool&);
    virtual boolean manipulating(const Event&, Tool&);
    virtual boolean effect(const Event&, Tool&);
    virtual Glyph* clone() const;
protected:
    virtual void drawclipped_gs(
        Canvas*, Coord, Coord, Coord, Coord, Graphic*
    );
protected:
    GraphicList* _gr_list;
    const Color* _bg;
    Allocation _a;
};
inline const Color* GraphicMaster::background () { return _bg; }

class Line : public Graphic {
public:
    Line (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord x1, Coord y1, Coord x2, Coord y2, Transformer*
    );
    virtual Glyph* clone() const;
    
protected:
    virtual ~Line ();
};

class Rectangle : public Graphic {
public:
    Rectangle (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord l, Coord b, Coord r, Coord t, Transformer*
    );
    virtual Glyph* clone() const;

protected:
    virtual ~Rectangle ();
};

class Circle : public Graphic {
public:
    Circle (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord x, Coord y, Coord r, Transformer*
    );
    virtual Glyph* clone() const;

protected:
    virtual ~Circle ();
};

class Ellipse : public Graphic {
public:
    Ellipse (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord x, Coord y, Coord rx, Coord ry, Transformer*
    );
    virtual Glyph* clone() const;

protected:
    virtual ~Ellipse ();
};

class Open_BSpline : public Graphic {
public:
    Open_BSpline (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord* x, Coord* y, int ctrlpts, Transformer*
    );
    virtual Glyph* clone() const;

protected:
    Open_BSpline(Open_BSpline*);
    virtual ~Open_BSpline ();
};

class Closed_BSpline : public Graphic {
public:
    Closed_BSpline (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord* x, Coord* y, int ctrlpts, Transformer*
    );
    virtual Glyph* clone() const;

protected:
    Closed_BSpline(Closed_BSpline*);
    virtual ~Closed_BSpline ();
};

class Polyline : public Graphic {
public:
    Polyline (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord* x, Coord* y, int ctrlpts, Transformer*
    );
    virtual Glyph* clone() const;

protected:
    virtual ~Polyline ();
};

class Polygon : public Graphic {
public:
    Polygon (
        const Brush* brush, const Color* stroke, const Color* fill,
        Coord* x, Coord* y, int ctrlpts, Transformer*
    );
    virtual Glyph* clone() const;

protected:
    virtual ~Polygon ();
};

class Text : public Graphic {
public:
    Text (
        const Font* font, const Color* stroke, const char*, Transformer*
    );
    virtual void text(const char*);
    virtual const char* text();
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual Glyph* clone() const;

protected:
    virtual ~Text();
    
    virtual void getextent_gs (Coord&, Coord&, Coord&, Coord&,float&,Graphic*);
    virtual void draw_gs(Canvas*, Graphic*);

    void init();
protected:
    String* _text;
    Allocation _a;
    PolyGlyph* _body;
};

class ToolState {
public:
    Event _init;
    Event _last;
    Coord _l, _b, _r, _t;
    Graphic _gs;
};

#endif
