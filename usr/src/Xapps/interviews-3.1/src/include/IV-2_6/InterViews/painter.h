/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * Graphics interface
 */

#ifndef iv2_6_painter_h
#define iv2_6_painter_h

#include <InterViews/enter-scope.h>
#include <InterViews/boolean.h>
#include <InterViews/coord.h>
#include <InterViews/resource.h>
#include <IV-2_6/InterViews/textstyle.h>

#include <IV-2_6/_enter.h>

class Canvas;
class Color;
class PainterRep;
class Pattern;
class Brush;
class Font;
class Transformer;
class Bitmap;
class Raster;

class Painter : public Resource {
public:
    Painter();
    Painter(Painter*);
    ~Painter();

    void FillBg(boolean);
    boolean BgFilled() const;
    void SetColors(const Color* f, const Color* b);
    const Color* GetFgColor() const;
    const Color* GetBgColor() const;
    void SetPattern(const Pattern*);
    const Pattern* GetPattern() const;
    void SetBrush(const Brush*);
    const Brush* GetBrush() const;
    void SetFont(const Font*);
    const Font* GetFont() const;
    void SetStyle(int);
    int GetStyle() const;
    void SetTransformer(Transformer*);
    Transformer* GetTransformer() const;
    void MoveTo(int x, int y);
    void GetPosition(int& x, int& y) const;
    void SetOrigin(int x0, int y0);
    void GetOrigin(int& x0, int& y0) const;

    void Translate(float dx, float dy);
    void Scale(float x, float y);
    void Rotate(float angle);

    virtual void Clip(
	Canvas*, Coord left, Coord bottom, Coord right, Coord top
    );
    virtual void NoClip();
    virtual void SetOverwrite(boolean);
    virtual void SetPlaneMask(int);

    virtual void Text(Canvas*, const char*);
    virtual void Text(Canvas*, const char*, int);
    virtual void Text(Canvas*, const char*, Coord, Coord);
    virtual void Text(Canvas*, const char*, int, Coord, Coord);
    virtual void Stencil(
	Canvas*, Coord x, Coord y, Bitmap* image, Bitmap* mask = nil
    );
    virtual void RasterRect(Canvas*, Coord x, Coord y, Raster*);
    virtual void Point(Canvas*, Coord x, Coord y);
    virtual void MultiPoint(Canvas*, Coord x[], Coord y[], int n);
    virtual void Line(Canvas*, Coord x1, Coord y1, Coord x2, Coord y2);
    virtual void Rect(Canvas*, Coord x1, Coord y1, Coord x2, Coord y2);
    virtual void FillRect(Canvas*, Coord x1, Coord y1, Coord x2, Coord y2);
    virtual void ClearRect(Canvas*, Coord x1, Coord y1, Coord x2, Coord y2);
    virtual void Circle(Canvas*, Coord x, Coord y, int r);
    virtual void FillCircle(Canvas*, Coord x, Coord y, int r);
    virtual void Ellipse(Canvas*, Coord x, Coord y, int r1, int r2);
    virtual void FillEllipse(Canvas*, Coord x, Coord y, int r1, int r2);
    virtual void MultiLine(Canvas*, Coord x[], Coord y[], int n);
    virtual void Polygon(Canvas*, Coord x[], Coord y[], int n);
    virtual void FillPolygon(Canvas*, Coord x[], Coord y[], int n);
    virtual void BSpline(Canvas*, Coord x[], Coord y[], int n);
    virtual void ClosedBSpline(Canvas*, Coord x[], Coord y[], int n);
    virtual void FillBSpline(Canvas*, Coord x[], Coord y[], int n);
    virtual void Curve(Canvas*,
	Coord x0, Coord y0, Coord x1, Coord y1,
	Coord x2, Coord y2, Coord x3, Coord y3
    );
    virtual void CurveTo(Canvas*,
	Coord x0, Coord y0, Coord x1, Coord y1, Coord x2, Coord y2
    );
    virtual void Copy(
	Canvas* src, Coord x1, Coord y1, Coord x2, Coord y2,
	Canvas* dst, Coord x0, Coord y0
    );

    PainterRep* Rep() const;
private:
    friend class Rubberband;

    const Font* font;
    const Color* foreground;
    const Color* background;
    const Brush* br;
    const Pattern* pattern;
    int style;
    Coord curx, cury;
    int xoff, yoff;
    Transformer* matrix;
    PainterRep* rep;

    void Init();
    void Copy(Painter*);
    void Begin_xor();
    void End_xor();
    void Map(Canvas*, Coord x, Coord y, Coord& mx, Coord& my);
    void Map(Canvas*, Coord x, Coord y, short& sx, short& sy);
    void MapList(Canvas*, Coord x[], Coord y[], int n, Coord mx[], Coord my[]);
    void MapList(Canvas*, float x[], float y[], int n, Coord mx[], Coord my[]);
    void MultiLineNoMap(Canvas* c, Coord x[], Coord y[], int n);
    void FillPolygonNoMap(Canvas* c, Coord x[], Coord y[], int n);
};

inline PainterRep* Painter::Rep() const { return rep; }

#include <IV-2_6/_leave.h>

#endif
