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
 * Rubber - rubberbanding graphical objects
 */

#ifndef iv2_6_rubband_h
#define iv2_6_rubband_h

#include <InterViews/boolean.h>
#include <InterViews/coord.h>
#include <InterViews/resource.h>

enum Side { LeftSide, RightSide, BottomSide, TopSide };

class Canvas;
class Painter;

class Rubberband : public Resource {
public:
    Rubberband(Painter*, Canvas*, IntCoord offx, IntCoord offy);
    virtual ~Rubberband();

    virtual void Draw();
    virtual void Redraw();
    virtual void Erase();
    virtual void Track(IntCoord x, IntCoord y);

    virtual void SetPainter(Painter*);
    virtual void SetCanvas(Canvas*);
    Painter* GetPainter();
    Canvas* GetCanvas();
protected:
    float Angle(IntCoord, IntCoord, IntCoord, IntCoord);
    float Distance(IntCoord, IntCoord, IntCoord, IntCoord);
protected:
    Painter* output;
    Canvas* canvas;
    boolean drawn;
    IntCoord trackx, offx;
    IntCoord tracky, offy;
};

inline Painter* Rubberband::GetPainter () { return output; }
inline Canvas* Rubberband::GetCanvas () { return canvas; }

#include <IV-2_6/_leave.h>

#endif
