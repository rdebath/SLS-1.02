/*
 * Copyright (c) 1990, 1991 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * Grid - constrains points to lie on a grid and provides a corresponding
 * visual representation of the grid.
 */

#ifndef unidraw_grid_h
#define unidraw_grid_h

#include <InterViews/enter-scope.h>
#include <Unidraw/enter-scope.h>

#include <IV-2_6/_enter.h>

class Graphic;

class Grid {
public:
    Grid(float w, float h, float xincr, float yincr);
    virtual ~Grid();

    virtual Graphic* GetGraphic();              // redefine all virtuals
    virtual void Constrain(Coord&, Coord&);     // if you redefine GetGraphic
    virtual void Visibility(boolean);
    virtual boolean IsVisible();
    virtual void SetSpacing(float incr, float yincr);
protected:
    Grid(Graphic*);
protected:
    Graphic* _graphic;
};

#include <IV-2_6/_leave.h>

#endif
