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
 * A viewport contains another interactor.  Unlike a MonoScene or Frame,
 * a viewport always gives the interactor its desired shape.  However,
 * the interactor may not be entirely viewable through the viewport.
 * The viewport's perspective can be used to adjust what portion is visible.
 */

#ifndef iv2_6_viewport_h
#define iv2_6_viewport_h

#include <IV-2_6/InterViews/scene.h>

#include <IV-2_6/_enter.h>

class Viewport : public MonoScene {
public:
    Viewport(Interactor* = nil, Alignment = Center);
    Viewport(const char*, Interactor* = nil, Alignment = Center);
    ~Viewport();

    virtual void Adjust(Perspective&);
    virtual void Resize();

    void AdjustTo(float px, float py, float zx, float zy);
    void AdjustBy(float dpx, float dpy, float dzx, float dzy);

    void ScrollTo(float px, float py);
    void ScrollXTo(float px);
    void ScrollYTo(float py);
    void ScrollBy(float dpx, float dpy);
    void ScrollXBy(float dpx);
    void ScrollYBy(float dpy);

    void ZoomTo(float zx, float zy);
    void ZoomXTo(float zx);
    void ZoomYTo(float zy);
    void ZoomBy(float dzx, float dzy);
    void ZoomXBy(float dzx);
    void ZoomYBy(float dzy);

    float XPos();
    float YPos();
    float XMag();
    float YMag();
protected:
    Alignment align;
    Painter* background;

    virtual void Reconfig();
    virtual void Redraw(Coord, Coord, Coord, Coord);
    virtual void DoMove(Interactor*, Coord& x, Coord& y);
private:
    int cwidth;
    int cheight;

    void Init(Interactor*, Alignment);
    void DoAdjust(float px, float py, float zx, float zy);
};

#include <IV-2_6/_leave.h>

#endif
