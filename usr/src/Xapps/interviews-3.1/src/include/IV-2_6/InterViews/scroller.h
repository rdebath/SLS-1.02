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
 * General scrolling interface.
 */

#ifndef ivlook2_6_scroller_h
#define ivlook2_6_scroller_h

#include <IV-2_6/InterViews/interactor.h>

#include <IV-2_6/_enter.h>

class Scroller : public Interactor {
protected:
    Interactor* interactor;
    int size;
    Perspective* view;
    Perspective* shown;
    double scale;
    Sensor* tracking;
    boolean syncScroll;

    Scroller(Interactor*, int);
    Scroller(const char*, Interactor*, int);
    virtual ~Scroller();

    void Background(IntCoord, IntCoord, IntCoord, IntCoord);
    void MakeBackground();
    virtual void Resize();
private:
    void Init();
};

class HScroller : public Scroller {
public:
    HScroller(Interactor*, int size = 0);
    HScroller(const char*, Interactor*, int size = 0);
    virtual ~HScroller();

    virtual void Handle(Event&);
    virtual void Update();
private:
    void Bar(IntCoord, int);
    void Border(IntCoord);
    void GetBarInfo(Perspective*, IntCoord&, int&);
    void Init();
    void Outline(IntCoord, int);
    virtual void Reconfig();
    virtual void Redraw(IntCoord, IntCoord, IntCoord, IntCoord);
    void Sides(IntCoord, IntCoord);
    IntCoord Slide(Event&);
};

class VScroller : public Scroller {
public:
    VScroller(Interactor*, int size = 0);
    VScroller(const char*, Interactor*, int size = 0);
    virtual ~VScroller();

    virtual void Handle(Event&);
    virtual void Update();
private:
    void Bar(IntCoord, int);
    void Border(IntCoord);
    void GetBarInfo(Perspective*, IntCoord&, int&);
    void Init();
    void Outline(IntCoord, int);
    virtual void Reconfig();
    virtual void Redraw(IntCoord, IntCoord, IntCoord, IntCoord);
    void Sides(IntCoord, IntCoord);
    IntCoord Slide(Event&);
};

#include <IV-2_6/_leave.h>

#endif
