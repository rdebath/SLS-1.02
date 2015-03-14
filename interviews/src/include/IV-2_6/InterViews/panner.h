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
 * Panner - an interactor for two-dimensional scrolling and zooming.
 */

#ifndef ivlook2_6_panner_h
#define ivlook2_6_panner_h

#include <IV-2_6/InterViews/scene.h>

#include <IV-2_6/_enter.h>

class Panner : public MonoScene {
public:
    Panner(Interactor*, int size = 0);
    Panner(const char*, Interactor*, int size = 0);
    virtual ~Panner();
protected:
    int size;

    virtual void Reconfig();
private:
    Interactor* adjusters;
    Interactor* slider;

    void Init(Interactor*, int);
};

class Slider : public Interactor {
public:
    Slider(Interactor*);
    Slider(const char*, Interactor*);
    virtual ~Slider();

    virtual void Draw();
    virtual void Handle(Event&);
    virtual void Update();
    virtual void Reshape(Shape&);
    virtual void Resize();
protected:
    virtual void Reconfig();
    virtual void Redraw(IntCoord, IntCoord, IntCoord, IntCoord);
private:
    Interactor* interactor;
    Perspective* view;
    Perspective* shown;
    IntCoord left, bottom, right, top;
    IntCoord prevl, prevb, prevr, prevt;	// for smart update
    IntCoord llim, blim, rlim, tlim;	// sliding limits
    boolean constrained, syncScroll;
    int moveType;
    IntCoord origx, origy;

    void Init(Interactor*);
    IntCoord ViewX(IntCoord);
    IntCoord ViewY(IntCoord);
    IntCoord SliderX(IntCoord);
    IntCoord SliderY(IntCoord);
    void CalcLimits(Event&);		// calculate sliding limits
    void SizeKnob();			// calculate size of slider knob
    boolean Inside(Event&);		// true if inside slider knob
    void Constrain(Event&);		// constrain slider knob motion
    void Move(IntCoord dx, IntCoord dy);// move view to reflect slider position
    void Slide(Event&);			// rubberband rect while mousing
    void Jump(Event&);			// for click outside knob
};

#include <IV-2_6/_leave.h>

#endif
