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
 * Adjuster - button-like interactors for incremental adjustment of an
 * interactor's perspective.
 */

#ifndef ivlook2_6_adjuster_h
#define ivlook2_6_adjuster_h

#include <IV-2_6/InterViews/interactor.h>

#include <IV-2_6/_enter.h>

static const int NO_AUTOREPEAT = -1;

class Bitmap;
class Shape;

class Adjuster : public Interactor {
public:
    Adjuster(Interactor*, int = NO_AUTOREPEAT);
    Adjuster(const char*, Interactor*, int = NO_AUTOREPEAT);
    virtual ~Adjuster();

    void Handle(Event&);
    virtual void Highlight(boolean on);
    void Redraw(IntCoord, IntCoord, IntCoord, IntCoord);
    void Reshape(Shape&);
protected:
    Interactor* view;
    Bitmap* plain;
    Bitmap* hit;
    Bitmap* mask;
    int delay;
    boolean timer;
    Perspective* shown;
    boolean highlighted;

    virtual void AdjustView(Event&);
    void AutoRepeat();
    void Flash();
    void HandlePress();
    void Invert();
    virtual void Reconfig();
    void TimerOn();
    void TimerOff();
private:
    void Init(Interactor*, int);
};

class Zoomer : public Adjuster {
public:
    Zoomer(Interactor*, float factor);
    Zoomer(const char*, Interactor*, float factor);
    virtual ~Zoomer();
protected:
    void AdjustView(Event&);
private:
    float factor;

    void Init(float);
};

class Enlarger : public Zoomer {
public:
    Enlarger(Interactor*);
    Enlarger(const char*, Interactor*);
    virtual ~Enlarger();
private:
    void Init();
};

class Reducer : public Zoomer {
public:
    Reducer(Interactor*);
    Reducer(const char*, Interactor*);
    virtual ~Reducer();
private:
    void Init();
};

class Mover : public Adjuster {
public:
    Mover(Interactor*, int delay, int moveType);
    Mover(const char*, Interactor*, int delay, int moveType);
    virtual ~Mover();
protected:
    int moveType;
    void AdjustView(Event&);
private:
    void Init(int);
};

class LeftMover : public Mover {
public:
    LeftMover(Interactor*, int delay = NO_AUTOREPEAT);
    LeftMover(const char*, Interactor*, int delay = NO_AUTOREPEAT);
    virtual ~LeftMover();
private:
    void Init();
};

class RightMover : public Mover {
public:
    RightMover(Interactor*, int delay = NO_AUTOREPEAT);
    RightMover(const char*, Interactor*, int delay = NO_AUTOREPEAT);
    virtual ~RightMover();
private:
    void Init();
};

class UpMover : public Mover {
public:
    UpMover(Interactor*, int delay = NO_AUTOREPEAT);
    UpMover(const char*, Interactor*, int delay = NO_AUTOREPEAT);
    virtual ~UpMover();
private:
    void Init();
};

class DownMover : public Mover {
public:
    DownMover(Interactor*, int delay = NO_AUTOREPEAT);
    DownMover(const char*, Interactor*, int delay = NO_AUTOREPEAT);
    virtual ~DownMover();
private:
    void Init();
};

#include <IV-2_6/_leave.h>

#endif
