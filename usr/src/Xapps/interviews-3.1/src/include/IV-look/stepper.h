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
 * Stepper -- button with autorepeat
 */

#ifndef ivlook_stepper_h
#define ivlook_stepper_h

#include <IV-look/button.h>
#include <InterViews/adjust.h>

#include <InterViews/_enter.h>

class Color;
class IOHandler;
class TelltaleState;

class Stepper : public Button {
public:
    Stepper(Glyph*, Style*, TelltaleState*, Action* = nil);
    virtual ~Stepper();

    virtual void press(const Event&);
    virtual void release(const Event&);

    virtual void start_stepping();
    virtual void stop_stepping();
protected:
    virtual void adjust() = 0;
private:
    long start_delay_;
    long next_delay_;
    IOHandler* timer_;

    void tick(long, long);
};

#define declareAdjustStepper(StepperSubclassName) \
class StepperSubclassName : public Stepper { \
public: \
    StepperSubclassName( \
	Glyph*, Style*, TelltaleState*, Adjustable*, DimensionName \
    ); \
    virtual ~StepperSubclassName(); \
\
    virtual void adjust(); \
private: \
    Adjustable* adjustable_; \
    DimensionName dimension_; \
};

declareAdjustStepper(ForwardScroller)
declareAdjustStepper(BackwardScroller)
declareAdjustStepper(ForwardPager)
declareAdjustStepper(BackwardPager)

#define declareArrowGlyph(ArrowSubclassName) \
class ArrowSubclassName : public Glyph { \
public: \
    ArrowSubclassName(const Color*); \
    virtual ~ArrowSubclassName(); \
\
    virtual void draw(Canvas*, const Allocation&) const; \
private: \
    const Color* color_; \
};

declareArrowGlyph(UpArrow)
declareArrowGlyph(DownArrow)
declareArrowGlyph(LeftArrow)
declareArrowGlyph(RightArrow)

#include <InterViews/_leave.h>

#endif
