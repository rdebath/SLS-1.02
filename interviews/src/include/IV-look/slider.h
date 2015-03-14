/*
 * Copyright (c) 1991 Stanford University
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
 * Slider - common slider behavior
 */

#ifndef ivlook_slider_h
#define ivlook_slider_h

#include <InterViews/adjust.h>
#include <InterViews/input.h>
#include <InterViews/observe.h>

class SliderImpl;
class Stepper;
class Style;

typedef void (Adjustable::*SliderAdjustment)(DimensionName);

class Slider : public ActiveHandler, public Observer {
public:
    Slider(Style*);
    virtual ~Slider();

    virtual void normal_thumb(Glyph*);
    virtual void visible_thumb(Glyph*);
    virtual void old_thumb(Glyph*);
    virtual Coord minimum_thumb_size() const;

    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void undraw();

    virtual void move(const Event&);
    virtual void press(const Event&);
    virtual void drag(const Event&);
    virtual void release(const Event&);

    virtual void allocation_changed(Canvas*, const Allocation&);

    virtual void update(Observable*);

    virtual void forward_stepper(Stepper*);
    virtual void backward_stepper(Stepper*);
    virtual void move_to(Coord x, Coord y);
    virtual void allocate_thumb(const Allocation&);
    virtual void allot_thumb_major_axis(
	const Allocation&, DimensionName, Adjustable*, Coord min_thumb_size,
	float& scale, Allotment&
    );
    virtual void allot_thumb_minor_axis(const Allotment&, Allotment&);
    virtual void redraw_thumb();
    virtual void reallocate_thumb(const Allocation&);
    virtual void apply_adjustment(SliderAdjustment);
    virtual void do_adjustment(Adjustable*, SliderAdjustment, DimensionName);
private:
    SliderImpl* impl_;
};

class XSlider : public Slider {
public:
    XSlider(Style*, Adjustable*);
    virtual ~XSlider();

    virtual void move_to(Coord x, Coord y);
    virtual void allocate_thumb(const Allocation&);
    virtual void disconnect(Observable*);
    virtual void apply_adjustment(SliderAdjustment);
private:
    Adjustable* adjustable_;
    float xscale_;
    Stepper* forward_;
    Stepper* backward_;
};

class YSlider : public Slider {
public:
    YSlider(Style*, Adjustable*);
    virtual ~YSlider();

    virtual void move_to(Coord x, Coord y);
    virtual void allocate_thumb(const Allocation&);
    virtual void disconnect(Observable*);
    virtual void apply_adjustment(SliderAdjustment);
private:
    Adjustable* adjustable_;
    float yscale_;
    Stepper* forward_;
    Stepper* backward_;
};

class XYSlider : public Slider {
public:
    XYSlider(Style*, Adjustable* x, Adjustable* y);
    virtual ~XYSlider();

    virtual void move_to(Coord x, Coord y);
    virtual void allocate_thumb(const Allocation&);
    virtual void disconnect(Observable*);
    virtual void apply_adjustment(SliderAdjustment);
private:
    Adjustable* x_adjustable_;
    Adjustable* y_adjustable_;
    float xscale_;
    float yscale_;
};

#endif
