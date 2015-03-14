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
 * Stepper -- button with auto-repeat
 */

#include <Dispatch/dispatcher.h>
#include <Dispatch/iocallback.h>
#include <IV-look/stepper.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/style.h>
#include <InterViews/target.h>

declareIOCallback(Stepper)
implementIOCallback(Stepper)

Stepper::Stepper(
    Glyph* g, Style* s, TelltaleState* t, Action* a
) : Button(new Target(g, TargetPrimitiveHit), s, t, a) {
    float seconds = 0.25;
    s->find_attribute("autorepeatStart", seconds);
    start_delay_ = long(seconds * 1000000);
    seconds = 0.05;
    s->find_attribute("autorepeatDelay", seconds);
    next_delay_ = long(seconds * 1000000);
    timer_ = new IOCallback(Stepper)(this, &Stepper::tick);
}

Stepper::~Stepper() {
    delete timer_;
}

void Stepper::press(const Event& e) {
    Button::press(e);
    start_stepping();
}

void Stepper::release(const Event& e) {
    stop_stepping();
    Button::release(e);
}

void Stepper::start_stepping() {
    adjust();
    if (start_delay_ > 10) {
	Dispatcher::instance().startTimer(0, start_delay_, timer_);
    }
}

void Stepper::stop_stepping() {
    Dispatcher::instance().stopTimer(timer_);
}

void Stepper::tick(long, long) {
    adjust();
    Dispatcher::instance().startTimer(0, next_delay_, timer_);
}

#define implementAdjustStepper(name,scroll) \
name::name( \
    Glyph* g, Style* s, TelltaleState* t, Adjustable* a, DimensionName d \
) : Stepper(g, s, t) { \
    adjustable_ = a; \
    dimension_ = d; \
} \
\
name::~name() { } \
\
void name::adjust() { adjustable_->scroll(dimension_); }

implementAdjustStepper(ForwardScroller,scroll_forward)
implementAdjustStepper(BackwardScroller,scroll_backward)
implementAdjustStepper(ForwardPager,page_forward)
implementAdjustStepper(BackwardPager,page_backward)

#define implementArrowGlyph(name) \
name::name(const Color* c) { \
    Resource::ref(c); \
    color_ = c; \
} \
\
name::~name() { \
    Resource::unref(color_); \
}

implementArrowGlyph(UpArrow)
implementArrowGlyph(DownArrow)
implementArrowGlyph(LeftArrow)
implementArrowGlyph(RightArrow)

void UpArrow::draw(Canvas* c, const Allocation& a) const {
    Coord x1 = a.left();
    Coord y1 = a.bottom();
    Coord x2 = a.right();
    Coord y2 = a.top();
    c->new_path();
    c->move_to(x1, y1);
    c->line_to(x2, y1);
    c->line_to((x1 + x2) * 0.5, y2);
    c->close_path();
    c->fill(color_);
}

void DownArrow::draw(Canvas* c, const Allocation& a) const {
    Coord x1 = a.left();
    Coord y1 = a.bottom();
    Coord x2 = a.right();
    Coord y2 = a.top();
    c->new_path();
    c->move_to(x1, y2);
    c->line_to(x2, y2);
    c->line_to((x1 + x2) * 0.5, y1);
    c->close_path();
    c->fill(color_);
}

void LeftArrow::draw(Canvas* c, const Allocation& a) const {
    Coord x1 = a.left();
    Coord y1 = a.bottom();
    Coord x2 = a.right();
    Coord y2 = a.top();
    c->new_path();
    c->move_to(x2, y1);
    c->line_to(x2, y2);
    c->line_to(x1, (y1 + y2) * 0.5);
    c->close_path();
    c->fill(color_);
}

void RightArrow::draw(Canvas* c, const Allocation& a) const {
    Coord x1 = a.left();
    Coord y1 = a.bottom();
    Coord x2 = a.right();
    Coord y2 = a.top();
    c->new_path();
    c->move_to(x1, y1);
    c->line_to(x1, y2);
    c->line_to(x2, (y1 + y2) * 0.5);
    c->close_path();
    c->fill(color_);
}
