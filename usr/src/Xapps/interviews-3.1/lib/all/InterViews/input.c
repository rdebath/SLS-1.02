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
 * InputHandler - glyph that handles input
 */

#include <InterViews/alloctbl.h>
#include <InterViews/canvas.h>
#include <InterViews/display.h>
#include <InterViews/event.h>
#include <InterViews/handler.h>
#include <InterViews/hit.h>
#include <InterViews/input.h>
#include <InterViews/style.h>
#include <InterViews/transformer.h>
#include <InterViews/window.h>
#include <OS/list.h>

declarePtrList(InputHandlerList,InputHandler)
implementPtrList(InputHandlerList,InputHandler)

class InputHandlerImpl : public Handler {
private:
    friend class InputHandler;

    InputHandlerImpl(InputHandler*, Style*);
    virtual ~InputHandlerImpl();

    InputHandler* input_;
    Style* style_;
    InputHandlerList children_;
    GlyphIndex focus_item_;
    InputHandler* focus_handler_;
    InputHandler* parent_;
    AllocationTable* allocations_;
    boolean pressed_ : 1;
    boolean recorded_time_ : 1;
    EventButton button_;
    unsigned long click_time_;

    virtual boolean event(Event&);

    AllocationInfo& info(Canvas*, const Allocation&);
    AllocationInfo* most_recent_info();
    void reset();
    void down(Event&);
    void motion(Event&);
    void up(Event&);
    boolean inside(const Event&, const AllocationInfo&);

    static unsigned long threshold_;
};

unsigned long InputHandlerImpl::threshold_ = 0;

InputHandler::InputHandler(Glyph* g, Style* s) : MonoGlyph(g) {
    impl_ = new InputHandlerImpl(this, s);
    Resource::ref(impl_);
}

InputHandler::~InputHandler() {
    AllocationInfo* info = impl_->most_recent_info();
    if (info != nil) {
	info->canvas()->window()->display()->ungrab(impl_, true);
    }
    Resource::unref(impl_);
}

Handler* InputHandler::handler() const {
    return impl_;
}

InputHandler* InputHandler::parent() const {
    return impl_->parent_;
}

Style* InputHandler::style() const {
    return impl_->style_;
}

void InputHandler::append_input_handler(InputHandler* h) {
    if (h != nil) {
	impl_->children_.append(h);
	h->impl_->parent_ = this;
    }
}

void InputHandler::remove_input_handler(GlyphIndex index) {
    InputHandlerImpl& i = *impl_;
    InputHandlerList& list = i.children_;
    if (list.item(index) == i.focus_handler_) {
	next_focus();
	if (list.item(index) == i.focus_handler_) {
	    i.focus_handler_ = nil;
	}
    }
    list.remove(index);
}

void InputHandler::remove_all_input_handlers() {
    InputHandlerImpl& i = *impl_;
    i.children_.remove_all();
    i.focus_handler_ = nil;
}

GlyphIndex InputHandler::input_handler_count() const {
    return impl_->children_.count();
}

InputHandler* InputHandler::input_handler(GlyphIndex index) const {
    return impl_->children_.item(index);
}

void InputHandler::focus(InputHandler* h) {
    InputHandlerImpl& i = *impl_;
    GlyphIndex n = i.children_.count();
    for (GlyphIndex g = 0; g < n; g++) {
	if (i.children_.item(g) == h) {
	    if (i.focus_handler_ != nil) {
		i.focus_handler_->focus_out();
	    }
	    i.focus_item_ = g;
	    i.focus_handler_ = h->focus_in();
	    break;
	}
    }
}

void InputHandler::next_focus() {
    InputHandlerImpl& i = *impl_;
    if (i.focus_handler_ != nil) {
	i.focus_handler_->focus_out();
    }
    GlyphIndex n = i.children_.count();
    GlyphIndex f = i.focus_item_ + 1;
    if (f >= n) {
	if (i.parent_ != nil) {
	    i.parent_->next_focus();
	    return;
	} else if (n == 0) {
	    return;
	} else {
	    f = 0;
	}
    }
    i.focus_item_ = f;
    i.focus_handler_ = i.children_.item(f)->focus_in();
}

void InputHandler::prev_focus() {
    InputHandlerImpl& i = *impl_;
    if (i.focus_handler_ != nil) {
	i.focus_handler_->focus_out();
    }
    GlyphIndex n = i.children_.count();
    GlyphIndex f = i.focus_item_ - 1;
    if (f < 0) {
	if (i.parent_ != nil) {
	    i.parent_->prev_focus();
	    return;
	} else if (n == 0) {
	    return;
	} else {
	    f = n - 1;
	}
    }
    i.focus_item_ = f;
    i.focus_handler_ = i.children_.item(f)->focus_in();
}

void InputHandler::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    AllocationInfo& info = impl_->info(c, a);
    ext.merge(info.extension());
    allocation_changed(c, a);
}

void InputHandler::draw(Canvas* c, const Allocation& a) const {
    AllocationInfo& info = impl_->info(c, a);
    Glyph* g = body();
    if (g != nil && c->damaged(info.extension())) {
	g->draw(c, a);
    }
}

void InputHandler::pick(Canvas* c, const Allocation& a, int depth, Hit& h) {
    AllocationInfo& info = impl_->info(c, a);
    const Event* e = h.event();
    EventType t = (e == nil) ? Event::undefined : e->type();
    switch (t) {
    case Event::key:
	if (impl_->inside(*e, info)) {
	    InputHandler* ih = impl_->focus_handler_;
	    InputHandlerImpl* handler = (ih == nil) ? impl_ : ih->impl_;
	    h.target(depth, this, 0, handler); 
	}
	break;
    case Event::undefined:
    case Event::other_event:
	MonoGlyph::pick(c, a, depth, h);
	break;
    default:
	h.begin(depth, this, 0, impl_);
	MonoGlyph::pick(c, a, depth, h);
	h.end();
	break;
    }
}

void InputHandler::undraw() {
    MonoGlyph::undraw();
    AllocationTable* table = impl_->allocations_;
    if (table != nil) {
	AllocationInfo* info = impl_->most_recent_info();
	if (info != nil) {
	    Window* w = info->canvas()->window();
	    if (w != nil) {
		w->display()->ungrab(impl_, true);
	    }
	}
	table->flush();
    }
}

Canvas* InputHandler::canvas() const {
    AllocationInfo* info = impl_->most_recent_info();
    return info == nil ? nil : info->canvas();
}

const Transformer& InputHandler::transformer() const {
    return impl_->most_recent_info()->transformer();
}

const Allocation& InputHandler::allocation() const {
    return impl_->most_recent_info()->allocation();
}

void InputHandler::redraw() const {
    InputHandlerImpl& i = *impl_;
    AllocationInfo* info = i.most_recent_info();
    if (info != nil) {
	info->canvas()->damage(info->extension());
    }
}

void InputHandler::repick(int depth, Hit& h) {
    Canvas* c = canvas();
    if (c != nil) {
	const Transformer& t = transformer();
	c->push_transform();
	c->transformer(t);
	h.push_transform();
	h.transform(t);
	pick(c, allocation(), depth, h);
	h.pop_transform();
	c->pop_transform();
    }
}

void InputHandler::move(const Event&) { }
void InputHandler::press(const Event&) { }
void InputHandler::drag(const Event&) { }
void InputHandler::release(const Event&) { }

void InputHandler::keystroke(const Event& e) {
    InputHandlerImpl& i = *impl_;
    if (i.focus_item_ != -1) {
	i.focus_handler_->keystroke(e);
    }
}

void InputHandler::double_click(const Event&) { }

InputHandler* InputHandler::focus_in() { return this; }
void InputHandler::focus_out() { }

void InputHandler::allocation_changed(Canvas*, const Allocation&) { }

boolean InputHandler::inside(const Event& e) {
    InputHandlerImpl& i = *impl_;
    AllocationInfo* info = i.most_recent_info();
    return info != nil && i.inside(e, *info);
}

/* class InputHandlerImpl */

InputHandlerImpl::InputHandlerImpl(InputHandler* h, Style* s) {
    input_ = h;
    Resource::ref(s);
    style_ = s;
    parent_ = nil;
    allocations_ = nil;
    focus_item_ = -1;
    focus_handler_ = nil;
    reset();
    if (threshold_ == 0) {
	long t = 250;
	s->find_attribute("clickDelay", t);
	threshold_ = t;
    }
}

InputHandlerImpl::~InputHandlerImpl() {
    Resource::unref(style_);
    delete allocations_;
}

AllocationInfo& InputHandlerImpl::info(Canvas* c, const Allocation& a) {
    if (allocations_ == nil) {
	allocations_ = new AllocationTable(0, 1);
    }
    AllocationInfo* info = allocations_->find(c, a);
    if (info == nil) {
	/*
	 * The need for this code is unfortunate.
	 * The problem is that an input handler needs to ensure
	 * that it ungrabs if a canvas/window is replaced
	 * from the allocation table.  Perhaps there should be
	 * a general-purpose interface for this from allocation table,
	 * but for now we know InputHandler only keeps a single allocation.
	 * So, before allocating a new one we check to see if there is
	 * an old one that has a valid window.  Then we do the ungrab.
	 * If we didn't do anything about the ungrab, then handler
	 * might stay grabbed even when we forgot about the window.
	 */
	AllocationInfo* old_info = allocations_->most_recent();
	if (old_info != nil) {
	    Canvas* old_c = old_info->canvas();
	    if (old_c != nil) {
		Window* old_w = old_c->window();
		if (old_w != nil && old_w != c->window()) {
		    old_w->display()->ungrab(this, true);
		}
	    }
	}
	info = allocations_->allocate(c, a);
	Extension ext;
	ext.clear();
	input_->MonoGlyph::allocate(c, a, ext);
	info->extension(ext);
    }
    return *info;
}

AllocationInfo* InputHandlerImpl::most_recent_info() {
    AllocationTable* a = allocations_;
    if (a != nil) {
	AllocationInfo* info = a->most_recent();
	if (info != nil && info->canvas() != nil) {
	    return info;
	}
    }
    return nil;
}

void InputHandlerImpl::reset() {
    pressed_ = false;
    recorded_time_ = false;
}

boolean InputHandlerImpl::event(Event& e) {
    boolean handled = true;
    switch (e.type()) {
    case Event::down:
	down(e);
	break;
    case Event::motion:
	motion(e);
	break;
    case Event::up:
	up(e);
	break;
    case Event::key:
	input_->keystroke(e);
	break;
    default:
	/* ignore */
	break;
    }
    return handled;
}

void InputHandlerImpl::down(Event& e) {
    if (!pressed_) {
	pressed_ = true;
	button_ = e.pointer_button();
	input_->press(e);
	e.grab(this);
	if (parent_ != nil) {
	    parent_->focus(input_);
	} else {
	    if (focus_handler_ != input_) {
		if (focus_handler_ != nil) {
		    focus_handler_->focus_out();
		    focus_item_ = -1;
		}
		focus_handler_ = input_->focus_in();
	    }
	}
    }
}

void InputHandlerImpl::motion(Event& e) {
    if (pressed_) {
	input_->drag(e);
    } else {
	input_->move(e);
    }
}

void InputHandlerImpl::up(Event& e) {
    if (pressed_ && e.pointer_button() == button_) {
	pressed_ = false;
	e.ungrab(this);
	input_->release(e);
	unsigned long t = e.time();
	if (recorded_time_ && t - click_time_ < threshold_) {
	    input_->double_click(e);
	}
	click_time_ = t;
	recorded_time_ = true;
    }
}

boolean InputHandlerImpl::inside(
    const Event& event, const AllocationInfo& info
) {
    Coord x = event.pointer_x();
    Coord y = event.pointer_y();
    Canvas* c = info.canvas();
    Window* w = c->window();
    if (w == nil || w != event.window()) {
	return false;
    }
    const Extension& e = info.extension();
    if (x < e.right() && x >= e.left() && y < e.top() && y >= e.bottom()) {
	const Transformer& t = info.transformer();
	Hit hit(&event);
	hit.transform(t);
	c->push_transform();
	c->transformer(t);
	input_->MonoGlyph::pick(c, info.allocation(), 0, hit);
	c->pop_transform();
	if (hit.any()) {
	    return true;
	}
    }
    return false;
}

/* class ActiveHandler */

ActiveHandler::ActiveHandler(Glyph* g, Style* s) : InputHandler(g, s) {
    inside_ = false;
}

ActiveHandler::~ActiveHandler() { }

void ActiveHandler::undraw() {
    if (inside_) {
	inside_ = false;
	leave();
    }
    InputHandler::undraw();
}

void ActiveHandler::move(const Event& e) {
    Handler* h = handler();
    if (e.handler() == h) {
	if (!inside_) {
	    inside_ = true;
	    e.grab(h);
	    enter();
	}
    } else if (inside_) {
	inside_ = false;
	leave();
	e.ungrab(h);
    }
}

void ActiveHandler::drag(const Event& e) {
    move(e);
}

void ActiveHandler::enter() { }
void ActiveHandler::leave() { }
