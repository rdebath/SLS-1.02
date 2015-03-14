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
 * Browser -- select an item
 */

#include <IV-look/browser.h>
#include <IV-look/telltale.h>
#include <InterViews/action.h>
#include <InterViews/event.h>
#include <InterViews/hit.h>
#include <InterViews/style.h>
#include <OS/list.h>

declarePtrList(BrowserList,TelltaleState)
implementPtrList(BrowserList,TelltaleState)

Browser::Browser(
    Glyph* g, Style* s, Action* accept, Action* cancel
) : InputHandler(g, s) {
    Resource::ref(accept);
    accept_ = accept;
    Resource::ref(cancel);
    cancel_ = cancel;
    items_ = new BrowserList;
    item_ = -1;
}

Browser::~Browser() {
    Resource::unref(accept_);
    Resource::unref(cancel_);
    delete items_;
}

void Browser::append_selectable(TelltaleState* t) {
    items_->append(t);
}

void Browser::replace_selectable(GlyphIndex i, TelltaleState* t) {
    items_->remove(i);
    items_->insert(i, t);
}

void Browser::remove_selectable(GlyphIndex i) {
    items_->remove(i);
}

TelltaleState* Browser::state(GlyphIndex i) const {
    return items_->item(i);
}

void Browser::select(GlyphIndex i) {
    if (item_ != i) {
	if (item_ != -1) {
	    active(item_, false);
	}
	if (i >= -1 && i < items_->count()) {
	    item_ = i;
	    if (i >= 0) {
		active(item_, true);
	    }
	}
    }
}

void Browser::active(GlyphIndex i, boolean b) {
    TelltaleState* t = items_->item(i);
    t->attach(this);
    t->set(TelltaleState::is_active, b);
    t->detach(this);
}

GlyphIndex Browser::selected() const {
    return item_;
}

void Browser::choose(GlyphIndex i) const {
    if (i != -1 && accept_ != nil) {
	accept_->execute();
    }
}

void Browser::cancel() {
    if (cancel_ != nil) {
	cancel_->execute();
    }
}

void Browser::press(const Event& e) {
    Hit h(&e);
    repick(0, h);
    if (h.any()) {
	select(h.index(0));
    }
}

void Browser::drag(const Event& e) {
    if (inside(e)) {
	Hit h(&e);
	repick(0, h);
	if (h.any()) {
	    select(h.index(0));
	    return;
	}
    }
    select(-1);
}

void Browser::release(const Event&) {
    if (style()->value_is_on("singleClick")) {
	choose(item_);
    }
}

void Browser::double_click(const Event&) {
    choose(item_);
}

void Browser::update(Observable*) {
    redraw();
}
