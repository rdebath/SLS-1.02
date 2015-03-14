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

#include <IV-look/choice.h>
#include <IV-look/telltale.h>
#include <InterViews/deck.h>
#include <InterViews/target.h>

/* class TelltaleState */

TelltaleState::TelltaleState(const TelltaleFlags flags) {
    flags_ = flags;
    group_ = nil;
}

TelltaleState::~TelltaleState() {
    leave_group();
}

void TelltaleState::set(const TelltaleFlags flags, boolean b) {
    TelltaleFlags newflags = b ? (flags_ | flags) : (flags_ & ~flags);
    if (newflags != flags_) {
	flags_ = newflags;
	notify();
	if (group_ != nil) {
	    group_->update(this);
	}
    }
}

boolean TelltaleState::test(const TelltaleFlags flags) const {
    return (flags_ & flags) == flags;
}

void TelltaleState::join(TelltaleGroup* g) {
    if (g != group_) {
	Resource::ref(g);
	leave_group();
	group_ = g;
    }
}

void TelltaleState::leave_group() {
    if (group_ != nil) {
	group_->remove(this);
	Resource::unref(group_);
    }
}

/* class Telltale */

Telltale::Telltale(Glyph* g, TelltaleState* s) : MonoGlyph(g) {
    state_ = s;
    Resource::ref(state_);
    state_->attach(this);
}

Telltale::~Telltale() {
    state_->detach(this);
    Resource::unref(state_);
}

void Telltale::state(TelltaleState* s) {
    Resource::ref(s);
    Resource::unref(state_);
    state_ = s;
}

TelltaleState* Telltale::state() const { return state_; }

void Telltale::disconnect(Observable*) {
    state_ = nil;
}

/* class TelltaleGroup */

TelltaleGroup::TelltaleGroup() {
    current_ = nil;
}

TelltaleGroup::~TelltaleGroup() { }

void TelltaleGroup::update(TelltaleState* s) {
    if (s != current_ && s->test(TelltaleState::is_chosen)) {
	if (current_ != nil) {
	    current_->set(TelltaleState::is_chosen, false);
	}
	current_ = s;
    }
}

void TelltaleGroup::remove(TelltaleState* s) {
    if (current_ == s) {
	current_ = nil;
    }
}

/* class ChoiceItem */

ChoiceItem::ChoiceItem(TelltaleState* t) : Telltale(nil, t) {
    init();
}

ChoiceItem::ChoiceItem(
    TelltaleState* t, Glyph* normal, Glyph* pressed
) : Telltale(nil, t) {
    init();
    look(0, TelltaleState::is_active, normal);
    look(TelltaleState::is_enabled_active, 0, pressed);
}

ChoiceItem::ChoiceItem(
    TelltaleState* t,
    Glyph* disabled,
    Glyph* enabled, Glyph* visible, Glyph* visible_active, Glyph* active,
    Glyph* chosen, Glyph* visible_chosen, Glyph* active_chosen,
    Glyph* visible_active_chosen, Glyph* disabled_chosen
) : Telltale(nil, t) {
    init();
    look(0, TelltaleState::is_enabled_chosen, disabled);
    look(
	TelltaleState::is_enabled, TelltaleState::is_visible_active_chosen,
	enabled
    );
    look(
	TelltaleState::is_enabled_visible, TelltaleState::is_active_chosen,
	visible
    );
    look(
	TelltaleState::is_enabled_visible_active, TelltaleState::is_chosen,
	visible_active
    );
    look(
	TelltaleState::is_enabled_active, TelltaleState::is_visible_chosen,
	active
    );
    look(
	TelltaleState::is_enabled_chosen, TelltaleState::is_visible_active,
	chosen
    );
    look(
	TelltaleState::is_enabled_visible_chosen, TelltaleState::is_active,
	visible_chosen
    );
    look(
	TelltaleState::is_enabled_active_chosen, TelltaleState::is_visible,
	active_chosen
    );
    look(
	TelltaleState::is_enabled_visible_active_chosen, 0,
	visible_active_chosen
    );
    look(
	TelltaleState::is_chosen, TelltaleState::is_enabled, disabled_chosen
    );
}

ChoiceItem::~ChoiceItem() { }

void ChoiceItem::init() {
    deck_ = new Deck;
    for (TelltaleFlags i = 0; i < TelltaleState::max_flags; i++) {
	index_[i] = -1;
    }
    state()->set(TelltaleState::is_enabled, true);
    body(new Target(deck_, TargetPrimitiveHit));
}

void ChoiceItem::look(
    const TelltaleFlags include, const TelltaleFlags exclude, Glyph* g
) {
    GlyphIndex g_index = -1;
    TelltaleFlags flags = state()->flags();
    for (TelltaleFlags s = 0; s < TelltaleState::max_flags; s++) {
	if ((s & include) == include && (s & exclude) == 0) {
	    GlyphIndex& i = index_[s];
	    if (i == -1) {
		if (g_index == -1) {
		    g_index = deck_->count();
		    deck_->append(g);
		}
		i = g_index;
		if (s == flags) {
		    deck_->flip_to(g_index);
		}
	    } else {
		deck_->replace(i, g);
	    }
	}
    }
}

Glyph* ChoiceItem::look(const TelltaleFlags s) const {
    if (s < TelltaleState::max_flags && index_[s] != -1) {
	return deck_->component(index_[s]);
    }
    return nil;
}

void ChoiceItem::update(Observable*) {
    TelltaleFlags s = state()->flags();
    if (s < TelltaleState::max_flags && index_[s] != -1) {
	deck_->flip_to(index_[s]);
    }
}
