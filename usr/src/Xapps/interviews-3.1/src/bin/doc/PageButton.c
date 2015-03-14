/*
 * Copyright (c) 1991 Stanford University
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Stanford not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  Stanford makes no representations about
 * the suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * STANFORD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
 * IN NO EVENT SHALL STANFORD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
 * WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*
 * page button Telltale
 */

#include "PageButton.h"
#include "doc-target.h"

#include <InterViews/background.h>
#include <InterViews/bitmap.h>
#include <InterViews/box.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <InterViews/layout.h>
#include <InterViews/patch.h>
#include <InterViews/stencil.h>

#include "pagePlain.bm"
#include "pageHit.bm"
#include "pageChosen.bm"
#include "pageBoth.bm"

Color* PageButton::__fade;
Bitmap* PageButton::__plain;
Bitmap* PageButton::__hit;
Bitmap* PageButton::__chosen;
Bitmap* PageButton::__both;

PageButton::PageButton(
    Glyph* label, const Color* c
) : Telltale(nil, new TelltaleState(TelltaleState::is_enabled)) {
    if (__fade == nil) {
        __fade = new Color(1.0, 1.0, 1.0, 0.5);
        __fade->ref();
    }
    if (__plain == nil) {
        __plain = new Bitmap(
            pagePlain_bits, pagePlain_width, pagePlain_height,
            pagePlain_x_hot, pagePlain_y_hot
        );
        __plain->ref();
        __hit = new Bitmap(
            pageHit_bits, pageHit_width, pageHit_height,
            pageHit_x_hot, pageHit_y_hot
        );
        __hit->ref();
        __chosen = new Bitmap(
            pageChosen_bits, pageChosen_width, pageChosen_height,
            pageChosen_x_hot, pageChosen_y_hot
        );
        __chosen->ref();
        __both = new Bitmap(
            pageBoth_bits, pageBoth_width, pageBoth_height,
            pageBoth_x_hot, pageBoth_y_hot
        );
        __both->ref();
    }
    color_ = c;
    Resource::ref(color_);
    const LayoutKit& layout = *LayoutKit::instance();
    patch_ = new Patch(
        new DocTarget(
            layout.overlay(
                layout.shape_of(new Stencil(__plain, color_)),
		layout.hcenter(label)
            )
        )
    );
    body(patch_);
}

PageButton::~PageButton() {
    Resource::unref(color_);
}

void PageButton::allocate(Canvas* c, const Allocation& a, Extension& ext) {
    ext.merge(c, a);
    Telltale::allocate(c, a, ext);
}

void PageButton::draw(Canvas* c, const Allocation& a) const {
    Telltale::draw(c, a);
    Coord x = a.x();
    Coord y = a.y();
    Bitmap* b;
    TelltaleState* s = state();
    if (s->test(TelltaleState::is_chosen | TelltaleState::is_active)) {
	b = __both;
    } else if (s->test(TelltaleState::is_chosen)) {
	b = __chosen;
    } else if (s->test(TelltaleState::is_active)) {
	b = __hit;
    } else {
	b = __plain;
    }
    c->stencil(b, color_, x, y);
    if (!enabled()) {
	c->fill_rect(a.left(), a.bottom(), a.right(), a.top(), __fade);
    }
}

void PageButton::state_changed(const TelltaleState) {
    patch_->redraw();
}
