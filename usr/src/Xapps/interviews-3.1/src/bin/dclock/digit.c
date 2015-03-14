/*
 * Copyright (c) 1987, 1988, 1989 Stanford University
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
 * Digit class for digital clock
 */

#include "dclock.h"
#include "digit.h"
#include "segment.h"

Digit::Digit (float Xoff, float Yoff) {
    Xorg = Xoff;
    Yorg = Yoff;
    for (Seg s = SegA; s <= SegG; s++) {
	Segs[s] = new Segment(s, Xoff, Yoff);
    }
}

Digit::~Digit () {
    for (Seg s = SegA; s <= SegG; s++) {
	delete Segs[s];
    }
}

boolean Digit::Set (int value) {
    if (value > 9 || value < 0) {
	// out of range, use blank
	value = 10;
    }
    boolean done = true;
    for (Seg s = SegA; s <= SegG; s++) {
	done &= SegCode[value][s] ? Segs[s]->On() : Segs[s]->Off();
    };
    return done;
}

void Digit::Reconfig (Painter* output) {
    // configure any segment to initialize all segment patterns
    Segs[SegA]->Reconfig(output);
}

void Digit::Resize (Canvas* canvas, int height) {
    for (Seg s = SegA; s <= SegG; s++) {
	Segs[s]->Resize(canvas, height);
    }
}

void Digit::Redraw () {
    for (Seg s = SegA; s <= SegG; s++) {
	Segs[s]->Redraw();
    }
}
