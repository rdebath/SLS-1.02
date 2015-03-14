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
 * Segment class for digital clock
 */

#include "dclock.h"
#include "segment.h"
#include <stdio.h>

Painter* Segment::fadePainter[17];

// pattern initialization
const int myPatSeed[17] = {
    0x0000, 0x8000, 0x8020, 0xA020,
    0xA0A0, 0xA4A0, 0xA4A1, 0xA5A1,
    0xA5A5, 0xA5B5, 0xE5B5, 0xF5B5,
    0xF5F5, 0xF5F7, 0xFDF7, 0xFFF7,
    0xFFFF
};

Pattern* Segment::MakePattern (int seed) {
    Pattern* pat;
    int dat[16];
    unsigned int Row[4];

    for (int i = 0; i <= 3; i++) {
	Row[i] = seed & 0xF;
	Row[i] |= Row[i]<<4;
	Row[i] |= Row[i]<<8;
	Row[i] |= Row[i]<<16;
	seed >>= 4;
    }
    for (i = 0; i < 16; i++) {
	dat[i] = Row[i%4];
    }
    pat = new Pattern(dat);
    return pat;
}

Segment::Segment (Seg s, float Xoff, float Yoff) {
    whichSeg = s;
    Xorg = Xoff;
    Yorg = Yoff;
    p.count = SegData[whichSeg].count;
    fade = 0;		// initially off
    fullFade = 16;
    canvas = nil;
}

void Segment::Reconfig (Painter* output) {
    register int i;

    for (i = 0; i <= fullFade; i++) {
	Painter* p = new Painter(output);
	p->SetPattern(MakePattern(myPatSeed[i]));
	fadePainter[i] = p;
    }
}

Segment::~Segment () {}

void Segment::Resize (Canvas* c, int height) {
    canvas = c;
    int w = canvas->Width();
    int h = height;

    for (int i = 0; i < p.count; i++) {
	p.x[i] = Coord((SegData[whichSeg].x[i]+Xorg) * w);
	p.y[i] = Coord((SegData[whichSeg].y[i]+Yorg) * h);
    }
}

void Segment::Draw() {
    if (canvas != nil) {
	fadePainter[fade]->FillPolygon(canvas, p.x, p.y, p.count);
    }
}

void Segment::Redraw () {
    if (!IsOff()) {
	Draw();
    }
}

boolean Segment::On () {
    if (!IsOn()) {
	FadeUp();
    }
    return IsOn();
}

boolean Segment::Off () {
    if (!IsOff()) {
	FadeDown();
    }
    return IsOff();
}
