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

#ifndef segment_h
#define segment_h

typedef unsigned int Seg;
enum { SegA, SegB, SegC, SegD, SegE, SegF, SegG };

class Segment {
public:
    Segment(Seg, float, float);
    ~Segment();
    void Reconfig(Painter*);
    void Resize(Canvas*, int);
    void Redraw();
    boolean On();
    boolean Off();
private:
    SegPoints p;			// polygon points for this segment
    Seg whichSeg;			// for indexing into segment data array
    float Xorg;				// fractional amount from canvas origin
    float Yorg;
    Canvas *canvas;			// canvas of DFace
    int fullFade;
    int fade;				// 0 = off; fullFade = on
    static Painter* fadePainter[17];	// painters for fading

    Pattern* MakePattern(int seed);

    void FadeUp () {
	if (fade < fullFade) {
	    fade += FadeStep;
	    Draw();
	}
    }
    void FadeDown() {
	if (fade > 0) {
	    fade -= FadeStep;
	    Draw();
	}
    }
    boolean IsOn () { return fade == fullFade; }
    boolean IsOff() { return fade == 0; }
    void Draw();
};

#endif
