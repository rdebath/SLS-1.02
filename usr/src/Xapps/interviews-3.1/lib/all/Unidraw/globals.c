/*
 * Copyright (c) 1990, 1991 Stanford University
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
 * Implementation of unidraw global stuff.
 */

#include <Unidraw/globals.h>
#include <Unidraw/Graphic/graphic.h>

#include <InterViews/resource.h>

#include <IV-2_6/_enter.h>

#include <string.h>

/*
 * global data
 */

CSolver* csolver;
Unidraw* unidraw;

PSColor* psblack;
PSColor* pswhite;
PSPattern* pssolid;
PSPattern* psclear;
PSPattern* psnonepat;
PSBrush* pssingle;
PSBrush* psnonebr;
PSFont* psstdfont;
Graphic* stdgraphic;

/*
 * global functions
 */

void NormalRect (Coord& left, Coord& bottom, Coord& right, Coord& top) {
    Coord tempx, tempy;
    
    tempx = min(left, right);
    right = max(left, right);
    left = tempx;
    
    tempy = min(bottom, top);
    top = max(bottom, top);
    bottom = tempy;
}

void GetLine (
    const char* s, int size, int begin, int& end, int& lineSize, int& nextBegin
) {
    int i = begin;

    while (i < size) {
        if (s[i] == '\n') {
            break;
        } else {
            ++i;
        }
    }
    end = i - 1;
    nextBegin = i + 1;
    lineSize = i - begin;
}

void GetAlignmentPoint (Graphic* gr, Alignment a, float& x, float& y) {
    float l, b, r, t;

    gr->GetBounds(l, b, r, t);
    switch (a) {
	case BottomLeft:
	case CenterLeft:
	case TopLeft:
	case Left:
	    x = l;
	    break;
	case BottomCenter:
	case Center:
	case TopCenter:
	case HorizCenter:
	    x = (l + r)/2;
	    break;
	case BottomRight:
	case CenterRight:
	case TopRight:
	case Right:
	    x = r;
	    break;
    }
    switch (a) {
	case BottomLeft:
	case BottomCenter:
	case BottomRight:
	case Bottom:
            y = b;
	    break;
	case CenterLeft:
	case Center:
	case CenterRight:
	case VertCenter:
	    y = (b + t)/2;
	    break;
	case TopLeft:
	case TopCenter:
	case TopRight:
	case Top:
	    y = t;
	    break;
    }
}

void Ref (Resource* r) {
    if (r != nil) {
        r->Reference();
    }
}

char* strnew (const char* s) {
    char* dup = new char[strlen(s) + 1];
    strcpy(dup, s);
    return dup;
}
