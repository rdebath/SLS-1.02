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
 * Implementation of persistent paint subclasses (nee idraw code).
 */

#include <Unidraw/globals.h>
#include <Unidraw/Graphic/pspaint.h>

#include <IV-2_6/InterViews/world.h>

#include <OS/memory.h>

#include <IV-2_6/_enter.h>

#include <stdlib.h>
#include <string.h>

/*****************************************************************************/

PSBrush::PSBrush () : Brush(0) { _none = true; }

PSBrush::PSBrush (int p, int w) : Brush(p, w) {
    CalcDashPat(p);
    _linepat = p;
    _none = false;
}

int PSBrush::Width () { return _none ? 0 : Brush::Width(); }

void PSBrush::CalcDashPat (int pat) {
    calc_dashes(pat, _dashpat, _dashpatsize);

    /* needed to make printout look exactly like screen */
    if (_dashpatsize & 1) {
	_dashpat[_dashpatsize++] = 0;
    }
    _dashoffset = 0;
}

/*****************************************************************************/

PSColor::PSColor(
    ColorIntensity r, ColorIntensity g, ColorIntensity b, const char* name
) : Color(r, g, b) {
    _name = strnew(name);
    _r = r;
    _g = g;
    _b = b;
}

PSColor::~PSColor () { delete _name; }

void PSColor::GetIntensities(
    ColorIntensity& r, ColorIntensity& g, ColorIntensity& b
) {
    r = _r;
    g = _g;
    b = _b;
}

/*****************************************************************************/

PSFont::PSFont(const char* name, const char* pf, const char* ps) : Font(name) {
    _name = strnew(name);
    _printfont = strnew(pf);
    _printsize = strnew(ps);
    _printfontandsize = new char[strlen(_printfont)+1 + strlen(_printsize)+1];
    strcpy(_printfontandsize, _printfont);
    strcat(_printfontandsize, " ");
    strcat(_printfontandsize, _printsize);
    _lineHt = atoi(_printsize);
}

PSFont::~PSFont () {
    delete _name;
    delete _printfont;
    delete _printsize;
    delete _printfontandsize;
}

/*****************************************************************************/

PSPattern::PSPattern () : Pattern(0xffff) {
    _graylevel = -1;
    _size = 0;
    _none = true;
}

PSPattern::PSPattern (int dither, float g) : Pattern(dither) {
    _graylevel = g;
    _size = 0;
    _none = false;
}

PSPattern::PSPattern (const int* data, int s) : Pattern(data) {
    Memory::copy(data, _data, sizeof(int) * patternHeight);
    _graylevel = -1;
    _size = s;
    _none = false;
}
