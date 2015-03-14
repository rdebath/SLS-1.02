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
 * CGlue implementation.
 */

#include <IV-2_6/InterViews/shape.h>
#include <Unidraw/Components/cglue.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

CGlue::CGlue (
    float hnat, float vnat, float hshr, float hstr, float vshr, float vstr,
    float hshrlim, float hstrlim, float vshrlim, float vstrlim
) {
    _width = hnat;
    _height = vnat;
    _hshrink = hshr;
    _hstretch = hstr;
    _vshrink = vshr;
    _vstretch = vstr;
    _hshrlim = hshrlim;
    _hstrlim = hstrlim;
    _vshrlim = vshrlim;
    _vstrlim = vstrlim;
}

CGlue::CGlue (const Shape& s) {
    _width = s.width;
    _height = s.height;
    _hshrink = _hshrlim = s.hshrink;
    _hstretch = _hstrlim = s.hstretch;
    _vshrink = _vshrlim = s.vshrink;
    _vstretch = _vstrlim = s.vstretch;
}

CGlue* CGlue::Copy () {
    return new CGlue(
        _width, _height, _hshrink, _hstretch, _vshrink,_vstretch,
        _hshrlim, _hstrlim, _vshrlim, _vstrlim
    );
}

inline boolean SignsDiffer (float f1, float f2) {
    return (f1 < 0 && f2 >= 0) || (f1 >= 0 && f2 < 0);
}

void CGlue::Interpose (CGlue* g) {
    if (g != nil) {
        if (SignsDiffer(_width, g->_width)) {
            _hshrink += g->_hstretch;
            _hstretch += g->_hshrink;
            _hshrlim += g->_hstrlim;
            _hstrlim += g->_hshrlim;

        } else {
            _hshrink += g->_hshrink;
            _hstretch += g->_hstretch;
            _hshrlim += g->_hshrlim;
            _hstrlim += g->_hstrlim;
        }

        if (SignsDiffer(_height, g->_height)) {
            _vshrink += g->_vstretch;
            _vstretch += g->_vshrink;
            _vshrlim += g->_vstrlim;
            _vstrlim += g->_vshrlim;

        } else {
            _vshrink += g->_vshrink;
            _vstretch += g->_vstretch;
            _vshrlim += g->_vshrlim;
            _vstrlim += g->_vstrlim;
        }
        _width += g->_width;
        _height += g->_height;
    }
}

void CGlue::Rigid () {
    _hshrink = _hstretch = _vshrink = _vstretch = 0;
    _hshrlim = _hstrlim = _vshrlim = _vstrlim = 0;
}
