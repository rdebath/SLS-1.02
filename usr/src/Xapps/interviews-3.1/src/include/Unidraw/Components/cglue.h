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
 * CGlue - connector glue for specifying connection behavior
 */

#ifndef unidraw_components_cglue_h
#define unidraw_components_cglue_h

#include <InterViews/enter-scope.h>
#include <Unidraw/enter-scope.h>

class Shape;

class CGlue {
public:
    CGlue(
        float hnat = 0, float vnat = 0,
        float hshr = 0, float hstr = 0, float vshr = 0, float vstr = 0,
        float hshrlim = 0, float hstrlim = 0,
        float vshrlim = 0, float vstrlim = 0
    );
    CGlue(const Shape&);
    CGlue* Copy();

    void Rigid();
    void Interpose(CGlue*);
public:
    float _width, _height;
    float _hshrink, _hstretch, _vshrink, _vstretch;
    float _hshrlim, _hstrlim, _vshrlim, _vstrlim;    
};

#endif
