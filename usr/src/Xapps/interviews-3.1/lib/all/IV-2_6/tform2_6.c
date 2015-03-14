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

/*
 * Implementation of transformation matrix operations.
 */

#include <InterViews/transformer.h>
#include <OS/math.h>
#include <math.h>

Transformer::Transformer(const Transformer* t) {
    if (t == nil) {
	identity_ = true;
	mat00 = mat11 = 1;
	mat01 = mat10 = mat20 = mat21 = 0;
    } else {
	mat00 = t->mat00;	mat01 = t->mat01;
	mat10 = t->mat10;	mat11 = t->mat11;
	mat20 = t->mat20;	mat21 = t->mat21;
	update();
    }
    ref();
}

void Transformer::Transform(IntCoord& x, IntCoord& y) const {
    IntCoord tx = x;
    x = Math::round(tx*mat00 + y*mat10 + mat20);
    y = Math::round(tx*mat01 + y*mat11 + mat21);
}

void Transformer::InvTransform(IntCoord& tx, IntCoord& ty) const {
    float d = det();
    float a = (float(tx) - mat20) / d;
    float b = (float(ty) - mat21) / d;

    tx = Math::round(a*mat11 - b*mat10);
    ty = Math::round(b*mat00 - a*mat01);
}

void Transformer::InvTransform(
    IntCoord tx, IntCoord ty, IntCoord& x, IntCoord& y
) const {
    float d = det();
    float a = (float(tx) - mat20) / d;
    float b = (float(ty) - mat21) / d;

    x = Math::round(a*mat11 - b*mat10);
    y = Math::round(b*mat00 - a*mat01);
}

void Transformer::InvTransform(float tx, float ty, float& x, float& y) const {
    float d = det();
    float a = (tx - mat20) / d;
    float b = (ty - mat21) / d;

    x = a*mat11 - b*mat10;
    y = b*mat00 - a*mat01;
}

void Transformer::TransformList(IntCoord x[], IntCoord y[], int n) const {
    register IntCoord* ox, * oy;
    IntCoord* lim;

    lim = &x[n];
    for (ox = x, oy = y; ox < lim; ox++, oy++) {
	Transform(*ox, *oy);
    }
}

void Transformer::TransformList(
    IntCoord x[], IntCoord y[], int n, IntCoord tx[], IntCoord ty[]
) const {
    register IntCoord* ox, * oy, * nx, * ny;
    IntCoord* lim;

    lim = &x[n];
    for (ox = x, oy = y, nx = tx, ny = ty; ox < lim; ox++, oy++, nx++, ny++) {
	Transform(*ox, *oy, *nx, *ny);
    }
}

void Transformer::InvTransformList(IntCoord tx[], IntCoord ty[], int n) const {
    register IntCoord* ox, * oy;
    IntCoord* lim;
    float a, b, d = det();

    lim = &tx[n];
    for (ox = tx, oy = ty; ox < lim; ox++, oy++) {
        a = (float(*ox) - mat20) / d;
        b = (float(*oy) - mat21) / d;

        *ox = Math::round(a*mat11 - b*mat10);
        *oy = Math::round(b*mat00 - a*mat01);
    }
}

void Transformer::InvTransformList(
    IntCoord tx[], IntCoord ty[], int n, IntCoord x[], IntCoord y[]
) const {
    register IntCoord* ox, * oy, * nx, * ny;
    IntCoord* lim;
    float a, b, d = det();

    lim = &tx[n];
    for (ox = tx, oy = ty, nx = x, ny = y; ox < lim; ox++, oy++, nx++, ny++) {
        a = (float(*ox) - mat20) / d;
        b = (float(*oy) - mat21) / d;

        *nx = Math::round(a*mat11 - b*mat10);
        *ny = Math::round(b*mat00 - a*mat01);
    }
}

void Transformer::TransformRect(
    IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
) const {
    float tx00, ty00, tx10, ty10, tx11, ty11, tx01, ty01;

    Transform(float(x0), float(y0), tx00, ty00);
    Transform(float(x1), float(y0), tx10, ty10);
    Transform(float(x1), float(y1), tx11, ty11);
    Transform(float(x0), float(y1), tx01, ty01);
    x0 = Math::round(Math::min(tx00, tx01, tx10, tx11));
    y0 = Math::round(Math::min(ty00, ty01, ty10, ty11));
    x1 = Math::round(Math::max(tx00, tx01, tx10, tx11));
    y1 = Math::round(Math::max(ty00, ty01, ty10, ty11));
}

void Transformer::TransformRect(
    float& x0, float& y0, float& x1, float& y1
) const {
    float tx00, ty00, tx10, ty10, tx11, ty11, tx01, ty01;

    Transform(x0, y0, tx00, ty00);
    Transform(x1, y0, tx10, ty10);
    Transform(x1, y1, tx11, ty11);
    Transform(x0, y1, tx01, ty01);
    x0 = Math::min(tx00, tx01, tx10, tx11);
    y0 = Math::min(ty00, ty01, ty10, ty11);
    x1 = Math::max(tx00, tx01, tx10, tx11);
    y1 = Math::max(ty00, ty01, ty10, ty11);
}

void Transformer::InvTransformRect(
    IntCoord& x0, IntCoord& y0, IntCoord& x1, IntCoord& y1
) const {
    float tx00, ty00, tx10, ty10, tx11, ty11, tx01, ty01;

    InvTransform(float(x0), float(y0), tx00, ty00);
    InvTransform(float(x1), float(y0), tx10, ty10);
    InvTransform(float(x1), float(y1), tx11, ty11);
    InvTransform(float(x0), float(y1), tx01, ty01);
    x0 = Math::round(Math::min(tx00, tx01, tx10, tx11));
    y0 = Math::round(Math::min(ty00, ty01, ty10, ty11));
    x1 = Math::round(Math::max(tx00, tx01, tx10, tx11));
    y1 = Math::round(Math::max(ty00, ty01, ty10, ty11));
}

void Transformer::InvTransformRect(
    float& x0, float& y0, float& x1, float& y1
) const {
    float tx00, ty00, tx10, ty10, tx11, ty11, tx01, ty01;

    InvTransform(x0, y0, tx00, ty00);
    InvTransform(x1, y0, tx10, ty10);
    InvTransform(x1, y1, tx11, ty11);
    InvTransform(x0, y1, tx01, ty01);
    x0 = Math::min(tx00, tx01, tx10, tx11);
    y0 = Math::min(ty00, ty01, ty10, ty11);
    x1 = Math::max(tx00, tx01, tx10, tx11);
    y1 = Math::max(ty00, ty01, ty10, ty11);
}

void Transformer::Transform(
    IntCoord x, IntCoord y, IntCoord& tx, IntCoord& ty
) const {
    tx = Math::round(x*mat00 + y*mat10 + mat20);
    ty = Math::round(x*mat01 + y*mat11 + mat21);
}

void Transformer::Transform(float x, float y, float& tx, float& ty) const {
    tx = x*mat00 + y*mat10 + mat20;
    ty = x*mat01 + y*mat11 + mat21;
}
