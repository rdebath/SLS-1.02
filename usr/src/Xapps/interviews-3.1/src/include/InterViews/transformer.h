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
 * Interface to transformation matrices.
 */

#ifndef iv_transformer_h
#define iv_transformer_h

#include <InterViews/coord.h>
#include <InterViews/resource.h>

#include <InterViews/_enter.h>

class Transformer : public Resource {
public:
    Transformer();	/* identity */
    Transformer(const Transformer&);
    Transformer(const Transformer*);
    Transformer(
	float a00, float a01, float a10, float a11, float a20, float a21
    );
    virtual ~Transformer();

    boolean identity() const;
    boolean invertible() const;

    boolean operator ==(const Transformer&) const;
    boolean operator !=(const Transformer&) const;
    Transformer& operator =(const Transformer&);

    virtual void premultiply(const Transformer&);
    virtual void postmultiply(const Transformer&);
    virtual void invert();

    virtual void translate(float dx, float dy);
    virtual void scale(float sx, float sy);
    virtual void rotate(float angle);
    virtual void skew(float sx, float sy);

    virtual void transform(float& x, float& y) const;
    virtual void transform(float x, float y, float& tx, float& ty) const;
    virtual void inverse_transform(float& tx, float& ty) const;
    virtual void inverse_transform(
	float tx, float ty, float& x, float& y
    ) const;

    float det() const;

    virtual void matrix(
	float& a00, float& a01, float& a10, float& a11, float& a20, float& a21
    ) const;
private:
    boolean identity_;
    float mat00, mat01, mat10, mat11, mat20, mat21;

    void update();

public:
    /*
     * Old definitions for backward compatibility.
     */
    void GetEntries(
	float& a00, float& a01, float& a10, float& a11, float& a20, float& a21
    ) const;
    void Premultiply(Transformer* t);
    void Postmultiply(Transformer* t);
    void Invert();

    void Translate(float dx, float dy);
    void Scale(float sx, float sy);
    void Rotate(float angle);
    boolean Translated(float = 1e-6) const;
    boolean Scaled(float = 1e-6) const;
    boolean Stretched (float = 1e-6) const;
    boolean Rotated(float = 1e-6) const;
    boolean Rotated90(float = 1e-6) const;

    void Transform(IntCoord& x, IntCoord& y) const;
    void Transform(IntCoord x, IntCoord y, IntCoord& tx, IntCoord& ty) const;
    void Transform(float x, float y, float& tx, float& ty) const;
    void TransformList(IntCoord x[], IntCoord y[], int n) const;
    void TransformList(
	IntCoord x[], IntCoord y[], int n, IntCoord tx[], IntCoord ty[]
    ) const;
    void TransformRect(IntCoord&, IntCoord&, IntCoord&, IntCoord&) const;
    void TransformRect(float&, float&, float&, float&) const;
    void InvTransform(IntCoord& tx, IntCoord& ty) const;
    void InvTransform(
	IntCoord tx, IntCoord ty, IntCoord& x, IntCoord& y
    ) const;
    void InvTransform(float tx, float ty, float& x, float& y) const;
    void InvTransformList(IntCoord tx[], IntCoord ty[], int n) const;
    void InvTransformList(
	IntCoord tx[], IntCoord ty[], int n, IntCoord x[], IntCoord y[]
    ) const;
    void InvTransformRect(IntCoord&, IntCoord&, IntCoord&, IntCoord&) const;
    void InvTransformRect(float&, float&, float&, float&) const;
};

inline float Transformer::det() const { return mat00*mat11 - mat01*mat10; }

inline boolean Transformer::identity() const { return identity_; }
inline boolean Transformer::invertible() const { return det() != 0; }

inline boolean Transformer::Translated(float tol) const {
    return -tol > mat20 || mat20 > tol || -tol > mat21 || mat21 > tol;
}

inline boolean Transformer::Scaled(float tol) const {
    float l = 1 - tol, u = 1 + tol;

    return l > mat00 || mat00 > u || l > mat11 || mat11 > u;
}

inline boolean Transformer::Stretched(float tol) const {
    float diff = mat00 - mat11;

    return -tol > diff || diff > tol;
}

inline boolean Transformer::Rotated(float tol) const {
    return -tol > mat01 || mat01 > tol || -tol > mat10 || mat10 > tol;
}

inline boolean Transformer::Rotated90(float tol) const {
    return Rotated(tol) && -tol <= mat00 && mat00 <= tol && 
        -tol <= mat11 && mat11 <= tol;
}

inline void Transformer::GetEntries(
    float& a00, float& a01, float& a10, float& a11, float& a20, float& a21
) const {
    matrix(a00, a01, a10, a11, a20, a21);
}

inline void Transformer::Translate(float dx, float dy) { translate(dx, dy); }
inline void Transformer::Scale(float sx, float sy) { scale(sx, sy); }
inline void Transformer::Rotate(float angle) { rotate(angle); }
inline void Transformer::Premultiply(Transformer* t) { premultiply(*t); }
inline void Transformer::Postmultiply(Transformer* t) { postmultiply(*t); }
inline void Transformer::Invert() { invert(); }

#include <InterViews/_leave.h>

#endif
