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
 * Patch - for repairing glyphs
 */

#ifndef iv_patch_h
#define iv_patch_h

#include <InterViews/monoglyph.h>
#include <InterViews/transformer.h>

class Patch : public MonoGlyph {
public:
    Patch(Glyph*);
    virtual ~Patch();

    Canvas* canvas() const;
    const Transformer& transformer() const;
    const Allocation& allocation() const;
    const Extension& extension() const;

    virtual void redraw() const;
    virtual void reallocate();
    virtual void repick(int depth, Hit&);

    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void undraw();
private:
    Canvas* canvas_;
    Transformer transformer_;
    Allocation allocation_;
    Extension extension_;
};

inline Canvas* Patch::canvas() const { return canvas_; }
inline const Transformer& Patch::transformer() const { return transformer_; }
inline const Allocation& Patch::allocation() const { return allocation_; }
inline const Extension& Patch::extension() const { return extension_; }

#endif
