/*
 * Copyright (c) 1991 Stanford University
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
 * PolyGlyph - list of glyphs
 */

#include <InterViews/polyglyph.h>
#include <OS/list.h>

declarePtrList(PolyGlyphList,Glyph)
implementPtrList(PolyGlyphList,Glyph)

class PolyGlyphImpl {
private:
    friend class PolyGlyph;

    PolyGlyphImpl(GlyphIndex size);

    PolyGlyphList components_;
};

PolyGlyph::PolyGlyph(GlyphIndex size) {
    impl_ = new PolyGlyphImpl(size);
}

PolyGlyph::~PolyGlyph() {
    PolyGlyphList& p = impl_->components_;
    for (ListItr(PolyGlyphList) i(p); i.more(); i.next()) {
	Resource::unref(i.cur());
    }
    delete impl_;
}

void PolyGlyph::undraw() {
    PolyGlyphList& p = impl_->components_;
    for (ListItr(PolyGlyphList) i(p); i.more(); i.next()) {
	Glyph* g = i.cur();
	if (g != nil) {
	    g->undraw();
	}
    }
}

void PolyGlyph::append(Glyph* g) {
    PolyGlyphList& p = impl_->components_;
    Resource::ref(g);
    GlyphIndex i = p.count();
    p.append(g);
    modified(i);
}

void PolyGlyph::prepend(Glyph* g) {
    PolyGlyphList& p = impl_->components_;
    Resource::ref(g);
    p.prepend(g);
    modified(0);
}

void PolyGlyph::insert(GlyphIndex i, Glyph* g) {
    PolyGlyphList& p = impl_->components_;
    Resource::ref(g);
    p.insert(i, g);
    modified(i);
}

void PolyGlyph::remove(GlyphIndex i) {
    PolyGlyphList& p = impl_->components_;
    Glyph* g = p.item(i);
    if (g != nil) {
	g->undraw();
	Resource::unref_deferred(g);
    }
    p.remove(i);
    modified(i);
}

void PolyGlyph::replace(GlyphIndex i, Glyph* g) {
    PolyGlyphList& p = impl_->components_;
    Glyph* old_g = p.item(i);
    if (g != old_g) {
	Resource::ref(g);
	if (old_g != nil) {
	    old_g->undraw();
	    Resource::unref_deferred(old_g);
	}
	p.remove(i);
	p.insert(i, g);
	modified(i);
    }
}

void PolyGlyph::change(GlyphIndex i) {
    modified(i);
}

GlyphIndex PolyGlyph::count() const {
    return impl_->components_.count();
}

Glyph* PolyGlyph::component(GlyphIndex i) const {
    return impl_->components_.item(i);
}

void PolyGlyph::modified(GlyphIndex) { }

PolyGlyphImpl::PolyGlyphImpl(GlyphIndex size) : components_(size) { }
