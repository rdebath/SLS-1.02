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
 * X11 font implementation
 */

#include <InterViews/display.h>
#include <InterViews/font.h>
#include <InterViews/session.h>
#include <IV-X11/Xlib.h>
#include <IV-X11/xdisplay.h>
#include <IV-X11/xfont.h>
#include <OS/list.h>
#include <OS/math.h>
#include <OS/string.h>
#include <OS/ustring.h>
#include <OS/table.h>
#include <X11/Xatom.h>
#include <stdio.h>
#include <string.h>

/** class FontImpl **/

declarePtrList(FontList,Font)
implementPtrList(FontList,Font)

declarePtrList(FontRepList,FontRep)
implementPtrList(FontRepList,FontRep)

inline unsigned long key_to_hash(UniqueString& s) { return s.hash(); }

struct KnownFonts {
    FontList fonts;
    FontRepList fontreps;
};

declareTable(NameToKnownFonts,UniqueString,KnownFonts*)
implementTable(NameToKnownFonts,UniqueString,KnownFonts*)

class FontImpl {
private:
    friend class Font;
    friend class FontRep;

    FontImpl(const String&, float);
    ~FontImpl();

    void remove(const Font*);

    static NameToKnownFonts* fonts();

    FontRep* rep(Display*);
    FontRep* default_rep();
    void new_rep(KnownFonts*, FontRep*);
    void attach(FontRep*);

    static const Font* lookup(Display*, const String&, float);
    static FontRep* find_rep(FontRepList&, Display*, float);
    static FontRep* create(Display*, const String&, float);
    static const Font* new_font(const String&, float, KnownFonts*, FontRep*);
    static KnownFonts* known(KnownFonts*, const UniqueString&);

    UniqueString* name_;
    float scale_;
    FontRepList replist_;
    KnownFonts* entry_;

    static NameToKnownFonts* fonts_;
};

NameToKnownFonts* FontImpl::fonts_;

FontImpl::FontImpl(const String& s, float scale) {
    name_ = new UniqueString(s);
    scale_ = scale;
    entry_ = nil;
}

FontImpl::~FontImpl() {
    for (ListItr(FontRepList) i(replist_); i.more(); i.next()) {
	Resource::unref(i.cur());
    }
    delete name_;
}

void FontImpl::remove(const Font* f) {
    if (entry_ != nil) {
	for (ListUpdater(FontList) i(entry_->fonts); i.more(); i.next()) {
	    if (i.cur() == f) {
		i.remove_cur();
		break;
	    }
	}
	if (entry_->fonts.count() == 0 && entry_->fontreps.count() == 0) {
	    fonts_->remove(*name_);
	    delete entry_;
	}
    }
    entry_ = nil;
}

NameToKnownFonts* FontImpl::fonts() {
    if (fonts_ == nil) {
	fonts_ = new NameToKnownFonts(256);
    }
    return fonts_;
}

/*
 * Find a fontrep for the given display.  First check the list
 * associated with this font.  Then lookup the font name and see
 * if there is an associated fontrep (attached to another
 * font object).
 */

FontRep* FontImpl::rep(Display* d) {
    FontRep* r;
    for (ListItr(FontRepList) i(replist_); i.more(); i.next()) {
	r = i.cur();
	if (r->display_ == d) {
	    return r;
	}
    }

    KnownFonts* k = nil;
    if (fonts()->find(k, *name_)) {
	r = find_rep(k->fontreps, d, scale_);
	if (r != nil) {
	    attach(r);
	    return r;
	}
    }

    r = create(d, *name_, scale_);
    if (r != nil) {
	new_rep(known(k, *name_), r);
    }
    return r;
}

FontRep* FontImpl::default_rep() {
    long n = replist_.count();
    if (n != 0) {
	return replist_.item(n - 1);
    }
    return rep(Session::instance()->default_display());
}

void FontImpl::new_rep(KnownFonts* k, FontRep* r) {
    r->entry_ = k;
    k->fontreps.append(r);
    attach(r);
}

void FontImpl::attach(FontRep* r) {
    replist_.append(r);
    Resource::ref(r);
}

const Font* FontImpl::lookup(Display* d, const String& name, float scale) {
    const Font* f;
    FontRep* r;
    KnownFonts* k = nil;
    UniqueString uname(name);
    if (fonts()->find(k, uname)) {
	for (ListItr(FontList) i(k->fonts); i.more(); i.next()) {
	    f = i.cur();
	    if (Math::equal(f->impl_->scale_, scale, float(0.0001))) {
		return f;
	    }
	}

	r = find_rep(k->fontreps, d, scale);
	if (r != nil) {
	    return new_font(uname, scale, k, r);
	}
    }

    r = create(d, uname, scale);
    if (r == nil) {
	return nil;
    }
    k = known(k, uname);
    f = new_font(uname, scale, k, r);
    f->impl_->new_rep(k, r);
    f->impl_->entry_ = k;
    return f;
}

FontRep* FontImpl::find_rep(FontRepList& list, Display* d, float s) {
    for (ListItr(FontRepList) i(list); i.more(); i.next()) {
	FontRep* r = i.cur();
	if (r->display_ == d && Math::equal(r->scale_, s, float(0.0001))) {
	    return r;
	}
    }
    return nil;
}

FontRep* FontImpl::create(Display* d, const String& name, float scale) {
    static Atom XA_CHARSET_REGISTRY = 0;

    XDisplay* dpy = d->rep()->display_;
    NullTerminatedString s(name);
    XFontStruct* xf = XLoadQueryFont(dpy, s.string());
    if (xf == nil) {
	return nil;
    }

    FontRep* f = new FontRep(d, xf, scale);
    unsigned long value;
    if (XGetFontProperty(xf, XA_FULL_NAME, &value) ||
	XGetFontProperty(xf, XA_FAMILY_NAME, &value)
    ) {
	char* fullname = XGetAtomName(dpy, (Atom)value);
	f->name_ = new CopyString(fullname);
	XFree(fullname);
    } else {
	f->name_ = new CopyString(s.string());
    }

    if (XA_CHARSET_REGISTRY == 0) {
	XA_CHARSET_REGISTRY = XInternAtom(dpy, "CHARSET_REGISTRY", False);
    }
    if (XGetFontProperty(xf, XA_CHARSET_REGISTRY, &value)) {
	char* registry = XGetAtomName(dpy, (Atom)value);
	f->encoding_ = new CopyString(registry);
	XFree(registry);
    } else {
	f->encoding_ = nil;
    }

    if (XGetFontProperty(xf, XA_POINT_SIZE, &value)) {
	f->size_ = float(value / 10) * f->scale_;
    } else {
	f->size_ = 0;
    }

    return f;
}

const Font* FontImpl::new_font(
    const String& name, float scale, KnownFonts* k, FontRep* r
) {
    Font* f = new Font(name, scale);
    f->impl_->attach(r);
    k->fonts.append(f);
    return f;
}

KnownFonts* FontImpl::known(KnownFonts* old_k, const UniqueString& name) {
    if (old_k != nil) {
	return old_k;
    }
    KnownFonts* k = new KnownFonts;
    fonts_->insert(name, k);
    return k;
}

/** class FontRep **/

FontRep::FontRep(Display* d, XFontStruct* xf, float scale) {
    display_ = d;
    font_ = xf;
    scale_ = scale;
    unscaled_ = (scale_ > 0.9999 && scale_ < 1.0001);
    entry_ = nil;
}

FontRep::~FontRep() {
    XFreeFont(display_->rep()->display_, font_);
    for (ListUpdater(FontRepList) i(entry_->fontreps); i.more(); i.next()) {
	if (i.cur() == this) {
	    i.remove_cur();
	    break;
	}
    }
    delete name_;
    delete encoding_;
}

/** class Font **/

Font::Font(const String& name, float scale) {
    impl_ = new FontImpl(name, scale);
}

Font::Font(const char* name, float scale) {
    impl_ = new FontImpl(String(name), scale);
}

Font::Font(FontImpl* i) {
    impl_ = i;
}

Font::~Font() {
    delete impl_;
}

void Font::cleanup() {
    impl_->remove(this);
}

const Font* Font::lookup(const String& name) {
    return FontImpl::lookup(
	Session::instance()->default_display(), name, 1.0
    );
}

const Font* Font::lookup(const char* name) {
    return FontImpl::lookup(
	Session::instance()->default_display(), String(name), 1.0
    );
}

boolean Font::exists(Display* d, const String& name) {
    return FontImpl::lookup(d, name, 1.0) != nil;
}

boolean Font::exists(Display* d, const char* name) {
    return FontImpl::lookup(d, String(name), 1.0) != nil;
}

FontRep* Font::rep(Display* d) const {
    return impl_->rep(d);
}

const char* Font::name() const {
    FontRep* f = impl_->default_rep();
    return f->name_->string();
}

const char* Font::encoding() const {
    FontRep* f = impl_->default_rep();
    return f->encoding_ == nil ? nil : f->encoding_->string();
}

Coord Font::size() const {
    return impl_->default_rep()->size_;
}

void Font::font_bbox(FontBoundingBox& b) const {
    FontRep* f = impl_->default_rep();
    float scale = f->scale_;
    XFontStruct* xf = f->font_;
    Display* d = f->display_;
    b.left_bearing_ = scale * d->to_coord(xf->max_bounds.lbearing);
    b.right_bearing_ = scale * d->to_coord(xf->max_bounds.rbearing);
    b.width_ = scale * d->to_coord(xf->max_bounds.width);
    b.ascent_ = scale * d->to_coord(xf->ascent);
    b.descent_ = scale * d->to_coord(xf->descent);
    b.font_ascent_ = b.ascent_;
    b.font_descent_ = b.descent_;
}

void Font::char_bbox(long c, FontBoundingBox& b) const {
    if (c < 0) {
	b.left_bearing_ = 0;
	b.right_bearing_ = 0;
	b.width_ = 0;
	b.ascent_ = 0;
	b.descent_ = 0;
	b.font_ascent_ = 0;
	b.font_descent_ = 0;
	return;
    }
    FontRep* f = impl_->default_rep();
    float scale = f->scale_;
    XFontStruct* xf = f->font_;
    Display* d = f->display_;
    XCharStruct xc;
    XChar2b xc2b;
    xc2b.byte1 = (unsigned char)((c & 0xff00) >> 8);
    xc2b.byte2 = (unsigned char)(c & 0xff);
    int dir, asc, des;
    XTextExtents16(xf, &xc2b, 1, &dir, &asc, &des, &xc);
    b.left_bearing_ = scale * d->to_coord(-xc.lbearing);
    b.right_bearing_ = scale * d->to_coord(xc.rbearing);
    b.width_ = width(c);
    b.ascent_ = scale * d->to_coord(xc.ascent);
    b.descent_ = scale * d->to_coord(xc.descent);
    b.font_ascent_ = scale * d->to_coord(xf->ascent);
    b.font_descent_ = scale * d->to_coord(xf->descent);
}

void Font::string_bbox(const char* s, int len, FontBoundingBox& b) const {
    FontRep* f = impl_->default_rep();
    float scale = f->scale_;
    XFontStruct* xf = f->font_;
    Display* d = f->display_;
    XCharStruct c;
    int dir, asc, des;
    XTextExtents(xf, s, len, &dir, &asc, &des, &c);
    b.left_bearing_ = scale * d->to_coord(-c.lbearing);
    b.right_bearing_ = scale * d->to_coord(c.rbearing);
    b.width_ = width(s, len);
    b.ascent_ = scale * d->to_coord(c.ascent);
    b.descent_ = scale * d->to_coord(c.descent);
    b.font_ascent_ = scale * d->to_coord(xf->ascent);
    b.font_descent_ = scale * d->to_coord(xf->descent);
}

Coord Font::width(long c) const {
    if (c < 0) {
	return 0;
    }
    FontRep* f = impl_->default_rep();
    XChar2b xc2b;
    xc2b.byte1 = (unsigned char)((c & 0xff00) >> 8);
    xc2b.byte2 = (unsigned char)(c & 0xff);
    return f->scale_ * f->display_->to_coord(XTextWidth16(f->font_, &xc2b, 1));
}

Coord Font::width(const char* s, int len) const {
    FontRep* f = impl_->default_rep();
    return f->scale_ * f->display_->to_coord(XTextWidth(f->font_, s, len));
}

int Font::index(const char* s, int len, float offset, boolean between) const {
    const char* p;
    int n, w;
    int coff, cw;

    if (offset < 0 || *s == '\0' || len == 0) {
        return 0;
    }
    FontRep* f = impl_->default_rep();
    XFontStruct* xf = f->font_;
    int xoffset = f->display_->to_pixels(Coord(offset * f->scale_));
    if (xf->min_bounds.width == xf->max_bounds.width) {
        cw = xf->min_bounds.width;
        n = xoffset / cw;
        coff = xoffset % cw;
    } else {
        w = 0;
        for (p = s, n = 0; *p != '\0' && n < len; ++p, ++n) {
            cw = XTextWidth(xf, p, 1);
            w += cw;
            if (w > xoffset) {
                break;
            }
        }
        coff = xoffset - w + cw;
    }
    if (between && coff > cw/2) {
        ++n;
    }
    return Math::min(n, len);
}

/* anachronisms */

int Font::Baseline() const {
    FontBoundingBox b;
    font_bbox(b);
    return impl_->default_rep()->display_->to_pixels(b.descent()) - 1;
}

int Font::Height() const {
    FontBoundingBox b;
    font_bbox(b);
    return impl_->default_rep()->display_->to_pixels(b.ascent() + b.descent());
}

int Font::Width(const char* s) const {
    return impl_->default_rep()->display_->to_pixels(width(s, strlen(s)));
}

int Font::Width(const char* s, int len) const {
    return impl_->default_rep()->display_->to_pixels(width(s, len));
}

int Font::Index(const char* s, int offset, boolean between) const {
    return impl_->default_rep()->display_->to_pixels(
	index(s, strlen(s), float(offset), between)
    );
}

int Font::Index(const char* s, int len, int offset, boolean between) const {
    return impl_->default_rep()->display_->to_pixels(
	index(s, len, float(offset), between)
    );
}

boolean Font::FixedWidth() const {
    FontRep* f = impl_->default_rep();
    XFontStruct* xf = f->font_;
    return xf->min_bounds.width == xf->max_bounds.width;
}

/** class FontFamily **/

declarePtrList(FontFamilyRepList,FontFamilyRep)
implementPtrList(FontFamilyRepList,FontFamilyRep)

class FontFamilyImpl {
private:
    friend class FontFamily;

    char* name;
    FontFamilyRepList replist;
};

static boolean contains(const char* string, const char* substring) {
    int sublength = strlen(substring);
    int length = strlen(string) - sublength;
    for (int i = 0; i <= length; ++i) {
        for (int j = 0; j < sublength; ++j) {
            if (string[i+j] != substring[j]) {
                break;
            }
        }
        if (j == sublength) {
            return true;
        }
    }
    return false;
}

class FontNameSet {
public:
    int value;
    char* names[6];
};

static FontNameSet weight_names [] = {
    { 1, { "ultralight" } },
    { 2, { "extralight" } },
    { 3, { "light" } },
    { 4, { "semilight", "book" } },
    { 5, { "medium", "normal", "regular" } },
    { 6, { "semibold", "demibold" } },
    { 7, { "bold" } },
    { 8, { "extrabold", "heavy" } },
    { 9, { "ultrabold", "black" } },
    { 0 }
};

static FontNameSet width_names [] = {
    { 1, { "ultracondensed" } },
    { 2, { "extracondensed" } },
    { 3, { "condensed" } },
    { 4, { "semicondensed" } },
    { 5, { "medium", "normal", "regular" } },
    { 6, { "semiexpanded", "demiexpanded" } },
    { 7, { "expanded" } },
    { 8, { "extraexpanded", "wide" } },
    { 9, { "ultraexpanded", "extrawide" } },
    { 0 }
};

static FontNameSet slant_names [] = {
    { 1, { "reverseitalic", "reverseoblique" } },
    { 2, { "roman", "normal", "regular" } },
    { 3, { "italic", "oblique" } },
    { 0 }
};

static int name_value(const char* name, FontNameSet* values, int def) {
    int i = 0;
    while (values[i].value != 0) {
        int j = 0;
        while (values[i].names[j] != nil) {
            if (contains(name, values[i].names[j])) {
                return values[i].value;
            }
            ++j;
        }
        ++i;
    }
    return def;
}

FontFamily::FontFamily(const char* familyname) {
    impl_ = new FontFamilyImpl;
    impl_->name = new char[strlen(familyname) + 1];
    strcpy(impl_->name, familyname);
}

FontFamily::~FontFamily() {
    FontFamilyRepList& list = impl_->replist;
    for (long i = 0; i < list.count(); i++) {
	destroy(list.item(i));
    }
    delete impl_->name;
    delete impl_;
}

FontFamilyRep* FontFamily::rep(Display* d) const {
    FontFamilyRep* f;
    FontFamilyRepList& list = impl_->replist;
    for (long i = 0; i < list.count(); i++) {
	FontFamilyRep* f = list.item(i);
	if (f->display_ == d) {
	    return f;
	}
    }
    f = create(d);
    list.append(f);
    return f;
}

FontFamilyRep* FontFamily::create(Display* d) const {
    register FontFamilyRep* r = new FontFamilyRep;
    char buffer[256];
    sprintf(buffer, "*-*-%s-*-*-*-*-75-75-*-*-*-*", impl_->name);
    char** fonts = XListFonts(
	d->rep()->display_, buffer, 100, &r->count_
    );
    r->display_ = d;
    r->names_ = new char*[r->count_];
    r->weights_ = new int[r->count_];
    r->slants_ = new int[r->count_];
    r->widths_ = new int[r->count_];
    r->sizes_ = new int[r->count_];
    r->min_weight_ = 1000;
    r->max_weight_ = 0;
    r->min_slant_ = 1000;
    r->max_slant_ = 0;
    r->min_width_ = 1000;
    r->max_width_ = 0;
    r->min_size_ = 1000;
    r->max_size_ = 0;
    for (unsigned int i = 0; i < r->count_; ++i) {
        r->names_[i] = new char[strlen(fonts[i]) + 1];
        strcpy(r->names_[i], fonts[i]);

        char weight[100];
        char slant[100];
        char width[100];
        int size;
        sscanf(
            r->names_[i],
            "-%*[^-]-%*[^-]-%[^-]-%[^-]-%[^-]--%*[^-]-%d",
            weight, slant, width, &size
        );
        r->weights_[i] = name_value(weight, weight_names, 5);

	String sl(slant);
        if (sl == "o" || sl == "i") {
            r->slants_[i] = 3;
        } else if (sl == "r") {
            r->slants_[i] = 2;
        } else if (sl == "ro" || sl == "ri") {
            r->slants_[i] = 1;
        } else {
            r->slants_[i] = 2;
        }            
        r->widths_[i] = name_value(width, width_names, 5);
        r->sizes_[i] = size/10;

        r->min_width_ = Math::min(r->widths_[i], r->min_width_);
        r->max_width_ = Math::max(r->widths_[i], r->max_width_);
        r->min_weight_ = Math::min(r->weights_[i], r->min_weight_);
        r->max_weight_ = Math::max(r->weights_[i], r->max_weight_);
        r->min_slant_ = Math::min(r->slants_[i], r->min_slant_);
        r->max_slant_ = Math::max(r->slants_[i], r->max_slant_);
        r->min_size_ = Math::min(r->sizes_[i], r->min_size_);
        r->max_size_ = Math::max(r->sizes_[i], r->max_size_);
    }
    XFreeFontNames(fonts);
    return r;
}

void FontFamily::destroy(FontFamilyRep* r) {
    for (int i = 0; i < r->count_; ++i) {
        delete r->names_[i];
    }
    delete r->names_;
    delete r->weights_;
    delete r->slants_;
    delete r->widths_;
    delete r->sizes_;
}

boolean FontFamily::font(int size, const char*& name, float& scale) const {
    return font(size, "", name, scale);
}

boolean FontFamily::font(
    int size, const char* style, const char*& name, float& scale
) const {
    int weight = name_value(style, weight_names, 5);
    int slant = name_value(style, slant_names, 2);
    int width = name_value(style, width_names, 5);

    int best_match = -1;
    int least_badness = 1000;

    FontFamilyRep* r = rep(Session::instance()->default_display());
    for (int i = 0; i < r->count_; ++i) {
        int badness = 0;
        badness += Math::abs(r->weights_[i] - weight);
        badness += Math::abs(r->widths_[i] - width);
        badness += Math::abs(r->slants_[i] - slant);
        badness += Math::abs(r->sizes_[i] - size);
        if (badness < least_badness) {
            least_badness = badness;
            best_match = i;
        }
    }
    if (best_match == -1) {
	return false;
    }
    int best_size = r->sizes_[best_match];
    name = r->names_[best_match];
    scale = (size == best_size) ? 1.0 : float(size)/float(best_size);
    return true;
}
