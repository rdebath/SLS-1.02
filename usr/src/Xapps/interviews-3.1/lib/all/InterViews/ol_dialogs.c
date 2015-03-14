/*
 * Copyright (c) 1992 Stanford University
 * Copyright (c) 1992 Silicon Graphics, Inc.
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
 * OLDialogKit -- object for creating common OpenLook-ish dialog boxes
 */

#include <IV-look/fchooser.h>
#include <IV-look/field.h>
#include <IV-look/ol_dialogs.h>
#include <IV-look/ol_kit.h>
#include <InterViews/canvas.h>
#include <InterViews/color.h>
#include <OS/string.h>

class OL_FieldEditor : public FieldEditor {
public:
    OL_FieldEditor(const String&, WidgetKit*, Style*, FieldEditorAction*);
    virtual ~OL_FieldEditor();
    
    virtual void request(Requisition&) const;
    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void draw(Canvas*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int, Hit&);
private:
    const Color* white_;
    const Color* dark_;
    Allocation field_allocation_;
};

OL_FieldEditor::OL_FieldEditor(
    const String& s, WidgetKit* k, Style* style, FieldEditorAction* a)
    : FieldEditor(s, k, style, a), field_allocation_()
{
    OLKit& kit = *(OLKit*)k;
    white_ = kit.white();
    dark_ = kit.bg3();
    Resource::ref(white_);
    Resource::ref(dark_);
}

OL_FieldEditor::~OL_FieldEditor() {
    Resource::unref(white_); 
    Resource::unref(dark_); 
}

void OL_FieldEditor::request(Requisition& req) const {
    FieldEditor::request(req); 
    const Requirement& r = req.y_requirement();
    Requirement r_y(r.natural() + 2.0, r.stretch(), r.shrink(), r.alignment());
    req.require_y(r_y);
}

void OL_FieldEditor::allocate(Canvas* c, const Allocation& a, Extension& e) {
    const Allotment& ay = a.y_allotment();
    Coord field_span = ay.span() - 2.0;
    Coord field_origin = ay.begin() + ay.alignment() * ay.span() + 2.0;
    Allotment fy(field_origin, field_span, ay.alignment());
    field_allocation_.allot_y(fy);
    field_allocation_.allot_x(a.x_allotment());
    FieldEditor::allocate(c, field_allocation_, e);
    e.set_xy(c, e.left(), e.bottom() - 2.0, e.right(), e.top());
}

void OL_FieldEditor::pick(Canvas* c, const Allocation&, int depth, Hit& h) {
    FieldEditor::pick(c, field_allocation_, depth, h);
}

void OL_FieldEditor::draw(Canvas* c, const Allocation& a) const {
    FieldEditor::draw(c, field_allocation_);
    Coord l = a.left(), b = a.bottom(), r = a.right(), t = a.top();
    c->fill_rect(l, b, l + 1.0, b + 2.0, white_);
    c->fill_rect(l + 1.0 , b + 1.0, r, b + 2.0, white_);
    c->fill_rect(l + 1.0, b, r - 1.0, b + 1.0, dark_);
    c->fill_rect(r - 1.0, b, r, b + 2.0, dark_);
}

OLDialogKit::OLDialogKit() { }
OLDialogKit::~OLDialogKit() { }

FieldEditor* OLDialogKit::make_field_editor(
    const String& sample, WidgetKit* k, Style* s, FieldEditorAction* a
) const {
    return new OL_FieldEditor(sample, k, s, a);
}

FileChooser* OLDialogKit::make_file_chooser(
    const String& dir, WidgetKit* k, Style* s, FileChooserAction* a
) const {
    return new FileChooser(dir, k, s, a);
}
