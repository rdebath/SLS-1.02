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
 * Implementation of StateVar subclasses.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/globals.h>
#include <Unidraw/statevars.h>
#include <Unidraw/stateview.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/Components/component.h>
#include <Unidraw/Graphic/pspaint.h>

#include <stream.h>
#include <string.h>

/*****************************************************************************/

UList* ModifStatusVar::_vars;

NameVar::NameVar (const char* n) { _name = nil; SetName(n); }

void NameVar::SetName (const char* n) {
    if (n == nil) {
	delete _name;
	_name = nil;
	Notify();

    } else if (_name == nil || (_name != nil && (strcmp(n, _name) != 0))) {
        delete _name;
        _name = strnew(n);
	Notify();
    }
}

const char* NameVar::GetName () { return _name; }

StateVar& NameVar::operator = (StateVar& var) {
    if (var.IsA(NAME_VAR)) {
        NameVar* nameVar = (NameVar*) &var;
        SetName(nameVar->GetName());
    }
    return *this;
}

StateVar* NameVar::Copy () { return new NameVar(GetName()); }
ClassId NameVar::GetClassId () { return NAME_VAR; }
boolean NameVar::IsA (ClassId id) {return NAME_VAR == id || StateVar::IsA(id);}

void NameVar::Read (istream& in) {
    StateVar::Read(in);
    _name = unidraw->GetCatalog()->ReadString(in);
}

void NameVar::Write (ostream& out) {
    StateVar::Write(out); 
    unidraw->GetCatalog()->WriteString(GetName(), out);
}

/*****************************************************************************/

CompNameVar::CompNameVar (Component* c) : NameVar(nil) {
    _comp = nil;
    SetComponent(c);
}

void CompNameVar::UpdateName () {
    if (_comp == nil) {
        SetName(nil);

    } else {
        const char* tmp = unidraw->GetCatalog()->GetName(_comp);
        const char* name = (tmp == nil) ? PartOf() : tmp;
        SetName(name);
    }
}

void CompNameVar::SetComponent (Component* c) {
    if (c != _comp) {
        _comp = c;
        UpdateName();
        Notify();
    }
}            

Component* CompNameVar::GetComponent () { return _comp; }

const char* CompNameVar::PartOf () {
    Component* curComp = _comp->GetParent();
    Catalog* catalog = unidraw->GetCatalog();

    while (curComp != nil && catalog->GetName(curComp) == nil) {
        curComp = curComp->GetParent();
    }

    return (curComp == nil) ? nil : catalog->GetName(curComp);
}

StateVar& CompNameVar::operator = (StateVar& var) {
    if (var.IsA(COMPNAME_VAR)) {
        CompNameVar* compNameVar = (CompNameVar*) &var;
        SetComponent(compNameVar->GetComponent());
    }
    return *this;
}

StateVar* CompNameVar::Copy () { return new CompNameVar(GetComponent()); }
ClassId CompNameVar::GetClassId () { return COMPNAME_VAR; }

boolean CompNameVar::IsA (ClassId id) {
    return COMPNAME_VAR == id || NameVar::IsA(id);
}

void CompNameVar::Read (istream& in) {
    NameVar::Read(in);
    _comp = unidraw->GetCatalog()->ReadComponent(in);
}

void CompNameVar::Write (ostream& out) {
    NameVar::Write(out); 
    unidraw->GetCatalog()->WriteComponent(GetComponent(), out);
}

/*****************************************************************************/

inline ModifStatusVar* var (UList* u) { return (ModifStatusVar*) (*u)(); }

ModifStatusVar::ModifStatusVar (Component* c, boolean m) {
    _modified = m;

    if (_vars == nil) {
	_vars = new UList;
    }
    SetComponent(c);
    _vars->Append(new UList(this));
}

ModifStatusVar::~ModifStatusVar () {
    _vars->Delete(this);
}

void ModifStatusVar::SetModifStatus (boolean status) {
    if (status != _modified) {
	for (UList* v = _vars->First(); v != _vars->End(); v = v->Next()) {
	    if (var(v)->GetComponent() == _component) {
		var(v)->modified(status);
	    }
	}
    }
}

void ModifStatusVar::SetComponent (Component* c) {
    _component = (c == nil) ? nil : c->GetRoot();

    for (UList* v = _vars->First(); v != _vars->End(); v = v->Next()) {
        ModifStatusVar* msv = var(v);

        if (msv != this && msv->GetComponent() == _component) {
            modified(msv->GetModifStatus());
            return;
        }
    }
    modified(false);
}

boolean ModifStatusVar::GetModifStatus () { return _modified; }
Component* ModifStatusVar::GetComponent () { return _component; }

void ModifStatusVar::modified (int m) {
    _modified = m;
    Notify();
}

StateVar& ModifStatusVar::operator = (StateVar& var) {
    if (var.IsA(MODIFSTATUS_VAR)) {
        ModifStatusVar* msVar = (ModifStatusVar*) &var;
        SetModifStatus(msVar->GetModifStatus());
    }
    return *this;
}

StateVar* ModifStatusVar::Copy () { 
    return new ModifStatusVar(GetComponent(),GetModifStatus());
}

ClassId ModifStatusVar::GetClassId () { return MODIFSTATUS_VAR; }

boolean ModifStatusVar::IsA (ClassId id) {
    return MODIFSTATUS_VAR == id || StateVar::IsA(id);
}

void ModifStatusVar::Read (istream& in) {
    StateVar::Read(in);
    in >> _modified;
}

void ModifStatusVar::Write (ostream& out) {
    StateVar::Write(out); 
    out << _modified << " ";
}

/*****************************************************************************/

MagnifVar::MagnifVar (float m) { _magnif = m; }

void MagnifVar::SetMagnif (float m) {
    if (m != _magnif) {
	_magnif = m;
	Notify();
    }
}

float MagnifVar::GetMagnif () { return _magnif; }

StateVar& MagnifVar::operator = (StateVar& var) {
    if (var.IsA(MAGNIF_VAR)) {
        MagnifVar* magVar = (MagnifVar*) &var;
        SetMagnif(magVar->GetMagnif());
    }
    return *this;
}

StateVar* MagnifVar::Copy () { return new MagnifVar(GetMagnif()); }
ClassId MagnifVar::GetClassId () { return MAGNIF_VAR; }

boolean MagnifVar::IsA (ClassId id) {
    return MAGNIF_VAR == id || StateVar::IsA(id);
}

void MagnifVar::Read (istream& in) {
    StateVar::Read(in);
    in >> _magnif;
}

void MagnifVar::Write (ostream& out) {
    StateVar::Write(out); 
    out << _magnif << " ";
}

/*****************************************************************************/

GravityVar::GravityVar (boolean active) { _active = active; }

boolean GravityVar::IsActive () { return _active; }

void GravityVar::Activate (boolean active) {
    if (active != _active) {
	_active = active;
	Notify();
    }
}

StateVar& GravityVar::operator = (StateVar& var) {
    if (var.IsA(GRAVITY_VAR)) {
        GravityVar* gravityVar = (GravityVar*) &var;
        Activate(gravityVar->IsActive());
    }
    return *this;
}

StateVar* GravityVar::Copy () { return new GravityVar(IsActive()); }
ClassId GravityVar::GetClassId () { return GRAVITY_VAR; }

boolean GravityVar::IsA (ClassId id) {
    return GRAVITY_VAR == id || StateVar::IsA(id);
}

void GravityVar::Read (istream& in) {
    StateVar::Read(in);
    in >> _active;
}

void GravityVar::Write (ostream& out) {
    StateVar::Write(out); 
    out << _active << " ";
}

/*****************************************************************************/

FontVar::FontVar (PSFont* f) { _psfont = f; Ref(_psfont); }
FontVar::~FontVar () { Unref(_psfont); }

void FontVar::SetFont (PSFont* f) {
    if (f != _psfont) {
        Unref(_psfont);
	_psfont = f;
        Ref(_psfont);
	Notify();
    }
}

PSFont* FontVar::GetFont () { return _psfont; }

StateVar& FontVar::operator = (StateVar& var) {
    if (var.IsA(FONT_VAR)) {
        FontVar* fontVar = (FontVar*) &var;
        SetFont(fontVar->GetFont());
    }
    return *this;
}

StateVar* FontVar::Copy () { return new FontVar(GetFont()); }
ClassId FontVar::GetClassId () { return FONT_VAR; }

boolean FontVar::IsA (ClassId id) {
    return FONT_VAR == id || StateVar::IsA(id);
}

void FontVar::Read (istream& in) {
    StateVar::Read(in);
    _psfont = unidraw->GetCatalog()->ReadFont(in);
}

void FontVar::Write (ostream& out) {
    StateVar::Write(out); 
    unidraw->GetCatalog()->WriteFont(_psfont, out);
}

/*****************************************************************************/

BrushVar::BrushVar (PSBrush* b) { _psbrush = b; Ref(_psbrush); }
BrushVar::~BrushVar () { Unref(_psbrush); }

void BrushVar::SetBrush (PSBrush* b) {
    if (b != _psbrush) {
        Unref(_psbrush);
	_psbrush = b;
        Ref(_psbrush);
	Notify();
    }
}

PSBrush* BrushVar::GetBrush () { return _psbrush; }

StateVar& BrushVar::operator = (StateVar& var) {
    if (var.IsA(BRUSH_VAR)) {
        BrushVar* brVar = (BrushVar*) &var;
        SetBrush(brVar->GetBrush());
    }
    return *this;
}

StateVar* BrushVar::Copy () { return new BrushVar(GetBrush()); }
ClassId BrushVar::GetClassId () { return BRUSH_VAR; }

boolean BrushVar::IsA (ClassId id) {
    return BRUSH_VAR == id || StateVar::IsA(id);
}

void BrushVar::Read (istream& in) {
    StateVar::Read(in);
    _psbrush = unidraw->GetCatalog()->ReadBrush(in);
}

void BrushVar::Write (ostream& out) {
    StateVar::Write(out); 
    unidraw->GetCatalog()->WriteBrush(_psbrush, out);
}

/*****************************************************************************/

PatternVar::PatternVar (PSPattern* p) { _pspattern = p; Ref(_pspattern); }
PatternVar::~PatternVar () { Unref(_pspattern); }

void PatternVar::SetPattern (PSPattern* p) {
    if (p != _pspattern) {
        Unref(_pspattern);
	_pspattern = p;
        Ref(_pspattern);
	Notify();
    }
}

PSPattern* PatternVar::GetPattern () { return _pspattern; }

StateVar& PatternVar::operator = (StateVar& var) {
    if (var.IsA(BRUSH_VAR)) {
        PatternVar* patVar = (PatternVar*) &var;
        SetPattern(patVar->GetPattern());
    }
    return *this;
}

StateVar* PatternVar::Copy () { return new PatternVar(GetPattern()); }
ClassId PatternVar::GetClassId () { return PATTERN_VAR; }

boolean PatternVar::IsA (ClassId id) {
    return PATTERN_VAR == id || StateVar::IsA(id);
}

void PatternVar::Read (istream& in) {
    StateVar::Read(in);
    _pspattern = unidraw->GetCatalog()->ReadPattern(in);
}

void PatternVar::Write (ostream& out) {
    StateVar::Write(out); 
    unidraw->GetCatalog()->WritePattern(_pspattern, out);
}

/*****************************************************************************/

ColorVar::ColorVar (PSColor* fg, PSColor* bg) {
    _psfgcolor = fg;
    _psbgcolor = bg;
    Ref(_psfgcolor);
    Ref(_psbgcolor);
}

ColorVar::~ColorVar () {
    Unref(_psfgcolor);
    Unref(_psbgcolor);
}

void ColorVar::SetColors (PSColor* fg, PSColor* bg) {
    if (fg != _psfgcolor || bg != _psbgcolor) {
        Unref(_psfgcolor);
        Unref(_psbgcolor);
        _psfgcolor = fg;
	_psbgcolor = bg;
        Ref(_psfgcolor);
        Ref(_psbgcolor);
        Notify();
    }
}

PSColor* ColorVar::GetFgColor() { return _psfgcolor; }
PSColor* ColorVar::GetBgColor() { return _psbgcolor; }

StateVar& ColorVar::operator = (StateVar& var) {
    if (var.IsA(COLOR_VAR)) {
        ColorVar* colorVar = (ColorVar*) &var;
        SetColors(colorVar->GetFgColor(),colorVar->GetBgColor());
    }
    return *this;
}

StateVar* ColorVar::Copy () { return new ColorVar(_psfgcolor, _psbgcolor); }
ClassId ColorVar::GetClassId () { return COLOR_VAR; }
boolean ColorVar::IsA (ClassId id) {return COLOR_VAR==id || StateVar::IsA(id);}

void ColorVar::Read (istream& in) {
    StateVar::Read(in);
    Catalog* catalog = unidraw->GetCatalog();

    _psfgcolor = catalog->ReadColor(in);
    _psbgcolor = catalog->ReadColor(in);
}

void ColorVar::Write (ostream& out) {
    StateVar::Write(out);
    Catalog* catalog = unidraw->GetCatalog();

    catalog->WriteColor(_psfgcolor, out);
    catalog->WriteColor(_psbgcolor, out);
}
