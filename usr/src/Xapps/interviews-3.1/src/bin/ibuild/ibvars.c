/*
 * Copyright (c) 1991 Stanford University
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
 * Implementation of user interface builder-specific state variables.
 */

#include "ibclasses.h"
#include "ibcreator.h"
#include "ibglobals.h"
#include "ibinteractor.h"
#include "ibvars.h"

#include <Unidraw/catalog.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/ulist.h>

#include <InterViews/button.h>
#include <InterViews/shape.h>

#include <OS/string.h>

#include <stdio.h>
#include <stdlib.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/

CanvasVar::CanvasVar (int w, int h) { _width = w; _height = h; }

void CanvasVar::SetSize (int w, int h) {
    if (w != _width || h != _height) {
        _width = w;
        _height = h;
        Notify();
    }
}

StateVar& CanvasVar::operator = (StateVar& var) {
    StateVar::operator=(var);
    if (var.IsA(CANVAS_VAR)) {
        CanvasVar* cvar = (CanvasVar*) &var;
        SetSize(cvar->Width(), cvar->Height());
    }
    return *this;
}

boolean CanvasVar::operator != (StateVar& var) {
    boolean nequal = true;
    if (var.IsA(CANVAS_VAR)) {
        CanvasVar* cvar = (CanvasVar*) &var;
        if (Width() == cvar->Width() && Height() == cvar->Height()) {
            nequal = false;
        }
    }
    return nequal;
}

StateVar* CanvasVar::Copy () { return new CanvasVar(_width, _height); }
ClassId CanvasVar::GetClassId () { return CANVAS_VAR; }

boolean CanvasVar::IsA (ClassId id) {
    return CANVAS_VAR == id || StateVar::IsA(id);
}

void CanvasVar::Read (istream& in) {
    StateVar::Read(in);
    in >> _width >> _height;
}

void CanvasVar::Write (ostream& out) {
    StateVar::Write(out);
    out << _width << " " << _height << " ";
}

/*****************************************************************************/

boolean IBNameVar::_unique;
int IBNameVar::_IBSerial;

/*****************************************************************************/
IBNameVar::IBNameVar (const char* name, boolean machgen ) : NameVar(name) {
    _machgen = machgen;
}

StateVar* IBNameVar::Copy () {
    IBNameVar* copy = new IBNameVar(GetName(), _machgen); 
    return copy;
}

ClassId IBNameVar::GetClassId () { return IBNAME_VAR; }

boolean IBNameVar::IsA (ClassId id) {
    return IBNAME_VAR == id || NameVar::IsA(id);
}

StateVar& IBNameVar::operator = (StateVar& var) {
    NameVar::operator=(var); 
    if (var.IsA(IBNAME_VAR)) {
        IBNameVar* name = (IBNameVar*) &var;
	_machgen = name->GetMachGen();
    }
    return *this;
}

void IBNameVar::GenNewName () {
    const char* name = GetName();

    if (name != nil && GetMachGen()) {
        if (_unique) {
            char Name[CHARBUFSIZE], tmp[CHARBUFSIZE];
            strcpy(tmp, name);

            if (*name == '_') {
                strcpy(tmp, &name[1]);
            }

            char* trail = strrchr(tmp, '_');

            if (trail != NULL) {
                *trail = '\0';
            }

            sprintf(Name, "_%s_%i\0", tmp, GetSerial()++);
            SetName(Name);

        } else {
            char* trail = strrchr(name, '_');

            if (trail != NULL) {
		String num_str(&trail[1]);
		int sequence;

		num_str.convert(sequence);
                int& serial = GetSerial();
                serial = max(++sequence, serial);
            }
        }
    }
}

void IBNameVar::Read (istream& in) {
    NameVar::Read(in);
    in >> _machgen;
    GenNewName();
}

void IBNameVar::Write (ostream& out) {
    NameVar::Write(out);
    out << " " << _machgen << " "; 
}

/*****************************************************************************/
class IDMapElem : public UMapElem {
public:
    IDMapElem(IDVar*, SubclassNameVar*);
    virtual void* id();
    virtual void* tag();
private:
    IDVar* _id;
    SubclassNameVar* _tag;
};

IDMapElem::IDMapElem(IDVar* id, SubclassNameVar* tag) {
    _id = id;
    _tag = tag;
}

void* IDMapElem::id () { return _id; }
void* IDMapElem::tag () { return _tag; }

/*****************************************************************************/
IDMap::IDMap () {}

void IDMap::Add (MemberSharedName* msnamer) {
    Add(msnamer->GetIDVar(), msnamer->GetSubclass());
}

void IDMap::Add (IDVar* id, SubclassNameVar* tag) {
    UMapElem* idelem = FindId(id);
    if (idelem == nil) {
        IDMapElem* idelem = new IDMapElem(id, tag);
        Register(idelem);
    }
}

void IDMap::Delete (IDVar* id) {
    UMapElem* idelem = FindId(id);
    Unregister(idelem);
    delete idelem;
}

int IDMap::FindID (SubclassNameVar* tag) {
    const char* subclass = tag->GetName();
    const char* baseclass = tag->GetBaseClass();
    return FindID(subclass, baseclass);
}

int IDMap::FindID (const char* subclass, const char* baseclass) {
    int ok = 0;
    for (int i = 0; i < _elems.Count(); ++i) {
        IDMapElem* idelem = (IDMapElem*) Elem(i);
        SubclassNameVar* target = (SubclassNameVar*) idelem->tag();
        if (strcmp(target->GetName(), subclass) == 0) {
            if (strcmp(target->GetBaseClass(), baseclass) == 0) {
                IDVar* idvar = (IDVar*) idelem->id();
                ok = idvar->GetID();
            } else {
                ok = -1;
            }
            break;
        }
    }
    return ok;
}

void IDMap::Update (IDVar* id) {
    UMapElem* idelem = FindId(id);
    if (idelem != nil) {
        SubclassNameVar* tag = (SubclassNameVar*) idelem->tag();
        const char* subclass = tag->GetName();
        const char* baseclass = tag->GetBaseClass();
        for (int i = 0; i < _elems.Count(); ++i) {
            IDMapElem* idelem = (IDMapElem*) Elem(i);
            SubclassNameVar* target = (SubclassNameVar*) idelem->tag();
            if (
                target != tag && 
                strcmp(target->GetName(), subclass) == 0 && 
                strcmp(target->GetBaseClass(), baseclass) == 0
            ) {
                IDVar* idvar = (IDVar*) idelem->id();
                idvar->SetName(id->GetName());
            }
        }
    }
}

/*****************************************************************************/
int IDVar::_IDSerial = 3999;
IDMap* IDVar::_idmap = new IDMap;
/*****************************************************************************/

IDVar::IDVar (int id, int origid) {
    SetID(id);
    SetOrigID(origid);
}

void IDVar::CreateMap () {
    delete _idmap;
    _idmap = new IDMap;
}

IDVar::~IDVar () {}

int IDVar::GetID () {
    const char* name = GetName();
    if (name != nil) {
        return atoi(name);
    }
    return -1;
}

void IDVar::SetID (int id) {
    char idbuf[CHARBUFSIZE];
    sprintf(idbuf, "%d", id);
    SetName(idbuf);
}

void IDVar::GenNewName () {}

void IDVar::Update () {
    _idmap->Update(this);
}

StateVar& IDVar::operator = (StateVar& var) {
    IBNameVar::operator = (var);
    Update();
    return *this;
}

StateVar* IDVar::Copy () { return new IDVar(GetID(), GetOrigID()); }

ClassId IDVar::GetClassId () { return ID_VAR; }

boolean IDVar::IsA (ClassId id) {
    return ID_VAR == id || IBNameVar::IsA(id);
}

void IDVar::Read(istream& in) {
    IBNameVar::Read(in);
    _IDSerial = max(_IDSerial, GetID());
}

/*****************************************************************************/

int InstanceNameVar::_iSerial;

/*****************************************************************************/
InstanceNameVar::InstanceNameVar (
    const char* name, boolean machgen
) : IBNameVar(name, machgen) {}

InstanceNameVar::InstanceNameVar (IBNameVar* name) {
    SetName(name->GetName());
    SetMachGen(name->GetMachGen());
}

StateVar* InstanceNameVar::Copy () {
    InstanceNameVar* copy = new InstanceNameVar(this); 
    return copy;
}

ClassId InstanceNameVar::GetClassId () { return INSTANCENAME_VAR; }

boolean InstanceNameVar::IsA (ClassId id) {
    return INSTANCENAME_VAR == id || IBNameVar::IsA(id);
}

void InstanceNameVar::Read (istream& in) {
    IBNameVar::Read(in);
    if (GetMachGen()) {
        char* number = strrchr(GetName(), '_');
        if (number != nil) {
            number += 1;
            _iSerial = (_iSerial > atoi(number)) ? _iSerial : atoi(number) +1;
        }
    }
}

/*****************************************************************************/
SharedName::SharedName (
    const char* name, boolean machgen
) : IBNameVar(name, machgen) { _refcount = 0; }

SharedName::~SharedName () {}

void SharedName::ref () const {
    SharedName* s = (SharedName*) this;
    s->_refcount += 1;
}

void SharedName::unref () const {
    SharedName* s = (SharedName*) this;
    if (s->_refcount != 0) {
        s->_refcount -= 1;
        if (s->_refcount == 0) {
            delete s;
        }
    }
}
/*****************************************************************************/

MemberSharedName::MemberSharedName (
    const char* name, boolean export, boolean machgen
) : SharedName(name, machgen) {
    _export = export;
    _subclass = new SubclassNameVar("", false, false);
    _subclass->ref();
    _idVar = nil;
}

MemberSharedName::~MemberSharedName () {
    if (_subclass != nil) _subclass->unref();
    delete _idVar;
}

void MemberSharedName::SetSubclass(SubclassNameVar* sub) {
    if (sub != nil) sub->ref();
    if (_subclass != nil) _subclass->unref();
    _subclass = sub;
}

StateVar* MemberSharedName::Copy () {
    MemberSharedName* copy = new MemberSharedName(
        GetName(), GetExport(), GetMachGen()
    );
    copy->SetSubclass(_subclass);
    copy->SetIDVar(_idVar);
    return copy;
}

ClassId MemberSharedName::GetClassId () { return MEMBERSHAREDNAME; }

boolean MemberSharedName::IsA (ClassId id) {
    return MEMBERSHAREDNAME == id || SharedName::IsA(id);
}

StateVar& MemberSharedName::operator = (StateVar& var) {
    IBNameVar::operator = (var);

    if (var.IsA(MEMBERSHAREDNAME)) {
        MemberSharedName* mvar = (MemberSharedName*) &var;
        _export = mvar->GetExport();
        SetSubclass(mvar->GetSubclass());
        SetIDVar(mvar->GetIDVar());
    }
    return *this;
}

void MemberSharedName::SetName(const char* name) {
    char* fname = FilterName(name);
    SharedName::SetName(fname);
    delete fname;
}

void MemberSharedName::SetIDVar (IDVar* idvar ) {
    if (idvar != nil) {
        if (_idVar == nil) {
            _idVar = (IDVar*) idvar->Copy();
        } else {
            *_idVar = *idvar;
        }
    } 
}

void MemberSharedName::Read (istream& in) {
    Catalog* catalog = unidraw->GetCatalog();
    IBNameVar::Read(in);
    in >> _export;
    float version = catalog->FileVersion();
    if (version > 1.05 || !IBCreator::GetLock()) {
        if (_subclass != nil) _subclass->unref();
        _subclass = (SubclassNameVar*) catalog->ReadStateVar(in);
        if (_subclass != nil) _subclass->ref();
        delete _idVar;
        _idVar = (IDVar*) catalog->ReadStateVar(in);
    }
    
    if (GetMachGen()) {
        char* number = strrchr(GetName(), '_');
        if (number != nil) {
            number += 1;
            _mSerial = (_mSerial > atoi(number)) ? _mSerial : atoi(number) + 1;
        }
    }
    char* fname = FilterName(GetName());
    SetName(fname);
    delete fname;
}

void MemberSharedName::Write (ostream& out) {
    IBNameVar::Write(out);
    out << " " << _export << " "; 
    unidraw->GetCatalog()->WriteStateVar(_subclass, out);
    unidraw->GetCatalog()->WriteStateVar(_idVar, out);
}

/*****************************************************************************/

int MemberSharedName::_mSerial;

/*****************************************************************************/
MemberNameVar::MemberNameVar (
    const char* name, boolean export, boolean machgen
) {
    _msharedname = nil;
    if (name != nil) {
        _msharedname = new MemberSharedName(name, export, machgen);
        _msharedname->ref();
    }
}

MemberNameVar::MemberNameVar (MemberSharedName* msharedname) {
    _msharedname = msharedname;
    _msharedname->ref();
}

MemberNameVar::~MemberNameVar () {
    _msharedname->unref();
}

StateVar* MemberNameVar::Copy () {
    MemberNameVar* copy = new MemberNameVar(_msharedname);
    return copy;
}

ClassId MemberNameVar::GetClassId () { return MEMBERNAME_VAR; }

boolean MemberNameVar::IsA (ClassId id) {
    return MEMBERNAME_VAR == id || StateVar::IsA(id);
}

StateVar& MemberNameVar::operator = (StateVar& var) {
    StateVar::operator=(var); 

    if (var.IsA(MEMBERNAME_VAR)) {
        MemberNameVar* mvar = (MemberNameVar*) &var;
        SetMemberSharedName(mvar->GetMemberSharedName());
    }
    return *this;
}

void MemberNameVar::SetMemberSharedName (MemberSharedName* msharedname) {
    if (msharedname != nil) {
        msharedname->ref();
    }
    if (_msharedname != nil && _msharedname != msharedname) {
	_msharedname->unref();
    }
    _msharedname = msharedname;
}

void MemberNameVar::Read (istream& in) {
    StateVar::Read(in);
    Catalog* catalog = unidraw->GetCatalog();

    if (_msharedname != nil) {
	_msharedname->unref();
    }

    _msharedname = (MemberSharedName*) catalog->ReadStateVar(in);
    _msharedname->ref();
}

void MemberNameVar::Write (ostream& out) {
    StateVar::Write(out);
    unidraw->GetCatalog()->WriteStateVar(_msharedname, out);
}

/*****************************************************************************/

int ButtonSharedName::_bsSerial;

/*****************************************************************************/

ButtonSharedName::ButtonSharedName(
    const char* name, const char* func, boolean machgen, const char* baseclass
) : SharedName(name, machgen) {
    _initial = 0;
    _export = true;
    if (baseclass == nil) {
        _subclass = new SubclassNameVar("ButtonState", false, false);
    } else {
        _subclass = new SubclassNameVar(baseclass, false, false);
    }
    _subclass->ref();
    if (func != nil) {
	_func = strnew(func);
    } else {
	_func = strnew("");
    }
}

ButtonSharedName::~ButtonSharedName () {
    delete _func;
    if (_subclass != nil) _subclass->unref();
}

void ButtonSharedName::SetFuncName(const char* func) {
    delete _func;
    if (func != nil) {
	_func = strnew(func);
    }
}

void ButtonSharedName::SetSubclass(SubclassNameVar* sub) {
    if (sub != nil) sub->ref();
    if (_subclass != nil) _subclass->unref();
    _subclass = sub;
}

StateVar* ButtonSharedName::Copy () {
    ButtonSharedName* copy = new ButtonSharedName(
	GetName(), _func, GetMachGen()
    );
    copy->SetInitial(_initial);
    copy->SetExport(_export);
    *copy->GetSubclass() = *_subclass;
    return copy;
}

StateVar& ButtonSharedName::operator = (StateVar& var) {
    IBNameVar::operator=(var); 

    if (var.IsA(BUTTONSHAREDNAME)) {
        ButtonSharedName* bsname = (ButtonSharedName*) &var;
	_initial = bsname->GetInitial();
	_export = bsname->GetExport();
        *_subclass = *bsname->GetSubclass();
	SetFuncName(bsname->GetFuncName());
    }
    return *this;
}

ClassId ButtonSharedName::GetClassId () { return BUTTONSHAREDNAME; }

boolean ButtonSharedName::IsA (ClassId id) {
    return BUTTONSHAREDNAME == id || IBNameVar::IsA(id);

}

void ButtonSharedName::Read (istream& in) {
    Catalog* catalog = unidraw->GetCatalog();
    float version = catalog->FileVersion();

    IBNameVar::Read(in);
    in >> _initial >> _export;
    _func = unidraw->GetCatalog()->ReadString(in);
    if (version >= 1.05 || !IBCreator::GetLock()) {
        if (_subclass != nil) _subclass->unref();
        _subclass = (SubclassNameVar*) catalog->ReadStateVar(in);
        if (_subclass != nil) _subclass->ref();
    }
}

void ButtonSharedName::Write (ostream& out) {
    IBNameVar::Write(out);
    out << " " << _initial << " " << _export; 
    unidraw->GetCatalog()->WriteString(_func, out);
    unidraw->GetCatalog()->WriteStateVar(_subclass, out);
}

/*****************************************************************************/

ButtonStateVar::ButtonStateVar (const char* name) {
    Init();
    _bsharedname = new ButtonSharedName(name);
    _bsharedname->ref();
    _showsetting = true;
}

ButtonStateVar::~ButtonStateVar () {
    _bsharedname->unref();
}

StateVar* ButtonStateVar::Copy () {
    return new ButtonStateVar(_bsharedname, _setting);
}

ClassId ButtonStateVar::GetClassId () { return BUTTONSTATE_VAR; }

boolean ButtonStateVar::IsA (ClassId id) {
    return BUTTONSTATE_VAR == id || StateVar::IsA(id);
}

void ButtonStateVar::Init () {
    _setting = 0;
    _showsetting = true;
    _bsharedname = nil;
}

ButtonStateVar::ButtonStateVar (ButtonSharedName* bsharedname, int setting) {
    Init();
    _setting = setting;
    _bsharedname = bsharedname;
    _bsharedname->ref();
}

StateVar& ButtonStateVar::operator = (StateVar& var) {
    StateVar::operator=(var);
    if (var.IsA(BUTTONSTATE_VAR)) {
        ButtonStateVar* bvar = (ButtonStateVar*) &var;
	_setting = bvar->GetSetting();
        SetButtonSharedName(bvar->GetButtonSharedName());
    }
    return *this;
}

void ButtonStateVar::SetSetting (int setting) {
    _setting = setting;
}

void ButtonStateVar::SetButtonSharedName (ButtonSharedName* bsharedname) {
    if (bsharedname != nil) {
        bsharedname->ref();
    }
    if (_bsharedname != nil && _bsharedname != bsharedname) {
	_bsharedname->unref();
    }
    _bsharedname = bsharedname;
}

void ButtonStateVar::Read (istream& in) {
    Catalog* catalog = unidraw->GetCatalog();
    if (_bsharedname != nil) {
	_bsharedname->unref();

    }
    StateVar::Read(in);

    _bsharedname = (ButtonSharedName*) catalog->ReadStateVar(in);
    _bsharedname->ref();

    in >> _setting >> _showsetting;
}

void ButtonStateVar::Write (ostream& out) {
    Catalog* catalog = unidraw->GetCatalog();
    StateVar::Write(out);
    catalog->WriteStateVar(_bsharedname, out);
    out << " " << _setting << " " << _showsetting << "\n";
}

/*****************************************************************************/

IBShape::IBShape () {
    hnat = vnat = false;
    hstr = vstr = false;
    hshr = vshr = false;
}

IBShape& IBShape::operator = (IBShape& ishape) {
    width = ishape.width;
    height = ishape.height;
    hshrink = ishape.hshrink;
    hstretch = ishape.hstretch;
    vshrink = ishape.vshrink;
    vstretch = ishape.vstretch;
    return *this;
}

IBShape* IBShape::Copy () {
    IBShape* copy = new IBShape;
    *copy = *this;
    return copy;
}

/*****************************************************************************/

ShapeVar::ShapeVar (IBShape* s) {
    _ibshape = s; 
}

ShapeVar::~ShapeVar () { delete _ibshape; }

void ShapeVar::SetShape (IBShape* s) {
    if (s != _ibshape) {
        delete _ibshape;
        _ibshape = s;
        Notify();
    }
}

StateVar& ShapeVar::operator = (StateVar& var) {
    if (var.IsA(SHAPE_VAR)) {
        StateVar::operator=(var);
        ShapeVar* svar = (ShapeVar*) &var;
        IBShape* varshape = svar->GetShape();
        
        if (varshape == nil) {
            SetShape(varshape); 
        } else {
            if (_ibshape == nil) {
                _ibshape = new IBShape;
            }
            *_ibshape = *varshape;
            Notify();
        }
    }
    return *this;
}

StateVar* ShapeVar::Copy () { 
    ShapeVar* copy = new ShapeVar;
    *copy = *this;
    return copy;
}

ClassId ShapeVar::GetClassId () { return SHAPE_VAR; }
boolean ShapeVar::IsA (ClassId id) { return SHAPE_VAR==id ||StateVar::IsA(id);}

void ShapeVar::Read (istream& in) {
    StateVar::Read(in);
    char lookahead;
    in >> lookahead;

    if (lookahead != '~') {
        in.putback(lookahead);
        _ibshape = new IBShape;

        in >> _ibshape->width >> _ibshape->height;
        in >> _ibshape->hstretch >> _ibshape->vstretch;
        in >> _ibshape->hshrink >> _ibshape->vshrink;
        in >> _ibshape->aspect >> _ibshape->hunits >> _ibshape->vunits;

	in >> _ibshape->hnat >> _ibshape->vnat;
	in >> _ibshape->hstr >> _ibshape->vstr;
	in >> _ibshape->hshr >> _ibshape->vshr;
    }
}

void ShapeVar::Write (ostream& out) {
    StateVar::Write(out);
    
    if (_ibshape == nil) {
        out << "~ ";

    } else {
        out << _ibshape->width << " " << _ibshape->height << " ";
        out << _ibshape->hstretch << " " << _ibshape->vstretch << " ";
        out << _ibshape->hshrink << " " << _ibshape->vshrink << " ";
        out << _ibshape->aspect << " ";
        out << _ibshape->hunits << " " << _ibshape->vunits << " ";

        out << _ibshape->hnat << " " << _ibshape->vnat << " ";
        out << _ibshape->hstr << " " << _ibshape->vstr << " ";
        out << _ibshape->hshr << " " << _ibshape->vshr << " ";
    }
}

/*****************************************************************************/

TrackNameVar::TrackNameVar (const char* n) : IBNameVar(n, true) {}

StateVar* TrackNameVar::Copy () { 
    return new TrackNameVar(GetName());
}

void TrackNameVar::GenNewName () {}

ClassId TrackNameVar::GetClassId () { return PROCNAME_VAR; }

boolean TrackNameVar::IsA (ClassId id) {
    return PROCNAME_VAR == id || IBNameVar::IsA(id);
}

/*****************************************************************************/

int SubclassNameVar::_subclassSerial;

/*****************************************************************************/

SubclassNameVar::SubclassNameVar (
    const char* baseclass, boolean machgen, boolean abstract
) : SharedName(baseclass, machgen) {
    _baseClass = nil;
    if (baseclass != nil) {
        SetBaseClass(baseclass);
    }
    _abstract = abstract;
}

SubclassNameVar::~SubclassNameVar() {
    delete _baseClass;
}

boolean SubclassNameVar::IsSubclass() {
    return (strcmp(GetName(), _baseClass) != 0);
}

StateVar& SubclassNameVar::operator = (StateVar& var) {
    IBNameVar::operator=(var); 

    if (var.IsA(SUBCLASSNAME_VAR)) {
        SubclassNameVar* sVar = (SubclassNameVar*) &var;
	SetBaseClass(sVar->GetBaseClass());
        SetAbstract(sVar->IsAbstract());
    }
    return *this;
}

void SubclassNameVar::SetBaseClass(const char* name) {
    delete _baseClass;
   _baseClass = strnew(name);
}

StateVar* SubclassNameVar::Copy () {
    SubclassNameVar* copy = new SubclassNameVar(
        _baseClass, GetMachGen(), IsAbstract()
    );
    copy->SetName(GetName());
    return copy;
}

ClassId SubclassNameVar::GetClassId () { return SUBCLASSNAME_VAR; }

boolean SubclassNameVar::IsA (ClassId id) {
    return SUBCLASSNAME_VAR == id || IBNameVar::IsA(id);
}

void SubclassNameVar::Read (istream& in) {
    delete _baseClass;
    _baseClass = unidraw->GetCatalog()->ReadString(in);
    IBNameVar::Read(in);
    in >> _abstract;
}

void SubclassNameVar::Write (ostream& out) {
    unidraw->GetCatalog()->WriteString(_baseClass, out);
    IBNameVar::Write(out);
    out << _abstract << " ";
}

/*****************************************************************************/

FBrowserVar::FBrowserVar (const char* dir, const char* textfilter) {
    _dir = nil;
    _textfilter = nil;
    if (dir != nil) {
        SetDirName(dir);
    }
    if (textfilter != nil) {
        SetTextFilter(textfilter);
    }
}

FBrowserVar::~FBrowserVar() {
    delete _dir;
    delete _textfilter;
}

StateVar& FBrowserVar::operator = (StateVar& var) {
    StateVar::operator=(var); 
    if (var.IsA(FBROWSER_VAR)) {
        FBrowserVar* fVar = (FBrowserVar*) &var;
        SetDirName(fVar->GetDirName());
        SetTextFilter(fVar->GetTextFilter());
    }
    return *this;
}

void FBrowserVar::SetDirName(const char* dir) {
    delete _dir;
    _dir = strnew(dir);
}

void FBrowserVar::SetTextFilter(const char* textfilter) {
    delete _textfilter;
    _textfilter = strnew(textfilter);
}

StateVar* FBrowserVar::Copy () {
    FBrowserVar* copy = new FBrowserVar(_dir, _textfilter);
    return copy;
}

ClassId FBrowserVar::GetClassId () { return FBROWSER_VAR; }

boolean FBrowserVar::IsA (ClassId id) {
    return FBROWSER_VAR == id || StateVar::IsA(id);
}

void FBrowserVar::Read (istream& in) {
    StateVar::Read(in);
    delete _dir;
    delete _textfilter;
    _dir = unidraw->GetCatalog()->ReadString(in);
    _textfilter = unidraw->GetCatalog()->ReadString(in);
}

void FBrowserVar::Write (ostream& out) {
    StateVar::Write(out);
    unidraw->GetCatalog()->WriteString(_dir, out);
    unidraw->GetCatalog()->WriteString(_textfilter, out);
}

/*****************************************************************************/

BooleanStateVar::BooleanStateVar (boolean bstate) { _bstate = bstate; }

StateVar* BooleanStateVar::Copy () {
    BooleanStateVar* copy = new BooleanStateVar(_bstate);
    return copy;
}

StateVar& BooleanStateVar::operator = (StateVar& var) {
    StateVar::operator=(var);
    if (var.IsA(BOOLEANSTATE_VAR)) {
        BooleanStateVar* bvar = (BooleanStateVar*) &var;
        SetBooleanState(bvar->GetBooleanState());
    }
    return *this;
}

ClassId BooleanStateVar::GetClassId () { return BOOLEANSTATE_VAR; }

boolean BooleanStateVar::IsA (ClassId id) {
    return BOOLEANSTATE_VAR == id || StateVar::IsA(id);
}

void BooleanStateVar::Read (istream& in) {
    StateVar::Read(in);
    in >> _bstate;
}

void BooleanStateVar::Write (ostream& out) {
    StateVar::Write(out);
    out << _bstate << " ";
}
