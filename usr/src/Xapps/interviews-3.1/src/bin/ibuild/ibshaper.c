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
 * Implementation of Shaper component and derived classes.
 */

#include "ibclasses.h"
#include "ibshaper.h"
#include "ibvars.h"
#include <Unidraw/ulist.h>
#include <Unidraw/Graphic/graphic.h>
#include <string.h>
#include <stream.h>

/*****************************************************************************/

ShaperComp::ShaperComp () {
    GetClassNameVar()->SetName("Shaper");
    GetClassNameVar()->SetBaseClass("Shaper");
    IBShape* ibshape = GetShapeVar()->GetShape();
    ibshape->hnat = ibshape->hstr = ibshape->hshr = true;
    ibshape->vnat = ibshape->vstr = ibshape->vshr = true;
    _lock = false;
}

void ShaperComp::Reconfig () {
    InteractorComp* kid = GetKid();
    if (kid != nil) {
        kid->Reconfig();
        if (!_lock) {
            _lock = true;
            *GetShapeVar() = *kid->GetShapeVar();
        }
    }
}

void ShaperComp::Read (istream& in) {
    MonoSceneComp::Read(in);
    in >> _lock;
}

void ShaperComp::Write(ostream& out) {
    MonoSceneComp::Write(out);
    out << _lock << " ";
}

ClassId ShaperComp::GetClassId () { return SHAPER_COMP; }

boolean ShaperComp::IsA (ClassId id) { 
    return SHAPER_COMP==id || MonoSceneComp::IsA(id);
}

/*****************************************************************************/

ClassId ShaperCode::GetClassId () { return SHAPER_CODE; }

boolean ShaperCode::IsA (ClassId id) { 
    return SHAPER_CODE ==id || MonoSceneCode::IsA(id);
}

ShaperCode::ShaperCode (ShaperComp* subj) : MonoSceneCode(subj) { }
ShaperComp* ShaperCode::GetShaperComp () { return (ShaperComp*) GetSubject(); }

void ShaperCode::Update () {
    MonoSceneCode::Update();
    InteractorComp* subj = GetIntComp();
    Graphic* gr = subj->GetGraphic();
    gr->SetColors(nil, nil);
    gr->SetFont(nil);
}

boolean ShaperCode::Definition (ostream& out) {
    boolean ok = true;

    char coreclass[CHARBUFSIZE];
    ShaperComp* shcomp = GetShaperComp();
    SubclassNameVar* snamer = shcomp->GetClassNameVar();
    MemberNameVar* mnamer = shcomp->GetMemberNameVar();
    const char* subclass = snamer->GetName();
    const char* mname = mnamer->GetName();
    GetCoreClassName(coreclass);
    CodeView* kview = GetKidView();

    if (_emitProperty) {
        ok = ok && MonoSceneCode::Definition(out);
        if (kview != nil) {
            ok = ok && kview->Definition(out);
        }
    } else if (_emitInstanceDecls) {
        if (!snamer->IsSubclass()) {
            if (_emitExport) {
                if (mnamer->GetExport()) {
                    out << "    MonoScene* " << mname << ";\n";
                }
            } else {
                if (!mnamer->GetExport() || _emitMain) {
                    out << "    " << subclass << "* " << mname << ";\n";
                }
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }
        if (kview != nil) {
            ok = ok && kview->Definition(out);
        }
    } else if (_emitForward) {
        if (!snamer->IsSubclass()) {
            if (_scope) {
                if (!_namelist->Search("MonoScene") && mnamer->GetExport()) {
                    _namelist->Append("MonoScene");
                    out << "class MonoScene;\n";
                }
            }            
        } else {
            ok = ok && CodeView::Definition(out);
        }
        if (kview != nil) {
            ok = ok && kview->Definition(out);
        }
    } else if (_emitExpHeader) {
        if (!snamer->IsSubclass()) {
            if (_scope && mnamer->GetExport()&&!_namelist->Search("scene")) {
                _namelist->Append("scene");
                out << "#include <InterViews/scene.h>\n";
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }
        if (kview != nil) {
            ok = ok && kview->Definition(out);
        }
    } else if (_emitHeaders || _emitClassHeaders) {
	ok = ok && CodeView::Definition(out);
        if (kview != nil) {
            ok = ok && kview->Definition(out);
        }

    } else if (_emitCorehHeader) {
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("scene")) {
                _namelist->Append("scene");
                out << "#include <InterViews/scene.h>\n";
            }
        } else {
            if (kview != nil) {
                ok = ok && kview->Definition(out);
            }
        }
    } else if (_emitInstanceInits) {
        Shape* shape = shcomp->GetShapeVar()->GetShape();

	if (!_instancelist->Find((void*)mname)) {
	    _instancelist->Append(new UList((void*)mname));

            char coreclass[CHARBUFSIZE];
            char ShaperClass[CHARBUFSIZE];
            
            GetCoreClassName(coreclass);
            strcpy(ShaperClass, coreclass);
            strcat(ShaperClass, "_Shaper");
            
            if (snamer->IsSubclass()) {
                BeginInstantiate(out);
                out << "(";

            } else {
                if (mnamer->GetExport() && !_emitMain) {
                    out << "    " << mname << " = new " << ShaperClass;
                    out << "(";
                } else {
                    out << "    " << ShaperClass << "* ";
                    out << mname << " = new " << ShaperClass << "(";
                }
            }
            InstanceName(out);
            
            out << shape->width << ", " << shape->height << ", ";
            out << shape->hstretch << ", " << shape->vstretch << ", ";
            out << shape->hshrink << ", " << shape->vshrink << ")";
            EndInstantiate(out);
	}

        if (kview != nil) {
            ok = kview->Definition(out) && ok;
        }

	if (AllKidsDefined() && !_lock) {
	    _lock = true;
            InteractorComp* kid = shcomp->GetKid();
            const char* instance = kid->GetMemberNameVar()->GetName();
            out << "    " << mname << "->Insert(";
            out << instance << ");\n";	
	}

    } else if (
	_emitBSDecls || _emitBSInits || 
	_emitFunctionDecls || _emitFunctionInits ||
        _emitCreatorHeader || _emitCreatorSubj || _emitCreatorView
    ) {
        if (kview != nil) {
            ok = ok && kview->Definition(out);
        }

    } else if (_emitCoreDecls) {
        if (
            strcmp(subclass, _classname) == 0 &&
            !_globallist->Search(_classname)
        ) {
	    _globallist->Append(_classname);
            out << "class " << coreclass;
            out << " : public MonoScene {\n";
            out << "public:\n";
            out << "    " << coreclass;
            out << "(const char*, int, int, int, int, int, int);\n\n";
            out << "    virtual void Reconfig();\n";
            out << "private:\n";
            out << "    int _h, _v;\n";
            out << "    int _hstr, _vstr;\n";
            out << "    int _hshr, _vshr;\n";
            out << "};\n\n";
        }                
        if (kview != nil) {
            ok = ok && kview->Definition(out);
        }

    } else if (
        _emitCoreInits || _emitClassDecls || _emitClassInits
    ) {
	ok = ok && CodeView::Definition(out);
        if (kview != nil) {
            ok = ok && kview->Definition(out);
        }

    } else if (_emitMain) {
	ok = ok && CodeView::Definition(out);
        
    }
    return out.good() && ok;
}

boolean ShaperCode::CoreConstInits(ostream& out) {
    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* subclass = snamer->GetName();
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, int h, int v, ";
    out << "int hstr, int vstr, int hshr, int vshr\n) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "    SetInstance(name);\n";
    out << "    _h = h, _v = v;\n";
    out << "    _hstr = hstr, _vstr = vstr;\n";
    out << "    _hshr = hshr, _vshr = vshr;\n";
    out << "}\n\n";

    out << "void " << coreclass << "::Reconfig () {\n";
    out << "    MonoScene::Reconfig();\n";
    out << "    shape->width = _h;\n";
    out << "    shape->height = _v;\n";
    out << "    shape->hstretch = _hstr;\n";
    out << "    shape->vstretch = _vstr;\n";
    out << "    shape->hshrink = _hshr;\n";
    out << "    shape->vshrink = _vshr;\n";
    out << "}\n\n";
    return out.good();
}

boolean ShaperCode::ConstDecls(ostream& out) {
    out << "(const char*, int, int, int, int, int, int);\n";
    return out.good();
}

boolean ShaperCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, int h, int v, ";
    out << "int hstr, int vstr, int hshr, int vshr\n) : " << coreclass;
    out << "(name, h, v, hstr, vstr, hshr, vshr) {}\n\n";

    return out.good();
}

boolean ShaperCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("scene")) {
        _namelist->Append("scene");
        out << "#include <InterViews/scene.h> \n";
    }
    if (!_namelist->Search("shape")) {
        _namelist->Append("shape");
        out << "#include <InterViews/shape.h> \n";
    }
    return out.good();
}

