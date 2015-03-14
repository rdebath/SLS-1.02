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
 * IStencil component definitions.
 */

#include "ibstencil.h"
#include "ibclasses.h"
#include "ibvars.h"
#include "ibglobals.h"
#include <Unidraw/statevars.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/catalog.h>
#include <Unidraw/Graphic/ustencil.h>
#include <stdio.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/
static const char* stencilcomp_delim = "%stencilcomp_delim";
/*****************************************************************************/

IStencilComp::IStencilComp (UStencil* graphic) {
    _gclassNameVar->SetName("UStencil");
    _gclassNameVar->SetBaseClass("UStencil");
    _cclassNameVar->SetName("StencilComp");
    _cclassNameVar->SetBaseClass("StencilComp");
    _vclassNameVar->SetName("StencilView");
    _vclassNameVar->SetBaseClass("StencilView");
    _compid->SetOrigID(STENCIL_COMP);

    GetClassNameVar()->SetName("UStencil");
    GetClassNameVar()->SetBaseClass("UStencil");
    if (!_release || graphic != nil) {
        _target = new StencilComp(graphic);
        if (graphic != nil) {
            Append(_target);
        }
    }
}

void IStencilComp::ReadStateVars(istream& in) {
    IComp::ReadStateVars(in);
    float version = unidraw->GetCatalog()->FileVersion();
    if (version < 1.05) {
        if (strcmp(_gclassNameVar->GetBaseClass(), "Stencil") == 0) {
            _gclassNameVar->SetBaseClass("UStencil");

            if (strcmp(_gclassNameVar->GetName(), "Stencil") == 0) {
                _gclassNameVar->SetName("UStencil");
            }
        }
    }
}

ClassId IStencilComp::GetSubstId(const char*& delim) {
    delim = stencilcomp_delim;
    return STENCIL_COMP;
}

ClassId IStencilComp::GetClassId () { return ISTENCIL_COMP; }

boolean IStencilComp::IsA (ClassId id) {
    return ISTENCIL_COMP == id || IComp::IsA(id);
}

/*****************************************************************************/

ClassId StencilCode::GetClassId () { return ISTENCIL_CODE; }

boolean StencilCode::IsA(ClassId id) {
    return ISTENCIL_CODE == id || GraphicCodeView::IsA(id);
}

StencilCode::StencilCode (
    IStencilComp* subj
) : GraphicCodeView(subj) {
    _unidraw = true;
}

void StencilCode::Update () {
    GraphicCodeView::Update();
    GetIStencilComp()->Bequeath();
}

IStencilComp* StencilCode::GetIStencilComp () {
    return (IStencilComp*) GetSubject(); 
}

const char* StencilCode::GetGHeader () { return "ustencil"; }
const char* StencilCode::GetCVHeader () { return "stencilcomp"; }

boolean StencilCode::Definition (ostream& out) {
    boolean ok = true;
    const char* sfile;

    IStencilComp* stencilcomp = GetIStencilComp();
    StencilComp* target = (StencilComp*) stencilcomp->GetTarget();
    UStencil* stencil = target->GetStencil();

    SubclassNameVar* cnamer = stencilcomp->GetCClassNameVar();
    SubclassNameVar* gnamer = stencilcomp->GetGClassNameVar();
    MemberNameVar* mnamer = stencilcomp->GetMemberNameVar();

    const char* mname = mnamer->GetName();
    const char* cname = cnamer->GetName();
    const char* gname = gnamer->GetName();

    if (_emitInstanceDecls || _emitGraphicState) {
	ok = ok && GraphicCodeView::Definition(out);

    } else if (_emitInstanceInits) {
        static int stencil_id;

        char substName[CHARBUFSIZE];
        sfile = target->GetFileName();
        Catalog* catalog = unidraw->GetCatalog();

        if (sfile == nil) {
            sprintf(substName, "stencil%d.ps", stencil_id++);
            sfile = substName;
        }
        if (!catalog->Exists(sfile)) {
            char orig[CHARBUFSIZE];
            const char* name = catalog->GetName(stencilcomp->GetRoot());
            char* dir = GetDirName(name);
            char* index = strrchr(sfile, '/');
            if (index == nil) {
                strcpy(orig, sfile);
            } else {
                strcpy(orig, &index[1]);
            }
            strcpy(substName, dir);
            strcat(substName, orig);
            sfile = substName;
            if (!catalog->Exists(sfile)) {
                catalog->Save(target, sfile);
            }
        }
        out << "    {\n";
        out << "        StencilComp* " << mname << "_comp = (StencilComp*) ";
        out << "ImportCmd::Import(\"" << sfile << "\");\n"; 
        out << "        Bitmap* " << mname << "_image, *";
        out << mname << "_mask;\n";
        out << "        " << mname << "_comp->GetStencil()->GetOriginal(";
        out << mname << "_image, " << mname << "_mask);\n";
        if (_emitGraphicComp) {
            out << "        " << mname << "_gr";
        } else {
            out << "        " << mname;
        }
        out << " = new " << gname << "(" << mname << "_image, ";
        out << mname << "_mask, ";
        out << mname << "_comp->GetGraphic());\n";
        out << "        delete " << mname << "_comp;\n";

        ok = WriteGraphicInits(stencil, out);
        if (_emitGraphicComp) {
            out << "        " << mname << " = new " << cname << "(";
            out << mname << "_gr, \"" << sfile << "\");\n";
            out << "        " << mname << "->Update();\n";
        }
        out << "    }\n";

    } else if (_emitExpHeader) {
        ok = ok && GraphicCodeView::Definition(out);

        if (strcmp(gname, _classname) == 0) {
            if (!_namelist->Search("stencil")) {
                _namelist->Append("stencil");
                out << "#include <InterViews/stencil.h> \n";
            }
        }
    } else {
        ok = ok && GraphicCodeView::Definition(out);
    }
    return ok && out.good();
}

boolean StencilCode::GCoreConstDecls(ostream& out) { 
    out << "(Bitmap* image, Bitmap* mask = nil, Graphic* gr = nil);\n";
    return out.good();
}

boolean StencilCode::GCoreConstInits(ostream& out) {
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* baseclass = gnamer->GetBaseClass();

    out << "(\n    Bitmap* image, Bitmap* mask, Graphic* gr\n) : ";
    out << baseclass << "(image, mask, gr) {}\n\n";

    return out.good();
}

boolean StencilCode::GConstDecls(ostream& out) {
    out << "(Bitmap* image, Bitmap* mask = nil, Graphic* gr = nil);\n";
    out << "    virtual Graphic* Copy();\n";
    return out.good();
}

boolean StencilCode::GConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* gname = gnamer->GetName();

    out << "(\n    Bitmap* image, Bitmap* mask, Graphic* gr\n) : ";
    out << coreclass << "(image, mask, gr) {}\n\n";
    out << "Graphic* " << gname << "::Copy () {\n";
    out << "    Bitmap* image_cpy = new Bitmap(*_image);\n";
    out << "    Bitmap* mask_cpy = \n";
    out << "        (_mask == nil) ? nil :\n";
    out << "        (_mask == _image) ? image_cpy :\n";
    out << "        new Bitmap(*_mask);\n";
    out << "    return new " << gname << "(image_cpy, mask_cpy, this);\n";
    out << "}\n\n";

    return out.good();
}

boolean StencilCode::EmitIncludeHeaders(ostream& out) {
    GraphicCodeView::EmitIncludeHeaders(out);

    SubclassNameVar* gnamer = GetIComp()->GetGClassNameVar();
    SubclassNameVar* cnamer = GetIComp()->GetCClassNameVar();
    SubclassNameVar* vnamer = GetIComp()->GetVClassNameVar();
    if (
        strcmp(gnamer->GetName(), _classname) != 0 && 
        strcmp(cnamer->GetName(), _classname) != 0 && 
        strcmp(vnamer->GetName(), _classname) != 0
    ) {
        if (!_namelist->Search("import")) {
            _namelist->Append("import");
            out << "#include <Unidraw/Commands/import.h> \n";
        }
        if (!_namelist->Search("stencilcomp")) {
            _namelist->Append("stencilcomp");
            out << "#include <Unidraw/Components/stencilcomp.h> \n";
        }
    }
        
    return out.good();
}

