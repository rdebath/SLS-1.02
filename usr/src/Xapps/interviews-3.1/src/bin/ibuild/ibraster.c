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
 * IRaster component definitions.
 */

#include "ibraster.h"
#include "ibclasses.h"
#include "ibglobals.h"
#include "ibvars.h"
#include <Unidraw/statevars.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/catalog.h>
#include <Unidraw/Graphic/rasterrect.h>
#include <stdio.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/
static const char* rastercomp_delim = "%rastercomp_delim";
/*****************************************************************************/

IRasterComp::IRasterComp (RasterRect* graphic) {
    _gclassNameVar->SetName("RasterRect");
    _gclassNameVar->SetBaseClass("RasterRect");
    _cclassNameVar->SetName("RasterComp");
    _cclassNameVar->SetBaseClass("RasterComp");
    _vclassNameVar->SetName("RasterView");
    _vclassNameVar->SetBaseClass("RasterView");
    _compid->SetOrigID(RASTER_COMP);

    if (!_release || graphic != nil) {
        _target = new RasterComp(graphic);
        if (graphic != nil) {
            Append(_target);
        }
    }
}

ClassId IRasterComp::GetSubstId(const char*& delim) {
    delim = rastercomp_delim;
    return RASTER_COMP;
}

ClassId IRasterComp::GetClassId () { return IRASTER_COMP; }

boolean IRasterComp::IsA (ClassId id) {
    return IRASTER_COMP == id || IComp::IsA(id);
}

/*****************************************************************************/

ClassId RasterCode::GetClassId () { return IRASTER_CODE; }

boolean RasterCode::IsA(ClassId id) {
    return IRASTER_CODE == id || GraphicCodeView::IsA(id);
}

RasterCode::RasterCode (
    IRasterComp* subj
) : GraphicCodeView(subj) {
    _unidraw = true;
}

void RasterCode::Update () {
    GraphicCodeView::Update();
    GetIRasterComp()->Bequeath();
}

IRasterComp* RasterCode::GetIRasterComp () {
    return (IRasterComp*) GetSubject(); 
}

const char* RasterCode::GetGHeader () { return "rasterrect"; }
const char* RasterCode::GetCVHeader () { return "raster"; }

boolean RasterCode::Definition (ostream& out) {
    boolean ok = true;

    const char* sfile;
    IRasterComp* rastercomp = GetIRasterComp();
    RasterComp* target = (RasterComp*) rastercomp->GetTarget();
    RasterRect* raster = target->GetRasterRect();

    SubclassNameVar* cnamer = rastercomp->GetCClassNameVar();
    SubclassNameVar* gnamer = rastercomp->GetGClassNameVar();
    MemberNameVar* mnamer = rastercomp->GetMemberNameVar();

    const char* mname = mnamer->GetName();
    const char* cname = cnamer->GetName();
    const char* gname = gnamer->GetName();

    if (_emitInstanceDecls || _emitGraphicState) {
	ok = ok && GraphicCodeView::Definition(out);

    } else if (_emitInstanceInits) {
        static int raster_id;

        char substName[CHARBUFSIZE];
        sfile = target->GetFileName();
        Catalog* catalog = unidraw->GetCatalog();

        if (sfile == nil) {
            sprintf(substName, "raster%d.ps", raster_id++);
            sfile = substName;
        }
        if (!catalog->Exists(sfile)) {
            char orig[CHARBUFSIZE];
            const char* name = catalog->GetName(rastercomp->GetRoot());
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
        out << "        RasterComp* " << mname << "_comp = (RasterComp*) ";
        out << "ImportCmd::Import(\"" << sfile << "\");\n"; 
        out << "        Raster* " << mname << "_raster = ";
        out << mname << "_comp->GetRasterRect()->GetOriginal();\n";
        if (_emitGraphicComp) {
           out << "        " << mname << "_gr";
        } else {
            out << "        " << mname;
        }
        out << " = new " << gname << "(" << mname << "_raster, ";
        out << mname << "_comp->GetGraphic());\n";
        out << "        delete " << mname << "_comp;\n";

        ok = WriteGraphicInits(raster, out);
        if (_emitGraphicComp) {
            out << "        " << mname << " = new " << cname << "(";
            out << mname << "_gr, \"" << sfile << "\");\n";
            out << "        " << mname << "->Update();\n";
        }
        out << "    }\n";

    } else if (_emitExpHeader) {
        ok = ok && GraphicCodeView::Definition(out);

        if (strcmp(gname, _classname) == 0) {
            if (!_namelist->Search("raster")) {
                _namelist->Append("raster");
                out << "#include <InterViews/raster.h> \n";
            }
        }
    } else {
        ok = ok && GraphicCodeView::Definition(out);
    }
    return ok && out.good();
}

boolean RasterCode::GCoreConstDecls(ostream& out) { 
    out << "(Raster*, Graphic* gr = nil);\n";
    return out.good();
}

boolean RasterCode::GCoreConstInits(ostream& out) {
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* baseclass = gnamer->GetBaseClass();

    out << "(\n    Raster* raster, Graphic* gr\n) : ";
    out << baseclass << "(raster, gr) {}\n\n";

    return out.good();
}

boolean RasterCode::GConstDecls(ostream& out) {
    out << "(Raster*, Graphic* gr = nil);\n";
    out << "    virtual Graphic* Copy();\n";
    return out.good();
}

boolean RasterCode::GConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* gname = gnamer->GetName();

    out << "(\n    Raster* raster, Graphic* gr\n) : ";
    out << coreclass << "(raster, gr) {}\n\n";
    out << "Graphic* " << gname << "::Copy () {\n";
    out << "    return new " << gname;
    out << "(new Raster(*GetOriginal()), this);\n";
    out << "}\n\n";

    return out.good();
}

boolean RasterCode::EmitIncludeHeaders(ostream& out) {
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
        if (!_namelist->Search("rastercomp")) {
            _namelist->Append("rastercomp");
            out << "#include <Unidraw/Components/rastercomp.h> \n";
        }
    }
    return out.good();
}

