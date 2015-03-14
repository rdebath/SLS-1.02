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
 * IPolygon component definitions.
 */


#include "ibpolygon.h"
#include "ibclasses.h"
#include "ibvars.h"
#include <Unidraw/Graphic/polygons.h>
#include <stream.h>
#include <string.h>

/****************************************************************************/
static const char* polycomp_delim = "%polycomp_delim";
/*****************************************************************************/

IPolygonComp::IPolygonComp (SF_Polygon* graphic) {
    _gclassNameVar->SetName("SF_Polygon");
    _gclassNameVar->SetBaseClass("SF_Polygon");
    _cclassNameVar->SetName("PolygonComp");
    _cclassNameVar->SetBaseClass("PolygonComp");
    _vclassNameVar->SetName("PolygonView");
    _vclassNameVar->SetBaseClass("PolygonView");
    _compid->SetOrigID(POLYGON_COMP);

    if (!_release || graphic != nil) {
        _target = new PolygonComp(graphic);
        if (graphic != nil) {
            Append(_target);
        }
    }
}

ClassId IPolygonComp::GetSubstId(const char*& delim) {
    delim = polycomp_delim;
    return POLYGON_COMP;
}

ClassId IPolygonComp::GetClassId () { return IPOLYGON_COMP; }

boolean IPolygonComp::IsA (ClassId id) {
    return IPOLYGON_COMP == id || IComp::IsA(id);
}

/*****************************************************************************/

ClassId PolygonCode::GetClassId () { return IPOLYGON_CODE; }

boolean PolygonCode::IsA(ClassId id) {
    return IPOLYGON_CODE == id || GraphicCodeView::IsA(id);
}

PolygonCode::PolygonCode (IPolygonComp* subj) : GraphicCodeView(subj) {}

void PolygonCode::Update () {
    GraphicCodeView::Update();
    GetIPolygonComp()->Bequeath();
}

IPolygonComp* PolygonCode::GetIPolygonComp () {
    return (IPolygonComp*) GetSubject(); 
}

const char* PolygonCode::GetGHeader () { return "polygons"; }
const char* PolygonCode::GetCVHeader () { return "polygon"; }

boolean PolygonCode::Definition (ostream& out) {
    boolean ok = true;

    IPolygonComp* polygoncomp = GetIPolygonComp();
    PolygonComp* target = (PolygonComp*) polygoncomp->GetTarget();
    SF_Polygon* polygongr= target->GetPolygon();

    SubclassNameVar* cnamer = polygoncomp->GetCClassNameVar();
    SubclassNameVar* gnamer = polygoncomp->GetGClassNameVar();
    MemberNameVar* mnamer = polygoncomp->GetMemberNameVar();

    const char* mname = mnamer->GetName();
    const char* cname = cnamer->GetName();
    const char* gname = gnamer->GetName();

    if (_emitInstanceDecls || _emitGraphicState) {
	ok = ok && GraphicCodeView::Definition(out);

    } else if (_emitInstanceInits) {
        const Coord *x, *y;
        int count;

        count = polygongr->GetOriginal(x, y);

        out << "    {\n";
        out << "        Coord polyx[" << count << "];\n";
        out << "        Coord polyy[" << count << "];\n";

        for (int i = 0; i < count; i++) {
            out << "        polyx[" << i << "] = ";
            out << x[i] << ";\n";
            out << "        polyy[" << i << "] = ";
            out << y[i] << ";\n";
        }
        if (_emitGraphicComp) {
            out << "        " << mname << "_gr";
        } else {
            out << "        " << mname;
        }
        out << " = new " << gname << "(";
        out << "polyx, polyy, " << count;
        out << ");\n";
        ok = WriteGraphicInits(polygongr, out);
        if (_emitGraphicComp) {
            out << "        " << mname << " = new " << cname << "(";
            out << mname << "_gr);\n";
        }
        out << "    }\n";

    } else {
	ok = ok && GraphicCodeView::Definition(out);
    }
    return ok && out.good();
}

boolean PolygonCode::GCoreConstDecls(ostream& out) { 
    out << "(Coord* x, Coord* y, int count, Graphic* = nil);\n";
    return out.good();
}

boolean PolygonCode::GCoreConstInits(ostream& out) {
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* baseclass = gnamer->GetBaseClass();

    out <<"(\n    Coord* x, Coord* y, int count, Graphic* gr\n) : ";
    out << baseclass << "(x, y, count, gr) {}\n\n";

    return out.good();
}

boolean PolygonCode::GConstDecls(ostream& out) {
    out << "(Coord* x, Coord* y, int count, Graphic* = nil);\n";
    out << "    virtual Graphic* Copy();\n";
    return out.good();
}

boolean PolygonCode::GConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* gname = gnamer->GetName();

    out <<"(\n    Coord* x, Coord* y, int count, Graphic* gr\n) : ";
    out << coreclass << "(x, y, count, gr) {}\n\n";
    out << "Graphic* " << gname << "::Copy () {\n";
    out << "    return new " << gname << "(_x, _y, _count, this);\n";
    out << "}\n\n";

    return out.good();
}

