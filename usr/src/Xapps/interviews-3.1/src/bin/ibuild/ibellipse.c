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
 * IEllipse component definitions.
 */


#include "ibellipse.h"
#include "ibclasses.h"
#include "ibvars.h"
#include <Unidraw/Graphic/ellipses.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/
static const char* ellcomp_delim = "%ellcomp_delim";
/*****************************************************************************/

IEllipseComp::IEllipseComp (SF_Ellipse* graphic) {
    _gclassNameVar->SetName("SF_Ellipse");
    _gclassNameVar->SetBaseClass("SF_Ellipse");
    _cclassNameVar->SetName("EllipseComp");
    _cclassNameVar->SetBaseClass("EllipseComp");
    _vclassNameVar->SetName("EllipseView");
    _vclassNameVar->SetBaseClass("EllipseView");
    _compid->SetOrigID(ELLIPSE_COMP);

    if (!_release || graphic != nil) {
        _target = new EllipseComp(graphic);
        if (graphic != nil) {
            Append(_target);
        }
    }
}

ClassId IEllipseComp::GetSubstId(const char*& delim) {
    delim = ellcomp_delim;
    return ELLIPSE_COMP;
}

ClassId IEllipseComp::GetClassId () { return IELLIPSE_COMP; }

boolean IEllipseComp::IsA (ClassId id) {
    return IELLIPSE_COMP == id || IComp::IsA(id);
}


/*****************************************************************************/

ClassId EllipseCode::GetClassId () { return IELLIPSE_CODE; }

boolean EllipseCode::IsA(ClassId id) {
    return IELLIPSE_CODE == id || GraphicCodeView::IsA(id);
}

EllipseCode::EllipseCode (IEllipseComp* subj) : GraphicCodeView(subj) {}

void EllipseCode::Update () {
    GraphicCodeView::Update();
    GetIEllipseComp()->Bequeath();
}

IEllipseComp* EllipseCode::GetIEllipseComp () {
    return (IEllipseComp*) GetSubject(); 
}

const char* EllipseCode::GetGHeader () { return "ellipses"; }
const char* EllipseCode::GetCVHeader () { return "ellipse"; }

boolean EllipseCode::Definition (ostream& out) {
    boolean ok = true;

    IEllipseComp* ellipsecomp = GetIEllipseComp();
    EllipseComp* target = (EllipseComp*) ellipsecomp->GetTarget();
    SF_Ellipse* ellipsegr= target->GetEllipse();

    SubclassNameVar* cnamer = ellipsecomp->GetCClassNameVar();
    SubclassNameVar* gnamer = ellipsecomp->GetGClassNameVar();
    MemberNameVar* mnamer = ellipsecomp->GetMemberNameVar();

    const char* mname = mnamer->GetName();
    const char* cname = cnamer->GetName();
    const char* gname = gnamer->GetName();

    if (_emitInstanceDecls || _emitGraphicState) {
	ok = ok && GraphicCodeView::Definition(out);

    } else if (_emitInstanceInits) {
        Coord x0, y0;
        int r1, r2;
        ellipsegr->GetOriginal(x0, y0, r1, r2);

        out << "    {\n";
        if (_emitGraphicComp) {
            out << "        " << mname << "_gr";
        } else {
            out << "        " << mname;
        }
        out << " = new " << gname << "(";
        out << x0 << ", " << y0 << ", " << r1 << ", " << r2 << ");\n";
        ok = WriteGraphicInits(ellipsegr, out);
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

boolean EllipseCode::GCoreConstDecls(ostream& out) { 
    out << "(Coord x0, Coord y0, int r1, int r2, Graphic* = nil);\n";
    return out.good();
}

boolean EllipseCode::GCoreConstInits(ostream& out) {
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* baseclass = gnamer->GetBaseClass();

    out <<"(\n    Coord x0, Coord y0, int r1, int r2, Graphic* gr\n) : ";
    out << baseclass << "(x0, y0, r1, r2, gr) {}\n\n";

    return out.good();
}

boolean EllipseCode::GConstDecls(ostream& out) {
    out << "(Coord x0, Coord y0, int r1, int r2, Graphic* = nil);\n";
    out << "    virtual Graphic* Copy();\n";
    return out.good();
}

boolean EllipseCode::GConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* gname = gnamer->GetName();

    out <<"(\n    Coord x0, Coord y0, int r1, int r2, Graphic* gr\n) : ";
    out << coreclass << "(x0, y0, r1, r2, gr) {}\n\n";
    out << "Graphic* " << gname << "::Copy () {\n";
    out << "    return new " << gname << "(_x0, _y0, _r1, _r2, this);\n";
    out << "}\n\n";

    return out.good();
}


