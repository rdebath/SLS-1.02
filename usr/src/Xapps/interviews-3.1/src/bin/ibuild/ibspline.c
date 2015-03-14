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
 * Implementation of ISplineComp
 */

#include "ibspline.h"
#include "ibclasses.h"
#include "ibvars.h"
#include <Unidraw/Graphic/splines.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/
static const char* spcomp_delim = "%spcomp_delim";
/*****************************************************************************/

ISplineComp::ISplineComp (SFH_OpenBSpline* g) {
    _gclassNameVar->SetName("SFH_OpenBSpline");
    _gclassNameVar->SetBaseClass("SFH_OpenBSpline");
    _cclassNameVar->SetName("SplineComp");
    _cclassNameVar->SetBaseClass("SplineComp");
    _vclassNameVar->SetName("SplineView");
    _vclassNameVar->SetBaseClass("SplineView");
    _compid->SetOrigID(SPLINE_COMP);

    if (!_release || g != nil) {
        _target = new SplineComp(g);
        if (g != nil) {
            Append(_target);
        }
    }
}

ClassId ISplineComp::GetSubstId(const char*& delim) {
    delim = spcomp_delim;
    return SPLINE_COMP;
}

ClassId ISplineComp::GetClassId () { return ISPLINE_COMP; }

boolean ISplineComp::IsA (ClassId id) {
    return ISPLINE_COMP == id || IComp::IsA(id);
}

/*****************************************************************************/

ClassId SplineCode::GetClassId () { return ISPLINE_CODE; }

boolean SplineCode::IsA(ClassId id) {
    return ISPLINE_CODE == id || GraphicCodeView::IsA(id);
}

SplineCode::SplineCode (
    ISplineComp* subj
) : GraphicCodeView(subj) {}

void SplineCode::Update () {
    GraphicCodeView::Update();
    GetISplineComp()->Bequeath();
}

ISplineComp* SplineCode::GetISplineComp () {
    return (ISplineComp*) GetSubject(); 
}

const char* SplineCode::GetGHeader () { return "splines"; }
const char* SplineCode::GetCVHeader () { return "spline"; }

boolean SplineCode::Definition (ostream& out) {
    boolean ok = true;

    ISplineComp* splinecomp = GetISplineComp();
    SplineComp* target = (SplineComp*) splinecomp->GetTarget();
    SFH_OpenBSpline* splinegr= target->GetSpline();

    SubclassNameVar* cnamer = splinecomp->GetCClassNameVar();
    SubclassNameVar* gnamer = splinecomp->GetGClassNameVar();
    MemberNameVar* mnamer = splinecomp->GetMemberNameVar();

    const char* mname = mnamer->GetName();
    const char* cname = cnamer->GetName();
    const char* gname = gnamer->GetName();

    if (_emitInstanceDecls || _emitGraphicState) {
	ok = ok && GraphicCodeView::Definition(out);

    } else if (_emitInstanceInits) {
        const Coord *x, *y;
        int count;

        count = splinegr->GetOriginal(x, y);

        out << "    {\n";
        out << "        Coord sx[" << count << "];\n";
        out << "        Coord sy[" << count << "];\n";

        for (int i = 0; i < count; i++) {
            out << "        sx[" << i << "] = ";
            out << x[i] << ";\n";
            out << "        sy[" << i << "] = ";
            out << y[i] << ";\n";
        }
        if (_emitGraphicComp) {
            out << "        " << mname << "_gr";
        } else {
            out << "        " << mname;
        }
        out << " = new " << gname << "(";
        out << "sx, sy, " << count <<");\n";
        ok = WriteGraphicInits(splinegr, out);
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

boolean SplineCode::GCoreConstDecls(ostream& out) { 
    out << "(\n";
    out << "        Coord* x, Coord* y, int count, ";
    out << "Graphic* gr = nil\n";
    out << "    );\n";
    return out.good();
}

boolean SplineCode::GCoreConstInits(ostream& out) {
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* baseclass = gnamer->GetBaseClass();

    out << "(\n    Coord* x, Coord* y, int count, ";
    out << "Graphic* gr\n) : ";
    out << baseclass << "(x, y, count, gr) {}\n\n";

    return out.good();
}

boolean SplineCode::GConstDecls(ostream& out) {
    out << "(Coord* x, Coord* y, int count, Graphic* = nil);\n";
    out << "    virtual Graphic* Copy();\n";
    return out.good();
}

boolean SplineCode::GConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* gname = gnamer->GetName();

    out << "(\n    Coord* x, Coord* y, int count, ";
    out << "Graphic* gr\n) : ";
    out << coreclass << "(x, y, count, gr) {}\n\n";
    out << "Graphic* " << gname << "::Copy () {\n";
    out << "    return new " << gname << "(_x, _y, count, this);\n";
    out << "}\n\n";

    return out.good();
}

/*****************************************************************************/
static const char* cspcomp_delim = "%cspcomp_delim";
/*****************************************************************************/

IClosedSplineComp::IClosedSplineComp (SFH_ClosedBSpline* graphic) {
    _gclassNameVar->SetName("SFH_ClosedBSpline");
    _gclassNameVar->SetBaseClass("SFH_ClosedBSpline");
    _cclassNameVar->SetName("ClosedSplineComp");
    _cclassNameVar->SetBaseClass("ClosedSplineComp");
    _vclassNameVar->SetName("ClosedSplineView");
    _vclassNameVar->SetBaseClass("ClosedSplineView");
    _compid->SetOrigID(CLOSEDSPLINE_COMP);

    if (!_release || graphic != nil) {
        _target = new ClosedSplineComp(graphic);
        if (graphic != nil) {
            Append(_target);
        }
    }
}

ClassId IClosedSplineComp::GetSubstId(const char*& delim) {
    delim = cspcomp_delim;
    return CLOSEDSPLINE_COMP;
}

ClassId IClosedSplineComp::GetClassId () { return ICLOSEDSPLINE_COMP; }

boolean IClosedSplineComp::IsA (ClassId id) {
    return ICLOSEDSPLINE_COMP == id || ISplineComp::IsA(id);
}

/*****************************************************************************/

ClassId ClosedSplineCode::GetClassId () { return ICLOSEDSPLINE_CODE; }

boolean ClosedSplineCode::IsA(ClassId id) {
    return ICLOSEDSPLINE_CODE == id || SplineCode::IsA(id);
}

ClosedSplineCode::ClosedSplineCode (
    IClosedSplineComp* subj
) : SplineCode(subj) {}

void ClosedSplineCode::Update () {
    SplineCode::Update();
    GetIClosedSplineComp()->Bequeath();
}

IClosedSplineComp* ClosedSplineCode::GetIClosedSplineComp () {
    return (IClosedSplineComp*) GetSubject(); 
}

boolean ClosedSplineCode::Definition (ostream& out) {
    boolean ok = true;

    IClosedSplineComp* csplinecomp = GetIClosedSplineComp();
    ClosedSplineComp* target = (ClosedSplineComp*) csplinecomp->GetTarget();
    SFH_ClosedBSpline* closedsplinegr= target->GetClosedSpline();

    SubclassNameVar* cnamer = csplinecomp->GetCClassNameVar();
    SubclassNameVar* gnamer = csplinecomp->GetGClassNameVar();
    MemberNameVar* mnamer = csplinecomp->GetMemberNameVar();

    const char* mname = mnamer->GetName();
    const char* cname = cnamer->GetName();
    const char* gname = gnamer->GetName();

    if (_emitInstanceDecls || _emitGraphicState) {
	ok = ok && GraphicCodeView::Definition(out);

    } else if (_emitInstanceInits) {
        const Coord *x, *y;
        int count;

        count = closedsplinegr->GetOriginal(x, y);

        out << "    {\n";
        out << "        Coord cbsx[" << count << "];\n";
        out << "        Coord cbsy[" << count << "];\n";

        for (int i = 0; i < count; i++) {
            out << "        cbsx[" << i << "] = ";
            out << x[i] << ";\n";
            out << "        cbsy[" << i << "] = ";
            out << y[i] << ";\n";
        }
        if (_emitGraphicComp) {
            out << "        " << mname << "_gr";
        } else {
            out << "        " << mname;
        }
        out << " = new " << gname << "(";
        out << "cbsx, cbsy, " << count;
        out << ");\n";
        ok = WriteGraphicInits(closedsplinegr, out);
        if (_emitGraphicComp) {
            out << "        " << mname << " = new " << cname << "(";
            out << mname << "_gr);\n";
        }
        out << "    }\n";

    } else {
	ok = ok && SplineCode::Definition(out);
    }
    return ok && out.good();
}

boolean ClosedSplineCode::GCoreConstDecls(ostream& out) { 
    out << "(Coord* x, Coord* y, int count, Graphic* = nil);\n";
    return out.good();
}

boolean ClosedSplineCode::GCoreConstInits(ostream& out) {
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* baseclass = gnamer->GetBaseClass();

    out <<"(\n    Coord* x, Coord* y, int count, Graphic* gr\n) : ";
    out << baseclass << "(x, y, count, gr) {}\n\n";

    return out.good();
}

boolean ClosedSplineCode::GConstDecls(ostream& out) {
    out << "(Coord* x, Coord* y, int count, Graphic* = nil);\n";
    out << "    virtual Graphic* Copy();\n";
    return out.good();
}

boolean ClosedSplineCode::GConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* gname = gnamer->GetName();

    out <<"(\n    Coord* x, Coord* y, int count, Graphic* gr\n) : ";
    out << coreclass << "(x, y, count, gr) {}\n\n";
    out << "Graphic* " << gname << "::Copy () {\n";
    out << "    return new " << gname << "(_x, _y, count, this);\n";
    out << "}\n\n";

    return out.good();
}

