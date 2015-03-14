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
 * Implementation of ILineComp and IMultiLineComp
 */

#include "ibline.h"
#include "ibclasses.h"
#include "ibvars.h"
#include <Unidraw/Graphic/lines.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/
static const char* lcomp_delim = "%lcomp_delim";
/*****************************************************************************/

ILineComp::ILineComp (Line* g) {
    _gclassNameVar->SetName("Line");
    _gclassNameVar->SetBaseClass("Line");
    _cclassNameVar->SetName("LineComp");
    _cclassNameVar->SetBaseClass("LineComp");
    _vclassNameVar->SetName("LineView");
    _vclassNameVar->SetBaseClass("LineView");
    _compid->SetOrigID(LINE_COMP);

    if (!_release || g != nil) {
        _target = new LineComp(g);
        if (g != nil) {
            Append(_target);
        }
    }
}

ClassId ILineComp::GetSubstId(const char*& delim) {
    delim = lcomp_delim;
    return LINE_COMP;
}

ClassId ILineComp::GetClassId () { return ILINE_COMP; }

boolean ILineComp::IsA (ClassId id) {
    return ILINE_COMP == id || IComp::IsA(id);
}

/*****************************************************************************/

ClassId LineCode::GetClassId () { return ILINE_CODE; }

boolean LineCode::IsA(ClassId id) {
    return ILINE_CODE == id || GraphicCodeView::IsA(id);
}

LineCode::LineCode (ILineComp* subj) : GraphicCodeView(subj) {}

void LineCode::Update () {
    GraphicCodeView::Update();
    GetILineComp()->Bequeath();
}

ILineComp* LineCode::GetILineComp () {
    return (ILineComp*) GetSubject(); 
}

const char* LineCode::GetGHeader () { return "lines"; }
const char* LineCode::GetCVHeader () { return "line"; }

boolean LineCode::Definition (ostream& out) {
    boolean ok = true;

    ILineComp* linecomp = GetILineComp();
    LineComp* target = (LineComp*) linecomp->GetTarget();
    Line* linegr= target->GetLine();

    SubclassNameVar* cnamer = linecomp->GetCClassNameVar();
    SubclassNameVar* gnamer = linecomp->GetGClassNameVar();
    MemberNameVar* mnamer = linecomp->GetMemberNameVar();

    const char* mname = mnamer->GetName();
    const char* cname = cnamer->GetName();
    const char* gname = gnamer->GetName();

    if (_emitInstanceDecls || _emitGraphicState) {
	ok = ok && GraphicCodeView::Definition(out);

    } else if (_emitInstanceInits) {
        Coord x0, y0, x1, y1;
        linegr->GetOriginal(x0, y0, x1, y1);

        out << "    {\n";
        if (_emitGraphicComp) {
            out << "        " << mname << "_gr";
        } else {
            out << "        " << mname;
        }
        out << " = new " << gname << "(";
        out << x0 << ", " << y0 << ", " << x1 << ", " << y1 << ");\n";
        ok = WriteGraphicInits(linegr, out);
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

boolean LineCode::GCoreConstDecls(ostream& out) { 
    out << "(\n";
    out << "        Coord x0, Coord y0, Coord x1, Coord y1,";
    out << " Graphic* gr = nil\n";
    out << "    );\n";
    return out.good();
}

boolean LineCode::GCoreConstInits(ostream& out) {
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* baseclass = gnamer->GetBaseClass();

    out << "(\n    Coord x0, Coord y0, Coord x1, Coord y1,";
    out << " Graphic* gr\n) : ";
    out << baseclass << "(x0, y0, x1, y1, gr) {}\n\n";

    return out.good();
}

boolean LineCode::GConstDecls(ostream& out) {
    out << "(\n";
    out << "        Coord x0, Coord y0, Coord x1, Coord y1,";
    out << " Graphic* gr = nil\n";
    out << "    );\n";
    out << "    virtual Graphic* Copy();\n";
    return out.good();
}

boolean LineCode::GConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* gname = gnamer->GetName();

    out << "(\n    Coord x0, Coord y0, Coord x1, Coord y1,";
    out << " Graphic* gr\n) : ";
    out << coreclass << "(x0, y0, x1, y1, gr) {}\n\n";
    out << "Graphic* " << gname << "::Copy () {\n";
    out << "    return new " << gname << "(_x0, _y0, _x1, _x2, this);\n";
    out << "}\n\n";

    return out.good();
}

/****************************************************************************/
static const char* mlcomp_delim = "%mlcomp_delim";
/*****************************************************************************/

IMultiLineComp::IMultiLineComp (SF_MultiLine* g) {
    _gclassNameVar->SetName("SF_MultiLine");
    _gclassNameVar->SetBaseClass("SF_MultiLine");
    _cclassNameVar->SetName("MultiLineComp");
    _cclassNameVar->SetBaseClass("MultiLineComp");
    _vclassNameVar->SetName("MultiLineView");
    _vclassNameVar->SetBaseClass("MultiLineView");
    _compid->SetOrigID(MULTILINE_COMP);

    if (!_release || g != nil) {
        _target = new MultiLineComp(g);
        if (g != nil) {
            Append(_target);
        }
    }
}

ClassId IMultiLineComp::GetSubstId(const char*& delim) {
    delim = mlcomp_delim;
    return MULTILINE_COMP;
}

ClassId IMultiLineComp::GetClassId () { return IMULTILINE_COMP; }

boolean IMultiLineComp::IsA (ClassId id) {
    return IMULTILINE_COMP == id || ILineComp::IsA(id);
}

/*****************************************************************************/

ClassId MultiLineCode::GetClassId () { return IMULTILINE_CODE; }

boolean MultiLineCode::IsA(ClassId id) {
    return IMULTILINE_CODE == id || LineCode::IsA(id);
}

MultiLineCode::MultiLineCode (
    IMultiLineComp* subj
) : LineCode(subj) {}

void MultiLineCode::Update () {
    LineCode::Update();
    GetIMultiLineComp()->Bequeath();
}

IMultiLineComp* MultiLineCode::GetIMultiLineComp () {
    return (IMultiLineComp*) GetSubject(); 
}

boolean MultiLineCode::Definition (ostream& out) {
    boolean ok = true;

    IMultiLineComp* mlinecomp = GetIMultiLineComp();
    MultiLineComp* target = (MultiLineComp*) mlinecomp->GetTarget();
    SF_MultiLine* Mlinegr= target->GetMultiLine();

    SubclassNameVar* cnamer = mlinecomp->GetCClassNameVar();
    SubclassNameVar* gnamer = mlinecomp->GetGClassNameVar();
    MemberNameVar* mnamer = mlinecomp->GetMemberNameVar();

    const char* mname = mnamer->GetName();
    const char* cname = cnamer->GetName();
    const char* gname = gnamer->GetName();

    if (_emitInstanceDecls || _emitGraphicState) {
	ok = ok && GraphicCodeView::Definition(out);

    } else if (_emitInstanceInits) {
        const Coord *x, *y;
        int count;

        count = Mlinegr->GetOriginal(x, y);

        out << "    {\n";
        out << "        Coord mlx[" << count << "];\n";
        out << "        Coord mly[" << count << "];\n";

        for (int i = 0; i < count; i++) {
            out << "        mlx[" << i << "] = ";
            out << x[i] << ";\n";
            out << "        mly[" << i << "] = ";
            out << y[i] << ";\n";
        }
        if (_emitGraphicComp) {
            out << "        " << mname << "_gr";
        } else {
            out << "        " << mname;
        }
        out << " = new " << gname << "(";
        out << "mlx, mly, " << count << ");\n";
        ok = WriteGraphicInits(Mlinegr, out);
        if (_emitGraphicComp) {
            out << "        " << mname << " = new " << cname << "(";
            out << mname << "_gr);\n";
        }
        out << "    }\n";

    } else {
	ok = ok && LineCode::Definition(out);
    }
    return ok && out.good();
}

boolean MultiLineCode::GCoreConstDecls(ostream& out) { 
    out << "(\n";
    out << "        Coord* x, Coord* y, int count, ";
    out << "Graphic* gr = nil\n";
    out << "    );\n";
    return out.good();
}

boolean MultiLineCode::GCoreConstInits(ostream& out) {
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* baseclass = gnamer->GetBaseClass();

    out << "(\n    Coord* x, Coord* y, int count, ";
    out << "Graphic* gr\n) : ";
    out << baseclass << "(x, y, count, gr) {}\n\n";

    return out.good();
}

boolean MultiLineCode::GConstDecls(ostream& out) {
    out << "(\n";
    out << "        Coord* x, Coord* y, int count, ";
    out << "Graphic* gr = nil\n";
    out << "    );\n";
    out << "    virtual Graphic* Copy();\n";
    return out.good();
}

boolean MultiLineCode::GConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* gname = gnamer->GetName();

    out << "(\n    Coord* x, Coord* y, int count, ";
    out << "Graphic* gr\n) : ";
    out << coreclass << "(x, y, count, gr) {}\n\n";
    out << "Graphic* " << gname << "::Copy () {\n";
    out << "    return new " << gname << "(_x, _y, _count, this);\n";
    out << "}\n\n";

    return out.good();
}

