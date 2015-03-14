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
 * IRect component definitions.
 */


#include "ibrect.h"
#include "ibclasses.h"
#include "ibvars.h"
#include <Unidraw/Graphic/polygons.h>
#include <Unidraw/Tools/tool.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/
static const char* rectcomp_delim = "%rectcomp_delim";
/*****************************************************************************/

IRectComp::IRectComp (SF_Rect* graphic) {
    _gclassNameVar->SetName("SF_Rect");
    _gclassNameVar->SetBaseClass("SF_Rect");
    _cclassNameVar->SetName("RectComp");
    _cclassNameVar->SetBaseClass("RectComp");
    _vclassNameVar->SetName("RectView");
    _vclassNameVar->SetBaseClass("RectView");
    _compid->SetOrigID(RECT_COMP);

    if (!_release || graphic != nil) {
        _target = new RectComp(graphic);
        if (graphic != nil) {
            Append(_target);
        }
    }
}

ClassId IRectComp::GetClassId () { return IRECT_COMP; }

ClassId IRectComp::GetSubstId(const char*& delim) {
    delim = rectcomp_delim;
    return RECT_COMP;
}

boolean IRectComp::IsA (ClassId id) {
    return IRECT_COMP == id || IComp::IsA(id);
}

/*****************************************************************************/

IRectView::IRectView (IRectComp* subj) : IView(subj) {}

Manipulator* IRectView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;
    if (!tool->IsA(RESHAPE_TOOL)) {
        m = IView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

/*****************************************************************************/

ClassId RectCode::GetClassId () { return IRECT_CODE; }

boolean RectCode::IsA(ClassId id) {
    return IRECT_CODE == id || GraphicCodeView::IsA(id);
}

RectCode::RectCode (IRectComp* subj) : GraphicCodeView(subj) {}

void RectCode::Update () {
    GraphicCodeView::Update();
    GetIRectComp()->Bequeath();
}

IRectComp* RectCode::GetIRectComp () {
    return (IRectComp*) GetSubject(); 
}

const char* RectCode::GetGHeader () { return "polygons"; }
const char* RectCode::GetCVHeader () { return "rect"; }

boolean RectCode::Definition (ostream& out) {
    boolean ok = true;

    IRectComp* rectcomp = GetIRectComp();
    RectComp* target = (RectComp*) rectcomp->GetTarget();
    SF_Rect* rectgr= target->GetRect();

    SubclassNameVar* cnamer = rectcomp->GetCClassNameVar();
    SubclassNameVar* gnamer = rectcomp->GetGClassNameVar();
    MemberNameVar* mnamer = rectcomp->GetMemberNameVar();

    const char* mname = mnamer->GetName();
    const char* cname = cnamer->GetName();
    const char* gname = gnamer->GetName();

    if (_emitInstanceDecls || _emitGraphicState) {
	ok = ok && GraphicCodeView::Definition(out);

    } else if (_emitInstanceInits) {
        Coord x0, y0, x1, y1;
        rectgr->GetOriginal(x0, y0, x1, y1);

        out << "    {\n";
        if (_emitGraphicComp) {
            out << "        " << mname << "_gr";
        } else {
            out << "        " << mname;
        }
        out << " = new " << gname << "(";
        out << x0 << ", " << y0 << ", " << x1 << ", " << y1 << ");\n";
        ok = WriteGraphicInits(rectgr, out);
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

boolean RectCode::GCoreConstDecls(ostream& out) { 
    out << "(Coord x0, Coord y0, Coord x1, Coord y1, Graphic* = nil);\n";
    return out.good();
}

boolean RectCode::GCoreConstInits(ostream& out) {
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* baseclass = gnamer->GetBaseClass();

    out <<"(\n    Coord x0, Coord y0, Coord x1, Coord y1, Graphic* gr\n) : ";
    out << baseclass << "(x0, y0, x1, y1, gr) {}\n\n";

    return out.good();
}

boolean RectCode::GConstDecls(ostream& out) {
    out << "(Coord x0, Coord y0, Coord x1, Coord y1, Graphic* = nil);\n";
    out << "    virtual Graphic* Copy();\n";
    return out.good();
}

boolean RectCode::GConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* gname = gnamer->GetName();

    out <<"(\n    Coord x0, Coord y0, Coord x1, Coord y1, Graphic* gr\n) : ";
    out << coreclass << "(x0, y0, x1, y1, gr) {}\n\n";
    out << "Graphic* " << gname << "::Copy () {\n";
    out << "    return new " << gname << "(_x0, _y0, _x1, _y1, this);\n";
    out << "}\n\n";

    return out.good();
}


