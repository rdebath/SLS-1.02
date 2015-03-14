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
 * Implementation of Box component and derived classes.
 */

#include "ibbox.h"
#include "ibclasses.h"
#include "ibvars.h"

#include <Unidraw/iterator.h>
#include <Unidraw/ulist.h>
#include <Unidraw/Graphic/graphic.h>
#include <InterViews/shape.h>
#include <string.h>
#include <stream.h>
 
/*****************************************************************************/

struct BoxDimension {
    int natural, stretch, shrink;
};

struct BoxCanonical {
    BoxDimension major, minor;
};

/*****************************************************************************/

BoxComp::BoxComp () { }
ClassId BoxComp::GetClassId () { return BOX_COMP; }
boolean BoxComp::IsA (ClassId id) { return BOX_COMP==id || SceneComp::IsA(id);}

void BoxComp::Resize () {
    Shape aggrshape;		// combined shape of components 
    BoxCanonical total;		// components' shape along major axis 
    int major, minor;		// actual dimensions of box 
    int have, need;		// how much box is willing/needs to change 
    boolean grow;		// true if stretching, false if shrinking 
    BoxCanonical s;		// element shape along major axis 
    int pos, len;		// pos and size of next elem on major axis 

    CanvasVar* cvar = GetCanvasVar();
    float cx, cy, w = float(cvar->Width()), h = float(cvar->Height());
    GetGraphic()->GetCenter(cx, cy);
    PointObj lb(round(cx - w/2), round(cy - h/2));
    
    ComputeShape(&aggrshape);
    GetActual(major, minor);
    GetCanonical(&aggrshape, total);
    int n = total.major.natural;
    if (major > n) {
	grow = true;            // more space than desired ==> stretch elements
	have = total.major.stretch;
	need = min(major - n, have);
    } else {
	grow = false;           // less space than desired ==> shrink elements
	have = total.major.shrink;
	need = min(n - major, have);
    }
    pos = 0;
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        InteractorComp* kid = GetIComp(i);
	GetCanonical(kid->GetShapeVar()->GetShape(), s);
	len = s.major.natural;
	if (have > 0) {
	    if (grow) {
		n = int(double(s.major.stretch)*double(need)/double(have));
		len += n;
		have -= s.major.stretch;
	    } else {
		n = int(double(s.major.shrink)*double(need)/double(have));
		len -= n;
		have -= s.major.shrink;
	    }
	    need -= n;
	}
	n = s.minor.natural;
	if (n == 0) {
	    n = minor;
	} else if (n > minor) {
	    n = max(n - s.minor.shrink, minor);
	} else if (n < minor) {
	    n = min(n + s.minor.stretch, minor);
	}
        PlaceElement(kid, pos, len, n, lb);
	pos += len;
    }
}

void BoxComp::Reconfig () {
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        GetIComp(i)->Reconfig();
    }
    ComputeShape(GetShapeVar()->GetShape()); 
}

void BoxComp::ComputeShape (Shape*) { }
void BoxComp::GetActual (int&, int&) { }
void BoxComp::GetCanonical (Shape*, struct BoxCanonical&) { }
void BoxComp::PlaceElement (InteractorComp*, Coord, int, int, PointObj&) {}

/*****************************************************************************/

HBoxComp::HBoxComp () {
    GetClassNameVar()->SetName("HBox"); 
    GetClassNameVar()->SetBaseClass("HBox"); 
}

ClassId HBoxComp::GetClassId () { return HBOX_COMP; }
boolean HBoxComp::IsA (ClassId id) { return HBOX_COMP==id || BoxComp::IsA(id);}

void HBoxComp::ComputeShape (Shape* box) {
    box->width = 0;
    box->height = 0;
    box->Rigid(0, 0, vfil, vfil);
    int vmin = -vfil;
    int vmax = vfil;
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        InteractorComp* kid = GetIComp(i);
	Shape* s = kid->GetShapeVar()->GetShape();

	box->width += s->width;
	box->height = max(box->height, s->height);
	box->hstretch += s->hstretch;
	box->hshrink += s->hshrink;
	vmin = max(s->height - s->vshrink, vmin);
	vmax = min(s->height + s->vstretch, vmax);
    }
    box->vstretch = max(0, vmax - box->height);
    box->vshrink = max(0, box->height - vmin);
}

void HBoxComp::GetActual (int& major, int& minor) {
    major = GetCanvasVar()->Width();
    minor = GetCanvasVar()->Height();
}

void HBoxComp::GetCanonical (Shape* s, BoxCanonical& b) {
    b.major.natural = s->width;
    b.major.shrink = s->hshrink;
    b.major.stretch = s->hstretch;
    b.minor.natural = s->height;
    b.minor.shrink = s->vshrink;
    b.minor.stretch = s->vstretch;
}

void HBoxComp::PlaceElement (
    InteractorComp* comp, Coord x, int length, int h, PointObj& lb
) {
    Coord x1, y1, x2, y2;

    x1 = lb._x + x;
    x2 = x1 + length - 1;
    y1 = lb._y;
    y2 = y1 + h - 1;
    Place(comp, x1, y1, x2, y2);
}

/*****************************************************************************/

VBoxComp::VBoxComp () {
    GetClassNameVar()->SetName("VBox"); 
    GetClassNameVar()->SetBaseClass("VBox"); 
}

ClassId VBoxComp::GetClassId () { return VBOX_COMP; }
boolean VBoxComp::IsA (ClassId id) { return VBOX_COMP==id || BoxComp::IsA(id);}

void VBoxComp::ComputeShape (Shape* box) {
    box->width = 0;
    box->height = 0;
    box->Rigid(hfil, hfil, 0, 0);
    int hmin = -hfil;
    int hmax = hfil;
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        InteractorComp* kid = GetIComp(i);
	Shape* s = kid->GetShapeVar()->GetShape();

        box->width = max(box->width, s->width);
        box->height += s->height;
        box->vstretch += s->vstretch;
        box->vshrink += s->vshrink;
        hmin = max(s->width - s->hshrink, hmin);
        hmax = min(s->width + s->hstretch, hmax);
    }
    box->hstretch = max(0, hmax - box->width);
    box->hshrink = max(0, box->width - hmin);
}

void VBoxComp::GetActual (int& major, int& minor) {
    major = GetCanvasVar()->Height();
    minor = GetCanvasVar()->Width();
}

void VBoxComp::GetCanonical (Shape* s, BoxCanonical& b) {
    b.major.natural = s->height;
    b.major.shrink = s->vshrink;
    b.major.stretch = s->vstretch;
    b.minor.natural = s->width;
    b.minor.shrink = s->hshrink;
    b.minor.stretch = s->hstretch;
}

void VBoxComp::PlaceElement (
    InteractorComp* comp, Coord y, int length, int w, PointObj& lb
) {
    Coord x1, y1, x2, y2;

    x1 = lb._x;
    y2 = lb._y + GetCanvasVar()->ymax() - y;
    x2 = x1 + w - 1;
    y1 = y2 - length + 1;
    Place(comp, x1, y1, x2, y2);
}

/*****************************************************************************/

ClassId BoxCode::GetClassId () { return BOX_CODE; }
boolean BoxCode::IsA (ClassId id) { return BOX_CODE ==id || CodeView::IsA(id);}
BoxCode::BoxCode (BoxComp* subj) : CodeView(subj) { }
BoxComp* BoxCode::GetBoxComp () { return (BoxComp*) GetSubject(); }

void BoxCode::Update () {
    CodeView::Update();
    InteractorComp* subj = GetIntComp();
    Graphic* gr = subj->GetGraphic();
    gr->SetColors(nil, nil);
    gr->SetFont(nil);
}

boolean BoxCode::Definition (ostream& out) {
    boolean ok = true;
    Iterator i;

    if (
        _emitInstanceDecls || _emitForward || _emitProperty ||
        _emitClassHeaders || _emitHeaders
    ) {
	ok = ok && CodeView::Definition(out);
        ok = ok && Iterate(out);

    } else if (_emitExpHeader) {
	InteractorComp* icomp = GetIntComp();
	MemberNameVar* mnamer = icomp->GetMemberNameVar();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        if (!snamer->IsSubclass()) {
            if (_scope && mnamer->GetExport() && !_namelist->Search("box")) {
                _namelist->Append("box");
                out << "#include <InterViews/box.h>\n";
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }
        ok = ok && Iterate(out);

    } else if (_emitCorehHeader) {
	InteractorComp* icomp = GetIntComp();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        const char* subclass = snamer->GetName();
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("box")) {
                _namelist->Append("box");
                out << "#include <InterViews/box.h>\n";
            }
        } else {
            ok = ok && Iterate(out);
        }
    } else if (_emitInstanceInits) {
	InteractorComp* icomp = GetIntComp();
        const char* mname = icomp->GetMemberNameVar()->GetName();

	if (!_instancelist->Find((void*) mname)) {
	    _instancelist->Append(new UList((void*)mname));
		
	    BeginInstantiate(out);
            out << "();\n"; 
	}

        ok = ok && Iterate(out);

	if (AllKidsDefined() && !_lock) {
	    _lock = true;
	    for (icomp->First(i); !icomp->Done(i); icomp->Next(i)) {
		InteractorComp* kid = (InteractorComp*) icomp->GetComp(i);
		const char* instance = kid->GetMemberNameVar()->GetName();
                out << "    " << mname << "->Insert(";
                out << instance << ");\n";	
            }
	}

    } else if (
	_emitBSDecls || _emitBSInits || 
	_emitFunctionDecls || _emitFunctionInits ||
        _emitCreatorHeader || _emitCreatorSubj || _emitCreatorView
    ) {
        ok = ok && Iterate(out);
    } else if (
        _emitCoreDecls || _emitCoreInits || _emitClassDecls || _emitClassInits
    ) {
	ok = ok && CodeView::Definition(out);
        ok = ok && Iterate(out);
        
    } else if (_emitMain) {
	ok = ok && CodeView::Definition(out);
        
    }
    return ok;
}

boolean BoxCode::CoreConstDecls(ostream& out) { 
    out << "();\n";
    return out.good();
}

boolean BoxCode::CoreConstInits(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();
    const char* subclass = snamer->GetName();

    out << "() {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "}\n\n";
    return out.good();
}

boolean BoxCode::ConstDecls(ostream& out) {
    out << "();\n";
    return out.good();
}

boolean BoxCode::ConstInits(ostream& out) {
    out << "() {}\n\n";
    return out.good();
}

boolean BoxCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("box")) {
        _namelist->Append("box");
        out << "#include <InterViews/box.h> \n";
    }
    return out.good();
}
