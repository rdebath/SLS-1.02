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
 * Implementation of Deck component and derived classes.
 */

#include "ibclasses.h"
#include "ibcmds.h"
#include "ibdeck.h"
#include "ibvars.h"

#include <Unidraw/iterator.h>
#include <Unidraw/ulist.h>
#include <Unidraw/Graphic/graphic.h>
#include <Unidraw/Graphic/picture.h>

#include <InterViews/shape.h>
#include <InterViews/transformer.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/

DeckComp::DeckComp () {
    GetClassNameVar()->SetName("Deck");
    GetClassNameVar()->SetBaseClass("Deck");
}

ClassId DeckComp::GetClassId () { return DECK_COMP; }
boolean DeckComp::IsA (ClassId id){return DECK_COMP==id || SceneComp::IsA(id);}

void DeckComp::Resize () {
    Iterator i;
    float cx, cy;
    GetGraphic()->GetCenter(cx, cy);
    CanvasVar* cvar = GetCanvasVar();
    int xmax = cvar->Width();
    int ymax = cvar->Height();
    PointObj lb(round(cx - xmax/2), round(cy - ymax/2));

    for (First(i); !Done(i); Next(i)) {
        Shape* s = GetIComp(i)->GetShapeVar()->GetShape();
        int l, r, b, t;
        int width = xmax+1;
        width = max(width, s->width - s->hshrink);
        width = min(width, s->width + s->hstretch);
        int height = ymax+1;
        height = max(height, s->height - s->vshrink);
        height = min(height, s->height + s->vstretch);
        l = (xmax+1-width)/2; r = xmax - l;
        b = (ymax+1-height)/2; t = ymax - b;
	l += lb._x;
	b += lb._y;
	r += lb._x;
	t += lb._y;
        Place(GetIComp(i), l, b, r, t);
    }
}

void DeckComp::Reconfig () {
    int hnat = 0, hmin = 0, hmax = hfil;
    int vnat = 0, vmin = 0, vmax = vfil;
    Iterator i;

    for (First(i); !Done(i); Next(i)) {
        GetIComp(i)->Reconfig();
    }
    for (First(i); !Done(i); Next(i)) {
        Shape* s = GetIComp(i)->GetShapeVar()->GetShape();
        hnat = max(hnat, s->width);
        hmin = max(hmin, s->width - s->hshrink);
        hmax = min(hmax, s->width + s->hstretch);
        vnat = max(vnat, s->height);
        vmin = max(vmin, s->height - s->vshrink);
        vmax = min(vmax, s->height + s->vstretch);
    }
    Shape* shape = GetShapeVar()->GetShape();
    shape->width = hnat;
    shape->hshrink = max(0, shape->width - hmin);
    shape->hstretch = max(0, hmax - shape->width);
    shape->height = vnat;
    shape->vshrink = max(0, shape->height - vmin);
    shape->vstretch = max(0, vmax - shape->height);
}

/*************************************************************************/

DeckView::DeckView (DeckComp* subj) : SceneView(subj) { }
DeckComp* DeckView::GetDeckComp () { return (DeckComp*) GetSubject();}
ClassId DeckView::GetClassId () { return DECK_VIEW; }

boolean DeckView::IsA (ClassId id) {
    return DECK_VIEW == id || SceneView::IsA(id);
}

/*****************************************************************************/

ClassId DeckCode::GetClassId () { return DECK_CODE; }
boolean DeckCode::IsA (ClassId id) {return DECK_CODE==id || CodeView::IsA(id);}
DeckCode::DeckCode (DeckComp* subj) : CodeView(subj) { }
DeckComp* DeckCode::GetDeckComp () { return (DeckComp*) GetSubject(); }

void DeckCode::Update () {
    CodeView::Update();
    InteractorComp* subj = GetIntComp();
    Graphic* gr = subj->GetGraphic();
    gr->SetColors(nil, nil);
    gr->SetFont(nil);
}    

boolean DeckCode::Definition (ostream& out) {
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
            if (_scope && mnamer->GetExport() && !_namelist->Search("deck")) {
                _namelist->Append("deck");
                out << "#include <InterViews/deck.h>\n";
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
            if (!_namelist->Search("deck")) {
                _namelist->Append("deck");
                out << "#include <InterViews/deck.h>\n";
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
            out << "(";
            InstanceName(out, ")");
	    EndInstantiate(out);
	}

        ok = ok && Iterate(out);
	if (AllKidsDefined() && !_lock) {
	    _lock = true;
            for (icomp->Last(i); !icomp->Done(i); icomp->Prev(i)) {
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

boolean DeckCode::CoreConstDecls(ostream& out) { 
    out << "(const char*);\n";
    return out.good();
}

boolean DeckCode::CoreConstInits(ostream& out) {
    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* baseclass = snamer->GetBaseClass();
    const char* subclass = snamer->GetName();

    out << "(\n    const char* name\n) : " << baseclass;
    out << "(name) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "}\n\n";
    return out.good();
}

boolean DeckCode::ConstDecls(ostream& out) {
    out << "(const char*);\n";
    return out.good();
}

boolean DeckCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name\n) : " << coreclass;
    out << "(name) {}\n\n";
    return out.good();
}

boolean DeckCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("deck")) {
        _namelist->Append("deck");
        out << "#include <InterViews/deck.h> \n";
    }
    return out.good();
}

