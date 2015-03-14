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
 * Message component definitions.
 */

#include "ibmessage.h"
#include "ibclasses.h"
#include "ibcmds.h"
#include "ibed.h"
#include "ibglobals.h"
#include "ibtools.h"
#include "ibvars.h"

#include <Unidraw/catalog.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/creator.h>
#include <Unidraw/manips.h>
#include <Unidraw/statevars.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/ulist.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/align.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/macro.h>
#include <Unidraw/Graphic/pspaint.h>
#include <Unidraw/Tools/reshape.h>
#include <Unidraw/Tools/tool.h>

#include <InterViews/painter.h>
#include <InterViews/shape.h>
#include <InterViews/transformer.h>

#include <stream.h>
#include <string.h>

/*****************************************************************************/

ClassId MessageComp::GetClassId () { return MESSAGE_COMP; }

boolean MessageComp::IsA (ClassId id) {
    return MESSAGE_COMP == id || InteractorComp::IsA(id);
}

MessageComp::MessageComp (MessageGraphic* g) : InteractorComp(g) { 
    if (g != nil) {
        GetClassNameVar()->SetName("Message");
        GetClassNameVar()->SetBaseClass("Message");
        IBShape* ibshape = GetShapeVar()->GetShape();
        ibshape->hstr = ibshape->vstr = true;
        ibshape->hstretch = 0;
        ibshape->vstretch = 0;
    }
}

MessageGraphic* MessageComp::GetMessageGraphic () {
    return (MessageGraphic*) GetGraphic();
}

InteractorComp& MessageComp::operator = (InteractorComp& comp) { 
    InteractorComp::operator = (comp);
    return *this;
}

void MessageComp::Interpret(Command* cmd) {
    if (cmd->IsA(EDIT_CMD)) {
        EditCmd* editcmd = (EditCmd*) cmd;
        editcmd->SetOldText(GetMessageGraphic()->GetText());
        GetMessageGraphic()->SetText(editcmd->GetNewText());
        Reconfig();
        StoreCanvas(cmd);
        Place(this);
        Propagate(cmd);

    } else if (cmd->IsA(ALIGN_CMD)) {
        AlignCmd* alignCmd = (AlignCmd*) cmd;
        Alignment a;
	MessageGraphic* mg = GetMessageGraphic();
	cmd->Store(this, new VoidData((void*) mg->GetAlignment()));
        alignCmd->GetAlignment(a, a);
	GetMessageGraphic()->SetAlignment(a);
        Notify();
        Propagate(cmd);

    } else {
	InteractorComp::Interpret(cmd);
    }
}

void MessageComp::Uninterpret(Command* cmd) {
    if (cmd->IsA(EDIT_CMD)) {
        EditCmd* editcmd = (EditCmd*) cmd;
        editcmd->SetNewText(GetMessageGraphic()->GetText());
        GetMessageGraphic()->SetText(editcmd->GetOldText());
        Reconfig();
        RestoreCanvas(cmd);
        Unpropagate(cmd);

    } else if (cmd->IsA(ALIGN_CMD)) {
        VoidData* vd = (VoidData*) cmd->Recall(this);
	Alignment a = (Alignment) vd->_void;
	GetMessageGraphic()->SetAlignment(a);
        Notify();
        Unpropagate(cmd);

    } else {
        InteractorComp::Uninterpret(cmd);
    }
}

void MessageComp::Reconfig () {
    Shape* shape = GetShapeVar()->GetShape();
    int w, h;
    GetMessageGraphic()->Natural(w, h);
    shape->width = w;
    shape->height = h;
    shape->hshrink = shape->vshrink = 0;
    GetShapeVar()->Notify();
}

void MessageComp::Read (istream& in) {
    Catalog* catalog = unidraw->GetCatalog();
    InteractorComp::Read(in);

    /* backward compability */

    IBShape* ibshape = GetShapeVar()->GetShape();
    ibshape->hstr = ibshape->vstr = true;

    ClassId id;
    in >> id;
    MessageGraphic* g = (MessageGraphic*) catalog->GetCreator()->Create(id);
    g->Read(in);
    g->SetCanvasVar(GetCanvasVar());
    SetGraphic(g);
}

void MessageComp::Write (ostream& out) {
    ClassId id;
    Catalog* catalog = unidraw->GetCatalog();
    InteractorComp::Write(out);

    MessageGraphic* g = GetMessageGraphic();
    id = g->GetClassId();
    out << " " << id << " ";
    g->Write(out);
}

/*****************************************************************************/

MessageView::MessageView (MessageComp* subj) : InteractorView(subj) { }
MessageComp* MessageView::GetMessageComp(){return (MessageComp*) GetSubject();}
ClassId MessageView::GetClassId () { return MESSAGE_VIEW; }

boolean MessageView::IsA (ClassId id) {
    return MESSAGE_VIEW == id || InteractorView::IsA(id);
}

void MessageView::Update () {
    MessageGraphic* bcomp = GetMessageComp()->GetMessageGraphic();
    MessageGraphic* bview = (MessageGraphic*) GetGraphic();

    IncurDamage(bview);
    *(Graphic*) bview = *(Graphic*) bcomp;
    UpdateCanvasVar();
    bview->SetText(bcomp->GetText());
    bview->SetAlignment(bcomp->GetAlignment());
    IncurDamage(bview);
    EraseHandles();
}

Painter* InitPainter (Graphic* g, Transformer* rel) {
    Painter* p = new Painter;
    p->SetFont(g->GetFont());
    p->FillBg(g->BgFilled());
    p->SetColors(g->GetFgColor(), g->GetBgColor());
    p->SetTransformer(rel);
    return p;
}

Manipulator* MessageView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;
    IBEditor* ed = (IBEditor*) v->GetEditor();

    if (tool->IsA(IBGRAPHIC_COMP_TOOL)) {
	m = InteractorView::CreateManipulator(v, e, rel, tool);

    } else if (tool->IsA(GRAPHIC_COMP_TOOL)) {
	FontVar* fontvar = (FontVar*) ed->GetState("FontVar");
	PSFont* font = fontvar->GetFont();
	ColorVar* colorvar = (ColorVar*) ed->GetState("ColorVar");
	PSColor* fg =  colorvar->GetFgColor();
	PSColor* bg =  colorvar->GetBgColor();
	Graphic* g = GetMessageComp()->GetGraphic();
	g->SetFont(font);
	g->SetColors(fg, bg);
        Painter* painter = InitPainter(g, rel);
        m = new TextManip(v, painter, 0, tool);

    } else if (tool->IsA(RESHAPE_TOOL)) {
        MessageGraphic* msg = (MessageGraphic*) GetGraphic();
        rel = new Transformer;
        Painter* painter = InitPainter(msg, rel);
        const char* text = msg->GetText();
        int size = strlen(text);
        
        msg->TotalTransformation(*rel);
        Coord xpos, ypos;
        msg->GetTextPosition(xpos, ypos, msg->GetFont());
        rel->Transform(xpos, ypos);
        m = new TextManip(v, text, size, xpos, ypos, painter, 0, tool);

    } else {
        m = InteractorView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* MessageView::InterpretManipulator (Manipulator* m) {
    IBEditor* ed = (IBEditor*) m->GetViewer()->GetEditor();
    Tool* tool = m->GetTool();
    Command* cmd = nil;

    if (tool->IsA(IBGRAPHIC_COMP_TOOL)) {
	cmd = InteractorView::InterpretManipulator(m);

    } else if (tool->IsA(GRAPHIC_COMP_TOOL) || tool->IsA(RESHAPE_TOOL)) {
        TextManip* tm = (TextManip*) m;
        int size;
        const char* text = tm->GetText(size);
        
        if (tool->IsA(GRAPHIC_COMP_TOOL)) {
            
            MessageGraphic* protogr = GetMessageComp()->GetMessageGraphic();
            MessageComp* msgComp = (MessageComp*) GetMessageComp()->Copy(); 
            MessageGraphic* msg = msgComp->GetMessageGraphic();
            msg->SetText(text);
            msg->Position(tm);
            cmd = new MacroCmd(
                ed, new PasteCmd(ed, new Clipboard(msgComp)),
                new PlaceCmd(ed, new Clipboard(msgComp))
            );
        } else {
            cmd = new EditCmd(ed, new Clipboard(GetMessageComp()), text);
        }
    } else {
        cmd = InteractorView::InterpretManipulator(m);
    }

    return cmd;
}

void MessageView::Interpret(Command* cmd) {
    if (cmd->IsA(TAB_CMD)) {
        TabTool tabTool;
        GetViewer()->UseTool(&tabTool);
        InteractorView::Interpret(cmd);
    } else {
        InteractorView::Interpret(cmd);
    }
}

/*****************************************************************************/

MessageCode::MessageCode (MessageComp* subj) : CodeView(subj) { }
MessageComp* MessageCode::GetMessageComp(){return (MessageComp*) GetSubject();}
ClassId MessageCode::GetClassId () { return MESSAGE_CODE; }

boolean MessageCode::IsA(ClassId id) {
    return MESSAGE_CODE == id || CodeView::IsA(id);
}

boolean MessageCode::Definition (ostream& out) {
    boolean ok = true;
    if (
	_emitProperty || _emitInstanceDecls || 
        _emitForward || _emitClassHeaders || _emitHeaders
    ) {
        return CodeView::Definition(out);

    } else if (_emitExpHeader) {
	InteractorComp* icomp = GetIntComp();
	MemberNameVar* mnamer = icomp->GetMemberNameVar();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        if (!snamer->IsSubclass()) {
            if (_scope && mnamer->GetExport()&&!_namelist->Search("message")) {
                _namelist->Append("message");
                out << "#include <InterViews/message.h>\n";
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }

    } else if (_emitCorehHeader) {
	InteractorComp* icomp = GetIntComp();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        const char* subclass = snamer->GetName();
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("message")) {
                _namelist->Append("message");
                out << "#include <InterViews/message.h>\n";
            }
        }
    } else if (_emitInstanceInits) {
        InteractorComp* icomp = GetIntComp();
        Shape* shape = icomp->GetShapeVar()->GetShape();
        const char* mname = icomp->GetMemberNameVar()->GetName();

        if (!_instancelist->Find((void*) mname)) {
            _instancelist->Append(new UList((void*)mname));
            BeginInstantiate(out);

	    MessageComp* mcomp = GetMessageComp();
            const char* text = mcomp->GetMessageGraphic()->GetText();
            out << "(";
            InstanceName(out);
            out << "\"" << text << "\", ";

	    Alignment a = mcomp->GetMessageGraphic()->GetAlignment();
            ok = ok && Align(a, out);
            out << ", 0, " << shape->hstretch << ", " << shape->vstretch;
	    out << ")";
            EndInstantiate(out);
	}

    } else if (
	_emitBSDecls || _emitBSInits || 
	_emitFunctionDecls || _emitFunctionInits
    ) {
        return true;

    } else if (
        _emitCoreDecls || _emitCoreInits || _emitClassDecls || _emitClassInits
    ) {
	ok = ok && CodeView::Definition(out);
        
    } else if (_emitMain) {
	ok = ok && CodeView::Definition(out);
        
    }
    return out.good() && ok;
}

boolean MessageCode::CoreConstDecls(ostream& out) { 
    out<<"(const char*, const char*, Alignment, int = 0, int = 0, int = 0);\n";
    return out.good();
}

boolean MessageCode::CoreConstInits(ostream& out) {
    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* baseclass = snamer->GetBaseClass();
    const char* subclass = snamer->GetName();

    out << "(\n    const char* i, const char* name, Alignment al, int p";
    out << ", int hstr, int hshr\n) : ";
    out << baseclass;
    out << "(i, name, al, p, hstr, hshr) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "}\n\n";
    return out.good();
}

boolean MessageCode::ConstDecls(ostream& out) {
    out<<"(const char*, const char*, Alignment, int = 0, int = 0, int = 0);\n";
    return out.good();
}

boolean MessageCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* i, const char* name, Alignment al, int p";
    out << ", int hstr, int hshr\n) : ";
    out << coreclass;
    out << "(i, name, al, p, hstr, hshr) {}\n";
    return out.good();
}

boolean MessageCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("message")) {
        _namelist->Append("message");
        out << "#include <InterViews/message.h> \n";
    }
    return out.good();
}

/*****************************************************************************/

MessageGraphic::MessageGraphic (
    const char* text, CanvasVar* c, Graphic* g, Alignment align, int pad
) : IBGraphic(c, g) {
    _text = (text == nil) ? nil : strnew(text);
    _align = align;
    _pad = pad;
}

MessageGraphic::~MessageGraphic () { delete _text; }
ClassId MessageGraphic::GetClassId () { return MESSAGE_GRAPHIC; }
const char* MessageGraphic::GetClassName () { return "Message"; }
Graphic* MessageGraphic::Copy(){
    return new MessageGraphic(GetText(), nil, this, _align);
}

void MessageGraphic::Natural (int& w, int& h) {
    FullGraphic gs;
    totalGS(gs);
    PSFont* font = gs.GetFont();
    w = font->Width(GetText()) + 2*_pad;
    h = font->Height() + 2*_pad;
}

void MessageGraphic::Read (istream& in) {
    char* text;
    ReadGS(in);
    text = unidraw->GetCatalog()->ReadString(in);
    in >> _align >> _pad;
    SetText(text);
    delete text;
}

void MessageGraphic::Write (ostream& out) {
    WriteGS(out);
    unidraw->GetCatalog()->WriteString(GetText(), out);
    out << _align << " " << _pad << " ";
}

void MessageGraphic::SetText (const char* text) {
    if (text != _text || strcmp(text, _text) != 0) {
        delete _text;
        _text = strnew(text);
    }
}

void MessageGraphic::getExtent (
    float& l, float& b, float& cx, float& cy, float& tol, Graphic* gs
) {
    CanvasVar* cvar = GetCanvasVar();

    if (cvar == nil) {
        int w, h;
        Natural(w, h);
        CalcExtent(w, h, l, b, cx, cy, tol, gs);
    } else {
        CalcExtent(cvar->Width(), cvar->Height(), l,b,cx,cy,tol,gs);
    }
    tol = 0;
}

void MessageGraphic::draw (Canvas* c, Graphic* gs) {
    Coord xmax, ymax;
    CanvasVar* cvar = GetCanvasVar();

    if (cvar == nil) {
        Natural(xmax, ymax);
    } else {
        xmax = cvar->xmax();
        ymax = cvar->ymax();
    }
    update(gs);
    _p->ClearRect(c, 0, 0, xmax, ymax);

    Coord tx, ty;
    GetTextPosition(tx, ty, _p->GetFont());

    _p->SetColors(gs->GetBgColor(), gs->GetFgColor());
    _p->FillRect(c, 0, 0, xmax, ymax);
    _p->SetColors(gs->GetFgColor(), gs->GetBgColor());
    _p->Text(c, GetText(), tx+_pad, ty+_pad);
}

void MessageGraphic::drawClipped (
    Canvas* c, Coord l, Coord b, Coord r, Coord t, Graphic* gs
) {
    Graphic::drawClipped(c, l, b, r, t, gs);
}

void MessageGraphic::GetTextPosition (Coord& l, Coord& b, const Font*) {
    Coord xmax, ymax;

    l = b = 0;
    if (GetCanvasVar() == nil) {
        Natural(xmax, ymax);
    } else {
        xmax = GetCanvasVar()->xmax();
        ymax = GetCanvasVar()->ymax();
    }
    int width, height;
    Natural(width, height);
    switch (_align) {
	case Left:
	case TopLeft:
	case CenterLeft:
	case BottomLeft:
            l = 0;
            break;
	case TopCenter:
	case Center:
	case BottomCenter:
	case HorizCenter:
            l = (xmax + 1 - width)/2;
            break;
	case Right:
	case TopRight:
	case CenterRight:
	case BottomRight:
            l = xmax + 1 - width;
            break;
	case Bottom:
	case Top:
	case VertCenter:
	    /* leave unchanged */
	    break;
    }
    switch (_align) {
	case Bottom:
	case BottomLeft:
	case BottomCenter:
	case BottomRight:
	    b = 0;
	    break;
	case CenterLeft:
	case Center:
	case CenterRight:
	case VertCenter:
	    b = (ymax + 1 - height) / 2;
	    break;
	case Top:
	case TopLeft:
	case TopCenter:
	case TopRight:
	    b = ymax + 1 - height;
	    break;
	case Left:
	case Right:
	case HorizCenter:
	    /* leave unchanged */
	    break;
    }
}
    

void MessageGraphic::Position (TextManip* tm) {
    Painter* p = tm->GetPainter();
    Transformer* rel = p->GetTransformer();
    Coord xpos, ypos, tx, ty;
    tm->GetPosition(xpos, ypos);
    GetTextPosition(tx, ty, p->GetFont());

    if (rel != nil) {
        rel->InvTransform(xpos, ypos);
    }
    Translate(xpos-tx, ypos-ty);
}

