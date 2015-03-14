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
 * IText component definitions.
 */

#include "ibtext.h"
#include "ibclasses.h"
#include "ibcmds.h"
#include "ibgrblock.h"
#include "ibvars.h"
#include <Unidraw/clipboard.h>
#include <Unidraw/editor.h>
#include <Unidraw/iterator.h>
#include <Unidraw/manips.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Graphic/picture.h>
#include <Unidraw/Graphic/pspaint.h>
#include <Unidraw/Tools/tool.h>
#include <InterViews/transformer.h>
#include <InterViews/painter.h>
#include <OS/memory.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/
static const char* textcomp_delim = "%textcomp_delim";
/*****************************************************************************/

class ITextGraphic : public Picture {
public:
    ITextGraphic();
    void GetTotalGS(FullGraphic* gs);
};

ITextGraphic::ITextGraphic () {}

void ITextGraphic::GetTotalGS (FullGraphic* gs) {
    Iterator i;

    totalGS(*gs);
    First(i);
    Graphic* gr = GetGraphic(i);
    concatGraphic(gr, gr, gs, gs);
}

/*****************************************************************************/
ITextComp::ITextComp (TextGraphic* graphic) : IComp (new ITextGraphic) {
    _gclassNameVar->SetName("TextGraphic");
    _gclassNameVar->SetBaseClass("TextGraphic");
    _cclassNameVar->SetName("TextComp");
    _cclassNameVar->SetBaseClass("TextComp");
    _vclassNameVar->SetName("TextView");
    _vclassNameVar->SetBaseClass("TextView");
    _compid->SetOrigID(TEXT_COMP);

    if (!_release || graphic != nil) {
        _target = new TextComp(graphic);
        if (graphic != nil) {
            Append(_target);
        }
    }
}

void ITextComp::Interpret (Command* cmd) {
    if (cmd->IsA(EDIT_CMD)) {
        EditCmd* editcmd = (EditCmd*) cmd;

        GraphicComp* target = editcmd->SwapComp(GetTarget());
        Append(target);

        Graphic* src = _target->GetGraphic();
        Graphic* dest = target->GetGraphic();
        src->Align(TopLeft, dest, TopLeft);

        Remove(_target);
        _target = target;
        Notify();
        Propagate(cmd);

    } else {
        IComp::Interpret(cmd);
    }
}

ClassId ITextComp::GetSubstId(const char*& delim) {
    delim = textcomp_delim;
    return TEXT_COMP;
}

ClassId ITextComp::GetClassId () { return ITEXT_COMP; }

boolean ITextComp::IsA (ClassId id) {
    return ITEXT_COMP == id || IComp::IsA(id);
}

/*****************************************************************************/
ITextView::ITextView (ITextComp* icomp) : IView(icomp) {}

ITextComp* ITextView::GetITextComp () { return (ITextComp*) GetSubject(); }

Graphic* ITextView::GetGraphic () {
    Graphic* g = GraphicView::GetGraphic();
    
    if (g == nil) {
        g = new ITextGraphic;
        Iterator i;

        for (First(i); !Done(i); Next(i)) {
            g->Append(GetView(i)->GetGraphic());
        }
        SetGraphic(g);
    }
    return g;
}

Manipulator* ITextView::CreateManipulator (
    Viewer* v, Event& e, Transformer* rel, Tool* tool
) {
    Manipulator* m = nil;
    Editor* ed = v->GetEditor();
    int tabWidth = round(.5*inch);

    if (tool->IsA(RESHAPE_TOOL)) {
        FullGraphic gs;

        ITextComp* itext = GetITextComp();
        ITextGraphic* ipic = (ITextGraphic*) itext->GetGraphic();

        ipic->GetTotalGS(&gs);
        
        TextGraphic* textgr = (TextGraphic*) GetKidView()->GetGraphic();

        Painter* painter = new Painter;
        int lineHt = textgr->GetLineHeight();
        float xpos, ypos;
        rel = new Transformer;
        const char* text = textgr->GetOriginal();
        int size = strlen(text);
        
        textgr->TotalTransformation(*rel);
        rel->transform(0.0, 0.0, xpos, ypos);

        painter->FillBg(true);
        painter->SetFont(gs.GetFont());
	painter->SetColors(gs.GetFgColor(), gs.GetBgColor());
        painter->SetTransformer(rel);
        Unref(rel);

        m = new TextManip(
            v, text, size, (Coord)xpos, (Coord)ypos, 
            painter, lineHt, tabWidth, tool
        );

    } else {
        m = IView::CreateManipulator(v, e, rel, tool);
    }
    return m;
}

Command* ITextView::InterpretManipulator (Manipulator* m) {
    Viewer* v = m->GetViewer();
    Editor* ed = v->GetEditor();
    Tool* tool = m->GetTool();
    Command* cmd = nil;

    if (tool->IsA(RESHAPE_TOOL)) {
        TextManip* tm = (TextManip*) m;
        int lineHt = tm->GetLineHeight();
        int size;
        const char* text = tm->GetText(size);
        Graphic* pg = GetITextComp()->GetTarget()->GetGraphic();
        TextGraphic* textgr = new TextGraphic(text, lineHt, pg);

        cmd = new EditCmd(
            ed, new Clipboard(GetITextComp()), new TextComp(textgr)
        );

    } else {
        cmd = IView::InterpretManipulator(m);
    }

    return cmd;
}

/*****************************************************************************/

ClassId TextCode::GetClassId () { return ITEXT_CODE; }

boolean TextCode::IsA(ClassId id) {
    return ITEXT_CODE == id || GraphicCodeView::IsA(id);
}

TextCode::TextCode (ITextComp* subj) : GraphicCodeView(subj) {}

void TextCode::Update () {
    GraphicCodeView::Update();
    GetITextComp()->Bequeath();
}

ITextComp* TextCode::GetITextComp () {
    return (ITextComp*) GetSubject(); 
}

const char* TextCode::GetCVHeader () { return "text"; }

boolean TextCode::Definition (ostream& out) {
    boolean ok = true;

    ITextComp* textcomp = GetITextComp();
    TextComp* target = (TextComp*) textcomp->GetTarget();
    TextGraphic* textgr= target->GetText();

    SubclassNameVar* cnamer = textcomp->GetCClassNameVar();
    SubclassNameVar* gnamer = textcomp->GetGClassNameVar();
    SubclassNameVar* vnamer = textcomp->GetVClassNameVar();
    MemberNameVar* mnamer = textcomp->GetMemberNameVar();

    const char* mname = mnamer->GetName();
    const char* cname = cnamer->GetName();
    const char* gname = gnamer->GetName();
    const char* vname = vnamer->GetName();

    if (_emitInstanceDecls || _emitGraphicState) {
	ok = ok && GraphicCodeView::Definition(out);

    } else if (_emitExpHeader) {
        if (!gnamer->IsSubclass()) {
            if (_scope && mnamer->GetExport()&&!_namelist->Search("text")) {
                _namelist->Append("text");
                out << "#include <Unidraw/Components/text.h> \n";
            }
        } else {
            ok = GraphicCodeView::Definition(out);
        }
    } else if (_emitCorehHeader) {
        if (gnamer->IsSubclass() && strcmp(gname, _classname) == 0) {
            if (!_namelist->Search("text")) {
                _namelist->Append("text");
                out << "#include <Unidraw/Components/text.h> \n";
            }
        }
        if (_emitGraphicComp) {
            if (
                cnamer->IsSubclass() && strcmp(cname, _classname) == 0 ||
                vnamer->IsSubclass() && strcmp(vname, _classname) == 0
            ) {
                if (!_namelist->Search("text")) {
                    _namelist->Append("text");
                    out << "#include <Unidraw/Components/text.h> \n";
                }
            }
        }
    } else if (_emitInstanceInits) {
        const char* text = textgr->GetOriginal();
        char* copy = new char[strlen(text)*2];
        strcpy(copy, text);
        char* tmp = copy;
        
        for(tmp = strchr(tmp, '\n'); tmp != nil; tmp = strchr(++tmp, '\n')){
            Memory::copy(tmp+1, tmp+2, strlen(tmp+1)+1);
            *tmp = '\\';
            tmp++;
            *tmp = 'n';
        }
        
        int h = textgr->GetLineHeight();

        out << "    {\n";
        if (_emitGraphicComp) {
            out << "        " << mname << "_gr";
        } else {
            out << "        " << mname;
        }
        out << " = new " << gname << "(\"";
        out << copy << "\", " << h << ");\n";
        ok = WriteGraphicInits(textgr, out);
        if (_emitGraphicComp) {
            out << "        " << mname << " = new " << cname << "(";
            out << mname << "_gr);\n";
        }
        out << "    }\n";
        delete copy;

    } else {
	ok = ok && GraphicCodeView::Definition(out);
    }
    return ok && out.good();
}

boolean TextCode::GCoreConstDecls(ostream& out) { 
    out << "(const char*, int h, Graphic* = nil);\n";
    return out.good();
}

boolean TextCode::GCoreConstInits(ostream& out) {
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* baseclass = gnamer->GetBaseClass();

    out << "(\n    const char* name, int h, Graphic* gr\n) : ";
    out << baseclass << "(name, h, gr) {}\n\n";

    return out.good();
}

boolean TextCode::GConstDecls(ostream& out) {
    out << "(const char*, int h, Graphic* = nil);\n";
    out << "    virtual Graphic* Copy();\n";
    return out.good();
}

boolean TextCode::GConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);
    IComp* icomp = GetIComp();
    SubclassNameVar* gnamer = icomp->GetGClassNameVar();
    const char* gname = gnamer->GetName();

    out << "(\n    const char* name, int h, Graphic* gr\n) : ";
    out << coreclass << "(name, h, gr) {}\n\n";
    out << "Graphic* " << gname << "::Copy () {\n";
    out << "    return new " << gname << "(_string, _lineHt, this);\n";
    out << "}\n\n";

    return out.good();
}

boolean TextCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* gnamer = GetIComp()->GetGClassNameVar();

    if (
        (!gnamer->IsSubclass() || strcmp(gnamer->GetName(), _classname) == 0)
        && !_namelist->Search("text")
    ) {
        _namelist->Append("text");
        out << "#include <Unidraw/Components/text.h> \n";
    }
    return out.good();
}



