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
 *  TextEdit component definitions
 */

#include "ibclasses.h"
#include "ibdialogs.h"
#include "ibmessage.h"
#include "ibtextedit.h"
#include "ibvars.h"

#include <Unidraw/ulist.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/

TextEditComp::TextEditComp (StrBrowserGraphic* g) : StrBrowserComp(g) { }
ClassId TextEditComp::GetClassId () { return TEXTEDIT_COMP; }

boolean TextEditComp::IsA (ClassId id) {
    return TEXTEDIT_COMP == id || MessageComp::IsA(id);
}

StateVar* TextEditComp::GetState (const char* name) {
    return MessageComp::GetState(name);
}

void TextEditComp::SetState(const char* name, StateVar* stateVar) {
    MessageComp::SetState(name, stateVar);
}

/*****************************************************************************/

TextEditComp* TextEditView::GetTextEditComp() {
    return (TextEditComp*) GetSubject();
}

TextEditView::TextEditView (TextEditComp* subj) : StrBrowserView(subj) { }

ClassId TextEditView::GetClassId () { return TEXTEDIT_VIEW; }

boolean TextEditView::IsA (ClassId id) {
    return TEXTEDIT_VIEW == id || StrBrowserView::IsA(id);
}

InfoDialog* TextEditView::GetInfoDialog () {
    return MessageView::GetInfoDialog();
}

/*****************************************************************************/

boolean TextEditCode::IsA (ClassId id) {
    return TEXTEDIT_CODE == id || CodeView::IsA(id);
}

ClassId TextEditCode::GetClassId () { return TEXTEDIT_CODE; }
TextEditCode::TextEditCode (TextEditComp* subj) : CodeView(subj) { }

TextEditComp* TextEditCode::GetTextEditComp () {
    return (TextEditComp*) GetSubject();
}

boolean TextEditCode::Definition (ostream& out) {
    boolean ok = true;
    if (
	_emitProperty || _emitInstanceDecls || 
        _emitClassHeaders || _emitHeaders || _emitForward
    ) {
        return CodeView::Definition(out);

    } else if (_emitExpHeader) {
	InteractorComp* icomp = GetIntComp();
	MemberNameVar* mnamer = icomp->GetMemberNameVar();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        if (!snamer->IsSubclass()) {
            if (
                _scope && mnamer->GetExport()&&!_namelist->Search("texteditor")
            ) {
                _namelist->Append("texteditor");
                out << "#include <InterViews/texteditor.h>\n";
                out << "#include <InterViews/textbuffer.h>\n";
            }
        } else {
            ok = ok && CodeView::Definition(out);
        }
    } else if (_emitCorehHeader) {
	InteractorComp* icomp = GetIntComp();
        SubclassNameVar* snamer = icomp->GetClassNameVar();
        const char* subclass = snamer->GetName();
        if (snamer->IsSubclass() && strcmp(subclass, _classname) == 0) {
            if (!_namelist->Search("texteditor")) {
                _namelist->Append("texteditor");
                out << "#include <InterViews/texteditor.h>\n";
            }
        }
    } else if (_emitInstanceInits) {
        InteractorComp* icomp = GetIntComp();
        const char* mname = icomp->GetMemberNameVar()->GetName();

        if (!_instancelist->Find((void*) mname)) {
            _instancelist->Append(new UList((void*)mname));

            TextEditComp* tc = GetTextEditComp();
            int rows, cols;
    
            BeginInstantiate(out);
            StrBrowserGraphic* graphic = tc->GetStrBrowserGraphic();
            graphic->GetRowsCols(rows, cols);

            out << "(";
            InstanceName(out);
            out << rows << ", " << cols << ", " << 4 << ", Reversed" << ");\n";
	    out << "    " << mname;
	    out << "->Edit(new TextBuffer(new char[256], 0, 256))";
            EndInstantiate(out);
        }

    } else if (
	_emitFunctionDecls || _emitFunctionInits || 
	_emitBSDecls || _emitBSInits 
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

boolean TextEditCode::CoreConstDecls(ostream& out) { 
    out << "(const char*, int r, int c, int t, int h);\n";
    return out.good();
}

boolean TextEditCode::CoreConstInits(ostream& out) {
    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* baseclass = snamer->GetBaseClass();
    const char* subclass = snamer->GetName();

    out << "(\n    const char* name, int r, int c, int t, int h\n)";
    out << " : " << baseclass;
    out << "(name, r, c, t, h) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "}\n\n";
    return out.good();
}

boolean TextEditCode::ConstDecls(ostream& out) {
    out << "(const char*, int r, int c, int t, int h);\n";
    return out.good();
}

boolean TextEditCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, int r, int c, int t, int h\n)";
    out << " : " << coreclass;
    out << "(name, r, c, t, h) {}\n\n";
    return out.good();
}

boolean TextEditCode::EmitIncludeHeaders(ostream& out) {
    SubclassNameVar* snamer = GetIntComp()->GetClassNameVar();

    if (!snamer->IsSubclass() && !_namelist->Search("texteditor")) {
        _namelist->Append("texteditor");
        out << "#include <InterViews/texteditor.h> \n";
    }
    if (
        strcmp(snamer->GetName(), _classname) != 0 && 
        !_namelist->Search("textbuffer")
    ) {
        _namelist->Append("textbuffer");
        out << "#include <InterViews/textbuffer.h> \n";
    }
    return out.good();
}

