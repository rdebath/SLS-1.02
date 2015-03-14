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
 * Implementation of CommandCtrl
 */

#include "ibclasses.h"
#include "ibcmds.h"
#include "ibtext.h"
#include "ibdialogs.h"
#include "ibed.h"
#include "ibeditor.h"
#include "ibgrcomp.h"
#include "ibvars.h"
#include "ibvarviews.h"
#include "ibcommandctrl.h"

#include <Unidraw/iterator.h>
#include <Unidraw/statevars.h>
#include <Unidraw/selection.h>
#include <Unidraw/viewer.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/upage.h>
#include <Unidraw/Tools/tool.h>
#include <Unidraw/Components/text.h>
#include <Unidraw/Commands/command.h>
#include <Unidraw/Graphic/graphic.h>

#include <InterViews/brush.h>
#include <InterViews/shape.h>
#include <InterViews/transformer.h>

#include <math.h>
#include <stdio.h>
#include <stream.h>
#include <string.h>

/*****************************************************************************/

static const int HPAD = 4;          // horizontal padding around control labels
static const int VPAD = 1;          // vertical padding around control labels
static const int SEP = 8;           // separation between label & equiv
static const int MINHT = 15;        // minimum height of control labels
static const int MINWD = 15;        // minimum width of control labels

/*****************************************************************************/

CommandCtrlComp::CommandCtrlComp (PanelCtrlGraphic* gr) : PanelCtrlComp(gr) { 
    if (gr != nil) {
        SubclassNameVar* subclass = GetClassNameVar();
        subclass->SetName("CommandControl");
        subclass->SetBaseClass("CommandControl");
    }
}

ClassId CommandCtrlComp::GetClassId () { return COMMANDCONTROL_COMP; }

boolean CommandCtrlComp::IsA (ClassId id) {
    return COMMANDCONTROL_COMP == id || PanelCtrlComp::IsA(id);
}

void CommandCtrlComp::Instantiate () {
    if (_toolname == nil) {
        _toolname = new MemberNameVar("NOPCmd");
        _toolname->GenNewName();
        SubclassNameVar* svar = _toolname->GetSubclass();
        svar->SetBaseClass("NOPCmd");
        svar->SetName("NOPCmd");
        _toolname->SetIDVar(new IDVar);
    }
    PanelCtrlComp::Instantiate();
}

void CommandCtrlComp::Reconfig () {
    Shape* shape = GetShapeVar()->GetShape();
    int x0, y0, x1, y1;
    TextComp* textcomp = (TextComp*) GetKeyLabel()->GetTarget();
    TextGraphic* textgr = textcomp->GetText();
    const char* kl = textgr->GetOriginal();

    GetTop()->GetGraphic()->GetBox(x0, y0, x1, y1);
    shape->width = 2*HPAD + x1 - x0;
    shape->height = max(2*VPAD + y1 - y0, MINHT);

    if (*kl != '\0') {
	PSFont* f = stdgraphic->GetFont();
	shape->width += f->Width(kl) + SEP;
	shape->height = max(shape->height, f->Height() + 2*VPAD);
    }
    shape->Rigid(shape->width, hfil, 0, 0);
}

void CommandCtrlComp::Resize () {
    Iterator i;

    PanelCtrlGraphic* pcgr = GetPanelCtrlGraphic();
    TextComp* textcomp = (TextComp*) GetKeyLabel()->GetTarget();
    TextGraphic* textgr = textcomp->GetText();
    const char* kl = textgr->GetOriginal();

    Picture* topgr = (Picture*) GetTop()->GetGraphic();
    pcgr->SetPattern(psclear);
    
    if (*kl == '\0') {
        if (!topgr->IsEmpty()) {
            topgr->Align(Center, pcgr, Center);
        }
    } else {
        if (!topgr->IsEmpty()) {
            topgr->Translate(-HPAD, 0);
            topgr->Align(CenterLeft, pcgr, CenterLeft);
            topgr->Translate(HPAD, 0);
            pcgr->Align(CenterRight, textgr, CenterRight);
            textgr->Translate(-HPAD, 0);
        }
    }
    textcomp->Notify();
    SubNotify();
}

void CommandCtrlComp::Interpret(Command* cmd) {
    if (cmd->IsA(GETNAMEVARS_CMD)) {
        GrBlockComp::Interpret(cmd);
        GetNameVarsCmd* gcmd = (GetNameVarsCmd*) cmd;
        gcmd->AppendExtras(_toolname);
        gcmd->AppendExtras(_toolname->GetSubclass());

    } else if (cmd->IsA(GETCONFLICT_CMD)) {
        GetConflictCmd* gcmd = (GetConflictCmd*) cmd;
        const char* cname = gcmd->GetCName();
        UList* conflictlist = gcmd->GetConflict();
        const char* toolm = _toolname->GetName();
        SubclassNameVar* toolnamer = _toolname->GetSubclass();
        const char* tools = toolnamer->GetName();
        const char* toolb = toolnamer->GetBaseClass();
        
        if (strcmp(tools, cname) == 0 || strcmp(toolb, cname) == 0) {
            conflictlist->Append(
                new UList(toolnamer)
            );
        }
        if (strcmp(toolm, cname) == 0) {
            conflictlist->Append(
                new UList(_toolname->GetMemberSharedName())
            );
        }
    } else {
        PanelCtrlComp::Interpret(cmd);
    }
}

void CommandCtrlComp::Uninterpret(Command* cmd) {
    if (!cmd->IsA(GETCONFLICT_CMD)) {
        PanelCtrlComp::Uninterpret(cmd);
    }
}

boolean CommandCtrlComp::IsRelatableTo (InteractorComp* comp) {
    boolean ok = false;
    if (comp->IsA(EDITOR_COMP) || comp->IsA(COMMANDCONTROL_COMP)) {
        ok = true;
    }
    return ok;
}

/*****************************************************************************/
static const int comID[] = {
    ALIGN_CMD, ALIGN_CMD, ALIGN_CMD, ALIGN_CMD, ALIGN_CMD,
    ALIGN_CMD, ALIGN_CMD, ALIGN_CMD, ALIGN_CMD, ALIGN_CMD,
    ALIGN_CMD, ALIGNTOGRID_CMD, BACK_CMD, BRUSH_CMD, CENTER_CMD,
    CLOSEEDITOR_CMD, COLOR_CMD, COLOR_CMD, COPY_CMD, CUT_CMD,
    DELETE_CMD, DUP_CMD, FONT_CMD, FRONT_CMD, GRAVITY_CMD,
    GRID_CMD, GRIDSPACING_CMD, GROUP_CMD, IMPORT_CMD, MOVE_CMD,
    NEWCOMP_CMD, NOP_CMD, NORMSIZE_CMD, ORIENTATION_CMD, PATTERN_CMD,
    PASTE_CMD, PRINT_CMD, QUIT_CMD, REDO_CMD, REDTOFIT_CMD,
    REVERT_CMD, ROTATE_CMD, ROTATE_CMD, SAVECOMP_CMD, SAVECOMPAS_CMD,
    SCALE_CMD, SCALE_CMD, SLCTALL_CMD, UNDO_CMD, UNGROUP_CMD,
    VIEWCOMP_CMD
};
/*****************************************************************************/

CommandCtrlView::CommandCtrlView (
    CommandCtrlComp* subj
) : PanelCtrlView(subj) { _keylabel = nil; }


CommandCtrlComp* CommandCtrlView::GetCommandCtrlComp () {
    return (CommandCtrlComp*) GetSubject();
}

ClassId CommandCtrlView::GetClassId () { return COMMANDCONTROL_VIEW; }

boolean CommandCtrlView::IsA (ClassId id) {
    return COMMANDCONTROL_VIEW == id || PanelCtrlView::IsA(id);
}

GraphicComp* CommandCtrlView::CreateProtoComp (
    Editor* ed, Coord l, Coord b, Coord r, Coord t
) {
    ColorVar* colVar = (ColorVar*) ed->GetState("ColorVar");
    FontVar* fontVar = (FontVar*) ed->GetState("FontVar");
    
    float mcx, mcy, cx, cy;
    CommandCtrlComp* comComp = (CommandCtrlComp*) GetCommandCtrlComp()->Copy();
    Graphic* topgr = comComp->GetTop()->GetGraphic();
    mcx = (r-l)/2.0; mcy = (t-b)/2.0;

    topgr->SetColors(colVar->GetFgColor(), colVar->GetBgColor());
    topgr->SetFont(fontVar->GetFont());
    topgr->GetCenter(cx, cy);
    topgr->Translate(mcx-cx, mcy-cy);
    topgr->Translate(HPAD, 0);
    return comComp;
}

InfoDialog* CommandCtrlView::GetInfoDialog () {
    IBEditor* ibed = (IBEditor*) GetViewer()->GetEditor();
    InfoDialog* info = InteractorView::GetInfoDialog();
    ButtonState* state = info->GetState();
    CommandCtrlComp* comComp = GetCommandCtrlComp();

    MemberNameVar* comName = comComp->GetToolName();
    MemberNameVar* edVar = comComp->GetEditorVar();

    SMemberNameVarView* comChooser = new SMemberNameVarView(
        comName, state, comComp, ibed, "Command", comID
    );

    comChooser->Append("AlignCmd Bottom Bottom");
    comChooser->Append("AlignCmd Bottom Top");
    comChooser->Append("AlignCmd Center Center");
    comChooser->Append("AlignCmd HorizCenter HorizCenter");
    comChooser->Append("AlignCmd Left Left");
    comChooser->Append("AlignCmd Left Right");
    comChooser->Append("AlignCmd Right Left");
    comChooser->Append("AlignCmd Right Right");
    comChooser->Append("AlignCmd Top Bottom");
    comChooser->Append("AlignCmd Top Top");
    comChooser->Append("AlignCmd VertCenter VertCenter");
    comChooser->Append("AlignToGridCmd");
    comChooser->Append("BackCmd");
    comChooser->Append("BrushCmd");
    comChooser->Append("CenterCmd");
    comChooser->Append("CloseEditorCmd");
    comChooser->Append("ColorCmd fg");
    comChooser->Append("ColorCmd bg");
    comChooser->Append("CopyCmd");
    comChooser->Append("CutCmd");
    comChooser->Append("DeleteCmd");
    comChooser->Append("DupCmd");
    comChooser->Append("FontCmd");
    comChooser->Append("FrontCmd");
    comChooser->Append("GravityCmd");
    comChooser->Append("GridCmd");
    comChooser->Append("GridSpacingCmd");
    comChooser->Append("GroupCmd");
    comChooser->Append("ImportCmd");

    comChooser->Append("MoveCmd 0.0 0.0");
    comChooser->Append("NewCompCmd");
    comChooser->Append("NOPCmd");
    comChooser->Append("NormSizeCmd");
    comChooser->Append("OrientationCmd");
    comChooser->Append("PatternCmd");
    comChooser->Append("PasteCmd");
    comChooser->Append("PrintCmd");
    comChooser->Append("QuitCmd");
    comChooser->Append("RedoCmd");
    comChooser->Append("RedToFitCmd");
    comChooser->Append("RevertCmd");
    comChooser->Append("RotateCmd 90.0");
    comChooser->Append("RotateCmd -90.0");
    comChooser->Append("SaveCompCmd");
    comChooser->Append("SaveCompAsCmd");
    comChooser->Append("ScaleCmd -1.0 1.0");
    comChooser->Append("ScaleCmd 1.0 -1.0");
    comChooser->Append("SlctAllCmd");

    comChooser->Append("UndoCmd");
    comChooser->Append("UngroupCmd");
    comChooser->Append("ViewCompCmd");

    info->Include(new RelatedVarView(
        edVar, state, comComp, "Editor Name: ")
        
    );
    info->Include(comChooser);
    return info;
}

/*****************************************************************************/

CommandCtrlCode::CommandCtrlCode (
    CommandCtrlComp* subj
) : PanelCtrlCode(subj) { 
    _emitToolInits = false;
    _emitBaseclass = false;
}

CommandCtrlComp* CommandCtrlCode::GetCommandCtrlComp() {
    return (CommandCtrlComp*) GetSubject();
}

ClassId CommandCtrlCode::GetClassId () { return COMMANDCONTROL_CODE; }

boolean CommandCtrlCode::IsA(ClassId id) {
    return COMMANDCONTROL_CODE == id || PanelCtrlCode::IsA(id);
}

boolean CommandCtrlCode::Definition (ostream& out) {
    char coreclass[CHARBUFSIZE];
    char Keycode[64], Coms[64], Comb[64];
    char Kidname[64];
    char Arg1s[64], Arg2s[64];
    char Arg1b[64], Arg2b[64];

    boolean ok = true;
    Iterator i;

    CommandCtrlComp* ctrlComp = GetCommandCtrlComp();
    SubclassNameVar* snamer = ctrlComp->GetClassNameVar();
    MemberNameVar* mnamer = ctrlComp->GetMemberNameVar();
    TextComp* textcomp = (TextComp*) ctrlComp->GetKeyLabel()->GetTarget();
    TextGraphic* textgr = textcomp->GetText();
    MemberNameVar* edVar = ctrlComp->GetEditorVar();
    MemberNameVar* comm = ctrlComp->GetToolName();

    SubclassNameVar* comnamer = comm->GetSubclass();
    const char* edname = edVar->GetName();

    const char* text = textgr->GetOriginal();
    const char* coms = comnamer->GetName();
    const char* comb = comnamer->GetBaseClass();

    const char* subclass = snamer->GetName();
    const char* baseclass = snamer->GetBaseClass();
    const char* mname = mnamer->GetName();
    strcpy(Keycode, text);

    *Arg1s = '\0';
    *Arg2s = '\0';
    
    /* some sscanfs need writable string */
    char* writable_com = strnew(coms);
    sscanf(writable_com, "%s%s%s", Coms, Arg1s, Arg2s);
    delete writable_com;
    writable_com = strnew(comb);
    sscanf(writable_com, "%s%s%s", Comb, Arg1b, Arg2b);
    delete writable_com;
            

    HashKeyCode(Keycode);
    InteractorComp* dummy;

    GetCoreClassName(coreclass);
    if (*edname == '\0') {
        if (_err_count < 10) {
            strcat(_errbuf, mname);
            strcat(_errbuf, " has undefined Editor.\n");
            _err_count++;
        }
        return false;

    } else if (!Search(edVar, dummy)) {
        if (_err_count < 10) {
            strcat(_errbuf, mname);
            strcat(
                _errbuf, "'s Editor is not in the same hierachy.\n"
            );
            _err_count++;
        } 
        return false;
    }

    _emitGraphicComp = true;

    if (_emitInstanceInits) {
        if (!_instancelist->Find((void*)mname)) {
            _instancelist->Append(new UList((void*)mname));

            ok = ok && EmitGraphicState(out);
            for(First(i); !Done(i); Next(i)) {
                CodeView* kid = (CodeView*) GetView(i);
                ok = ok && EmitInstanceDecls(kid, out);
            }

            ok = ok && Iterate(out);

            SubclassNameVar* kidclass = nil;

            if (!SingleKid()) {
                out << "    GraphicComps* " << mname;
                out << "_comp = new GraphicComps;\n";
                
                for(First(i); !Done(i); Next(i)) {
                    CodeView* kid = (CodeView*) GetView(i);
                    IComp* kidcomp = kid->GetIComp();
                    MemberNameVar* kmnamer = kidcomp->GetMemberNameVar();
                    SubclassNameVar* kidcclass = kidcomp->GetCClassNameVar();
                    const char* kmname = kmnamer->GetName();
                    out << "    " << mname << "_comp->Append(";
                    out << kmname << ");\n";
                }
                strcpy(Kidname, mname);
                strcat(Kidname, "_comp");

            } else {
                First(i);
                CodeView* kidv = (CodeView*) GetView(i);
                MemberNameVar* kmnamer = kidv->GetIComp()->GetMemberNameVar();
                strcpy(Kidname, kmnamer->GetName());
                kidclass = kidv->GetIComp()->GetCClassNameVar();
            }
            out << "    ControlInfo* " << mname;
            out << "_info = new ControlInfo(";

            out << Kidname << ", \"" << text << "\", \"";
            out << Keycode << "\");\n";

            if (!comm->GetExport() || _emitMain) {
                out << "    " << Coms << "* " << comm->GetName() <<";\n";
            }
            out << "    " << comm->GetName() << " = new ";
            
            if (*Comb == '\0') {
                if (_err_count < 10) {
                    strcat(_errbuf, mname);
                    strcat(_errbuf, " has undefined Command.\n");
                }
                return false;
            }
            
            if (strcmp(Comb, "NewCompCmd") == 0) {
                out << Coms << "(" << mname << "_info, new ";
                if (
                    kidclass != nil && 
                    strcmp(kidclass->GetBaseClass(), "GraphicComps") == 0
                ) {
                    out << kidclass->GetName() << ";\n";
                } else {
                    out << "GraphicComps);\n";
                }
            } else if (
                strcmp(Comb, "ScaleCmd") == 0 ||
                strcmp(Comb, "AlignCmd") == 0 ||
                strcmp(Comb, "MoveCmd") == 0
            ) {
                out << Coms << "(" << mname << "_info, ";
                out << Arg1s << ", " << Arg2s << ");\n";

            } else if (strcmp(Comb, "RotateCmd") == 0) {
                out << Coms << "(" << mname << "_info, ";
                out << Arg1s << ");\n";

            } else if (strcmp(Comb, "FontCmd") == 0) {
                out << Coms << "(\n        " << mname << "_info, ";
                out << Kidname << "->GetGraphic()->GetFont()\n    );\n";

            } else if (strcmp(Comb, "BrushCmd") == 0) {
                out << Coms << "(\n        " << mname << "_info, ";
                out << Kidname << "->GetGraphic()->GetBrush()\n    );\n";

            } else if (strcmp(Comb, "PatternCmd") == 0) {
                out << Coms << "(\n        " << mname << "_info, ";
                out << Kidname << "->GetGraphic()->GetPattern()\n    );\n";

            } else if (strcmp(Comb, "ColorCmd") == 0) {
                out << Coms << "(" << mname << "_info, ";
                if (strcmp(Arg1s, "fg") == 0) {
                    out << Kidname;
                    out << "->GetGraphic()->GetFgColor(), nil\n    );\n";

                } else if (strcmp(Arg1s, "bg") == 0) {
                    out << "nil, " << Kidname;
                    out << "->GetGraphic()->GetBgColor()\n    );\n";

                } else {
                    out << Kidname << "->GetGraphic()->GetFgColor(), ";
                    out << Kidname;
                    out << "->GetGraphic()->GetBgColor()\n    );\n";
                }

            } else {
                out << Coms << "(" << mname << "_info);\n";
            }

            BeginInstantiate(out);
            out << "(";
            InstanceName(out);
            out << mname << "_info)";
            EndInstantiate(out);
            out << "    GetKeyMap()->Register(";
            out << mname << ");\n";
            out << "    " << comm->GetName() << "->SetEditor(this);\n";
        }
    } else {
        SubclassNameVar* clonenamer = (SubclassNameVar*) comnamer->Copy();
        clonenamer->SetName(Coms);
        clonenamer->SetBaseClass(Comb);
        clonenamer->ref();
        comnamer->ref();
        comm->SetSubclass(clonenamer);
        
        if (_emitForward) {
            if (_scope) {
                ok = ok && GrBlockCode::Definition(out);
                if (
                    comm->GetExport() && !_namelist->Search(coms)
                ) {
                    _namelist->Append(coms);
                    out << "class " << coms << ";\n";
                }
            }
        } else if (_emitBSDecls || _emitBSInits) {
            ok = true;
            
        } else {
            ok = ok && PanelCtrlCode::Definition(out);
        }
        comm->SetSubclass(comnamer);
        clonenamer->unref();
        comnamer->unref();
    }
    _emitGraphicComp = false;

    return out.good() && ok;
}

boolean CommandCtrlCode::CoreConstDecls(ostream& out) { 
    out << "(const char*, ControlInfo*);\n";
    return out.good();
}

boolean CommandCtrlCode::CoreConstInits(ostream& out) {
    InteractorComp* icomp = GetIntComp();
    SubclassNameVar* snamer = icomp->GetClassNameVar();
    const char* baseclass = snamer->GetBaseClass();
    const char* subclass = snamer->GetName();

    out << "(\n    const char* name, ControlInfo* info";
    out << "\n) : ";
    out << baseclass << "(name, info) {\n";
    out << "    SetClassName(\"" << subclass << "\");\n";
    out << "}\n\n";

    return out.good();
}

boolean CommandCtrlCode::ConstDecls(ostream& out) {
    out << "(const char*, ControlInfo*);\n";
    return out.good();
}

boolean CommandCtrlCode::ConstInits(ostream& out) {
    char coreclass[CHARBUFSIZE];
    GetCoreClassName(coreclass);

    out << "(\n    const char* name, ControlInfo* info";
    out << "\n) : ";
    out << coreclass << "(name, info) {}\n\n";

    return out.good();
}

boolean CommandCtrlCode::ToolCoreConstDecls(ostream& out) { 
    boolean ok = true;
    CommandCtrlComp* ccComp = GetCommandCtrlComp();
    MemberNameVar* comm = ccComp->GetToolName();
    SubclassNameVar* comnamer = comm->GetSubclass();

    const char* coms = comnamer->GetName();
    const char* comb = comnamer->GetBaseClass();

    char coreclass[CHARBUFSIZE];

    char Coms[64], Comb[64];
    char Arg1s[64], Arg2s[64];
    char Arg1b[64], Arg2b[64];


    *Arg1s = '\0';
    *Arg2s = '\0';
    
    /* some sscanfs need writable string */
    char* writable_com = strnew(coms);
    sscanf(writable_com, "%s%s%s", Coms, Arg1s, Arg2s);
    delete writable_com;
    writable_com = strnew(comb);
    sscanf(writable_com, "%s%s%s", Comb, Arg1b, Arg2b);
    delete writable_com;

    strcpy(coreclass, Coms);
    strcat(coreclass, "_core");

    out << "class " << coreclass << " : public " << Comb << " {\n";
    out << "public:\n";
    out << "    " << coreclass << "(ControlInfo* = nil";

    ok = ok && EmitParameters(Comb, out);
    out << ");\n";
    out << "    virtual ClassId GetClassId();\n";
    out << "    virtual boolean IsA(ClassId);\n";
    out << "};\n\n";

    return out.good() && ok;
}

boolean CommandCtrlCode::ToolCoreConstInits(ostream& out) {
    boolean ok = true;
    CommandCtrlComp* ccComp = GetCommandCtrlComp();
    MemberNameVar* comm = ccComp->GetToolName();
    SubclassNameVar* comnamer = comm->GetSubclass();
    IDVar* idvar = comm->GetIDVar();

    const char* coms = comnamer->GetName();
    const char* comb = comnamer->GetBaseClass();

    char coreclass[CHARBUFSIZE];

    char Coms[64], Comb[64];
    char Arg1s[64], Arg2s[64];
    char Arg1b[64], Arg2b[64];


    *Arg1s = '\0';
    *Arg2s = '\0';
    
    /* some sscanfs need writable string */
    char* writable_com = strnew(coms);
    sscanf(writable_com, "%s%s%s", Coms, Arg1s, Arg2s);
    delete writable_com;
    writable_com = strnew(comb);
    sscanf(writable_com, "%s%s%s", Comb, Arg1b, Arg2b);
    delete writable_com;

    strcpy(coreclass, Coms);
    strcat(coreclass, "_core");

    out << coreclass << "::" << coreclass << "(\n";
    out << "    ControlInfo* ctrlinfo";
    
    _emitToolInits = true;

    ok = ok && EmitParameters(Comb, out);
    out << "\n) : " << Comb << "(ctrlinfo";

    _emitBaseclass = true;
    ok = ok && EmitParameters(Comb, out);
    _emitBaseclass = false;

    _emitToolInits = false;
    out << ") {}\n\n";

    out << "ClassId " << coreclass << "::GetClassId() { return ";
    out << comm->GetIDVar()->GetID() << ";}\n";
    out << "boolean " << coreclass << "::IsA(ClassId id) {\n";
    out << "    return id == " << comm->GetIDVar()->GetID() << " || ";
    out << Comb << "::IsA(id);\n";
    out << "}\n";
    return out.good() && ok;
}

boolean CommandCtrlCode::ToolConstDecls(ostream& out) {
    boolean ok = true;
    CommandCtrlComp* ccComp = GetCommandCtrlComp();
    MemberNameVar* comm = ccComp->GetToolName();
    SubclassNameVar* comnamer = comm->GetSubclass();

    const char* coms = comnamer->GetName();
    const char* comb = comnamer->GetBaseClass();

    char coreclass[CHARBUFSIZE];

    char Coms[64], Comb[64];
    char Arg1s[64], Arg2s[64];
    char Arg1b[64], Arg2b[64];


    *Arg1s = '\0';
    *Arg2s = '\0';
    
    /* some sscanfs need writable string */
    char* writable_com = strnew(coms);
    sscanf(writable_com, "%s%s%s", Coms, Arg1s, Arg2s);
    delete writable_com;
    writable_com = strnew(comb);
    sscanf(writable_com, "%s%s%s", Comb, Arg1b, Arg2b);
    delete writable_com;

    strcpy(coreclass, Coms);
    strcat(coreclass, "_core");

    out << "class " << Coms << " : public " << coreclass << " {\n";
    out << "public:\n";
    out << "    " << Coms << "(ControlInfo* = nil";
    ok = ok && EmitParameters(Comb, out);
    out << ");\n";
    out << "};\n\n";

    return out.good() && ok;
}

boolean CommandCtrlCode::ToolConstInits(ostream& out) {
    boolean ok = true;
    CommandCtrlComp* ccComp = GetCommandCtrlComp();
    MemberNameVar* comm = ccComp->GetToolName();
    SubclassNameVar* comnamer = comm->GetSubclass();

    const char* coms = comnamer->GetName();
    const char* comb = comnamer->GetBaseClass();

    char coreclass[CHARBUFSIZE];

    char Coms[64], Comb[64];
    char Arg1s[64], Arg2s[64];
    char Arg1b[64], Arg2b[64];


    *Arg1s = '\0';
    *Arg2s = '\0';
    
    /* some sscanfs need writable string */
    char* writable_com = strnew(coms);
    sscanf(writable_com, "%s%s%s", Coms, Arg1s, Arg2s);
    delete writable_com;
    writable_com = strnew(comb);
    sscanf(writable_com, "%s%s%s", Comb, Arg1b, Arg2b);
    delete writable_com;

    strcpy(coreclass, Coms);
    strcat(coreclass, "_core");

    out << Coms << "::" << Coms << "(\n";
    out << "    ControlInfo* ctrlinfo";

    _emitToolInits = true;

    ok = ok && EmitParameters(Comb, out);
    out << "\n) : " << coreclass << "(ctrlinfo";

    _emitBaseclass = true;
    ok = ok && EmitParameters(Comb, out);
    _emitBaseclass = false;

    _emitToolInits = false;

    out << ") {}\n\n";

    return out.good() && ok;
}

boolean CommandCtrlCode::EmitParameters(const char* com, ostream& out) {
    if (strcmp(com, "AlignCmd") == 0) {
        if (_emitToolInits) {
            if (_emitBaseclass) {
                out << ", a1, a2";
            } else {
                out << ", Alignment a1, Alignment a2";
            }
        } else {
            out << ", Alignment = Left, Alignment = Left";
        }
    } else if (strncmp(com, "ColorCmd", 8) == 0) {
        if (_emitToolInits) {
            if (_emitBaseclass) {
                out << ", fg, bg";
            } else {
                out << ", PSColor* fg, PSColor* bg";
            }
        } else {
            out << ", PSColor* fg = nil, PSColor* bg = nil";
        }
    } else if (strcmp(com, "BrushCmd") == 0) {
        if (_emitToolInits) {
            if (_emitBaseclass) {
                out << ", psbrush";
            } else {
                out << ", PSBrush* psbrush";
            }
        } else {
            out << ", PSBrush* = nil";
        }
    } else if (
        strcmp(com, "CutCmd") == 0 || strcmp(com, "CopyCmd") == 0 ||
        strcmp(com, "PasteCmd") == 0 || 
        strcmp(com, "DupCmd") == 0 || strcmp(com, "DeleteCmd") == 0
    ) {
        if (_emitToolInits) {
            if (_emitBaseclass) {
                out << ", cb";
            } else {
                out << ", Clipboard* cb";
            }
        } else {
            out << ", Clipboard* = nil";
        }
    } else if (strcmp(com, "ConnectCmd") == 0) {
        if (_emitToolInits) {
            if (_emitBaseclass) {
                out << ", c1, c2";
            } else {
                out << ", Connector* c1, Connector* c2";
            }
        } else {
            out << ", Connector* = nil, Connector* = nil";
        }
    } else if (strcmp(com, "MobilityCmd") == 0) {
        if (_emitToolInits) {
            if (_emitBaseclass) {
                out << ", mb";
            } else {
                out << ", Mobility mb";
            }
        } else {
            out << ", Mobility = Fixed";
        }
    } else if (strcmp(com, "ReplaceCmd") == 0 || strcmp(com, "GroupCmd") == 0){
        if (_emitToolInits) {
            if (_emitBaseclass) {
                out << ", grcomp";
            } else {
                out << ", GraphicComp* grcomp";
            } 
        } else {
            out << ", GraphicComp* = nil";
        }
    } else if (strcmp(com, "FontCmd") == 0) {
        if (_emitToolInits) {
            if (_emitBaseclass) {
                out << ", psfont";
            } else {
                out << ", PSFont* psfont";
            } 
        } else {
            out << ", PSFont* = nil";
        }
    } else if (strcmp(com, "PatternCmd") == 0) {
        if (_emitToolInits) {
            if (_emitBaseclass) {
                out << ", pspattern";
            } else {
                out << ", PSPattern* pspattern";
            }
        } else {
            out << ", PSPattern* = nil";
        }
    } else if (
        strcmp(com, "ImportCmd") == 0 || strcmp(com, "ViewCompCmd") == 0 ||
        strcmp(com, "SaveCompAsCmd") == 0
    ) {
        if (_emitToolInits) {
            if (_emitBaseclass) {
                out << ", fc";
            } else {
                out << ", FileChooser* fc";
            }
        } else {
            out << ", FileChooser* = nil";
        }
    } else if (strcmp(com, "PrintCmd") == 0) {
        if (_emitToolInits) {
            if (_emitBaseclass) {
                out << ", pdialog";
            } else {
                out << ", PrintDialog* pdialog";
            }
        } else {
            out << ", PrintDialog* = nil";
        }
    } else if (strcmp(com, "MoveCmd") == 0) {
        if (_emitToolInits) {
            if (_emitBaseclass) {
                out << ", m1, m2";
            } else {
                out << ", float m1, float m2";
            }
        } else {
            out << ", float = 0.0, float = 0.0";
        }
    } else if (strcmp(com, "ScaleCmd") == 0) {
        if (_emitToolInits) {
            if (_emitBaseclass) {
                out << ", s1, s2";
            } else {
                out << ", float s1, float s2";
            }
        } else {
            out << ", float = 1.0, float = 1.0, Alignment = Center";
        }
    } else if (strcmp(com, "RotateCmd") == 0) {
        if (_emitToolInits) {
            if (_emitBaseclass) {
                out << ", r";
            } else {
                out << ", float r";
            }
        } else {
            out << ", float = 0.0";
        }
    } else if (strcmp(com, "NewCompCmd") == 0) {
        if (_emitToolInits) {
            if (_emitBaseclass) {
                out << ", prototype";
            } else {
                out << ", Component* prototype";
            }
        } else {
            out << ", Component* prototype = nil";
        }
    } 
    return out.good();
}

boolean CommandCtrlCode::EmitIncludeHeaders(ostream& out) {
    boolean ok = true;
    GrBlockCode::EmitIncludeHeaders(out);
    CommandCtrlComp* ctrlComp = GetCommandCtrlComp();
    SubclassNameVar* cnamer = ctrlComp->GetToolName()->GetSubclass();
    const char* com = cnamer->GetName();
    SubclassNameVar* snamer = ctrlComp->GetClassNameVar();


    if (!snamer->IsSubclass() && !_namelist->Search("uctrls")) {
        _namelist->Append("uctrls");
        out << "#include <Unidraw/uctrls.h> \n\n";
    }
    if (strcmp(snamer->GetName(), _classname) != 0) {
        if (!_namelist->Search("ctrlinfo")) {
            _namelist->Append("ctrlinfo");
            out << "#include <Unidraw/ctrlinfo.h> \n";
        }
        if (!_namelist->Search("grcomp")) {
            _namelist->Append("grcomp");
            out << "#include <Unidraw/Components/grcomp.h>\n";
        }
        if (!cnamer->IsSubclass()) {
            ok = ok && EmitCommonHeaders(com, out);
        }
    }

    return out.good() && ok;
}

boolean CommandCtrlCode::EmitCommonHeaders(const char* com, ostream& out) {
    if (strncmp(com, "Align", 5) == 0 && !_namelist->Search("align")) {
        _namelist->Append("align");
        out << "#include <Unidraw/Commands/align.h>\n";
    }
    if (
        strcmp(com, "BackCmd") == 0 || strcmp(com, "FrontCmd") == 0 ||
        strcmp(com, "GroupCmd") == 0 || strcmp(com, "UngroupCmd") == 0
    ) {
        if (!_namelist->Search("struct")) {
            _namelist->Append("struct");
            out << "#include <Unidraw/Commands/struct.h>\n";
        }
    } else if (strcmp(com, "BrushCmd") == 0 && !_namelist->Search("brushcmd")){
        _namelist->Append("brushcmd");
        out << "#include <Unidraw/Commands/brushcmd.h>\n";
    } else if (
        strcmp(com, "NormSizeCmd") == 0 || strcmp(com, "RedToFitCmd") == 0 ||
        strcmp(com, "CenterCmd") == 0 || strncmp(com, "Grid", 4) == 0 ||
        strcmp(com, "GravityCmd") == 0 || strcmp(com, "OrientationCmd") == 0 ||
        strcmp(com, "CloseEditorCmd") == 0
    ) {
        if (!_namelist->Search("viewcmds")) {
            _namelist->Append("viewcmds");
            out << "#include <Unidraw/Commands/viewcmds.h>\n";
        }
    } else if (
        strncmp(com, "ColorCmd", 8) == 0 && !_namelist->Search("colorcmd")
    ) {
        _namelist->Append("colorcmd");
        out << "#include <Unidraw/Commands/colorcmd.h>\n";
    } else if (
        strcmp(com, "UndoCmd") == 0 || strcmp(com, "RedoCmd") == 0 ||
        strcmp(com, "CutCmd") == 0 || strcmp(com, "CopyCmd") == 0 ||
        strcmp(com, "PasteCmd") == 0 || strcmp(com, "ReplaceCmd") == 0 ||
        strcmp(com, "DupCmd") == 0 || strcmp(com, "DeleteCmd") == 0 ||
        strcmp(com, "SlctAllCmd") == 0 || strcmp(com, "ConnectCmd") == 0 ||
        strcmp(com, "MobilityCmd") == 0
    ) {
        if (!_namelist->Search("edit")) {
            _namelist->Append("edit");
            out << "#include <Unidraw/Commands/edit.h>\n";
        }
    } else if (strncmp(com, "FontCmd", 8) == 0 && !_namelist->Search("font")) {
        _namelist->Append("font");
        out << "#include <Unidraw/Commands/font.h>\n";
    } else if (
        strncmp(com, "ImportCmd", 8) == 0 && !_namelist->Search("import")
    ) {
        _namelist->Append("import");
        out << "#include <Unidraw/Commands/import.h>\n";
    } else if (
        strncmp(com, "ColorCmd", 8) == 0 && !_namelist->Search("colorcmd")
    ) {
        _namelist->Append("colorcmd");
        out << "#include <Unidraw/Commands/colorcmd.h>\n";
    } else if (
        strcmp(com, "NewCompCmd") == 0 || strcmp(com, "RevertCmd") == 0 ||
        strcmp(com, "ViewCompCmd") == 0 || strcmp(com, "SaveCompCmd") == 0 ||
        strcmp(com, "SaveCompAsCmd") == 0 || strcmp(com, "PrintCmd") == 0 ||
        strcmp(com, "QuitCmd") == 0 
    ) {
        if (!_namelist->Search("catcmds")) {
            _namelist->Append("catcmds");
            out << "#include <Unidraw/Commands/catcmds.h>\n";
        }
    } else if (
        strncmp(com, "MoveCmd", 7) == 0 || strncmp(com, "ScaleCmd", 8) == 0 ||
        strncmp(com, "RotateCmd", 9) == 0
    ) {
        if (!_namelist->Search("transforms")) {
            _namelist->Append("transforms");
            out << "#include <Unidraw/Commands/transforms.h>\n";
        }
    } else if (strcmp(com, "NOPCmd") == 0 && !_namelist->Search("nop")) {
        _namelist->Append("nop");
        out << "#include <Unidraw/Commands/nop.h>\n";
    } else if (strcmp(com, "BrushCmd") == 0 && strcmp(com, "BrushCmd") == 0) {
        if (!_namelist->Search("brushcmd")) {
            _namelist->Append("brushcmd");
            out << "#include <Unidraw/Commands/brushcmd.h>\n";
        } 
    } else if (
        strcmp(com, "PatternCmd") == 0 && strcmp(com, "PatternCmd") == 0
    ) {
        if (!_namelist->Search("patcmd")) {
            _namelist->Append("patcmd");
            out << "#include <Unidraw/Commands/patcmd.h>\n";
        } 
    }

    return out.good();
}
