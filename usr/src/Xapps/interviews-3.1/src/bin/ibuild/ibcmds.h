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
 * User interface builder-specific commands.
 */

#ifndef ibcmds_h
#define ibcmds_h

#include <Unidraw/umap.h>
#include <Unidraw/Commands/brushcmd.h>
#include <Unidraw/Commands/catcmds.h>
#include <Unidraw/Commands/colorcmd.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/font.h>
#include <Unidraw/Commands/import.h>
#include <Unidraw/Commands/patcmd.h>
#include <Unidraw/Commands/struct.h>

class Clipboard;
class CloneMap;
class CodeView;
class ConflictDialog;
class GraphicComp;
class GraphicView;
class GrBlockComp;
class IBShape;
class IComp;
class InfoData;
class InfoDialog;
class InstallRemoveDialog;
class InstanceNameVar;
class InteractorComp;
class InteractorView;
class MemberNameVar;
class MonoSceneComp;
class MonoSceneClass;
class MoveDialog;
class OptionDialog;
class RotateDialog;
class SaveCompAsCmd;
class ScaleDialog;
class SceneComp;
class StateVar;
class SubclassNameVar;
class UList;

class IDMapCmd : public Command {
public:
    IDMapCmd(Editor*);
    virtual void Execute();
    
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class IBImportCmd : public ImportCmd {
public:
    IBImportCmd(ControlInfo*);
    IBImportCmd(Editor* = nil);

    virtual void Execute();
    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class PreciseMoveCmd : public Command {
public:
    PreciseMoveCmd(ControlInfo*);
    PreciseMoveCmd(Editor* = nil);
    virtual ~PreciseMoveCmd();

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    MoveDialog* _dialog;
};

class PreciseScaleCmd : public Command {
public:
    PreciseScaleCmd(ControlInfo*);
    PreciseScaleCmd(Editor* = nil);
    virtual ~PreciseScaleCmd();

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    ScaleDialog* _dialog;
};

class PreciseRotateCmd : public Command {
public:
    PreciseRotateCmd(ControlInfo*);
    PreciseRotateCmd(Editor* = nil);
    virtual ~PreciseRotateCmd();

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    RotateDialog* _dialog;
};

class GetClonesCmd : public Command {
public:
    GetClonesCmd(MonoSceneClass*);
    virtual ~GetClonesCmd();

    MonoSceneClass* GetOriginal();
    UList* GetCloneList();

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    MonoSceneClass* _orig;
    UList* _clone;
};

inline MonoSceneClass* GetClonesCmd::GetOriginal () { return _orig; }
inline UList* GetClonesCmd::GetCloneList () { return _clone; }
inline boolean GetClonesCmd::Reversible () { return false; }

class GetFirewallCmd : public Command {
public:
    GetFirewallCmd (GraphicComp*);

    void SetFirewall(InteractorComp*);
    InteractorComp* GetFirewall();

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    GraphicComp* _target;
    InteractorComp* _firewall;
};

inline void GetFirewallCmd::SetFirewall (InteractorComp* firewall) {
    _firewall = firewall; 
}
inline InteractorComp* GetFirewallCmd::GetFirewall () { return _firewall; }
inline boolean GetFirewallCmd::Reversible () { return false; }

class GetTopLevelCmd : public Command {
public:
    GetTopLevelCmd (GraphicComp*);

    void SetTopLevel(InteractorComp*);
    InteractorComp* GetTopLevel();

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    GraphicComp* _target;
    InteractorComp* _toplevel;
};

inline void GetTopLevelCmd::SetTopLevel (InteractorComp* toplevel) {
    _toplevel = toplevel; 
}
inline InteractorComp* GetTopLevelCmd::GetTopLevel () { return _toplevel; }
inline boolean GetTopLevelCmd::Reversible () { return false; }

class GetConflictCmd : public Command {
public:
    GetConflictCmd (GraphicComp*, const char*, boolean global = false);
    virtual ~GetConflictCmd();

    UList* GetConflict();
    const char* GetCName();
    boolean IsGlobal();

    InteractorComp* GetCTarget();
    void SetCTarget(InteractorComp*);

    boolean GetScope();
    void SetScope(boolean);

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    InteractorComp* _ctarget;
    GraphicComp* _target;
    UList* _conflictlist;
    char* _cname;
    boolean _global;
    boolean _scope;
};

inline UList* GetConflictCmd::GetConflict () { return _conflictlist; }
inline const char* GetConflictCmd::GetCName () { return _cname; }
inline boolean GetConflictCmd::IsGlobal () { return _global; }
inline boolean GetConflictCmd::Reversible () { return false; }
inline boolean GetConflictCmd::GetScope () { return _scope; }
inline void GetConflictCmd::SetScope (boolean scope) { _scope = scope; }
inline InteractorComp* GetConflictCmd::GetCTarget () { return _ctarget; }
inline void GetConflictCmd::SetCTarget (InteractorComp* ctarget) {
    _ctarget = ctarget;
}

class GetNameVarsCmd : public Command {
public:
    GetNameVarsCmd(GraphicComp*);
    virtual ~GetNameVarsCmd();

    SubclassNameVar* GetClassNameVar();
    MemberNameVar* GetMemberNameVar();
    InstanceNameVar* GetInstanceNameVar();

    void SetClassNameVar(SubclassNameVar*);
    void SetMemberNameVar(MemberNameVar*);
    void SetInstanceNameVar(InstanceNameVar*);

    void AppendExtras(StateVar*);
    UList* GetExtras();

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    GraphicComp* _target;
    SubclassNameVar* _subclass;
    MemberNameVar* _member;
    InstanceNameVar* _instance;
    UList* _extras;
};

inline SubclassNameVar* GetNameVarsCmd::GetClassNameVar (){ return _subclass; }
inline MemberNameVar* GetNameVarsCmd::GetMemberNameVar () { return _member; }
inline InstanceNameVar* GetNameVarsCmd::GetInstanceNameVar () {
    return _instance; 
}
inline void GetNameVarsCmd::SetClassNameVar(SubclassNameVar* s) {
    _subclass = s;
}
inline void GetNameVarsCmd::SetMemberNameVar(MemberNameVar* m) {
    _member = m;
}
inline void GetNameVarsCmd::SetInstanceNameVar(InstanceNameVar* i) {
    _instance = i;
}
inline UList* GetNameVarsCmd::GetExtras () { return _extras; }
inline boolean GetNameVarsCmd::Reversible () { return false; }

class ScanCmd : public Command {
public:
    ScanCmd(GraphicComp*, const char* classname, ClassId);
    virtual ~ScanCmd();

    const char* GetClassName();
    ClassId GetTargetId();

    boolean GetScope();
    void SetScope(boolean);

    boolean Succeeded();
    void SetSucceeded(boolean);

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    GraphicComp* _target;
    char* _classname;
    ClassId _classid;
    boolean _succeeded;
    boolean _scope;
};

inline const char* ScanCmd::GetClassName () { return _classname; }
inline ClassId ScanCmd::GetTargetId () { return _classid; }
inline boolean ScanCmd::GetScope () { return _scope; }
inline void ScanCmd::SetScope (boolean scope) { _scope = scope; }
inline boolean ScanCmd::Succeeded () { return _succeeded; }
inline void ScanCmd::SetSucceeded (boolean succeeded) {_succeeded =succeeded; }
inline boolean ScanCmd::Reversible () { return false; }

class AboutCmd : public Command {
public:
    AboutCmd(ControlInfo*);
    AboutCmd(Editor* = nil);

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class TabCmd : public Command {
public:
    TabCmd(ControlInfo*);
    TabCmd(Editor* = nil);

    virtual Command* Copy();
    virtual boolean Reversible();
    virtual void Execute();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class NewViewCmd : public Command {
public:
    NewViewCmd(ControlInfo*, GraphicComp* = nil);
    NewViewCmd(Editor* = nil, GraphicComp* = nil);

    virtual void Execute();
    virtual boolean Reversible();

    GraphicComp* GetGraphicComp();
    void SetGraphicComp(GraphicComp*);

    virtual Command* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    GraphicComp* _comp;
};

inline GraphicComp* NewViewCmd::GetGraphicComp () { return _comp; }
inline void NewViewCmd::SetGraphicComp (GraphicComp* comp) { _comp = comp; }

class NavigateCmd : public Command {
public:
    NavigateCmd(
        ControlInfo*, boolean root = false,
        GraphicComp* parent = nil, GraphicComp* kid = nil
    );
    NavigateCmd(
        Editor* = nil, boolean root = nil,
        GraphicComp* parent = nil, GraphicComp* kid = nil
    );

    virtual void Execute();
    virtual void Unexecute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    GraphicComp* _parent;
    GraphicComp* _kid;
    GraphicComp* _oparent;
    GraphicComp* _okid;
    boolean _root;
    boolean _reversible;
};

class PlaceCmd : public Command {
public:
    PlaceCmd(ControlInfo*, Clipboard* = nil);
    PlaceCmd(Editor* = nil, Clipboard* = nil);
    PlaceCmd(Editor*, Coord, Coord, Coord, Coord, Clipboard* = nil);

    virtual void Execute();
    virtual void Unexecute();

    boolean Placement(Coord&, Coord&, Coord&, Coord&);

    virtual Command* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    int _natural;
    Coord _l, _b, _r, _t;
};

class GlueVisibilityCmd : public BrushCmd {
public:
    GlueVisibilityCmd(ControlInfo*, boolean visible = true);
    GlueVisibilityCmd(Editor* = nil, boolean = true);

    boolean Visible();
   
    virtual void Execute();
    virtual void Unexecute();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class SceneCmd : public GroupCmd {
public:
    SceneCmd(ControlInfo*, SceneComp* dest = nil);
    SceneCmd(Editor* = nil, SceneComp* dest = nil);

    virtual ~SceneCmd();

    Command* GetLogCmd();
    virtual void Execute();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    Command* _log;
};

inline Command* SceneCmd::GetLogCmd () { return _log; }

class MonoSceneCmd : public GroupCmd {
public:
    MonoSceneCmd(ControlInfo*, MonoSceneComp* dest = nil);
    MonoSceneCmd(Editor* = nil, MonoSceneComp* dest = nil);
    virtual ~MonoSceneCmd();

    Clipboard* GetMSClipboard();
    Command* GetLogCmd();

    virtual void Execute();
    virtual void Unexecute();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    Clipboard* _mcb;
private:
    Command* _log;
};

inline Clipboard* MonoSceneCmd::GetMSClipboard () { return _mcb; }
inline Command* MonoSceneCmd::GetLogCmd () { return _log; }


class RelateCmd : public Command {
public:
    RelateCmd(
	ControlInfo*, InteractorComp* src = nil, InteractorComp* dest = nil
    );
    RelateCmd(
	Editor* = nil, InteractorComp* src = nil, InteractorComp* dest = nil
    );

    virtual ~RelateCmd();

    virtual void Execute();
    virtual void Unexecute();

    void RecurRelate(InteractorComp*, InteractorComp*);
    void RecurUnrelate(InteractorComp*, InteractorComp*);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    CloneMap* _clonemap;
    InteractorComp* _src, *_dest;
};

class InfoCmd : public Command {
public:
    InfoCmd(ControlInfo*, GraphicView* view = nil);
    InfoCmd(Editor* = nil, GraphicView* view = nil);
    virtual ~InfoCmd();

    virtual boolean Reversible();
    virtual void Execute();
    virtual void Unexecute();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    boolean Accept();
    void SetIBShape(IBShape*);
    void SetOrigIBShape(IBShape*);
    void SetInfoDialog(InfoDialog*);
private:
    GraphicView* _view;
    PlaceCmd* _placeCmd;
    InfoData* _infoData;
    InfoDialog* _info;
    IBShape* _ibshape, *_oshape;
    UList* _iStates;
    boolean _reversible;
};

inline void InfoCmd::SetIBShape (IBShape* ibshape) { _ibshape = ibshape; }
inline void InfoCmd::SetOrigIBShape (IBShape* oshape) { _oshape = oshape; }
inline void InfoCmd::SetInfoDialog (InfoDialog* info) { _info = info; }

class PropsCmd : public Command {
public:
    PropsCmd(ControlInfo*, InteractorComp*);
    PropsCmd(Editor* = nil, InteractorComp* = nil);

    virtual boolean Reversible();
    virtual void Execute();
    virtual void Unexecute();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    InteractorComp* _icomp;
    boolean _reversible;
};

class IdrawCmd : public Command {
public:
    IdrawCmd(ControlInfo*, GrBlockComp*);
    IdrawCmd(Editor* = nil, GrBlockComp* = nil);
    virtual ~IdrawCmd();

    virtual boolean Reversible();
    virtual void Execute();
    virtual void Unexecute();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    GrBlockComp* _grblock;
    PlaceCmd* _placeCmd;
    boolean _reversible;
};

inline boolean IdrawCmd::Reversible () { return _reversible; }

class CodeCmd : public Command {
public:
    CodeCmd(ControlInfo*);
    CodeCmd(Editor* = nil);
    virtual ~CodeCmd();

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    void Init();
    void DoIt(InteractorComp*);
    void Make(const char*);

    boolean GenFile(const char*, CodeView*);
    boolean GenFiles(const char*, CodeView*);
    boolean GenMultipleFiles(const char*, CodeView*);

    boolean GenMakeFile(const char*, CodeView*);
    boolean GenIMakeFile(const char*, CodeView*);
    boolean GenCreatorh(const char*, CodeView*);
    boolean GenCreatorc(const char*, CodeView*);
    boolean GenDothFile(const char*, CodeView*);
    boolean GenDotcFile(const char*, CodeView*);
    boolean GenPropFile(const char*, CodeView*);
    boolean GenMainFile(const char*, CodeView*);
    boolean GenCorehFile(const char*, CodeView*);
    boolean GenCorecFile(const char*, CodeView*);

    boolean CheckConflicts(
        ConflictDialog*&,
        const char*, boolean,
        const char*, boolean,
        const char* = "", boolean = true,
        const char* = "", boolean = true
    );
private:
    SaveCompAsCmd* _save_cmd;
    OptionDialog* _option;
    UList* _dialogList;
    char _errbuf[CHARBUFSIZE*10];
    char _dotc[8];
};

class ExeDialog;

class ExeCmd : public Command {
public:
    ExeCmd(ControlInfo*);
    ExeCmd(Editor* = nil);

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    ExeDialog* _dialog;
};

class NewToolCmd : public Command {
public:
    NewToolCmd(ControlInfo*);
    NewToolCmd(Editor* = nil);
    virtual ~NewToolCmd();

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    boolean CreateCtrlInfo (GraphicComp*, char*, ControlInfo*&, int);
private:
    void InitNewTool(const char*, int);
private:
    OptionDialog* _option;
    SaveCompAsCmd* _scmd;
};

class ToolsCmd : public Command {
public:
    ToolsCmd(ControlInfo*);
    ToolsCmd(Editor* = nil);

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class EditCmd : public Command {
public:
    EditCmd(ControlInfo*, Clipboard* = nil, const char* = nil);
    EditCmd(Editor* = nil, Clipboard* = nil, const char* = nil);
    EditCmd(Editor*, Clipboard* = nil, GraphicComp* = nil);
    virtual ~EditCmd();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    const char* GetOldText();
    void SetOldText(const char*);

    const char* GetNewText();
    void SetNewText(const char*);

    GraphicComp* SwapComp(GraphicComp*);
private:
    char* _oldtext;
    char* _newtext;
    GraphicComp* _replacee;
};

inline const char* EditCmd::GetOldText() { return _oldtext; }
inline const char* EditCmd::GetNewText() { return _newtext; }

class IBViewCompCmd : public ViewCompCmd {
public:
    IBViewCompCmd(ControlInfo*, FileChooser* = nil);
    virtual void Execute();
    static boolean Executing();
private:
    static boolean _executing;
};

inline boolean IBViewCompCmd::Executing () { return _executing; }

class ReorderCmd : public Command {
public:
    ReorderCmd(ControlInfo*);
    ReorderCmd(Editor* = nil);
    virtual ~ReorderCmd();

    Command* GetLogCmd();

    virtual boolean Reversible();
    virtual void Execute();
    virtual void Unexecute();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    Command* _log;
    boolean _executed;
};

inline Command* ReorderCmd::GetLogCmd () { return _log; }
inline boolean ReorderCmd::Reversible () { return true; }

class CompCheckCmd : public Command {
public:
    CompCheckCmd(IComp*, const char* c, const char* v, const char* g);
    virtual ~CompCheckCmd();

    void Check(boolean);
    boolean IsOK();
    const char* GetCName();
    const char* GetVName();
    const char* GetGName();
    IComp* GetTarget();
    
    virtual boolean Reversible();
    virtual void Execute();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    boolean _ok;
    IComp* _target;
    char* _c, *_v, *_g;
};

inline void CompCheckCmd::Check (boolean ok) { _ok = ok; }
inline boolean CompCheckCmd::IsOK () { return _ok; }
inline IComp* CompCheckCmd::GetTarget () { return _target; }
inline boolean CompCheckCmd::Reversible () { return false; }
inline const char* CompCheckCmd::GetCName () { return _c; }
inline const char* CompCheckCmd::GetVName () { return _v; }
inline const char* CompCheckCmd::GetGName () { return _g; }

#endif

