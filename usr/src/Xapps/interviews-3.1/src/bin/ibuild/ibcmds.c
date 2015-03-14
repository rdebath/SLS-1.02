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
 * Implementation of user interface builder-specific commands.
 */

#include "ibbitmap.h"
#include "ibbutton.h"
#include "ibclasses.h"
#include "ibcmds.h"
#include "ibcode.h"
#include "ibcomp.h"
#include "ibcreator.h"
#include "ibdialogs.h"
#include "ibed.h"
#include "ibframe.h"
#include "ibglobals.h"
#include "ibgrblock.h"
#include "ibgrcomp.h"
#include "ibprocs.h"
#include "ibraster.h"
#include "ibscene.h"
#include "ibstencil.h"
#include "ibtoolpanel.h"
#include "ibtools.h"
#include "ibvars.h"
#include "ibversion.h"

#include <Unidraw/catalog.h>
#include <Unidraw/clipboard.h>
#include <Unidraw/ctrlinfo.h>
#include <Unidraw/editorinfo.h>
#include <Unidraw/editor.h>
#include <Unidraw/globals.h>
#include <Unidraw/iterator.h>
#include <Unidraw/selection.h>
#include <Unidraw/statevars.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>
#include <Unidraw/creator.h>
#include <Unidraw/ulist.h>

#include <Unidraw/Commands/catcmds.h>
#include <Unidraw/Commands/datas.h>
#include <Unidraw/Commands/transforms.h>

#include <Unidraw/Components/grcomp.h>

#include <Unidraw/Graphic/damage.h>
#include <Unidraw/Graphic/graphic.h>
#include <Unidraw/Graphic/pspaint.h>
#include <Unidraw/Graphic/util.h>

#include <Unidraw/Tools/grcomptool.h>

#include <IV-look/dialogs.h>
#include <InterViews/bitmap.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <IV-2_6/InterViews/frame.h>
#include <IV-2_6/InterViews/message.h>
#include <IV-2_6/InterViews/sensor.h>
#include <IV-2_6/InterViews/shape.h>
#include <OS/types.h>

#undef FileChooser
#define FileChooser _lib_iv(FileChooser)

#include <osfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <stream.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/wait.h>

#if defined(sgi) || defined(sco)
/* not all vforks are alike */
#define vfork fork
#endif

/*****************************************************************************/

enum {Gen_File, Gen_Files, Gen_Make}; 

/*****************************************************************************/

// 4.2 -> hpux standard conversion of getwd
#if defined(hpux) || defined(sco) || (defined(sun) && OSMajorVersion >= 5)
#include <sys/param.h>
#define  getwd(a) getcwd(a,MAXPATHLEN)
#endif

#ifdef __DECCXX
extern "C" {
    int getwd(char*);
    int vfork();
    int execlp(char*, ...);
    int unlink(char*);
    pid_t fork();
    void _exit(int);
}
#endif

#ifdef __GNUC__
extern "C" {
    char* getwd(char*);
}
#endif

/*****************************************************************************/

char* GetDirName (const char* fullname) {
    static char retval[CHARBUFSIZE];
    char pathname[CHARBUFSIZE];
    strcpy(retval, fullname);
    if (retval[0] == '.' && retval[1] == '.') {
        getwd(pathname);
        sprintf(retval, "%s/%s", pathname, fullname);
    }
    char* tmp = strrchr(retval, '/');
    if (tmp != nil) {
        tmp[1] = '\0';
    } else {
        strcpy(retval, "./");
    }
    if (retval[0] == '.' && retval[1] == '/') {
        char buf[CHARBUFSIZE];
        strcpy(buf, retval);
        getwd(pathname);
        sprintf(retval, "%s/%s", pathname, &buf[2]);
    }
    return retval;
}

char* GetOrigName (const char* fullname) {
    static char retval[CHARBUFSIZE];
    char* orig = strrchr(fullname, '/');
    if (orig == nil) {
        strcpy(retval, fullname);
    } else {
        strcpy(retval, &orig[1]);
    }
    return retval;
}

/*****************************************************************************/

static void Warning (Editor* ed, const char* warning) {
    StringBrowserDialog dialog(warning);
    ed->InsertDialog(&dialog);
    dialog.Acknowledge();
    ed->RemoveDialog(&dialog);
}

static boolean Abort (Editor* ed, int pid, const char* warning) {
    boolean aborted;
    AbortDialog dialog(pid, warning);
    ed->InsertDialog(&dialog);
    aborted = dialog.Abort();
    ed->RemoveDialog(&dialog);
    return aborted;
}

/*****************************************************************************/

IDMapCmd::IDMapCmd (Editor* ed) : Command(ed) {}

void IDMapCmd::Execute () {
    IDVar::CreateMap();
    GetEditor()->GetComponent()->GetRoot()->Interpret(this);
}

ClassId IDMapCmd::GetClassId () { return IDMAP_CMD; }

boolean IDMapCmd::IsA (ClassId id) {
    return IDMAP_CMD == id || Command::IsA(id);
}

/*****************************************************************************/

ClassId IBImportCmd::GetClassId () { return IBIMPORT_CMD; }

boolean IBImportCmd::IsA (ClassId id) {
    return IBIMPORT_CMD == id || ImportCmd::IsA(id);
}

IBImportCmd::IBImportCmd (ControlInfo* c) : ImportCmd(c) {}
IBImportCmd::IBImportCmd (Editor* ed) : ImportCmd(ed) {}

Command* IBImportCmd::Copy () {
    Command* copy = new IBImportCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void IBImportCmd::Execute () {
    IComp::SetRelease(false);
    IBCreator::SetLock(false);
    GraphicComp* comp = PostDialog();

    if (comp != nil) {
        if (comp->IsA(RASTER_COMP)) {
            Graphic* gr = comp->GetGraphic()->Copy();
            delete comp;
            comp = new IRasterComp((RasterRect*) gr);
        } else if (comp->IsA(STENCIL_COMP)) {
            Graphic* gr = comp->GetGraphic()->Copy();
            delete comp;
            comp = new IStencilComp((UStencil*) gr);
        }
        IComp* icomp = (IComp*) comp;
        icomp->Instantiate();
    }
    if (comp != nil) {
        PasteCmd* paste_cmd = new PasteCmd(GetEditor(), new Clipboard(comp));
        paste_cmd->Execute();
        paste_cmd->Log();
        GetEditor()->GetViewer()->Align(comp, Center);
    }
    IBCreator::SetLock(true);
    IComp::SetRelease(true);
}
/*****************************************************************************/

ClassId PreciseMoveCmd::GetClassId () { return PRECISEMOVE_CMD; }

boolean PreciseMoveCmd::IsA (ClassId id) {
    return PRECISEMOVE_CMD == id || Command::IsA(id);
}

PreciseMoveCmd::PreciseMoveCmd (ControlInfo* c) : Command(c) { _dialog = nil; }
PreciseMoveCmd::PreciseMoveCmd (Editor* ed) : Command(ed) { _dialog = nil; }
PreciseMoveCmd::~PreciseMoveCmd () { delete _dialog; }

Command* PreciseMoveCmd::Copy () {
    Command* copy = new PreciseMoveCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void PreciseMoveCmd::Execute () {
    float dx = 0.0, dy = 0.0;
    Editor* ed = GetEditor();

    if (_dialog == nil) {
	_dialog = new MoveDialog();
    }

    ed->InsertDialog(_dialog);
    boolean accepted = _dialog->Accept();
    ed->RemoveDialog(_dialog);

    if (accepted) {
	_dialog->GetValues(dx, dy);

	if (dx != 0.0 || dy != 0.0) {
	    MoveCmd* moveCmd = new MoveCmd(ed, dx, dy);
	    moveCmd->Execute();
	    moveCmd->Log();
	}
    }
}

boolean PreciseMoveCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId PreciseScaleCmd::GetClassId () { return PRECISESCALE_CMD; }

boolean PreciseScaleCmd::IsA (ClassId id) {
    return PRECISESCALE_CMD == id || Command::IsA(id);
}

PreciseScaleCmd::PreciseScaleCmd (ControlInfo* c) : Command(c) {_dialog = nil;}
PreciseScaleCmd::PreciseScaleCmd (Editor* ed) : Command(ed) { _dialog = nil; }
PreciseScaleCmd::~PreciseScaleCmd () { delete _dialog; }

Command* PreciseScaleCmd::Copy () {
    Command* copy = new PreciseScaleCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void PreciseScaleCmd::Execute () {
    float x = 0.0, y = 0.0;
    Editor* ed = GetEditor();

    if (_dialog == nil) {
	_dialog = new ScaleDialog();
    }

    ed->InsertDialog(_dialog);
    boolean accepted = _dialog->Accept();
    ed->RemoveDialog(_dialog);

    if (accepted) {
	_dialog->GetValues(x, y);
	if (x != 0.0 && y != 0.0) {
	    ScaleCmd* scaleCmd = new ScaleCmd(ed, x, y);
	    scaleCmd->Execute();
	    scaleCmd->Log();
	}
    }
}

boolean PreciseScaleCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId PreciseRotateCmd::GetClassId () { return PRECISEROTATE_CMD; }

boolean PreciseRotateCmd::IsA (ClassId id) {
    return PRECISEROTATE_CMD == id || Command::IsA(id);
}

PreciseRotateCmd::PreciseRotateCmd (ControlInfo* c) : Command(c) {_dialog=nil;}
PreciseRotateCmd::PreciseRotateCmd (Editor* ed) : Command(ed) { _dialog = nil;}
PreciseRotateCmd::~PreciseRotateCmd () { delete _dialog; }

Command* PreciseRotateCmd::Copy () {
    Command* copy = new PreciseRotateCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void PreciseRotateCmd::Execute () {
    float angle = 0.0;
    Editor* ed = GetEditor();

    if (_dialog == nil) {
	_dialog = new RotateDialog();
    }

    ed->InsertDialog(_dialog);
    boolean accepted = _dialog->Accept();
    ed->RemoveDialog(_dialog);

    if (accepted) {
	_dialog->GetValue(angle);
	if (angle != 0.0) {
	    RotateCmd* rotateCmd = new RotateCmd(ed, angle);
	    rotateCmd->Execute();
	    rotateCmd->Log();
	}
    }
}

boolean PreciseRotateCmd::Reversible () { return false; }

/*****************************************************************************/

GetClonesCmd::GetClonesCmd (MonoSceneClass* orig) {
    _orig = orig;
    _clone = new UList;
}

GetClonesCmd::~GetClonesCmd () {
    delete _clone;
}

void GetClonesCmd::Execute () {
    Component* root = _orig->GetRoot();
    root->Interpret(this);
}

Command* GetClonesCmd::Copy () {
    return new GetClonesCmd(_orig);
}

ClassId GetClonesCmd::GetClassId () { return GETCLONES_CMD; }

boolean GetClonesCmd::IsA (ClassId id) {
    return GETCLONES_CMD == id || Command::IsA(id);
}

/*****************************************************************************/

GetFirewallCmd::GetFirewallCmd (GraphicComp* target) {
    _target = target; 
    _firewall = nil;
}

void GetFirewallCmd::Execute () {
    if (_target != nil) {
        _target->Interpret(this);
    }
}

Command* GetFirewallCmd::Copy () {
    return new GetFirewallCmd(_target);
}

ClassId GetFirewallCmd::GetClassId () { return GETFIREWALL_CMD; }

boolean GetFirewallCmd::IsA (ClassId id) {
    return GETFIREWALL_CMD == id || Command::IsA(id);
}

/*****************************************************************************/

GetTopLevelCmd::GetTopLevelCmd (GraphicComp* target) {
    _target = target; 
    _toplevel = nil;
}

void GetTopLevelCmd::Execute () {
    _target->Interpret(this);
}

Command* GetTopLevelCmd::Copy () {
    return new GetTopLevelCmd(_target);
}

ClassId GetTopLevelCmd::GetClassId () { return GETTOPLEVEL_CMD; }

boolean GetTopLevelCmd::IsA (ClassId id) {
    return GETTOPLEVEL_CMD == id || Command::IsA(id);
}

/*****************************************************************************/

GetConflictCmd::GetConflictCmd (
    GraphicComp* target, const char* cname, boolean global
) {
    _ctarget = nil;
    _target = target; 
    _conflictlist = new UList;
    _cname = strnew(cname);
    _global = global;
    _scope = false;
}

GetConflictCmd::~GetConflictCmd () {
    delete _conflictlist;
    delete _cname;
}

void GetConflictCmd::Execute () {
    _scope = false;
    if (_target != nil) {
        _target->Interpret(this);
    }
}

Command* GetConflictCmd::Copy () {
    return new GetConflictCmd(_target, _cname, _global);
}

ClassId GetConflictCmd::GetClassId () { return GETCONFLICT_CMD; }

boolean GetConflictCmd::IsA (ClassId id) {
    return GETCONFLICT_CMD == id || Command::IsA(id);
}

/*****************************************************************************/

GetNameVarsCmd::GetNameVarsCmd (GraphicComp* target) {
    _target = target; 
    _subclass = nil;
    _member = nil;
    _instance = nil;
    _extras = new UList;
}

GetNameVarsCmd::~GetNameVarsCmd () {
    delete _extras;
}

void GetNameVarsCmd::AppendExtras(StateVar* svar) {
    _extras->Append(new UList(svar));
}

void GetNameVarsCmd::Execute () {
    if (_target != nil) {
        _target->Interpret(this);
    }
}

Command* GetNameVarsCmd::Copy () {
    return new GetNameVarsCmd(_target);
}

ClassId GetNameVarsCmd::GetClassId () { return GETNAMEVARS_CMD; }

boolean GetNameVarsCmd::IsA (ClassId id) {
    return GETNAMEVARS_CMD == id || Command::IsA(id);
}

/*****************************************************************************/

ScanCmd::ScanCmd (GraphicComp* target, const char* classname, ClassId id) {
    _target = target; 
    _classname = strnew(classname);
    _classid = id;
    _succeeded = false;
    _scope = false;
}

ScanCmd::~ScanCmd () {
    delete _classname;
}

void ScanCmd::Execute () {
    _scope = false;
    if (_target != nil) {
        _target->Interpret(this);
    }
}

Command* ScanCmd::Copy () {
    return new ScanCmd(_target, _classname, _classid);
}

ClassId ScanCmd::GetClassId () { return SCAN_CMD; }

boolean ScanCmd::IsA (ClassId id) {
    return SCAN_CMD == id || Command::IsA(id);
}

/*****************************************************************************/

class LogCmd : public Command {
public:
    LogCmd();
};

LogCmd::LogCmd () {}

/*****************************************************************************/

ClassId AboutCmd::GetClassId () { return ABOUT_CMD; }
boolean AboutCmd::IsA (ClassId id) {return ABOUT_CMD==id || Command::IsA(id);}

AboutCmd::AboutCmd (ControlInfo* c) : Command(c) { }
AboutCmd::AboutCmd (Editor* ed) : Command(ed) { }

Command* AboutCmd::Copy () {
    Command* copy = new AboutCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void AboutCmd::Execute () {
    Editor* ed = GetEditor();
    AcknowledgeDialog dialog(VERSION);

    ed->InsertDialog(&dialog);
    dialog.Acknowledge();
    ed->RemoveDialog(&dialog);
}

boolean AboutCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId TabCmd::GetClassId () { return TAB_CMD; }
boolean TabCmd::IsA (ClassId id) {return TAB_CMD==id || Command::IsA(id);}

TabCmd::TabCmd (ControlInfo* c) : Command(c) { }
TabCmd::TabCmd (Editor* ed) : Command(ed) { }

Command* TabCmd::Copy () {
    Command* copy = new TabCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void TabCmd::Execute () {
    Iterator i;
    Editor* ed = GetEditor();
    Viewer* viewer = ed->GetViewer();
    GraphicView* gv = viewer->GetGraphicView();
    Selection* gsel = viewer->GetSelection();
    GraphicView* target = nil;

    if (gsel->IsEmpty()) {
        gv->First(i);
        target = gv->GetView(i);
    } else {
        gsel->Last(i);
        target = gsel->GetView(i);
        gv->SetView(target, i);
        gv->Next(i);
        if (gv->Done(i)) {
            gv->First(i);
        }
        target = gv->GetView(i);
    }
    if (target != nil) {
        gsel->Clear();
        gsel->Append(target);
        gsel->Update();
        target->Interpret(this);
    }
}

boolean TabCmd::Reversible() { return false; }

/*****************************************************************************/

ClassId NewViewCmd::GetClassId () { return NEWVIEW_CMD; }

boolean NewViewCmd::IsA (ClassId id) {
    return NEWVIEW_CMD == id || Command::IsA(id);
}

NewViewCmd::NewViewCmd (ControlInfo* c, GraphicComp* comp) : Command(c) {
    _comp = comp;
}

NewViewCmd::NewViewCmd (Editor* ed, GraphicComp* comp) : Command(ed) {
    _comp = comp;
}

Command* NewViewCmd::Copy () {
    Command* copy = new NewViewCmd(CopyControlInfo(), GetGraphicComp());
    InitCopy(copy);
    return copy;
}

void NewViewCmd::Execute () {
    IBEditor* ed = (IBEditor*) GetEditor();
    GraphicComp* comp = (_comp == nil) ? Command::GetGraphicComp() : _comp;
    IBEditor* newEd = new IBEditor(comp);

    *newEd->GetState("ModifStatusVar") = *ed->GetState("ModifStatusVar");

    *newEd->GetState("FontVar") = *ed->GetState("FontVar");
    *newEd->GetState("BrushVar") = *ed->GetState("BrushVar");
    *newEd->GetState("ColorVar") = *ed->GetState("ColorVar");
    *newEd->GetState("PatternVar") = *ed->GetState("PatternVar");
    unidraw->Open(newEd);
}

boolean NewViewCmd::Reversible () { return false; }

void NewViewCmd::Read (istream& in) {
    Command::Read(in);
    _comp = (GraphicComp*) unidraw->GetCatalog()->ReadComponent(in);
}

void NewViewCmd::Write (ostream& out) {
    Command::Write(out); 
    unidraw->GetCatalog()->WriteComponent(GetGraphicComp(), out);
}

/*****************************************************************************/

static void SelectView(Viewer* viewer, GraphicComp* target) {
    
    Selection* sel = viewer->GetSelection();
    sel->Clear();

    GraphicView* gv = viewer->GetGraphicView();
    GraphicView* view = gv->GetGraphicView(target);
    if (view != nil) sel->Append(view);
    sel->Update();
}

/*****************************************************************************/

ClassId NavigateCmd::GetClassId () { return NAVIGATE_CMD; }

boolean NavigateCmd::IsA (ClassId id) {
    return NAVIGATE_CMD == id || Command::IsA(id);
}

NavigateCmd::NavigateCmd (
    ControlInfo* c, boolean root, GraphicComp* parent, GraphicComp* kid
) : Command(c) {
    _parent = parent;
    _kid = kid;
    _oparent = nil;
    _okid = nil;
    _root = root;
    _reversible = true;
}

NavigateCmd::NavigateCmd (
    Editor* ed, boolean root, GraphicComp* parent, GraphicComp* kid
) : Command(ed) {
    _parent = parent;
    _kid = kid;
    _oparent = nil;
    _okid = nil;
    _root = root;
    _reversible = true;
}

Command* NavigateCmd::Copy () {
    Command* copy = new NavigateCmd(CopyControlInfo(), _root, _parent, _kid);
    InitCopy(copy);
    return copy;
}

void NavigateCmd::Execute () {
    Iterator i;
    Editor* ed = GetEditor();
    Viewer* viewer = ed->GetViewer();
    _oparent = (GraphicComp*) ed->GetComponent();
    if (_okid == nil) {
        Selection* sel = ed->GetSelection();
        if (sel->Number() == 1) {
            sel->First(i);
            _okid = (GraphicComp*) sel->GetView(i)->GetGraphicComp();
            sel->Clear();
        }
    }

    if (_parent != nil) {
        if (_parent != _oparent) {
            ed->SetComponent(_parent);
        } else {
            _reversible = false;
            return;
        }
    } else if (_root) {
        GraphicComp* grcomp = (GraphicComp*) ed->GetComponent();
        GraphicComp* root = (GraphicComp*) grcomp->GetRoot();
        if (root != _oparent) {
            ed->SetComponent(root);
            GetTopLevelCmd toplevelCmd(grcomp);
            toplevelCmd.Execute();
            _kid = toplevelCmd.GetTopLevel();
        } else {
            _reversible = false;
            return;
        }
    } else {
        GraphicComp* icomp = (GraphicComp*) ed->GetComponent();
        GraphicComp* parent = (GraphicComp*) icomp->GetParent();
        if (parent != nil) {
            ed->SetComponent(parent);
            _kid = icomp;
        } else {
            _reversible = false;
            return;
        }
    }
    Component* comp = ed->GetComponent();
    CompNameVar* compNameVar = (CompNameVar*) ed->GetState("CompNameVar");
    if (compNameVar != nil) compNameVar->SetComponent(comp);

    ed->Update();
    comp->Notify();    // hack for menus
    unidraw->Update();
    if (_kid != nil) {
        SelectView(viewer, _kid);
    }
}

void NavigateCmd::Unexecute () {
    Editor* ed = GetEditor();
    Viewer* viewer = ed->GetViewer();
    Component* comp = ed->GetComponent();
    ed->SetComponent(_oparent);
    CompNameVar* compNameVar = (CompNameVar*) ed->GetState("CompNameVar");
    if (compNameVar != nil) compNameVar->SetComponent(_oparent);

    ed->Update();
    _oparent->Notify();
    unidraw->Update();
    if (_okid != nil) {
        SelectView(viewer, _okid);
    }
}

boolean NavigateCmd::Reversible () { return _reversible; }

/*****************************************************************************/

ClassId PlaceCmd::GetClassId () { return PLACE_CMD; }
boolean PlaceCmd::IsA (ClassId id) { return PLACE_CMD==id || Command::IsA(id);}

PlaceCmd::PlaceCmd (ControlInfo* c, Clipboard* cb) : Command(c, cb) {
    _natural = true;
}

PlaceCmd::PlaceCmd (Editor* ed, Clipboard* cb) : Command(ed,cb){
    _natural = true; 
}

PlaceCmd::PlaceCmd (
    Editor* ed, Coord l, Coord b, Coord r, Coord t, Clipboard* cb
) : Command(ed, cb) {
    _natural = false;
    _l = l; _b = b; _r = r; _t = t;
}

Command* PlaceCmd::Copy () {
    PlaceCmd* copy = new PlaceCmd(CopyControlInfo(), DeepCopyClipboard());
    InitCopy(copy);
    copy->_natural = _natural;
    copy->_l = _l; copy->_b = _b; copy->_r = _r; copy->_t = _t;
    return copy;
}

void PlaceCmd::Execute () {
    GetEditor()->GetComponent()->Interpret(this);
}

void PlaceCmd::Unexecute () {
    GetEditor()->GetComponent()->Uninterpret(this);
}

boolean PlaceCmd::Placement (Coord& l, Coord& b, Coord& r, Coord& t) {
    l = _l; b = _b; r = _r; t = _t;
    return !_natural;
}

void PlaceCmd::Read (istream& in) {
    Command::Read(in);
    in >> _natural >> _l >> _b >> _r >> _t;
}

void PlaceCmd::Write (ostream& out) {
    Command::Write(out);
    out << _natural << " " << _l << " " << _b << " " << _r << " " << _t << " ";
}

/*****************************************************************************/

ClassId GlueVisibilityCmd::GetClassId () { return GLUEVISIBILITY_CMD; }

boolean GlueVisibilityCmd::IsA (ClassId id) {
    return GLUEVISIBILITY_CMD == id || BrushCmd::IsA(id);
}

GlueVisibilityCmd::GlueVisibilityCmd (
    ControlInfo* c, boolean visible
) : BrushCmd(c, visible ? pssingle : psnonebr) { }

GlueVisibilityCmd::GlueVisibilityCmd (
    Editor* ed, boolean visible
) : BrushCmd(ed, visible ? pssingle : psnonebr) { }

boolean GlueVisibilityCmd::Visible () { return !GetBrush()->None(); }

Command* GlueVisibilityCmd::Copy () {
    Command* copy = new GlueVisibilityCmd(CopyControlInfo(), Visible());
    InitCopy(copy);
    return copy;
}

void GlueVisibilityCmd::Execute () {
    Command::Execute();
}

void GlueVisibilityCmd::Unexecute () {
    Command::Unexecute();
}

/*****************************************************************************/

ClassId SceneCmd::GetClassId () { return SCENE_CMD; }

boolean SceneCmd::IsA (ClassId id) {
    return SCENE_CMD == id || GroupCmd::IsA(id);
}

SceneCmd::SceneCmd (
    ControlInfo* c, SceneComp* dest
) : GroupCmd(c, dest) {
    _log = nil;
}

SceneCmd::SceneCmd (
    Editor* ed, SceneComp* dest
) : GroupCmd(ed, dest) {
    _log = nil;
}

SceneCmd::~SceneCmd () {
    delete _log;
}

Command* SceneCmd::Copy () {
    GraphicComp* group = GetGroup();
    SceneComp* dest = (group == nil) ? nil : (SceneComp*)group->Copy();
    Command* copy = new SceneCmd(CopyControlInfo(), dest);
    InitCopy(copy);
    return copy;
}

void SceneCmd::Execute () {
    Clipboard* cb = GetClipboard();
    GraphicComp* group = GetGroup();

    if (cb == nil) {
        SetClipboard(cb = new Clipboard);
        Editor* ed = GetEditor();
        Selection* s = ed->GetSelection();

        if (s->Number() >= 1) {
            Iterator i;
            _log = new LogCmd;
            GraphicView* views = ed->GetViewer()->GetGraphicView();

            for (s->First(i); !s->Done(i); s->Next(i)) {
                s->GetView(i)->Interpret(this);
            }
        }
    }
    if (!cb->IsEmpty()) {
        group->Interpret(this);
        _executed = true;
    }
}

/*****************************************************************************/

ClassId MonoSceneCmd::GetClassId () { return MONOSCENE_CMD; }

boolean MonoSceneCmd::IsA (ClassId id) {
    return MONOSCENE_CMD == id || GroupCmd::IsA(id);
}

MonoSceneCmd::MonoSceneCmd (
    ControlInfo* c, MonoSceneComp* dest
) : GroupCmd(c, dest) {
    _mcb = nil;
    _log = nil;
    _executed = false;
}

MonoSceneCmd::MonoSceneCmd (
    Editor* ed, MonoSceneComp* dest
) : GroupCmd(ed, dest) {
    _mcb = nil; 
    _log = nil;
    _executed = false;
}

MonoSceneCmd::~MonoSceneCmd () {
    if (!_executed && _mcb != nil) {
        _mcb->DeleteComps();
    }
    delete _mcb;
    delete _log;
}

Command* MonoSceneCmd::Copy () {
    GraphicComp* group = GetGroup();
    MonoSceneComp* dest = (group == nil) ? nil : (MonoSceneComp*)group->Copy();
    dest->GetClassNameVar()->GenNewName();
    Command* copy = new MonoSceneCmd(CopyControlInfo(), dest);
    InitCopy(copy);
    return copy;
}

void MonoSceneCmd::Execute () {
    Iterator i;

    GraphicComp* group = GetGroup();
    Editor* editor = GetEditor();
    Clipboard* cb = GetClipboard();

    BrushVar* brVar = (BrushVar*) editor->GetState("BrushVar");
    ColorVar* colVar = (ColorVar*) editor->GetState("ColorVar");
    if (cb == nil) {
        Selection* s = editor->GetSelection();
        SetClipboard(cb = new Clipboard);

        if (s->IsEmpty()) {
            return;
        }
        cb->Init(s);
	_mcb = new Clipboard;
        _log = new LogCmd;

        for (cb->First(i); !cb->Done(i); cb->Next(i)) {
            GraphicComp* parent = (GraphicComp*) group->Copy();
            parent->GetGraphic()->SetColors(
                colVar->GetFgColor(), colVar->GetBgColor()
            );
            parent->GetGraphic()->SetBrush(brVar->GetBrush());
            parent->Interpret(this);
	    _mcb->Append(parent);
	}
    } else {
        for (_mcb->First(i); !_mcb->Done(i); _mcb->Next(i)) {
            GraphicComp* parent = _mcb->GetComp(i);
            parent->Interpret(this);
        }
    }
    GetEditor()->GetComponent()->Interpret(this);
    _executed = true;
}

void MonoSceneCmd::Unexecute() {
    Iterator i;
    for (_mcb->First(i); !_mcb->Done(i); _mcb->Next(i)) {
        GraphicComp* parent = _mcb->GetComp(i);
        parent->Uninterpret(this);
    }
    GetEditor()->GetComponent()->Uninterpret(this);
    _executed = false;
}


/*****************************************************************************/

class InfoData : public Data {
public:
    InfoData(UList*);
    virtual ~InfoData();

    void SwitchStates(UList*);
private:
    void SwitchStates(StateVar*, StateVar*);
private:
    UList* _statelist;
};


InfoData::InfoData (UList* statelist) {
    _statelist = new UList;
    for (UList* i = statelist->First(); i != statelist->End(); i = i->Next()) {
        StateVar* state = (StateVar*) (*i)();
	StateVar* copy = state->Copy();
	_statelist->Append(new UList(copy));
    }
}

InfoData::~InfoData () {
    for (UList* i=_statelist->First(); i != _statelist->End(); i = i->Next()) {
        StateVar* state = (StateVar*) (*i)();
	delete state;
    }
    delete _statelist;
}

void InfoData::SwitchStates(StateVar* src, StateVar* dest) {
    StateVar* tmp = dest->Copy();
    *tmp = *dest;
    *dest = *src;
    *src = *tmp;
    delete tmp;
}

void InfoData::SwitchStates(UList* statelist) {
    UList* i = statelist->First();
    UList* j = _statelist->First(); 
    for (;
        i != statelist->End() && j != _statelist->End();
        i = i->Next(), j = j->Next()
    ) {
        SwitchStates((StateVar*)(*i)(), (StateVar*)(*j)());
    }
}

/*****************************************************************************/

ClassId InfoCmd::GetClassId () { return INFO_CMD; }
boolean InfoCmd::IsA (ClassId id) { return INFO_CMD == id || Command::IsA(id);}
InfoCmd::InfoCmd (ControlInfo* c, GraphicView* view) : Command(c) {
    _view = view;
    _placeCmd = nil;
    _infoData = nil;
    _info = nil;
    _iStates = nil;
    _ibshape = nil;
    _oshape = nil;
    _reversible = false;
}

InfoCmd::InfoCmd (Editor* ed, GraphicView* view) : Command(ed) { 
    _view = view;
    _placeCmd = nil;
    _infoData = nil;
    _info = nil;
    _iStates = nil;
    _ibshape = nil;
    _oshape = nil;
    _reversible = false;
}

InfoCmd::~InfoCmd () {
    delete _placeCmd;
    delete _infoData;
    delete _info;
    delete _iStates;
    delete _oshape;
}

Command* InfoCmd::Copy () {
    Command* newCmd = new InfoCmd(CopyControlInfo(), _view);
    InitCopy(newCmd);
    return newCmd;
}

static boolean Different(Shape* src, Shape* dest) {
    return (
	(src->width != dest->width) || 
	(src->height != dest->height) ||
	(src->hstretch != dest->hstretch) ||
	(src->vstretch != dest->vstretch) ||
	(src->hshrink != dest->hshrink) ||
	(src->vshrink != dest->vshrink)
    );
}

boolean InfoCmd::Accept() {
    Editor* ed = GetEditor();
    boolean done = false;
    boolean accepted;
    const char* errors;

    ed->InsertDialog(_info);
    while (!done) {
        accepted = _info->Accept();
        if (accepted) {
            delete _iStates;
            delete _infoData;
            _iStates = _info->GetAffectedStates();
            _infoData = new InfoData(_iStates);
            if (!_info->AppliedChanges(errors)) {
                AcknowledgeDialog errorMsg("Error!", errors);
                ed->InsertDialog(&errorMsg);
                errorMsg.Acknowledge();
                ed->RemoveDialog(&errorMsg);
                _infoData->SwitchStates(_iStates);
            } else {
                done = true;
            }
        } else {
            done = true;
        }
    }
    ed->RemoveDialog(_info);
    return accepted;
}


void InfoCmd::Execute () {
    Iterator i;
    Clipboard* cb = GetClipboard();
    Editor* ed = GetEditor();
    IDMapCmd idmapcmd(ed);
    idmapcmd.Execute();
    if (cb == nil) {
	SetClipboard(cb = new Clipboard);

        GraphicComp* grcomp = (GraphicComp*) _view->GetGraphicComp();
        cb->Append(grcomp);
        _view->Interpret(this);

        if (Accept()) {
            _reversible = true;
            if (_oshape != nil && Different(_ibshape, _oshape)) {
                InteractorComp* intcomp = (InteractorComp*) grcomp;
                CanvasVar* cvar = intcomp->GetCanvasVar();
		Coord x1, y1, x2, y2;
        	float cx, cy;

                int w = _ibshape->width;
                int h = _ibshape->height;
                if (w == 0 && !_ibshape->hnat) {
                    w = cvar->Width();
                }
                if (h == 0 && !_ibshape->vnat) {
                    h = cvar->Height();
                }
        	intcomp->GetGraphic()->GetCenter(cx, cy);
        	x1 = round(cx) - w/2;
        	y1 = round(cy) - h/2;
        	x2 = round(cx) + (w+1)/2;
        	y2 = round(cy) + (h+1)/2;

		_placeCmd = new PlaceCmd(
		    ed, x1, y1, x2-1, y2-1, new Clipboard(intcomp)
                );                
                _placeCmd->Execute();
            }
            GetFirewallCmd firewallCmd(grcomp);
            firewallCmd.Execute();
            InteractorComp* firewall = firewallCmd.GetFirewall();
            if (firewall != nil && firewall->IsANewScope()) {
                MonoSceneClass* mfirewall = (MonoSceneClass*) firewall;
                mfirewall->Clone(this);
            }
        }
        delete _info;
        _info = nil;

    } else {
        cb->First(i);
        GraphicComp* grcomp = cb->GetComp(i);
        grcomp->Interpret(this);
        GetFirewallCmd firewallCmd(grcomp);
        firewallCmd.Execute();
        InteractorComp* firewall = firewallCmd.GetFirewall();

        _infoData->SwitchStates(_iStates);
        if (_placeCmd != nil) {
            _placeCmd->Execute();
        }
        if (firewall != nil && firewall->IsANewScope()) {
            MonoSceneClass* mfirewall = (MonoSceneClass*) firewall;
            mfirewall->Clone(this);
        }
    }
}

void InfoCmd::Unexecute () {
    Clipboard* cb = GetClipboard();
    Editor* ed = GetEditor();
    
    if (_placeCmd != nil) {
	_placeCmd->Unexecute();
    }
    
    IDMapCmd idmapcmd(ed);
    idmapcmd.Execute();
    
    _infoData->SwitchStates(_iStates);

    Iterator i;
    cb->First(i);
    GraphicComp* grcomp = cb->GetComp(i);
    grcomp->Uninterpret(this);

    GetFirewallCmd firewallCmd(grcomp);
    firewallCmd.Execute();
    InteractorComp* firewall = firewallCmd.GetFirewall();

    if (firewall != nil && firewall->IsANewScope()) {
        MonoSceneClass* mfirewall = (MonoSceneClass*) firewall;
        mfirewall->UnClone(this);
    }
}

boolean InfoCmd::Reversible () { return _reversible; }

/*****************************************************************************/

ClassId PropsCmd::GetClassId () { return PROPS_CMD; }
boolean PropsCmd::IsA (ClassId id) { 
    return PROPS_CMD == id || Command::IsA(id);
}

PropsCmd::PropsCmd (ControlInfo* c, InteractorComp* icomp) : Command(c) {
    _icomp = icomp;
    _reversible = false;
}

PropsCmd::PropsCmd (Editor* ed, InteractorComp* icomp) : Command(ed) { 
    _icomp = icomp;
    _reversible = false;
}

Command* PropsCmd::Copy () {
    Command* newCmd = new PropsCmd(CopyControlInfo(), _icomp);
    InitCopy(newCmd);
    return newCmd;
}

void PropsCmd::Execute () {
    Editor* ed = GetEditor();

    Clipboard* cb = GetClipboard();
    if (cb == nil) {
	const char* errors;
        boolean accepted;
	SetClipboard(cb = new Clipboard);
        cb->Append(_icomp);
        _icomp->Interpret(this);

        PropsDialog* propsDialog = new PropsDialog(_icomp);

        ed->InsertDialog(propsDialog);
        while (
            (accepted = propsDialog->Accept()) && 
             !propsDialog->AppliedChanges(errors)
        ) {
            AcknowledgeDialog errorMsg("Error!", errors);
            ed->InsertDialog(&errorMsg);
            errorMsg.Acknowledge();
            ed->RemoveDialog(&errorMsg);
        }
        ed->RemoveDialog(propsDialog);
        delete propsDialog;

        if (accepted) {
            _reversible = true;
            _icomp->Propagate(this);
        }

    } else {
        _icomp->Interpret(this);
    }
}

void PropsCmd::Unexecute () {
    Iterator i;
    Clipboard* cb = GetClipboard();

    cb->First(i);
    InteractorComp* icomp = (InteractorComp*) cb->GetComp(i);
    icomp->Uninterpret(this);
}

boolean PropsCmd::Reversible () { return _reversible; }

/*****************************************************************************/

ClassId IdrawCmd::GetClassId () { return IDRAW_CMD; }
boolean IdrawCmd::IsA (ClassId id) { 
    return IDRAW_CMD == id || Command::IsA(id);
}

IdrawCmd::IdrawCmd (ControlInfo* c, GrBlockComp* grblock) : Command(c) {
    _grblock = grblock;
    _placeCmd = nil;
    _reversible = true;
}

IdrawCmd::IdrawCmd (Editor* ed, GrBlockComp* grblock) : Command(ed) { 
    _grblock = grblock;
    _placeCmd = nil;
    _reversible = true;
}

IdrawCmd::~IdrawCmd () {
    delete _placeCmd;
}

Command* IdrawCmd::Copy () {
    Command* newCmd = new IdrawCmd(CopyControlInfo(), _grblock);
    InitCopy(newCmd);
    return newCmd;
}

void IdrawCmd::Execute () {
    Editor* ed = GetEditor();
    char buf[CHARBUFSIZE];

    Clipboard* cb = GetClipboard();
    if (cb == nil) {
        boolean aborted = false;
        SetClipboard(cb = new Clipboard);
        cb->Append(_grblock);
        char* tmpname = tmpnam(NULL);
        _grblock->WriteGraphicComp(tmpname);

        int pid = vfork();
        if (pid == -1) {
            sprintf(buf, "Can't fork to execute idraw");
            Warning(ed, buf);
            
        } else if (pid == 0) {
            /* child */
            set_child_process_group();
            execlp("idraw", "idraw", tmpname, nil);
            _exit(0);

        } else {
            /* parent */
            sprintf(buf, "Executing idraw");
            aborted = Abort(ed, pid, buf);
            if (!aborted) {
                _grblock->Interpret(this);
                _grblock->ReadGraphicComp(tmpname);
                _grblock->Propagate(this);
                _placeCmd = new PlaceCmd(ed, new Clipboard(_grblock));

            } else {
                _reversible = false;
            }
            unlink(tmpname);
        }
    } else {
        _grblock->Interpret(this);
    }
    if (_placeCmd != nil) {
        _placeCmd->Execute();
        unidraw->Update(true);
    }
}

void IdrawCmd::Unexecute () {
    _grblock->Uninterpret(this);
    _placeCmd->Unexecute();
    unidraw->Update(true);
}

/*****************************************************************************/

ClassId CodeCmd::GetClassId () { return CODE_CMD; }
boolean CodeCmd::IsA (ClassId id) { return CODE_CMD == id || Command::IsA(id);}

CodeCmd::CodeCmd (ControlInfo* c) : Command(c) { Init(); }
CodeCmd::CodeCmd (Editor* ed) : Command(ed) { Init(); }

void CodeCmd::Init() {
    _option = nil; 
    _save_cmd = nil;
    _dialogList = new UList;
}

CodeCmd::~CodeCmd () { 
    delete _option; 
    delete _save_cmd;
    for(
        UList* i = _dialogList->First(); i != _dialogList->End(); i = i->Next()
    ) {
        ConflictDialog* conflict = (ConflictDialog*) (*i)();
        delete conflict;
    }
    delete _dialogList;
}

Command* CodeCmd::Copy () {
    Command* newCmd = new CodeCmd(CopyControlInfo());
    InitCopy(newCmd);
    return newCmd;
}

void CodeCmd::Execute () {
    InteractorComp* comps = (InteractorComp*) GetGraphicComp()->GetRoot();
    *_errbuf = '\0';
     const char* ccsuffix = getenv("CCSUFFIX");
     if (ccsuffix == nil) {
         strcpy(_dotc, CC_SUFFIX);
     } else {
         sprintf(_dotc, ".%s", ccsuffix);
     }
    DoIt(comps);
}

void CodeCmd::DoIt (InteractorComp* comps) {
    boolean ok;
    Editor* ed = GetEditor();
    CompNameVar* compNameVar = (CompNameVar*) ed->GetState("CompNameVar");
    const char* name = compNameVar->GetName();

    if (name == nil) {
	if (_save_cmd == nil) {
	    Style* style = new Style(Session::instance()->style());
	    style->attribute("caption", "Please save before generating.");
	    _save_cmd = new SaveCompAsCmd(
		ed, DialogKit::instance()->file_chooser(".", style)
	    );
	}
	_save_cmd->Execute();
    }

    name = compNameVar->GetName();

    if (name != nil) {
        if (_option == nil) {
            _option = new OptionDialog(
                "Interface generation options:", "",
	        Gen_File, " Generate source only",
	        Gen_Files, " Generate sources and makefile",
	        Gen_Make, " Generate and build"
	    );
        }

        _editor->InsertDialog(_option);
        boolean accepted = _option->Accept();
        _editor->RemoveDialog(_option);

        if (accepted) {
	    int select = _option->Option();

            CodeView* ev = (CodeView*) comps->Create(CODE_VIEW);
            comps->Attach(ev);
            ev->Update();

	    if (select == Gen_File) {
		ok = GenFile(name, ev);
                delete ev;
    
	    } else if (select == Gen_Files) {
		ok = GenFiles(name, ev);
                delete ev;

	    } else if (select == Gen_Make) {
		ok = GenFiles(name, ev);
                delete ev;

	        if (ok) {
	            Make(name);
	        }
	    }
	    if (!ok) {
                const char* errors = ev->GetErrors();
                strcat(_errbuf, errors);
		if (*_errbuf != '\0') {
                    Warning(_editor, _errbuf);
		}
	    }
	}
    }
}

boolean CodeCmd::GenFiles (const char* name, CodeView* ev) {
    boolean ok = GenMultipleFiles(name, ev);

    if (ok) {
        boolean skipped;
        Catalog* catalog = unidraw->GetCatalog();
        ConflictDialog* conflict = nil;
        
        char makefile[CHARBUFSIZE];
        char imakefile[CHARBUFSIZE];
        char properties[CHARBUFSIZE];
        char mainfile[CHARBUFSIZE];
        char creatorh[CHARBUFSIZE];
        char creatorc[CHARBUFSIZE];
        
        sprintf(properties, "%s-props", name);
        sprintf(mainfile, "%s-main%s", name, _dotc);
        sprintf(makefile, "%s-make", name);
        sprintf(imakefile, "%s-imake", name);
        sprintf(creatorh, "%s-creator.h", name);
        sprintf(creatorc, "%s-creator%s", name, _dotc);
        
        if (ev->IsSubUnidraw()) {
            ok = ok && CheckConflicts(
                conflict, creatorh, true, creatorc, true
            );
            skipped = conflict->Skipped();
            if (
                (skipped || ok) && !catalog->Exists(creatorh) || 
                ok && conflict->Checked(creatorh) &&catalog->Writable(creatorh)
            ) {
                ok = GenCreatorh(name, ev) && ok;
            }
            if (
                (skipped || ok) && !catalog->Exists(creatorc) || 
                ok && conflict->Checked(creatorc) &&catalog->Writable(creatorc)
            ) {
                ok = GenCreatorc(name, ev) && ok;
            }
        }
        ok = (ok || skipped) && CheckConflicts(
            conflict, properties, true, mainfile, true, 
            imakefile, true, makefile, true
        );

        skipped = conflict->Skipped();
        
        if (
            (skipped || ok) && !catalog->Exists(properties) || 
            ok && conflict->Checked(properties) &&catalog->Writable(properties)
        ) {
            ok = GenPropFile(name, ev) && ok;
        }
        if (
            (skipped || ok) && !catalog->Exists(mainfile) || 
            ok && conflict->Checked(mainfile) && catalog->Writable(mainfile)
        ) {
            ok = GenMainFile(name, ev) && ok;
        }
        if (
            (skipped || ok) && !catalog->Exists(imakefile) || 
            ok && conflict->Checked(imakefile) && catalog->Writable(imakefile)
        ) {
            ok = GenIMakeFile(name, ev) && ok;
        }
        if (
            (skipped || ok) && !catalog->Exists(makefile) || 
            ok && conflict->Checked(makefile) && catalog->Writable(makefile)
        ) {
            ok = GenMakeFile(name, ev) && ok;
        }
        
        ok = ok || skipped;
    }
    return ok;
}

boolean CodeCmd::GenFile (const char* name, CodeView* ev) {
    boolean ok = GenMultipleFiles(name, ev);
    
    if (ok) {
        Catalog* catalog = unidraw->GetCatalog();
        ConflictDialog* conflict = nil;
        
        char properties[CHARBUFSIZE];
        char mainfile[CHARBUFSIZE];
        char creatorh[CHARBUFSIZE];
        char creatorc[CHARBUFSIZE];
        
        sprintf(properties, "%s-props", name);
        sprintf(mainfile, "%s-main%s", name, _dotc);

        if (ev->IsSubUnidraw()) {
            sprintf(creatorh, "%s-creator.h", name);
            sprintf(creatorc, "%s-creator%s", name, _dotc);
        } else {
            *creatorh = '\0';
            *creatorc = '\0';
        }
        
        ok = ok && CheckConflicts(
            conflict, properties, true, mainfile, true,
            creatorh, true, creatorc, true
        );
        
        boolean skipped = conflict->Skipped();

        if (
            (skipped || ok) && !catalog->Exists(properties) || 
            ok && conflict->Checked(properties) &&catalog->Writable(properties)
        ) {
            ok = GenPropFile(name, ev) && ok;
        }
        if (
            (skipped || ok) && !catalog->Exists(mainfile) || 
            ok && conflict->Checked(mainfile) && catalog->Writable(mainfile)
        ) {
            ok = GenMainFile(name, ev) && ok;
        }
        if (ev->IsSubUnidraw()) {
            if (
                (skipped || ok) && !catalog->Exists(creatorh) || 
                ok && conflict->Checked(creatorh) &&catalog->Writable(creatorh)
            ) {
                ok = GenCreatorh(name, ev) && ok;
            }
            if (
                (skipped || ok) && !catalog->Exists(creatorc) || 
                ok && conflict->Checked(creatorc) &&catalog->Writable(creatorc)
            ) {
                ok = GenCreatorc(name, ev) && ok;
            }
        }
        
        ok = ok || skipped;
    }
    return ok;
}
    
boolean CodeCmd::GenMultipleFiles (const char* name, CodeView* ev) {
    ConflictDialog* conflict = nil;
    boolean ok = true;
    Catalog* catalog = unidraw->GetCatalog();

    char fheaderfile[CHARBUFSIZE];
    char fsrcfile[CHARBUFSIZE];
    char fcorehfile[CHARBUFSIZE];
    char fcorecfile[CHARBUFSIZE];
    char forig[CHARBUFSIZE];

    char* dir =GetDirName(name);
    
    UList* classlist = new UList;
    ev->GetClassList(classlist);

    for (UList* i = classlist->First(); i != classlist->End(); i = i->Next()) {
        SubclassNameVar* subclass = (SubclassNameVar*) (*i) ();
        const char* cname = subclass->GetName();
        sprintf(fheaderfile, "%s%s.h", dir, cname);
        sprintf(fsrcfile, "%s%s%s", dir, cname, _dotc);
        sprintf(fcorehfile, "%s%s-core.h", dir, cname);
        sprintf(fcorecfile, "%s%s-core%s", dir, cname, _dotc);
        sprintf(forig, "%s%s", dir, cname);

        ok = CheckConflicts(
            conflict, fheaderfile, false, fsrcfile, false,
            fcorehfile, true, fcorecfile, true
        );
        
        boolean skipped = conflict->Skipped();
        
        if (
            (skipped || ok) && !catalog->Exists(fheaderfile) || 
            ok && conflict->Checked(fheaderfile) && 
            catalog->Writable(fheaderfile)
        ) {
            ok = GenDothFile(forig, ev) && ok;
        }
        if (
            (skipped || ok) && !catalog->Exists(fsrcfile) ||
            ok && conflict->Checked(fsrcfile) &&
            catalog->Writable(fsrcfile)
        ) {
            ok = GenDotcFile(forig, ev) && ok;
        }
        if (
            (skipped || ok) && !catalog->Exists(fcorehfile) ||
            ok && conflict->Checked(fcorehfile) &&
            catalog->Writable(fcorehfile)
        ) {
            ok = GenCorehFile(forig, ev) && ok;
        }
        if (
            (skipped || ok) && !catalog->Exists(fcorecfile) ||
            ok && conflict->Checked(fcorecfile) &&
            catalog->Writable(fcorecfile)
        ) {
            ok = GenCorecFile(forig, ev) && ok;
        }
        ok = ok || skipped;
        if (!ok) {
            break;
        }

    }
    delete classlist;

    return ok;
}

static int StrToNum (const char* name) {
    int count = 0;
    for(int i = 0; name[i] != '\0'; i++) {
        count += name[i];
    }
    return count;
}

boolean CodeCmd::CheckConflicts (
    ConflictDialog*& conflict,
    const char* file1, boolean check1,
    const char* file2, boolean check2,
    const char* file3, boolean check3,
    const char* file4, boolean check4
) {
    Editor* ed = GetEditor();
    void* comp = ed->GetComponent();
    int hash_id = 0;
    hash_id += StrToNum(file1);
    hash_id += StrToNum(file2);
    hash_id += StrToNum(file3);
    hash_id += StrToNum(file4);

    Catalog* catalog = unidraw->GetCatalog();
    boolean accepted = true;
    conflict = nil;
    for (
        UList* i = _dialogList->First(); i != _dialogList->End(); i = i->Next()
    ) {
        ConflictDialog* cdialog = (ConflictDialog*) (*i) ();
        if (cdialog->GetId() == comp && cdialog->GetHashId() == hash_id) {
            conflict = cdialog;
            break;
        }
    }
            
    if (conflict == nil) {
 	conflict = new ConflictDialog(comp);
        _dialogList->Prepend(new UList(conflict));

        conflict->AddConflict(file1, check1);
        conflict->AddConflict(file2, check2);
        if (*file3 != '\0') {
            conflict->AddConflict(file3, check3);
        }
        if (*file4 != '\0') {
            conflict->AddConflict(file4, check4);
        }
    }
    conflict->Update();

    if (
        *file1 !='\0' && catalog->Writable(file1) && catalog->Exists(file1) || 
        *file2 !='\0' && catalog->Writable(file2) && catalog->Exists(file2) ||
	*file3 !='\0' && catalog->Writable(file3) && catalog->Exists(file3) ||
	*file4 !='\0' && catalog->Writable(file4) && catalog->Exists(file4)
    ) {
        ed->InsertDialog(conflict);
        accepted = conflict->Accept();
        ed->RemoveDialog(conflict);
    }

    return accepted;
}

boolean CodeCmd::GenIMakeFile(const char* filename, CodeView* ev) {
    char cmd[CHARBUFSIZE*10];
    char* dir = GetDirName(filename);
    char* orig = GetOrigName(filename);
    
    sprintf(cmd, "cd %s; ibmkmf -m %s %s", dir, &_dotc[1], orig);

    UList* classlist = new UList;
    ev->GetClassList(classlist);

    for (UList* i = classlist->First(); i != classlist->End(); i = i->Next()) {
        SubclassNameVar* subclass = (SubclassNameVar*) (*i) ();
        const char* cname = subclass->GetName();
        strcat(cmd, " ");
        strcat(cmd, cname);
        strcat(cmd, " ");
        strcat(cmd, cname);
        strcat(cmd, "-core");
    }
    if (ev->IsSubUnidraw()) {
        strcat(cmd, " ");
        strcat(cmd, orig);
        strcat(cmd, "-creator");
    }

    int retcode = system(cmd);
    if (retcode != 0) {
	strcat(_errbuf, "IMakefile generation failed\n");
	return false;
    }
    delete classlist;
    return true;
}

boolean CodeCmd::GenMakeFile(const char* filename, CodeView*) {
    char cmd[CHARBUFSIZE];
    boolean ok = true;

    sprintf(cmd, "ibmkmf %s", filename);
    int retcode = system(cmd);
    if (retcode != 0) {
	strcat(_errbuf, "Makefile generation failed\n");
	ok = false;
    } else {
        Catalog* catalog = unidraw->GetCatalog();
        char* dir = GetDirName(filename);
        char* orig = GetOrigName(filename);
        char mfile[CHARBUFSIZE];
        sprintf(mfile, "%smakefile", dir);
        char* tmpname = tmpnam(NULL);
        if (catalog->Exists(mfile)) {
            sprintf(cmd, "mv %s %s", mfile, tmpname);
            ok = (system(cmd) == 0) && ok;
        }
        sprintf(cmd, "mv %s-make %s", filename, mfile);
        ok = (system(cmd) == 0) && ok;

        char buf[CHARBUFSIZE];
        int pid = fork();
        if (pid == -1) {
            sprintf(buf, "Cannot fork to make %s", orig);
            Warning(GetEditor(), buf);
            ok = false;
            
        } else if (pid == 0) {
            /* child */
	    set_child_process_group();
            sprintf(cmd, "cd %s; make depend", dir);
            system(cmd);
            _exit(0);
            
        } else {
            /* parent */
            sprintf(buf, "Generating dependencies for \"%s\"", orig);
            ok = !Abort(GetEditor(), pid, buf);
        }

        sprintf(cmd, "mv %s %s-make", mfile, filename);
        ok = (system(cmd) == 0) && ok;
        if (catalog->Exists(tmpname)) {
            sprintf(cmd, "mv %s %s", tmpname, mfile);
            ok = (system(cmd) == 0) && ok;
        }
    }
    return ok;
}

boolean CodeCmd::GenPropFile(const char* filename, CodeView* ev) {
    filebuf fbuf;
    char* orig = GetOrigName(filename);
    char fullfile[CHARBUFSIZE];
    sprintf(fullfile, "%s-main.o", filename);
    unlink(fullfile);
    sprintf(fullfile, "%s-props", filename);

    boolean ok = fbuf.open(fullfile, output) != 0;

    if (ok) {
        ostream* pout = new ostream(&fbuf);
        ok = ok && ev->GenPropFile(orig, *pout);
	pout->flush();
	fbuf.sync();
        delete pout;
    } else {
	strcat(_errbuf, "Cannot open property file ");
	strcat(_errbuf, fullfile);
	strcat(_errbuf, "\n");
    }
    return ok;
}

boolean CodeCmd::GenCreatorh(const char* filename, CodeView* ev) {
    filebuf fbuf;
    char* orig = GetOrigName(filename);
    char fullfile[CHARBUFSIZE];
    sprintf(fullfile, "%s-main.o", filename);
    unlink(fullfile);
    sprintf(fullfile, "%s-creator.h", filename);

    boolean ok = fbuf.open(fullfile, output) != 0;

    if (ok) {
        ostream* pout = new ostream(&fbuf);
        ok = ok && ev->GenCreatorh(orig, *pout);
	pout->flush();
	fbuf.sync();
        delete pout;
    } else {
	strcat(_errbuf, "Cannot open creator header file ");
	strcat(_errbuf, fullfile);
	strcat(_errbuf, "\n");
    }
    return ok;
}

boolean CodeCmd::GenCreatorc(const char* filename, CodeView* ev) {
    filebuf fbuf;
    char* orig = GetOrigName(filename);
    char fullfile[CHARBUFSIZE];
    sprintf(fullfile, "%s-creator.o", filename);
    unlink(fullfile);
    sprintf(fullfile, "%s-creator%s", filename, _dotc);

    boolean ok = fbuf.open(fullfile, output) != 0;

    if (ok) {
        ostream* pout = new ostream(&fbuf);
        ok = ok && ev->GenCreatorc(orig, *pout);
	pout->flush();
	fbuf.sync();
        delete pout;
    } else {
	strcat(_errbuf, "Cannot open creator source file ");
	strcat(_errbuf, fullfile);
	strcat(_errbuf, "\n");
    }
    return ok;
}

boolean CodeCmd::GenMainFile(const char* filename, CodeView* ev) {
    filebuf fbuf;
    char* orig = GetOrigName(filename);
    char fullfile[CHARBUFSIZE];

    sprintf(fullfile, "%s-main.o", filename);
    unlink(fullfile);
    sprintf(fullfile, "%s-main%s", filename, _dotc);

    boolean ok = fbuf.open(fullfile, output) != 0;

    if (ok) {
        ostream* mout = new ostream(&fbuf);
        ok = ok && ev->GenMainFile(orig, *mout);
	mout->flush();
	fbuf.sync();
        delete mout;
    } else {
	strcat(_errbuf, "Cannot open main file ");
	strcat(_errbuf, fullfile);
	strcat(_errbuf, "\n");
    }
    return ok;
}
 
boolean CodeCmd::GenDothFile(const char* filename, CodeView* ev) {
    filebuf fbuf;
    char* orig = GetOrigName(filename);
    char fullfile[CHARBUFSIZE];
    sprintf(fullfile, "%s.o", filename);
    unlink(fullfile);
    sprintf(fullfile, "%s.h", filename);

    boolean ok = fbuf.open(fullfile, output) != 0;

    if (ok) {
        ostream* hout = new ostream(&fbuf);
        ok = ok && ev->GenDothFile(orig, *hout);
	hout->flush();
	fbuf.sync();
        delete hout;
    } else {
	strcat(_errbuf, "Cannot open header file ");
	strcat(_errbuf, fullfile);
	strcat(_errbuf, "\n");
    }
    return ok;
}

boolean CodeCmd::GenDotcFile(const char* filename, CodeView* ev) {
    filebuf fbuf;
    char* orig = GetOrigName(filename);
    char fullfile[CHARBUFSIZE];

    sprintf(fullfile, "%s.o", filename);
    unlink(fullfile);
    sprintf(fullfile, "%s%s", filename, _dotc);

    boolean ok = fbuf.open(fullfile, output) != 0;

    if (ok) {
        ostream* cout = new ostream(&fbuf);
        ok = ok && ev->GenDotcFile(orig, *cout);
	cout->flush();
	fbuf.sync();
        delete cout;
    } else {
	strcat(_errbuf, "Cannot open source file ");
	strcat(_errbuf, fullfile);
	strcat(_errbuf, "\n");
    }
    return ok;
}

boolean CodeCmd::GenCorehFile(const char* filename, CodeView* ev) {
    filebuf fbuf;
    char* orig = GetOrigName(filename);
    char fullfile[CHARBUFSIZE];
    sprintf(fullfile, "%s-core.o", filename);
    unlink(fullfile);
    sprintf(fullfile, "%s.o", filename);
    unlink(fullfile);
    sprintf(fullfile, "%s-core.h", filename);

    boolean ok = fbuf.open(fullfile, output) != 0;

    if (ok) {
        ostream* hout = new ostream(&fbuf);
        ok = ok && ev->GenCorehFile(orig, *hout);
	hout->flush();
	fbuf.sync();
        delete hout;
    } else {
	strcat(_errbuf, "Cannot open template header file ");
	strcat(_errbuf, fullfile);
	strcat(_errbuf, "\n");
    }
    return ok;
}

boolean CodeCmd::GenCorecFile(const char* filename, CodeView* ev) {
    filebuf fbuf;
    char* orig = GetOrigName(filename);
    char fullfile[CHARBUFSIZE];

    sprintf(fullfile, "%s-core.o", filename);
    unlink(fullfile);
    sprintf(fullfile, "%s-core%s", filename, _dotc);

    boolean ok = fbuf.open(fullfile, output) != 0;

    if (ok) {
        ostream* cout = new ostream(&fbuf);
        ok = ok && ev->GenCorecFile(orig, *cout);
	cout->flush();
	fbuf.sync();
        delete cout;
    } else {
	strcat(_errbuf, "Cannot open template source file ");
	strcat(_errbuf, fullfile);
	strcat(_errbuf, "\n");
    }
    return ok;
}

void CodeCmd::Make(const char* file) {
    char* orig = GetOrigName(file);
    char* dir = GetDirName(file);
    char buf[CHARBUFSIZE];
    int pid = fork();
    if (pid == -1) {
	sprintf(buf, "Cannot fork to make %s", orig);
	Warning(GetEditor(), buf);

    } else if (pid == 0) {
	/* child */
	set_child_process_group();
        char cmd[CHARBUFSIZE];
        sprintf(cmd, "cd %s; make -f %s-make", dir, orig);
        system(cmd);
	_exit(0);

    } else {
	/* parent */
	sprintf(buf, "Compiling \"%s\"", orig);
	Abort(GetEditor(), pid, buf);
    }
}

boolean CodeCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId ExeCmd::GetClassId () { return EXE_CMD; }
boolean ExeCmd::IsA (ClassId id) { return EXE_CMD==id || Command::IsA(id);}

ExeCmd::ExeCmd (ControlInfo* c) : Command(c) { _dialog = nil; }
ExeCmd::ExeCmd (Editor* ed) : Command(ed) { _dialog = nil; }

Command* ExeCmd::Copy () {
    Command* newCmd = new ExeCmd(CopyControlInfo());
    InitCopy(newCmd);
    return newCmd;
}

void ExeCmd::Execute () {
    Editor* ed = GetEditor();

    if (_dialog == nil) {
        _dialog = new ExeDialog("./");
    } else {
	_dialog->Update();
    }

    ed->InsertDialog(_dialog);
    boolean accepted = _dialog->Accept();
    ed->RemoveDialog(_dialog);

    if (accepted) {
	char buf[CHARBUFSIZE];
        char* name = (char* )_dialog->Choice();
	int pid = vfork();
	if (pid == -1) {
	    sprintf(buf, "Can't fork to execute \"%s\"\n", name);
	    Warning(ed, buf);
	} else if (pid == 0) {
	    /* child */
	    set_child_process_group();
	    sprintf(buf, "-display %s", getenv("DISPLAY"));
            execlp(name, name, buf, nil);
	    _exit(0);

	} else {
	    /* parent */
	    char* slash = strrchr(name, '/');
	    char* filename = (slash == nil) ? name : slash + 1;
	    sprintf(buf, "Executing \"%s\"", filename);
	    Abort(ed, pid, buf);
	}
    }        
}

boolean ExeCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId NewToolCmd::GetClassId () { return NEWTOOL_CMD; }

boolean NewToolCmd::IsA (ClassId id) {
    return NEWTOOL_CMD == id || Command::IsA(id);
}

NewToolCmd::NewToolCmd (ControlInfo* c) : Command(c) { 
    _option = nil; 
    _scmd = nil;
}

NewToolCmd::NewToolCmd (Editor* ed) : Command(ed) { 
    _option = nil; 
    _scmd = nil;
}

NewToolCmd::~NewToolCmd () { 
    delete _option; 
    delete _scmd;
}

Command* NewToolCmd::Copy () {
    Command* newCmd = new NewToolCmd(CopyControlInfo());
    InitCopy(newCmd);
    return newCmd;
}

enum {BITMAP, MINIATURE, PLAIN_TEXT};

void NewToolCmd::Execute () {
    Editor* ed = GetEditor();
    CompNameVar* compNameVar = (CompNameVar*) ed->GetState("CompNameVar");
    const char* name = compNameVar->GetName();

    if (name == nil) {
        if (_scmd == nil) {
	    Style* style = new Style(Session::instance()->style());
	    style->attribute(
		"caption", "Please save before creating the tool."
	    );
            _scmd = new SaveCompAsCmd(
		ed, DialogKit::instance()->file_chooser(".", style)
	    );
        }
        _scmd->Execute();
    }

    name = compNameVar->GetName();

    if (name != nil) {
        if (_option == nil) {
            _option = new OptionDialog(
                "Icon for the tool palette:", "",
	        BITMAP, " Custom bitmap", 
	        MINIATURE, " Miniature", 
	        PLAIN_TEXT, " Name of interface (plain text)"
	    );
        }

        ed->InsertDialog(_option);
        boolean accepted = _option->Accept();
        ed->RemoveDialog(_option);

        if (accepted) {
            InitNewTool(name, _option->Option());
        }
    }
}
   
static boolean WriteBitmap(const char* bitmapfile, const char* orig) {
    filebuf fbuf;
    boolean ok = fbuf.open(bitmapfile, output) != 0;
    if (ok) {
        ostream bout(&fbuf);
        bout << "#define " << orig << "_width 20\n";
        bout << "#define " << orig << "_height 20\n";
        bout << "static char " << orig << "_bits[] = {\n";
        for (int i = 0; i < 5; i++) {
            bout << "   ";
            for (int j = 0; j < 12; j++) {
                bout << "0x00, ";
            }
            bout << "\n";
        }
        bout << "};\n";
        bout.flush();
        fbuf.sync();
        ok = ok && bout.good();
    }        
    return ok;
}
                
boolean NewToolCmd::CreateCtrlInfo (
    GraphicComp* origComp, char* file, ControlInfo*& ctrlInfo, int select
) {
    IBEditor* ed = (IBEditor*) GetEditor();
    boolean success = false;

    if (select == BITMAP) { 
        boolean ok = true;
        char bitmapfile[CHARBUFSIZE];

    	Catalog* catalog = unidraw->GetCatalog();
        const char* toolspath = getenv("TOOLDIR");
	if (toolspath == nil) {
	    sprintf(bitmapfile, "./%s.bm", file);
	} else {
	    sprintf(bitmapfile, "%s/%s.bm", toolspath, file);
	}
        struct stat st;

        if (!catalog->Exists(bitmapfile)) {
            ok = WriteBitmap(bitmapfile, file);
            if (!ok) {    
                char Error[CHARBUFSIZE];
                sprintf(Error,"bitmap file %s generation failed!", bitmapfile);
                Warning(ed, Error);
            }
        }
        if (ok) {
            int pid = fork();
            if (pid == -1) {
                Warning(ed, "Can't fork to run bitmap!");
            } else if (pid == 0) {
                /* child */
                char cmd[CHARBUFSIZE];
                char* dir = GetDirName(bitmapfile);
                char* bm = GetOrigName(bitmapfile);
		set_child_process_group();
                sprintf(cmd, "cd %s; bitmap %s", dir, bm);
                system(cmd);
                _exit(0);

            } else {
                /* parent */
                char buf[CHARBUFSIZE];
                sprintf(buf, "Executing bitmap");
                boolean aborted = Abort(ed, pid, buf);

                if (!aborted) {
                    if (
                        stat(bitmapfile, &st) == 0 && 
                        (st.st_mode & S_IREAD) != 0
                    ) {
                        success = true;
                        BitmapGraphic* bitmapgr= new BitmapGraphic(
                            stdgraphic, bitmapfile
                        );
                        BitmapComp* bitmapcomp = new BitmapComp(bitmapgr);
                        ctrlInfo = new ControlInfo(bitmapcomp, "");
                        
                    } else {
                        Warning(
                            ed,"Bitmap creation failed.  Please try again!\n"
                        );
                    }
                } else {
                    success = false;
                }
            }
        }
    } else if (select == MINIATURE) {
        GraphicComp* comp = (GraphicComp*) origComp->Copy();
        GraphicView* view = (GraphicView*) comp->Create(COMPONENT_VIEW);
        comp->Attach(view);
        view->Update();
        Graphic* g = view->GetGraphic()->Copy();
        delete view;

        float l, b, r, t, scale;
        const float size = 25.0;
    
        g->GetBounds(l, b, r, t);
        scale = min(size/(r - l), size/(t - b));
        ScaleCmd scaleCmd((Editor*) nil, scale, scale);
        comp->GraphicComp::Interpret(&scaleCmd);
	ctrlInfo =  new ControlInfo(comp, "");
	success = true;

    } else if (select == PLAIN_TEXT) {
	ctrlInfo =  new ControlInfo(file);
	success = true;
    }
    return success;
}

void NewToolCmd::InitNewTool (const char* protoName, int selection) {
    char* filename = GetOrigName(protoName);
    ControlInfo* ctrlInfo;
    Iterator i;
    GraphicComp* protoComp;
    int numcomp = 0;
    
    IBEditor* ibEd = (IBEditor*) GetEditor();
    GraphicComp* EdComp = ibEd->GetGraphicComp();
    Selection* select = ibEd->GetSelection();
    for (EdComp->First(i); !EdComp->Done(i); EdComp->Next(i), numcomp++) {}

    if (numcomp == 1) {
	EdComp->First(i);
	EdComp = EdComp->GetComp(i);
    }
    if (CreateCtrlInfo(EdComp, (char*)filename, ctrlInfo, selection)) {
    	select->Hide();
        protoComp = (GraphicComp*) EdComp->Copy();
        Tool* tool = new IBGraphicCompTool(ctrlInfo, protoComp);
    
        char toolName[CHARBUFSIZE];
        sprintf(toolName, "%s.tool", filename);
   
	Iterator i;
        for (unidraw->First(i); !unidraw->Done(i); unidraw->Next(i)) {
	    IBEditor* iEd = (IBEditor*) unidraw->GetEditor(i); 
            ToolPanel* toolpanel = iEd->GetToolPanel();
            toolpanel->Install(toolName, tool);
            toolpanel->Change();
	}
        select->Show();
    }
}

boolean NewToolCmd::Reversible () { return false; }

/*****************************************************************************/

ClassId ToolsCmd::GetClassId () { return TOOLS_CMD; }
boolean ToolsCmd::IsA (ClassId id) {return TOOLS_CMD==id || Command::IsA(id);}

ToolsCmd::ToolsCmd (ControlInfo* c) : Command(c) { }
ToolsCmd::ToolsCmd (Editor* ed) : Command(ed) { }

Command* ToolsCmd::Copy () {
    Command* newCmd = new ToolsCmd(CopyControlInfo());
    InitCopy(newCmd);
    return newCmd;
}

void ToolsCmd::Execute () {
    IBEditor* ibEd = (IBEditor*) GetEditor();
    ToolPanel* toolpanel = ibEd->GetToolPanel();
    EditorInfo* edInfo = toolpanel->GetEditorInfo();
    InstallRemoveDialog toolsdialog = InstallRemoveDialog(edInfo);

    ibEd->InsertDialog(&toolsdialog);
    boolean accepted = toolsdialog.Accept();
    ibEd->RemoveDialog(&toolsdialog);

    if (accepted) {
	EditorInfo* installed = (EditorInfo*) toolsdialog.Installed();
        EditorInfo* removed = (EditorInfo*) toolsdialog.Removed();

	Iterator i;
	for (unidraw->First(i); !unidraw->Done(i); unidraw->Next(i)) {
	    IBEditor* iEd = (IBEditor*) unidraw->GetEditor(i);
	    ToolPanel* itoolpanel = iEd->GetToolPanel();

            for (int i = 0; i < installed->Count(); i++) {
                itoolpanel->Install(installed->GetName(i));
            }
            for (i = 0; i < removed->Count(); i++) {
                itoolpanel->Uninstall(removed->GetName(i));
            }
            itoolpanel->Change();
	}
    }        
}

boolean ToolsCmd::Reversible () { return false; }

/*****************************************************************************/
boolean IBViewCompCmd::_executing = false;

IBViewCompCmd::IBViewCompCmd (
    ControlInfo* info, FileChooser* fc
) : ViewCompCmd (info, fc) {}

void IBViewCompCmd::Execute () {
    _executing = true;
    ViewCompCmd::Execute();
    _executing = false;
}

/*****************************************************************************/
class CloneMapElem : public UMapElem {
public:
    CloneMapElem(InteractorComp*);
    virtual ~CloneMapElem();

    virtual void* id();
    virtual void* tag();
public:
    InteractorComp* _orig;
    InteractorComp* _clone;
};

class CloneMap : public UMap {
public:
    CloneMap();
    void Register(InteractorComp*);
    InteractorComp* GetClone(InteractorComp*);
};

/*****************************************************************************/

CloneMapElem::CloneMapElem (InteractorComp* orig) {
    Catalog* catalog = unidraw->GetCatalog();
    ClassId id = orig->GetClassId();
    _orig = orig;
    _clone = (InteractorComp*) catalog->GetCreator()->Create(id);
    _clone->Instantiate();
    *_clone = *_orig;
}

CloneMapElem::~CloneMapElem () { delete _clone; }
void* CloneMapElem::id () { return (void*) _orig; }
void* CloneMapElem::tag () { return (void*) _clone; }

/*****************************************************************************/

CloneMap::CloneMap () { }

void CloneMap::Register (InteractorComp* orig) {
    UMap::Register(new CloneMapElem(orig));
}

InteractorComp* CloneMap::GetClone(InteractorComp* orig) {
    CloneMapElem* map = (CloneMapElem*) FindId((void*) orig);
    return (InteractorComp*) map->tag();
}

/*****************************************************************************/

ClassId RelateCmd::GetClassId () { return RELATE_CMD; }
boolean RelateCmd::IsA (ClassId id){return RELATE_CMD==id || Command::IsA(id);}

RelateCmd::RelateCmd (
    ControlInfo* c, InteractorComp* src, InteractorComp* dest
) : Command(c) {
    _src = src;
    _dest = dest;
    _clonemap = new CloneMap;
}

RelateCmd::RelateCmd (
    Editor* ed, InteractorComp* src, InteractorComp* dest
) : Command(ed) {
    _src = src;
    _dest = dest;
    _clonemap = new CloneMap;
}

RelateCmd::~RelateCmd () { delete _clonemap; }

void RelateCmd::Execute () {
    RecurRelate(_src, _dest);
    _src->Propagate(this);
    _dest->Propagate(this);
}

void RelateCmd::Unexecute() {
    RecurUnrelate(_src, _dest);
    _src->Unpropagate(this);
    _dest->Unpropagate(this);
}

void RelateCmd::RecurRelate(InteractorComp* src, InteractorComp* dest) {
    if (src != nil && dest != nil) {
        if (dest->IsRelatableTo(src)) {
	    _clonemap->Register(dest);
	    _clonemap->Register(src);
            dest->Relate(src);
            _src->Interpret(this);
            _dest->Interpret(this);

        } else if (src->IsRelatableTo(dest)) {
	    _clonemap->Register(dest);
	    _clonemap->Register(src);
            src->Relate(dest);
            _src->Interpret(this);
            _dest->Interpret(this);

        } else if (src->IsAScene() && !dest->IsAScene()) {
            Iterator i;
            for (src->First(i); !src->Done(i); src->Next(i)) {
                InteractorComp* kid = (InteractorComp*) src->GetComp(i);
                RecurRelate(kid, dest);
            }

        } else if (dest->IsAScene() && !src->IsAScene()) {
            Iterator i;
            for (dest->First(i); !dest->Done(i); dest->Next(i)) {
                InteractorComp* kid = (InteractorComp*) dest->GetComp(i);
                RecurRelate(src, kid);
            }
        }
    }
}

void RelateCmd::RecurUnrelate(InteractorComp* src, InteractorComp* dest) {
    if (src != nil && dest != nil) {
        if (dest->IsRelatableTo(src) || src->IsRelatableTo(dest)) {
	    InteractorComp* dclone = _clonemap->GetClone(dest);
	    InteractorComp* sclone = _clonemap->GetClone(src);
            dest->Unrelate(dclone);
            src->Unrelate(sclone);
            _src->Uninterpret(this);
            _dest->Uninterpret(this);

        } else if (src->IsAScene() && !dest->IsAScene()) {
            Iterator i;
            for (src->First(i); !src->Done(i); src->Next(i)) {
                InteractorComp* kid = (InteractorComp*) src->GetComp(i);
                RecurUnrelate(kid, dest);
            }

        } else if (dest->IsAScene() && !src->IsAScene()) {
            Iterator i;
            for (dest->First(i); !dest->Done(i); dest->Next(i)) {
                InteractorComp* kid = (InteractorComp*) dest->GetComp(i);
                RecurUnrelate(src, kid);
            }
        }
    }
}

/*****************************************************************************/

ClassId EditCmd::GetClassId () { return EDIT_CMD; }
boolean EditCmd::IsA (ClassId id) { return EDIT_CMD == id || Command::IsA(id);}

EditCmd::EditCmd (
    ControlInfo* c, Clipboard* cb, const char* text
) : Command(c, cb) { 
    _oldtext = _newtext = nil;
    _replacee = nil;
    SetNewText(text); 
}

EditCmd::EditCmd (
    Editor* ed, Clipboard* cb, const char* text
) : Command(ed, cb) { 
    _oldtext = _newtext = nil;
    _replacee = nil;
    SetNewText(text); 
}

EditCmd::EditCmd (
    Editor* ed, Clipboard* cb, GraphicComp* replacee
) : Command(ed, cb) { 
    _oldtext = _newtext = nil;
    _replacee = replacee;
}

EditCmd::~EditCmd() {
    delete _oldtext;
    delete _newtext;
    delete _replacee;
}

Command* EditCmd::Copy () {
    Command* newCmd = new EditCmd(CopyControlInfo());
    InitCopy(newCmd);
    return newCmd;
}

void EditCmd::SetOldText (const char* text) {
    if (text != nil) {
        delete  _oldtext;
	_oldtext = strnew(text);
    }
}

void EditCmd::SetNewText (const char* text) {
    if (text != nil) {
        delete _newtext;
	_newtext = strnew(text);
    }
}

GraphicComp* EditCmd::SwapComp (GraphicComp* replacee) {
    GraphicComp* tmp = _replacee;
    _replacee = replacee;
    return tmp;
}

/*****************************************************************************/

ClassId ReorderCmd::GetClassId () { return REORDER_CMD; }
boolean ReorderCmd::IsA (ClassId id) {
    return REORDER_CMD==id || Command::IsA(id);
}

ReorderCmd::ReorderCmd (ControlInfo* c) : Command(c) {
    _log = nil;
    _executed = false;
}

ReorderCmd::ReorderCmd (Editor* ed) : Command(ed) {
    _log = nil;
    _executed = false;
}

ReorderCmd::~ReorderCmd () {
    if (!_executed) {
        delete _log;
    }
}

Command* ReorderCmd::Copy () {
    Command* copy = new ReorderCmd(CopyControlInfo());
    InitCopy(copy);
    return copy;
}

void ReorderCmd::Execute () {
    _executed = true;
    _log = new LogCmd;
    GetEditor()->GetComponent()->Interpret(this);
}

void ReorderCmd::Unexecute() {
    _executed = false;
    GetEditor()->GetComponent()->Uninterpret(this);
}

/*****************************************************************************/

ClassId CompCheckCmd::GetClassId () { return COMPCHECK_CMD; }
boolean CompCheckCmd::IsA (ClassId id) {
    return COMPCHECK_CMD==id || Command::IsA(id);
}

CompCheckCmd::CompCheckCmd (
    IComp* target, const char* c, const char* v, const char* g
) {
    _c = strnew(c);
    _v = strnew(v);
    _g = strnew(g);
    _target = target;
    _ok = true;
}

CompCheckCmd::~CompCheckCmd () {
    delete _c;
    delete _v;
    delete _g;
}

Command* CompCheckCmd::Copy () {
    Command* copy = new CompCheckCmd(_target, _c, _v, _g);
    return copy;
}

void CompCheckCmd::Execute () {
    _target->GetRoot()->Interpret(this);
}
