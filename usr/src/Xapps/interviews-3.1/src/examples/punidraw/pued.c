/*
 * PunidrawEditor implementation.
 */

#include "pued.h"

#include <Unidraw/classes.h>
#include <Unidraw/ctrlinfo.h>
#include <Unidraw/keymap.h>
#include <Unidraw/kybd.h>
#include <Unidraw/selection.h>
#include <Unidraw/uctrls.h>
#include <Unidraw/upage.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/catcmds.h>
#include <Unidraw/Commands/edit.h>

#include <Unidraw/Components/ellipse.h>
#include <Unidraw/Components/line.h>
#include <Unidraw/Components/rect.h>

#include <Unidraw/Graphic/ellipses.h>
#include <Unidraw/Graphic/lines.h>
#include <Unidraw/Graphic/polygons.h>

#include <Unidraw/Tools/grcomptool.h>
#include <Unidraw/Tools/move.h>
#include <Unidraw/Tools/rotate.h>
#include <Unidraw/Tools/scale.h>
#include <Unidraw/Tools/select.h>

#include <InterViews/border.h>
#include <InterViews/glue.h>
#include <InterViews/menu.h>
#include <InterViews/panner.h>

/*****************************************************************************/

Punidraw::Punidraw (GraphicComp* comp) {
    SetClassName("Punidraw");

    _comp = (comp == nil) ? (new GraphicComps) : comp;
    _keymap = new KeyMap;
    _curCtrl = new ControlState;
    _selection = new Selection;

    InitViewer();
    Insert(Interior());
    GetKeyMap()->Execute(CODE_SELECT);
}

Punidraw::~Punidraw () {
    delete _keymap;
    delete _selection;
}

Component* Punidraw::GetComponent () { return _comp; }
Viewer* Punidraw::GetViewer (int id) { return (id == 0) ? _viewer : nil; }
KeyMap* Punidraw::GetKeyMap () { return _keymap; }
Selection* Punidraw::GetSelection () { return _selection; }

Tool* Punidraw::GetCurTool () {
    UControl* c = (UControl*) _curCtrl->Selection();
    return (Tool*) c->GetControlInfo()->GetOwner();
}

void Punidraw::SetComponent (Component* comp) {
    if (comp == nil || comp->IsA(GRAPHIC_COMP)) {
        _comp = (GraphicComp*) comp;
    }
}

void Punidraw::SetViewer (Viewer* v, int i) {
    if (i == 0) {
        _viewer = v;
    }
}

void Punidraw::SetKeyMap (KeyMap* k) { _keymap = k; }
void Punidraw::SetSelection (Selection* s) { _selection = s; }

Interactor* Punidraw::Interior () {
    return new VBox(
        new HBox(
            Commands(),
            new HGlue,
        ),
        new HBorder,
        new HBox(
            _viewer,
            new VBorder,
            new VBox(
                Tools(),
                new VGlue,
                new HBorder,
                new Panner(_viewer)
            )
        )
    );
}

Interactor* Punidraw::Commands () {
    MenuBar* commands = new MenuBar;

    commands->Include(FileMenu());
    commands->Include(EditMenu());

    return commands;
}

Interactor* Punidraw::Tools () {
    VBox* tools = new VBox;
    const Coord unit = round(0.5*cm);

    Include(
        new SelectTool(new ControlInfo("Select", KLBL_SELECT, CODE_SELECT)),
        tools
    );
    Include(
        new MoveTool(new ControlInfo("Move", KLBL_MOVE, CODE_MOVE)),
        tools
    );
    Include(
        new ScaleTool(new ControlInfo("Scale", KLBL_SCALE, CODE_SCALE)),
        tools
    );
    Include(
        new RotateTool(new ControlInfo("Rotate", KLBL_ROTATE, CODE_ROTATE)),
        tools
    );

    Line* line = new Line(0, 0, unit, unit, stdgraphic);
    LineComp* lineComp = new LineComp(line);
    Include(
        new GraphicCompTool(
            new ControlInfo(lineComp, KLBL_LINE, CODE_LINE), lineComp
        ), tools
    );

    SF_Rect* rect = new SF_Rect(0, 0, unit*2, unit, stdgraphic);
    rect->SetPattern(psnonepat);
    RectComp* rectComp = new RectComp(rect);
    Include(
        new GraphicCompTool(
            new ControlInfo(rectComp, KLBL_RECT, CODE_RECT), rectComp
        ), tools
    );

    SF_Ellipse* ellipse = new SF_Ellipse(0, 0, unit, unit/2, stdgraphic);
    ellipse->SetPattern(psnonepat);
    EllipseComp* ellipseComp = new EllipseComp(ellipse);
    Include(
        new GraphicCompTool(
            new ControlInfo(ellipseComp, KLBL_ELLIPSE, CODE_ELLIPSE),
            ellipseComp
        ), tools
    );

    return tools;
}

void Punidraw::InitViewer () {
    const float w = 8.5 * inches;
    const float h = 11 * inches;

    GraphicView* view = (GraphicView*) _comp->Create(COMPONENT_VIEW);
    _comp->Attach(view);
    view->Update();

    _viewer = new Viewer(
        this, view, new UPage(w, h), nil, round(w/2), round(h/2)
    );
}

void Punidraw::Include (Command* cmd, PulldownMenu* pdm) {
    ControlInfo* ctrlInfo = cmd->GetControlInfo();
    UControl* ctrl = new CommandControl(ctrlInfo);
    _keymap->Register(ctrl);
    pdm->Include(ctrl);
    cmd->SetEditor(this);
}

void Punidraw::Include (Tool* tool, Box* box) {
    ControlInfo* ctrlInfo = tool->GetControlInfo();
    UControl* ctrl = new VPanelControl(ctrlInfo, _curCtrl);
    _keymap->Register(ctrl);
    box->Insert(ctrl);
}

PulldownMenu* Punidraw::FileMenu () {
    PulldownMenu* pdm = new PulldownMenu("File");

    Include(
        new NewCompCmd(
            new ControlInfo("New", KLBL_NEWCOMP, CODE_NEWCOMP),
            new GraphicComps
        ),
        pdm
    );
    Include(
        new ViewCompCmd(
            new ControlInfo("Open...", KLBL_VIEWCOMP, CODE_VIEWCOMP)
        ), pdm
    );
    Include(
        new SaveCompCmd(new ControlInfo("Save", KLBL_SAVECOMP, CODE_SAVECOMP)),
        pdm
    );
    Include(
        new PrintCmd(
            new ControlInfo("Print...", KLBL_PRINT, CODE_PRINT)
        ), pdm
    );
    Include(
        new QuitCmd(
            new ControlInfo("Quit", KLBL_QUIT, CODE_QUIT)
        ), pdm
    );

    return pdm;
}

PulldownMenu* Punidraw::EditMenu () {
    PulldownMenu* pdm = new PulldownMenu("Edit");

    Include(
        new UndoCmd(
            new ControlInfo("Undo", KLBL_UNDO, CODE_UNDO)
        ), pdm
    );
    Include(
        new RedoCmd(
            new ControlInfo("Redo", KLBL_REDO, CODE_REDO)
        ), pdm
    );
    Include(
        new DeleteCmd(
            new ControlInfo("Delete", KLBL_DEL, CODE_DEL)
        ), pdm
    );
    Include(
        new DupCmd(
            new ControlInfo("Duplicate", KLBL_DUP, CODE_DUP)
        ), pdm
    );

    return pdm;
}
