/*
 * Copyright (c) 1990, 1991 Stanford University
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
 * Idraw editor main class implementation.
 */

#include "idarrow.h"
#include "idarrows.h"
#include "idclasses.h"
#include "idcmds.h"
#include "idcomp.h"
#include "ided.h"
#include "idkybd.h"
#include "idvars.h"

#include <Unidraw/catalog.h>
#include <Unidraw/ctrlinfo.h>
#include <Unidraw/editor.h>
#include <Unidraw/editorinfo.h>
#include <Unidraw/grid.h>
#include <Unidraw/globals.h>
#include <Unidraw/keymap.h>
#include <Unidraw/kybd.h>
#include <Unidraw/selection.h>
#include <Unidraw/statevars.h>
#include <Unidraw/stateviews.h>
#include <Unidraw/uctrls.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/upage.h>
#include <Unidraw/viewer.h>

#include <Unidraw/Commands/align.h>
#include <Unidraw/Commands/brushcmd.h>
#include <Unidraw/Commands/catcmds.h>
#include <Unidraw/Commands/colorcmd.h>
#include <Unidraw/Commands/edit.h>
#include <Unidraw/Commands/font.h>
#include <Unidraw/Commands/import.h>
#include <Unidraw/Commands/patcmd.h>
#include <Unidraw/Commands/struct.h>
#include <Unidraw/Commands/transforms.h>
#include <Unidraw/Commands/viewcmds.h>

#include <Unidraw/Components/ellipse.h>
#include <Unidraw/Components/line.h>
#include <Unidraw/Components/polygon.h>
#include <Unidraw/Components/rect.h>
#include <Unidraw/Components/spline.h>
#include <Unidraw/Components/text.h>

#include <Unidraw/Graphic/ellipses.h>
#include <Unidraw/Graphic/picture.h>
#include <Unidraw/Graphic/polygons.h>
#include <Unidraw/Graphic/splines.h>

#include <Unidraw/Tools/connect.h>
#include <Unidraw/Tools/grcomptool.h>
#include <Unidraw/Tools/magnify.h>
#include <Unidraw/Tools/move.h>
#include <Unidraw/Tools/reshape.h>
#include <Unidraw/Tools/rotate.h>
#include <Unidraw/Tools/scale.h>
#include <Unidraw/Tools/select.h>
#include <Unidraw/Tools/stretch.h>

#include <InterViews/border.h>
#include <InterViews/box.h>
#include <InterViews/frame.h>
#include <InterViews/glue.h>
#include <InterViews/menu.h>
#include <InterViews/message.h>
#include <InterViews/panner.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/tray.h>
#include <InterViews/window.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*****************************************************************************/

static const char* brAttrib = "brush";
static const char* fontAttrib = "font";
static const char* patAttrib = "pattern";
static const char* fgAttrib = "fgcolor";
static const char* bgAttrib = "bgcolor";

static const char* initBrAttrib = "initialbrush";
static const char* initFontAttrib = "initialfont";
static const char* initPatAttrib = "initialpattern";
static const char* initFgAttrib = "initialfgcolor";
static const char* initBgAttrib = "initialbgcolor";
static const char* initArrowAttrib = "initialarrow";

static const char* page_width_attrib = "pagewidth";
static const char* page_height_attrib = "pageheight";
static const char* grid_x_incr = "gridxincr";
static const char* grid_y_incr = "gridyincr";

/*****************************************************************************/

inline void InsertSeparator (PulldownMenu* pdm) {
    pdm->GetScene()->Insert(
        new VBox(
            new VGlue(2, 0, 0),
            new HBorder,
            new VGlue(2, 0, 0)
        )
    );
}

inline PulldownMenu* MakePulldown (const char* name) {
    return new PulldownMenu(
        new HBox(
            new Message(name, Center, round(.1*cm)),
            new HGlue(0, 5*strlen(name), 0)
        )
    );
}

/*****************************************************************************/

IdrawEditor::IdrawEditor (GraphicComp* comp) {
    Init(comp);
}

IdrawEditor::IdrawEditor (const char* file) {
    if (file == nil) {
	Init();

    } else {
	Catalog* catalog = unidraw->GetCatalog();
	GraphicComp* comp;

	if (catalog->Retrieve(file, (Component*&) comp)) {
	    Init(comp);

	} else {
	    Init();
	    fprintf(stderr, "idraw: couldn't open %s\n", file);
	}
    }
}

void IdrawEditor::Init (GraphicComp* comp) {
    if (GetWindow() == nil) {
	ManagedWindow* w = new ApplicationWindow(this);
	SetWindow(w);
	Style* s = new Style(Session::instance()->style());
	s->alias("IdrawEditor");
	w->style(s);
    }

    _comp = (comp == nil) ? (new IdrawComp) : comp;
    _keymap = new KeyMap;
    _curCtrl = new ControlState;
    _selection = new Selection;
    _tray = new Tray;

    InitStateVars();
    InitViewer();

    Insert(new Frame(Interior()));
    GetKeyMap()->Execute(CODE_SELECT);
}

void IdrawEditor::InitStateVars () {
    _name = new CompNameVar(_comp);
    _modifStatus = new ModifStatusVar(_comp);
    _gravity = new GravityVar;
    _magnif = new MagnifVar;
    _font = new FontVar;
    _brush = new BrushVar;
    _pattern = new PatternVar;
    _color = new ColorVar;
    _arrows = new ArrowVar;

    Catalog* catalog = unidraw->GetCatalog();

    const char* brIndex = catalog->GetAttribute(initBrAttrib);
    const char* fontIndex = catalog->GetAttribute(initFontAttrib);
    const char* patIndex = catalog->GetAttribute(initPatAttrib);
    const char* fgIndex = catalog->GetAttribute(initFgAttrib);
    const char* bgIndex = catalog->GetAttribute(initBgAttrib);
    const char* arrowState = catalog->GetAttribute(initArrowAttrib);

    _brush->SetBrush(catalog->ReadBrush(brAttrib, atoi(brIndex)));
    _font->SetFont(catalog->ReadFont(fontAttrib, atoi(fontIndex)));
    _pattern->SetPattern(catalog->ReadPattern(patAttrib, atoi(patIndex)));
    _color->SetColors(
        catalog->ReadColor(fgAttrib, atoi(fgIndex)),
        catalog->ReadColor(bgAttrib, atoi(bgIndex))
    );

    if (strcmp(arrowState,"both") == 0) {
	_arrows->SetArrows(true, true);
    } else if (strcmp(arrowState,"head") == 0) {
	_arrows->SetArrows(true, false);
    } else if (strcmp(arrowState,"tail") == 0) {
	_arrows->SetArrows(false, true);
    } else {
	_arrows->SetArrows(false, false);
    }
}

IdrawEditor::~IdrawEditor () {
    delete _keymap;
    delete _selection;
    delete _modifStatus;

    delete _name;
    delete _modifStatus;
    delete _gravity;
    delete _magnif;
    delete _font;
    delete _brush;
    delete _pattern;
    delete _color;
    delete _arrows;
}

Component* IdrawEditor::GetComponent () { return _comp; }
Viewer* IdrawEditor::GetViewer (int id) { return (id == 0) ? _viewer : nil; }
KeyMap* IdrawEditor::GetKeyMap () { return _keymap; }
Selection* IdrawEditor::GetSelection () { return _selection; }

StateVar* IdrawEditor::GetState (const char* name) {
    if (strcmp(name, "CompNameVar") == 0) {
        return _name;
    } else if (strcmp(name, "ModifStatusVar") == 0) {
        return _modifStatus;
    } else if (strcmp(name, "MagnifVar") == 0) {
        return _magnif;
    } else if (strcmp(name, "GravityVar") == 0) {
        return _gravity;
    } else if (strcmp(name, "FontVar") == 0) {
        return _font;
    } else if (strcmp(name, "BrushVar") == 0) {
        return _brush;
    } else if (strcmp(name, "PatternVar") == 0) {
        return _pattern;
    } else if (strcmp(name, "ColorVar") == 0) {
        return _color;
    } else if (strcmp(name, "ArrowVar") == 0) {
	return _arrows;
    } else {
        return Editor::GetState(name);
    }
}

Tool* IdrawEditor::GetCurTool () {
    UControl* c = (UControl*) _curCtrl->Selection();
    return (Tool*) c->GetControlInfo()->GetOwner();
}

void IdrawEditor::SetComponent (Component* comp) {
    if (comp == nil || comp->IsA(GRAPHIC_COMP)) {
        _comp = (GraphicComp*) comp;
    }
}

void IdrawEditor::SetViewer (Viewer* v, int i) { 
    if (i == 0) {
        _viewer = v;
    }
}

void IdrawEditor::SetKeyMap (KeyMap* k) { _keymap = k; }
void IdrawEditor::SetSelection (Selection* s) { _selection = s; }

Interactor* IdrawEditor::Interior () {
    Interactor* tools = Tools();
    Interactor* commands = new HBox(Commands(), new HGlue);
    HBorder* hborder = new HBorder;
    VBorder* vborder = new VBorder;
    int gap = round(.1*cm);

    HBox* indicators = new HBox(
        new ArrowVarView(_arrows, _brush, _color),
        new VBorder,
        new PatternVarView(_pattern, _color)
    );
    VBox* status = new VBox(
        new HBox(
            new HGlue(gap, 0, 0),
            new ModifStatusVarView(_modifStatus),
            new CompNameVarView(_name, Left),
            new MagnifVarView(_magnif),
            new GravityVarView(_gravity, Right),
            new FontVarView(_font, Right)
        ),
        new HBorder 
    );
    VBox* panel = new VBox(
        tools,
        new VGlue,
        new HBorder,
        new Panner(_viewer)
    );
    panel->Propagate(false);
    
    _tray->HBox(_tray, status, _tray);
    _tray->HBox(_tray, indicators, vborder, commands, _tray);
    _tray->HBox(_tray, hborder, _tray);
    _tray->HBox(_tray, panel, vborder, _viewer, _tray);

    _tray->VBox(_tray, status, indicators, hborder, panel, _tray);
    _tray->VBox(_tray, status, vborder, _tray);
    _tray->VBox(_tray, status, commands, hborder, _viewer, _tray);

    return _tray;
}

Interactor* IdrawEditor::Commands () {
    MenuBar* commands = new MenuBar;

    commands->Include(FileMenu());
    commands->Include(EditMenu());
    commands->Include(StructureMenu());
    commands->Include(FontMenu());
    commands->Include(BrushMenu());
    commands->Include(PatternMenu());
    commands->Include(ColorMenu("FgColor", fgAttrib));
    commands->Include(ColorMenu("BgColor", bgAttrib));
    commands->Include(AlignMenu());
    commands->Include(ViewMenu());

    return commands;
}

static const unit = 15;

static Coord xClosed[] = { unit/5, unit, unit, unit*3/5, 0 };
static Coord yClosed[] = { 0, unit/5, unit*3/5, unit, unit*2/5 };
static const int nClosed = 5;

static Coord xOpen[] = { 0, unit/2, unit/2, unit };
static Coord yOpen[] = { 0, unit/4, unit*3/4, unit };
static const int nOpen = 4;

Interactor* IdrawEditor::Tools () {
    VBox* tools = new VBox;

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
        new StretchTool(new ControlInfo("Stretch", KLBL_STRETCH,CODE_STRETCH)),
        tools
    );
    Include(
        new RotateTool(new ControlInfo("Rotate", KLBL_ROTATE, CODE_ROTATE)),
        tools
    );
    Include(
        new ReshapeTool(new ControlInfo("Alter", KLBL_RESHAPE, CODE_RESHAPE)),
        tools
    );
    Include(
        new MagnifyTool(new ControlInfo("Magnify", KLBL_MAGNIFY,CODE_MAGNIFY)),
        tools
    );

    TextGraphic* text = new TextGraphic("Text", stdgraphic);
    TextComp* textComp = new TextComp(text);
    Include(
        new GraphicCompTool(
            new ControlInfo("Text", KLBL_TEXT, CODE_TEXT), textComp
        ), tools
    );

    ArrowLine* line = new ArrowLine(
	0, 0, unit, unit, false, false, 1., stdgraphic
    );
    ArrowLineComp* arrowLineComp = new ArrowLineComp(line);
    Include(
        new GraphicCompTool(
            new ControlInfo(arrowLineComp, KLBL_LINE, CODE_LINE), arrowLineComp
        ), tools
    );

    ArrowMultiLine* ml = new ArrowMultiLine(
        xOpen, yOpen, nOpen, false, false, 1., stdgraphic
    );
    ml->SetPattern(psnonepat);
    ArrowMultiLineComp* mlComp = new ArrowMultiLineComp(ml);
    Include(
        new GraphicCompTool(
            new ControlInfo(mlComp, KLBL_MULTILINE, CODE_MULTILINE), mlComp
        ), tools
    );

    ArrowOpenBSpline* spl = new ArrowOpenBSpline(
        xOpen, yOpen, nOpen, false, false, 1., stdgraphic
    );
    spl->SetPattern(psnonepat);
    ArrowSplineComp* splComp = new ArrowSplineComp(spl);
    Include(
        new GraphicCompTool(
            new ControlInfo(splComp, KLBL_SPLINE, CODE_SPLINE), splComp
        ), tools
    );

    SF_Rect* rect = new SF_Rect(0, 0, unit, unit*4/5, stdgraphic);
    rect->SetPattern(psnonepat);
    RectComp* rectComp = new RectComp(rect);
    Include(
        new GraphicCompTool(
            new ControlInfo(rectComp, KLBL_RECT, CODE_RECT), rectComp
        ), tools
    );

    SF_Ellipse* ellipse = new SF_Ellipse(0, 0, unit*2/3, unit*2/5, stdgraphic);
    ellipse->SetPattern(psnonepat);
    EllipseComp* ellipseComp = new EllipseComp(ellipse);
    Include(
        new GraphicCompTool(
            new ControlInfo(ellipseComp, KLBL_ELLIPSE, CODE_ELLIPSE),
            ellipseComp
        ), tools
    );
    SF_Polygon* polygon = new SF_Polygon(xClosed, yClosed, nClosed,stdgraphic);
    polygon->SetPattern(psnonepat);
    PolygonComp* polygonComp = new PolygonComp(polygon);
    Include(
        new GraphicCompTool(
            new ControlInfo(polygonComp, KLBL_POLY, CODE_POLY), polygonComp
        ), tools
    );

    SFH_ClosedBSpline* cspline = new SFH_ClosedBSpline(
        xClosed, yClosed, nClosed, stdgraphic
    );
    cspline->SetPattern(psnonepat);
    ClosedSplineComp* csplineComp = new ClosedSplineComp(cspline);
    Include(
        new GraphicCompTool(
            new ControlInfo(csplineComp, KLBL_CSPLINE,CODE_CSPLINE),csplineComp
        ), tools
    );

    return tools;
}

void IdrawEditor::InitViewer () {
    Catalog* catalog = unidraw->GetCatalog();

    const char* page_w = catalog->GetAttribute(page_width_attrib);
    const char* page_h = catalog->GetAttribute(page_height_attrib);
    const char* x_incr = catalog->GetAttribute(grid_x_incr);
    const char* y_incr = catalog->GetAttribute(grid_y_incr);

    GraphicView* view = (GraphicView*) _comp->Create(COMPONENT_VIEW);
    _comp->Attach(view);
    view->Update();

    /*
     * These statements had to be moved down here to workaround
     * a strange cfront 3.0 bug.
     */
    const float w = round(atof(page_w) * inches);
    const float h = round(atof(page_h) * inches);

    UPage* page = new UPage(w, h);
    Grid* grid = new Grid(w, h, atof(x_incr), atof(y_incr));
    grid->Visibility(false);

    _viewer = new Viewer(this, view, page, grid);
}

void IdrawEditor::Include (Command* cmd, PulldownMenu* pdm) {
    ControlInfo* ctrlInfo = cmd->GetControlInfo();
    UControl* ctrl = new CommandControl(ctrlInfo);
    _keymap->Register(ctrl);
    if (pdm != nil) pdm->Include(ctrl);
    cmd->SetEditor(this);
}

void IdrawEditor::Include (Tool* tool, Box* box) {
    ControlInfo* ctrlInfo = tool->GetControlInfo();
    UControl* ctrl = new VPanelControl(ctrlInfo, _curCtrl);
    _keymap->Register(ctrl);
    box->Insert(ctrl);
}

PulldownMenu* IdrawEditor::FileMenu () {
    PulldownMenu* pdm = MakePulldown("File");

    Include(
        new AboutCmd(new ControlInfo("About idraw", KLBL_ABOUT, CODE_ABOUT))
    );

    Include(
        new NewCompCmd(
            new ControlInfo("New", KLBL_NEWCOMP, CODE_NEWCOMP), new IdrawComp
        ),
        pdm
    );
    Include(
        new RevertCmd(new ControlInfo("Revert", KLBL_REVERT, CODE_REVERT)),
        pdm
    );
    InsertSeparator(pdm);
    Include(
        new OpenCmd(
            new ControlInfo("Open...", KLBL_VIEWCOMP, CODE_VIEWCOMP)
        ),
        pdm
    );
    Include(
        new SaveCompCmd(new ControlInfo("Save", KLBL_SAVECOMP, CODE_SAVECOMP)),
        pdm
    );
    Include(
        new SaveCompAsCmd(
            new ControlInfo("Save As...", KLBL_SAVECOMPAS, CODE_SAVECOMPAS)
        ), pdm
    );
    Include(
        new PrintCmd(
            new ControlInfo("Print...", KLBL_PRINT, CODE_PRINT)
        ), pdm
    );
    Include(
        new ImportCmd(
            new ControlInfo("Import Graphic...", KLBL_IMPORT, CODE_IMPORT)
        ), pdm
    );
    InsertSeparator(pdm);
    Include(
        new QuitCmd(
            new ControlInfo("Quit", KLBL_QUIT, CODE_QUIT)
        ), pdm
    );

    return pdm;
}

PulldownMenu* IdrawEditor::EditMenu () {
    PulldownMenu* pdm = MakePulldown("Edit");

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
    InsertSeparator(pdm);
    Include(
        new CutCmd(
            new ControlInfo("Cut", KLBL_CUT, CODE_CUT)
        ), pdm
    );
    Include(
        new CopyCmd(
            new ControlInfo("Copy", KLBL_COPY, CODE_COPY)
        ), pdm
    );
    Include(
        new PasteCmd(
            new ControlInfo("Paste", KLBL_PASTE,CODE_PASTE)
        ), pdm
    );
    Include(
        new DupCmd(
            new ControlInfo("Duplicate", KLBL_DUP, CODE_DUP)
        ), pdm
    );
    Include(
        new DeleteCmd(
            new ControlInfo("Delete", KLBL_DEL, CODE_DEL)
        ), pdm
    );
    Include(
        new SlctAllCmd(
            new ControlInfo("Select All", KLBL_SLCTALL, CODE_SLCTALL)
        ), pdm
    );
    InsertSeparator(pdm);
    Include(
        new ScaleCmd(
            new ControlInfo("Flip Horizontal", KLBL_HFLIP, CODE_HFLIP),
            -1.0, 1.0
        ), pdm
    );
    Include(
        new ScaleCmd(
            new ControlInfo("Flip Vertical", KLBL_VFLIP, CODE_VFLIP),
            1.0, -1.0
        ), pdm
    );
    Include(
        new RotateCmd(
            new ControlInfo("90 Clockwise", KLBL_CW90, CODE_CW90), -90.0
        ), pdm
    );
    Include(
        new RotateCmd(
            new ControlInfo("90 CounterCW", KLBL_CCW90, CODE_CCW90), 90.0
        ), pdm
    );
    InsertSeparator(pdm);
    Include(
        new PreciseMoveCmd(
            new ControlInfo("Precise Move", KLBL_PMOVE, CODE_PMOVE)
        ), pdm
    );
    Include(
        new PreciseScaleCmd(
            new ControlInfo("Precise Scale", KLBL_PSCALE, CODE_PSCALE)
        ), pdm
    );
    Include(
        new PreciseRotateCmd(
            new ControlInfo("Precise Rotate", KLBL_PROTATE, CODE_PROTATE)
        ), pdm
    );

    return pdm;
}

PulldownMenu* IdrawEditor::StructureMenu () {
    PulldownMenu* pdm = MakePulldown("Structure");

    Include(
        new GroupCmd(
            new ControlInfo("Group", KLBL_GROUP, CODE_GROUP)
        ), pdm
    );
    Include(
        new UngroupCmd(
            new ControlInfo("Ungroup", KLBL_UNGROUP, CODE_UNGROUP)
        ), pdm
    );
    Include(
        new FrontCmd(
            new ControlInfo("Bring to Front", KLBL_FRONT, CODE_FRONT)
        ), pdm
    );
    Include(
        new BackCmd(
            new ControlInfo("Send to Back", KLBL_BACK, CODE_BACK)
        ), pdm
    );

    return pdm;
}

PulldownMenu* IdrawEditor::FontMenu () {
    Catalog* catalog = unidraw->GetCatalog();
    PulldownMenu* pdm = MakePulldown("Font");

    int i = 1;
    PSFont* font = catalog->ReadFont(fontAttrib, i);

    while (font != nil) {
        TextGraphic* text = new TextGraphic(
            font->GetPrintFontAndSize(), stdgraphic
        );
        text->SetFont(font);

        Include(
            new FontCmd(
                new ControlInfo(new TextComp(text)), font
            ), pdm
        );
        font = catalog->ReadFont(fontAttrib, ++i);
    }

    return pdm;
}

static float MENU_WIDTH = 1.3;   /* in cm */
static float MENU_HEIGHT = 0.5;

PulldownMenu* IdrawEditor::BrushMenu () {
    ControlInfo* ctrlInfo;
    ArrowLine* line;
    Catalog* catalog = unidraw->GetCatalog();
    PulldownMenu* pdm = MakePulldown("Brush");

    int i = 1;
    PSBrush* br = catalog->ReadBrush(brAttrib, i);

    while (br != nil) {

        if (br->None()) {
            ctrlInfo = new ControlInfo("None");

        } else {
            line = new ArrowLine(
		0, 0, round(MENU_WIDTH*cm), 0, false, false, 1., stdgraphic
	    );
            line->SetBrush(br);
            ctrlInfo = new ControlInfo(new ArrowLineComp(line));
        }
        Include(new BrushCmd(ctrlInfo, br), pdm);
        br = catalog->ReadBrush(brAttrib, ++i);
    }
    InsertSeparator(pdm);

    line = new ArrowLine(
	0, 0, round(MENU_WIDTH*cm), 0, false, false, 1., stdgraphic
    );
    ctrlInfo = new ControlInfo(new ArrowLineComp(line));
    Include(new ArrowCmd(ctrlInfo, false, false), pdm);

    line = new ArrowLine(
	0, 0, round(MENU_WIDTH*cm), 0, true, false, 1., stdgraphic
    );
    ctrlInfo = new ControlInfo(new ArrowLineComp(line));
    Include(new ArrowCmd(ctrlInfo, true, false), pdm);

    line = new ArrowLine(
	0, 0, round(MENU_WIDTH*cm), 0, false, true, 1., stdgraphic
    );
    ctrlInfo = new ControlInfo(new ArrowLineComp(line));
    Include(new ArrowCmd(ctrlInfo, false, true), pdm);

    line = new ArrowLine(
	0, 0, round(MENU_WIDTH*cm), 0, true, true, 1., stdgraphic
    );
    ctrlInfo = new ControlInfo(new ArrowLineComp(line));
    Include(new ArrowCmd(ctrlInfo, true, true), pdm);

    return pdm;
}

PulldownMenu* IdrawEditor::PatternMenu () {
    Catalog* catalog = unidraw->GetCatalog();
    PulldownMenu* pdm = MakePulldown("Pattern");

    int i = 1;
    PSPattern* pat = catalog->ReadPattern(patAttrib, i);
    
    while (pat != nil) {
        ControlInfo* ctrlInfo;

        if (pat->None()) {
            ctrlInfo = new ControlInfo("None");

        } else {
            SF_Rect* sfr = new SF_Rect(
                0, 0, round(MENU_WIDTH*cm), round(MENU_HEIGHT*cm), stdgraphic
            );
            sfr->SetPattern(pat);
            ctrlInfo = new ControlInfo(new RectComp(sfr));
        }
        
        Include(new PatternCmd(ctrlInfo, pat), pdm);
        pat = catalog->ReadPattern(patAttrib, ++i);
    }

    return pdm;
}

PulldownMenu* IdrawEditor::ColorMenu (const char* name, const char* attrib) {
    Catalog* catalog = unidraw->GetCatalog();
    PulldownMenu* pdm = MakePulldown(name);

    int i = 1;
    PSColor* color = catalog->ReadColor(attrib, i);
    
    while (color != nil) {
        ControlInfo* ctrlInfo;
        Coord w = round(MENU_WIDTH*cm);
        Coord h = round(MENU_HEIGHT*cm);

        SF_Rect* sfr = new SF_Rect(0, 0, w, h, stdgraphic);
	sfr->SetColors(color, color);
	ctrlInfo = new ControlInfo(new RectComp(sfr), color->GetName());

        if (strcmp(attrib, fgAttrib) == 0) {
            Include(new ColorCmd(ctrlInfo, color, nil), pdm);
        } else {
            Include(new ColorCmd(ctrlInfo, nil, color), pdm);
        }
	color = catalog->ReadColor(attrib, ++i);
    }
    return pdm;
}

PulldownMenu* IdrawEditor::AlignMenu () {
    PulldownMenu* pdm = MakePulldown("Align");

    Include(
        new AlignCmd(
            new ControlInfo("Left Sides", KLBL_ALGNLEFT, CODE_ALGNLEFT), 
            Left, Left
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Right Sides", KLBL_ALGNRIGHT, CODE_ALGNRIGHT),
            Right, Right
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Tops", KLBL_ALGNTOP, CODE_ALGNTOP), Top, Top
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Bottoms", KLBL_ALGNBOT, CODE_ALGNBOT),	
            Bottom, Bottom
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Horiz Centers", KLBL_ALGNHCTR, CODE_ALGNHCTR),
            HorizCenter, HorizCenter
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Vert Centers", KLBL_ALGNVCTR, CODE_ALGNVCTR),
            VertCenter, VertCenter
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Centers", KLBL_ALGNCTR, CODE_ALGNCTR),	
            Center, Center
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Abut Left", KLBL_ABUTLEFT, CODE_ABUTLEFT), 
            Left, Right
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Abut Right", KLBL_ABUTRIGHT, CODE_ABUTRIGHT),
            Right, Left
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Abut Up", KLBL_ABUTUP, CODE_ABUTUP),
            Top, Bottom
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Abut Down", KLBL_ABUTDOWN, CODE_ABUTDOWN),
            Bottom, Top
        ), pdm
    );
    InsertSeparator(pdm);
    Include(
        new AlignToGridCmd(
            new ControlInfo("Align to Grid", KLBL_ALGNTOGRID, CODE_ALGNTOGRID)
        ), pdm
    );

    return pdm;
}

PulldownMenu* IdrawEditor::ViewMenu () {
    PulldownMenu* pdm = MakePulldown("View");

    Include(
        new NewViewCmd(
            new ControlInfo("New View", KLBL_NEWVIEW, CODE_NEWVIEW)
        ), pdm
    );
    Include(
        new CloseEditorCmd(
            new ControlInfo("Close View", KLBL_CLOSEEDITOR, CODE_CLOSEEDITOR)
        ), pdm
    );
    InsertSeparator(pdm);
    Include(
        new NormSizeCmd(
            new ControlInfo("Normal Size", KLBL_NORMSIZE, CODE_NORMSIZE)
        ), pdm
    );
    Include(
        new RedToFitCmd(
            new ControlInfo("Reduce to Fit", KLBL_REDTOFIT, CODE_REDTOFIT)
        ), pdm
    );
    Include(
        new CenterCmd(
            new ControlInfo("Center Page", KLBL_CENTER, CODE_CENTER)
        ), pdm
    );
    Include(
        new OrientationCmd(
            new ControlInfo("Orientation", KLBL_ORIENTATION, CODE_ORIENTATION)
        ), pdm
    );
    InsertSeparator(pdm);
    Include(
        new GridCmd(
            new ControlInfo("Grid on/off", KLBL_GRID, CODE_GRID)
        ), pdm
    );
    Include(
        new IGridSpacingCmd(
            new ControlInfo("Grid Spacing...", KLBL_GRIDSPC, CODE_GRIDSPC)
        ), pdm
    );
    Include(
        new GravityCmd(
            new ControlInfo("Gravity on/off", KLBL_GRAVITY, CODE_GRAVITY)
        ), pdm
    );

    return pdm;
}

