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
 * IBEditor main class implementation.
 */

#include "ibbox.h"
#include "ibbutton.h"
#include "ibclasses.h"
#include "ibcreator.h"
#include "ibcmds.h"
#include "ibdeck.h"
#include "ibdialog.h"
#include "ibed.h"
#include "ibeditor.h"
#include "ibellipse.h"
#include "ibframe.h"
#include "ibgrcomp.h"
#include "ibinteractor.h"
#include "ibkybd.h"
#include "ibline.h"
#include "ibmenu.h"
#include "ibpolygon.h"
#include "ibrect.h"
#include "ibscene.h"
#include "ibshaper.h"
#include "ibspline.h"
#include "ibtext.h"
#include "ibtoolpanel.h"
#include "ibtools.h"
#include "ibvars.h"
#include "ibviewport.h"

#include <Unidraw/catalog.h>
#include <Unidraw/ctrlinfo.h>
#include <Unidraw/editor.h>
#include <Unidraw/editorinfo.h>
#include <Unidraw/globals.h>
#include <Unidraw/grid.h>
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
#include <Unidraw/Graphic/polygons.h>
#include <Unidraw/Graphic/splines.h>

#include <Unidraw/Tools/grcomptool.h>
#include <Unidraw/Tools/magnify.h>
#include <Unidraw/Tools/move.h>
#include <Unidraw/Tools/reshape.h>
#include <Unidraw/Tools/rotate.h>
#include <Unidraw/Tools/scale.h>
#include <Unidraw/Tools/select.h>
#include <Unidraw/Tools/stretch.h>

#include <InterViews/adjuster.h>
#include <InterViews/bitmap.h>
#include <InterViews/border.h>
#include <InterViews/box.h>
#include <InterViews/deck.h>
#include <InterViews/frame.h>
#include <InterViews/glue.h>
#include <InterViews/menu.h>
#include <InterViews/message.h>
#include <InterViews/panner.h>
#include <InterViews/shape.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <InterViews/world.h>

#include <OS/string.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*****************************************************************************/

const float VIEWER_WIDTH = 0;
const float VIEWER_HEIGHT = 6;

/*****************************************************************************/

static const char* borderAttrib = "border";
static const char* brAttrib = "brush";
static const char* fontAttrib = "font";
static const char* fgAttrib = "fgcolor";
static const char* bgAttrib = "bgcolor";
static const char* patAttrib = "pattern";

static const char* initBorderAttrib = "initialborder";
static const char* initBrAttrib = "initialbrush";
static const char* initFontAttrib = "initialfont";
static const char* initFgAttrib = "initialfgcolor";
static const char* initBgAttrib = "initialbgcolor";
static const char* initPatAttrib = "initialpattern";

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
    return new PulldownMenu(new Message(name, Center, round(.1*cm)));
}

/*****************************************************************************/

IBEditor::IBEditor (GraphicComp* comp) {
    Init(comp);
}

IBEditor::IBEditor (const char* file) {
    if (file == nil) {
	Init();

    } else {
	Catalog* catalog = unidraw->GetCatalog();
	GraphicComp* comp;

	if (catalog->Retrieve(file, (Component*&) comp)) {
	    Init(comp);

	} else {
	    Init();
	    fprintf(stderr, "ibuild: couldn't open %s\n", file);
	}
    }
}

void IBEditor::Init(GraphicComp* comp) {
    if (GetWindow() == nil) {
        ManagedWindow* window = new ApplicationWindow(this);
        Style* s = new Style(Session::instance()->style());
        s->name("IBEditor");
        s->attribute("name", "InterViews interface builder");
        s->attribute("iconName", "IBuild");
        s->alias("IBuild");
        window->style(s);
        SetWindow(window);
    } 

    if (comp == nil) {
        SceneComp* scomp = new SceneComp;
        scomp->GetClassNameVar()->SetName("[root]");
        scomp->GetClassNameVar()->SetBaseClass("[root]");
        _comp = scomp;

    } else {
        _comp = comp;
    }
    _curCtrl1 = new ControlState;
    _curCtrl2 = new ControlState;
    _curCtrl1->ref();
    _curCtrl2->ref();
    _keymap1 = new KeyMap;
    _keymap2 = new KeyMap;
    _selection = new Selection;

    _gravity_on = false;
    _grid_on = false;
    _graphics_mode = false;

    InitStateVars();
    InitViewer();

    _prevBrush = nil;
    _prevPattern = nil;
    _prevFont = nil;
    _prevFg = nil;
    _prevBg = nil;

    Insert(new Frame(Interior()));
    _keymap1->Execute(CODE_SELECT);
    _keymap2->Execute(CODE_SELECT);
    _keymap = _keymap1;
    _curCtrl = _curCtrl1;
    CheckMode();
}

void IBEditor::InitStateVars () {
    _name = new CompNameVar(_comp);
    _modifStatus = new ModifStatusVar(_comp);
    _gravity = new GravityVar;
    _magnif = new MagnifVar;

    _font = new FontVar;
    _brush = new BrushVar;
    _color = new ColorVar;
    _pattern = new PatternVar;

    Catalog* catalog = unidraw->GetCatalog();

    const char* brIndex = catalog->GetAttribute(initBorderAttrib);
    const char* fontIndex = catalog->GetAttribute(initFontAttrib);
    const char* fgIndex = catalog->GetAttribute(initFgAttrib);
    const char* bgIndex = catalog->GetAttribute(initBgAttrib);

    _brush->SetBrush(catalog->ReadBrush(borderAttrib, atoi(brIndex)));
    _font->SetFont(catalog->ReadFont(fontAttrib, atoi(fontIndex)));
    _pattern->SetPattern(new PSPattern(0, 1.0));
    _color->SetColors(
        catalog->ReadColor(fgAttrib, atoi(fgIndex)),
        catalog->ReadColor(bgAttrib, atoi(bgIndex))
    );
    IBNameVar::SetUniqueFlag(true);

}

IBEditor::~IBEditor () {
    delete _keymap1;
    delete _keymap2;
    delete _selection;
    
    if (_curCtrl1 != nil) _curCtrl1->unref();
    if (_curCtrl2 != nil) _curCtrl2->unref();
    if (_prevBrush != nil) _prevBrush->unref();
    if (_prevPattern != nil) _prevPattern->unref();
    if (_prevFont != nil) _prevFont->unref();
    if (_prevFg != nil) _prevFg->unref();
    if (_prevBg != nil) _prevBg->unref();

    delete _name;
    delete _modifStatus;
    delete _gravity;
    delete _magnif;
    delete _font;
    delete _brush;
    delete _pattern;
    delete _color;
}

void IBEditor::CheckMode () {
    if (GraphicsMode()) {
        if (!_graphics_mode) {
            _graphics_mode = true;
            _tooldeck->Bottom();
            _commanddeck->Bottom();
            _grtooldeck->Bottom();
            _keymap = _keymap2;
            _curCtrl = _curCtrl2;
            
            _gravity->Activate(_gravity_on);
            GetViewer()->GetGrid()->Visibility(_grid_on);
            SwitchBrush();
            SwitchColors();
            SwitchFont();
            SwitchPattern();
        }
    } else {
        if (_graphics_mode) {
            _graphics_mode = false;
            Grid* grid = GetViewer()->GetGrid();
            
            _tooldeck->Top();
            _commanddeck->Top();
            _grtooldeck->Top();
            _keymap = _keymap1;
            _curCtrl = _curCtrl1;
            
            _gravity_on = _gravity->IsActive();
            _grid_on = grid->IsVisible();
            _gravity->Activate(false);
            grid->Visibility(false);
            
            SwitchBrush();
            SwitchColors();
            SwitchFont();
            SwitchPattern();
        }
    }
}

void IBEditor::SwitchBrush () {
    if (_prevBrush != nil) {
        PSBrush* tmp = _brush->GetBrush();
        tmp->ref();
        _brush->SetBrush(_prevBrush);
        _prevBrush->unref();
        _prevBrush = tmp;
    } else {
        Catalog* catalog = unidraw->GetCatalog();
        const char* brIndex = catalog->GetAttribute(initBrAttrib);
        _prevBrush = _brush->GetBrush();
        _prevBrush->ref();
        _brush->SetBrush(
            catalog->ReadBrush(brAttrib, atoi(brIndex))
        );
    }
}
    
void IBEditor::SwitchColors () {
    if (_prevFg != nil && _prevBg != nil) {
        PSColor* tmp1 = _color->GetFgColor();
        PSColor* tmp2 = _color->GetBgColor();
        tmp1->ref();
        tmp2->ref();
        _color->SetColors(_prevFg, _prevBg);
        _prevFg->unref();
        _prevBg->unref();
        _prevFg = tmp1;
        _prevBg = tmp2;
    } else {
        Catalog* catalog = unidraw->GetCatalog();
        const char* fgIndex = catalog->GetAttribute(initFgAttrib);
        const char* bgIndex = catalog->GetAttribute(initBgAttrib);
        _prevFg = _color->GetFgColor();
        _prevBg = _color->GetBgColor();
        _prevFg->ref();
        _prevBg->ref();
        _color->SetColors(
            catalog->ReadColor(fgAttrib, atoi(fgIndex)),
            catalog->ReadColor(bgAttrib, atoi(bgIndex))
        );
    }
}
    
void IBEditor::SwitchFont () {
    if (_prevFont != nil) {
        PSFont* tmp = _font->GetFont();
        tmp->ref();
        _font->SetFont(_prevFont);
        _prevFont->unref();
        _prevFont = tmp;
    } else {
        Catalog* catalog = unidraw->GetCatalog();
        const char* fontIndex = catalog->GetAttribute(initFontAttrib);
        _prevFont = _font->GetFont();
        _prevFont->ref();
        _font->SetFont(
            catalog->ReadFont(fontAttrib, atoi(fontIndex))
        );
    }
}
    
void IBEditor::SwitchPattern () {
    if (_prevPattern != nil) {
        PSPattern* tmp = _pattern->GetPattern();
        tmp->ref();
        _pattern->SetPattern(_prevPattern);
        _prevPattern->unref();
        _prevPattern = tmp;
    } else {
        Catalog* catalog = unidraw->GetCatalog();
        const char* patIndex = catalog->GetAttribute(initPatAttrib);
        _prevPattern = _pattern->GetPattern();
        _prevPattern->ref();
        _pattern->SetPattern(
            catalog->ReadPattern(patAttrib, atoi(patIndex))
        );
    }
}
    
boolean IBEditor::GraphicsMode () {
    boolean grmode = false;
    for (Component* gcomp = _comp; gcomp != nil; gcomp = gcomp->GetParent()){
        if (gcomp->IsA(GRBLOCK_COMP)) {
            grmode = true;
            break;
        }
    }
    return grmode;
}

Component* IBEditor::GetComponent () { return _comp; }
Viewer* IBEditor::GetViewer (int id) { return (id == 0) ? _viewer : nil; }
KeyMap* IBEditor::GetKeyMap () { return _keymap; }
Selection* IBEditor::GetSelection () { return _selection; }
ToolPanel* IBEditor::GetToolPanel () { return _toolpanel; }

StateVar* IBEditor::GetState (const char* name) {
    if (strcmp(name, "FontVar") == 0) {
        return _font;
    } else if (strcmp(name, "BrushVar") == 0) {
        return _brush;
    } else if (strcmp(name, "ColorVar") == 0) {
        return _color;
    } else if (strcmp(name, "PatternVar") == 0) {
        return _pattern;
    } else if (strcmp(name, "CompNameVar") == 0) {
        return _name;
    } else if (strcmp(name, "ModifStatusVar") == 0) {
        return _modifStatus;
    } else if (strcmp(name, "MagnifVar") == 0) {
        return _magnif;
    } else if (strcmp(name, "GravityVar") == 0) {
        return _gravity;
    } else {
        return Editor::GetState(name);
    }
}

Tool* IBEditor::GetCurTool () {
    UControl* c = (UControl*) _curCtrl->Selection();
    return (Tool*) c->GetControlInfo()->GetOwner();
}

void IBEditor::SetComponent (Component* comp) {
    if (comp == nil || comp->IsA(GRAPHIC_COMP)) {
        _comp = (GraphicComp*) comp;
        CheckMode();
    }
}

void IBEditor::SetViewer (Viewer* v, int i) { 
    if (i == 0) {
        _viewer = v;
    }
}

void IBEditor::SetKeyMap (KeyMap* k) { _keymap = k; }
void IBEditor::SetSelection (Selection* s) { _selection = s; }

Interactor* IBEditor::Interior () {
    int gap = round(.1*cm);
    int wpanner = round(1.5*cm);

    HBox* attr = new HBox (
        new BrushVarView(_brush, _color),
        new VBorder,
        new PatternVarView(_pattern, _color),
        new VBorder,
        new HGlue,
        new FontVarView(_font, Right)
    );
    HBox* status = new HBox(
	new HBox (
            new BrushVarView(_brush, _color),
            new VBorder,
            new PatternVarView(_pattern, _color),
            new VBorder
        ),
	new HBox (
            new HGlue(gap, 0, 0),
            new ModifStatusVarView(_modifStatus),
            new CompNameVarView(_name, Left),
            new HGlue,
            new GravityVarView(_gravity, Right),
            new FontVarView(_font, Right),
            new MagnifVarView(_magnif, Right)
	)
    );        
    HBox* panel = new HBox(
	new VBox(	
            status,
            new HBorder,
            CreateTools(),
            new HBorder,
            CreateCommands()
	),
	new VBorder,
        new Panner(_viewer, wpanner)
    );
    VBox* staticview = new VBox(
	_viewer,
	new HBorder,
        CreateGrTools()
    );
    panel->Propagate(false);
    staticview->Propagate(false);

    return new VBox(
        panel,
        new HBorder,
	staticview
    );
}

Interactor* IBEditor::CreateCommands () {
    _commanddeck = new Deck;
    _keymap = _keymap1;
    _commanddeck->Insert(new HBox(Commands1(), new HGlue));
    _keymap = _keymap2;
    _commanddeck->Insert(new HBox(Commands2(), new HGlue));
    _keymap = _keymap1;
    return _commanddeck;
}

Interactor* IBEditor::CreateTools () {
    _tooldeck = new Deck;
    _keymap = _keymap1;
    _curCtrl = _curCtrl1;
    _tooldeck->Insert(new HBox(Tools1(), new HGlue));
    _keymap = _keymap2;
    _curCtrl = _curCtrl2;
    _tooldeck->Insert(new HBox(Tools2(), new HGlue));
    _keymap = _keymap1;
    _curCtrl = _curCtrl1;
    return _tooldeck;
}
    
Interactor* IBEditor::CreateGrTools () {
    _grtooldeck = new Deck;
    _keymap = _keymap1;
    _curCtrl = _curCtrl1;
    _grtooldeck->Insert(GrTools1());
    _keymap = _keymap2;
    _curCtrl = _curCtrl2;
    _grtooldeck->Insert(new HBox(GrTools2(), new HGlue));
    _keymap = _keymap1;
    _curCtrl = _curCtrl1;
    return _grtooldeck;
}

Interactor* IBEditor::Commands1 () {
    MenuBar* commands = new MenuBar;

    commands->Include(FileMenu1());
    commands->Include(EditMenu1());
    commands->Include(CompositionMenu());
    commands->Include(FontMenu1());
    commands->Include(BrushMenu1());
    commands->Include(ColorMenu1("FgColor", fgAttrib));
    commands->Include(ColorMenu1("BgColor", bgAttrib));
    commands->Include(AlignMenu1());
    commands->Include(ViewMenu1());

    return commands;
}

Interactor* IBEditor::Tools1 () {
    HBox* tools = new HBox;

    Include(
        new SelectTool(new ControlInfo("Select", KLBL_SELECT, CODE_SELECT)),
        tools
    );
    Include(
        new MoveTool(new ControlInfo("Move", KLBL_MOVE, CODE_MOVE)), tools
    );
    Include(
        new StretchTool(new ControlInfo("Resize", KLBL_STRETCH, CODE_STRETCH)),
        tools
    );
    Include(
        new ExamineTool(new ControlInfo("Examine", KLBL_EXAMINE,CODE_EXAMINE)),
        tools
    );
    Include(
        new RelateTool(new ControlInfo("Relate", KLBL_RELATE,CODE_RELATE)),
        tools
    );
    Include(
        new ReshapeTool(new ControlInfo("Edit", KLBL_RESHAPE, CODE_RESHAPE)),
        tools
    );
    Include(
        new MagnifyTool(new ControlInfo("Magnify", KLBL_MAGNIFY, CODE_MAGNIFY)),
        tools
    );
    Include(
        new NarrowTool(new ControlInfo("Narrow", KLBL_NARROW,CODE_NARROW)),
        tools
    );

    return tools;
}

Interactor* IBEditor::GrTools1 () {
    _toolpanel = new ToolPanel(_curCtrl, _keymap);
    return _toolpanel;
}

Interactor* IBEditor::Commands2 () {
    MenuBar* commands = new MenuBar;

    commands->Include(FileMenu2());
    commands->Include(EditMenu2());
    commands->Include(StructureMenu());
    commands->Include(FontMenu2());
    commands->Include(BrushMenu2());
    commands->Include(PatternMenu());
    commands->Include(ColorMenu2("FgColor", fgAttrib));
    commands->Include(ColorMenu2("BgColor", bgAttrib));
    commands->Include(AlignMenu2());
    commands->Include(ViewMenu2());

    return commands;
}

Interactor* IBEditor::Tools2 () {
    HBox* tools = new HBox;

    Include(
        new SelectTool(new ControlInfo("Select", KLBL_SELECT, CODE_SELECT)),
        tools
    );
    Include(
        new MoveTool(new ControlInfo("Move", KLBL_MOVE, CODE_MOVE)),
        tools
    );
    Include(
        new ExamineTool(new ControlInfo("Examine", KLBL_EXAMINE,CODE_EXAMINE)),
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
    Include(
        new NarrowTool(new ControlInfo("Narrow", KLBL_NARROW,CODE_NARROW)),
        tools
    );
    return tools;
}

static const unit = 15;

static Coord xClosed[] = { unit/5, unit, unit, unit*3/5, 0 };
static Coord yClosed[] = { 0, unit/5, unit*3/5, unit, unit*2/5 };
static const int nClosed = 5;

static Coord xOpen[] = { 0, unit/2, unit/2, unit };
static Coord yOpen[] = { 0, unit/4, unit*3/4, unit };
static const int nOpen = 4;

Interactor* IBEditor::GrTools2 () {
    HBox* tools = new HBox;
    TextGraphic* text = new TextGraphic("Text", stdgraphic);
    ITextComp* textComp = new ITextComp(text);
    Include(
        new GraphicCompTool(
            new ControlInfo("Text", KLBL_TEXT, CODE_TEXT), textComp
        ), tools
    );

    Line* line = new Line(0, 0, unit, unit, stdgraphic);
    ILineComp* lineComp = new ILineComp(line);
    Include(
        new GraphicCompTool(
            new ControlInfo(lineComp, KLBL_LINE, CODE_LINE), lineComp
        ), tools
    );

    SF_MultiLine* ml = new SF_MultiLine(xOpen, yOpen, nOpen, stdgraphic);
    ml->SetPattern(psnonepat);
    IMultiLineComp* mlComp = new IMultiLineComp(ml);
    Include(
        new GraphicCompTool(
            new ControlInfo(mlComp, KLBL_MULTILINE, CODE_MULTILINE), mlComp
        ), tools
    );

    SFH_OpenBSpline* spl = new SFH_OpenBSpline(xOpen, yOpen, nOpen,stdgraphic);
    spl->SetPattern(psnonepat);
    ISplineComp* splComp = new ISplineComp(spl);
    Include(
        new GraphicCompTool(
            new ControlInfo(splComp, KLBL_SPLINE, CODE_SPLINE), splComp
        ), tools
    );

    SF_Rect* rect = new SF_Rect(0, 0, unit, unit*4/5, stdgraphic);
    rect->SetPattern(psnonepat);
    IRectComp* rectComp = new IRectComp(rect);
    Include(
        new GraphicCompTool(
            new ControlInfo(rectComp, KLBL_RECT, CODE_RECT), rectComp
        ), tools
    );

    SF_Ellipse* ellipse = new SF_Ellipse(0, 0, unit*2/3, unit*2/5, stdgraphic);
    ellipse->SetPattern(psnonepat);
    IEllipseComp* ellipseComp = new IEllipseComp(ellipse);
    Include(
        new GraphicCompTool(
            new ControlInfo(ellipseComp, KLBL_ELLIPSE, CODE_ELLIPSE),
            ellipseComp
        ), tools
    );
    SF_Polygon* polygon = new SF_Polygon(xClosed, yClosed, nClosed,stdgraphic);
    polygon->SetPattern(psnonepat);
    IPolygonComp* polygonComp = new IPolygonComp(polygon);
    Include(
        new GraphicCompTool(
            new ControlInfo(polygonComp, KLBL_POLY, CODE_POLY), polygonComp
        ), tools
    );

    SFH_ClosedBSpline* cspline = new SFH_ClosedBSpline(
        xClosed, yClosed, nClosed, stdgraphic
    );
    cspline->SetPattern(psnonepat);
    IClosedSplineComp* csplineComp = new IClosedSplineComp(cspline);
    Include(
        new GraphicCompTool(
            new ControlInfo(csplineComp, KLBL_CSPLINE,CODE_CSPLINE),csplineComp
        ), tools
    );

    return tools;
}

static Graphic* ScreenBoundary () {
    Coord x[32], y[32];
    World* world = unidraw->GetWorld();
    Coord xmax = world->Width();
    Coord ymax = world->Height();
    Coord screenWidth = xmax + round(2*inches);
    Coord screenHeight = ymax + round(0.25*inches);

    Coord sx0 = 0, sy0 = 0;
    Coord sx1 = screenWidth;
    Coord sy1 = screenHeight;
    Coord dx = (sx1 - xmax) / 2;
    Coord dy = (sy1 - ymax) / 2;
    Coord fx0 = dx;
    Coord fy0 = dy;
    Coord fx1 = dx + xmax;
    Coord fy1 = dy + ymax;
    Coord sseamx = fx0 + xmax/2;
    Coord sseamy = 0;
    Coord fseamx = sseamx;
    Coord fseamy = fy0;
    
    x[0] = x[1] = x[2] = x[11] = x[12] = x[13] = sseamx;
    y[0] = y[1] = y[2] = y[11] = y[12] = y[13] = sseamy;

    x[3] = x[4] = x[5] = x[6] = sx1;
    x[7] = x[8] = x[9] = x[10] = sx0;

    y[3] = y[4] = sy0;
    y[5] = y[6] = y[7] = y[8] = sy1;
    y[9] = y[10] = sy0;

    x[14] = x[15] = x[16] = x[29] = x[30] = x[31] = fseamx;
    y[14] = y[15] = y[16] = y[29] = y[30] = y[31] = fseamy;

    x[17] = x[18] = x[19] = x[20] = x[21] = x[22] = fx0;
    x[23] = x[24] = x[25] = x[26] = x[27] = x[28] = fx1;

    y[17] = y[18] = y[19] = fy0;
    y[20] = y[21] = y[22] = y[23] = y[24] = y[25] = fy1;
    y[26] = y[27] = y[28] = fy0;

    return new F_ClosedBSpline(x, y, 32, stdgraphic);
}

static const float DEFAULT_XINCR = 8;
static const float DEFAULT_YINCR = 8;

void IBEditor::InitViewer () {
    const int vw = round(VIEWER_WIDTH * inches);
    const int vh = round(VIEWER_HEIGHT * inches);

    Grid* grid = new Grid(vw, vh, DEFAULT_XINCR, DEFAULT_YINCR);
    grid->Visibility(false);

    GraphicView* view = (GraphicView*) _comp->Create(COMPONENT_VIEW);
    _comp->Attach(view);
    _viewer = new Viewer(
	this, view, new UPage(ScreenBoundary()), grid, vw, vh
    );
    view->Update();

}

void IBEditor::Include (Command* cmd, PulldownMenu* pdm) {
    ControlInfo* ctrlInfo = cmd->GetControlInfo();
    UControl* ctrl = new CommandControl(ctrlInfo);
    _keymap->Register(ctrl);
    if (pdm != nil) pdm->Include(ctrl);
    cmd->SetEditor(this);
}

void IBEditor::Include (Tool* tool, Box* box) {
    ControlInfo* ctrlInfo = tool->GetControlInfo();
    UControl* ctrl = new HPanelControl(ctrlInfo, _curCtrl);
    _keymap->Register(ctrl);
    box->Insert(ctrl);
}

PulldownMenu* IBEditor::FontMenu1 () {
    Catalog* catalog = unidraw->GetCatalog();
    PulldownMenu* pdm = MakePulldown("Font");

    int i = 1;
    PSFont* font = catalog->ReadFont(fontAttrib, i);

    while (font != nil) {
        TextGraphic* text = new TextGraphic(
            font->GetPrintFontAndSize(), stdgraphic
        );
        text->SetFont(font);

        Include(new FontCmd(new ControlInfo(new TextComp(text)), font), pdm);
        font = catalog->ReadFont(fontAttrib, ++i);
    }
    return pdm;
}

static float MENU_WIDTH = 1;   /* in cm */
static float MENU_HEIGHT = 0.5;

PulldownMenu* IBEditor::BrushMenu1 () {
    Catalog* catalog = unidraw->GetCatalog();
    PulldownMenu* pdm = MakePulldown("Border");

    int i = 1;
    PSBrush* br = catalog->ReadBrush(borderAttrib, i);

    while (br != nil) {
        ControlInfo* ctrlInfo;

        if (br->None()) {
            ctrlInfo = new ControlInfo("None");

        } else {
            Line* line = new Line(0, 0, round(MENU_WIDTH*cm), 0, stdgraphic);
            line->SetBrush(br);
            ctrlInfo = new ControlInfo(new LineComp(line));
        }
        Include(new BrushCmd(ctrlInfo, br), pdm);
        br = catalog->ReadBrush(borderAttrib, ++i);
    }
    return pdm;
}


PulldownMenu* IBEditor::ColorMenu1 (const char* name, const char* attrib){
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

PulldownMenu* IBEditor::AlignMenu1 () {
    PulldownMenu* pdm = MakePulldown("Align");

    Include(
        new AlignCmd(
            new ControlInfo("Left", KLBL_ALGNLEFT, CODE_ALGNLEFT),
            Left, Left
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Right", KLBL_ALGNRIGHT, CODE_ALGNRIGHT),
            Right, Right
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Bottom", KLBL_ALGNBOT, CODE_ALGNBOT),	
            Bottom, Bottom
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Top", KLBL_ALGNTOP, CODE_ALGNTOP), Top, Top
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Vert Center", KLBL_ALGNVCTR, CODE_ALGNVCTR),
            VertCenter, VertCenter
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Horiz Center", KLBL_ALGNHCTR, CODE_ALGNHCTR),
            HorizCenter, HorizCenter
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("Center", KLBL_ALGNCTR, CODE_ALGNCTR),
            Center, Center
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("TopLeft", " ", ""), 
            TopLeft, TopLeft
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("CenterLeft", " ", ""), 
            CenterLeft, CenterLeft
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("BottomLeft", " ", ""), 
            BottomLeft, BottomLeft
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("TopCenter", " ", ""), 
            TopCenter, TopCenter
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("BottomCenter", " ", ""), 
            BottomCenter, BottomCenter
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("TopRight", " ", ""), 
            TopRight, TopRight
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("CenterRight", " ", ""), 
            CenterRight, CenterRight
        ), pdm
    );
    Include(
        new AlignCmd(
            new ControlInfo("BottomRight", " ", ""), 
            BottomRight, BottomRight
        ), pdm
    );

    return pdm;
}

PulldownMenu* IBEditor::FileMenu1 () {
    PulldownMenu* pdm = MakePulldown("File");

    Include(
        new AboutCmd(new ControlInfo("About ibuild", KLBL_ABOUT, CODE_ABOUT))
    );
    Include(
        new TabCmd(new ControlInfo("Tab", KLBL_TAB, CODE_TAB))
    );
    SceneComp* scomp = new SceneComp;
    scomp->GetClassNameVar()->SetName("[root]");
    scomp->GetClassNameVar()->SetBaseClass("[root]");
    Include(
        new NewCompCmd(
            new ControlInfo("New", KLBL_NEWCOMP, CODE_NEWCOMP), scomp
        ),
        pdm
    );
    Include(
        new RevertCmd(new ControlInfo("Revert", KLBL_REVERT, CODE_REVERT)),
        pdm
    );
    InsertSeparator(pdm);
    Include(
        new IBViewCompCmd(
            new ControlInfo("Open...", KLBL_VIEWCOMP, CODE_VIEWCOMP)
        ), pdm
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
        new CodeCmd(
            new ControlInfo("Generate...", KLBL_PRINT, CODE_PRINT)
        ), pdm
    );
    Include(
        new NewToolCmd(
            new ControlInfo("Create Tool...", KLBL_NEWTOOL, CODE_NEWTOOL)
        ), pdm
    );
    Include(
        new ToolsCmd(
            new ControlInfo("Tools...", KLBL_TOOLS, CODE_TOOLS)
        ), pdm
    );
    Include(
        new ExeCmd(
            new ControlInfo("Execute...", KLBL_EXE, CODE_EXE)
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

PulldownMenu* IBEditor::EditMenu1 () {
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
        new GlueVisibilityCmd(
            new ControlInfo("Show Glue", KLBL_SHOWGLUE, CODE_SHOWGLUE), true
        ), pdm
    );
    Include(
        new GlueVisibilityCmd(
            new ControlInfo("Hide Glue", KLBL_HIDEGLUE, CODE_HIDEGLUE), false
        ), pdm
    );
    Include(
        new PlaceCmd(
            new ControlInfo(
		"Natural Size", KLBL_NATURALSIZE, CODE_NATURALSIZE
	    ), nil
        ), pdm
    );

    return pdm;
}

PulldownMenu* IBEditor::CompositionMenu () {
    PulldownMenu* pdm = MakePulldown("Composition");

    Include(
        new UngroupCmd(
            new ControlInfo("Dissolve Scene", KLBL_UNGROUP, CODE_UNGROUP)
        ), pdm
    );
    InsertSeparator(pdm);
    Include(
        new SceneCmd(
            new ControlInfo("HBox", KLBL_HBOX, CODE_HBOX), new HBoxComp
        ), pdm
    );
    Include(
        new SceneCmd(
            new ControlInfo("VBox", KLBL_VBOX, CODE_VBOX), new VBoxComp
        ), pdm
    );
    Include(
        new SceneCmd(
            new ControlInfo("Deck", KLBL_DECK, CODE_DECK), new DeckComp
        ), pdm
    );
    FrameGraphic* framegr = new FrameGraphic(nil, stdgraphic);
    FrameComp* framecomp = new FrameComp(framegr);
    Include(
        new MonoSceneCmd(
            new ControlInfo("Frame", KLBL_FRAME, CODE_FRAME), framecomp
        ), pdm
    );
    ShadowFrameGraphic* shadowgr = new ShadowFrameGraphic(nil, stdgraphic);
    FrameComp* shadowframe = new FrameComp(shadowgr);
    Include(
        new MonoSceneCmd(
            new ControlInfo(
		"ShadowFrame", KLBL_SHADOWFRAME, CODE_SHADOWFRAME), shadowframe
        ), pdm
    );
    ViewportGraphic* viewportgr = new ViewportGraphic(nil, stdgraphic);
    ViewportComp* viewportcomp = new ViewportComp(viewportgr);
    Include(
        new MonoSceneCmd(
            new ControlInfo("Viewport", KLBL_VIEWPORT, CODE_VIEWPORT), 
	    viewportcomp
        ), pdm
    );

    Include(
        new SceneCmd(
            new ControlInfo("MenuBar", KLBL_MBCMD, CODE_MBCMD),new MenuBarComp
        ), pdm
    );

    Include(
        new MonoSceneCmd(
            new ControlInfo("Shaper", KLBL_SHAPER, CODE_SHAPER),new ShaperComp
        ), pdm
    );
/*
    Include(
        new SceneCmd(
            new ControlInfo("Popup Menu", KLBL_PUCMD, CODE_PUCMD), 
	    new PopupMenuComp
        ), pdm
    );

    InsertSeparator(pdm);
*/

    InsertSeparator(pdm);

    MonoSceneClass* monoSceneClass = new MonoSceneClass(new IBGraphic);
    Include(
        new MonoSceneCmd(
            new ControlInfo(
		"MonoScene Subclass", KLBL_MSCLASS, CODE_MSCLASS
	    ), monoSceneClass
        ), pdm
    );
    DialogClass* dialogClass = new DialogClass(new IBGraphic);
    Include(
        new MonoSceneCmd(
            new ControlInfo(
		"Dialog Subclass", KLBL_DIALOGCLASS, CODE_DIALOGCLASS
	    ), dialogClass
        ), pdm
    );
    EditorComp* edcomp = new EditorComp(new IBGraphic);
    Include(
        new MonoSceneCmd(
            new ControlInfo(
		"Editor Subclass", KLBL_EDCOMP, CODE_EDCOMP
	    ), edcomp
        ), pdm
    );

    InsertSeparator(pdm);

    Include(
        new ReorderCmd(
            new ControlInfo("Reorder", KLBL_REORDER, CODE_REORDER)
        ), pdm
    );
    Include(
        new FrontCmd(
            new ControlInfo("Raise", KLBL_FRONT, CODE_FRONT)
        ), pdm
    );
    Include(
        new BackCmd(
            new ControlInfo("Lower", KLBL_BACK, CODE_BACK)
        ), pdm
    );

    return pdm;
}

PulldownMenu* IBEditor::ViewMenu1 () {
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
        new NavigateCmd(
            new ControlInfo("View Parent", KLBL_VIEWPARENT, CODE_VIEWPARENT)
        ), pdm
    );
    Include(
        new NavigateCmd(
            new ControlInfo("View Root", KLBL_VIEWROOT, CODE_VIEWROOT), true
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
            new ControlInfo("Center Screen", KLBL_CENTER, CODE_CENTER)
        ), pdm
    );

    return pdm;
}

PulldownMenu* IBEditor::FileMenu2 () {
    PulldownMenu* pdm = MakePulldown("File");

    Include(
        new AboutCmd(new ControlInfo("About ibuild", KLBL_ABOUT, CODE_ABOUT))
    );

    SceneComp* scomp = new SceneComp;
    scomp->GetClassNameVar()->SetName("[root]");
    scomp->GetClassNameVar()->SetBaseClass("[root]");
    Include(
        new NewCompCmd(
            new ControlInfo("New", KLBL_NEWCOMP, CODE_NEWCOMP), scomp
        ),
        pdm
    );
    Include(
        new RevertCmd(new ControlInfo("Revert", KLBL_REVERT, CODE_REVERT)),
        pdm
    );
    InsertSeparator(pdm);
    Include(
        new ViewCompCmd(
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
        new CodeCmd(
            new ControlInfo("Generate...", KLBL_PRINT, CODE_PRINT)
        ), pdm
    );
    Include(
        new PrintCmd(
            new ControlInfo("Print...", KLBL_PRINT, CODE_PRINT)
        ), pdm
    );
    Include(
        new IBImportCmd(
            new ControlInfo("Import Graphic...", KLBL_IMPORT, CODE_IMPORT)
        ), pdm
    );
    Include(
        new ExeCmd(
            new ControlInfo("Execute...", KLBL_EXE, CODE_EXE)
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

PulldownMenu* IBEditor::EditMenu2 () {
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

PulldownMenu* IBEditor::StructureMenu () {
    PulldownMenu* pdm = MakePulldown("Structure");

    IGraphicComps* igrcomp = new IGraphicComps;
    igrcomp->Instantiate();

    Include(
        new GroupCmd(
            new ControlInfo("Group", KLBL_GROUP, CODE_GROUP), igrcomp
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

PulldownMenu* IBEditor::FontMenu2 () {
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

PulldownMenu* IBEditor::BrushMenu2 () {
    ControlInfo* ctrlInfo;
    Line* line;
    Catalog* catalog = unidraw->GetCatalog();
    PulldownMenu* pdm = MakePulldown("Brush");

    int i = 1;
    PSBrush* br = catalog->ReadBrush(brAttrib, i);

    while (br != nil) {

        if (br->None()) {
            ctrlInfo = new ControlInfo("None");

        } else {
            line = new Line(0, 0, round(MENU_WIDTH*cm), 0, stdgraphic);
            line->SetBrush(br);
            ctrlInfo = new ControlInfo(new LineComp(line));
        }
        Include(new BrushCmd(ctrlInfo, br), pdm);
        br = catalog->ReadBrush(brAttrib, ++i);
    }

    return pdm;
}

PulldownMenu* IBEditor::PatternMenu () {
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

PulldownMenu* IBEditor::ColorMenu2 (const char* name, const char* attrib) {
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

PulldownMenu* IBEditor::AlignMenu2 () {
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

PulldownMenu* IBEditor::ViewMenu2 () {
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
        new NavigateCmd(
            new ControlInfo("View Parent", KLBL_VIEWPARENT, CODE_VIEWPARENT)
        ), pdm
    );
    Include(
        new NavigateCmd(
            new ControlInfo("View Root", KLBL_VIEWROOT, CODE_VIEWROOT), true
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
        new GridSpacingCmd(
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
