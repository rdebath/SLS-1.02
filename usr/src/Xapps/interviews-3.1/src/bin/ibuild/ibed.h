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
 * IBEditor for user interface builder, derived from Editor class.
 */

#ifndef ibed_h
#define ibed_h

#include <Unidraw/editor.h>

class Box;
class BrushVar;
class ColorVar;
class Command;
class CompNameVar;
class ControlState;
class Control;
class Deck;
class EditorInfo;
class FontVar;
class GraphicComp;
class GravityVar;
class MagnifVar;
class ModifStatusVar;
class PatternVar;
class PSBrush;
class PSColor;
class PSFont;
class PSPattern;
class PulldownMenu;
class ToolPanel;

class IBEditor : public Editor {
public:
    IBEditor(GraphicComp* = nil);
    IBEditor(const char* file);
    virtual ~IBEditor();

    GraphicComp* GetGraphicComp();
    ToolPanel* GetToolPanel();

    virtual Component* GetComponent();
    virtual Viewer* GetViewer(int = 0);
    virtual KeyMap* GetKeyMap();
    virtual Tool* GetCurTool();
    virtual Selection* GetSelection();
    virtual StateVar* GetState(const char*);

    virtual void SetComponent(Component*);
    virtual void SetViewer(Viewer*, int = 0);
    virtual void SetKeyMap(KeyMap*);
    virtual void SetSelection(Selection*);

private:
    void Init(GraphicComp* = nil);
    void InitStateVars();
    void InitViewer();
    Interactor* Interior();

    Interactor* CreateCommands();
    Interactor* CreateTools();
    Interactor* CreateGrTools();

    Interactor* Commands1();
    Interactor* Tools1();
    Interactor* GrTools1();

    Interactor* Commands2();
    Interactor* Tools2();
    Interactor* GrTools2();

    void Include(Command*, PulldownMenu* = nil);
    void Include(Tool*, Box*);

    PulldownMenu* FileMenu1();
    PulldownMenu* EditMenu1();
    PulldownMenu* CompositionMenu();

    PulldownMenu* FontMenu1();
    PulldownMenu* BrushMenu1();
    PulldownMenu* ColorMenu1(const char* name, const char* attrib);
    PulldownMenu* AlignMenu1();
    PulldownMenu* ViewMenu1();

    PulldownMenu* FileMenu2();
    PulldownMenu* EditMenu2();
    PulldownMenu* StructureMenu();

    PulldownMenu* FontMenu2();
    PulldownMenu* BrushMenu2();
    PulldownMenu* PatternMenu();
    PulldownMenu* ColorMenu2(const char* name, const char* attrib);
    PulldownMenu* AlignMenu2();
    PulldownMenu* ViewMenu2();

    void CheckMode();
    boolean GraphicsMode();

    void SwitchBrush();
    void SwitchColors();
    void SwitchFont();
    void SwitchPattern();
private:
    GraphicComp* _comp;
    KeyMap* _keymap;
    KeyMap* _keymap1;
    KeyMap* _keymap2;
    ControlState* _curCtrl;
    ControlState* _curCtrl1;
    ControlState* _curCtrl2;
    Viewer* _viewer;
    Selection* _selection;
    ToolPanel* _toolpanel;

    Deck* _tooldeck;
    Deck* _commanddeck;
    Deck* _grtooldeck;

    CompNameVar* _name;
    ModifStatusVar* _modifStatus;
    GravityVar* _gravity;
    MagnifVar* _magnif;

    FontVar* _font;
    BrushVar* _brush;
    ColorVar* _color;
    PatternVar* _pattern;

    PSBrush* _prevBrush;
    PSPattern* _prevPattern;
    PSFont* _prevFont;
    PSColor* _prevFg;
    PSColor* _prevBg;

    boolean _gravity_on;
    boolean _grid_on;
    boolean _graphics_mode;
};

inline GraphicComp* IBEditor::GetGraphicComp () { return _comp; }

#endif
