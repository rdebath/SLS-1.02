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
 * Main class for idraw editor, derived from Editor class.
 */

#ifndef ided_h
#define ided_h

#include "idcomp.h"

#include <Unidraw/editor.h>

class ArrowVar;
class Box;
class BrushVar;
class ColorVar;
class Command;
class CompNameVar;
class ControlState;
class FontVar;
class GravityVar;
class MagnifVar;
class ModifStatusVar;
class PatternVar;
class PulldownMenu;
class Tray;

class IdrawEditor : public Editor {
public:
    IdrawEditor(GraphicComp* = nil);
    IdrawEditor(const char* file);
    virtual ~IdrawEditor();

    GraphicComp* GetGraphicComp();

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
    Interactor* Interior();
    void Init(GraphicComp* = nil);
    void InitEditorInfo();
    void InitStateVars();
    void InitViewer();

    Interactor* Commands();
    Interactor* Tools();

    void Include(Command*, PulldownMenu* = nil);
    void Include(Tool*, Box*);

    PulldownMenu* FileMenu();
    PulldownMenu* EditMenu();
    PulldownMenu* StructureMenu();
    PulldownMenu* FontMenu();
    PulldownMenu* BrushMenu();
    PulldownMenu* PatternMenu();
    PulldownMenu* ColorMenu(const char* name, const char* attrib);
    PulldownMenu* AlignMenu();
    PulldownMenu* ViewMenu();
private:
    GraphicComp* _comp;
    KeyMap* _keymap;
    ControlState* _curCtrl;
    Viewer* _viewer;
    Selection* _selection;
    class Tray* _tray;

    class CompNameVar* _name;
    class ModifStatusVar* _modifStatus;
    class GravityVar* _gravity;
    class MagnifVar* _magnif;
    class FontVar* _font;
    class BrushVar* _brush;
    class PatternVar* _pattern;
    class ColorVar* _color;
    class ArrowVar* _arrows;
};

inline GraphicComp* IdrawEditor::GetGraphicComp () { return _comp; }

#endif
