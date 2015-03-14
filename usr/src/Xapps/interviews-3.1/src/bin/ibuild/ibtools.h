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
 * User interface builder-specific tools.
 */

#ifndef ibtools_h
#define ibtools_h

#include <InterViews/menu.h>
#include <Unidraw/Tools/grcomptool.h>
#include <Unidraw/Tools/reshape.h>

class Control;
class Editor;
class Event;
class GraphicComp;
class GraphicView;
class NavigateCmd;
class Selection;

class TabTool : public ReshapeTool {
public:
    TabTool();
    virtual Manipulator* CreateManipulator(Viewer*, Event&, Transformer* =nil);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class CommandItem : public MenuItem {
public:
    CommandItem(const char*, Alignment, Command*);
    virtual ~CommandItem();

    virtual void Do();
private:
    Command* _cmd;
    boolean _executed;
};

class ExamineTool : public Tool {
public:
    ExamineTool(ControlInfo* = nil);
    virtual ~ExamineTool();

    virtual Manipulator* CreateManipulator(Viewer*, Event&, Transformer* =nil);

    Control* CreateInfoEntry(Selection*, Editor*);
    Control* CreatePropsEntry(Selection*, Editor*);
    Selection* GetSelPath();

    virtual Tool* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    Selection* _selPath;
    boolean _shift;
};

inline Selection* ExamineTool::GetSelPath () { return _selPath; }

class NarrowTool : public Tool {
public:
    NarrowTool(ControlInfo* = nil);

    virtual Manipulator* CreateManipulator(Viewer*, Event&, Transformer* =nil);
    virtual Command* InterpretManipulator(Manipulator*);

    virtual Tool* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    Control* CreateViewEntry(Selection*, Editor*);
    Manipulator* CreatePopupManip(Selection*, Viewer*);
private:
    boolean _popup;
};

class RelateTool : public Tool {
public:
    RelateTool(ControlInfo* = nil);
    virtual Manipulator* CreateManipulator(Viewer*, Event&, Transformer* =nil);
    virtual Command* InterpretManipulator(Manipulator*);
    virtual Tool* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};
    
class IBGraphicCompTool : public GraphicCompTool {
public:
    IBGraphicCompTool();
    IBGraphicCompTool(ControlInfo*, GraphicComp* prototype);

    virtual Tool* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

extern void ComputeViewPath (Event& e, GraphicView* views, Selection* s);

#endif
