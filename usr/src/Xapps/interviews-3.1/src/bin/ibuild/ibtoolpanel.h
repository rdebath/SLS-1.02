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
 * ToolPanel - a panel of controls tiled horizontally (on separate lines 
 * if necessary) that automatically includes a default set of controls
 * for engaging the current tool.  ToolPanel also provides convenience 
 * functions for installing and removing tools.
 */

#ifndef ibtoolpanel_h
#define ibtoolpanel_h

#include <InterViews/scene.h>

class ControlState;
class EditorInfo;
class KeyMap;
class Tool;
class UControl;

class ToolPanel : public MonoScene {
public:
    ToolPanel(ControlState*, KeyMap*);
    virtual ~ToolPanel();

    EditorInfo* GetEditorInfo();

    boolean InstallOrRemove(Tool* tool, const char* toolName);
    void Install(const char* toolName, Tool* tool = nil);
    void Uninstall(const char* toolName);
protected:
    void AddTool(UControl*);
    void RemoveTool(UControl*);
private:
    void InitEditorInfo();
    Tool* GetCurTool();
    void Include(Tool*, const char* = "");

    void InitTools();
    void InitCompTools();
    void ReadCompTools();

    class UControl* FindControl(const char* toolName);
    Scene* GetScene();
private:
    KeyMap* _keymap;
    ControlState* _curCtrl;

    EditorInfo* _edInfo;
    boolean _initEdInfo;
};

inline EditorInfo* ToolPanel::GetEditorInfo () { return _edInfo; }

#endif
