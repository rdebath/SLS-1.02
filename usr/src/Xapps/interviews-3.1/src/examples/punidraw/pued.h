#ifndef pued_h
#define pued_h

#include <Unidraw/editor.h>

class GraphicComp;

class Punidraw : public Editor {
public:
    Punidraw(GraphicComp* = nil);
    virtual ~Punidraw();

    virtual Component* GetComponent();
    virtual Viewer* GetViewer(int = 0);
    virtual KeyMap* GetKeyMap();
    virtual Tool* GetCurTool();
    virtual Selection* GetSelection();

    virtual void SetComponent(Component*);
    virtual void SetViewer(Viewer*, int = 0);
    virtual void SetKeyMap(KeyMap*);
    virtual void SetSelection(Selection*);
private:
    Interactor* Interior();
    void InitViewer();

    Interactor* Commands();
    Interactor* Tools();

    void Include(class Command*, class PulldownMenu*);
    void Include(Tool*, class Box*);

    PulldownMenu* FileMenu();
    PulldownMenu* EditMenu();
private:
    GraphicComp* _comp;
    KeyMap* _keymap;
    class ControlState* _curCtrl;
    Viewer* _viewer;
    Selection* _selection;
};

#endif
