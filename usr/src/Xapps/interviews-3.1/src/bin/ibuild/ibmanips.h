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
 * User interface builder-specific manipulators.
 */

#ifndef ibmanips_h
#define ibmanips_h

#include <Unidraw/manips.h>
#include <InterViews/menu.h>

class ConnectorView;
class Frame;
class GraphicComp;
class InteractorComp;
class PopupMenu;
class RubberGroup;
class RelateMenu;

class PopupManip : public Manipulator {
public:
    PopupManip(Viewer*, PopupMenu*, Tool* = nil);
    virtual ~PopupManip();

    virtual void Grasp(Event&);
    virtual boolean Manipulating(Event&);
    virtual void Effect(Event&);

    virtual void SetViewer(Viewer*);
    virtual void SetTool(Tool*);

    virtual Viewer* GetViewer();
    virtual Tool* GetTool();

    PopupMenu* GetPopupMenu();
private:
    Viewer* _viewer;
    PopupMenu* _popup;
    Tool* _tool;
};

inline PopupMenu* PopupManip::GetPopupMenu () { return _popup; }

class NarrowManip : public Manipulator {
public:
    NarrowManip(Viewer*);

    virtual void Grasp(Event&);
    virtual boolean Manipulating(Event&);

    virtual void SetViewer(Viewer*);
    virtual Viewer* GetViewer();

    GraphicComp* GetParent();
    GraphicComp* GetKid();
private:
    Viewer* _viewer;
    GraphicComp* _parent;
    GraphicComp* _kid;
};

inline void NarrowManip::SetViewer(Viewer* viewer) { _viewer = viewer; }
inline Viewer* NarrowManip::GetViewer() { return _viewer; }
inline GraphicComp* NarrowManip::GetParent() { return _parent; }
inline GraphicComp* NarrowManip::GetKid() { return _kid; }

class RelateManip : public Manipulator {
public:
    RelateManip(Viewer*, Tool* = nil);
    virtual ~RelateManip();

    virtual void Grasp(Event&);
    virtual boolean Manipulating(Event&);
    virtual void Effect(Event&);

    virtual void SetViewer(Viewer*);
    virtual void SetTool(Tool*);

    virtual Viewer* GetViewer();
    virtual Tool* GetTool();

    void GetSrcDest(InteractorComp*&, InteractorComp*&);

    boolean CreatePopupMenu(Event&, RelateMenu*&);
    boolean SemanticCheck();
protected:
    void Manipulate(Manipulator*, Event&);
private:
    Viewer* _viewer;
    Tool* _tool;
    InteractorComp* _src;
    InteractorComp* _dest;
    RubberGroup* _drag;
    boolean _cont;
};

class RelateItem : public MenuItem {
public:
    RelateItem(const char*, Alignment, InteractorComp*, RelateMenu*);
    InteractorComp* GetInteractorComp();

    virtual void Do();    
private:
    InteractorComp* _relatedcomp;
    RelateMenu* _menu;
};

inline InteractorComp* RelateItem::GetInteractorComp () { 
    return _relatedcomp; 
}

class ExamineMenu : public PopupMenu {
public:
    ExamineMenu();

protected:
    virtual void InsertBody(IntCoord, IntCoord);
};

class H_PopupMenu : public PopupMenu {
public:
    H_PopupMenu();
    virtual void Include(Control*);
    void LockPosition();

protected:
    virtual void InsertBody(IntCoord, IntCoord);
private:
    int _count;
    int _center;
};

inline void H_PopupMenu::LockPosition() { _center = _count; }

class H_PullrightMenu : public PullrightMenu {
public:
    H_PullrightMenu(Interactor*);
    virtual void Include(Control*);
protected:
    virtual void InsertBody(IntCoord, IntCoord);
private:
    int _count;
};

class RelateMenu : public PopupMenu {
public:
    RelateMenu();
    virtual void Include(Control*);

    void Init();
    InteractorComp* GetInteractorComp();
    void SetInteractorComp(InteractorComp*);
protected:
    virtual void InsertBody(IntCoord, IntCoord);
private:
    InteractorComp* _relatedcomp;
    int _count;
};

inline InteractorComp* RelateMenu::GetInteractorComp () { 
    return _relatedcomp; 
}

inline void RelateMenu::SetInteractorComp(InteractorComp* comp) {
    _relatedcomp = comp;
}
#endif
