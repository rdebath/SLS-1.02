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
 * Implementation of user interface builder-specific manipulators.
 */

#include "ibcmds.h"
#include "ibmanips.h"
#include "ibtools.h"
#include "ibglobals.h"
#include "ibinteractor.h"
#include "ibclasses.h"

#include <InterViews/control.h>
#include <InterViews/event.h>
#include <InterViews/frame.h>
#include <InterViews/message.h>
#include <InterViews/rubgroup.h>
#include <InterViews/rubline.h>
#include <InterViews/sensor.h>
#include <InterViews/shape.h>
#include <InterViews/world.h>

#include <Unidraw/iterator.h>
#include <Unidraw/selection.h>
#include <Unidraw/viewer.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/editor.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Components/pin.h>

/*****************************************************************************/

PopupManip::PopupManip (Viewer* viewer, PopupMenu* popup, Tool* tool) {
    _viewer = viewer;
    _popup = popup;
    _tool = tool;
}

PopupManip::~PopupManip () { delete _popup; }
void PopupManip::Grasp (Event& e) { _popup->Popup(e); }
void PopupManip::Effect (Event&) { _popup->Unselect(); }

boolean PopupManip::Manipulating (Event& e) {
    e.target->Handle(e);
    return e.leftmouse;
}

void PopupManip::SetViewer (Viewer* viewer) { _viewer = viewer; }
void PopupManip::SetTool (Tool* tool) { _tool = tool; }
Viewer* PopupManip::GetViewer () { return _viewer; }
Tool* PopupManip::GetTool () { return _tool; }

/*****************************************************************************/

NarrowManip::NarrowManip (Viewer* viewer) {
    _viewer = viewer;
    _parent = _kid = nil;
}

void NarrowManip::Grasp (Event& e) {
    Iterator i;
    GraphicView* views = _viewer->GetGraphicView();
    Selection* s = _viewer->GetSelection(), *newSel = new Selection;
    boolean success = true;

    s->Clear();
    ComputeViewPath(e, views, newSel);

    if (!newSel->IsEmpty()) {
        newSel->First(i);
        GraphicView* gv = newSel->GetView(i);
        s->Append(gv);
        s->Update();
        _parent = gv->GetGraphicComp();
        newSel->Next(i);
        if (!newSel->Done(i)) {
            gv = newSel->GetView(i);
            _kid = gv->GetGraphicComp();
        }
    }
    delete newSel;
}

boolean NarrowManip::Manipulating (Event& e) {
    if (e.eventType == MotionEvent) {
        Iterator i;
        GraphicView* views = _viewer->GetGraphicView();
        Selection* s = _viewer->GetSelection(), *newSel = new Selection;
        
        ComputeViewPath(e, views, newSel);
        
        if (!newSel->IsEmpty()) {
            newSel->First(i);
            GraphicView* gv = newSel->GetView(i);
            if (!s->Includes(gv)) {
                s->Clear();
                s->Append(gv);
                s->Update();
                _parent = gv->GetGraphicComp();
                newSel->Next(i);
                if (!newSel->Done(i)) {
                    GraphicView* gv = newSel->GetView(i);
                    _kid = gv->GetGraphicComp();
                } else {
                    _kid = nil;
                }
            }
        } else {
            s->Clear();
            _parent = _kid = nil;
        }
        delete newSel;
    } else if (e.eventType == UpEvent) {
        return false;
    }
    
    return true;
}

/*****************************************************************************/

void IComputeViewPath (Event& e, GraphicView* views, Selection* s){
    Selection* newSel =
        views->ViewIntersecting(e.x-SLOP, e.y-SLOP, e.x+SLOP, e.y+SLOP);

    Iterator i;
    for (newSel->First(i); !newSel->Done(i); newSel->Next(i)) {
        GraphicView* gv = newSel->GetView(i);
	if (gv != nil && gv->IsA(INTERACTOR_VIEW)) {
            s->Append(gv);
            IComputeViewPath(e, gv, s);
	}
    }
    delete newSel;
}

inline boolean SameFirewall (InteractorComp* src, InteractorComp* dest) {
    GetFirewallCmd sCmd(src);
    GetFirewallCmd dCmd(dest);
    sCmd.Execute();
    dCmd.Execute();
    return sCmd.GetFirewall() == dCmd.GetFirewall();
}

/*****************************************************************************/

RelateManip::RelateManip (Viewer* v, Tool* tool) {
    _viewer = v;
    _tool = tool;
    _drag = nil;
    _src = _dest = nil;
    _cont = true;
}


boolean RelateManip::CreatePopupMenu(Event& e, RelateMenu*& menu) {
    Iterator i, j;
    GraphicView* views = _viewer->GetGraphicView();
    Selection* s = _viewer->GetSelection(), *newSel = new Selection;

    s->Clear();
    IComputeViewPath(e, views, newSel);
    menu = nil;
    boolean hit = false;

    if (!newSel->IsEmpty()) {
        hit = true;
        newSel->Last(j);
        GraphicView* gv = newSel->GetView(j);
	InteractorComp* icomp = (InteractorComp*) gv->GetGraphicComp();
	if (_src == nil || SameFirewall(icomp, _src)) {
           newSel->First(j);
            GraphicView* gv = newSel->GetView(j);
            s->Append(gv);
            s->Update();

            menu = new RelateMenu();

            for (newSel->First(i); !newSel->Done(i); newSel->Next(i)) {
                GraphicView* view = s->GetView(i);
                InteractorComp* comp = (InteractorComp*)view->GetGraphicComp();
                menu->Include(
                    new RelateItem(
                        GetName(
                            comp, e.shift_is_down(), true
                        ), Center, comp, menu
                    )
                );
            }
        }
    }
    delete newSel;
    return hit;
}

void RelateManip::GetSrcDest(InteractorComp*& src, InteractorComp*& dest) {
    src = _src;
    dest = _dest;
}

RelateManip::~RelateManip () { 
    if (_drag != nil) {
	delete _drag;
    }
}

static boolean Relatable (InteractorComp* src) {
    boolean success = false;
    if (src->IsRelatable()) {
        success = true;

    } else {
        Iterator i;
        for(src->First(i); !src->Done(i); src->Next(i)) {
            InteractorComp* kid = (InteractorComp*) src->GetComp(i);
            success = Relatable(kid);
            if (success) {
                break;
            }
        }
    }
    return success;
}

static boolean Relatable (InteractorComp* src, InteractorComp* dest) {
    boolean success = false;
    if (src->IsRelatableTo(dest) || dest->IsRelatableTo(src)) {
        success = true;

    } else if (src->IsAScene() && !dest->IsAScene()) {
        Iterator i;
        for(src->First(i); !src->Done(i); src->Next(i)) {
            InteractorComp* kid = (InteractorComp*) src->GetComp(i);
            success = Relatable(kid, dest);
            if (success) {
                break;
            }
        }
    } else if (dest->IsAScene() && !src->IsAScene()) {
        Iterator i;
        for (dest->First(i); !dest->Done(i); dest->Next(i)) {
            InteractorComp* kid = (InteractorComp*) dest->GetComp(i);
            success = Relatable(src, kid);
            if (success) {
                break;
            }
        }
    }
    return success;
}

void RelateManip::Manipulate(Manipulator* m, Event& e) {
    boolean b;
    m->Grasp(e);
    do {
        _viewer->Read(e);
	b = m->Manipulating(e);
    } while (b);
    m->Effect(e);
}

void RelateManip::Grasp (Event& e) { 
    RelateMenu* src_menu;
    Coord cx = e.x, cy = e.y;
    CreatePopupMenu(e, src_menu);

    if (src_menu != nil) {
	PopupManip* popup = new PopupManip(_viewer, src_menu, _tool);
        Manipulate(popup, e);
	_src = src_menu->GetInteractorComp();
	delete popup;

	if (_src != nil && Relatable(_src)) {
            _drag = new RubberGroup(nil, nil);
            
            _drag->Append(
                new SlidingPin(nil, nil, cx, cy, PIN_RAD, cx, cy),
                new RubberLine(nil, nil, cx, cy, cx, cy),
                new FixedPin(nil, nil, cx, cy, PIN_RAD)
            );
            
            _viewer->InitRubberband(_drag);
            _drag->Track(cx, cy);
        }
    }
}
	
boolean RelateManip::Manipulating (Event& e) {
    RelateMenu* dest_menu;
    if (_drag == nil) {
        _cont = false;

    } else if (e.eventType == MotionEvent) {
        _drag->Track(e.x, e.y);

    } else if (e.eventType == DownEvent) {
	boolean hit = CreatePopupMenu(e, dest_menu);

	if (dest_menu != nil) {
	    PopupManip* popup = new PopupManip(_viewer, dest_menu, _tool);
            Manipulate(popup, e);
	    _dest = dest_menu->GetInteractorComp();
	    delete popup;

	    if (SemanticCheck()) {
	        _cont = false;
	    }
        } else {
            _cont = hit;
        }
    }
    return _cont;
}

void RelateManip::Effect(Event&) {
    if (_drag != nil) {
        _drag->Erase();
    }
}


boolean RelateManip::SemanticCheck () {
   boolean ok = false;

   return (
       _src != nil && _dest != nil && 
       Relatable(_src, _dest) &&
       SameFirewall(_src, _dest)
   );
}

void RelateManip::SetViewer (Viewer* viewer) { _viewer = viewer; }
void RelateManip::SetTool (Tool* tool) { _tool = tool; }
Viewer* RelateManip::GetViewer () { return _viewer; }
Tool* RelateManip::GetTool () { return _tool; }

RelateItem::RelateItem(
    const char* string, Alignment al, InteractorComp* icomp, RelateMenu* m
) : MenuItem(string, al) {
    _relatedcomp = icomp;
    _menu = m;
}

void RelateItem::Do () {
    _menu->SetInteractorComp(_relatedcomp);
}

/*************************************************************************/

ExamineMenu::ExamineMenu() {}

void ExamineMenu::InsertBody(IntCoord x, IntCoord y) {
    Interactor* i = GetScene();
    PopupMenu::InsertBody(x, y - i->GetShape()->height/4);
}

/*************************************************************************/

H_PopupMenu::H_PopupMenu() {
    _count = 0;
    _center = 0;
}

void H_PopupMenu::Include(Control* menu) {
    PopupMenu::Include(menu);
    _count++;
}
    
void H_PopupMenu::InsertBody(IntCoord x, IntCoord y) {
    Interactor* i = GetScene();
    float height = float(i->GetShape()->height);
    float fratio = float(_center)/float(_count);

    int adjustment = round(height * (1.0/2.0 - fratio + 0.5/float(_count)));
    IntCoord adjy = y-adjustment;
    PopupMenu::InsertBody(x, adjy);
}

/*************************************************************************/

H_PullrightMenu::H_PullrightMenu(Interactor* i) : PullrightMenu(i) {
    _count = 0;
}

void H_PullrightMenu::Include(Control* menu) {
    PullrightMenu::Include(menu);
    _count++;
}
    
void H_PullrightMenu::InsertBody(IntCoord x, IntCoord y) {
    Interactor* i = GetScene();
    float height = float(i->GetShape()->height);

    int adjustment = round(height * (1.0 - 1.0/float(_count)));
    IntCoord adjy = y + adjustment + 1;
    PullrightMenu::InsertBody(x, adjy);
}

/*************************************************************************/

RelateMenu::RelateMenu() { Init(); }

void RelateMenu::Init() {
    SetClassName("RelateMenu");
    SetState(new ControlState);
    SetAlign(TopCenter);
    _relatedcomp = nil;
    _count = 0;
}

void RelateMenu::Include(Control* menu) {
    PopupMenu::Include(menu);
    _count++;
}
    
void RelateMenu::InsertBody(IntCoord x, IntCoord y) {
    Interactor* i = GetScene();
    Shape* s = i->GetShape();
    PopupMenu::InsertBody(x, y + s->height/2 - s->height/_count/2);
}
