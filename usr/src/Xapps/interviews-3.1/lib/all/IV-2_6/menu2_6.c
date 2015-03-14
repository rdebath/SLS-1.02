/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * Implementation of common menus.
 */

#include <InterViews/event.h>
#include <InterViews/pattern.h>
#include <InterViews/window.h>
#include <IV-2_6/InterViews/box.h>
#include <IV-2_6/InterViews/frame.h>
#include <IV-2_6/InterViews/glue.h>
#include <IV-2_6/InterViews/menu.h>
#include <IV-2_6/InterViews/message.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/shape.h>
#include <IV-2_6/InterViews/world.h>
#include <OS/math.h>
#include <stdlib.h>

#include <IV-2_6/_enter.h>

/** class MenuItem **/

MenuItem::MenuItem(Interactor* i) : Control(i) { Init(); }
MenuItem::MenuItem(const char* name, Interactor* i) : Control(name, i) {
    Init();
}

MenuItem::MenuItem(
    const char* str, Alignment al
) : Control(new Message(str, al, 2, hfil, 0)) {
    Init();
}

MenuItem::MenuItem(
    const char* name,
    const char* str, Alignment al
) : Control(name, new Message(str, al, 2, hfil, 0)) {
    Init();
}

MenuItem::~MenuItem() { }

void MenuItem::Init() {
    SetClassName("MenuItem");
}

void MenuItem::Busy() {
    RootControl()->Highlight(true);
}

void MenuItem::Done() {
    RootControl()->Highlight(false);
}

/** class MenuShadow **/

class MenuShadow : public MonoScene {
public:
    static Pattern* halftone;
    int depth_;

    MenuShadow(Interactor*, int depth = 4);
    virtual ~MenuShadow();

    virtual void Reconfig();
    virtual void Resize();
    virtual void Redraw(IntCoord, IntCoord, IntCoord, IntCoord);
};

Pattern* MenuShadow::halftone;

MenuShadow::MenuShadow(Interactor* i, int d) {
    SetClassName("MenuShadow");
    depth_ = d;
    if (halftone == nil) {
	halftone = new Pattern(Pattern::gray);
	halftone->ref();
    }
    if (i != nil) {
	Insert(new Frame(i));
    }
}

MenuShadow::~MenuShadow() { }

void MenuShadow::Reconfig() {
    MonoScene::Reconfig();
    const char* d = GetAttribute("depth");
    if (d != nil) {
	depth_ = atoi(d);
    }
    shape->width += depth_;
    shape->height += depth_;
}

void MenuShadow::Resize() {
    if (interior() != nil) {
	Place(interior(), 0, depth_, xmax - depth_, ymax);
    }
}

void MenuShadow::Redraw(IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2) {
    if (x2 >= depth_ && y1 <= ymax - depth_) {
	const Pattern* p = output->GetPattern();
	Resource::ref(p);
	boolean b = output->BgFilled();
	output->SetPattern(halftone);
	output->FillBg(false);
	IntCoord left = Math::max(x1, depth_);
	IntCoord top = Math::min(y2, ymax - depth_);
	output->FillRect(canvas, left, y1, x2, top);
	output->FillBg(b);
	output->SetPattern(p);
	Resource::unref(p);
    }
}

/** class Menu **/

Menu::Menu(Interactor* i) : Control(i) { Init(); }
Menu::Menu(const char* name, Interactor* i) : Control(name, i) { Init(); }

void Menu::Init() {
    SetClassName("Menu");
    state_ = new ControlState;
    state_->Reference();
    scene_ = new VBox;
    body_ = new MenuShadow(scene_);
    depth_ = 3;
    align_ = BottomLeft;
    world_ = nil;
    rel_x_ = 0;
    rel_y_ = 0;
    ins_x_ = 0;
    ins_y_ = 0;
}

Menu::~Menu() {
    delete body_;
    Unref(state_);
}

void Menu::SetAlign(Alignment a) { align_ = a; }
void Menu::SetDepth(int d) { depth_ = d; }

void Menu::SetBodyState(ControlState* s) {
    Unref(state_);
    state_ = s;
    s->Reference();
}

void Menu::SetScene(Scene* s) {
    delete body_;
    scene_ = s;
    body_ = nil;
    for (Scene* p = scene_; p != nil; p = p->Parent()) {
	body_ = p;
    }
}

void Menu::Include(Control* c) {
    scene_->Insert(c);
    c->SetState(GetBodyState());
    Reparent(c, this);
}

void Menu::Reconfig() {
    Control::Reconfig();
    world_ = GetWorld();
    Setup();
}

void Menu::Setup() {
    body_->Config(world_);
    Shape* s = body_->GetShape();
    rel_x_ = s->width / 2;
    rel_y_ = s->height / 2;
}

void Menu::Popup(Event& e) {
    if (Enabled()) {
	World* w;
	IntCoord wx, wy;
	e.GetAbsolute(w, wx, wy);
	if (w != world_) {
	    world_ = w;
	    Setup();
	}
	InsertBody(wx - rel_x_, wy - rel_y_);
	State()->Selection(this);
    }
}

void Menu::Leave() { }

void Menu::Open() {
    IntCoord x, y;
    Align(align_, 0, 0, x, y);
    GetRelative(x, y);
    InsertBody(x, y - body_->GetShape()->height);
}

void Menu::InsertBody(IntCoord x, IntCoord y) {
    ins_x_ = x;
    ins_y_ = y;
    world_->InsertPopup(body_, x, y);
    State()->Push(state_);
    state_->Activate();
    world_->Flush();
}

void Menu::Close() {
    if (body_->GetCanvas() != nil) {
	Control* action = state_->Action();
	state_->Action(nil);
	if (action != nil) {
	    action->Align(Center, 0, 0, rel_x_, rel_y_);
	    action->GetRelative(rel_x_, rel_y_, body_);
	}
    }
    state_->Pop();
    world_->Remove(body_);
    world_->Flush();
}

/** class MenuBar **/

MenuBar::MenuBar() { Init(); }
MenuBar::MenuBar(const char* name) { SetInstance(name); Init(); }

void MenuBar::Init() {
    SetClassName("MenuBar");
    state_ = new ControlState;
    state_->Reference();
}

MenuBar::~MenuBar() {
    Unref(state_);
}

void MenuBar::Include(Control* c) {
    Insert(c);
    c->SetState(state_);
}

/** class PulldownMenu **/

PulldownMenu::PulldownMenu(Interactor* i) : Menu(i) { Init(); }
PulldownMenu::PulldownMenu(
    const char* name, Interactor* i
) : Menu(name, i) {
    Init();
}

PulldownMenu::PulldownMenu(
    const char* str
) : Menu(str, new Message(str, Center, 2)) {
    Init();
}

PulldownMenu::PulldownMenu(
    const char* name, const char* str
) : Menu(name, new Message(str, Center, 2)) {
    Init();
}

PulldownMenu::~PulldownMenu() { }

void PulldownMenu::Init() {
    SetClassName("PulldownMenu");
    SetAlign(BottomLeft);
}

/** class PullrightMenu **/

PullrightMenu::PullrightMenu(Interactor* i) : Menu(i) { Init(); }
PullrightMenu::PullrightMenu(
    const char* name, Interactor* i
) : Menu(name, i) {
    Init();
}

PullrightMenu::PullrightMenu(
    const char* str
) : Menu(str, new Message(str, Left, 2)) {
    Init();
}

PullrightMenu::PullrightMenu(
    const char* name, const char* str
) : Menu(name, new Message(str, Left, 2)) {
    Init();
}

PullrightMenu::~PullrightMenu() { }

void PullrightMenu::Init() {
    SetClassName("PullrightMenu");
    SetAlign(TopRight);
}

/** class PopupMenu **/

PopupMenu::PopupMenu() : Menu((Interactor*)nil) { Init(); }
PopupMenu::PopupMenu(const char* name) : Menu(name, (Interactor*)nil) {
    Init();
}

PopupMenu::~PopupMenu() { }

void PopupMenu::Init() {
    SetClassName("PopupMenu");
    SetState(new ControlState);
    SetAlign(Center);
}
