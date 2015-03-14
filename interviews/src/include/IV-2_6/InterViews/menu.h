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
 * Common menus built on top of controls
 */

#ifndef ivlook2_6_menu_h
#define ivlook2_6_menu_h

#include <IV-2_6/InterViews/box.h>
#include <IV-2_6/InterViews/control.h>

#include <IV-2_6/_enter.h>

class MenuItem : public Control {
public:
    MenuItem(Interactor*);
    MenuItem(const char* name, Interactor*);
    MenuItem(const char* str, Alignment = Left);
    MenuItem(const char* name, const char* str, Alignment = Left);
    virtual ~MenuItem();
protected:
    virtual void Busy();
    virtual void Done();
private:
    void Init();
};

class Menu : public Control {
public:
    Menu(Interactor*);
    Menu(const char* name, Interactor* i);
    virtual ~Menu();

    void SetAlign(Alignment);
    Alignment GetAlign() { return align_; }

    void SetDepth(int);
    int GetDepth() { return depth_; }

    void SetBodyState(ControlState*);
    ControlState* GetBodyState() { return state_; }

    void SetScene(Scene*);
    Scene* GetScene() { return scene_; }
    virtual void Include(Control*);

    virtual void Popup(Event&);

    IntCoord InsertX() { return ins_x_; }
    IntCoord InsertY() { return ins_y_; }
protected:
    virtual void Leave();
    virtual void Open();
    virtual void Close();

    virtual void Reconfig();
    virtual void Setup();
    virtual void InsertBody(IntCoord, IntCoord);
private:
    Interactor* body_;
    Scene* scene_;
    ControlState* state_;
    Interactor* shadow_;
    unsigned int depth_ : 16;
    unsigned int align_ : 16;
    class World* world_;
    IntCoord ins_x_, ins_y_;
    IntCoord rel_x_, rel_y_;
private:
    void Init();
};

class MenuBar : public HBox {
public:
    MenuBar();
    MenuBar(const char* name);
    virtual ~MenuBar();

    virtual void Include(Control*);
protected:
    ControlState* state_;
private:
    void Init();
};

class PulldownMenu : public Menu {
public:
    PulldownMenu(Interactor* i);
    PulldownMenu(const char* name, Interactor* i);
    PulldownMenu(const char* str);
    PulldownMenu(const char* name, const char* str);
    virtual ~PulldownMenu();
private:
    void Init();
};

class PullrightMenu : public Menu {
public:
    PullrightMenu(Interactor* i);
    PullrightMenu(const char* name, Interactor* i);
    PullrightMenu(const char* str);
    PullrightMenu(const char* name, const char* str);
    virtual ~PullrightMenu();
private:
    void Init();
};

class PopupMenu : public Menu {
public:
    PopupMenu();
    PopupMenu(const char*);
    virtual ~PopupMenu();
private:
    void Init();
};

#include <IV-2_6/_leave.h>

#endif
