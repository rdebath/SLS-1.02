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
 * Menu -- visible list of actions
 */

#ifndef ivlook_menu_h
#define ivlook_menu_h

#include <InterViews/input.h>
#include <InterViews/observe.h>

#include <InterViews/_enter.h>

class Action;
class Menu;
class MenuImpl;
class Patch;
class TelltaleState;
class Window;

class MenuItem : public Observer {
public:
    MenuItem(Glyph*, TelltaleState*);
    MenuItem(Glyph*, TelltaleState*, Action*);
    MenuItem(Glyph*, TelltaleState*, Menu*, Window* = nil);
    virtual ~MenuItem();

    Glyph* body() const;
    TelltaleState* state() const;

    virtual void action(Action*);
    Action* action() const;
    virtual void menu(Menu*, Window* = nil);
    Menu* menu() const;
    Window* window() const;

    virtual void update(Observable*);
private:
    friend class Menu;
    friend class MenuImpl;

    Patch* patch_;
    TelltaleState* state_;
    Action* action_;
    Menu* menu_;
    Window* window_;

    void init(Glyph*, TelltaleState*);
};

inline TelltaleState* MenuItem::state() const { return state_; }
inline Action* MenuItem::action() const { return action_; }
inline Menu* MenuItem::menu() const { return menu_; }
inline Window* MenuItem::window() const { return window_; }

class Menu : public InputHandler {
public:
    Menu(Glyph*, Style*, float x1, float y1, float x2, float y2);
    virtual ~Menu();

    virtual void append_item(MenuItem*);
    virtual void prepend_item(MenuItem*);
    virtual void insert_item(GlyphIndex, MenuItem*);
    virtual void remove_item(GlyphIndex);
    virtual void replace_item(GlyphIndex, MenuItem*);

    virtual GlyphIndex item_count() const;
    virtual MenuItem* item(GlyphIndex) const;

    virtual void select(GlyphIndex);
    virtual GlyphIndex selected() const;
    virtual void unselect();

    virtual void press(const Event&);
    virtual void drag(const Event&);
    virtual void release(const Event&);

    virtual void open();
    virtual void close();
private:
    MenuImpl* impl_;
};

#include <InterViews/_leave.h>

#endif
