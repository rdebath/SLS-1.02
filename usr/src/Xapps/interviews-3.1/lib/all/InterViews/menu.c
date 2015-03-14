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
 * Menu -- visible list of Actions
 */

#include <IV-look/menu.h>
#include <IV-look/telltale.h>
#include <InterViews/action.h>
#include <InterViews/cursor.h>
#include <InterViews/event.h>
#include <InterViews/hit.h>
#include <InterViews/observe.h>
#include <InterViews/patch.h>
#include <InterViews/window.h>
#include <OS/list.h>
#include <X11/cursorfont.h>

declarePtrList(MenuItemList,MenuItem)
implementPtrList(MenuItemList,MenuItem)

class MenuImpl {
private:
    friend class Menu;
    friend class MenuItem;

    MenuItemList itemlist_;
    GlyphIndex item_;
    float x1_, y1_, x2_, y2_;
    boolean saved_cursor_;
    Cursor* cursor_;
    boolean grabbed_;

    static Cursor* menu_cursor_;

    Menu* selected_menu() const;
    void save_cursor(Canvas*);
    void restore_cursor(Canvas*);
    void grab(Menu*, const Event&);
    void ungrab(Menu*, const Event&);

    static Cursor* menu_cursor();
};

Cursor* MenuImpl::menu_cursor_;

MenuItem::MenuItem(Glyph* g, TelltaleState* t) {
    init(g, t);
    action_ = nil;
    menu_ = nil;
    window_ = nil;
}

MenuItem::MenuItem(Glyph* g, TelltaleState* t, Action* a) {
    init(g, t);
    Resource::ref(a);
    action_ = a;
    menu_ = nil;
    window_ = nil;
}

MenuItem::MenuItem(Glyph* g, TelltaleState* t, Menu* m, Window* w) {
    init(g, t);
    action_ = nil;
    Resource::ref(m);
    menu_ = m;
    if (w == nil) {
	w = new PopupWindow(menu_);
    }
    w->cursor(MenuImpl::menu_cursor());
    window_ = w;
}

MenuItem::~MenuItem() {
    state_->detach(this);
    Resource::unref(state_);
    Resource::unref(action_);
    Resource::unref(menu_);
    Resource::unref(patch_);
    delete window_;
}

void MenuItem::init(Glyph* g, TelltaleState* t) {
    patch_ = new Patch(g);
    Resource::ref(patch_);
    state_ = t;
    Resource::ref(state_);
    state_->attach(this);
}

Glyph* MenuItem::body() const { return patch_; }

void MenuItem::action(Action* a) {
    Resource::ref(a);
    Resource::unref(action_);
    action_ = a;
}

void MenuItem::menu(Menu* m, Window* w) {
    Resource::ref(m);
    Resource::unref(menu_);
    menu_ = m;
    if (w == nil) {
	w = new PopupWindow(menu_);
    }
    w->cursor(MenuImpl::menu_cursor());
    window_ = w;
}

void MenuItem::update(Observable*) {
    patch_->redraw();
}

Menu::Menu(
    Glyph* g, Style* s, float x1, float y1, float x2, float y2
) : InputHandler(g, s) {
    impl_ = new MenuImpl;
    MenuImpl& i = *impl_;
    i.item_ = -1;
    i.x1_ = x1;
    i.y1_ = y1;
    i.x2_ = x2;
    i.y2_ = y2;
    i.saved_cursor_ = false;
    i.grabbed_ = false;
}

Menu::~Menu() {
    for (ListItr(MenuItemList) i(impl_->itemlist_); i.more(); i.next()) {
	MenuItem* m = i.cur();
	delete m;
    }
    delete impl_;
}

void Menu::append_item(MenuItem* item) {
    MenuImpl& i = *impl_;
    i.itemlist_.append(item);
    append(item->body());
}

void Menu::prepend_item(MenuItem* item) {
    MenuImpl& i = *impl_;
    i.itemlist_.prepend(item);
    prepend(item->body());
}

void Menu::insert_item(GlyphIndex index, MenuItem* item) {
    MenuImpl& i = *impl_;
    i.itemlist_.insert(index, item);
    insert(index, item->body());
}

void Menu::remove_item(GlyphIndex index) {
    MenuImpl& i = *impl_;
    i.itemlist_.remove(index);
    remove(index);
}

void Menu::replace_item(GlyphIndex index, MenuItem* item) {
    MenuImpl& i = *impl_;
    if (index >= 0 && index < i.itemlist_.count()) {
	MenuItem* mi = i.itemlist_.item(index);
	delete mi;
	i.itemlist_.remove(index);
	i.itemlist_.insert(index, item);
	replace(index, item->body());
    }
}

GlyphIndex Menu::item_count() const {
    MenuImpl& i = *impl_;
    return i.itemlist_.count();
}

MenuItem* Menu::item(GlyphIndex index) const {
    MenuImpl& i = *impl_;
    if (index < 0 || index >= i.itemlist_.count()) {
	return nil;
    }
    return i.itemlist_.item(index);
}

void Menu::select(GlyphIndex index) {
    MenuImpl& i = *impl_;
    if (index != i.item_ && index >= 0 && index < i.itemlist_.count()) {
	close();
        i.item_ = index;
	open();
    }
}

void Menu::open() {
    MenuImpl& i = *impl_;
    GlyphIndex index = i.item_;
    if (index >= 0) {
	MenuItem* mi = item(index);
	TelltaleState* t = mi->state();
	if (t != nil && t->test(TelltaleState::is_enabled)) {
	    t->set(TelltaleState::is_active, true);
	    if (mi->menu() != nil) {
		mi->menu()->unselect();
		Action* a = mi->action();
		if (a != nil) {
		    a->execute();
		}
	    }
	    if (mi->window() != nil) {
		const Window& rel = *mi->patch_->canvas()->window();
		const Allocation& a = mi->patch_->allocation();
		Window& w = *mi->window();
		w.display(rel.display());
		w.place(
		    rel.left() + (1 - i.x1_) * a.left() + i.x1_ * a.right(),
		    rel.bottom() + (1 - i.y1_) * a.bottom() + i.y1_ * a.top()
		);
		w.align(i.x2_, i.y2_);
		w.map();
	    }
	}
    }
}

void Menu::close() {
    MenuImpl& i = *impl_;
    GlyphIndex index = i.item_;
    if (index >= 0) {
	MenuItem* mi = item(index);
	TelltaleState* t = mi->state();
	if (t != nil && t->test(TelltaleState::is_enabled)) {
	    t->set(TelltaleState::is_active, false);
	}
	if (mi->menu() != nil) {
	    mi->menu_->unselect();
	}
	Window* w = mi->window();
	if (w != nil) {
	    w->unmap();
	}
    }
}

GlyphIndex Menu::selected() const {
    return impl_->item_;
}

void Menu::unselect() {
    MenuImpl& i = *impl_;
    if (i.item_ != -1) {
	close();
        i.item_ = -1;
    }
}

void Menu::press(const Event& e) {
    Canvas* c = canvas();
    if (c != nil) {
	impl_->save_cursor(c);
	drag(e);
    }
}

void Menu::drag(const Event& e) {
    Canvas* c = canvas();
    if (c == nil) {
	unselect();
	return;
    }
    Window* w = c->window();
    Hit hit(e.pointer_root_x() - w->left(), e.pointer_root_y() - w->bottom());
    pick(c, allocation(), 0, hit);
    if (hit.any()) {
	GlyphIndex index = hit.index(0);
	Menu* submenu = item(index)->menu();
	if (submenu != nil) {
	    submenu->unselect();
	}
	select(index);
    } else {
	GlyphIndex index = selected();
	if (index >= 0) {
	    Menu* submenu = item(index)->menu();
	    if (submenu != nil) {
		submenu->drag(e);
		return;
	    }
	}
	unselect();
    }
}

void Menu::release(const Event& e) {
    Menu* m = this;
    GlyphIndex index;
    for (;;) {
	index = m->selected();
	if (index < 0) {
	    break;
	}
	Menu* submenu = m->item(index)->menu();
	if (submenu == nil) {
	    break;
	}
	m = submenu;
    }
    if (index >= 0) {
	GlyphIndex top_index = selected();
	TelltaleState* top_t = item(top_index)->state();
	top_t->set(TelltaleState::is_running, true);
	impl_->ungrab(this, e);
	Canvas* c = canvas();
	if (c != nil) {
	    impl_->restore_cursor(c);
	}
	unselect();
	MenuItem* mi = m->item(index);
	TelltaleState* t = mi->state();
	Action* a = mi->action();
	if (t != nil && t->test(TelltaleState::is_enabled)) {
	    boolean act = !t->test(TelltaleState::is_chosen);
	    if (t->test(TelltaleState::is_toggle)) {
		t->set(TelltaleState::is_chosen, act);
		act = true;
	    } else if (t->test(TelltaleState::is_choosable)) {
		t->set(TelltaleState::is_chosen, true);
	    }
	    if (act && a != nil) {
		a->execute();
	    }
	}
	top_t->set(TelltaleState::is_running, false);
    } else {
	/*
	 * If we hit an item with a submenu, then we leave
	 * the submenu open (with item 0 selected), grab, and return.
	 */
	Canvas* c;
	Menu* submenu;
	for (m = this, c = canvas(); c != nil; m = submenu, c = m->canvas()) {
	    Window* w = c->window();
	    Hit hit(
		e.pointer_root_x() - w->left(),
		e.pointer_root_y() - w->bottom()
	    );
	    m->pick(c, m->allocation(), 0, hit);
	    if (hit.any()) {
		m = item(hit.index(0))->menu();
		if (m != nil) {
		    m->select(0);
		    impl_->grab(this, e);
		    return;
		}
		break;
	    }
	    submenu = m->impl_->selected_menu();
	    if (submenu == nil) {
		break;
	    }
	}
	impl_->ungrab(this, e);
	c = canvas();
	if (c != nil) {
	    impl_->restore_cursor(c);
	}
	unselect();
    }
}

/* class MenuImpl */

Menu* MenuImpl::selected_menu() const {
    Menu* m = nil;
    if (item_ >= 0) {
	m = itemlist_.item(item_)->menu();
    }
    return m;
}

void MenuImpl::save_cursor(Canvas* c) {
    if (!saved_cursor_) {
	Window* w = c->window();
	cursor_ = w->cursor();
	saved_cursor_ = true;
	w->cursor(menu_cursor());
    }
}

void MenuImpl::restore_cursor(Canvas* c) {
    if (saved_cursor_) {
	c->window()->cursor(cursor_);
	saved_cursor_ = false;
    }
}

void MenuImpl::grab(Menu* m, const Event& e) {
    if (!grabbed_) {
	e.grab(m->handler());
	grabbed_ = true;
    }
}

void MenuImpl::ungrab(Menu* m, const Event& e) {
    if (grabbed_) {
	e.ungrab(m->handler());
	grabbed_ = false;
    }
}

Cursor* MenuImpl::menu_cursor() {
    if (menu_cursor_ == nil) {
	menu_cursor_ = new Cursor(XC_arrow);
    }
    return menu_cursor_;
}
