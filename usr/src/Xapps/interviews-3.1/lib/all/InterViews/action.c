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

#include <InterViews/action.h>
#include <OS/list.h>

Action::Action() { }
Action::~Action() { }

declarePtrList(MacroActionList,Action)
implementPtrList(MacroActionList,Action)

Macro::Macro(Action* a0, Action* a1, Action* a2, Action* a3) : Action() {
    list_ = new MacroActionList;
    if (a0 != nil) {
	list_->append(a0);
    }
    if (a1 != nil) {
	list_->append(a1);
    }
    if (a2 != nil) {
	list_->append(a2);
    }
    if (a3 != nil) {
	list_->append(a3);
    }
}

Macro::~Macro() {
    for (ListItr(MacroActionList) i(*list_); i.more(); i.next()) {
	Action* a = i.cur();
	Resource::unref(a);
    }
    delete list_;
}

void Macro::prepend(Action* a) {
    Resource::ref(a);
    list_->prepend(a);
}

void Macro::append(Action* a) {
    Resource::ref(a);
    list_->append(a);
}

void Macro::insert(MacroIndex i, Action* a) {
    Resource::ref(a);
    list_->insert(i, a);
}

void Macro::remove(MacroIndex i) {
    if (i >= 0 && i < list_->count()) {
	Action* a = list_->item(i);
	Resource::unref(a);
	list_->remove(i);
    }
}

MacroIndex Macro::count() const {
    return list_->count();
}

Action* Macro::action(MacroIndex i) const {
    Action* a;
    if (i >= 0 && i < list_->count()) {
	a = list_->item(i);
    } else {
	a = nil;
    }
    return a;
}

void Macro::execute() {
    for (ListItr(MacroActionList) i(*list_); i.more(); i.next()) {
	Action* a = i.cur();
	if (a != nil) {
	    a->execute();
	}
    }
}
