/*
 * Copyright (c) 1990, 1991 Stanford University
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
 * Unidraw class implementation.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/editor.h>
#include <Unidraw/globals.h>
#include <Unidraw/keymap.h>
#include <Unidraw/selection.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/viewer.h>
#include <Unidraw/Components/component.h>
#include <Unidraw/Components/grview.h>

#include <InterViews/event.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/target.h>
#include <InterViews/window.h>

#include <OS/list.h>

declarePtrList(EditorImpl,TransientWindow)
implementPtrList(EditorImpl,TransientWindow)

/*****************************************************************************/

static void DetachComponentViews (Editor* ed) {
    int i = 0;
    Viewer* viewer;
    Component* comp = ed->GetComponent();

    if (comp != nil) {
        while ((viewer = ed->GetViewer(i++)) != nil) {
            GraphicView* gv = viewer->GetGraphicView();
            comp->Detach(gv);
        }
    }
}

/*****************************************************************************/

Editor::Editor() : InputHandler(nil, new Style(Session::instance()->style())) {
    _window = nil;
    _impl = new EditorImpl;
}

Editor::~Editor () {
    for (ListItr(EditorImpl) i(*_impl); i.more(); i.next()) {
	TransientWindow* t = i.cur();
	delete t;
    }
    delete _impl;
}

Component* Editor::GetComponent () { return nil; }
Viewer* Editor::GetViewer (int) { return nil; }
KeyMap* Editor::GetKeyMap () { return nil; }
Tool* Editor::GetCurTool () { return nil; }
Selection* Editor::GetSelection () { return nil; }
StateVar* Editor::GetState (const char*) { return nil; }

void Editor::SetComponent (Component*) { }
void Editor::SetViewer (Viewer*, int) { }
void Editor::SetCurTool (Tool*) { }
void Editor::SetKeyMap (KeyMap*) { }
void Editor::SetSelection (Selection*) { }

void Editor::Open () { }

void Editor::Close () {
    Selection* s = GetSelection();

    if (s != nil) {
        s->Clear();
    }
    DetachComponentViews(this);
}

void Editor::Insert(Interactor* i) {
    body(new Target(i, TargetPrimitiveHit));
}

void Editor::SetClassName(const char* s) {
    style()->alias(s);
}

void Editor::SetInstance(const char* s) {
    style()->name(s);
}

void Editor::keystroke(const Event& e) {
    char buf[100];
    int n = e.mapkey(buf, sizeof(buf) - 1);
    if (n > 0) {
	buf[n] = '\0';
	GetKeyMap()->Execute(buf);
    }
}

void Editor::InsertDialog (Glyph* g) { 
    ManagedWindow* w = GetWindow();
    if (w != nil) w->deiconify();
    TransientWindow* dialog = nil;
    for (ListItr(EditorImpl) i(*_impl); i.more(); i.next()) {
	TransientWindow* t = i.cur();
	if (t->glyph() == g) {
	    dialog = t;
	    break;
	}
    }
    if (dialog == nil) {
	dialog = new TransientWindow(g);
	_impl->prepend(dialog);
    }
    dialog->transient_for(w);
    dialog->place(
	w->left() + w->width() * 0.5, w->bottom() + w->height() * 0.5
    );
    dialog->align(0.5, 0.5);
    dialog->map();
}

void Editor::RemoveDialog (Glyph* g) {
    for (ListUpdater(EditorImpl) i(*_impl); i.more(); i.next()) {
	TransientWindow* t = i.cur();
	if (t->glyph() == g) {
	    t->unmap();
	    i.remove_cur();
	    delete t;
	    break;
	}
    }
}

void Editor::Update () { 
    Viewer* v;

    for (int i = 0; (v = GetViewer(i)) != nil; ++i) {
        v->Update();
    }
}

boolean Editor::DependsOn (Component* parent) {
    Component* child = GetComponent();

    while (child != nil) {
        if (parent == child) {
            return true;
        }
        child = child->GetParent();
    }
    return false;
}
