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
 * Base composite interactor.
 */

#include <InterViews/canvas.h>
#include <IV-2_6/InterViews/iwindow.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/scene.h>
#include <IV-2_6/InterViews/shape.h>
#include <IV-2_6/InterViews/world.h>

#include <IV-2_6/_enter.h>

Scene::Scene() {
    propagate = true;
}

Scene::~Scene() { }

void Scene::Insert(Interactor* component) {
    Interactor* i = Wrap(component);
    PrepareToInsert(i);
    IntCoord left, bottom;
    DoInsert(i, false, left, bottom);
}

void Scene::Insert(Interactor* component, IntCoord x, IntCoord y, Alignment a) {
    Interactor* i = Wrap(component);
    PrepareToInsert(i);
    IntCoord ax = x, ay = y;
    DoAlign(i, a, ax, ay);
    DoInsert(i, true, ax, ay);
}

void Scene::PrepareToInsert(Interactor* i) {
    if (parent != nil ||
	(canvas != nil && canvas->status() == Canvas::mapped)
    ) {
	i->Config(this);
    }
}

void Scene::DoAlign(Interactor* i, Alignment a, IntCoord& x, IntCoord& y) {
    switch (a) {
	case TopLeft:
	case CenterLeft:
	case BottomLeft:
	    /* nothing to do */
	    break;
	case TopCenter:
	case Center:
	case BottomCenter:
	    x -= i->shape->width/2;
	    break;
	case TopRight:
	case CenterRight:
	case BottomRight:
	    x -= i->shape->width;
	    break;
    }
    switch (a) {
	case BottomLeft:
	case BottomCenter:
	case BottomRight:
	    /* nothing to do */
	    break;
	case CenterLeft:
	case Center:
	case CenterRight:
	    y -= i->shape->height/2;
	    break;
	case TopLeft:
	case TopCenter:
	case TopRight:
	    y -= i->shape->height;
	    break;
    }
}

void Scene::Change(Interactor* i) {
    if (propagate) {
	DoChange(i);
	if (parent == nil) {
	    if (world != nil) {
		world->Change(this);
	    }
	} else {
	    parent->Change(this);
	}
    } else if (canvas != nil) {
	Resize();
    }
}

void Scene::Remove(Interactor* i) {
    DoRemove(i);
    i->parent = nil;
    if (i->canvas != nil) {
	Unmap(i);
	i->Orphan();
    }
}

void Scene::Orphan() {
    Interactor* children[100];
    Interactor** a;
    int n;

    GetComponents(children, sizeof(children) / sizeof(Interactor*), a, n);
    if (n > 0) {
	for (register int i = 0; i < n; i++) {
	    a[i]->Orphan();
	}
	if (a != children) {
	    delete a;
	}
    }
    Interactor::Orphan();
}

void Scene::Raise(Interactor* i) {
    DoRaise(i);
    Window* w = i->window;
    if (w != nil) {
	w->raise();
    }
}

void Scene::Lower(Interactor* i) {
    DoLower(i);
    Window* w = i->window;
    if (w != nil) {
	w->lower();
    }
}

void Scene::DoInsert(Interactor*, boolean, IntCoord&, IntCoord&) { }
void Scene::DoChange(Interactor*) { }
void Scene::DoRemove(Interactor*) { }
void Scene::DoRaise(Interactor*) { }
void Scene::DoLower(Interactor*) { }
void Scene::DoMove(Interactor*, IntCoord&, IntCoord&) { }

/*
 * Wrap is called to put any desired layer around an interactor
 * that is inserted into a scene.  The default is to simply
 * return the interactor as is.
 */

Interactor* Scene::Wrap(Interactor* i) {
    return i;
}

void Scene::Propagate(boolean b) {
    propagate = b;
}

/*
 * Highlight a scene by highlighting each components.
 */

void Scene::Highlight(boolean b) {
    Interactor* children[100];
    Interactor** a;
    int n;

    GetComponents(children, sizeof(children) / sizeof(Interactor*), a, n);
    if (n > 0) {
	for (register int index = 0; index < n; index++) {
	    a[index]->Highlight(b);
	}
	if (a != children) {
	    delete a;
	}
    }
}

/*
 * A common case is a scene with a single subcomponent.  This construct
 * occurs when one interactor is defined in terms of another, e.g.,
 * a menu is built out of a frame around a box.  The reason a MonoScene
 * is preferred over subclassing is that it simplies implementing the virtuals.
 * In the menu example, menus can handle events independently of frames.
 */

MonoScene::MonoScene() {
    interior_ = nil;
}

MonoScene::~MonoScene() {
    delete interior_;
}

void MonoScene::DoInsert(Interactor* i, boolean, IntCoord&, IntCoord&) {
    delete interior_;
    interior_ = i;
}

void MonoScene::DoChange(Interactor*) {
    if (output != nil) {
	Reconfig();
    }
}

void MonoScene::DoRemove(Interactor*) {
    interior_ = nil;
}

void MonoScene::Reconfig() {
    if (interior_ != nil) {
	*shape = *interior_->GetShape();
    }
}

void MonoScene::Resize() {
    if (output != nil && GetCanvasType() != CanvasInputOnly) {
	canvas->SetBackground(output->GetBgColor());
    }
    if (interior_ != nil) {
	Place(interior_, 0, 0, xmax, ymax);
    }
}

void MonoScene::Draw() {
    Scene::Draw();
    if (interior_ != nil) {
	interior_->Draw();
    }
}

void MonoScene::GetComponents(
    Interactor** c, int nc, Interactor**& a, int& n
) {
    if (interior_ == nil) {
	n = 0;
    } else if (nc > 0) {
	n = 1;
	a = c;
	a[0] = interior_;
    } else {
	n = 1;
	a = new Interactor*[1];
	a[0] = interior_;
    }
}
