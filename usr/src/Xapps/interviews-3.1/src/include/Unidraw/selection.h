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
 * Selection - manages a set of selected components.
 */

#ifndef unidraw_selection_h
#define unidraw_selection_h

#include <IV-2_6/InterViews/defs.h>
#include <Unidraw/enter-scope.h>

#include <IV-2_6/_enter.h>

class GraphicView;
class Iterator;
class Painter;
class UList;
class Viewer;

class Selection {
public:
    Selection(Selection* = nil);
    virtual ~Selection();

    void Show(Viewer* = nil);	/* inits and draws handles (in given viewer) */
    void Update(Viewer* = nil); /* draws newly-added handles */
    void Hide(Viewer* = nil);
    void Init(Viewer* = nil);   /* explicitly init handles */
    void Clear(Viewer* = nil);	/* removes & clears all views */

    void Append(GraphicView*);
    void Prepend(GraphicView*);
    void InsertAfter(Iterator, GraphicView*);
    void InsertBefore(Iterator, GraphicView*);
    void Remove(GraphicView*);
    void Remove(Iterator&);

    GraphicView* GetView(Iterator);
    void SetView(GraphicView*, Iterator&);

    void First(Iterator&);
    void Last(Iterator&);
    void Next(Iterator&);
    void Prev(Iterator&);
    boolean Done(Iterator);
    boolean IsEmpty();
    boolean Includes(GraphicView*);
    int Number();

    void Sort(GraphicView*); 
    void Exclusive(Selection*);          /* union minus intersection */
    void Merge(Selection*);              /* union of this's & Selection's */
                                         /* GraphicViews */
    void GetBox(Coord&, Coord&, Coord&, Coord&); /* box bounding selection */
protected:
    GraphicView* View(UList*);
    UList* Elem(Iterator);
protected:
    UList* _ulist;
    int _count;
};

#include <IV-2_6/_leave.h>

#endif
