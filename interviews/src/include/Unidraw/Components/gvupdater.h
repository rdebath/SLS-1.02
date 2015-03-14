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
 * GVUpdater - class for reconciling a GraphicView's state and structure 
 * with its subject's.
 */

#ifndef unidraw_components_gvupdater_h
#define unidraw_components_gvupdater_h

#include <Unidraw/globals.h>

class GraphicView;
class GVU_HashTable;
class Iterator;

class GVUpdater {
public:
    GVUpdater(GraphicView*);
    virtual void Update();
protected:
    void UpdateStructure();
    void UpdateState();

    void AddDamage(Graphic*);
    void IncurDamage(Graphic*);
    void Unselect(GraphicView*);

    void Add(GraphicView*);
    void Append(GraphicView*);
    void InsertBefore(Iterator, GraphicView*);
    void Remove(Iterator&);
    void DeleteView(Iterator&);

    virtual ClassId ViewCategory();
protected:
    GraphicView* _gv;
private:
    void RegisterSubjects(GVU_HashTable*);
    void InitViews(GVU_HashTable*);
    void RearrangeViews(GVU_HashTable*);
    void DamageViews(GVU_HashTable*);
};

#endif
