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
 * GVUpdater implementation.
 */

#include <Unidraw/classes.h>
#include <Unidraw/iterator.h>
#include <Unidraw/uhash.h>
#include <Unidraw/ulist.h>
#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Components/gvupdater.h>
#include <Unidraw/Graphic/graphic.h>

#include <InterViews/transformer.h>

#include <IV-2_6/_enter.h>

/*****************************************************************************/

static const int SLOTS = 1000;

/*****************************************************************************/

class GVU_HashElem : public UHashElem {
public:
    GVU_HashElem();
    boolean Undefined();
    void Init(Iterator iview, int);
public:
    Iterator _iview;
    int _n;
};

GVU_HashElem::GVU_HashElem () { _iview.SetValue(nil); _n = 0; }
boolean GVU_HashElem::Undefined () { return _iview.GetValue() == nil; }
void GVU_HashElem::Init (Iterator iview, int n) { _iview = iview; _n = n; }

/*****************************************************************************/

class GVU_HashTable : public UHashTable {
public:
    GVU_HashTable();

    GVU_HashElem* Find(GraphicComp*);
};

GVU_HashTable::GVU_HashTable () : UHashTable(SLOTS) { }

GVU_HashElem* GVU_HashTable::Find (GraphicComp* gc) {
    return (GVU_HashElem*) UHashTable::Find(gc);
}

/*****************************************************************************/

GVUpdater::GVUpdater (GraphicView* gv) { _gv = gv; }

void GVUpdater::Update () {
    UpdateStructure();
    UpdateState();
}

static boolean Different (Graphic* g1, Graphic* g2) {
    boolean different = true;

    if (
        g1->GetFgColor() == g2->GetFgColor() &&
        g1->GetBgColor() == g2->GetBgColor() &&
        g1->BgFilled() == g2->BgFilled() &&
        g1->GetPattern() == g2->GetPattern() &&
        g1->GetBrush() == g2->GetBrush() &&
        g1->GetFont() == g2->GetFont()
    ) {
        Transformer identity;
        Transformer* t1 = g1->GetTransformer();
        Transformer* t2 = g2->GetTransformer();

        if (t1 == t2) {
            different = false;
        } else if (t1 == nil) {
            different = *t2 != identity;
        } else if (t2 == nil) {
            different = *t1 != identity;
        } else {
            different = *t1 != *t2;
        }
    }
    return different;
}

void GVUpdater::UpdateState () {
    Graphic* g1 = _gv->GetGraphic();
    Graphic* g2 = _gv->GetGraphicComp()->GetGraphic();

    if (Different(g1, g2)) {
        IncurDamage(g1);
        *g1 = *g2;
        IncurDamage(g1);
    }
}

void GVUpdater::UpdateStructure () {
    GVU_HashTable table;
    RegisterSubjects(&table);
    InitViews(&table);
    RearrangeViews(&table);
    DamageViews(&table);
}

ClassId GVUpdater::ViewCategory () { return COMPONENT_VIEW; }
void GVUpdater::AddDamage (Graphic* g) { _gv->AddDamage(g); }
void GVUpdater::IncurDamage (Graphic* g) { _gv->IncurDamage(g); }
void GVUpdater::Unselect (GraphicView* view) { _gv->Unselect(view); }
void GVUpdater::Append (GraphicView* view) { _gv->Append(view); }
void GVUpdater::Remove (Iterator& i) { _gv->Remove(i); }
void GVUpdater::DeleteView (Iterator& i) { _gv->DeleteView(i); }

void GVUpdater::InsertBefore (Iterator i, GraphicView* view) {
    _gv->InsertBefore(i, view);
}

void GVUpdater::RegisterSubjects (GVU_HashTable* table) {
    GraphicComp* gc = _gv->GetGraphicComp();
    Iterator i;

    for (gc->First(i); !gc->Done(i); gc->Next(i)) {
        GraphicComp* subj = gc->GetComp(i);
        table->Register(subj, new GVU_HashElem);
    }
}

static boolean IsAGroup (GraphicView* view, GraphicComp* newParent) {
    boolean result = false;
    Iterator i;
    view->First(i);

    if (!view->Done(i)) {
        GraphicView* kidview = view->GetView(i);
        GraphicComp* kidsubj = kidview->GetGraphicComp();

        result = (kidsubj != nil) && (kidsubj->GetParent() == newParent);
    }
    return result;
}

void GVUpdater::InitViews (GVU_HashTable* table) {
    GraphicComp* gc = _gv->GetGraphicComp();
    int count = 0;
    Iterator i;
    
    for (_gv->First(i); !_gv->Done(i);) {       // ungroup subjectless view
        GraphicView* view = _gv->GetView(i);    // delete unknown views and
        GraphicComp* subj = view->GetGraphicComp(); // register all others
        GVU_HashElem* info = table->Find(subj);

        if (info == nil) {
            Unselect(view);

            if (IsAGroup(view, gc)) {
                Iterator j, k = i;
                view->First(j);
                _gv->Next(k);

                while (!view->Done(j)) {
                    GraphicView* subview = view->GetView(j);
                    view->Remove(j);
                    subview->Update();
                    InsertBefore(k, subview);
                }
                Remove(i);
                delete view;
            } else {
                DeleteView(i);
            }

        } else {
            info->Init(i, ++count);
            _gv->Next(i);
        }
    }
    for (gc->First(i); !gc->Done(i); gc->Next(i)) { // create missing views
        GraphicComp* subj = gc->GetComp(i);
        GVU_HashElem* info = table->Find(subj);

        if (info->Undefined()) {
            GraphicView* view = (GraphicView*) subj->Create(ViewCategory());
            subj->Attach(view);
            Append(view);
            view->Update();
            IncurDamage(view->GetGraphic());

            Iterator j;
            _gv->Last(j);
            info->Init(j, ++count);
        }
    }
}

void GVUpdater::RearrangeViews (GVU_HashTable* table) {
    GraphicComp* gc = _gv->GetGraphicComp();
    Iterator i, j, k;
    
    for (gc->First(i), _gv->First(j); !gc->Done(i); gc->Next(i)) {
        GraphicComp* comp = gc->GetComp(i);
        GraphicView* view = _gv->GetView(j);

        if (comp != view->GetGraphicComp()) {
            GVU_HashElem* info = table->Find(comp);
            view = _gv->GetView(info->_iview);
            Remove(info->_iview);
            InsertBefore(j, view);

            k = j;
            _gv->Prev(k);
            info->_iview = k;

        } else {
            _gv->Next(j);
        }
    }
}

void GVUpdater::DamageViews (GVU_HashTable* table) {
    GraphicComp* gc = _gv->GetGraphicComp();
    UList fdamage, bdamage;
    Iterator f, b;
    int fcount = 0, bcount = 0;
    int fcur = 0, bcur;
    GVU_HashElem* info;

    gc->First(f);
    gc->Last(b);
    if (!gc->Done(b)) {
        info = table->Find(gc->GetComp(b));
        bcur = info->_n;
    }
    for (gc->First(f); !gc->Done(f); gc->Next(f), gc->Prev(b)) {
        GraphicComp* fcomp = gc->GetComp(f);
        GraphicComp* bcomp = gc->GetComp(b);
        GVU_HashElem* finfo = table->Find(fcomp);
        GVU_HashElem* binfo = table->Find(bcomp);

        if (finfo->_n < fcur) {
            fdamage.Append(new UList(finfo));
            ++fcount;
        } else {
            fcur = finfo->_n;
        }

        if (binfo->_n > bcur) {
            bdamage.Append(new UList(binfo));
            ++bcount;
        } else {
            bcur = binfo->_n;
        }
    }
    UList* damage = (fcount < bcount) ? &fdamage : &bdamage;

    for (UList* u = damage->First(); u != damage->End(); u = u->Next()) {
        info = (GVU_HashElem*) (*u)();
        GraphicView* view = _gv->GetView(info->_iview);
        IncurDamage(view->GetGraphic());
    }
}
