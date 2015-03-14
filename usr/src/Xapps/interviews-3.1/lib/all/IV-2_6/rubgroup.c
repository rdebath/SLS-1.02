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
 * RubberGroup implementation.
 */

#include <IV-2_6/InterViews/rubgroup.h>

class RubberList {
private:
    RubberList* next;
    RubberList* prev;
public:
    Rubberband* rub;

    RubberList(Rubberband* =nil);
    ~RubberList();

    void Append(Rubberband*);
    void Prepend(Rubberband*);
    void Delete(RubberList*);
    RubberList* Find(Rubberband*);
    RubberList* Next() { return next; }
    RubberList* Prev() { return prev; }
    RubberList* End() { return this; }
    boolean IsEmpty() { return next == this; }
};

RubberList::RubberList(Rubberband* r) {
    rub = r;
    next = this;
    prev = this;
}

RubberList::~RubberList() {
    RubberList* doomed;

    while (next != this) {
	doomed = next;
	next = doomed->next;
	doomed->next = doomed;
	delete doomed;
    }
    if (rub != nil) {
	delete rub;
    }
}

void RubberList::Append(Rubberband* b) {
    RubberList* cur = new RubberList(b);

    cur->next = this;
    cur->prev = prev;
    prev->next = cur;
    prev = cur;
}

void RubberList::Prepend(Rubberband* b) {
    RubberList* cur = new RubberList(b);

    cur->next = next;
    cur->prev = this;
    next->prev = cur;
    next = cur;
}

void RubberList::Delete(RubberList* elem) {
    elem->prev->next = elem->next;
    elem->next->prev = elem->prev;
    elem->next = elem;
    delete elem;
}

RubberList* RubberList::Find(Rubberband* target) {
    RubberList* r;
    
    for (r = Next(); r != this; r = r->Next()) {
	if (r->rub == target) {
	    return r;
	}
    }
    return nil;
}

/*****************************************************************************/

RubberGroup::RubberGroup(Painter* p, Canvas* c) : Rubberband(p, c, 0, 0) {
    rlist = cur = new RubberList;
}

void RubberGroup::Draw() {
    for (RubberList* r = rlist->Next(); r != rlist->End(); r = r->Next()) {
	r->rub->Draw();
    }
}

void RubberGroup::Erase() {
    for (RubberList* r = rlist->Next(); r != rlist->End(); r = r->Next()) {
	r->rub->Erase();
    }
}

void RubberGroup::Track(IntCoord x, IntCoord y) {
    for (RubberList* r = rlist->Next(); r != rlist->End(); r = r->Next()) {
	r->rub->Track(x, y);
    }
}

RubberGroup::~RubberGroup() { delete rlist; }

void RubberGroup::SetPainter(Painter* p) {
    Rubberband::SetPainter(p);

    for (RubberList* r = rlist->Next(); r != rlist->End(); r = r->Next()) {
	r->rub->SetPainter(p);
    }
}

void RubberGroup::SetCanvas(Canvas* c) {
    Rubberband::SetCanvas(c);

    for (RubberList* r = rlist->Next(); r != rlist->End(); r = r->Next()) {
	r->rub->SetCanvas(c);
    }
}

void RubberGroup::Append(
    Rubberband* r1, Rubberband* r2, Rubberband* r3, Rubberband* r4
) {
    rlist->Append(r1);
    if (r2 != nil) {
	rlist->Append(r2);

        if (r3 != nil) {
            rlist->Append(r3);

            if (r4 != nil) {
                rlist->Append(r4);
            }
        }
    }
}

void RubberGroup::Remove(Rubberband* target) {
    RubberList* r = rlist->Find(target);
    
    if (r != nil) {
	cur = cur->Next();
	rlist->Delete(r);
    }
}

void RubberGroup::RemoveCur() {
    RubberList* doomed = cur;
    
    if (!AtEnd()) {
	cur = cur->Next();
	rlist->Delete(doomed);
    }
}

void RubberGroup::SetCurrent(Rubberband* target) {
    RubberList* r = rlist->Find(target);
    
    if (r != nil) {
	cur = r;
    }
}

Rubberband* RubberGroup::GetCurrent() {
    return cur->rub;
}

Rubberband* RubberGroup::First() {
    cur = rlist->Next(); 
    return cur->rub;
}
Rubberband* RubberGroup::Last() {
    cur = rlist->Prev(); 
    return cur->rub;
}

Rubberband* RubberGroup::Next() {
    cur = cur->Next(); 
    return cur->rub;
}

Rubberband* RubberGroup::Prev() {
    cur = cur->Prev(); 
    return cur->rub;
}

boolean RubberGroup::AtEnd() { return cur == rlist; }
boolean RubberGroup::IsEmpty() { return rlist->IsEmpty(); }
