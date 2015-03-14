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
 * UHash implementation.
 */

#include <Unidraw/iterator.h>
#include <Unidraw/uhash.h>
#include <Unidraw/ulist.h>

#include <OS/memory.h>

/*****************************************************************************/

UHashElem::UHashElem (void* key) { _key = key; }
UHashElem::~UHashElem () { }

/*****************************************************************************/

UHashElem* UHashTable::CreateElem () { return nil; }
UHashElem* UHashTable::Elem (UList* r) { return (UHashElem*) (*r)(); }
int UHashTable::Hash (void* c) { return (unsigned) c % _nslots; }

UHashTable::UHashTable (int nslots) {
    _nslots = nslots;
    _slot = new UList*[_nslots];
    Memory::zero(_slot, sizeof(void*) * _nslots);
}

UHashTable::~UHashTable () {
    for (int i = 0; i < _nslots; ++i) {
        if (_slot[i] != nil) {
            DeleteSlot(_slot[i]);
        }
    }
    delete _slot;
}

void UHashTable::Register (void* key, UHashElem* elem) {
    int n = Hash(key);

    if (_slot[n] == nil) {
        _slot[n] = new UList;
    }
    if (elem == nil) {
        elem = CreateElem();
    }
    elem->SetKey(key);
    _slot[n]->Prepend(new UList(elem));
}

void UHashTable::Unregister (void* key) {
    int n = Hash(key);
    UList* slot = _slot[n];

    if (slot != nil) {
        for (UList* u = slot->First(); u != slot->End(); u = u->Next()) {
            UHashElem* elem = Elem(u);

            if (Equal(elem->GetKey(), key)) {
                slot->Remove(u);
                delete elem;
                delete u;

		if (_slot[n]->IsEmpty()) {
		    delete _slot[n];
		    _slot[n] = nil;
		}
                break;
            }
        }
    }
}

UList* UHashTable::UElem (Iterator i) { return (UList*) i.GetValue(); }
UHashElem* UHashTable::GetElem (Iterator i) { return Elem(UElem(i)); }

void UHashTable::First (Iterator& i) {
    for (int j = 0; j < _nslots; ++j) {
        if (_slot[j] != nil) {
            i.SetValue(_slot[j]->First());
            return;
        }
    }
    i.SetValue(nil);
}

void UHashTable::Next (Iterator& i) {
    UHashElem* elem = GetElem(i);

    if (elem != nil) {
        int n = Hash(elem->GetKey());
        UList* u = UElem(i);
        u = u->Next();

        if (u == _slot[n]->End()) {
            for (int j = n+1; j < _nslots; ++j) {
                if (_slot[j] != nil) {
                    u = _slot[j]->First();
                    break;
                }
            }
        }
        i.SetValue(u);
    }
}

boolean UHashTable::Done (Iterator i) {
    for (int j = _nslots - 1; j >= 0; --j) {
        if (_slot[j] != nil) {
            return UElem(i) == _slot[j]->End();
        }
    }
    return true;
}

boolean UHashTable::Equal (void* key1, void* key2) { return key1 == key2; }

UHashElem* UHashTable::Find (void* key) {
    int n = Hash(key);
    UList* slot = _slot[n];

    if (slot != nil) {
        for (UList* u = slot->First(); u != slot->End(); u = u->Next()) {
            UHashElem* elem = Elem(u);

            if (Equal(elem->GetKey(), key)) {
                return elem;
            }
        }
    }
    return nil;
}

void UHashTable::DeleteSlot (UList* slot) {
    while (!slot->IsEmpty()) {
        UList* u = slot->First();
        slot->Remove(u);
        UHashElem* elem = Elem(u);
        delete elem;
        delete u;
    }
    delete slot;
}

