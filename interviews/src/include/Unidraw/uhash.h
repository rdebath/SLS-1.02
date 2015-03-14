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
 * UHash - hash table.
 */

#ifndef unidraw_uhash_h
#define unidraw_uhash_h

#include <InterViews/enter-scope.h>
#include <Unidraw/enter-scope.h>

#include <InterViews/_enter.h>

class Iterator;
class UList;

class UHashElem {
public:
    UHashElem(void* = nil);
    virtual ~UHashElem();

    void* GetKey();
    void SetKey(void*);
private:
    void* _key;
};

inline void* UHashElem::GetKey () { return _key; }
inline void UHashElem::SetKey (void* key) { _key = key; }

class UHashTable {
public:
    UHashTable(int nslots);
    virtual ~UHashTable();

    virtual void Register(void* key, UHashElem* = nil);
    virtual void Unregister(void* key);

    void First(Iterator&);
    void Next(Iterator&);
    boolean Done(Iterator);

    UHashElem* GetElem(Iterator);
    UHashElem* Find(void* key);
protected:
    virtual UHashElem* CreateElem();
    virtual int Hash(void*);
    virtual boolean Equal(void* key1, void* key2);
protected:
    int _nslots;
private:
    UList* UElem(Iterator);
    UHashElem* Elem(UList*);
    void DeleteSlot(UList*);
private:
    UList** _slot;
};

#include <InterViews/_leave.h>

#endif
