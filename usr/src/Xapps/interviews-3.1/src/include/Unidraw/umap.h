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
 * UMap - maintains a void* <-> void* mapping (linear search by default).
 */

#ifndef unidraw_umap_h
#define unidraw_umap_h

#include <Unidraw/uarray.h>

class UMapElem {
public:
    virtual ~UMapElem();

    virtual void* id();
    virtual void* tag();
protected:
    UMapElem();
};

class UMap {
public:
    virtual ~UMap();

    int Count();
    void Clear();
protected:
    UMap();

    void Register(UMapElem*);
    void Unregister(UMapElem*);

    int Index(UMapElem*);
    UMapElem* Elem(int index);

    virtual UMapElem* FindId(void*);
    virtual UMapElem* FindTag(void*);
protected:
    UArray _elems;
};

inline int UMap::Index (UMapElem* elem) { return _elems.Index(elem); }
inline UMapElem* UMap::Elem (int index) { return (UMapElem*) _elems[index]; }
inline int UMap::Count () { return _elems.Count(); }

#endif
