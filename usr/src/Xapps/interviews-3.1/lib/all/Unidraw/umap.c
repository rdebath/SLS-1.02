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
 * UMap implementation.
 */

#include <Unidraw/globals.h>
#include <Unidraw/umap.h>

#include <string.h>

/*****************************************************************************/

UMapElem::UMapElem () { }
UMapElem::~UMapElem () { }
void* UMapElem::id () { return nil; }
void* UMapElem::tag () { return nil; }

/*****************************************************************************/

UMap::UMap () { }
UMap::~UMap () { Clear(); }

void UMap::Clear () { 
    for (int i = 0; i < _elems.Count(); ++i) {
        UMapElem* e = Elem(i);
        delete e;
    }
    _elems.Clear(); 
}


void UMap::Register (UMapElem* elem) { _elems.Insert(elem, _elems.Count()); }
void UMap::Unregister (UMapElem* elem) { _elems.Remove(_elems.Index(elem)); }

UMapElem* UMap::FindId (void* id) {
    for (int i = 0; i < _elems.Count(); ++i) {
        if (Elem(i)->id() == id) {
            return (UMapElem*) _elems[i];
        }
    }
    return nil;
}

UMapElem* UMap::FindTag (void* tag) {
    for (int i = 0; i < _elems.Count(); ++i) {
        if (Elem(i)->tag() == tag) {
            return (UMapElem*) _elems[i];
        }
    }
    return nil;
}
