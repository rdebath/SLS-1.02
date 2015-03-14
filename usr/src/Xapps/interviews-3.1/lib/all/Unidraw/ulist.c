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
 * UList implementation.
 */

#include <Unidraw/ulist.h>

/*****************************************************************************/

UList::UList (void* p) { _next = this; _prev = this; _object = p; }

UList::~UList () {
    UList* next = _next;

    if (next != this && next != nil) {
        Remove(this);
        delete next;
    }
}

void UList::Append (UList* e) {
    _prev->_next = e;
    e->_prev = _prev;
    e->_next = this;
    _prev = e;
}

void UList::Prepend (UList* e) {
    _next->_prev = e;
    e->_prev = this;
    e->_next = _next;
    _next = e;
}

void UList::Remove (UList* e) {
    e->_prev->_next = e->_next;
    e->_next->_prev = e->_prev;
    e->_prev = e->_next = nil;
}

void UList::Delete (void* p) {
    register UList* e;

    e = Find(p);
    if (e != nil) {
	Remove(e);
	delete e;
    }
}

UList* UList::Find (void* p) {
    register UList* e;

    for (e = _next; e != this; e = e->_next) {
	if (e->_object == p) {
	    return e;
	}
    }
    return nil;
}

UList* UList::operator[] (int count) {
    UList* pos = First();
    int i;

    for (i = 1; i < count && pos != End(); ++i) {
	pos = pos->Next();
    }
    if (i == count) {
	return pos;
    }
    return nil;
}	
