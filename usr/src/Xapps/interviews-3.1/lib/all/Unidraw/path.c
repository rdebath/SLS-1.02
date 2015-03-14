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
 * Path implementation.
 */

#include <Unidraw/path.h>
#include <Unidraw/ulist.h>

#include <OS/memory.h>

/*****************************************************************************/

inline int Hash (Connector* c) { return ((unsigned int) c) % SLOTS; }
inline void* Elem (UList* u) { return (*u)(); }

static UList* Copy (UList* ulist) {
    UList* copy = nil;

    if (ulist != nil) {
        copy = new UList;

        for (UList* u = ulist->First(); u != ulist->End(); u = u->Next()) {
            copy->Append(new UList(Elem(u)));
        }
    }
    return copy;
}

/*****************************************************************************/

Path::Path (Path* path) {
    Memory::zero(_slot, sizeof(void*) * SLOTS);

    if (path != nil) {
        for (int i = 0; i < SLOTS; ++i) {
            _slot[i] = Copy(path->_slot[i]);
        }
    }
}

Path::~Path () {
    for (int i = 0; i < SLOTS; ++i) {
        delete _slot[i];
    }
}

void Path::Visit (Connector* c) {
    int n = Hash(c);

    if (_slot[n] == nil) {
        _slot[n] = new UList;
    }
    _slot[n]->Append(new UList(c));
}

boolean Path::Visited (Connector* c) {
    int n = Hash(c);
    UList* slot = _slot[n];

    return (slot == nil) ? false : slot->Find(c) != nil;
}
