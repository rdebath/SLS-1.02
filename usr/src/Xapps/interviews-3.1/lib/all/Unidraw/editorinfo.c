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
 * EditorInfo implementation.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/editorinfo.h>
#include <Unidraw/globals.h>
#include <Unidraw/unidraw.h>

#include <ctype.h>
#include <stdio.h>
#include <string.h>

/*****************************************************************************/

class StringStringElem : public UMapElem {
public:
    StringStringElem(const char* idstring, const char* tagstring);
    virtual ~StringStringElem();
    
    virtual void* id();
    virtual void* tag();
private:
    void* _idstring;
    char* _tagstring;
};

StringStringElem::StringStringElem (const char* id, const char* tag) {
    _idstring = strnew(id);
    _tagstring = strnew(tag);
}

StringStringElem::~StringStringElem () { delete _idstring; delete _tagstring; }
void* StringStringElem::id () { return _idstring; }
void* StringStringElem::tag () { return _tagstring; }

/*****************************************************************************/

EditorInfo::EditorInfo () { }
EditorInfo::~EditorInfo () { unidraw->GetCatalog()->Forget(this); }

void EditorInfo::Register (const char* name, const char* info) {
    UMap::Register(new StringStringElem(name, info));
}

void EditorInfo::UnregisterName (const char* name) {
    UMapElem* elem = FindId((void*) name);

    if (elem != nil) {
         UMap::Unregister(elem);
         delete elem;
     }
}

void EditorInfo::UnregisterInfo (const char* info) {
    UMapElem* elem = FindTag((void*) info);

    if (elem != nil) {
        UMap::Unregister(elem);
        delete elem;
    }
}

boolean EditorInfo::Registered (const char* name) {
    return FindId((void*) name) != nil;
}

const char* EditorInfo::GetName (const char* info) { 
    UMapElem* elem = FindTag((void*) info);
    return (elem == nil) ? nil : (char*) elem->id();
}

const char* EditorInfo::GetName (int index) { 
    return (char*) Elem(index)->id();
}

const char* EditorInfo::GetInfo (const char* name) {
    UMapElem* elem = FindId((void*) name);
    return (elem == nil) ? nil : (char*) elem->tag();
}

const char* EditorInfo::GetInfo (int index) {
    return (char*) Elem(index)->tag();
}

UMapElem* EditorInfo::FindId (void* id) {
    for (int i = 0; i < _elems.Count(); ++i) {
        if (strcmp(GetName(i), (char*) id) == 0) {
            return (UMapElem*) _elems[i];
        }
    }
    return nil;
}

UMapElem* EditorInfo::FindTag (void* tag) {
    for (int i = 0; i < _elems.Count(); ++i) {
        if (strcmp(GetInfo(i), (char*) tag) == 0) {
            return (UMapElem*) _elems[i];
        }
    }
    return nil;
}
