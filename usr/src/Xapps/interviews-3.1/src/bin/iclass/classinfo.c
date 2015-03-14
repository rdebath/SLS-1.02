/*
 * Copyright (c) 1989 Stanford University
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
 * ClassInfo implementation.
 */

#include "classinfo.h"
#include "globals.h"

#include <InterViews/defs.h>

#include <string.h>

/*****************************************************************************/

ClassInfo::ClassInfo (const char* name, const char* path, int lineno) {
    const int defaultSize = 8;

    _name = strnew(name);
    _path = nil;
    _lineno = lineno;
    Path(path);
    _childbufsize = _parentbufsize = defaultSize;
    _childbuf = new ClassInfo*[_childbufsize];
    _parentbuf = new ClassInfo*[_parentbufsize];
    _childcount = _parentcount = 0;
}

ClassInfo::~ClassInfo () {
    delete _name;
    delete _path;
    delete _childbuf;
    delete _parentbuf;
}

void ClassInfo::Path (const char* path) {
    delete _path;
    _path = (path == nil) ? nil : strnew(path);
}

void ClassInfo::IncludeChild (ClassInfo* child) {
    Insert(
        child, Position(child, _childbuf, _childcount), 
        _childbuf, _childbufsize, _childcount
    );
}

void ClassInfo::IncludeParent (ClassInfo* parent) {
    Insert(
        parent, Position(parent, _parentbuf, _parentcount), 
        _parentbuf, _parentbufsize, _parentcount
    );
}

int ClassInfo::ChildIndex (const char* child) {
    return Index(child, _childbuf, _childcount);
}

int ClassInfo::ParentIndex (const char* parent) {
    return Index(parent, _parentbuf, _parentcount);
}

ClassInfo* ClassInfo::Child (int index) {
    return Info(index, _childbuf, _childcount);
}

ClassInfo* ClassInfo::Parent (int index) {
    return Info(index, _parentbuf, _parentcount);
}

void ClassInfo::Insert (
    ClassInfo* info, int index, ClassInfo**& buf, int& bufsize, int& count
) {
    BufInsert(info, index, (const void**&) buf, bufsize, count);
}

void ClassInfo::Remove (int index, ClassInfo** buf, int& count) {
    if (0 <= index && index < count) {
        BufRemove(index, (const void**) buf, count);
    }
}

int ClassInfo::Index (const char* s, ClassInfo** buf, int count) {
    for (int i = 0; i < count; ++i) {
        if (strcmp(s, buf[i]->Name()) == 0) {
            return i;
        }
    }
    return -1;
}

ClassInfo* ClassInfo::Info (int index, ClassInfo** buf, int count) { 
    return (0 <= index && index < count) ? buf[index] : nil;
}

int ClassInfo::Position (ClassInfo* info, ClassInfo** buf, int count) {
    for (int i = 0; i < count; ++i) {
        if (strcmp(info->Name(), buf[i]->Name()) < 0) {
            return i;
        }
    }
    return count;
}

void ClassInfo::Clear (ClassInfo** buf, int count) {
    for (int i = 0; i < count; ++i) {
        delete buf[i];
    }
    count = 0;
}
