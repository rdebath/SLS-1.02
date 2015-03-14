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
 * ClassInfo - an object containing information about a class.
 */

#ifndef classinfo_h
#define classinfo_h

#include <InterViews/defs.h>

class ClassInfo {
public:
    ClassInfo(const char* className, const char* path = "", int lineno = 1);
    ~ClassInfo();

    const char* Name();
    const char* Path();
    void Path(const char*);
    int LineNumber();
    void LineNumber(int);

    void IncludeChild(ClassInfo*);
    void IncludeParent(ClassInfo*);

    int ChildIndex(const char* child);
    int ParentIndex(const char* parent);

    ClassInfo* Child(int index);
    ClassInfo* Parent(int index);
private:
    void Insert(ClassInfo*, int index, ClassInfo**&, int&, int&);
    void Remove(int index, ClassInfo**, int&);
    int Index(const char*, ClassInfo**, int);
    ClassInfo* Info(int index, ClassInfo**, int);
    int Position(ClassInfo*, ClassInfo**, int);
    void Clear(ClassInfo**, int);
private:
    char* _name;
    char* _path;
    int _lineno;
    ClassInfo** _childbuf;
    int _childbufsize;
    int _childcount;
    ClassInfo** _parentbuf;
    int _parentbufsize;
    int _parentcount;
};    

inline const char* ClassInfo::Name () { return _name; }
inline const char* ClassInfo::Path () { return _path; }
inline int ClassInfo::LineNumber () { return _lineno; }
inline void ClassInfo::LineNumber (int n) { _lineno = n; }

#endif
