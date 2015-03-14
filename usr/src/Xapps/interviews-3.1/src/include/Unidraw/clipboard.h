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
 * Clipboard - contains a list of component subjects.
 */

#ifndef unidraw_clipboard_h
#define unidraw_clipboard_h

#include <InterViews/enter-scope.h>
#include <Unidraw/enter-scope.h>

#include <InterViews/_enter.h>

class GraphicComp;
class Iterator;
class UList;
class Selection;

class Clipboard {
public:
    Clipboard(GraphicComp* = nil);
    virtual ~Clipboard();

    void Init(Selection*);
    void CopyInit(Selection*);

    void Clear();
    void DeleteComps();

    void Append(GraphicComp*);
    void Prepend(GraphicComp*);
    void InsertAfter(Iterator, GraphicComp*);
    void InsertBefore(Iterator, GraphicComp*);
    void Remove(GraphicComp*);
    void Remove(Iterator&);

    GraphicComp* GetComp(Iterator);
    void SetComp(GraphicComp*, Iterator&);

    void First(Iterator&);
    void Last(Iterator&);
    void Next(Iterator&);
    void Prev(Iterator&);
    boolean Done(Iterator);
    boolean IsEmpty();
    boolean Includes(GraphicComp*);

    virtual Clipboard* Copy();
    virtual Clipboard* DeepCopy();
protected:
    UList* Elem(Iterator);
    GraphicComp* Comp(UList*);
protected:
    UList* _comps;
};

#include <InterViews/_leave.h>

#endif
