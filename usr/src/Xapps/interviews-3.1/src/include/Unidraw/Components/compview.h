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
 * ComponentView - base class for views of Components.
 */

#ifndef unidraw_components_compview_h
#define unidraw_components_compview_h

#include <Unidraw/globals.h>

class Command;
class Component;
class Iterator;

class ComponentView {
public:
    virtual void Update();
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);
    virtual ComponentView* GetParent();

    virtual void First(Iterator&);
    virtual void Last(Iterator&);
    virtual void Next(Iterator&);
    virtual void Prev(Iterator&);
    virtual boolean Done(Iterator);

    Component* GetSubject();

    virtual ~ComponentView();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    ComponentView(Component* subject = nil);

    friend class Component;
    virtual void SetSubject(Component*);
    virtual void SetParent(ComponentView* child, ComponentView* parent);
protected:
    Component* _subject;
};

#endif
