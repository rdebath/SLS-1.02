/*
 * Copyright (c) 1991 Stanford University
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
 * Box component declarations.
 */

#ifndef ibbox_h
#define ibbox_h

#include "ibscene.h"

class PointObj;
class Shape;

struct BoxCanonical;

class BoxComp : public SceneComp {
public:
    virtual void Resize();
    virtual void Reconfig();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    BoxComp();

    virtual void ComputeShape(Shape*);
    virtual void GetActual(int&, int&);
    virtual void GetCanonical(Shape*, BoxCanonical&);
    virtual void PlaceElement(InteractorComp*, Coord, int, int, PointObj&);
private:
    int MinorPos();
};

class HBoxComp : public BoxComp {
public:
    HBoxComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    virtual void ComputeShape(Shape*);
    virtual void GetActual(int&, int&);
    virtual void GetCanonical(Shape*, BoxCanonical&);
    virtual void PlaceElement(InteractorComp*, Coord, int, int, PointObj&);
};

class VBoxComp : public BoxComp {
public:
    VBoxComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    virtual void ComputeShape(Shape*);
    virtual void GetActual(int&, int&);
    virtual void GetCanonical(Shape*, BoxCanonical&);
    virtual void PlaceElement(InteractorComp*, Coord, int, int, PointObj&);
};

class BoxCode : public CodeView {
public:
    BoxCode(BoxComp* = nil);

    virtual void Update();
    virtual boolean Definition(ostream&);
    BoxComp* GetBoxComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
};

#endif
