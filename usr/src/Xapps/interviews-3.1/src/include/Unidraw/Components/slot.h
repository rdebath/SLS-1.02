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
 * Slot component declarations.
 */

#ifndef unidraw_components_slot_h
#define unidraw_components_slot_h

#include <Unidraw/Components/connector.h>
#include <Unidraw/Components/psview.h>
#include <Unidraw/Graphic/graphic.h>

#include <IV-2_6/_enter.h>

class SlotGraphic;

class SlotComp : public Connector {
public:
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual Mobility GetMobility();
    virtual void SetMobility(Mobility);

    SlotGraphic* GetSlot();

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    SlotComp(SlotGraphic* = nil);

    void SetOrientation(SlotGraphic*, Orientation);
protected:
    Mobility _mobility;
};

class SlotView : public ConnectorView {
public:
    virtual void Interpret(Command*);
    virtual void Update();

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    SlotComp* GetSlotComp();
    virtual Graphic* GetGraphic();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    SlotView(SlotComp* = nil);

    SlotGraphic* GetSlot();
    virtual SlotComp* NewSubject(SlotGraphic*);

    Manipulator* CreateGraphicCompManip(Viewer*,Event&,Transformer*,Tool*);
    Manipulator* CreateConnectManip(Viewer*,Event&,Transformer*,Tool*);
    Command* InterpGraphicCompManip(Manipulator*);
    Command* InterpConnectManip(Manipulator*);
};

class SlotGraphic : public Graphic {
public:
    SlotGraphic(Coord, Coord, Coord, Graphic* = nil);
    virtual ~SlotGraphic();

    virtual void SetBrush(PSBrush*);
    virtual PSBrush* GetBrush();

    void GetOriginal(Coord&, Coord&, Coord&);
    Orientation GetOrientation();

    virtual Graphic* Copy();
protected:
    virtual void concatGS(Graphic*, Graphic*, Graphic*);
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
protected:
    Coord _x, _y, _length;
    PSBrush* _br;
private:
    friend class SlotComp;
    void SetOrientation(Orientation);
};

class HSlotComp : public SlotComp {
public:
    HSlotComp(SlotGraphic* = nil);

    virtual void Connect(Connector*, CGlue* = nil);

    virtual Component* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class HSlotView : public SlotView {
public:
    HSlotView(HSlotComp* = nil);

    HSlotComp* GetHSlotComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual SlotComp* NewSubject(SlotGraphic*);
};

class VSlotComp : public SlotComp {
public:
    VSlotComp(SlotGraphic* = nil);

    virtual void Connect(Connector*, CGlue* = nil);

    virtual Component* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class VSlotView : public SlotView {
public:
    VSlotView(VSlotComp* = nil);

    VSlotComp* GetVSlotComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual SlotComp* NewSubject(SlotGraphic*);
};

class PSSlot : public PostScriptView {
public:
    PSSlot(SlotComp* = nil);

    virtual boolean Definition(ostream&);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

#include <IV-2_6/_leave.h>

#endif
