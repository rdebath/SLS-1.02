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
 * Pin component declarations.
 */

#ifndef unidraw_components_pin_h
#define unidraw_components_pin_h

#include <Unidraw/Components/connector.h>
#include <Unidraw/Components/psview.h>
#include <Unidraw/Graphic/graphic.h>

#include <IV-2_6/InterViews/rubcurve.h>

#include <IV-2_6/_enter.h>

class PinGraphic;

class PinComp : public Connector {
public:
    PinComp(PinGraphic* = nil);

    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual void Connect(Connector*, CGlue* = nil);

    virtual Mobility GetMobility();
    virtual void SetMobility(Mobility);

    PinGraphic* GetPin();

    virtual Component* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    Mobility _mobility;
};

class PinView : public ConnectorView {
public:
    PinView(PinComp* = nil);

    virtual void Interpret(Command*);
    virtual void Update();

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    PinComp* GetPinComp();
    virtual Graphic* GetGraphic();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    PinGraphic* GetPin();
    virtual PinComp* NewSubject(PinGraphic*);

    Manipulator* CreateGraphicCompManip(Viewer*,Event&,Transformer*,Tool*);
    Manipulator* CreateConnectManip(Viewer*,Event&,Transformer*,Tool*);
    Command* InterpGraphicCompManip(Manipulator*);
    Command* InterpConnectManip(Manipulator*);
};

class PinGraphic : public Graphic {
public:
    PinGraphic(Coord = 0, Coord = 0, Graphic* = nil);
    virtual ~PinGraphic();

    virtual void SetBrush(PSBrush*);
    virtual PSBrush* GetBrush();
    void GetOriginal(Coord&, Coord&);

    virtual Graphic* Copy();
protected:
    virtual void concatGS(Graphic*, Graphic*, Graphic*);
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
protected:
    Coord _x, _y;
    PSBrush* _br;
};

class PSPin : public PostScriptView {
public:
    PSPin(PinComp* = nil);

    virtual boolean Definition(ostream&);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class SlidingPin : public SlidingEllipse {
public:
    SlidingPin(
        Painter*, Canvas*, Coord cx, Coord cy, int, Coord rfx, Coord rfy
    );
    virtual void Draw();
};

class FixedPin : public Rubberband {
public:
    FixedPin(Painter*, Canvas*, Coord, Coord, int);
    virtual void GetOriginal(Coord&, Coord&, int&);
    virtual void Draw();
protected:
    Coord _cx, _cy;
    int _rad;
};

#include <IV-2_6/_leave.h>

#endif
