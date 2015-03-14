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
 * Button component declarations.
 */

#ifndef ibbutton_h
#define ibbutton_h

#include "ibmessage.h"

class ButtonState;
class ButtonStateVar;
class UList;

class ButtonComp : public MessageComp {
public:
    ButtonComp(MessageGraphic* = nil);
    virtual ~ButtonComp();

    virtual void Reconfig();
    virtual boolean IsRelatableTo(InteractorComp*);
    virtual boolean IsRelatable();
    virtual void Relate(InteractorComp*);

    virtual ButtonStateVar* GetButtonStateVar();
    virtual StateVar* GetState(const char*);
    virtual void SetState(const char*, StateVar*);
    virtual InteractorComp& operator = (InteractorComp&);
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    virtual void Instantiate();
protected:
    ButtonStateVar* _bsVar;
};

inline ButtonStateVar* ButtonComp::GetButtonStateVar() { return _bsVar; }
inline boolean ButtonComp::IsRelatable () { return true; }

class ButtonView : public MessageView {
public:
    ButtonView(ButtonComp* = nil);
    ButtonComp* GetButtonComp();

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);
    virtual InfoDialog* GetInfoDialog();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class ButtonCode : public CodeView {
public:
    ButtonCode(ButtonComp* = nil);

    virtual boolean Definition(ostream&);
    ButtonComp* GetButtonComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
};

class PushButtonGraphic : public MessageGraphic {
public:
    PushButtonGraphic(const char* = nil, CanvasVar* = nil, Graphic* = nil);

    virtual void Natural(int&, int&);
    virtual void GetTextPosition(Coord&, Coord&, const Font*);

    virtual const char* GetClassName();
    virtual Graphic* Copy();
    virtual ClassId GetClassId();
protected:
    virtual void draw(Canvas*, Graphic*);
};

class RadioButtonGraphic : public MessageGraphic {
public:
    RadioButtonGraphic(const char* = nil, CanvasVar* = nil, Graphic* = nil);

    virtual void Natural(int&, int&);
    virtual void GetTextPosition(Coord&, Coord&, const Font*);

    virtual const char* GetClassName();
    virtual Graphic* Copy();
    virtual ClassId GetClassId();
protected:
    virtual void draw(Canvas*, Graphic*);
private:
    static class Bitmap* radioMask, *radioPlain;
};

class CheckBoxGraphic : public MessageGraphic {
public:
    CheckBoxGraphic(const char* = nil, CanvasVar* = nil, Graphic* = nil);

    virtual void Natural(int&, int&);
    virtual void GetTextPosition(Coord&, Coord&, const Font*);

    virtual const char* GetClassName();
    virtual Graphic* Copy();
    virtual ClassId GetClassId();
protected:
    virtual void draw(Canvas*, Graphic*);
};

#endif

