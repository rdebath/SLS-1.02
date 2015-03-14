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
 * Message component declarations.
 */

#ifndef ibmessage_h
#define ibmessage_h

#include "ibcode.h"
#include "ibgraphic.h"
#include "ibinteractor.h"

class Command;
class MessageGraphic;
class MessageStateVar;
class TextManip;

class MessageComp : public InteractorComp {
public:
    MessageComp(MessageGraphic* = nil);

    MessageGraphic* GetMessageGraphic();
    virtual void Reconfig();

    virtual InteractorComp& operator = (InteractorComp&);
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class MessageView : public InteractorView {
public:
    MessageView(MessageComp* = nil);

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);
    virtual void Interpret(Command*);

    virtual void Update();

    MessageComp* GetMessageComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class MessageCode : public CodeView {
public:
    MessageCode(MessageComp* = nil);

    virtual boolean Definition(ostream&);
    MessageComp* GetMessageComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
};

class MessageGraphic : public IBGraphic {
public:
    MessageGraphic(
        const char* = nil, CanvasVar* = nil,
        Graphic* = nil, Alignment = Center, int pad = 0
    );

    virtual ~MessageGraphic();

    void SetAlignment(Alignment);
    Alignment GetAlignment();

    virtual void Natural(int&, int&);
    virtual void GetTextPosition(Coord&, Coord&, const Font*);
    const char* GetText();
    void SetText(const char*);
    void Position(TextManip*);

    virtual const char* GetClassName();
    virtual Graphic* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
protected:
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
    virtual void drawClipped(Canvas*, Coord, Coord, Coord, Coord, Graphic*);
private:
    char* _text;
    Alignment _align;
    int _pad;
};

inline const char* MessageGraphic::GetText () { return _text; }
inline void MessageGraphic::SetAlignment (Alignment a) { _align = a; }
inline Alignment MessageGraphic::GetAlignment () { return _align; }

#endif

