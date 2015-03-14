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
 * Text component declarations.
 */

#ifndef unidraw_components_text_h
#define unidraw_components_text_h

#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Components/psview.h>

#include <Unidraw/Graphic/ulabel.h>

#include <IV-2_6/_enter.h>

class TextGraphic;
class TextManip;

class TextComp : public GraphicComp {
public:
    TextComp(TextGraphic* = nil);

    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);
    
    TextGraphic* GetText();
    
    virtual Component* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class TextView : public GraphicView {
public:
    TextView(TextComp* = nil);

    virtual void Interpret(Command*);
    virtual void Update();

    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    TextComp* GetTextComp();
    virtual Graphic* GetGraphic();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean TextChanged();
};

class TextGraphic : public ULabel {
public:
    TextGraphic(const char*, int h, Graphic* = nil);
    TextGraphic(const char*, Graphic*);

    void SetLineHeight(int);
    int GetLineHeight();

    virtual boolean operator == (TextGraphic&);
    virtual boolean operator != (TextGraphic&);

    virtual Graphic* Copy();
protected:
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual boolean contains(PointObj&, Graphic*);
    virtual boolean intersects(BoxObj&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
private:
    void CalcBox(Coord&, Coord&, Coord&, Coord&, PSFont*);
    boolean RotatedIntersects(BoxObj&, Graphic*);
    boolean TransformedIntersects(BoxObj&, Graphic*);
    boolean UntransformedIntersects(BoxObj&, Graphic*);
protected:
    int _lineHt;
};

class PSText : public PostScriptView {
public:
    PSText(TextComp* = nil);

    virtual boolean Definition(ostream&);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    const char* Filter(const char*, int);
};

#include <IV-2_6/_leave.h>

#endif
