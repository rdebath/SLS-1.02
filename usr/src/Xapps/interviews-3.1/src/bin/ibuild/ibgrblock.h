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
 *  GraphicBlock component declaration
 */

#ifndef ibgrblock_h
#define ibgrblock_h

#include "ibscene.h"
#include "ibgraphic.h"

class Command;
class IGraphicComps;
class GrBlockGraphic;

class GrBlockPicture : public IBGraphic {
public:
    GrBlockPicture(CanvasVar* = nil, Graphic* = nil);

    virtual void SetCanvasVar(CanvasVar*);
    virtual void SetColors(PSColor* f, PSColor* b);
    virtual PSColor* GetBgColor();
    void SetGrBlockGraphic(GrBlockGraphic*);
    void SetClip(boolean);
    boolean GetClip();

protected:
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
    virtual void drawClipped(Canvas*, Coord, Coord, Coord, Coord, Graphic*);
    virtual void concatGS(Graphic*, Graphic*, Graphic*);

protected:
    GrBlockGraphic* _grblockgr;
    boolean _clipped;
    boolean _damage;
};

class GrBlockComp : public MonoSceneComp {
public:
    GrBlockComp(GrBlockGraphic* = nil);
    virtual ~GrBlockComp();

    void ReadGraphicComp(const char*);
    void WriteGraphicComp(const char*);

    IGraphicComps* GetTop();

    virtual boolean IsRelatable();

    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);

    virtual void First(Iterator&);
    virtual void Last(Iterator&);
    virtual void Next(Iterator&);
    virtual void Prev(Iterator&);
    virtual boolean Done(Iterator);
    virtual boolean IsEmpty();

    virtual GraphicComp* GetComp(Iterator);
    virtual void SetComp(GraphicComp*, Iterator&);
    virtual void Bequeath();

    virtual void Append(GraphicComp*);
    virtual void Prepend(GraphicComp*);
    virtual void InsertBefore(Iterator, GraphicComp*);
    virtual void InsertAfter(Iterator, GraphicComp*);

    virtual void Remove(GraphicComp*);
    virtual void Remove(Iterator&);

    virtual void Resize();
    virtual void Reconfig();

    virtual void Instantiate();
    virtual boolean IsAScene();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual void StoreCanvas(Command*);
    virtual void RestoreCanvas(Command*);

    void SetGrBlockGraphic(GrBlockGraphic*);
    GrBlockGraphic* GetGrBlockGraphic(); 
protected:
    void SubNotify();

private:
    IGraphicComps* _top;
    GrBlockGraphic* _grblockgr;
};

inline GrBlockGraphic* GrBlockComp::GetGrBlockGraphic() { return _grblockgr; }
inline boolean GrBlockComp::IsAScene () { return false; }
inline boolean GrBlockComp::IsRelatable () { return true; }
inline IGraphicComps* GrBlockComp::GetTop () { return _top; }

class GrBlockView : public MonoSceneView {
public:
    GrBlockView(GrBlockComp* = nil);
    virtual ~GrBlockView();
    GrBlockComp* GetGrBlockComp();

    virtual GraphicComp* CreateProtoComp(Editor*, Coord, Coord, Coord, Coord);
    virtual Graphic* GetGraphic();
    virtual void Update();
    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    GrBlockGraphic* _grblockgr;
};

class GrBlockCode : public MonoSceneCode {
public:
    GrBlockCode(GrBlockComp* = nil);

    virtual void Update();
    virtual boolean Definition(ostream&);
    GrBlockComp* GetGrBlockComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
    boolean EmitGraphicState(ostream&);
};

class GrBlockGraphic : public IBGraphic {
public:
    GrBlockGraphic(
        CanvasVar* = nil, Graphic* = nil, Alignment = Center
    );

    void SetAlignment(Alignment);
    Alignment GetAlignment();

    virtual Graphic* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

protected:
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
    virtual void drawClipped(Canvas*, Coord, Coord, Coord, Coord, Graphic*);
protected:
    Alignment _align;
};

inline void GrBlockGraphic::SetAlignment (Alignment a) { _align = a; }
inline Alignment GrBlockGraphic::GetAlignment () { return _align; }

#endif
