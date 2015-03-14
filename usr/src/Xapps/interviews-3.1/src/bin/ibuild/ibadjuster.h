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
 * Adjuster component declarations.
 */

#ifndef ibadjuster_h
#define ibadjuster_h

#include "ibcode.h"
#include "ibinteractor.h"
#include "ibgraphic.h"

class AdjusterGraphic;
class ButtonState;
class InfoDialog;
class MemberNameVar;
class UList;

class AdjusterComp : public InteractorComp {
public:
    AdjusterComp(AdjusterGraphic* = nil);
    virtual ~AdjusterComp();

    AdjusterGraphic* GetAdjusterGraphic();
    MemberNameVar* GetAdjusteeVar();

    virtual void Instantiate();
    virtual boolean IsRelatableTo(InteractorComp*);
    virtual boolean IsRelatable();

    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);
    virtual void Reconfig();

    virtual StateVar* GetState(const char*);
    virtual void SetState(const char*, StateVar*);

    virtual InteractorComp& operator = (InteractorComp&);

    virtual void Read(istream&);
    virtual void Write(ostream&);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    MemberNameVar* _adjusteeVar;
};

inline MemberNameVar* AdjusterComp::GetAdjusteeVar () { return _adjusteeVar; }
inline boolean AdjusterComp::IsRelatable () { return true; }

class AdjusterView : public InteractorView {
public:
    AdjusterView(AdjusterComp* = nil);
    AdjusterComp* GetAdjusterComp();

    virtual void Update();
    virtual InfoDialog* GetInfoDialog();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class AdjusterCode : public CodeView {
public:
    AdjusterCode(AdjusterComp* = nil);

    virtual void Update();
    virtual boolean Definition(ostream&);
    AdjusterComp* GetAdjusterComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
};

class AdjusterGraphic : public IBGraphic {
public:
    AdjusterGraphic(
	CanvasVar* = nil, Graphic* = nil,
	Bitmap* = nil, Bitmap* = nil, int h = 0, int w = 0
    );
    virtual ~AdjusterGraphic();
    virtual const char* GetClassName();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual Graphic* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    void GetSize(int&, int&);
    virtual void Init(Bitmap* fg_map, Bitmap* bg_map, int h, int w);
protected:
    virtual void getExtent(float&, float&, float&, float&, float&, Graphic*);
    virtual void draw(Canvas*, Graphic*);
    virtual void drawClipped(Canvas*, Coord, Coord, Coord, Coord, Graphic*);
protected:
    Bitmap* _fg_map, *_bg_map;
    int _hmap, _wmap;
};

inline void AdjusterGraphic::GetSize(int& w, int& h) 
						{w = _wmap, h = _hmap; }

class LMoverGraphic : public AdjusterGraphic {
public:
    LMoverGraphic(CanvasVar* = nil, Graphic* = nil);

    virtual const char* GetClassName();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class RMoverGraphic : public AdjusterGraphic {
public:
    RMoverGraphic(CanvasVar* = nil, Graphic* = nil);

    virtual const char* GetClassName();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class UMoverGraphic : public AdjusterGraphic {
public:
    UMoverGraphic(CanvasVar* = nil, Graphic* = nil);

    virtual const char* GetClassName();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class DMoverGraphic : public AdjusterGraphic {
public:
    DMoverGraphic(CanvasVar* = nil, Graphic* = nil);

    virtual const char* GetClassName();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class EnlargerGraphic : public AdjusterGraphic {
public:
    EnlargerGraphic(CanvasVar* = nil, Graphic* = nil);

    virtual const char* GetClassName();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class ReducerGraphic : public AdjusterGraphic {
public:
    ReducerGraphic(CanvasVar* = nil, Graphic* = nil);

    virtual const char* GetClassName();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

#endif

