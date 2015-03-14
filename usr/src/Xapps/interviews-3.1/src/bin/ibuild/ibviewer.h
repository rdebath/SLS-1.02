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
 *  Viewer component declarations
 */

#ifndef ibviewer_h
#define ibviewer_h

#include "ibmessage.h"
#include "ibgrblock.h"

class MemberNameVar;
class InfoDialog;
class IBViewerGraphic;

class IBViewerComp : public GrBlockComp {
public:
    IBViewerComp(IBViewerGraphic* = nil);
    virtual ~IBViewerComp();

    virtual boolean IsRelatableTo(InteractorComp*);
    virtual boolean IsRelatable();

    virtual void Instantiate();
    virtual void Resize();
    virtual void Reconfig();
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    virtual StateVar* GetState(const char*);
    virtual void SetState(const char*, StateVar*);
    virtual InteractorComp& operator = (InteractorComp&);

    MemberNameVar* GetEditorVar();
    IBViewerGraphic* GetIBViewerGraphic();
    Graphic* GetPage();
protected:
    MemberNameVar* _edVar;
    Graphic* _page;
};

inline MemberNameVar* IBViewerComp::GetEditorVar() { return _edVar; }
inline boolean IBViewerComp::IsRelatable () { return true; }
inline Graphic* IBViewerComp::GetPage () { return _page; }

class IBViewerView : public GrBlockView {
public:
    IBViewerView(IBViewerComp* = nil);
    virtual ~IBViewerView();
    IBViewerComp* GetIBViewerComp();

    virtual GraphicComp* CreateProtoComp(Editor*, Coord, Coord, Coord, Coord);
    virtual Graphic* GetGraphic();
    virtual void Update();
    virtual InfoDialog* GetInfoDialog();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    Graphic* _page;
};

class IBViewerGraphic : public GrBlockGraphic {
public:
    IBViewerGraphic(CanvasVar* = nil, Graphic* = nil);

    void SetShape(int, int);
    void GetShape(int&, int&);

    virtual Graphic* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    int _w, _h;
};

inline void IBViewerGraphic::SetShape(int w, int h) { _w = w; _h = h; }
inline void IBViewerGraphic::GetShape(int& w, int& h) { w = _w; h = _h; }

class IBViewerCode : public GrBlockCode {
public:
    IBViewerCode(IBViewerComp* = nil);

    virtual boolean Definition(ostream&);
    IBViewerComp* GetIBViewerComp();

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

