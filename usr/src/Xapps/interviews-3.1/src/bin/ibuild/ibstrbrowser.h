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
 *  StrBrowser component declarations
 */

#ifndef ibstrbrowser_h
#define ibstrbrowser_h

#include "ibbutton.h"

class BooleanStateVar;
class StrBrowserGraphic;

class StrBrowserComp : public ButtonComp {
public:
    StrBrowserComp(StrBrowserGraphic* = nil);
    virtual ~StrBrowserComp();

    StrBrowserGraphic* GetStrBrowserGraphic();
    BooleanStateVar* GetUniqueSel();

    virtual void Instantiate();
    virtual void Reconfig();
    virtual StateVar* GetState(const char*);
    virtual void SetState(const char*, StateVar*);

    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

protected:
    BooleanStateVar* _uniqueSel;
};

inline BooleanStateVar* StrBrowserComp::GetUniqueSel () { return _uniqueSel; }

class StrBrowserView : public ButtonView {
public:
    StrBrowserView(StrBrowserComp* = nil);

    StrBrowserComp* GetStrBrowserComp();
    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);

    virtual InfoDialog* GetInfoDialog();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class StrBrowserCode : public ButtonCode {
public:
    StrBrowserCode(StrBrowserComp* = nil);

    virtual boolean Definition(ostream&);
    StrBrowserComp* GetStrBrowserComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
};

class StrBrowserGraphic : public MessageGraphic {
public:
    StrBrowserGraphic(
	const char* = nil, int w = 0, int h = 0, 
	CanvasVar* = nil, Graphic* = nil
    );

    virtual void Natural(int&, int&);
    void GetRowsCols(int&, int&);
    void SetRowsCols(int, int);

    virtual const char* GetClassName();
    virtual Graphic* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
protected:
    virtual void draw(Canvas*, Graphic*);
private:
    int _width, _height;
};

#endif
