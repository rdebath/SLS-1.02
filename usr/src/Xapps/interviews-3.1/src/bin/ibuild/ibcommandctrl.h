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

#ifndef ibcommandctrl_h
#define ibcommandctrl_h

#include "ibpanelctrl.h"

class CommandCtrlComp : public PanelCtrlComp {
public:
    CommandCtrlComp(PanelCtrlGraphic* = nil);

    virtual boolean IsRelatableTo(InteractorComp*);
    virtual void Resize();
    virtual void Reconfig();

    virtual void Instantiate();
    virtual void Interpret(Command*);
    virtual void Uninterpret(Command*);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class CommandCtrlView : public PanelCtrlView {
public:
    CommandCtrlView(CommandCtrlComp* = nil);
    CommandCtrlComp* GetCommandCtrlComp();

    virtual GraphicComp* CreateProtoComp(Editor*, Coord, Coord, Coord, Coord);
    virtual InfoDialog* GetInfoDialog();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class CommandCtrlCode : public PanelCtrlCode {
public:
    CommandCtrlCode(CommandCtrlComp* = nil);

    virtual boolean Definition(ostream&);
    CommandCtrlComp* GetCommandCtrlComp();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);
    virtual boolean EmitIncludeHeaders(ostream&);
    virtual boolean EmitCommonHeaders(const char*, ostream&);
    virtual boolean EmitParameters(const char*, ostream&);

    virtual boolean ToolCoreConstDecls(ostream&);
    virtual boolean ToolCoreConstInits(ostream&);
    virtual boolean ToolConstDecls(ostream&);
    virtual boolean ToolConstInits(ostream&);
protected:
    boolean _emitToolInits;
    boolean _emitBaseclass;
};

#endif

