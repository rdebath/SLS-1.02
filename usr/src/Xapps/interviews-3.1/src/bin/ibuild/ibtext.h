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
 * IText component declarations.
 */

#ifndef ibtext_h
#define ibtext_h

#include <Unidraw/Components/text.h>
#include "ibcode.h"
#include "ibcomp.h"

class ITextComp : public IComp {
public:
    ITextComp(TextGraphic* = nil);

    virtual void Interpret(Command*);
    virtual ClassId GetSubstId(const char*& delim);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class ITextView : public IView {
public:
    ITextView(ITextComp* = nil);

    ITextComp* GetITextComp();
    virtual Graphic* GetGraphic();
    virtual Manipulator* CreateManipulator(Viewer*,Event&,Transformer*,Tool*);
    virtual Command* InterpretManipulator(Manipulator*);
};

class TextCode : public GraphicCodeView {
public:
    TextCode(ITextComp* = nil);

    ITextComp* GetITextComp();
    virtual boolean Definition(ostream&);

    virtual void Update();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual boolean GCoreConstDecls(ostream&);
    virtual boolean GCoreConstInits(ostream&);
    virtual boolean GConstDecls(ostream&);
    virtual boolean GConstInits(ostream&);

    virtual boolean EmitIncludeHeaders(ostream&);
    virtual const char* GetCVHeader();
};

#endif
