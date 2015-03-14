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
 * IdrawComp -- top level component that stores grid spacing info.
 */

#ifndef idcomp_h
#define idcomp_h

#include <Unidraw/Components/grcomp.h>
#include <Unidraw/Components/grview.h>
#include <Unidraw/Components/psview.h>

class IdrawComp : public GraphicComps {
public:
    IdrawComp();

    void SetGridSpacing(float, float);
    void GetGridSpacing(float&, float&);

    virtual ClassId GetClassId();
    virtual ClassId GetSubstId(const char*&);
    virtual boolean IsA(ClassId);
private:
    float _xincr, _yincr;
};

class IdrawView : public GraphicViews {
public:
    IdrawView(IdrawComp* = nil);
    
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class PSIdraw : public PostScriptViews {
public:
    PSIdraw(IdrawComp* = nil);
protected:
    virtual void Creator(ostream&);

    virtual void ArrowHeader(ostream&);
    virtual void ConstProcs(ostream&);
    virtual void GridSpacing(ostream&);
    virtual void LineProc(ostream&);
    virtual void MultiLineProc(ostream&);
    virtual void BSplineProc(ostream&);
    virtual void Prologue(ostream&);
    virtual void SetBrushProc(ostream&);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

#endif



