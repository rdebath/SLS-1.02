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
 * FBrowser component declarations.
 */

#ifndef ibfbrowser_h
#define ibfbrowser_h

#include "ibstrbrowser.h"

class FBrowserVar;

class FBrowserComp : public StrBrowserComp {
public:
    FBrowserComp(StrBrowserGraphic* = nil);
    virtual ~FBrowserComp();

    virtual StateVar* GetState(const char*);
    virtual void SetState(const char*, StateVar*);

    FBrowserVar* GetFBrowserVar();

    virtual void Instantiate();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    FBrowserVar* _fbVar;
};

inline FBrowserVar* FBrowserComp::GetFBrowserVar () { return _fbVar; }

class FBrowserView : public StrBrowserView {
public:
    FBrowserView(FBrowserComp* = nil);
    FBrowserComp* GetFBrowserComp();

    virtual InfoDialog* GetInfoDialog();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class FBrowserCode : public ButtonCode {
public:
    FBrowserCode(FBrowserComp* = nil);

    virtual boolean Definition(ostream&);
    FBrowserComp* GetFBrowserComp();

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

