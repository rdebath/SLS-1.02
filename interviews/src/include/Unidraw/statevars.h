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
 * State variable subclasses.
 */

#ifndef unidraw_statevars_h
#define unidraw_statevars_h

#include <Unidraw/statevar.h>

class Component;
class PSBrush;
class PSColor;
class PSPattern;
class PSFont;

class NameVar : public StateVar {
public:
    NameVar(const char* = nil);

    virtual const char* GetName();
    virtual void SetName(const char*);

    virtual StateVar& operator = (StateVar&);

    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    char* _name;
};

class CompNameVar : public NameVar {
public:
    CompNameVar(Component* = nil);

    virtual Component* GetComponent();
    virtual void SetComponent(Component*);
    virtual void UpdateName();
    virtual const char* PartOf();

    virtual StateVar& operator = (StateVar&);

    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    const char* CatalogName(Component*);
private:
    Component* _comp;
};

class ModifStatusVar : public StateVar {
public:
    ModifStatusVar(Component* = nil, boolean = false);
    ~ModifStatusVar();

    virtual boolean GetModifStatus();
    virtual void SetModifStatus(boolean);
    virtual Component* GetComponent();
    virtual void SetComponent(Component*);

    virtual StateVar& operator = (StateVar&);

    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    void modified(int);
private:
    int _modified;
    Component* _component;
    static class UList* _vars;
};

class MagnifVar : public StateVar {
public:
    MagnifVar(float = 1);

    virtual float GetMagnif();
    virtual void SetMagnif(float);

    virtual StateVar& operator = (StateVar&);

    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    float _magnif;
};

class GravityVar : public StateVar {
public:
    GravityVar(boolean = false);

    virtual boolean IsActive();
    virtual void Activate(boolean);

    virtual StateVar& operator = (StateVar&);

    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    int _active;
};

class FontVar : public StateVar {
public:
    FontVar(PSFont* = nil);
    virtual ~FontVar();

    virtual PSFont* GetFont();
    virtual void SetFont(PSFont*);

    virtual StateVar& operator = (StateVar&);

    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    PSFont* _psfont;
};

class BrushVar : public StateVar {
public:
    BrushVar(PSBrush* = nil);
    virtual ~BrushVar();

    virtual PSBrush* GetBrush();
    virtual void SetBrush(PSBrush*);

    virtual StateVar& operator = (StateVar&);

    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    PSBrush* _psbrush;
};

class PatternVar : public StateVar {
public:
    PatternVar(PSPattern* = nil);
    virtual ~PatternVar();

    virtual PSPattern* GetPattern();
    virtual void SetPattern(PSPattern*);

    virtual StateVar& operator = (StateVar&);

    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    PSPattern* _pspattern;
};

class ColorVar : public StateVar {
public:
    ColorVar(PSColor* fg = nil, PSColor* bg = nil);
    virtual ~ColorVar();

    virtual PSColor* GetFgColor();
    virtual PSColor* GetBgColor();
    virtual void SetColors(PSColor* fg, PSColor* bg);

    virtual StateVar& operator = (StateVar&);

    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    PSColor* _psfgcolor, *_psbgcolor;
};

#endif
