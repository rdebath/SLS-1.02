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
 * User interface builder-specific state variables.
 */

#ifndef ibvars_h
#define ibvars_h

#include <InterViews/defs.h>
#include <InterViews/shape.h>
#include <InterViews/resource.h>
#include <Unidraw/statevars.h>
#include <Unidraw/umap.h>

class InteractorComp;
class IDVar;
class SubclassNameVar;
class MemberSharedName;

class CanvasVar : public StateVar {
public:
    CanvasVar(int w = 0, int h = 0);

    int Width();
    int Height();
    Coord xmax();
    Coord ymax();
    void SetSize(int w, int h);

    virtual StateVar& operator = (StateVar&);
    virtual boolean operator != (StateVar&);
    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    int _width, _height;
};

inline int CanvasVar::Width () { return _width; }
inline int CanvasVar::Height () { return _height; }
inline Coord CanvasVar::xmax () { return _width-1; }
inline Coord CanvasVar::ymax () { return _height-1; }

class IBNameVar : public NameVar {
public:
    IBNameVar(const char* = nil, boolean = true);
    boolean GetMachGen();
    void SetMachGen(boolean);

    virtual StateVar& operator = (StateVar&);
    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    static boolean GetUniqueFlag();
    static void SetUniqueFlag(boolean unique);

    virtual void GenNewName();
    virtual int& GetSerial();
protected:
    int _machgen;
    static boolean _unique;
    static int _IBSerial;
};

inline boolean IBNameVar::GetMachGen () { return _machgen; }
inline void IBNameVar::SetMachGen (boolean machgen) { _machgen = machgen; }
inline boolean IBNameVar::GetUniqueFlag () { return _unique; }
inline void IBNameVar::SetUniqueFlag (boolean unique) { _unique = unique; }
inline int& IBNameVar::GetSerial () { return _IBSerial; }

class SharedName : public IBNameVar {
public:
    SharedName(const char*, boolean);
    virtual ~SharedName();
    void ref() const;
    void unref() const;
private:
    unsigned _refcount;
};

class IDMap : public UMap {
public:
    IDMap();
    void Add(MemberSharedName*);
    void Add(IDVar*, SubclassNameVar*);
    void Update(IDVar*);
    void Delete(IDVar*);
    int FindID(SubclassNameVar*);
    int FindID(const char* subclass, const char* baseclass);
};

class IDVar : public IBNameVar {
public:
    IDVar(int id = -1, int origid = -1);
    virtual ~IDVar();

    int GetID();
    void SetID(int);
    int GetOrigID();
    void SetOrigID(int);
    void Update();

    virtual StateVar& operator = (StateVar&);
    virtual StateVar* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
    virtual void Read(istream&);

    virtual void GenNewName();
    virtual int& GetSerial();

    static void CreateMap();
    static IDMap* GetIDMap();
protected:
    static int _IDSerial;
protected:
    int _origid;
private:
    static IDMap* _idmap;
};

inline int& IDVar::GetSerial () { return _IDSerial; }
inline int IDVar::GetOrigID () { return _origid; }
inline void IDVar::SetOrigID (int origid) { _origid = origid; }
inline IDMap* IDVar::GetIDMap () { return _idmap; }

class SubclassNameVar : public SharedName {
public:
    SubclassNameVar(const char* = nil, boolean = true, boolean = false);
    virtual ~SubclassNameVar();

    void SetBaseClass(const char*);
    const char* GetBaseClass();

    boolean IsSubclass();

    void SetAbstract(boolean);
    boolean IsAbstract();

    virtual StateVar& operator = (StateVar&);
    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
    virtual int& GetSerial();
private:
    char* _baseClass;
    int _abstract;
    static int _subclassSerial;
};

inline const char* SubclassNameVar::GetBaseClass () { return _baseClass; }
inline void SubclassNameVar::SetAbstract (boolean abstract) {
    _abstract = abstract;
}
inline boolean SubclassNameVar::IsAbstract () { return _abstract; }
inline int& SubclassNameVar::GetSerial () { return _subclassSerial; }

class InstanceNameVar : public IBNameVar {
public:
    InstanceNameVar(const char* = "instance", boolean = true);

    virtual int& GetSerial();
    virtual StateVar* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
    virtual void Read(istream&);
private:
    InstanceNameVar(IBNameVar*);
private:
    static int _iSerial;
};

inline int& InstanceNameVar::GetSerial () { return _iSerial; }

class MemberSharedName : public SharedName {
public:
    MemberSharedName(const char* = nil, boolean = false, boolean = true);
    ~MemberSharedName();

    boolean GetExport();
    void SetExport(boolean);

    SubclassNameVar* GetSubclass();
    void SetSubclass(SubclassNameVar*);

    IDVar* GetIDVar();
    void SetIDVar(IDVar*);

    virtual void SetName(const char*);
    virtual int& GetSerial();

    virtual StateVar& operator = (StateVar&);
    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    SubclassNameVar* _subclass;
    IDVar* _idVar;
    int _export;
    static int _mSerial;
};

inline boolean MemberSharedName::GetExport() { return _export; }
inline SubclassNameVar* MemberSharedName::GetSubclass () { return _subclass; }
inline void MemberSharedName::SetExport(boolean export) { _export = export;}
inline int& MemberSharedName::GetSerial () { return _mSerial; }
inline IDVar* MemberSharedName::GetIDVar () { return _idVar; }

class MemberNameVar : public StateVar {
public:
    MemberNameVar(const char* = nil, boolean = false, boolean = true);
    virtual ~MemberNameVar();

    MemberSharedName* GetMemberSharedName();
    void SetMemberSharedName(MemberSharedName*);

    boolean GetExport();
    void SetExport(boolean);

    const char* GetName();
    void GenNewName();

    SubclassNameVar* GetSubclass();
    void SetSubclass(SubclassNameVar*);

    IDVar* GetIDVar();
    void SetIDVar(IDVar*);

    virtual StateVar& operator = (StateVar&);
    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    MemberNameVar(MemberSharedName*);
private:
    MemberSharedName* _msharedname;
};

inline boolean MemberNameVar::GetExport () { return _msharedname->GetExport();}
inline void MemberNameVar::SetExport(boolean b) { _msharedname->SetExport(b); }
inline const char* MemberNameVar::GetName () { return _msharedname->GetName();}
inline void MemberNameVar::GenNewName () { _msharedname->GenNewName(); }
inline MemberSharedName* MemberNameVar::GetMemberSharedName () {
    return _msharedname;
}
inline SubclassNameVar* MemberNameVar::GetSubclass () { 
    return _msharedname->GetSubclass();
}
inline void MemberNameVar::SetSubclass (SubclassNameVar* svar) {
    _msharedname->SetSubclass(svar);
}
inline IDVar* MemberNameVar::GetIDVar () { 
    return _msharedname->GetIDVar();
}
inline void MemberNameVar::SetIDVar (IDVar* idvar) {
    _msharedname->SetIDVar(idvar);
}

class ButtonSharedName : public SharedName {
public:
    ButtonSharedName(
        const char* = nil, const char* = nil, boolean = true, const char* = nil
    );
    virtual ~ButtonSharedName();

    int GetInitial();
    void SetInitial(int);

    boolean GetExport();
    void SetExport(boolean);

    const char* GetFuncName();
    void SetFuncName(const char*);

    SubclassNameVar* GetSubclass();
    void SetSubclass(SubclassNameVar*);

    virtual StateVar& operator = (StateVar&);

    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
    virtual int& GetSerial();
private:
    int _initial;
    int _export;
    char* _func;
    SubclassNameVar* _subclass;
    static int _bsSerial;
};

inline int ButtonSharedName::GetInitial() { return _initial; }
inline void ButtonSharedName::SetInitial(int initial) { _initial = initial;}
inline boolean ButtonSharedName::GetExport() { return _export; }
inline void ButtonSharedName::SetExport(boolean export) { _export = export;}
inline const char* ButtonSharedName::GetFuncName() { return _func; }
inline int& ButtonSharedName::GetSerial() { return _bsSerial; }
inline SubclassNameVar* ButtonSharedName::GetSubclass () { return _subclass; }

class ButtonStateVar : public StateVar {
public:
    ButtonStateVar(const char* = "BS");
    virtual ~ButtonStateVar();

    int GetInitial();
    void SetInitial(int);

    boolean GetExport();
    void SetExport(boolean);

    const char* GetFuncName();
    void SetFuncName(const char*);

    const char* GetName();
    void GenNewName();

    int GetSetting();
    void SetSetting(int);

    const char* GetSubclassName();
    void SetSubclassName(const char*);

    boolean IsSubclass();

    ButtonSharedName* GetButtonSharedName();
    void SetButtonSharedName(ButtonSharedName*);

    virtual StateVar& operator = (StateVar&);
    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    boolean DisplaySetting();
    void HideSetting();
private:
    ButtonStateVar(ButtonSharedName*, int);
private:
    void Init();
private:
    int _setting;
    int _showsetting;
    ButtonSharedName* _bsharedname;
};

inline int ButtonStateVar::GetInitial () { return _bsharedname->GetInitial(); }
inline void ButtonStateVar::SetInitial(int i) { _bsharedname->SetInitial(i); }
inline boolean ButtonStateVar::GetExport () {return _bsharedname->GetExport();}
inline void ButtonStateVar::SetExport(boolean b) { _bsharedname->SetExport(b);}
inline const char* ButtonStateVar::GetFuncName() { 
    return _bsharedname->GetFuncName();
}
inline void ButtonStateVar::SetFuncName(const char* name) { 
    _bsharedname->SetFuncName(name);
}
inline void ButtonStateVar::GenNewName() { _bsharedname->GenNewName(); }
inline const char* ButtonStateVar::GetName() { return _bsharedname->GetName();}
inline void ButtonStateVar::HideSetting () { _showsetting = false; }
inline boolean ButtonStateVar::DisplaySetting() { return _showsetting; }
inline int ButtonStateVar::GetSetting () { return _setting; }
inline ButtonSharedName* ButtonStateVar::GetButtonSharedName () {
    return _bsharedname;
}
inline const char* ButtonStateVar::GetSubclassName() { 
    return _bsharedname->GetSubclass()->GetName();
}
inline void ButtonStateVar::SetSubclassName(const char* name) { 
    _bsharedname->GetSubclass()->SetName(name);
}
inline boolean ButtonStateVar::IsSubclass () {
    return _bsharedname->GetSubclass()->IsSubclass();
}

class IBShape : public Shape {
public:
    IBShape();
    virtual IBShape* Copy();
    virtual IBShape& operator = (IBShape&);
    int hnat, vnat;
    int hstr, vstr;
    int hshr, vshr;
};

class ShapeVar : public StateVar {
public:
    ShapeVar(IBShape* = nil);
    virtual ~ShapeVar();

    IBShape* GetShape();
    void SetShape(IBShape*);

    virtual StateVar& operator = (StateVar&);
    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    IBShape* _ibshape;
};

inline IBShape* ShapeVar::GetShape () { return _ibshape; }

class TrackNameVar : public IBNameVar {
public:
    TrackNameVar(const char* = nil);

    virtual void GenNewName();
    virtual StateVar* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class FBrowserVar : public StateVar {
public:
    FBrowserVar(const char* dir = nil, const char* textfilter = nil);
    virtual ~FBrowserVar();

    void SetDirName(const char*);
    const char* GetDirName();

    void SetTextFilter(const char*);
    const char* GetTextFilter();
    virtual StateVar& operator = (StateVar&);

    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    char* _dir;
    char* _textfilter;
};

inline const char* FBrowserVar::GetDirName() { return _dir; }
inline const char* FBrowserVar::GetTextFilter() {return _textfilter; }

class BooleanStateVar : public StateVar {
public:
    BooleanStateVar(boolean = false);

    void SetBooleanState(boolean);
    boolean GetBooleanState();

    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
    virtual StateVar& operator = (StateVar&);

private:
    int _bstate;
};

inline void BooleanStateVar::SetBooleanState(boolean bstate) {
    _bstate = bstate;
}
inline boolean BooleanStateVar::GetBooleanState () { return _bstate; }

#endif
