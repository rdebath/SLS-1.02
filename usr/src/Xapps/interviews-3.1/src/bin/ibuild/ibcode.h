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
 * CodeView base class for generating a C++ external representation
 * and subclasses for interactor components.
 */

#ifndef ibcode_h
#define ibcode_h

#include <Unidraw/Components/externview.h>
#include <OS/list.h>

class IComp;
class InteractorComp;
class GraphicComp;
class UList;
class MemberNameVar;


declarePtrList(CharStringList,char)

class StringList {
public:
    StringList();
    ~StringList();

    void Append(const char*);
    boolean Search(const char*);
private:
    CharStringList list;
};

class CodeView : public PreorderView {
public:
    virtual ~CodeView();

    virtual boolean Definition(ostream&);
    virtual void Update();

    InteractorComp* GetIntComp();
    IComp* GetIComp();
    const char* GetErrors();
    void GetClassList(UList*);

    boolean GenDothFile(const char*, ostream&);
    boolean GenDotcFile(const char*, ostream&);
    boolean GenPropFile(const char*, ostream&);
    boolean GenMainFile(const char*, ostream&);
    boolean GenCorehFile(const char*, ostream&);
    boolean GenCorecFile(const char*, ostream&);
    boolean GenCreatorh(const char*, ostream&);
    boolean GenCreatorc(const char*, ostream&);

    virtual ExternView* GetView(Iterator);
    virtual void SetView(ExternView*, Iterator&);

    virtual void First(Iterator&);
    virtual void Last(Iterator&);
    virtual void Next(Iterator&);
    virtual void Prev(Iterator&);
    virtual boolean Done(Iterator);

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    boolean IsUnidraw();
    boolean IsSubUnidraw();
protected:
    CodeView(GraphicComp* = nil);

    boolean Align(Alignment, ostream&);
    boolean Search(MemberNameVar*, InteractorComp*&);
    boolean Scan(ClassId);

    CodeView* CreateCodeView(GraphicComp*);

    void BeginInstantiate(ostream&);
    void EndInstantiate(ostream&);
    void InstanceName(ostream&, const char* = ", ");

    UList* Elem(Iterator);
    CodeView* View(UList*);
    void DeleteView(Iterator&);
    void DeleteViews();
    void CleanUp();

    boolean AllKidsDefined();

    boolean CentralEmitter(ostream&, boolean&, const char*);
    boolean CentralEmitter(CodeView*, ostream&, boolean&, const char*);

    boolean EmitCorehHeader(ostream&);
    boolean EmitExpHeader(ostream&);

    boolean EmitPropertyData(ostream&);
    boolean EmitForwardDecls(ostream&);
    boolean EmitCoreDecls(ostream&);
    boolean EmitCoreInits(ostream&);
    boolean EmitBSDecls(CodeView*, ostream&);
    boolean EmitBSInits(CodeView*, ostream&);
    boolean EmitInstanceDecls(CodeView*, ostream&);
    boolean EmitInstanceInits(CodeView*, ostream&);
    boolean EmitFunctionDecls(CodeView*, ostream&);
    boolean EmitFunctionInits(CodeView*, ostream&);
    boolean EmitClassDecls(ostream&);

    boolean EmitCreatorHeader(ostream&);
    boolean EmitCreatorSubj(ostream&);
    boolean EmitCreatorView(ostream&);

    boolean EmitClassInits(ostream&);
    boolean EmitClassHeaders(ostream&);
    boolean EmitHeaders(ostream&);

    boolean EmitButtonState(ostream&);
    boolean EmitGlue(ostream&);
    boolean EmitMenu(ostream&);
    boolean EmitShaper(ostream&);
    boolean EmitSlider(ostream&);
    boolean Iterate(ostream&);

    boolean CheckToEmitHeader(ostream&, const char*);
    boolean CheckToEmitClassHeader(ostream&, const char*);

    boolean DeclsTemplate(ostream&, const char*, const char*);
    void GetCoreClassName(char*);

    virtual boolean CoreConstDecls(ostream&);
    virtual boolean CoreConstInits(ostream&);
    virtual boolean ConstDecls(ostream&);
    virtual boolean ConstInits(ostream&);

    virtual boolean BSCoreConstDecls(ostream&);
    virtual boolean BSCoreConstInits(ostream&);
    virtual boolean BSConstDecls(ostream&);
    virtual boolean BSConstInits(ostream&);

    virtual boolean CSCoreConstDecls(ostream&);
    virtual boolean CSCoreConstInits(ostream&);
    virtual boolean CSConstDecls(ostream&);
    virtual boolean CSConstInits(ostream&);

    virtual boolean EmitIncludeHeaders(ostream&);

    boolean WriteGraphicDecls(Graphic*, ostream&);
    boolean WriteGraphicInits(Graphic*, ostream&);

    const char* GetFirewall();
protected:
    boolean _lock;
    UList* _views;
    
    static boolean _emitForward;
    static boolean _emitBSDecls;
    static boolean _emitBSInits;
    static boolean _emitExpHeader;
    static boolean _emitCorehHeader;
    static boolean _emitCreatorHeader;
    static boolean _emitCreatorSubj;
    static boolean _emitCreatorView;

    static boolean _emitInstanceDecls;
    static boolean _emitInstanceInits;
    static boolean _emitCoreDecls;
    static boolean _emitCoreInits;
    static boolean _emitProperty;
    static boolean _emitClassDecls;
    static boolean _emitClassInits;
    static boolean _emitClassHeaders;
    static boolean _emitHeaders;

    static boolean _emitFunctionDecls;
    static boolean _emitFunctionInits;
    static boolean _icomplete;
    static boolean _emitExport;
    static boolean _emitMain;
    static boolean _scope;
    static boolean _emitGraphicState;
    static boolean _emitGraphicComp;
    
    static UList* _instancelist;
    static StringList* _functionlist;
    static StringList* _bsdeclslist;
    static StringList* _bsinitslist;
    static StringList* _namelist;
    static StringList* _globallist;
    
    static StringList* _brushlist;
    static StringList* _colorlist;
    static StringList* _fontlist;
    static StringList* _patternlist;

    static const char* _classname;
    static boolean _unidraw;
    static boolean _subunidraw;

    static char _errbuf[CHARBUFSIZE*10];
    static int _err_count;
};

inline boolean CodeView::IsUnidraw () { return _unidraw; }
inline boolean CodeView::IsSubUnidraw () { return _subunidraw; }

class RootCodeView : public CodeView {
public:
    RootCodeView(GraphicComp* = nil);
    virtual ~RootCodeView();
};

class GraphicCodeView : public CodeView {
public:
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);

    virtual boolean Definition(ostream&);

    virtual void Update();
    IComp* GetIComp();
protected:
    virtual boolean CCoreConstDecls(ostream&);
    virtual boolean CCoreConstInits(ostream&);
    virtual boolean CConstDecls(ostream&);
    virtual boolean CConstInits(ostream&);

    virtual boolean VCoreConstDecls(ostream&);
    virtual boolean VCoreConstInits(ostream&);
    virtual boolean VConstDecls(ostream&);
    virtual boolean VConstInits(ostream&);

    virtual boolean GCoreConstDecls(ostream&);
    virtual boolean GCoreConstInits(ostream&);
    virtual boolean GConstDecls(ostream&);
    virtual boolean GConstInits(ostream&);

    virtual const char* GetGHeader();
    virtual const char* GetCVHeader();
    virtual boolean EmitIncludeHeaders(ostream&);

    GraphicCodeView(IComp* = nil);
};

#endif

