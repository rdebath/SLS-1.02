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
 * Views of user interface builder-specific state variables.
 */

#ifndef ibvarviews_h
#define ibvarviews_h

#include <Unidraw/stateviews.h>
#include <InterViews/matcheditor.h>

class ButtonState;
class BooleanStateVar;
class BSDialog;
class Deck;
class Displayer;
class IBEditor;
class IComp;
class IDMatchEditor;
class CanvasVar;
class MatchEditor;
class MemberNameVar;
class Message;
class MonoShapeVar;
class GraphicComp;
class SubclassNameVar;
class SubclassNameVarView;
class SMemberDialog;
class ShapeVar;
class UList;

class IBVarView : public StateVarView {
public:
    virtual boolean ChangedSubject(const char*& errors);
    virtual void SetAffectedStates(UList*);
    GraphicComp* GetGraphicComp ();
protected:
    IBVarView(StateVar*, GraphicComp* = nil);
protected:
    GraphicComp* _icomp;
};

inline GraphicComp* IBVarView::GetGraphicComp () { return _icomp; }

class IDVarView : public IBVarView {
public:
    IDVarView(
        IDVar*, ButtonState*, const char*
    );
    void SetSubclassNameVarView(SubclassNameVarView*);
    void DMessage(int);
    void ShowStred(boolean);

    virtual boolean ChangedSubject(const char*& errors);
    void IDUpdate();
    void IDUpdate(const char* subclass, const char* baseclass);
protected:
    virtual void Init();
protected:
    IDMatchEditor* _ided;
    Displayer* _displayer;
    Deck* _iddeck;
    SubclassNameVarView* _subvarview;
    boolean _tochange;
private:
    Interactor* Interior(ButtonState*, const char*);
};

inline void IDVarView::SetSubclassNameVarView(SubclassNameVarView* s) {
    _subvarview = s;
}

class CanvasVarView : public IBVarView {
public:
    CanvasVarView(CanvasVar*);
protected:
    virtual void Init();
};

class ButtonStateVarView : public IBVarView {
public:
    ButtonStateVarView(
        ButtonStateVar*, ButtonState*, GraphicComp*, IBEditor*
    );
    virtual ~ButtonStateVarView();
    virtual boolean ChangedSubject(const char*& errors);
    virtual void SetAffectedStates(UList*);
    MatchEditor* GetBSEditor();
    IBEditor* GetIBEditor();
    ButtonSharedName* GetBSName();
    void SetNameChange(boolean);
    boolean GetNameChange();
protected:
    virtual void Init();
protected:
    MatchEditor* _bs, *_setting;
    boolean _showsetting;
    BSDialog* _bsdialog;
    IBEditor* _ibed;
    ButtonSharedName* _bsname;
    boolean _namechange;
private:
    Interactor* Interior(ButtonState*);
};

inline MatchEditor* ButtonStateVarView::GetBSEditor() { return _bs; }
inline IBEditor* ButtonStateVarView::GetIBEditor() { return _ibed; }
inline ButtonSharedName* ButtonStateVarView::GetBSName() { return _bsname; }
inline void ButtonStateVarView::SetNameChange(boolean namechange) {
    _namechange = namechange; 
}
inline boolean ButtonStateVarView::GetNameChange() { return _namechange; }

class CtrlStateVarView : public ButtonStateVarView {
public:
    CtrlStateVarView(
        ButtonStateVar*, ButtonState*, GraphicComp*, IBEditor*
    );
private:
    Interactor* Interior(ButtonState*);
};

class SMemberNameVarView : public IBVarView {
public:
    SMemberNameVarView(
        MemberNameVar*, ButtonState*, GraphicComp*, IBEditor*, 
        const char*, const int* = nil
    );
    SMemberNameVarView(
        IComp*, ButtonState*, GraphicComp*, IBEditor*, 
        const char*
    );
    virtual ~SMemberNameVarView();
    virtual boolean ChangedSubject(const char*& errors);
    virtual void SetAffectedStates(UList*);

    MatchEditor* GetSMemberEditor();
    IBEditor* GetIBEditor();

    MemberSharedName* GetSMemberName();
    void SetNameChange(boolean);
    boolean GetNameChange();
    void Append(const char*);
protected:
    virtual void Init();
protected:
    MatchEditor* _smember;
    SMemberDialog* _smemberdialog;
    IBEditor* _ibed;
    MemberSharedName* _msnamer;
    IComp* _scomp, *_subject;
    boolean _namechange;
private:
    Interactor* Interior(ButtonState*, const char*);
};

inline MatchEditor* SMemberNameVarView::GetSMemberEditor() { return _smember; }
inline IBEditor* SMemberNameVarView::GetIBEditor() { return _ibed; }
inline MemberSharedName* SMemberNameVarView::GetSMemberName() {
    return _msnamer; 
}
inline void SMemberNameVarView::SetNameChange(boolean namechange) {
    _namechange = namechange; 
}
inline boolean SMemberNameVarView::GetNameChange() { return _namechange; }
inline void SMemberNameVarView::Append(const char* n) {
    _smemberdialog->Append(n);
}

class InstanceNameVarView : public IBVarView {
public:
    InstanceNameVarView(
        InstanceNameVar*, ButtonState*, GraphicComp*, 
        const char* = "Instance Name: "
    );
    virtual boolean ChangedSubject(const char*& errors);
protected:
    virtual void Init();
protected:
    MatchEditor* _name;
private:
    Interactor* Interior(ButtonState*, const char*);
};

class MemberNameVarView : public IBVarView {
public:
    MemberNameVarView(
        MemberNameVar*, ButtonState*, GraphicComp*,
        const char* = "Member Name: ", boolean = true
    );

    virtual ~MemberNameVarView();
    virtual boolean ChangedSubject(const char*& errors);
    virtual void SetAffectedStates(UList*);
protected:
    virtual void Init();
protected:
    MatchEditor* _name;
    ButtonState* _export;
private:
    Interactor* Interior(ButtonState*);
private:
    char* _msg;
    boolean _show_exp;
};

class TrackNameVarView : public IBVarView {
public:
    TrackNameVarView(TrackNameVar*, ButtonState*, GraphicComp*);

    virtual boolean ChangedSubject(const char*& errors);
protected:
    virtual void Init();
private:
    Interactor* Interior(ButtonState*);
private:
    MatchEditor* _name;
};

class ShapeVarView : public IBVarView {
public:
    ShapeVarView(ShapeVar*, ButtonState* = nil);

    virtual boolean ChangedSubject(const char*& errors);
protected:
    virtual void Init();
    virtual Interactor* Interior(ButtonState*);
    virtual Interactor* Interior(
        Interactor*, Interactor*, Interactor*, 
        Interactor*, Interactor*, Interactor*
    );

    virtual void GetMessages(Message*&, Message*&, Message*&);
protected:
    MatchEditor* _width, *_height;
    MatchEditor* _hshrink, *_vshrink;
    MatchEditor* _hstretch, *_vstretch;
};

class MarginVarView : public ShapeVarView {
public:
    MarginVarView(ShapeVar*, ButtonState*);
protected:
    virtual void GetMessages(Message*&, Message*&, Message*&);
};

class RelatedVarView : public IBVarView {
public:
    RelatedVarView(
        MemberNameVar*, ButtonState*, GraphicComp*, 
        const char* = "Member Adjusted: "
    );

    virtual ~RelatedVarView();
    virtual boolean ChangedSubject(const char*& errors);
protected:
    virtual void Init();
private:
    Interactor* Interior(ButtonState*);
private:
    MatchEditor* _name;
    char* _msg;
};

class SubMatchEditor : public MatchEditor {
public:
    SubMatchEditor(
        SubclassNameVarView*, ButtonState*, const char*, 
        const char* done = SEDone
    );
    SubMatchEditor(
        SMemberDialog*, ButtonState*, const char*, 
        const char* done = SEDone
    );
protected:
    virtual boolean HandleChar(char);
private:
    SMemberDialog* _d;
    SubclassNameVarView* _s;
};

class SubclassNameVarView : public IBVarView {
public:
    SubclassNameVarView(
        SubclassNameVar*, ButtonState*, GraphicComp*, const char* = nil
    );

    void SetIDVarView(IDVarView*);
    boolean IsSubclass();
    const char* GetSubclassName();

    void IDUpdate ();
    virtual boolean ChangedSubject(const char*& errors);
protected:
    virtual void Init();
protected:
    MatchEditor* _subclass;
    IDVarView* _idvarview;
private:
    Interactor* Interior(ButtonState*, const char*);
    boolean _resolve;
};

class ICompNameVarView: public SubclassNameVarView {
public:
    ICompNameVarView(
        SubclassNameVar*, ButtonState*, GraphicComp*, const char* = nil
    );
    void SetViewNameVarView(SubclassNameVarView*);
    void SetGraphicNameVarView(SubclassNameVarView*);
    virtual boolean ChangedSubject(const char*& errors);
protected:
    SubclassNameVarView* _view;
    SubclassNameVarView* _graphic;
};

inline void ICompNameVarView::SetViewNameVarView(SubclassNameVarView* v) {
    _view = v;
}
inline void ICompNameVarView::SetGraphicNameVarView(SubclassNameVarView* g) {
    _graphic = g;
}

class BooleanStateVarView : public IBVarView {
public:
    BooleanStateVarView(BooleanStateVar*, const char*);
    virtual ~BooleanStateVarView();
    virtual boolean ChangedSubject(const char*& errors);
private:
    Interactor* Interior();
private:
    ButtonState* _bstate;
    char* _message;
};

class FBrowserVarView : public IBVarView {
public:
    FBrowserVarView(FBrowserVar*, ButtonState*);
    virtual ~FBrowserVarView();
    virtual boolean ChangedSubject(const char*& errors);
protected:
    virtual void Init();
private:
    Interactor* Interior(ButtonState*);
private:
    MatchEditor* _dir;
    MatchEditor* _textfilter;
};

#endif
