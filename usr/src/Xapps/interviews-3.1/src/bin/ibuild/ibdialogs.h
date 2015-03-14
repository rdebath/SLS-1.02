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
 * User interface builder-specific dialog boxes.
 */

#ifndef ibdialogs_h
#define ibdialogs_h

#include <Unidraw/dialogs.h>
#include <Unidraw/uarray.h>
#include <InterViews/filechooser.h>

class ButtonStateVarView;
class ButtonSharedName;
class CtrlStateVarView;
class Deck;
class Displayer;
class EditorInfo;
class IDVarView;
class InteractorComp;
class InstanceNameVarView;
class Iterator;
class IBVarView;
class SMemberNameVarView;
class Props;
class PropsEditor;
class UList;
class VBox;

class MoveDialog : public BasicDialog {
public:
    MoveDialog();
    virtual void GetValues(float& x, float& y);

    virtual boolean Accept();
    virtual void Handle(Event&);

    void SelectMessage();
private:
    Interactor* Interior();
    class MatchEditor* _medit;
    ButtonState* _units;
};

class ScaleDialog : public BasicDialog {
public:
    ScaleDialog();
    virtual void GetValues(float& x, float& y);

    virtual boolean Accept();
    virtual void Handle(Event&);

    void SelectMessage();
private:
    Interactor* Interior();
    class MatchEditor* _medit;
};

class RotateDialog : public BasicDialog {
public:
    RotateDialog();
    virtual void GetValue(float& angle);

    virtual boolean Accept();
    virtual void Handle(Event&);

    void SelectMessage();
private:
    Interactor* Interior();
    class MatchEditor* _medit;
};

class InfoDialog : public BasicDialog {
public:
    InfoDialog(const char* = "Interactor Information");
    virtual ~InfoDialog();

    virtual boolean Accept();
    virtual void Handle(Event&);

    void First(Iterator&);
    void Next(Iterator&);
    boolean Done(Iterator);
    IBVarView* GetStateView(Iterator);

    void Include(IBVarView*);
    ButtonState* GetState();
    boolean AppliedChanges(const char*& errors);
    UList* GetAffectedStates();
private:
    Interactor* Interior();
    Interactor* Buttons(int);
private:
    VBox* _body;
    UList* _bodylist;
};

class PropsDialog : public BasicDialog {
public:
    PropsDialog(InteractorComp* icomp);

    virtual boolean Accept();
    virtual void Handle(Event&);
    boolean AppliedChanges(const char*& errors);
private:
    Interactor* Interior();
    Interactor* Buttons(int);
private:
    InstanceNameVarView* _instanceView;
    Props* _props;
    PropsEditor* _propsEd;
};

class BSDialog : public BasicDialog {
public:
    BSDialog(ButtonStateVarView* = nil);
    virtual boolean Accept();
    virtual void Handle(Event&);
    virtual boolean ChangeBS();
    virtual boolean Init();
protected:
    Interactor* Buttons(int);
    boolean NameNotChanged();
protected:
    ButtonStateVarView* _bsvarview;
    MatchEditor* _bs, *_funcname, *_initial, *_subclass;
    ButtonState* _exporter;
private:
    Interactor* Interior();
};

class CtrlDialog : public BSDialog {
public:
    CtrlDialog(CtrlStateVarView*);
private:
    Interactor* Interior();
};

class SMemberDialog : public BSDialog {
public:
    SMemberDialog(SMemberNameVarView*, const char* = nil, const int* = nil);
    virtual ~SMemberDialog();

    void SMemberUpdate();
    virtual boolean Accept();
    virtual boolean ChangeBS();
    virtual boolean Init();
    void Append (const char*);
protected:
    virtual boolean NameNotChanged();
protected:
    SMemberNameVarView* _mvarview;
    MatchEditor* _smember, *_subclass, *_baseclass;
    IDVarView* _idvarview;
    ButtonState* _exporter;
    StringBrowser* _sb;
    const int* _array;
    char* _str;
private:
    Interactor* Interior();
};

class OptionDialog : public BasicDialog {
public:
    OptionDialog(
        const char*, const char*,
        int, const char*, int, const char*, int, const char*
    );
    virtual boolean Accept();
    int Option();
private:
    Interactor* Interior(int, const char*, int, const char*, int, const char*);
private:
    ButtonState* _select;
};

class ConflictDialog : public BasicDialog {
public:
    ConflictDialog(void*);
    virtual ~ConflictDialog();

    void AddConflict(const char* = "", boolean checked = true);

    virtual boolean Accept();
    virtual void Update();

    boolean Checked(const char*);
    boolean Skipped();
    int GetHashId();
    void* GetId();
    
private:
    Interactor* Interior();
    void AddCheckBox(Scene*, int);

    char* conflict(int);
    ButtonState* bs(int);
    void conflict(char*, int);
    void bs(ButtonState*, int);
private:
    void* _id;
    UArray _conflict;
    UArray _bs;
    int _hash_id;
};

inline void* ConflictDialog::GetId () { return _id; }
inline char* ConflictDialog::conflict (int i) { return (char*) _conflict[i]; }
inline ButtonState* ConflictDialog::bs (int i) { return (ButtonState*)_bs[i]; }
inline int ConflictDialog::GetHashId () { return _hash_id; }

class ExeDialog : public FileChooser {
public:
    ExeDialog(const char* dir = "~");
    virtual void Update();
private:
    Interactor* Interior();
};

class InstallRemoveDialog : public BasicDialog {
public:
    InstallRemoveDialog(EditorInfo* orig, int rows = 10, int cols = 24);
    virtual ~InstallRemoveDialog();

    EditorInfo* Installed();
    EditorInfo* Removed();

    virtual boolean Accept();
    virtual void Handle(Event&);
    virtual void Update();
private:
    void Init(EditorInfo*, int, int);
    void InstallSelections();
    void RemoveSelections();

    Interactor* Interior();
    Interactor* AddScroller(Interactor*);
    int Position(StringBrowser*, const char*);
private:
    StringBrowser* _current;
    class FileBrowser* _available;

    ButtonState* _install;
    ButtonState* _remove;
    ButtonState* _clear;

    EditorInfo* _originals;
    EditorInfo* _installed, *_removed;
};

inline EditorInfo* InstallRemoveDialog::Installed () { return _installed; }
inline EditorInfo* InstallRemoveDialog::Removed () { return _removed; }

class AbortDialog : public AcknowledgeDialog {
public:
    AbortDialog(int pid, const char* title);
    boolean Abort();
private:
    Interactor* Interior(const char* title);
private:
    int _pid;
};

class StringBrowserDialog : public AcknowledgeDialog {
public:
    StringBrowserDialog(const char* text);
    virtual ~StringBrowserDialog();
private:
    Interactor* Interior();
    char* _text;
};

#endif

