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
 * Implementation of user interface builder-specific state variable views.
 */

#include "ibadjuster.h"
#include "ibbutton.h"
#include "ibclasses.h"
#include "ibcmds.h"
#include "ibcomp.h"
#include "ibdialogs.h"
#include "ibed.h"
#include "ibmenu.h"
#include "ibgrblock.h"
#include "ibvars.h"
#include "ibvarviews.h"

#include <Unidraw/iterator.h>
#include <Unidraw/ulist.h>

#include <InterViews/box.h>
#include <InterViews/border.h>
#include <InterViews/button.h>
#include <InterViews/deck.h>
#include <InterViews/frame.h>
#include <InterViews/glue.h>
#include <InterViews/message.h>
#include <InterViews/matcheditor.h>
#include <InterViews/scene.h>
#include <InterViews/scroller.h>
#include <InterViews/scrollbar.h>
#include <InterViews/strbrowser.h>
#include <InterViews/strchooser.h>
#include <InterViews/tray.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*****************************************************************************/

static char buf[CHARBUFSIZE];

/*****************************************************************************/

class Shaper : public MonoScene {
public:
    Shaper(int, int, int, int);
    void Reconfig();
private:
    int _hstr, _vstr;
    int _hshr, _vshr;
};

Shaper::Shaper (int hstr, int vstr, int hshr, int vshr) {
    _hstr = hstr, _vstr = vstr;
    _hshr = hshr, _vshr = vshr;
}

void Shaper::Reconfig () {
    MonoScene::Reconfig();
    shape->hstretch = _hstr;
    shape->vstretch = _vstr;
    shape->hshrink = _hshr;
    shape->vshrink = _vshr;
}

static const char* IntToString (int i) {
    sprintf(buf, "%d", i);
    return strnew(buf);
}

static Message* IntToMsg (int i) {
    sprintf(buf, "%d", i);
    return new Message(buf);
}

static Interactor* FixedLength(Interactor* i, const char* len) {
    Shaper* shaper = new Shaper(hfil, vfil, hfil, vfil);
    shaper->Insert(i);
    Deck* deck = new Deck;
    deck->Insert(shaper);
    deck->Insert(new Message(len));
    return deck;
}

inline Interactor* PaddedFrame (Interactor* i) {
    return new Frame(new MarginFrame(i, 2));
}

inline Interactor* RightJustified (Interactor* i) {
    return new HBox(new HGlue(0, 0, 500), i);
}

inline Interactor* LeftJustified (Interactor* i) {
    return new HBox(i, new HGlue);
}

/*****************************************************************************/

class Displayer : public Message {
public:
    Displayer(const char* msg);
    const char* Text();
    void DMessage(const char* msg);
};

Displayer::Displayer (const char* msg) : Message(msg) {}

void Displayer::DMessage (const char* msg) {
    delete text;
    text = nil;
    if (msg != nil) {
        text = strnew(msg);
        if (canvas != nil) {
            Reconfig();
            Scene* parent = Parent();
            if (parent != nil) {
                parent->Change(this);
            }
        }
    }
}

const char* Displayer::Text () {
    return text;
}

/*****************************************************************************/

IBVarView::IBVarView (
    StateVar* stateVar, GraphicComp* icomp
) : StateVarView(stateVar) {
    _icomp = icomp;
}

boolean IBVarView::ChangedSubject(const char*& errors) {
    errors = nil;
    return true;
}

void IBVarView::SetAffectedStates(UList* ilist) {
    StateVar* subject = GetSubject();
    UList* target = ilist->Find(subject);
    if (target == nil) {
        ilist->Append(new UList(subject));
    }
}

/*****************************************************************************/
class IDMatchEditor : public MatchEditor {
public:
    IDMatchEditor(ButtonState*, const char* sample);
    boolean Touched();
protected:
    virtual boolean HandleChar(char);
private:
    boolean _touched;
};

inline boolean IDMatchEditor::Touched () { return _touched; }

IDMatchEditor::IDMatchEditor(
    ButtonState* bs, const char* sample
) : MatchEditor(bs, sample) {
    _touched = false;
}

boolean IDMatchEditor::HandleChar (char a) {
    boolean ok = MatchEditor::HandleChar(a);
    _touched = true;
    return ok;
}

/*****************************************************************************/

IDVarView::IDVarView (
    IDVar* idvar, ButtonState* bs, const char* msg
) : IBVarView(idvar) {
    _subvarview = nil;
    _tochange = false;
    Insert(Interior(bs, msg));
}

boolean IDVarView::ChangedSubject (const char*& errors) {
    errors = nil;
    const char* id = _ided->Text(); 
    const char* origid = _displayer->Text();
    
    IDVar* idvar = (IDVar*) GetSubject();
    
    if (_tochange) {
        if (strcmp(origid, "Invalid") != 0) {
            idvar->SetOrigID(atoi(origid));
        }
        if (strcmp(idvar->GetName(), id) != 0) {
            idvar->SetName(id);
            idvar->SetMachGen(false);
            idvar->Update();
            int& serial = idvar->GetSerial();
            serial = max(serial, atoi(id));
        }
    } else {
        if (strcmp(origid, "Invalid") == 0) {
            sprintf(buf, "Class id %s has been used by a different class", id);
            errors = buf;
        }
    }
    return errors == nil;
}

void IDVarView::IDUpdate () {
    if (_subvarview != nil) {
        if (_subvarview->IsSubclass()) {
            SubclassNameVar* subclass = (SubclassNameVar*) 
                _subvarview->GetSubject();
            IDUpdate(_subvarview->GetSubclassName(), subclass->GetBaseClass());
        } else {
            ShowStred(false);
        }
    }
}

void IDVarView::IDUpdate (const char* subclass, const char* baseclass) {
    IDVar* idvar = (IDVar*) GetSubject();
    IDMap* idmap = idvar->GetIDMap();
    int ok = idmap->FindID(subclass, baseclass);
    if (ok == 0) {
        ShowStred(true);
        
    } else if (ok < 0) {
        DMessage(ok);
        ShowStred(false);

    } else if (!_ided->Touched()) {
        char buf[CHARBUFSIZE];
        sprintf(buf, "%d", ok);
        _ided->Message(buf);
        ShowStred(true);
    }
}

void IDVarView::DMessage (int id) {
    if (id > 0) {
        char msg[CHARBUFSIZE];
        sprintf(msg, "%d", id);
        const char* text = _displayer->Text();
        if (strcmp(text, msg) != 0) {
            _displayer->DMessage(msg);
        }
    } else {
        const char* text = _displayer->Text();
        if (strcmp(text, "Invalid") != 0) {
            _displayer->DMessage("Invalid");
        }
    }
}

void IDVarView::ShowStred (boolean flag) {
    if (flag) {
        if (strcmp(_ided->Text(), "-1") == 0) {
            IDVar* idvar = (IDVar*) GetSubject();
            char msg[CHARBUFSIZE];
            sprintf(msg, "%d", idvar->GetSerial()+1);
            _ided->Message(msg);
        }
        _iddeck->FlipTo(1);
        _tochange = true;
    } else {
        _iddeck->FlipTo(2);
        _tochange = false;
    }
}

void IDVarView::Init () { 
    IDVar* idvar = (IDVar*) GetSubject();
    const char* id = idvar->GetName();
    if( id != nil) {
	_ided->Message(id);
    }
    IDUpdate();
}

Interactor* IDVarView::Interior (ButtonState* bs, const char* msg) {
    const int gap = round(.1*cm);
    IDVar* idvar = (IDVar*) GetSubject();
    int id = idvar->GetOrigID();
    char origid[CHARBUFSIZE];
    char classid[CHARBUFSIZE];

    sprintf(origid, "%d", id);
    sprintf(classid, "%s Class ID: ", msg);

    _iddeck = new Deck;
    _iddeck->Insert(
        new HBox(
            PaddedFrame(_ided = new IDMatchEditor(bs, "99999")),
            new HGlue(0, 10*hfil, 10*hfil)
        )
    );
    _iddeck->Insert(
        new HBox(
            new HGlue(3, 0, 0),
            _displayer = new Displayer(origid),
            new HGlue(20)
        )
    );
    _iddeck->Propagate(false);

    HBox* hbox = new HBox(
        new Message(classid),
        _iddeck,
        new HGlue(0, hfil*100, hfil*100)
    );
    hbox->Align(Center);
    _ided->Match("%[0-9]", true);
    return hbox;
}

/*****************************************************************************/

CanvasVarView::CanvasVarView (CanvasVar* canvasVar) : IBVarView(canvasVar){}

void CanvasVarView::Init () {
    CanvasVar* cvar = (CanvasVar*) _subject;
    const char* string = "Canvas Dimensions (width x height): %d x %d";

    sprintf(buf, string, cvar->Width(), cvar->Height());
    Insert(new HBox(new Message(buf, Left), new HGlue));
}

/*****************************************************************************/

class InfoButtonState : public ButtonState {
public:
    InfoButtonState(
        int, BSDialog* = nil, IBEditor* = nil, IComp* = nil,StringEditor* = nil
    );
    virtual void Notify ();
private:
    BSDialog* _bsdialog;
    IBEditor* _ed;
    IComp* _icomp;
    StringEditor* _istred;
};

InfoButtonState::InfoButtonState (
    int i, BSDialog* bsdialog, IBEditor* ed, IComp* icomp, StringEditor* istred
) : ButtonState(i) {
    _bsdialog = bsdialog;
    _icomp = icomp;
    _ed = ed;
    _istred = istred;
}

void InfoButtonState::Notify () {
    ButtonState::Notify();
    int value;
    GetValue(value);
    if (value == 1) {
        if (_bsdialog != nil) {
            if (_bsdialog->Init()) {
                _ed->InsertDialog(_bsdialog);
                while(_bsdialog->Accept() && !_bsdialog->ChangeBS());
                _ed->RemoveDialog(_bsdialog);
            }
        } else {
            MemberNameVar* mnamer = _icomp->GetMemberNameVar();
            mnamer->GetMemberSharedName()->SetName(_istred->Text());
            IView* iview = (IView*) _icomp->Create(COMPONENT_VIEW);
            _icomp->Attach(iview);
            iview->Update();
            
            InfoCmd* infocmd = new InfoCmd(_ed, iview);
            infocmd->Execute();
            if (infocmd->Reversible()) {
                infocmd->Log();
            } else {
                delete infocmd;
            }
            delete iview;
            _istred->Message(mnamer->GetMemberSharedName()->GetName());
        }
        SetValue(0);
    }
}       

/*****************************************************************************/
        
SMemberNameVarView::SMemberNameVarView (
    MemberNameVar* mvar, ButtonState* bs, GraphicComp* icomp, IBEditor* ibed,
    const char* str, const int* i
) : IBVarView(mvar, icomp) {
    _ibed = ibed;
    _namechange = false;
    _msnamer = (MemberSharedName*) mvar->GetMemberSharedName()->Copy();
    _msnamer->ref();
    _subject = nil;
    _scomp = nil;
    _smemberdialog = new SMemberDialog(this, str, i);
    Insert(Interior(bs, str));
}

SMemberNameVarView::SMemberNameVarView (
    IComp* scomp, ButtonState* bs, GraphicComp* icomp, IBEditor* ibed,
    const char* str
) : IBVarView(scomp->GetMemberNameVar(), icomp) {
    _ibed = ibed;
    _namechange = false;
    _subject = scomp;
    _scomp = (IComp*) scomp->Copy();
    *(IComp*)_scomp = *(IComp*)_subject;
    _icomp->GetGraphic()->Append(_scomp->GetGraphic());    /* another magic */
    _msnamer = nil;
    _smemberdialog = nil;
    Insert(Interior(bs, str));
}

SMemberNameVarView::~SMemberNameVarView () {
    delete _smemberdialog;
    if (_msnamer != nil) {
        _msnamer->unref();
    }
    if (_scomp != nil) {
        _icomp->GetGraphic()->Remove(_scomp->GetGraphic()); /* another magic */
        delete _scomp;
    }
}

static MemberSharedName* FindFirstMSConflict(UList* ulist, const char* name) {
    MemberSharedName* mname = nil;
    for (UList* i = ulist->First(); i != ulist->End(); i = i->Next()) {
        StateVar* state = (StateVar*) (*i)();
        if (state->IsA(MEMBERSHAREDNAME)) {
            MemberSharedName* mtmp = (MemberSharedName*) state;
            if (strcmp(mtmp->GetName(), name) == 0) {
                mname = (MemberSharedName*) state;
                break;
            }
        }
    }
    return mname;
}

void SMemberNameVarView::SetAffectedStates(UList* ilist) {
    if (_scomp != nil) {
        Iterator i;
        IView* iview = (IView*) _scomp->Create(COMPONENT_VIEW);
        _scomp->Attach(iview);
        iview->Update();
        InfoDialog* infodialog = iview->GetInfoDialog();
        for (infodialog->First(i); !infodialog->Done(i); infodialog->Next(i)) {
            infodialog->GetStateView(i)->SetAffectedStates(ilist);
        }
        delete infodialog;
        delete iview;
    } else {
        IBVarView::SetAffectedStates(ilist);
        MemberNameVar* mnamer = (MemberNameVar*) GetSubject();
        MemberSharedName* msnamer = mnamer->GetMemberSharedName();
        ilist->Append(new UList(msnamer));
        ilist->Append(new UList(msnamer->GetSubclass()));
    } 
}

boolean SMemberNameVarView::ChangedSubject (const char*& errors) {
    errors = nil;
    if (*_smember->Text() != '\0') {
        MemberNameVar* mnamer = (MemberNameVar*) GetSubject();
        MemberSharedName* msnamer = mnamer->GetMemberSharedName();

        GetFirewallCmd firewallCmd(_icomp);
        firewallCmd.Execute();
        GetConflictCmd conflictCmd(firewallCmd.GetFirewall(),_smember->Text());
        conflictCmd.Execute();
        UList* ulist = conflictCmd.GetConflict();
        for (UList* i = ulist->First(); i != ulist->End(); i = i->Next()) {
	    StateVar* state = (StateVar*) (*i)();
            if (!state->IsA(INSTANCENAME_VAR) && state != msnamer) {
		sprintf(
		    buf,"Member name %s has been used", _smember->Text()
		);
        	errors = buf;
		return false;
            }
	}
        if (_scomp != nil) {
            if (strcmp(_smember->Text(), msnamer->GetName()) != 0) {
                msnamer->SetName(_smember->Text());
                msnamer->SetMachGen(false);
            }
            *(IComp*)_subject = *(IComp*)_scomp;
            return true;

        } else if (strcmp(_msnamer->GetName(), msnamer->GetName()) == 0) {
            msnamer->SetExport(_msnamer->GetExport());
            msnamer->SetSubclass(_msnamer->GetSubclass());
            msnamer->SetIDVar(_msnamer->GetIDVar());
            if (strcmp(_smember->Text(), msnamer->GetName()) != 0) {
                msnamer = FindFirstMSConflict(ulist, _smember->Text());
                if (msnamer != nil) {
                    mnamer->SetMemberSharedName(msnamer);
                } else {
                    msnamer = new MemberSharedName(
                        _smember->Text(), false , false
                    );
                    msnamer->SetSubclass(_msnamer->GetSubclass());
                    msnamer->SetIDVar(_msnamer->GetIDVar());
                    mnamer->SetMemberSharedName(msnamer);
                }
            }
	} else {
            if (strcmp(_smember->Text(), _msnamer->GetName()) != 0) {
                if (_namechange) {
                    *msnamer = *_msnamer;
                    msnamer->SetMachGen(false);
                }
                msnamer = FindFirstMSConflict(ulist, _smember->Text());
                if (msnamer != nil) {
                    mnamer->SetMemberSharedName(msnamer);
                } else {
                    msnamer = new MemberSharedName(
                        _smember->Text(), false, false
                    );
                    msnamer->SetSubclass(_msnamer->GetSubclass());
                    msnamer->SetIDVar(_msnamer->GetIDVar());
                    mnamer->SetMemberSharedName(msnamer);
                }
            } else {
                if (_namechange) {
                    *msnamer = *_msnamer;
                    msnamer->SetMachGen(false);
                } else {
                    msnamer = FindFirstMSConflict(ulist, _smember->Text());
                    if (msnamer != nil) {
                        mnamer->SetMemberSharedName(msnamer);
                    } else {
                        msnamer = new MemberSharedName(
                            _smember->Text(), false, false
                        );
                        mnamer->SetMemberSharedName(msnamer);
                    }
                    *msnamer = *_msnamer;
                    msnamer->SetMachGen(false);
                }
            }
        }
    
    } else {
	sprintf(buf, "Null Member name is invalid");
        errors = buf;
    }
    return errors == nil;
}

void SMemberNameVarView::Init () {
    MemberNameVar* mnamer;
    if (_scomp == nil) {
        mnamer = (MemberNameVar*) GetSubject();
    } else {
        mnamer = _scomp->GetMemberNameVar();
    }
    MemberSharedName* msnamer = mnamer->GetMemberSharedName();
    _smember->Message(msnamer->GetName());
}

Interactor* SMemberNameVarView::Interior (
    ButtonState* bs, const char* str
) {
    const char* sample = "999";
    const int gap = round(.1*cm);
    char colon[CHARBUFSIZE];
    char dotdotdot[CHARBUFSIZE];
    sprintf(colon, "%s: ", str);
    sprintf(dotdotdot, "%s...", str);
        
    _smember = new MatchEditor(bs, "a rather long name");
    _smember->Match("%[_a-zA-Z]%[_a-zA-Z0-9_]", true);

    Message* nameMsg = new Message(colon);

    PushButton* pbutton = new PushButton(
        dotdotdot, new InfoButtonState(0, _smemberdialog, _ibed, _scomp, 
        _smember),1
    );
    Interactor* bsname= PaddedFrame(_smember);

    HBox* namer = new HBox(
        bsname,
        new HGlue(2*gap, 0, 0),
        FixedLength(pbutton, "MMMMMMMMMM")
    );
    namer->Align(Center);

    Tray* t = new Tray;
    t->HBox(t, nameMsg, namer, t);
    t->VBox(t, new VGlue(gap), nameMsg, new VGlue(gap), t);

    return t;
}

/*****************************************************************************/
        
ButtonStateVarView::ButtonStateVarView (
    ButtonStateVar* bsv, ButtonState* bs, GraphicComp* icomp, IBEditor* ibed
) : IBVarView(bsv, icomp) {
    _showsetting = bsv->DisplaySetting();
    _bsdialog = new BSDialog(this);
    _ibed = ibed;
    _namechange = false;
    _bsname = (ButtonSharedName*) bsv->GetButtonSharedName()->Copy();
    Insert(Interior(bs));
}

ButtonStateVarView::~ButtonStateVarView () {
    delete _bsdialog;
    delete _bsname;
}

static ButtonSharedName* FindFirstBSConflict(UList* ulist, const char* name) {
    ButtonSharedName* bsname = nil;
    for (UList* i = ulist->First(); i != ulist->End(); i = i->Next()) {
        StateVar* state = (StateVar*) (*i)();
        if (state->IsA(BUTTONSHAREDNAME)) {
            ButtonSharedName* bstmp = (ButtonSharedName*) state;
            if (strcmp(bstmp->GetName(), name) == 0) {
                bsname = (ButtonSharedName*) state;
                break;
            }
        }
    }
    return bsname;
}

void ButtonStateVarView::SetAffectedStates(UList* ilist) {
    IBVarView::SetAffectedStates(ilist);
    ButtonStateVar* button = (ButtonStateVar*) GetSubject();
    ButtonSharedName* bsname = button->GetButtonSharedName();
    ilist->Append(new UList(bsname));
}

boolean ButtonStateVarView::ChangedSubject (const char*& errors) {
    errors = nil;

    if (*_bs->Text() != '\0') {
        ButtonStateVar* button = (ButtonStateVar*) GetSubject();
        ButtonSharedName* bsname = button->GetButtonSharedName();
        const char* baseclass = _bsname->GetSubclass()->GetBaseClass();

        GetFirewallCmd firewallCmd(_icomp);
        firewallCmd.Execute();
        GetConflictCmd conflictCmd(firewallCmd.GetFirewall(), _bs->Text());
        conflictCmd.Execute();
        UList* ulist = conflictCmd.GetConflict();
        for (UList* i = ulist->First(); i != ulist->End(); i = i->Next()) {
	    StateVar* state = (StateVar*) (*i)();
	    if (state->IsA(BUTTONSHAREDNAME)) {
                ButtonSharedName* bstmp = (ButtonSharedName*) state;
                if (strcmp(bstmp->GetFuncName(), _bs->Text()) == 0) {
                    sprintf(
                        buf,"ButtonState name %s has been used", 
                        _bs->Text()
                    );
                    errors = buf;
                    return false;
                }
	    } else if (!state->IsA(INSTANCENAME_VAR)) {
		sprintf(
		    buf,"ButtonState name %s has been used", _bs->Text()
		);
        	errors = buf;
		return false;
            }
	}
        if (_showsetting && _setting->Text() != '\0') {
            int setting = atoi(_setting->Text());
            button->SetSetting(setting);
        }
        if (strcmp(_bsname->GetName(), bsname->GetName()) == 0) {
            bsname->SetInitial(_bsname->GetInitial());
            bsname->SetExport(_bsname->GetExport());
            bsname->SetFuncName(_bsname->GetFuncName());
            bsname->SetSubclass(_bsname->GetSubclass());
            if (strcmp(_bs->Text(), bsname->GetName()) != 0) {
                bsname = FindFirstBSConflict(ulist, _bs->Text());
                if (bsname != nil) {
                    button->SetButtonSharedName(bsname);
                } else {
                    bsname = new ButtonSharedName(
                        _bs->Text(), "", false, baseclass
                    );
                    button->SetButtonSharedName(bsname);
                }
            }
	} else {
            if (strcmp(_bs->Text(), _bsname->GetName()) != 0) {
                if (_namechange) {
                    bsname->SetMachGen(false);
                    bsname->SetName(_bsname->GetName());
                    bsname->SetInitial(_bsname->GetInitial());
                    bsname->SetExport(_bsname->GetExport());
                    bsname->SetFuncName(_bsname->GetFuncName());
                    bsname->SetSubclass(_bsname->GetSubclass());
                }
                bsname = FindFirstBSConflict(ulist, _bs->Text());
                if (bsname != nil) {
                    button->SetButtonSharedName(bsname);
                } else {
                    bsname = new ButtonSharedName(
                        _bs->Text(), "", false, baseclass
                    );
                    button->SetButtonSharedName(bsname);
                }
            } else {
                if (_namechange) {
                    bsname->SetMachGen(false);
                    bsname->SetName(_bsname->GetName());
                    bsname->SetInitial(_bsname->GetInitial());
                    bsname->SetExport(_bsname->GetExport());
                    bsname->SetFuncName(_bsname->GetFuncName());
                    bsname->SetSubclass(_bsname->GetSubclass());
                } else {
                    bsname = FindFirstBSConflict(ulist, _bs->Text());
                    if (bsname != nil) {
                        button->SetButtonSharedName(bsname);
                    } else {
                        bsname = new ButtonSharedName(
                            _bs->Text(), "", false, baseclass
                        );
                        button->SetButtonSharedName(bsname);
                    }
                    bsname->SetMachGen(false);
                    bsname->SetInitial(_bsname->GetInitial());
                    bsname->SetExport(_bsname->GetExport());
                    bsname->SetFuncName(_bsname->GetFuncName());
                    bsname->SetSubclass(_bsname->GetSubclass());
                }
            }
        }
    
    } else {
	sprintf(buf, "Null ButtonState name is invalid");
        errors = buf;
    }
    return errors == nil;
}

void ButtonStateVarView::Init () {
    ButtonStateVar* subj = (ButtonStateVar*) GetSubject();
    int setting = subj->GetSetting();
    ButtonSharedName* bsname = subj->GetButtonSharedName();

    if (bsname != nil) { 
        _bs->Message(bsname->GetName());
    }
    if (_showsetting) {
        _setting->Message(IntToString(setting));
    }
}

Interactor* ButtonStateVarView::Interior (ButtonState* bs) {
    const char* sample = "999";
    const int gap = round(.1*cm);

    _bs = new MatchEditor(bs, "a rather long name");
    _bs->Match("%[_a-zA-Z]%[_a-zA-Z0-9_]", true);

    Message* nameMsg = new Message("ButtonState: ");

    PushButton* pbutton = new PushButton(
        "ButtonState...", new InfoButtonState(0, _bsdialog, _ibed), 1
    );
    Interactor* bsname= PaddedFrame(_bs);

    HBox* namer = new HBox(
        bsname,
        new HGlue(2*gap, 0, 0),
        FixedLength(pbutton, "MMMMMMMMMM")
    );
    namer->Align(Center);

    Tray* t = new Tray;

    t->HBox(t, nameMsg, namer, t);
    if (_showsetting) {
        _setting = new MatchEditor(bs, sample);
        _setting->Match("%d", true);

        Interactor* setter = new Frame(new MarginFrame(_setting, 2));
        Message* settingMsg = new Message("Setting Value: ");

        t->HBox(
            t,settingMsg,new HGlue(2*gap,0,0),setter,new HGlue(0,0,100*hfil),t
        );
        t->Align(VertCenter, settingMsg, setter);
	t->Align(Left, nameMsg, settingMsg);
	t->Align(Left, namer, setter);
	t->VBox(t, namer, new VGlue(gap), setter, new VGlue(gap), t);
	t->VBox(
            t, new VGlue(gap), nameMsg, new VGlue(gap), 
            settingMsg, new VGlue(gap), t
        );
    } else {
        t->VBox(t, new VGlue(gap), nameMsg, new VGlue(gap), t);
    }

    return t;
}

/*****************************************************************************/

CtrlStateVarView::CtrlStateVarView (
    ButtonStateVar* bsv, ButtonState* bs, GraphicComp* icomp, IBEditor* ibed
) : ButtonStateVarView(bsv, bs, icomp, ibed) {
    delete _bsdialog;
    _bsdialog = new CtrlDialog(this);
    Insert(Interior(bs));
}

Interactor* CtrlStateVarView::Interior (ButtonState* bs) {
    const char* sample = "999";
    const int gap = round(.1*cm);

    _bs = new MatchEditor(bs, "a rather long name");
    _bs->Match("%[_a-zA-Z]%[_a-zA-Z0-9_]", true);

    Message* nameMsg = new Message("ControlState: ");

    PushButton* pbutton = new PushButton(
        "ControlState...", new InfoButtonState(0, _bsdialog, _ibed), 1
    );
    Interactor* bsname= PaddedFrame(_bs);

    HBox* namer = new HBox(
        bsname,
        new HGlue(2*gap, 0, 0),
        FixedLength(pbutton, "MMMMMMMMMM")
    );
    namer->Align(Center);

    Tray* t = new Tray;

    t->HBox(t, nameMsg, namer, t);
    t->VBox(t, new VGlue(gap), nameMsg, new VGlue(gap), t);

    return t;
}

/*****************************************************************************/

InstanceNameVarView::InstanceNameVarView (
    InstanceNameVar* inv, ButtonState* bs, GraphicComp* icomp, 
    const char* msg
) : IBVarView(inv, icomp) {
    Insert(Interior(bs, msg));
}

boolean InstanceNameVarView::ChangedSubject (const char*&) {
    const char* name = _name->Text(); 
    InstanceNameVar* iname = (InstanceNameVar*) GetSubject();
    
    if (strcmp(iname->GetName(), name) != 0) {
        iname->SetName(name);
        iname->SetMachGen(false);
    }
    return true;
}

void InstanceNameVarView::Init () { 
    InstanceNameVar* iname = (InstanceNameVar*) GetSubject();
    const char* name = iname->GetName();
    if( name != nil) {
	_name->Message(name);
    }
}

Interactor* InstanceNameVarView::Interior (ButtonState* bs, const char* msg) {
    const int gap = round(.1*cm);

    HBox* hbox = new HBox(
        new Message(msg),
        PaddedFrame(_name = new MatchEditor(bs, "a rather long name"))
    );
    hbox->Align(Center);
    _name->Match("%[_a-zA-Z]%[_a-zA-Z0-9_.]", true);
    return hbox;
}

/*****************************************************************************/

MemberNameVarView::MemberNameVarView (
    MemberNameVar* inv, ButtonState* bs, GraphicComp* icomp, const char* msg,
    boolean show_exp
) : IBVarView(inv, icomp) {
    MemberNameVar* iname = (MemberNameVar*) GetSubject();
    _msg = strnew(msg);
    _show_exp = show_exp;
    if (_show_exp) {
        _export = new ButtonState((int) iname->GetExport());
    } else {
        _export = new ButtonState(1);
    }        
    Insert(Interior(bs));
}

MemberNameVarView::~MemberNameVarView () { delete _msg; }

void MemberNameVarView::SetAffectedStates(UList* ilist) {
    IBVarView::SetAffectedStates(ilist);
    MemberNameVar* mnamer = (MemberNameVar*) GetSubject();
    MemberSharedName* msnamer = mnamer->GetMemberSharedName();
    ilist->Append(new UList(msnamer));
}

boolean MemberNameVarView::ChangedSubject (const char*& errors) {
    errors = nil;
    int value;
    const char* name = _name->Text(); 

    _export->GetValue(value);
    MemberNameVar* mnamer = (MemberNameVar*) GetSubject();
    MemberSharedName* msnamer = mnamer->GetMemberSharedName();
    if (name != nil && *name != '\0') {
        
        GetFirewallCmd firewallCmd(_icomp);
        firewallCmd.Execute();
        GetConflictCmd conflictCmd(firewallCmd.GetFirewall(), name);
        conflictCmd.Execute();
        UList* ulist = conflictCmd.GetConflict();
	for(UList* i = ulist->First(); i != ulist->End(); i = i->Next()) {
	    StateVar* state = (StateVar*) (*i)();

	    if (
                !state->IsA(INSTANCENAME_VAR) && state != msnamer
            ) {
                sprintf( 
                    buf,
                    "Member name \"%s\" already exists.",
                    name
                );
                errors = buf;
                return false;
	    }
	}
        msnamer->SetExport(value);
        if (strcmp(msnamer->GetName(), name) != 0) {
            msnamer->SetName(name);
            msnamer->SetMachGen(false);
        }
        errors = nil;

    } else {
	sprintf(buf, "Null member name is invalid");
	errors = buf;
    }
    return errors == nil;
}

void MemberNameVarView::Init () { 
    MemberNameVar* mnamer = (MemberNameVar*) GetSubject();
    const char* name = mnamer->GetName();
    if( name != nil) {
	_name->Message(name);
    }
}

Interactor* MemberNameVarView::Interior (ButtonState* bs) {
    const int gap = round(.1*cm);
    HBox* hbox;

    if (_show_exp) {
        hbox = new HBox(
            new Message(_msg),
            PaddedFrame(_name = new MatchEditor(bs, "a rather long name")),
            new HGlue(2*gap, 0, 0),
            new CheckBox("Export", _export, 1, 0)
        );
    } else {
        hbox = new HBox(
            new Message(_msg),
            PaddedFrame(_name = new MatchEditor(bs, "a rather long name"))
        );
    }
    hbox->Align(Center);

    _name->Match("%[_a-zA-Z]%[_a-zA-Z0-9_]", true);
    return hbox;
}

/*****************************************************************************/

TrackNameVarView::TrackNameVarView (
    TrackNameVar* nameVar, ButtonState* bs, GraphicComp* icomp
) : IBVarView(nameVar, icomp) {
    Insert(Interior(bs));
}

boolean TrackNameVarView::ChangedSubject (const char*& errors) {
    errors = nil;
    const char* name = _name->Text(); 
    TrackNameVar* proc = (TrackNameVar*) GetSubject();

    if (name != nil) {
        GetFirewallCmd firewallCmd(_icomp);
        firewallCmd.Execute();
        GetConflictCmd conflictCmd(firewallCmd.GetFirewall(), name);
        conflictCmd.Execute();
        UList* cl = conflictCmd.GetConflict();
        for (UList* i = cl->First(); i != cl->End(); i = i->Next()) {
            StateVar* state = (StateVar*) (*i)();
	    if (state->IsA(BUTTONSHAREDNAME)) {
                ButtonSharedName* bstmp = (ButtonSharedName*) state;
                if (strcmp(bstmp->GetName(), name) == 0) {
                    sprintf(
                        buf, 
                        "A ButtonState name \"%s\" already exists.",
                        name
                    );
                    errors = buf;
                    return false;
                }
            } else if (
                !state->IsA(PROCNAME_VAR) && !state->IsA(INSTANCENAME_VAR)
            ) {
                sprintf(
                    buf, 
                    "A member or class name \"%s\" already exists.",
                    name
                );
                errors = buf;
                return false;

            }
        }
	proc->SetName(name);
        proc->SetMachGen(false);
    }
    return errors == nil;
}

void TrackNameVarView::Init () { 
    TrackNameVar* proc = (TrackNameVar*) GetSubject();
    const char* name = proc->GetName();
    if( name != nil) {
	_name->Message(name);
    }
}

Interactor* TrackNameVarView::Interior (ButtonState* bs) {
    TrackNameVar* proc = (TrackNameVar*) GetSubject();

    HBox* hbox = new HBox(
        new Message("Member function: "),
        PaddedFrame(_name = new MatchEditor(bs, "a rather long name"))
    );
    hbox->Align(Center);

    _name->Match("%[_a-zA-Z]%[_a-zA-Z0-9_]", true);
    return hbox;
}

/*****************************************************************************/

ShapeVarView::ShapeVarView (ShapeVar* s, ButtonState* bs) : IBVarView(s) {
    if (bs != nil) {
        Insert(Interior(bs));
    }
}

boolean ShapeVarView::ChangedSubject (const char*&) {
    ShapeVar* subj = (ShapeVar*) GetSubject();

    IBShape* subshape = subj->GetShape();

    subshape->width = 
	(subshape->hnat) ? atoi(_width->Text()) : subshape->width;
    subshape->height = 
	(subshape->vnat) ? atoi(_height->Text()) : subshape->height;
    subshape->hshrink = 
	(subshape->hshr) ? atoi(_hshrink->Text()) : subshape->hshrink;
    subshape->vshrink =
	(subshape->vshr) ? atoi(_vshrink->Text()) : subshape->vshrink;
    subshape->hstretch =
	(subshape->hstr) ? atoi(_hstretch->Text()) : subshape->hstretch;
    subshape->vstretch =
	(subshape->vstr) ? atoi(_vstretch->Text()) : subshape->vstretch;

    return true;
}

void ShapeVarView::Init () {
    ShapeVar* subj = (ShapeVar*) GetSubject();
    IBShape* ibshape = subj->GetShape();
    if (ibshape->hnat) {
        _width->Message(IntToString(ibshape->width));
    }
    if (ibshape->vnat) {
        _height->Message(IntToString(ibshape->height));
    }
    if (ibshape->hstr) {
        _hstretch->Message(IntToString(ibshape->hstretch));
    }
    if (ibshape->vstr) {
        _vstretch->Message(IntToString(ibshape->vstretch));
    }
    if (ibshape->hshr) {
        _hshrink->Message(IntToString(ibshape->hshrink));
    }
    if (ibshape->vshr) {
        _vshrink->Message(IntToString(ibshape->vshrink));
    }
}

Interactor* ShapeVarView::Interior (ButtonState* bs) {
    Interactor* interior = nil;

    Interactor* natw, *nath;
    Interactor* hshr, *vshr;
    Interactor* hstr, *vstr;

    const char* sample = "9999999999";
    ShapeVar* subj = (ShapeVar*) GetSubject();
    IBShape* ibshape = subj->GetShape();
    if (ibshape->hnat) {
        _width = new MatchEditor(bs, sample);
	_width->Match("%d", true);
	natw = PaddedFrame(_width);
    } else {
	natw = IntToMsg(ibshape->width);
    }
    if (ibshape->vnat) {
        _height = new MatchEditor(bs, sample);
	_height->Match("%d", true);
	nath = PaddedFrame(_height);
    } else {
	nath = IntToMsg(ibshape->height);
    }
    if (ibshape->hshr) {
        _hshrink = new MatchEditor(bs, sample);
	_hshrink->Match("%d", true);
	hshr = PaddedFrame(_hshrink);
    } else {
	hshr = IntToMsg(ibshape->hshrink);
    }
    if (ibshape->vshr) {
        _vshrink = new MatchEditor(bs, sample);
	_vshrink->Match("%d", true);
	vshr = PaddedFrame(_vshrink);
    } else {
	vshr = IntToMsg(ibshape->vshrink);
    }
    if (ibshape->hstr) {
        _hstretch = new MatchEditor(bs, sample);
	_hstretch->Match("%d", true);
	hstr = PaddedFrame(_hstretch);
    } else {
	hstr = IntToMsg(ibshape->hstretch);
    }
    if (ibshape->vstr) {
        _vstretch = new MatchEditor(bs, sample);
	_vstretch->Match("%d", true);
	vstr = PaddedFrame(_vstretch);
    } else {
	vstr = IntToMsg(ibshape->vstretch);
    }

    return Interior(natw, nath, hshr, vshr, hstr, vstr);
}

Interactor* ShapeVarView::Interior (
    Interactor* natw, Interactor* nath,
    Interactor* hshr, Interactor* vshr,
    Interactor* hstr, Interactor* vstr
) {
    natw = RightJustified(natw);        nath = LeftJustified(nath);	
    hshr = RightJustified(hshr);        vshr = LeftJustified(vshr);
    hstr = RightJustified(hstr);	vstr = LeftJustified(vstr);

    const int gap = round(.1*cm);
    Message* natSize, *shrink, *stretch;
    GetMessages(natSize, shrink, stretch);

    Message* xnat = new Message(" x ");
    Message* xshr = new Message(" x ");
    Message* xstr = new Message(" x ");

    Tray* t = new Tray;

    t->HBox(t, natSize, natw, xnat);   t->HBox(xnat, nath, new HGlue, t);
    t->HBox(t, shrink, hshr, xshr);    t->HBox(xshr, vshr, new HGlue, t);
    t->HBox(t, stretch, hstr, xstr);   t->HBox(xstr, vstr, new HGlue, t);

    t->Align(Left, natw, hshr, hstr);
    t->Align(Right, natw, hshr, hstr);
    
    t->Align(Left, nath, vshr, vstr);
    t->Align(Right, nath, vshr, vstr);

    t->Align(VertCenter, natSize, natw, xnat, nath);
    t->Align(VertCenter, shrink, hshr, xshr, vshr);
    t->Align(VertCenter, stretch, hstr, xstr, vstr);

    t->VBox(t, natw, new VGlue(gap), hshr, new VGlue(gap), hstr, t);
    t->VBox(t, nath, new VGlue(gap), vshr, new VGlue(gap), vstr, t);
    return new HBox(new VBox(t, new VGlue), new HGlue);
}

void ShapeVarView::GetMessages(Message*& m1, Message*& m2, Message*& m3) {
    m1 = new Message("Natural Size (width x height): ");
    m2 = new Message("Shrinkability (horiz x vert): ");
    m3 = new Message("Stretchability (horiz x vert): ");
}

/*****************************************************************************/

MarginVarView::MarginVarView (
    ShapeVar* s, ButtonState* bs
) : ShapeVarView(s, bs) {
    Insert(Interior(bs));
}

void MarginVarView::GetMessages(Message*& m1, Message*& m2, Message*& m3) {
    m1 = new Message("Natural Margins (horiz x vert): ");
    m2 = new Message("Shrinkability (horiz x vert): ");
    m3 = new Message("Stretchability (horiz x vert): ");
}

/*****************************************************************************/

RelatedVarView::RelatedVarView (
    MemberNameVar* sv, ButtonState* bs, GraphicComp* icomp, const char* msg
) : IBVarView(sv, icomp) {
    _msg = strnew(msg);
    Insert(Interior(bs));
}

RelatedVarView::~RelatedVarView () { delete _msg; }

boolean RelatedVarView::ChangedSubject (const char*& errors) {
    errors = nil;
    const char* name = _name->Text();
    if (name != nil && *name != '\0') {

        GetFirewallCmd firewallCmd(_icomp);
        firewallCmd.Execute();
        GetConflictCmd conflictCmd(firewallCmd.GetFirewall(), name);
        conflictCmd.Execute();
        UList* cl = conflictCmd.GetConflict();
        if (cl->IsEmpty()) {
            sprintf(
                buf, "No interactor member named \"%s\" exists.", name
            );
            errors = buf;
    
        } else {
            UList* i = cl->First();
	    StateVar* msnamer = (StateVar*) (*i)(); 
	    if (!msnamer->IsA(MEMBERSHAREDNAME)) {
		sprintf(
                    buf, "\"%s\" is not a defined member name.", name
                );
                errors = buf;

	    } else {
                MemberNameVar* mnamer = (MemberNameVar*) GetSubject();
                InteractorComp* icomp = (InteractorComp*) _icomp;
                InteractorComp* ctarget = conflictCmd.GetCTarget();
                if (ctarget != nil && icomp->IsRelatableTo(ctarget)) {
                    mnamer->SetMemberSharedName((MemberSharedName*) msnamer);

                } else {
                    const char* iname = icomp->GetMemberNameVar()->GetName();
                    sprintf(
                        buf, "\"%s\" is not relatable with \"%s\".",name,iname
                    );
                    errors = buf;
                }
	    }
	}
    } else if (*name == '\0') {
        MemberNameVar* mnamer = (MemberNameVar*) GetSubject();
        mnamer->SetMemberSharedName(new MemberSharedName("", false, false));
    }
    return errors == nil;
}

void RelatedVarView::Init () { 
    MemberNameVar* mnamer = (MemberNameVar*) GetSubject();
    const char* name = mnamer->GetName();
    if( name != nil) {
	_name->Message(name);
    }
}

Interactor* RelatedVarView::Interior (ButtonState* bs) {
    const int gap = round(.1*cm);

    HBox* hbox = new HBox(
        new Message(_msg),
        PaddedFrame(_name = new MatchEditor(bs, "a rather long name"))
    );
    _name->Match("%[_a-zA-Z]%[_a-zA-Z0-9_]", true);
    hbox->Align(Center);
    return hbox;
}

/*****************************************************************************/

SubclassNameVarView::SubclassNameVarView (
    SubclassNameVar* inv, ButtonState* bs, GraphicComp* icomp, const char* msg
) : IBVarView(inv, icomp) {
    _idvarview = nil;
    Insert(Interior(bs, msg));
}

void SubclassNameVarView::IDUpdate () {
    if (_idvarview != nil) {
        _idvarview->IDUpdate();
    }
}

boolean SubclassNameVarView::IsSubclass () {
    boolean flag;
    const char* subclass = _subclass->Text(); 

    SubclassNameVar* subvar = (SubclassNameVar*) GetSubject();
    const char* baseclass = subvar->GetBaseClass();
    if (strcmp(subclass, baseclass) != 0) {
        flag = true;

    } else {
        flag = false;
    }
    return flag;
}

const char* SubclassNameVarView::GetSubclassName () {
    return _subclass->Text();
}

void SubclassNameVarView::SetIDVarView (IDVarView* i) { 
    _idvarview = i;
    _idvarview->SetSubclassNameVarView(this);
}

boolean SubclassNameVarView::ChangedSubject (const char*& errors) {
    errors = nil;
    const char* subclass = _subclass->Text(); 

    SubclassNameVar* subvar = (SubclassNameVar*) GetSubject();
    const char* orig = subvar->GetName();
    const char* baseclass = subvar->GetBaseClass();
    if (strcmp(orig, subclass) != 0) {
        if (*subclass != '\0') {
            if (strcmp(subclass, baseclass) != 0) {
                GetConflictCmd conflictCmd(
                    (GraphicComp*) _icomp->GetRoot(), subclass, true
                );
                conflictCmd.Execute();
                UList* cl = conflictCmd.GetConflict();
                for(UList* i = cl->First(); i != cl->End(); i = i->Next()) {
                    StateVar* state = (StateVar*) (*i)();
                    if (state->IsA(SUBCLASSNAME_VAR) && state != subvar) {
                        SubclassNameVar* svar = (SubclassNameVar*) state;
                        if (strcmp(svar->GetBaseClass(), baseclass) != 0) {
                            sprintf(
                                buf, 
                                "\"%s\" has been used with a different base class.",
                                subclass
                            );
                            errors = buf;
                            return false;
                        }
                    }
                }
                GetFirewallCmd firewallCmd(_icomp);
                firewallCmd.Execute();
                GetConflictCmd sconflict(firewallCmd.GetFirewall(),subclass);
                sconflict.Execute();
                cl = sconflict.GetConflict();
                for(i = cl->First(); i != cl->End(); i = i->Next()) {
                    StateVar* state = (StateVar*) (*i)();
                    if (
                        !state->IsA(INSTANCENAME_VAR) && 
                        !state->IsA(SUBCLASSNAME_VAR) && 
                        state != subvar
                        
                    ) {
                        sprintf(
                            buf, 
                            "A member or procedure name \"%s\" already exists.",
                            subclass
                        );
                        errors = buf;
                        return false;
                    }
                }
            } else if (subvar->IsAbstract()) {
                sprintf(buf, "Base class %s is abstract", subclass);
                errors = buf;
                return false;
            }
            subvar->SetMachGen(false);
            subvar->SetName(subclass);
            errors = nil;

        } else {
            sprintf(buf, "Null subclass name is invalid");
            errors = buf;
        }
    }
    return errors == nil;
}

void SubclassNameVarView::Init () { 
    SubclassNameVar* subvar = (SubclassNameVar*) GetSubject();
    const char* subclass = subvar->GetName();
    if( subclass != nil) {
	_subclass->Message(subclass);
    }
}

SubMatchEditor::SubMatchEditor (
    SMemberDialog* d, ButtonState* bs, const char* sample, 
    const char* done
) : MatchEditor (bs, sample, done) {
    _d = d;
    _s = nil;
}

SubMatchEditor::SubMatchEditor (
    SubclassNameVarView* s, ButtonState* bs, const char* sample, 
    const char* done
) : MatchEditor (bs, sample, done) {
    _s = s;
    _d = nil;
}

boolean SubMatchEditor::HandleChar (char a) {
    boolean flag = MatchEditor::HandleChar(a);
    if (_d != nil) {
        _d->SMemberUpdate();
    } else if (_s != nil) {
        _s->IDUpdate();
    }
    return flag;
}

Interactor* SubclassNameVarView::Interior (ButtonState* bs, const char* msg) {
    const int gap = round(.1*cm);
    SubclassNameVar* subvar = (SubclassNameVar*) GetSubject();
    const char* baseclass = subvar->GetBaseClass();

    char msg1[CHARBUFSIZE];
    char msg2[CHARBUFSIZE];

    if (msg != nil) {
        sprintf(msg1, "%s Class: ", msg);
        sprintf(msg2, "%s Base Class: ", msg);
    } else {
        sprintf(msg1, "Class Name: ");
        sprintf(msg2, "Base Class Name: ");
    }
    HBox* hbox1 = new HBox(
	new Message(msg1),
        PaddedFrame(
	    _subclass = new SubMatchEditor(this, bs, "a rather long name")
	)
    );
    HBox* hbox2 = new HBox(
	new Message(msg2),
	new Message(baseclass),
	new HGlue
    );
    VBox* vbox = new VBox(hbox1, new VGlue(2, 0, 0), hbox2);
    hbox1->Align(Center);
    hbox2->Align(Center);

    _subclass->Match("%[_a-zA-Z]%[_a-zA-Z0-9_]", true);
    return vbox;
}
/*****************************************************************************/

ICompNameVarView::ICompNameVarView (
    SubclassNameVar* inv, ButtonState* bs, GraphicComp* icomp, const char* msg
) : SubclassNameVarView(inv, bs, icomp, msg) {}

boolean ICompNameVarView::ChangedSubject (const char*& errors) {
    errors = nil;
    boolean ok = true;
    CompCheckCmd compcheck(
        (IComp*) _icomp, GetSubclassName(), _view->GetSubclassName(),
        _graphic->GetSubclassName()
    );
    compcheck.Execute();
    if (!compcheck.IsOK()) {
        const char* subclass = _subclass->Text(); 
        sprintf(
            buf, 
            "\"%s\" has been used with a different GraphicView or Graphic subclass.", subclass
        );
        errors = buf;
        ok = false;
    } else {
        ok = SubclassNameVarView::ChangedSubject(errors);
    }
    return ok;
}

/*****************************************************************************/

BooleanStateVarView::BooleanStateVarView (
    BooleanStateVar* bstate, const char* message
) : IBVarView(bstate) {
    _bstate = new ButtonState(bstate->GetBooleanState());
    _message = strnew(message);
    Insert(Interior());
}

BooleanStateVarView::~BooleanStateVarView () {
    delete _message;
}
    
boolean BooleanStateVarView::ChangedSubject (const char*&) {
    int value;
    _bstate->GetValue(value);
    BooleanStateVar* bstate = (BooleanStateVar*) GetSubject();
    bstate->SetBooleanState(value);
    return true;
}

Interactor* BooleanStateVarView::Interior () {
    HBox* hbox = new HBox(
        new CheckBox(_message, _bstate, 1, 0),
        new HGlue
    );
    hbox->Align(Center);
    return hbox;
}

/*****************************************************************************/

FBrowserVarView::FBrowserVarView (
    FBrowserVar* fbVar, ButtonState* bs
) : IBVarView(fbVar) {
    Insert(Interior(bs));
}

FBrowserVarView::~FBrowserVarView () {
    delete _dir;
    delete _textfilter;
}

boolean FBrowserVarView::ChangedSubject (const char*&) {
    const char* dir = _dir->Text(); 
    const char* textfilter = _textfilter->Text(); 
    FBrowserVar* fbVar = (FBrowserVar*) GetSubject();

    fbVar->SetDirName(dir);
    fbVar->SetTextFilter(textfilter);
    return true;
}

void FBrowserVarView::Init () { 
    FBrowserVar* fbVar = (FBrowserVar*) GetSubject();
    _dir->Message(fbVar->GetDirName());
    _textfilter->Message(fbVar->GetTextFilter());
}

Interactor* FBrowserVarView::Interior (ButtonState* bs) {
    const int gap = round(.1*cm);

    FBrowserVar* fbVar = (FBrowserVar*) GetSubject();

    _dir = new MatchEditor(bs, "a rather long name");
    _textfilter = new MatchEditor(bs, "a rather long name");
    Tray* t = new Tray;

    Interactor* dir = PaddedFrame(_dir);
    Interactor* textfilter = PaddedFrame(_textfilter);

    Interactor* mdir = new Message("Directory: ");
    Interactor* mtextfilter = new Message("Text Filter: ");
    
    t->HBox(t, mdir, dir, t);
    t->HBox(t, mtextfilter, textfilter, t);
    t->VBox(
        t, new VGlue(gap),  mdir, new VGlue(gap), mtextfilter, 
        new VGlue(gap), t
    );
    t->VBox(
        t, new VGlue(gap),  dir, new VGlue(gap), textfilter, 
        new VGlue(gap), t
    );
    t->Align(VertCenter, mdir, dir);
    t->Align(VertCenter, mtextfilter, textfilter);
    t->Align(Left, dir, textfilter);
        
    return t;
}
