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
 * Implementation of user interface builder-specific dialog boxes.
 */

#include "ibclasses.h"
#include "ibcmds.h"
#include "ibdialogs.h"
#include "ibed.h"
#include "ibglobals.h"
#include "ibinteractor.h"
#include "ibprops.h"
#include "ibvars.h"
#include "ibvarviews.h"

#include <Unidraw/catalog.h>
#include <Unidraw/editorinfo.h>
#include <Unidraw/globals.h>
#include <Unidraw/iterator.h>
#include <Unidraw/ulist.h>
#include <Unidraw/unidraw.h>

#include <InterViews/adjuster.h>
#include <InterViews/border.h>
#include <InterViews/box.h>
#include <InterViews/button.h>
#include <InterViews/deck.h>
#include <InterViews/event.h>
#include <InterViews/filebrowser.h>
#include <InterViews/frame.h>
#include <InterViews/glue.h>
#include <InterViews/message.h>
#include <InterViews/painter.h>
#include <InterViews/matcheditor.h>
#include <InterViews/scrollbar.h>
#include <InterViews/scroller.h>
#include <InterViews/sensor.h>
#include <InterViews/streditor.h>
#include <InterViews/tray.h>
#include <OS/types.h>

#include <osfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/time.h>

/*****************************************************************************/

#if !defined(_PID_T_) && !defined(__sys_stdtypes_h) && !defined(hpux)
#if defined(sun) && OSMajorVersion >= 5 && !defined(_SYS_TYPES_H)
#ifndef sco
#define _PID_T_
typedef int     pid_t;                  /* POSIX compliance    */
#endif
#endif
#endif

#ifdef __DECCXX
extern "C" {
    int killpg(int, int);
}
#endif

#if SYSV
static inline void kill_process_group(pid_t p, int sig) { kill(-p, sig); }
#else
static inline void kill_process_group(pid_t p, int sig) { killpg(p, sig); }
#endif

#if !defined(sun) || OSMajorVersion < 5
#ifndef sco
extern "C" {
#ifdef ultrix
    pid_t waitpid(pid_t, union wait*, int);
#else
    pid_t waitpid(pid_t, int*, int);
#endif
}
#endif
#endif

/*****************************************************************************/

static void Warning (Editor* ed, const char* warning) {
    AcknowledgeDialog dialog("Error!", warning);
    ed->InsertDialog(&dialog);
    dialog.Acknowledge();
    ed->RemoveDialog(&dialog);
}

/*****************************************************************************/

MoveDialog::MoveDialog () : BasicDialog(
    new ButtonState, "", "Enter X and Y movement:"
) {
    _medit = new MatchEditor(state, "9999999999999999999");
    _medit->Message("");
    _medit->Match("%f %f");

    _units = new ButtonState('p');
    input = new Sensor;
    Ref(input);
    input->Catch(KeyEvent);

    Insert(Interior());
    SelectMessage();
}

void MoveDialog::Handle (Event& e) { _medit->Handle(e); }

boolean MoveDialog::Accept () {
    Event e;
    int v = 0;

    state->SetValue(0);
    _medit->Edit();
    state->GetValue(v);

    while (v == 0) {
	Read(e);
	Forward(e);
	state->GetValue((void*&) v);
    }

    return v == '\r';
}

void MoveDialog::SelectMessage () {
    _medit->Select(0, strlen(_medit->Text()));
}

Interactor* MoveDialog::Interior () {
    const int space = round(.5*cm);

    VBox* titleblock = new VBox(
        new HBox(_title, new HGlue),
        new HBox(_subtitle, new HGlue)
    );

    HBox* rbuttons = new HBox(
        new RadioButton("Pixels", _units, 'p'),
        new HGlue(space, 0),
        new RadioButton("Points", _units, 'o'),
        new HGlue(space, 0),
        new RadioButton("Centimeters", _units, 'c'),
        new HGlue(space, 0),
        new RadioButton("Inches", _units, 'i')
    );
    rbuttons->Insert(new HGlue);

    return new MarginFrame(
        new VBox(
            titleblock,
            new VGlue(space),
            new Frame(new MarginFrame(_medit, 2)),
            new VGlue(space/2, 0),
            rbuttons,
            new VGlue(space),
            new HBox(
                new HGlue,
                new PushButton("Cancel", state, '\007'),
		new HGlue(space, 0),
                new PushButton("  OK  ", state, '\r')
            )
        ), space, space/2, 0
    );
}
    
void MoveDialog::GetValues (float& x, float& y) {
    char* movement = nil;
    movement = strnew(_medit->Text());

    if (sscanf(movement,"%f %f",&x, &y) != 2) {
	x = y = 0.0;

    } else {
	int unit;
	_units->GetValue(unit);

	switch (unit) {
	     case 'i':   x *= inches; y *= inches; break;
	     case 'o':   x *= points; y *= points; break;
	     case 'c':   x *= cm; y *= cm; break;
	}
    }
    delete movement;
}

/*****************************************************************************/

ScaleDialog::ScaleDialog () : BasicDialog(
    new ButtonState, "", "Enter X and Y scaling:"
) {
    _medit = new MatchEditor(state, "9999999999999999999");
    _medit->Message("");
    _medit->Match("%f %f");

    input = new Sensor;
    Ref(input);
    input->Catch(KeyEvent);

    Insert(Interior());
    SelectMessage();
}

void ScaleDialog::Handle (Event& e) { _medit->Handle(e); }

boolean ScaleDialog::Accept () {
    Event e;
    int v = 0;

    state->SetValue(0);
    _medit->Edit();
    state->GetValue(v);

    while (v == 0) {
	Read(e);
	Forward(e);
	state->GetValue((void*&) v);
    }

    return v == '\r';
}

void ScaleDialog::SelectMessage () {
    _medit->Select(0, strlen(_medit->Text()));
}

Interactor* ScaleDialog::Interior () {
    const int space = round(.5*cm);

    VBox* titleblock = new VBox(
        new HBox(_title, new HGlue),
        new HBox(_subtitle, new HGlue)
    );

    return new MarginFrame(
        new VBox(
            titleblock,
            new VGlue(space),
            new Frame(new MarginFrame(_medit, 2)),
            new VGlue(space),
            new HBox(
                new HGlue,
                new PushButton("Cancel", state, '\007'),
		new HGlue(space, 0),
                new PushButton("  OK  ", state, '\r')
            )
        ), space, space/2, 0
    );
}
    
void ScaleDialog::GetValues (float& x, float& y) {
    char* movement = nil;
    movement = strnew(_medit->Text());

    if (sscanf(movement,"%f %f",&x, &y) != 2) {
	x = y = 1.0;
    }
    delete movement;
}

/*****************************************************************************/

RotateDialog::RotateDialog () : BasicDialog(
    new ButtonState, "", "Enter rotation in degrees:"
) {
    _medit = new MatchEditor(state, "9999999999999999999");
    _medit->Message("");
    _medit->Match("%f");

    input = new Sensor;
    Ref(input);
    input->Catch(KeyEvent);

    Insert(Interior());
    SelectMessage();
}

void RotateDialog::Handle (Event& e) { _medit->Handle(e); }

boolean RotateDialog::Accept () {
    Event e;
    int v = 0;

    state->SetValue(0);
    _medit->Edit();
    state->GetValue(v);

    while (v == 0) {
	Read(e);
	Forward(e);
	state->GetValue((void*&) v);
    }

    return v == '\r';
}

void RotateDialog::SelectMessage () {
    _medit->Select(0, strlen(_medit->Text()));
}

Interactor* RotateDialog::Interior () {
    const int space = round(.5*cm);

    VBox* titleblock = new VBox(
        new HBox(_title, new HGlue),
        new HBox(_subtitle, new HGlue)
    );

    return new MarginFrame(
        new VBox(
            titleblock,
            new VGlue(space),
            new Frame(new MarginFrame(_medit, 2)),
            new VGlue(space),
            new HBox(
                new HGlue,
                new PushButton("Cancel", state, '\007'),
		new HGlue(space, 0),
                new PushButton("  OK  ", state, '\r')
            )
        ), space, space/2, 0
    );
}
    
void RotateDialog::GetValue (float& angle) {
    char* movement = nil;
    movement = strnew(_medit->Text());

    if (sscanf(movement,"%f",&angle) != 1) {
	angle = 0.0;
    }
    delete movement;
}
/*****************************************************************************/

InfoDialog::InfoDialog (const char* info) : BasicDialog(new ButtonState) {
    input = new Sensor;
    input->Catch(KeyEvent);
    Ref(input);

    _bodylist = new UList;
    SetTitle(info);
    Insert(Interior());
}

InfoDialog::~InfoDialog () {
    delete _bodylist;
}

ButtonState* InfoDialog::GetState() { return state; }

boolean InfoDialog::Accept () {
    Event e;
    int v = 0;

    state->SetValue(0);

    while (v == 0) {
	Read(e);
	Forward(e);
	state->GetValue(v);
            
    }
    return (v == '\r');
}

UList* InfoDialog::GetAffectedStates () {
    Iterator i;
    UList* ilist = new UList;
    for(First(i); !Done(i); Next(i)) {
        IBVarView* ibvarview = GetStateView(i);
        ibvarview->SetAffectedStates(ilist);
    }
    return ilist;
}

void InfoDialog::Handle (Event& e) {
    if (e.eventType == KeyEvent && e.len != 0) {
        char c = e.keystring[0];

        if (c == '\r' || c == '\007') {
            state->SetValue(c);
        }
    }
}

boolean InfoDialog::AppliedChanges (const char*& errors) {
    Iterator i;
    for(First(i); !Done(i); Next(i)) {
	IBVarView* ibvarView = GetStateView(i);
	if (!ibvarView->ChangedSubject(errors)) {
	    return false;
	}
    }

    return true;
}

Interactor* InfoDialog::Interior () {
    const int space = round(.5*cm);

    _body = new VBox;
    return new MarginFrame(
        new VBox(
            _title,
            new VGlue(space/2, 0),
            new VBox(
                new HBorder,
                new VGlue(2, 0),
                new HBorder
            ),
            new VGlue(space, 0),
	    _body,
            new VGlue(space, 0),
            Buttons(space)
        ), space, space/2, 0
    );
}

void InfoDialog::First(Iterator& i) {
    i.SetValue(_bodylist->First());
}

boolean InfoDialog::Done(Iterator i) {
    UList* end = (UList*) i.GetValue();
    return end == _bodylist->End();
}

void InfoDialog::Next(Iterator& i) {
    UList* next = (UList*) i.GetValue();
    i.SetValue(next->Next());
}

IBVarView* InfoDialog::GetStateView(Iterator i) {
    UList* ulist = (UList*) i.GetValue();
    return (IBVarView*) (*ulist)();
}

void InfoDialog::Include (IBVarView* view) {
    const int space = round(.5*cm);
    int gap = space/5;

    _body->Insert(view);
    _body->Insert(new VGlue(gap));
    _bodylist->Append(new UList(view));
}

Interactor* InfoDialog::Buttons (int space) {
    return new HBox(
        new HGlue,
        new PushButton("Cancel", state, '\007'),
        new HGlue(space, 0),
        new PushButton("  OK  ", state, '\r')
    );
}

/*****************************************************************************/

PropsDialog::PropsDialog (
    InteractorComp* icomp
) : BasicDialog(new ButtonState) {
    InstanceNameVar* instanceNameVar = icomp->GetInstanceNameVar();
    _instanceView = new InstanceNameVarView(instanceNameVar, state, icomp);
    _props = icomp->GetProps();
    _propsEd = nil;
    input = new Sensor;
    input->Catch(KeyEvent);
    Ref(input);
    SetTitle("Properties Information");
    Insert(Interior());
}

boolean PropsDialog::Accept () {
    Event e;
    int v = 0;

    state->SetValue(0);

    while (v == 0) {
	Read(e);
	Forward(e);
	state->GetValue(v);
            
    }
    return v == '\r';
}

void PropsDialog::Handle (Event& e) {
    if (e.eventType == KeyEvent && e.len != 0) {
        char c = e.keystring[0];
        if (c == '\r' || c == '\007') {
            state->SetValue(c);
        }
    }
}

boolean PropsDialog::AppliedChanges (const char*& errors) {
    if (!_instanceView->ChangedSubject(errors)) {
        return false;
    }
    _props->SetPropsText(_propsEd->GetPropsText());
    return true;
}

Interactor* PropsDialog::Interior () {
    const int space = round(.5*cm);

    Tray* t = new Tray;
    _propsEd = new PropsEditor(state, _props);
    HBorder* hb = new HBorder;
    VBorder* vb = new VBorder;
    Interactor* hsb = new HScrollBar(_propsEd);
    Interactor* vsb = new VScrollBar(_propsEd);
    Interactor* propsEd = new MarginFrame(_propsEd, 2);
    Glue* hg = new HGlue;

    t->HBox(t, propsEd, vb, vsb, t);
    t->HBox(t, hb, t);
    t->HBox(t, hsb, vb, hg, t);

    t->VBox(t, propsEd, hb, hsb, t);
    t->VBox(t, vb, t);
    t->VBox(t, vsb, hb, hg, t);

    return new MarginFrame(
        new VBox(
            _title,
            new VGlue(space/2, 0),
            new VBox(
                new HBorder,
                new VGlue(2, 0),
                new HBorder
            ),
            new VGlue(space, 0),
            new VBox(
                _instanceView,
                new VGlue(space, 0),
                new Frame(t)
            ),
            new VGlue(space, 0),
            Buttons(space)
        ), space, space/2, 0
    );
}

Interactor* PropsDialog::Buttons (int space) {
    return new HBox(
        new HGlue,
        new PushButton("Cancel", state, '\007'),
        new HGlue(space, 0),
        new PushButton("  OK  ", state, '\r')
    );
}

/*****************************************************************************/

ExeDialog::ExeDialog (const char* dir) : FileChooser(
    new ButtonState, dir, 10, 24, Center
) {
    FileBrowser* eb = new FileBrowser(state, dir, 10, 24);
    eb->SetModeFilter(S_IEXEC);
    eb->Update();
    StringChooser::Init(new StringEditor(state, eb->GetDirectory()), eb);

    Init("", "Select a file to execute:");
    Insert(Interior());
}

void ExeDialog::Update() {
    browser()->Update();
}

Interactor* ExeDialog::Interior () {
    const int space = round(.5*cm);
    VBox* titleblock = new VBox(
        new HBox(title, new HGlue),
        new HBox(subtitle, new HGlue)
    );

    return new MarginFrame(
        new VBox(
            titleblock,
            new VBox(
                new VGlue(space, 0),
                new Frame(new MarginFrame(_sedit, 2)),
                new VGlue(space, 0),
                new Frame(AddScroller(browser())),
                new VGlue(space, 0)
            ),
            new HBox(
                new VGlue(space, 0),
                new HGlue,
                new PushButton("Cancel", state, '\007'),
                new HGlue(space, 0),
                new PushButton("Execute", state, '\r')
            )
        ), space, space/2, 0
    );
}

/*****************************************************************************/

BSDialog::BSDialog (
    ButtonStateVarView* bsvarview
) : BasicDialog(new ButtonState) {
    input = new Sensor;
    input->Catch(KeyEvent);
    Ref(input);
    SetTitle("ButtonState Information");
    _bsvarview = bsvarview;
    if (_bsvarview != nil) {
        Insert(Interior());
    }
}

boolean BSDialog::Accept () {
    Event e;
    int v = 0;
    
    state->SetValue(0);
    
    while (v == 0) {
	Read(e);
        Forward(e);
	state->GetValue(v);
        
    }
    return v == 1 || v == '\r';
}

void BSDialog::Handle (Event& e) {
    if (e.eventType == KeyEvent && e.len != 0) {
        char c = e.keystring[0];
        
        if (c == '\r' || c == '\007') {
            state->SetValue(c);
        }
    }
}

boolean BSDialog::ChangeBS () {
    boolean success = true;
    ButtonSharedName* subject = _bsvarview->GetBSName();
    MatchEditor* med = _bsvarview->GetBSEditor();
    boolean namechange = (strcmp(med->Text(), _bs->Text()) != 0);
    
    if (namechange) {
        GetFirewallCmd firewallCmd(_bsvarview->GetGraphicComp());
        firewallCmd.Execute();
        GetConflictCmd bconflict(firewallCmd.GetFirewall(), _bs->Text());
        bconflict.Execute();
        UList* ulist = bconflict.GetConflict();
        
        for (UList* i = ulist->First(); i != ulist->End(); i = i->Next()) {
            StateVar* state = (StateVar*) (*i)();
            if (state != subject && !state->IsA(INSTANCENAME_VAR)) {
                char buf[CHARBUFSIZE];
                sprintf(
                    buf, "ButtonState name %s has already been used",
                    _bs->Text()
                );
                success = false;
                Warning(_bsvarview->GetIBEditor(), buf);
                break;
            }
        }
    }
    
    if (success) {
        const char* func = _funcname->Text();
        GetFirewallCmd firewallCmd(_bsvarview->GetGraphicComp());
        firewallCmd.Execute();
        GetConflictCmd fconflict(firewallCmd.GetFirewall(), func);
        fconflict.Execute();
        UList* ulist = fconflict.GetConflict();
        
        for (UList* j = ulist->First(); j != ulist->End(); j = j->Next()) {
            StateVar* state = (StateVar*) (*j)();
            
            if (state->IsA(BUTTONSHAREDNAME)) {
                ButtonSharedName* bsname = (ButtonSharedName*) state;
                if (strcmp(func, bsname->GetName()) == 0) {
                    success = false;
                }
            } else if (!state->IsA(PROCNAME_VAR)) {
                success = false;
            }
            if (!success) {
                char buf[CHARBUFSIZE];
                sprintf(
                    buf, "Function name %s has already been used", func
                );
                success = false;
                Warning(_bsvarview->GetIBEditor(), buf);
                break;
            }
            
        }
        if (strcmp(func, _bs->Text()) == 0 && success) {
            success = false;
            Warning(
                _bsvarview->GetIBEditor(), 
                "Function name has been used by ButtonState name"
            );
        }
    }
    
    if (success) {
        int value;
        ButtonSharedName* bsname = _bsvarview->GetBSName();
        SubclassNameVar* snamer = bsname->GetSubclass();
        _exporter->GetValue(value);
        
        bsname->SetName(_bs->Text());
        bsname->SetFuncName(_funcname->Text());
        bsname->SetInitial(atoi(_initial->Text()));
        bsname->SetExport(value);
        snamer->SetName(_subclass->Text());
        if (namechange) {
            med->Message(_bs->Text());
            _bsvarview->SetNameChange(true);
        } else {
            _bsvarview->SetNameChange(false);
        }
    }
    return success;
}

/****************************************************************************/

inline Interactor* PaddedFrame (Interactor* i) {
    return new Frame(new MarginFrame(i, 2));
}

/****************************************************************************/

static const char* IntToString (int i) {
    char buf[CHARBUFSIZE];
    sprintf(buf, "%d", i);
    return strnew(buf);
}

/****************************************************************************/

boolean BSDialog::Init () {
    boolean success = true;
    const char* text = _bsvarview->GetBSEditor()->Text();
    boolean namechange = _bsvarview->GetNameChange();
    if (namechange) {
        boolean eqname = (strcmp(text, _bs->Text()) == 0);
        if (!eqname) {
            success = NameNotChanged();
        }
    } else {
        success = NameNotChanged();
    }
    return success;
}

boolean BSDialog::NameNotChanged () {
    boolean success = true;
    _bsvarview->SetNameChange(false);
    const char* text = _bsvarview->GetBSEditor()->Text();
    ButtonStateVar* bsvar = (ButtonStateVar*) _bsvarview->GetSubject();
    ButtonSharedName* bsname = bsvar->GetButtonSharedName();
    if (strcmp(bsname->GetName(), text) == 0) {
        if (strcmp(text, _bs->Text()) != 0) {
            _funcname->Message(bsname->GetFuncName());
            _initial->Message(IntToString(bsname->GetInitial()));
            _exporter->SetValue(bsname->GetExport());
            _subclass->Message(bsname->GetSubclass()->GetName());
        }
    } else {
        bsname = nil;
        GetFirewallCmd firewallCmd(_bsvarview->GetGraphicComp());
        firewallCmd.Execute();
        GetConflictCmd bconflict(firewallCmd.GetFirewall(), text);
        bconflict.Execute();
        UList* ulist = bconflict.GetConflict();
        char buf[CHARBUFSIZE];
        
        for (UList* i = ulist->First(); i != ulist->End(); i = i->Next()) {
            StateVar* state = (StateVar*) (*i)();
            if (state->IsA(BUTTONSHAREDNAME)) {
                ButtonSharedName* tmp = (ButtonSharedName*) state;
                if (strcmp(tmp->GetName(), text) == 0) {
                    bsname = tmp;
                } else {
                    sprintf(
                        buf, "ButtonState name %s has already been used",text
                    );
                    success = false;
                    Warning(_bsvarview->GetIBEditor(), buf);
                    break;
                }
            } else if (!state->IsA(INSTANCENAME_VAR)) {
                sprintf(
                    buf, "ButtonState name %s has already been used", text
                );
                success = false;
                Warning(_bsvarview->GetIBEditor(), buf);
                break;
            }
        }
        if (bsname != nil) {
            _funcname->Message(bsname->GetFuncName());
            _initial->Message(IntToString(bsname->GetInitial()));
            _exporter->SetValue(bsname->GetExport());
            _subclass->Message(bsname->GetSubclass()->GetName());
        } else {
            ButtonSharedName* bsname = _bsvarview->GetBSName();
            if (strcmp(bsname->GetName(), text) == 0) {
                _funcname->Message(bsname->GetFuncName());
                _initial->Message(IntToString(bsname->GetInitial()));
                _exporter->SetValue(bsname->GetExport());
                _subclass->Message(bsname->GetSubclass()->GetName());
            } else {
                _funcname->Message("");
                _initial->Message("0");
                _exporter->SetValue(1);
                _subclass->Message(bsname->GetSubclass()->GetBaseClass());
            }
        }
    }
    _bs->Message(text);
    return success;
}

Interactor* BSDialog::Interior () {
    const int gap = round(.1*cm);
    const int space = round(.5*cm);
    _bs = new MatchEditor(state, "a rather long name");
    _bs->Match("%[_a-zA-Z]%[_a-zA-Z0-9]", true);
    _funcname = new MatchEditor(state, "a rather long name");
    _funcname->Match("%[_a-zA-Z]%[_a-zA-Z0-9]", true);
    _initial = new MatchEditor(state, "999");
    _initial->Match("%d", true);
    _subclass = new MatchEditor(state, "a rather long name");
    _subclass->Match("%[_a-zA-Z]%[_a-zA-Z0-9_]", true);
    
    ButtonStateVar* bsvar = (ButtonStateVar*) _bsvarview->GetSubject();
    SubclassNameVar* subvar = bsvar->GetButtonSharedName()->GetSubclass();
    const char* baseclass = subvar->GetBaseClass();
    
    Message* cname = new Message("Class Name: ");
    Message* bcname = new Message("Base Class Name: ");
    Message* bcMsg = new Message(baseclass);
    Message* nameMsg = new Message("Member Name: ");
    Message* funcMsg = new Message("Member Function: ");
    Message* initialMsg = new Message("Initial Value: ");
    
    Interactor* subclass = PaddedFrame(_subclass);
    Interactor* bs = PaddedFrame(_bs);
    Interactor* funcname = PaddedFrame(_funcname);
    Interactor* initer = PaddedFrame(_initial);
    
    _exporter = new ButtonState;
    
    Interactor* exporter = new CheckBox("Export", _exporter, 1, 0);
    
    HBox* namer = new HBox(
        bs,
        new HGlue(2*gap, 0, 0),
        exporter
    );
    namer->Align(Center);
    
    Tray* t = new Tray;
    t->HBox(t, cname, subclass, t);
    t->HBox(t, bcname, bcMsg, new HGlue, t);
    t->HBox(t, nameMsg, namer, t);
    t->HBox(t,initialMsg,new HGlue(2*gap,0,0),initer,new HGlue(0,0,10*hfil),t);
    t->HBox(t, funcMsg, new HGlue(2*gap, 0, 0), funcname, t);
    
    t->Align(VertCenter, cname, subclass);
    t->Align(VertCenter, bcname, bcMsg);
    t->Align(VertCenter, nameMsg, namer);
    t->Align(VertCenter, initialMsg, initer);
    t->Align(VertCenter, funcMsg, funcname);
    
    t->VBox(t, subclass, new VGlue(gap), bcMsg, new VGlue(gap), namer);
    t->VBox(namer, new VGlue(gap), initer, new VGlue(gap), funcname, t);
    
    t->VBox(t, cname, new VGlue(gap), bcname, new VGlue(gap), nameMsg);
    t->VBox(
        nameMsg, new VGlue(gap), initialMsg , new VGlue(gap), funcMsg, 
        new VGlue(gap), t
    );
    t->Align(Left, subclass, bcMsg, namer, initer, funcname);
    
    return new MarginFrame(
        new VBox(
            new Message("ButtonState Information"),
            new VGlue(space/2),
            new VBox(
                new HBorder,
                new VGlue(2,0),
                new HBorder
            ),
            new VGlue(space, 0),
            t,
            new VGlue(space, 0),
            Buttons(space)
        ), space, space/2, 0
    );
}

Interactor* BSDialog::Buttons (int space) {
    return new HBox(
        new HGlue,
        new PushButton("Cancel", state, '\007'),
        new HGlue(space, 0),
        new PushButton("  OK  ", state, 1)
    );
}

/*****************************************************************************/

CtrlDialog::CtrlDialog (
    CtrlStateVarView* ctrlvarview
) : BSDialog(ctrlvarview) {
    SetTitle("ControlState Information");
    Insert(Interior());
}

Interactor* CtrlDialog::Interior () {
    const int gap = round(.1*cm);
    const int space = round(.5*cm);
    _bs = new MatchEditor(state, "a rather long name");
    _funcname = new MatchEditor(state, "dummy");
    _initial = new MatchEditor(state, "dummy");
    _subclass = new MatchEditor(state, "a rather long name");
    _subclass->Match("%[_a-zA-Z]%[_a-zA-Z0-9_]", true);
    
    ButtonStateVar* bsvar = (ButtonStateVar*) _bsvarview->GetSubject();
    SubclassNameVar* subvar = bsvar->GetButtonSharedName()->GetSubclass();
    const char* baseclass = subvar->GetBaseClass();
    
    Message* cname = new Message("Class Name: ");
    Message* bcname = new Message("Base Class Name: ");
    Message* bcMsg = new Message(baseclass);
    Message* nameMsg = new Message("Member Name: ");
    
    Interactor* bs = PaddedFrame(_bs);
    Interactor* subclass = PaddedFrame(_subclass);
    
    _exporter = new ButtonState;
    Interactor* exporter = new CheckBox("Export", _exporter, 1, 0);
    
    HBox* namer = new HBox(
        bs,
        new HGlue(2*gap, 0, 0),
        exporter
    );
    namer->Align(Center);
    
    Tray* t = new Tray;
    t->HBox(t, cname, subclass, t);
    t->HBox(t, bcname, bcMsg, new HGlue, t);
    t->HBox(t, nameMsg, namer, t);
    
    t->Align(VertCenter, cname, subclass);
    t->Align(VertCenter, bcname, bcMsg);
    t->Align(VertCenter, nameMsg, namer);
    
    t->VBox(t, subclass, new VGlue(gap), bcMsg, new VGlue(gap), namer, t);
    
    t->VBox(t, cname, new VGlue(gap), bcname);
    t->VBox(bcname, new VGlue(gap), nameMsg, new VGlue(gap), t);

    t->Align(Left, subclass, bcMsg, namer);
    
    return new MarginFrame(
        new VBox(
            new Message("ControlState Information"),
            new VGlue(space/2),
            new VBox(
                new HBorder,
                new VGlue(2,0),
                new HBorder
            ),
            new VGlue(space, 0),
            t,
            new VGlue(space, 0),
            Buttons(space)
        ), space, space/2, 0
    );
}

/*****************************************************************************/

static const char* SMDone = "\t\007\033";

SMemberDialog::SMemberDialog (
    SMemberNameVarView* mvarview, const char* str, const int* array
) {
    char info[CHARBUFSIZE];

    sprintf(info, "%s Information", str);
    SetTitle(info);
    _mvarview = mvarview;
    _idvarview = nil;
    _array = array;
    _sb = new StringBrowser(state, 10, 24, true, Reversed, SMDone);
    _smember = new MatchEditor(state, "a rather long name");
    _smember->Match("%[_a-zA-Z]%[_a-zA-Z0-9]", true);
    _subclass = new SubMatchEditor(this, state, "a rather long name");
    _subclass->Match("%[_a-zA-Z]%[_a-zA-Z0-9_]", true);
    _baseclass = new SubMatchEditor(this, state, "a rather long name");
    _baseclass->Match("%[_a-zA-Z]%[_a-zA-Z0-9_]", true);
    _exporter = new ButtonState;
    _str = strnew(str);
    IDVar* idvar = mvarview->GetSMemberName()->GetIDVar();
    if (idvar != nil) {
        _idvarview = new IDVarView(idvar, state, str);
    }
}

SMemberDialog::~SMemberDialog () {
    delete _str;
    if (_sb->Count() == 0) {
        delete _sb;
        delete _baseclass;
    }
    if (interior() == nil) {
        delete _smember;
        delete _subclass;
        delete _exporter;
        delete _idvarview;
    }
}

void SMemberDialog::SMemberUpdate () {
    if (_sb->Count() > 0) {
        const char* subclass = _subclass->Text();
        const char* baseclass = _baseclass->Text();
        if (strcmp(subclass, baseclass) != 0) {
            if (_sb->Index(baseclass) >= 0) {
                _idvarview->IDUpdate(subclass, baseclass);
                
            } else {
                _idvarview->ShowStred(false);
                _idvarview->DMessage(-1);
            }
        } else {
            _idvarview->ShowStred(false);
            const char* selected = _baseclass->Text();
            int index = _sb->Index(selected);

            if (index >= 0) {
                _idvarview->DMessage(_array[index]);
            } else {
                _idvarview->DMessage(-1);
            }
        }
    } else if (_idvarview != nil) {
        _idvarview->IDUpdate();
    }
}

boolean SMemberDialog::Accept () {
    Event e;
    int v = 0;

    state->SetValue(0);

    while (v == 0) {
	Read(e);
        Forward(e);
	state->GetValue(v);
        if (v == '\t' && _sb->Count() > 0) {
            int index = _sb->Selection();
            const char* selected = _sb->String(index);
            if (strcmp(_subclass->Text(), _baseclass->Text()) == 0) {
                _subclass->Message(selected);
            }
            _baseclass->Message(selected);
            SMemberUpdate();
            v = 0;
        }
    }            
    return v == 1 || v == '\r';
}

void SMemberDialog::Append (const char* name) { _sb->Append(name); }

boolean SMemberDialog::ChangeBS () {
    boolean success = true;
    MemberSharedName* msnamer = _mvarview->GetSMemberName();
    MatchEditor* med = _mvarview->GetSMemberEditor();
    boolean namechange = (strcmp(med->Text(), _smember->Text()) != 0);

    if (namechange) {
        GetFirewallCmd firewallCmd(_mvarview->GetGraphicComp());
        firewallCmd.Execute();
        GetConflictCmd bconflict(firewallCmd.GetFirewall(), _smember->Text());
        bconflict.Execute();
        UList* ulist = bconflict.GetConflict();
        
        for (UList* i = ulist->First(); i != ulist->End(); i = i->Next()) {
            StateVar* state = (StateVar*) (*i)();
            if (state != msnamer && !state->IsA(INSTANCENAME_VAR)) {
                char buf[CHARBUFSIZE];
                sprintf(
                    buf, "Member name %s has already been used",
                    _smember->Text()
                );
                success = false;
                Warning(_mvarview->GetIBEditor(), buf);
                break;
            }
        }
    }

    if (success) {
        int value;
        MemberSharedName* msnamer = _mvarview->GetSMemberName();
        SubclassNameVar* snamer = msnamer->GetSubclass();
        _exporter->GetValue(value);
        
        msnamer->SetName(_smember->Text());
        msnamer->SetExport(value);
        snamer->SetName(_subclass->Text());
        if (_sb->Count() > 0) {
            if (_sb->Index(_baseclass->Text()) >= 0) {
                snamer->SetBaseClass(_baseclass->Text());
            } else {
                char buf[CHARBUFSIZE];
                sprintf(
                    buf, "Base class name %s not in library",
                    _baseclass->Text()
                );
                success = false;
                Warning(_mvarview->GetIBEditor(), buf);
                return success;
            }
        }
        if (namechange) {
            med->Message(_smember->Text());
            _mvarview->SetNameChange(true);
        } else {
            _mvarview->SetNameChange(false);
        }
        if (_idvarview != nil) {
            const char* err;
            _idvarview->ChangedSubject(err);
        }
    }
    return success;
}

boolean SMemberDialog::Init () {
    boolean success = true;
    if (interior() == nil) {
        Insert(Interior());
    }
    const char* text = _mvarview->GetSMemberEditor()->Text();
    boolean namechange = _mvarview->GetNameChange();
    if (namechange) {
        boolean eqname = (strcmp(text, _smember->Text()) == 0);
        if (!eqname) {
            success = NameNotChanged();
        }
    } else {
        success = NameNotChanged();
    }
    if (success) {
        SMemberUpdate();
    }
    return success;
}

boolean SMemberDialog::NameNotChanged () {
    boolean success = true;
    _mvarview->SetNameChange(false);
    const char* text = _mvarview->GetSMemberEditor()->Text();
    MemberNameVar* mnamer = (MemberNameVar*) _mvarview->GetSubject();
    MemberSharedName* msnamer = mnamer->GetMemberSharedName();
    if (strcmp(msnamer->GetName(), text) == 0) {
        if (strcmp(text, _smember->Text()) != 0) {
            _exporter->SetValue(msnamer->GetExport());
            _subclass->Message(msnamer->GetSubclass()->GetName());
            if (_baseclass != nil) {
                _baseclass->Message(msnamer->GetSubclass()->GetBaseClass());
            }         
        }
    } else {
        msnamer = nil;
        GetFirewallCmd firewallCmd(_mvarview->GetGraphicComp());
        firewallCmd.Execute();
        GetConflictCmd bconflict(firewallCmd.GetFirewall(), text);
        bconflict.Execute();
        UList* ulist = bconflict.GetConflict();
        char buf[CHARBUFSIZE];

        for (UList* i = ulist->First(); i != ulist->End(); i = i->Next()) {
            StateVar* state = (StateVar*) (*i)();
            if (state->IsA(MEMBERSHAREDNAME)) {
                msnamer = (MemberSharedName*) state;
                break;

            } else if (!state->IsA(INSTANCENAME_VAR)) {
                sprintf(
                    buf, "Member name %s has already been used", text
                );
                success = false;
                Warning(_mvarview->GetIBEditor(), buf);
                break;
            }
        }
        if (msnamer != nil) {
            _exporter->SetValue(msnamer->GetExport());
            _subclass->Message(msnamer->GetSubclass()->GetName());
        } else {
            MemberSharedName* msnamer = _mvarview->GetSMemberName();
            _exporter->SetValue(msnamer->GetExport());
            _subclass->Message(msnamer->GetSubclass()->GetName());
            if (_baseclass != nil) {
                _baseclass->Message(msnamer->GetSubclass()->GetBaseClass());
            }         
        }
    }
    _smember->Message(text);
    return success;
}

Interactor* SMemberDialog::Interior () {
    const int gap = round(.1*cm);
    const int space = round(.5*cm);
    MemberNameVar* mnamer = (MemberNameVar*) _mvarview->GetSubject();
    SubclassNameVar* subvar = mnamer->GetMemberSharedName()->GetSubclass();
    const char* baseclass = subvar->GetBaseClass();

    char msg1[CHARBUFSIZE];
    char msg2[CHARBUFSIZE];
    char msg3[CHARBUFSIZE];

    sprintf(msg1, "%s Class: ", _str);
    sprintf(msg2, "%s Base Class: ", _str);
    sprintf(msg3, "%s Information", _str);

    Message* cname = new Message(msg1);
    Message* bcname = new Message(msg2);
    Message* nameMsg = new Message("Member Name: ");

    Interactor* subclass = PaddedFrame(_subclass);
    Interactor* smember = PaddedFrame(_smember);
    Interactor* bcMsg;
    if (_sb->Count() > 0) {
        bcMsg = PaddedFrame(_baseclass);
    } else {
        bcMsg = new Message(baseclass);
    }

    Interactor* exporter = new CheckBox("Export", _exporter, 1, 0);

    HBox* namer = new HBox(
        smember,
        new HGlue(2*gap, 0, 0),
        exporter
    );
    namer->Align(Center);

    Tray* t = new Tray;
    t->HBox(t, cname, subclass, t);
    t->HBox(t, bcname, bcMsg, t);
    t->HBox(t, nameMsg, namer, t);

    t->Align(VertCenter, cname, subclass);
    t->Align(VertCenter, bcname, bcMsg);
    t->Align(VertCenter, nameMsg, namer);

    t->VBox(t, subclass, new VGlue(gap), bcMsg, new VGlue(gap), namer, t);
    t->VBox(t, cname, new VGlue(gap), bcname, new VGlue(gap), nameMsg);
    t->VBox(nameMsg, new VGlue(gap), t);
    t->Align(Left, subclass, bcMsg, namer);

    VBox* body = new VBox;
    body->Insert(t);
    if (_idvarview != nil) {
        body->Insert(new VGlue(gap));
        body->Insert(_idvarview);
    }
    if (_sb->Count() > 0) {
        body->Insert(new VGlue(space/2, 0));
        body->Insert(
            new VBox(
                new HBox(
                    new Message("Library"),
                    new HGlue
                ),
                new VGlue(space/2, 0),
                new Frame(
                    new HBox(
                        new MarginFrame(_sb, 2),
                        new VBorder,
                        new VScrollBar(_sb)
                    )
                )
            )
        );
    }
    return new MarginFrame(
        new VBox(
            new Message(msg3),
            new VGlue(space/2),
            new VBox(
                new HBorder,
                new VGlue(2,0),
                new HBorder
            ),
            new VGlue(space, 0),
            body,
            new VGlue(space, 0),
            Buttons(space)
        ), space, space/2, 0
    );
}

/*****************************************************************************/

OptionDialog::OptionDialog (
    const char* title, const char* subtitle,
    int sel1, const char* opt1, 
    int sel2, const char* opt2, 
    int sel3, const char* opt3
) : BasicDialog(new ButtonState, title, subtitle) {
    input = new Sensor;
    input->Catch(KeyEvent);
    Ref(input);
    Insert(Interior(sel1, opt1, sel2, opt2, sel3, opt3));
}

int OptionDialog::Option () {
    int value;
    _select->GetValue(value);
    return value;
}

boolean OptionDialog::Accept () {
    Event e;
    int v = 0;

    state->SetValue(v);
    do {
        Read(e);
        if (e.eventType == KeyEvent) {
            state->SetValue(e.keystring[0]);
        } else {
            Forward(e);
        }
        state->GetValue(v);
    } while (v == 0);

    return v == '\r';
}

Interactor* OptionDialog::Interior (
    int sel1, const char* opt1, 
    int sel2, const char* opt2, 
    int sel3, const char* opt3
) {
    _select = new ButtonState(sel3);
    const int space = round(.5*cm);

    VBox* titleblock = new VBox(
        new HBox(_title, new HGlue),
        new HBox(_subtitle, new HGlue)
    );

    return new MarginFrame(
        new VBox(
            titleblock,
            new VGlue(space, 0),
            new VBox(
                new RadioButton(strnew(opt3), _select, sel3),
                new VGlue(2, 0, 0),
                new RadioButton(strnew(opt2), _select, sel2),
                new VGlue(2, 0, 0),
                new RadioButton(strnew(opt1), _select, sel1),
                new VGlue(space, 0)
            ),
            new HBox(
                new HGlue,
                new PushButton("Cancel", state, '\007'),
                new HGlue(space, 0),
                new PushButton("  OK  ", state, '\r')
            )
        ), space, space/2, 0
    );
}

/*****************************************************************************/

ConflictDialog::ConflictDialog (
    void* id
) : BasicDialog(new ButtonState) {
    input = new Sensor;
    input->Catch(KeyEvent);
    Ref(input);
    _id = id;
    _hash_id = 0;
}

static int CharToNum (const char* name) {
    int count = 0;
    for(int i = 0; name[i] != '\0'; i++) {
        count += name[i];
    }
    return count;
}

void ConflictDialog::AddConflict (const char* s, boolean checked) {
    if (*s != '\0') {
        Catalog* catalog = unidraw->GetCatalog();
        conflict(strnew(s), _conflict.Count());
        bs(new ButtonState(checked), _bs.Count());
    }
    _hash_id += CharToNum(s);
}

ConflictDialog::~ConflictDialog () {
    for (int i = 0; i < _conflict.Count(); ++i) {
        char* s = conflict(i);
        delete s;
        Unref(bs(i));
    }
}

void ConflictDialog::Update () { 
    Unref(output);
    output = nil;
    Insert(Interior()); 
}
void ConflictDialog::conflict (char* s, int i) { _conflict.Insert(s, i); }
void ConflictDialog::bs (ButtonState* bs, int i) { _bs.Insert(bs, i); }

boolean ConflictDialog::Accept () {
    Event e;
    int v = 0;

    state->SetValue(v);
    do {
        Read(e);
        if (e.eventType == KeyEvent) {
            state->SetValue(e.keystring[0]);
        } else {
            Forward(e);
        }
        state->GetValue(v);
    } while (v == 0);

    return v == '\r';
}

boolean ConflictDialog::Skipped () {
    int v = 0;
    state->GetValue(v);
    return v == 'n';
}

boolean ConflictDialog::Checked (const char* filename) {
    int retval = false;
    for (int i = 0; i < _conflict.Count(); i++) {
        if (strcmp(filename, conflict(i)) == 0) {
            bs(i)->GetValue(retval);
            break;
        }
    }
    return retval;
}

void ConflictDialog::AddCheckBox (Scene* scene, int i) {
    Catalog* catalog = unidraw->GetCatalog();
    const char* msg = conflict(i);
    
    if (msg != nil && catalog->Exists(msg) && catalog->Writable(msg)) {
        char buf[CHARBUFSIZE];
        const char* format = " Overwrite existing \"%s\"";

	sprintf(buf, format, msg);
	scene->Insert(new CheckBox(strnew(buf), bs(i), 1, 0));
	scene->Insert(new VGlue(2, 0, 0));
    }
}

Interactor* ConflictDialog::Interior () {
    const int space = round(.5*cm);
    Scene* varViews = new VBox;

    for (int i = _conflict.Count()-1; i >= 0; --i) {
        AddCheckBox(varViews, i);
    }

    VBox* titleblock = new VBox(
        new HBox(
	    new MarginFrame(
		new Message("Generated files already exist.")
	    ), new HGlue
	),
        new HBox(
	    new MarginFrame(
		new Message("Check to overwrite:")
	    ), new HGlue
	)
    );

    return new MarginFrame(
        new VBox(
            titleblock,
            new VGlue(space, 0),
            varViews,
            new VGlue(space, 0),
            new HBox(
                new HGlue,
                new PushButton(" Don't Generate ", state, 'n'),
                new HGlue(space, 0),
                new PushButton(" Cancel ", state, '\007'),
                new HGlue(space, 0),
                new PushButton("  OK  ", state, '\r')
            )
        ), space, space/2, 0
    );
}

/*****************************************************************************/

InstallRemoveDialog::InstallRemoveDialog (
    EditorInfo* cur, int rows, int cols
) : BasicDialog(new ButtonState, "Choose tools to install/uninstall:") {
    Init(cur, rows, cols);
    Insert(Interior());
}

void InstallRemoveDialog::Init (EditorInfo* orig, int rows, int cols) {
    Catalog* catalog = unidraw->GetCatalog();
    const char* toolpath = getenv("TOOLDIR");

    input = new Sensor;
    input->Catch(KeyEvent);
    Ref(input);

    _current = new StringBrowser(state, rows, cols, false);
    if (toolpath == nil) {
        _available = new FileBrowser(state, "./", rows, cols, false);
    } else {
        _available = new FileBrowser(state, toolpath, rows, cols, false);
    }
    _available->SetTextFilter(".*\\.[tT]ool");
    _available->SetDirectoryTextFilter("^$");

    for (int i = 0; i < orig->Count(); ++i) {
	const char* name = orig->GetName(i);
	_current->Insert(name, Position(_current, name));
    }

    _install = new ButtonState;
    _remove = new ButtonState;
    _clear = new ButtonState;

    _installed = _removed = nil;
    Update();
}

InstallRemoveDialog::~InstallRemoveDialog() {
    Unref(_install);
    Unref(_remove);
    Unref(_clear);
    delete _installed;
    delete _removed;
}

void InstallRemoveDialog::Update () {
    _available->Update();

    for (int i = 0; i < _current->Count(); ++i) {
        _available->Remove(_available->Index(_current->String(i)));
    }

    delete _installed;
    delete _removed;
    _installed = new EditorInfo;
    _removed = new EditorInfo;
}

void InstallRemoveDialog::Handle (Event& e) {
    if (e.eventType == KeyEvent && e.len != 0) {
        char c = e.keystring[0];

        if (c == '\r' || c == '\007') {
            state->SetValue(c);
        }
    }
}

boolean InstallRemoveDialog::Accept () {
    Event e;
    int v = 0;
    state->SetValue(v);

    while (v == 0 || v == '\t') {
        Read(e);
        Forward(e);

        int install, remove, clear;
        _install->GetValue(install);
        _remove->GetValue(remove);
        _clear->GetValue(clear);

        if (install != 0) {
            InstallSelections();
            _install->SetValue(0);
        }

        if (remove != 0) {
            RemoveSelections();
            _remove->SetValue(0);
        }

        if (clear != 0) {
            _current->UnselectAll();
            _available->UnselectAll();
            _clear->SetValue(0);
        }

        state->GetValue(v);
    }

    return v == '\r';
}

void InstallRemoveDialog::InstallSelections () {
    int count = _available->Selections();

    for (int i = 0; i < count; ++i) {
        int index = _available->Selection();
        const char* toolName = strnew(_available->String(index));

        _current->Insert(toolName, Position(_current, toolName));
        _installed->Register(toolName);
        _removed->UnregisterName(toolName);
        _available->Remove(index);
    }
}

void InstallRemoveDialog::RemoveSelections () {
    int count = _current->Selections();

    for (int i = 0; i < count; ++i) {
        int index = _current->Selection();
        const char* toolName = strnew(_current->String(index));

        _available->Insert(toolName, Position(_available, toolName));
        _removed->Register(toolName);
        _installed->UnregisterName(toolName);
        _current->Remove(index);
    }
}

int InstallRemoveDialog::Position (StringBrowser* sb, const char* s) {
    for (int i = 0; i < sb->Count(); ++i) {
        if (strcmp(s, sb->String(i)) < 0) {
            return i;
        }
    }
    return sb->Count();
}

Interactor* InstallRemoveDialog::Interior () {
    const int space = round(.5*cm);

    VBox* buttons = new VBox(
        new PushButton("<< Install ", _install, 'i'),
        new VGlue,
        new PushButton("  Remove >>", _remove, 'r'),
        new VGlue,
        new PushButton("  Clear  ", _clear, 'c')
    );
    buttons->Align(Center);

    Interactor* current = new Frame(AddScroller(_current));
    Interactor* available = new Frame(AddScroller(_available));

    Message* curMsg = new Message("Installed tools:");
    Message* availMsg = new Message("Uninstalled tools:");

    Tray* t = new Tray;

    t->HBox(t, curMsg, new HGlue, availMsg, new HGlue, t);
    t->HBox(t, current, new HGlue(space),buttons,new HGlue(space),available,t);

    t->VBox(t, curMsg, current, t);
    t->VBox(t, availMsg, available, t);
    t->VBox(buttons, t);

    t->Align(Left, curMsg, current);
    t->Align(Left, availMsg, available);
    t->Align(Top, current, buttons, available);

    return new MarginFrame(
        new VBox(
            new HBox(_title, new HGlue),
            new HBox(_subtitle, new HGlue),
            new VGlue(space, 0),
            t,
            new VGlue(space, 0),
            new HBox(
                new HGlue,
                new PushButton(" Cancel ", state, '\007'),
                new HGlue(space, 0),
                new PushButton("  OK  ", state, '\r')
            )
        ), space, space/2, 0
    );
}

Interactor* InstallRemoveDialog::AddScroller (Interactor* i) {
    return new HBox(
        new MarginFrame(i, 2),
        new VBorder,
        new VBox(
            new UpMover(i, 1),
            new HBorder,
            new VScroller(i),
            new HBorder,
            new DownMover(i, 1)
        )
    );
}

/*****************************************************************************/

AbortDialog::AbortDialog (
    int pid, const char* title
) : AcknowledgeDialog(title) {
    Insert(Interior(title));
    _pid = pid;
}

boolean AbortDialog::Abort () {
    Event e;
    int v = 0;
    boolean tokill = true;
    boolean success = false;

    state->SetValue(v);
    do {
	success = Read(0, 100000, e);

	if (success) {
	    Forward(e);
	    state->GetValue(v);

	} else if (waitpid(_pid, nil, WNOHANG) != 0) {
	    tokill = false;
	    break;
	}
    } while (v == 0);

    if (tokill) {
        kill_process_group(_pid, SIGKILL);
    }
    return tokill;
}

Interactor* AbortDialog::Interior (const char* title) {
    const int space = round(.5*cm);

    return new MarginFrame(
        new VBox(
            new HBox(new MarginFrame(new Message(title)), new HGlue),
            new VGlue(space),
            new HBox(
                new HGlue,
                new PushButton("  Abort  ", state, 1),
                new HGlue
            )
        ), space, space/2, 0
    );
}

/**************************************************************************/

StringBrowserDialog::StringBrowserDialog (
    const char* text
) : AcknowledgeDialog(text) {
    _text = strnew(text);
    Insert(Interior());
}

StringBrowserDialog::~StringBrowserDialog () {
    delete _text;
}

Interactor* StringBrowserDialog::Interior () {
    const int space = round(.5*cm);
    StringBrowser* sb = new StringBrowser(state, 8, 60);
    char* text = _text;

    while (true) {
	char* index = strchr(text, '\n');
	if (index == nil) {
            sb->Append(text);
	    break;
	} else {
	    *index = '\0';
	    sb->Append(text);
	    *index = '\n';
	    text = index + 1;
	}
    }

    Interactor* frame = new Frame (
	new HBox (
	    new MarginFrame(sb, 2),
	    new VBorder,
	    new VBox(
		new UpMover(sb),
		new HBorder,
	        new VScroller(sb),
		new HBorder,
		new DownMover(sb)
	    )
	)
    );

    return new MarginFrame(
        new VBox(
            new HBox(new Message("Errors encountered:"), new HGlue),
	    new VGlue(space, 0),
	    frame,
	    new VGlue(space, 0),
            new HBox(
                new HGlue,
                new PushButton("  OK  ", state, 1),
                new HGlue
            )
        ), space
    );
}

