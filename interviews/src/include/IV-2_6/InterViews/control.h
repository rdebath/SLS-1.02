/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * Controls provide an interface to selecting and executing some action.
 */

#ifndef ivlook2_6_control_h
#define ivlook2_6_control_h

#include <InterViews/resource.h>
#include <IV-2_6/InterViews/scene.h>
#include <IV-2_6/InterViews/subject.h>

#include <InterViews/_enter.h>

class Control;
class ControlState;

class Control : public MonoScene {
public:
    Control(Interactor*);
    Control(const char* name, Interactor*);
    ~Control();

    ControlState* State() { return state_; }
    void SetState(ControlState*);

    virtual void Handle(Event&);

    virtual void Enable(boolean);
    boolean Enabled();

    virtual void Select();		/* highlight, open, and grab */
    virtual void Unselect();		/* close, unhighlight */

    virtual void Do();			/* action for selection */

    Control* ParentControl();
    Control* RootControl();
protected:
    virtual void Down();		/* activate control */
    virtual void Enter();		/* select a control if active */
    virtual void Open();		/* open subviews, if any */
    virtual void Grab();		/* read input events */
    virtual void Skip();		/* ignore until enter active control */
    virtual void Leave();		/* unselect if active */
    virtual void Close();		/* close subviews, if any */
    virtual void Up();			/* deactivate control, do selection */

    virtual void Busy();
    virtual void Done();

    virtual boolean IsGrabbing(Interactor*);

    void Reparent(Control*, Control* parent);
private:
    ControlState* state_;
    boolean enabled_;
    Control* parent_;

    void Init(const char*, Interactor*);
};

inline boolean Control::Enabled() { return enabled_; }
inline Control* Control::ParentControl() { return parent_; }

/*
 * ControlState is a subject that several controls share to exchange
 * common information.
 */

enum ControlStatus { ControlActive = 0x1 };

class ControlState : virtual public Subject {
public:
    ControlState(unsigned status = 0);
    ~ControlState();

    boolean Active() { return Set(ControlActive); }
    void Activate() { Set(ControlActive, true); }
    virtual void Deactivate();

    Control* Selection() { return selection; }
    void Selection(Control* c) { selection = c; }
    virtual void NotifySelection(Control*);

    Control* Action() { return action; }
    void Action(Control* c) { action = c; }

    void Push(ControlState*);
    void Pop();
    ControlState* Next() { return next; }
    ControlState* Prev() { return prev; }
protected:
    unsigned status;
    Control* selection;
    Control* action;
    ControlState* next;
    ControlState* prev;

    boolean Set(ControlStatus s) { return (status & s) != 0; }
    void Set(ControlStatus s, boolean b) {
	status = b ? (status | s) : (status & ~s);
    }
};

#include <InterViews/_leave.h>

#endif
