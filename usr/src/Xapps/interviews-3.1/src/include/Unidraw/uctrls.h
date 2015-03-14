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
 * UControl and UControlInteractor subclasses.
 */

#ifndef unidraw_uctrls_h
#define unidraw_uctrls_h

#include <Unidraw/globals.h>
#include <Unidraw/uctrl.h>

class CommandControl : public UControl {
public:
    CommandControl(ControlInfo*);
    CommandControl(const char*, ControlInfo*);

    virtual void Do();
protected:
    virtual void Busy();
    virtual void Done();
private:
    void Init(ControlInfo*);
};

class PanelControl : public UControl {
public:
    PanelControl(Interactor*, ControlInfo*, ControlState* = nil);
    PanelControl(const char*, Interactor*, ControlInfo*, ControlState* = nil);

    virtual void Do();
protected:
    virtual void Down();
    virtual void Enter();
    virtual void Leave();
    virtual void Select();
private:
    void Init(Interactor*, ControlState*);
};

class HPanelControl : public PanelControl {
public:
    HPanelControl(ControlInfo*, ControlState* = nil);
    HPanelControl(const char*, ControlInfo*, ControlState* = nil);
};

class VPanelControl : public PanelControl {
public:
    VPanelControl(ControlInfo*, ControlState* = nil);
    VPanelControl(const char*, ControlInfo*, ControlState* = nil);
};

class CommandInteractor : public UControlInteractor {
public:
    CommandInteractor(ControlInfo*);
protected:
    virtual void Reconfig();
    virtual void Resize();
};

class PanelInteractor : public UControlInteractor {
public:
    PanelInteractor(ControlInfo*, Orientation);
protected:
    virtual void Reconfig();
    virtual void Resize();
private:
    Orientation _orient;
};

#endif
