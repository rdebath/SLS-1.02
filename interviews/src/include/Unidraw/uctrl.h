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
 * UControl - base class for KeyMap-compatible controls.
 * UControlInteractor - base class for interactors that base their
 * appearance on ControlInfo information
 */

#ifndef unidraw_uctrl_h
#define unidraw_uctrl_h

#include <IV-2_6/InterViews/control.h>
#include <Unidraw/enter-scope.h>

#include <IV-2_6/_enter.h>

class ControlInfo;
class Graphic;

class UControl : public Control {
public:
    virtual void SetControlInfo(ControlInfo*);
    ControlInfo* GetControlInfo();
protected:
    UControl(ControlInfo*);
    UControl(const char*, ControlInfo*);
protected:
    ControlInfo* _info;
private:
    void Init(ControlInfo*);
};

class UControlInteractor : public Interactor {
public:
    virtual ~UControlInteractor();

    virtual void Highlight(boolean);
    virtual void SetControlInfo(ControlInfo*);
    ControlInfo* GetControlInfo();
protected:
    UControlInteractor(ControlInfo*);
    UControlInteractor();

    virtual void Redraw(Coord, Coord, Coord, Coord);
    virtual void Invert();

    Graphic* InitLabel(ControlInfo*);
protected:
    ControlInfo* _info;
    Graphic* _picture;
    Graphic* _label;
    boolean _highlighted;
private:
    void Init(ControlInfo*);
};

#include <IV-2_6/_leave.h>

#endif
