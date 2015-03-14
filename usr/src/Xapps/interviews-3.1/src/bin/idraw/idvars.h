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
 * Idraw-specific state variables
 */

#ifndef idvars_h
#define idvars_h

#include <Unidraw/statevars.h>
#include <Unidraw/stateviews.h>

class ArrowVar : public StateVar {
public:
    ArrowVar(boolean h = false, boolean t = false);
    
    boolean Head();
    boolean Tail();
    void SetArrows(boolean, boolean);

    virtual StateVar& operator = (StateVar&);

    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    boolean _head, _tail;
};

inline boolean ArrowVar::Head() { return _head; }
inline boolean ArrowVar::Tail() { return _tail; }


class ArrowVarView : public StateVarView {
public:
    ArrowVarView(ArrowVar*, BrushVar*, ColorVar* = nil);
    virtual ~ArrowVarView();
protected:
    virtual void Init();
    virtual boolean Stale();
protected:
    ArrowVar* _arrowSubj;
    boolean _prevHead, _prevTail;
    PSBrush* _prevVal;
    PSColor* _prevFg, *_prevBg;
    ColorVar* _colorSubj;
};

#endif
