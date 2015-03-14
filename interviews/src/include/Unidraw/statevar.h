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
 * StateVar state variable subject.
 */

#ifndef unidraw_statevar_h
#define unidraw_statevar_h

#include <Unidraw/globals.h>

class Connector;
class StateVarView;
class istream;
class ostream;

class StateVar {
public:
    Connector* GetBinding();

    virtual void Attach(StateVarView*);
    virtual void Detach(StateVarView*);
    virtual void Notify();

    virtual StateVar& operator = (StateVar&);

    virtual ~StateVar();
    virtual StateVar* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual ClassId GetSubstId(const char*& delim);
    virtual boolean IsA(ClassId);
protected:
    StateVar();
private:
    friend class Connector;
    void SetBinding(Connector*);
private:
    class UList* _views;
    Connector* _conn;
};

#endif
