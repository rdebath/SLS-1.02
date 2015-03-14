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
 * Transfer function subclasses.
 */

#ifndef unidraw_transfns_h
#define unidraw_transfns_h

#include <Unidraw/transfn.h>

class TF_2Port : public TransferFunct {
public:
    virtual void Evaluate(Path* = nil);

    virtual StateVar* GetInput(int index);
    virtual StateVar* GetOutput(int index);
    virtual void SetInput(StateVar*, int index);
    virtual void SetOutput(StateVar*, int index);

    virtual int Inputs();
    virtual int Outputs();

    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    TF_2Port();

    virtual void Transfer();
    virtual boolean ChangedOutput(int index = 0);
};

class TF_Direct : public TF_2Port {
public:
    TF_Direct(StateVar* input = nil, StateVar* output = nil);

    virtual StateVar* GetInput(int index = 0);
    virtual StateVar* GetOutput(int index = 0);
    virtual void SetInput(StateVar*, int index = 0);
    virtual void SetOutput(StateVar*, int index = 0);

    virtual int Inputs();
    virtual int Outputs();

    virtual TransferFunct* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    virtual void Transfer();
    virtual boolean ChangedOutput(int index = 0);
private:
    StateVar* _input, *_output;
    boolean _changed;
};

#endif
