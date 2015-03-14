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
 * AlignCmd - aligns components relative to one another.
 * AlignToGridCmd - aligns components relative to a grid.
 */

#ifndef unidraw_commands_align_h
#define unidraw_commands_align_h

#include <Unidraw/Commands/command.h>

class GraphicComp;
class GraphicView;

class AlignCmd : public Command {
public:
    AlignCmd(ControlInfo*, Alignment = Left, Alignment = Left);
    AlignCmd(Editor* = nil, Alignment = Left, Alignment = Left);

    void GetAlignment(Alignment&, Alignment&);
    GraphicComp* GetReference(GraphicComp*);

    virtual Command* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    Alignment _align1, _align2;
};

class AlignToGridCmd : public Command {
public:
    AlignToGridCmd(ControlInfo*);
    AlignToGridCmd(Editor* = nil);

    virtual void Execute();
    virtual void Unexecute();

    virtual void Align(GraphicView*, float, float);
    virtual void Unalign(GraphicView*);

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    void Move(GraphicComp*);
    void Unmove(GraphicComp*);
};

#endif
