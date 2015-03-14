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
 * Structuring commands.
 */

#ifndef unidraw_commands_struct_h
#define unidraw_commands_struct_h

#include <Unidraw/Commands/command.h>

class GraphicComp;
class Iterator;
class Selection;

class GroupCmd : public Command {
public:
    GroupCmd(ControlInfo*, GraphicComp* dest = nil);
    GroupCmd(Editor* = nil, GraphicComp* dest = nil);
    virtual ~GroupCmd();

    virtual void Execute();
    virtual void Unexecute();
    virtual boolean Reversible();

    GraphicComp* GetGroup();
    void SetGroup(GraphicComp*);

    virtual Command* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    boolean _executed;
private:
    void Init(GraphicComp*);
private:
    GraphicComp* _group;
};

inline GraphicComp* GroupCmd::GetGroup () { return _group; }
inline void GroupCmd::SetGroup (GraphicComp* g) { _group = g; }

class UngroupCmd : public Command {
public:
    UngroupCmd(ControlInfo*);
    UngroupCmd(Editor* = nil);
    virtual ~UngroupCmd();

    virtual void Execute();
    virtual void Unexecute();

    Clipboard* GetKids();
    void SetKids(Clipboard*);

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    boolean _executed;
private:
    void Init();
private:
    Clipboard* _kids;
};

inline Clipboard* UngroupCmd::GetKids () { return _kids; }
inline void UngroupCmd::SetKids (Clipboard* k) { _kids = k; }

class FrontCmd : public Command {
public:
    FrontCmd(ControlInfo*);
    FrontCmd(Editor* = nil);

    virtual void Execute();
    virtual void Unexecute();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class BackCmd : public Command {
public:
    BackCmd(ControlInfo*);
    BackCmd(Editor* = nil);

    virtual void Execute();
    virtual void Unexecute();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

#endif
