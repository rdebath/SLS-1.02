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
 * Tool - supports direct manipulation of components.
 */

#ifndef unidraw_tools_tool_h
#define unidraw_tools_tool_h

#include <Unidraw/globals.h>

class Command;
class ControlInfo;
class Viewer;
class Event;
class Manipulator;
class Transformer;
class istream;
class ostream;

class Tool {
public:
    virtual Manipulator* CreateManipulator(Viewer*, Event&, Transformer* =nil);
    virtual Command* InterpretManipulator(Manipulator*);

    virtual void SetControlInfo(ControlInfo*);
    virtual ControlInfo* GetControlInfo();

    virtual ~Tool();
    virtual Tool* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual ClassId GetSubstId(const char*& delim);
    virtual boolean IsA(ClassId);
protected:
    Tool(ControlInfo* = nil);

    ControlInfo* CopyControlInfo();
private:
    ControlInfo* _ctrlInfo;
};

#endif
