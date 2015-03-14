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
 * Command - an object that operates on selected components, an editor,
 * or unidraw itself.
 */

#ifndef unidraw_commands_command_h
#define unidraw_commands_command_h

#include <Unidraw/globals.h>

class Clipboard;
class Component;
class ControlInfo;
class Data;
class DataCache;
class Editor;
class GraphicComp;
class Iterator;
class Selection;

class istream;
class ostream;

class Command {
public:
    virtual void Execute();
    virtual void Unexecute();
    virtual boolean Reversible();

    virtual void Store(Component*, Data* = nil);
    virtual Data* Recall(Component*);
    virtual void Log();
    
    virtual void SetControlInfo(ControlInfo*);
    virtual void SetEditor(Editor*);
    virtual void SetClipboard(Clipboard*);
    
    virtual ControlInfo* GetControlInfo();
    virtual Editor* GetEditor();
    virtual Clipboard* GetClipboard();

    virtual void First(Iterator&);
    virtual void Last(Iterator&);
    virtual void Next(Iterator&);
    virtual void Prev(Iterator&);
    virtual boolean Done(Iterator&);

    virtual ~Command();
    virtual Command* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual ClassId GetSubstId(const char*& delim);
    virtual boolean IsA(ClassId);
protected:
    Command(ControlInfo*, Clipboard* = nil);
    Command(Editor* = nil, Clipboard* = nil);

    void InitCopy(Command*);
    ControlInfo* CopyControlInfo();

    Clipboard* CopyClipboard();
    Clipboard* DeepCopyClipboard();

    GraphicComp* GetGraphicComp();
protected:
    ControlInfo* _ctrlInfo;
    Editor* _editor;
    Clipboard* _clipboard;
private:
    DataCache* CopyData();
    void SetData(DataCache*);
private:
    DataCache* _cache;
};

#endif
