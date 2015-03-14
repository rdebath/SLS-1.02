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
 * Cataloging commands.
 */

#ifndef unidraw_commands_catcmds_h
#define unidraw_commands_catcmds_h

#include <Unidraw/Commands/command.h>

#undef FileChooser
#define FileChooser _lib_iv(FileChooser)

class CatalogChooser;
class FileChooser;
class PrintDialog;

class NewCompCmd : public Command {
public:
    NewCompCmd(ControlInfo*, Component* prototype = nil);
    NewCompCmd(Editor* = nil, Component* prototype = nil);
    virtual ~NewCompCmd();

    virtual void Execute();
    virtual boolean Reversible();

    Component* GetPrototype();

    virtual Command* Copy();
    virtual void Read(istream&);
    virtual void Write(ostream&);
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    Component* prototype_;
};

inline Component* NewCompCmd::GetPrototype () { return prototype_; }

class RevertCmd : public Command {
public:
    RevertCmd(ControlInfo*);
    RevertCmd(Editor* = nil);

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class ViewCompCmd : public Command {
public:
    ViewCompCmd(ControlInfo*, FileChooser* = nil);
    ViewCompCmd(Editor* = nil, FileChooser* = nil);
    virtual ~ViewCompCmd();

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    FileChooser* chooser_;
};

class SaveCompCmd : public Command {
public:
    SaveCompCmd(ControlInfo*);
    SaveCompCmd(Editor* = nil);

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

class SaveCompAsCmd : public Command {
public:
    SaveCompAsCmd(ControlInfo*, FileChooser* = nil);
    SaveCompAsCmd(Editor* = nil, FileChooser* = nil);
    virtual ~SaveCompAsCmd();

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
private:
    FileChooser* chooser_;
};

class PrintCmd : public Command {
public:
    PrintCmd(ControlInfo*, PrintDialog* = nil);
    PrintCmd(Editor* = nil, PrintDialog* = nil);
    virtual ~PrintCmd();

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
protected:
    int print(const char* print_cmd, const char* file);
protected:
    PrintDialog* _dialog;
};

class QuitCmd : public Command {
public:
    QuitCmd(ControlInfo*);
    QuitCmd(Editor* = nil);

    virtual void Execute();
    virtual boolean Reversible();

    virtual Command* Copy();
    virtual ClassId GetClassId();
    virtual boolean IsA(ClassId);
};

#endif
