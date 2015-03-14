/*
 * Copyright (c) 1989 Stanford University
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
 * IClass - interface to choosing classes.
 */

#ifndef iclass_h
#define iclass_h

#include <InterViews/scene.h>

class ButtonState;
class ClassBuffer;
class ClassEditor;
class CompletionEditor;
class FileDialog;
class MarginFrame;
class StringBrowser;
class StringDialog;
class TextBuffer;

class IClass : public MonoScene {
public:
    IClass(ClassBuffer*);
    virtual ~IClass();

    void Run();
    boolean Command(char);

    void ScanCmd();
    void ClearCmd();
    void QuitCmd();
    void ForwardSearchCmd();
    void BackwardSearchCmd();
    void GotoCmd();

    virtual void Handle(Event&);
    virtual void Update();
private:
    void Init(ClassBuffer*);

    Interactor* Commands();
    Interactor* Classes();
    Interactor* SelectedClass();
    Interactor* Parents();
    Interactor* Children();
    Interactor* Editor();
    Interactor* Interior();
    Interactor* AddScroller(Interactor*);

    void InsertDialog(Interactor*);
    void RemoveDialog(Interactor*);

    void Visit(const char*);
    void Complain(const char* = nil);
    boolean ForwardSearch(const char*);
    boolean BackwardSearch(const char*);

    void UpdateClassesBrowser();
    void UpdateParentBrowser();
    void UpdateChildBrowser();
    void UpdateCurClass();
    void UpdateCurClass(const char*);
    void UnselectBrowsers();

    void SelectCurClass();
    void UnselectCurClass();
private:
    ButtonState* _state;
    ClassBuffer* _cbuf;
    CompletionEditor* _curClass;
    Interactor* _focus;
    StringBrowser* _classes;
    StringBrowser* _children;
    StringBrowser* _parents;
    MarginFrame* _fileIndic;
    FileDialog* _scanner;
    StringDialog* _fwdSearch;
    StringDialog* _bwdSearch;
    StringDialog* _goto;

    TextBuffer* _text;
    ClassEditor* _editor;
    const char** _completions;
    char* _lastFile;
    char* _buf;
    int _bufsize;
};

#endif
