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
 * ClassEditor - edits a class definition
 */

#ifndef classeditor_h
#define classeditor_h

#include <InterViews/texteditor.h>

static const char* CEDone = "\r\t\007\033";

class ButtonState;

class ClassEditor : public TextEditor {
public:
    ClassEditor(
        ButtonState*, int rows, int cols, int tab, int highlight,
        const char* done = CEDone
    );
    ClassEditor(
        ButtonState*, const char* name, int r, int c, int t, int h,
        const char* done = CEDone
    );

    void ScrollBy(int lines);
    virtual void Handle(Event&);
private:
    boolean HandleDownEvent(Event&);
    boolean HandleKeyEvent(Event&);
    boolean HandleChar(char);
private:
    void Init(ButtonState*, const char*);
private:
    const char* _done;
    ButtonState* _state;
};

#endif
