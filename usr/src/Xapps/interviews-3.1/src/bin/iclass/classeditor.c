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
 * ClassEditor implementation
 */

#include "classeditor.h"

#include <InterViews/button.h>
#include <InterViews/sensor.h>
#include <InterViews/shape.h>
#include <InterViews/textbuffer.h>
#include <InterViews/textdisplay.h>

#include <stdlib.h>

/*****************************************************************************/

static const char FirstLine = 'g';
static const char LastLine = 'G';
static const char NextLine = 'n';
static const char PrevLine= 'p';
static const char ScrollDown = 'j';
static const char ScrollUp = 'k';
static const char PageDown = ' ';
static const char PageUp = 'b';
static const char HalfPageDown = 'd';
static const char HalfPageUp = 'u';

/*****************************************************************************/

ClassEditor::ClassEditor (
    ButtonState* bs, int r, int c, int t, int h, const char* done
) : TextEditor(r, c, t, h) {
    Init(bs, done);
}

ClassEditor::ClassEditor (
    ButtonState* bs, const char* name, int r, int c, int t, int h,
    const char* done
) : TextEditor(name, r, c, t, h) {
    Init(bs, done);
}

void ClassEditor::Init (ButtonState* bs, const char* done) {
    SetClassName("ClassEditor");
    input = new Sensor;
    input->Catch(DownEvent);
    input->Catch(KeyEvent);

    _state = bs;
    _done = done;
}

void ClassEditor::ScrollBy (int lines) {
    TextEditor::ScrollBy(0, -lines*shape->vunits); 
        // explicit TextEditor:: works around cfront 1.2 bug

    int line = text->LineNumber(Dot());
    Coord b = display->Base(line);
    Coord t = display->Top(line);

    if (b < 0) {
        Select(Locate(0, 0));
    } else if (t > ymax) {
        Select(Locate(0, ymax));
    }
}

void ClassEditor::Handle (Event& e) {
    if (e.eventType == KeyEvent) {
        HandleKeyEvent(e);

    } else {
        boolean done = false;

        do {
            switch (e.eventType) {
            case DownEvent:
                done = HandleDownEvent(e);
                break;

            case KeyEvent:
                done = HandleKeyEvent(e);
                break;
            }
            if (!done) {
                Read(e);
            }
        } while (!done);
    }
}

boolean ClassEditor::HandleKeyEvent (Event& e) {
    boolean done = false;

    if (e.len != 0) {
        done = HandleChar(e.keystring[0]);
    }
    return done;
}    

boolean ClassEditor::HandleDownEvent (Event& e) {
    boolean done = true;

    if (e.target == this) {
        switch (e.button) {
            case LEFTMOUSE:     Select(Locate(e.x, e.y)); done = false; break;
            case MIDDLEMOUSE:   GrabScroll(e); break;
            case RIGHTMOUSE:    RateScroll(e); break;
        }

    } else {
        UnRead(e);
    }
    return done;
}

boolean ClassEditor::HandleChar (char c) {
    boolean done = false;
    int lines = display->LineNumber(0) - display->LineNumber(ymax) + 1;
    int i;

    switch (c) {
    case FirstLine:
        BeginningOfText();
        ScrollToSelection();
        break;

    case LastLine:
        EndOfText();
        ScrollToSelection();
        break;

    case PrevLine:
        BackwardLine();
        ScrollToSelection();
        break;

    case NextLine:
        ForwardLine();
        ScrollToSelection();
        break;

    case PageDown:
        ForwardPage();
        ScrollToSelection();
        break;

    case PageUp:
        BackwardPage(1);
        ScrollToSelection();
        break;

    case HalfPageDown:
        ForwardLine(lines/2);
        ScrollToSelection();
        break;

    case HalfPageUp:
        BackwardLine(lines/2);
        ScrollToSelection();
        break;

    case ScrollDown:
        ScrollBy(1);
        break;

    case ScrollUp:
        ScrollBy(-1);
        break;

    default:
        for (i = 0; _done[i] != '\0'; ++i) {
            if (c == _done[i]) {
                _state->SetValue(c);
                done = true;
            }
        }
        break;
    }
    return done;
}
