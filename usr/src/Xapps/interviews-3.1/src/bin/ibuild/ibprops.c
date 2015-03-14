/*
 * Copyright (c) 1991 Stanford University
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
 * Implementation of Props and PropsEditor.
 */

#include "ibprops.h"
#include "ibinteractor.h"

#include <InterViews/button.h>
#include <InterViews/font.h>
#include <InterViews/painter.h>
#include <InterViews/sensor.h>
#include <InterViews/textbuffer.h>
#include <string.h>
#include <ctype.h>

/*****************************************************************************/

static const int gap = 2;
static const int rows = 24;
static const int cols = 80;
static const int tabs = 4;

/*****************************************************************************/

PropsEditor::PropsEditor (
    ButtonState* bs, Props* props
) : TextEditor(rows/2, cols/2, tabs, Reversed) {
    SetClassName("PropsEditor");

    _state = bs;
    const char* text = props->GetPropsText();
    _props = new char[rows*cols];
    strcpy(_props, text);

    input = new Sensor;
    input->Catch(DownEvent);
    input->Catch(KeyEvent);
    Edit(new TextBuffer(_props, strlen(text), rows*cols));
    EndOfText();
}

PropsEditor::~PropsEditor () {
    delete text;
    delete _props;
}

void PropsEditor::Handle (Event& e) {
    boolean done = false;
    do {
        if (e.eventType == KeyEvent && e.len > 0) {
            HandleChar(e.keystring[0]);

        } else if (e.eventType == DownEvent) {
            if (e.target == this) {
                switch (e.button) {
                    case LEFTMOUSE:     LeftMouse(e); break;
                    case MIDDLEMOUSE:   GrabScroll(e); break;
                    case RIGHTMOUSE:    RateScroll(e); break;
                }
            } else {
                UnRead(e);
                done = true;
            }
        }
        if (!done) {
            Read(e);
        }
    } while (!done);
}

void PropsEditor::LeftMouse (Event& e) {
    Select(Locate(e.x, e.y));
    do {
        ScrollToView(e.x, e.y);
        SelectMore(Locate(e.x, e.y));
        Poll(e);
    } while (e.leftmouse);
}

void PropsEditor::HandleChar (char c) {
    char copy = c;
    switch (c) {
    case '\026':  ForwardPage(1); break;
    case '\001':  BeginningOfLine(); break;
    case '\005':  EndOfLine(); break;
    case '\006':  ForwardCharacter(1); break;
    case '\002':  BackwardCharacter(1); break;
    case '\016':  ForwardLine(1); break;
    case '\020':  BackwardLine(1); break;
    case '\004':
        if (Dot() != Mark()) {
            DeleteSelection();
        } else {
            DeleteText(1);
        }
        break;

    case '\010':
    case '\177':
        if (Dot() != Mark()) {
            DeleteSelection();
        } else {
            DeleteText(-1);
        }
        break;
        
    case '\011':
        copy = '\t'; 
        DeleteSelection();
        InsertText(&copy, 1);
        break;

    case '\015':
        copy = '\n'; 
        DeleteSelection();
        InsertText(&copy, 1);
        break;

    default:
        if (!iscntrl(c)) {
            DeleteSelection();
            InsertText(&copy, 1);
        }
        break;
    }
    ScrollToSelection();
}

