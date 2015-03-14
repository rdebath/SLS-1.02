/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Stanford and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Stanford and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 *
 * IN NO EVENT SHALL STANFORD OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

/*
 * Message class implementation.
 */

#include <InterViews/font.h>
#include <IV-2_6/InterViews/message.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/shape.h>
#include <stdlib.h>
#include <string.h>

Message::Message(const char* msg, Alignment al, int pad, int hstr, int vstr) {
    Init(msg, al, pad, hstr, vstr);
}

Message::Message(
    const char* name,
    const char* msg, Alignment al, int pad, int hstr, int vstr
) : Interactor(name) {
    Init(msg, al, pad, hstr, vstr);
}

void Message::Init (const char* t, Alignment a, int p, int hstr, int vstr) {
    SetClassName("Message");

    if (t == nil) {
	text = nil;
    } else {
	text = new char[strlen(t)+1];
	strcpy(text, t);
    }

    alignment = a;
    pad = p;
    shape->hstretch = hstr;
    shape->vstretch = vstr;
    highlighted = false;
}

Message::~Message() { delete text; }

void Message::Reconfig () {
    const char* a = GetAttribute("text");
    if (a != nil) {
	delete text;
	text = new char[strlen(a)+1];
	strcpy(text, a);
    }
    a = GetAttribute("padding");
    if (a != nil) {
	pad = atoi(a);
    }
    const Font* f = output->GetFont();
    shape->width = pad + f->Width(text) + pad;
    shape->height = pad + f->Height() + pad;
    shape->hshrink = pad + pad;
    shape->vshrink = pad + pad;
}

void Message::Realign (Alignment a) {
    alignment = a;
    Draw();
}

void Message::Redraw (IntCoord l, IntCoord b, IntCoord r, IntCoord t) {
    IntCoord x = 0, y = 0;
    Align(alignment, shape->width, shape->height, x, y);
    output->Clip(canvas, l, b, r, t);
    if (highlighted) {
	output->SetColors(output->GetBgColor(), output->GetFgColor());
    }
    output->ClearRect(canvas, l, b, r, t);
    output->Text(canvas, text, x + pad, y + pad);
    if (highlighted) {
	output->SetColors(output->GetBgColor(), output->GetFgColor());
    }
    output->NoClip();
}

void Message::Highlight (boolean b) {
    if (highlighted != b) {
	highlighted = b;
	Draw();
    }
}
