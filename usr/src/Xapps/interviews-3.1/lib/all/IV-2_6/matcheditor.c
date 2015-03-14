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
 * MatchEditor - StringEditor with pattern matching
 */

#include <IV-2_6/InterViews/matcheditor.h>
#include <IV-2_6/InterViews/textbuffer.h>
#include <IV-2_6/InterViews/world.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

MatchEditor::MatchEditor (
    ButtonState* s, const char* sample, const char* done
) : StringEditor(s, sample, done) {
    Init();
}

MatchEditor::MatchEditor (
    const char* name, ButtonState* s, const char* sample, const char* done
) : StringEditor(name, s, sample, done) {
    Init();
}

MatchEditor::~MatchEditor () { }

void MatchEditor::Init () {
    SetClassName("MatchEditor");
    Match("%[]", true);
}

void MatchEditor::Match (const char* p, boolean m) {
    for (char* pp = pattern; *p != '\0'; ++p, ++pp) {
        *pp = *p;
        if (*p == '%') {
            ++p;
            ++pp;
            if (*p != '%' && *p != '*') {
                *pp = '*';
                ++pp;
            }
            *pp = *p;
        }
    }
    *pp = '\0';
    strcat(pattern, "%*c"); /* workaround bug in Sparc sscanf */
    match_on_keystroke = m;
}

boolean MatchEditor::HandleChar (char c) {
    boolean done = StringEditor::HandleChar(c);
    if (done || (!iscntrl(c) && match_on_keystroke)) {
        char buf[1000];
        int length = text->Length();
        strncpy(buf, text->Text(), length);
        while (length > 0) {
            buf[length] = '\0';
            if (sscanf(buf, pattern) == EOF) {
                break;
            }
            --length;
        }
        if (length != text->Length()) {
            GetWorld()->RingBell(1);
            Select(length, text->Length());
        }
    }
    if (done && left == right) {
        return true;
    } else {
        return false;
    }
}
