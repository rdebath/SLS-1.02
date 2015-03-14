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
 * CompletionEditor - StringEditor with completion
 */

#include <IV-2_6/InterViews/compeditor.h>
#include <IV-2_6/InterViews/textbuffer.h>
#include <IV-2_6/InterViews/world.h>
#include <OS/math.h>
#include <string.h>

CompletionEditor::CompletionEditor (
    ButtonState* s, const char* sample, const char* done
) : StringEditor(s, sample, done) {
    Init();
}

CompletionEditor::CompletionEditor (
    const char* name, ButtonState* s, const char* sample, const char* done
) : StringEditor(name, s, sample, done) {
    Init();
}

CompletionEditor::~CompletionEditor() { }

void CompletionEditor::Init () {
    complete_ = CEComplete;
    completions_ = nil;
    count_ = 0;
}

void CompletionEditor::Completions (const char* list[], int c, char comp) {
    complete_ = comp;
    completions_ = list;
    count_ = c;
}

boolean CompletionEditor::HandleChar (char c) {
    if (c == complete_) {
        InsertText("", 0);

        const char* best = nil;
        int match = 0;
        int length = text->LineOffset(text->EndOfLine(0));
        for (int i = 0; i < count_; ++i) {
            for (int j = 0; ; ++j) {
                char c = completions_[i][j];
                if (j < length) {
                    if (text->Char(j) != c) {
                        match = Math::max(match, j);
                        break;
                    }
                } else {
                    if (best == nil) {
                        best = completions_[i];
                        match = strlen(best);
                        break;
                    } else {
                        if (c == '\0' || best[j] != c) {
                            match = Math::min(match, j);
                            break;
                        }
                    }
                }
            }
        }

        Select(match, length);
        if (match > length) {
            InsertText(best+length, match-length);
        } else if (best != nil && best[match] != '\0') {
            GetWorld()->RingBell(1);
        }
        return false;
    } else {
        return StringEditor::HandleChar(c);
    }
}
