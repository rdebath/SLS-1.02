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
 * StringEditor - interactive editor for character strings
 */

#ifndef ivlook2_6_streditor_h
#define ivlook2_6_streditor_h

#include <IV-2_6/InterViews/interactor.h>

#include <IV-2_6/_enter.h>

static const char* SEDone = "\r\t\007\033";

static const char SEBeginningOfLine = '\001';
static const char SEEndOfLine = '\005';
static const char SESelectAll = '\025';
static const char SESelectWord = '\027';
static const char SEPreviousCharacter = '\002';
static const char SENextCharacter = '\006';
static const char SEDeleteNextCharacter = '\004';
static const char SEDeletePreviousCharacter = '\177';
static const char SEDeletePreviousCharacterAlt = '\010';

class ButtonState;
class TextDisplay;
class TextBuffer;

class StringEditor : public Interactor {
public:
    StringEditor(ButtonState*, const char* sample, const char* done = SEDone);
    StringEditor(
        const char* name, ButtonState*, const char*, const char* = SEDone
    );
    virtual ~StringEditor();

    void Message(const char* text);
    void Select(int);
    void Select(int left, int right);
    void Edit();
    void Edit(const char* text, int left, int right);

    const char* Text();

    virtual void Handle(Event&);
    virtual void Reconfig();
protected:
    virtual boolean HandleChar(char);
    void InsertText(const char*, int);
    void DoSelect(int left, int right);

    virtual void Redraw(IntCoord, IntCoord, IntCoord, IntCoord);
    virtual void Resize();

    TextBuffer* text;
    int left, right;
    ButtonState* subject;
    char* done;
    char* sample;
    char* buffer;
    int size;
    TextDisplay* display;
private:
    void Init(ButtonState*, const char*, const char*);
};

#include <IV-2_6/_leave.h>

#endif
