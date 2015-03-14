/*
 * Copyright (c) 1987-1992 Stanford University
 * Copyright (c) 1991-1992 Silicon Graphics, Inc.
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

#ifndef dialogs_h
#define dialogs_h

#include <InterViews/enter-scope.h>

class Dialog;
class FieldEditor;
class FileChooser;
class Style;
class Window;

class Chooser {
public:
    Chooser(const char*, const char* prompt);
    virtual ~Chooser();

    virtual const char* post(Window*, const char* ext);
private:
    FileChooser* chooser_;
    Style* style_;
};

class Asker {
public:
    Asker(const char*, const char* prompt);
    virtual ~Asker();

    virtual const char* post(Window*, const char* initial);
private:
    Dialog* dialog_;
    FieldEditor* editor_;

    void accept_editor(FieldEditor*);
    void cancel_editor(FieldEditor*);
    void accept();
    void cancel();
};

class Confirmer {
public:
    Confirmer(const char*, const char* prompt);
    virtual ~Confirmer();

    virtual int post(Window*);
private:
    Dialog* dialog_;
    boolean confirm_;

    void yes();
    void no();
    void cancel();
};

class Reporter {
public:
    Reporter(const char*, const char* message);
    virtual ~Reporter();

    virtual void post(Window*);
private:
    Dialog* dialog_;

    void ok();
};

#endif
