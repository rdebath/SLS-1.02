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
 * Various dialog boxes.
 */

#ifndef dialogs_h
#define dialogs_h

#include <InterViews/dialog.h>

class FileBrowser;
class MarginFrame;
class StringEditor;

class BasicDialog : public Dialog {
public:
    BasicDialog();

    virtual boolean Accept();
protected:
    void Forward(Event&);
    boolean KeyEquiv(Event&);
};

class AcknowledgeDialog : public BasicDialog {
public:
    AcknowledgeDialog(const char* msg, const char* btnLbl = "   OK   ");

    virtual void Acknowledge();
};

class ConfirmDialog : public BasicDialog {
public:
    ConfirmDialog(const char* title);

    virtual char Confirm();
private:
    Interactor* Interior(const char*);
};

class StringDialog : public BasicDialog {
public:
    StringDialog(
        const char* msg, const char* sample = "",
        const char* confirmLbl = "   OK   "
    );
    StringDialog(
        const char* msg, int width,
        const char* confirmLbl = "   OK   "
    );

    const char* String();
    void Select();
    void Select(int);
    void Select(int, int);
    virtual boolean Accept();
private:
    void Init(const char*, const char*, int width = 0);
private:
    StringEditor* _sedit;
};

class FileDialog : public BasicDialog {
public:
    FileDialog(
        const char* msg, const char* dir = "~",
        const char* confirmLbl = "   OK   "
    );

    FileBrowser* GetBrowser();
    const char* FullPath(const char* relpath);
    virtual boolean Accept();
private:
    Interactor* AddScroller(Interactor*);
    const char* FullPath(FileBrowser*, const char* relpath = nil);
private:
    FileBrowser* _browser;
    FileBrowser* _dirs;
    MarginFrame* _cur_dir;
};

inline FileBrowser* FileDialog::GetBrowser () { return _browser; }

#endif
