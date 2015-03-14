/*
 * Copyright (c) 1990, 1991 Stanford University
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

#ifndef unidraw_dialogs_h
#define unidraw_dialogs_h

#include <IV-2_6/InterViews/dialog.h>
#include <IV-2_6/InterViews/filechooser.h>
#include <Unidraw/enter-scope.h>

#include <IV-2_6/_enter.h>

class MarginFrame;
class MatchEditor;
class PrintBS;

class BasicDialog : public Dialog {
public:
    void SetTitle(const char*);
    void SetSubtitle(const char*);
protected:
    BasicDialog(
        ButtonState*, const char* title = "", const char* subtitle = "",
        Alignment = Center
    );
    BasicDialog(
        const char* name, ButtonState*, const char* = "", const char* = "",  
        Alignment = Center
    );

    virtual void Forward(Event&);
    boolean IsAChild(Interactor*);
protected:
    MarginFrame* _title, *_subtitle;
private:
    void Init(const char*, const char*);
};

class AcknowledgeDialog : public BasicDialog {
public:
    AcknowledgeDialog(const char* title, const char* subtitle = "");

    virtual void Acknowledge();
private:
    Interactor* Interior();
};

class ConfirmDialog : public BasicDialog {
public:
    ConfirmDialog(const char* title, const char* subtitle = "");

    virtual char Confirm();
private:
    Interactor* Interior();
};

class UChooser : public StringChooser {
public:
    UChooser(
        const char* title, const char* subtitle,
        const char* acceptLabel, const char* sample
    );

    void Clear();
    void Include(const char*);
    void Exclude(const char*);

    void SetTitle(const char*);
    void SetSubtitle(const char*);

    virtual void Reconfig();
protected:
    UChooser(ButtonState*, int rows, int cols, Alignment);
    void Init(const char* title, const char* subtitle);
    Interactor* Interior(const char* acptlbl);
    Interactor* AddScroller(Interactor*);
    virtual int Position(const char*);
protected:
    MarginFrame* _title, *_subtitle;
};

class PrintDialog : public FileChooser {
public:
    PrintDialog(boolean to_printer = true);
    virtual ~PrintDialog();

    void ToPrinter(boolean);
    boolean ToPrinter();
protected:
    virtual void UpdateEditor();
    virtual void UpdateBrowser();
private:
    Interactor* Interior();
private:
    PrintBS* _dest;
    char* _last_print_cmd;
    char* _last_file_name;
    int _to_printer;
};

class GridDialog : public BasicDialog {
public:
    GridDialog();

    virtual boolean Accept();
    virtual void Handle(Event&);

    virtual void GetValues(float& xincr, float& yincr);
    void SelectMessage();
private:
    Interactor* Interior();
private:
    MatchEditor* _medit;
    ButtonState* _units;
};

#include <IV-2_6/_leave.h>

#endif
