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
 * Implementation of various dialog boxes.
 */

#include <Unidraw/dialogs.h>
#include <Unidraw/globals.h>
#include <Unidraw/iterator.h>

#include <IV-2_6/InterViews/adjuster.h>
#include <IV-2_6/InterViews/border.h>
#include <IV-2_6/InterViews/box.h>
#include <IV-2_6/InterViews/button.h>
#include <InterViews/event.h>
#include <IV-2_6/InterViews/filebrowser.h>
#include <IV-2_6/InterViews/frame.h>
#include <IV-2_6/InterViews/glue.h>
#include <IV-2_6/InterViews/matcheditor.h>
#include <IV-2_6/InterViews/message.h>
#include <IV-2_6/InterViews/scroller.h>
#include <IV-2_6/InterViews/sensor.h>
#include <IV-2_6/InterViews/streditor.h>

#include <IV-2_6/_enter.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*****************************************************************************/

static void ChangeMsg (const char* name, MarginFrame* frame) {
    Interactor* msg;

    if (*name == '\0') {
        msg = new VGlue(0, 0);
    } else {
        msg = new Message(name);
    }
    frame->Insert(msg);
    frame->Change(msg);
}

/*****************************************************************************/

BasicDialog::BasicDialog (
    ButtonState* bs, const char* t, const char* subt, Alignment a
) : Dialog(bs, nil, a) { 
    Init(t, subt);
}

BasicDialog::BasicDialog (
    const char* c, ButtonState* bs, const char* t, const char* subt,Alignment a
) : Dialog(c, bs, nil, a) {
    Init(t, subt);
}

void BasicDialog::Init (const char* t, const char* subt) {
    if (*t == '\0') {
        _title = new MarginFrame(new VGlue(0, 0));
    } else {
        _title = new MarginFrame(new Message(t));
    }
    if (*subt == '\0') {
        _subtitle = new MarginFrame(new VGlue(0, 0));
    } else {
        _subtitle = new MarginFrame(new Message(subt));
    }
}

void BasicDialog::SetTitle (const char* name) { ChangeMsg(name, _title); }
void BasicDialog::SetSubtitle (const char* name) { ChangeMsg(name, _subtitle); }

void BasicDialog::Forward (Event& e) {
    if (IsAChild(e.target)) {
        e.target->Handle(e);
    } else {
        Handle(e);
    }
}    

boolean BasicDialog::IsAChild (Interactor* i) {
    Scene* parent = i->Parent();

    while (parent != nil) {
        if (parent == this) {
            return true;
        }
        parent = parent->Parent();
    }
    return false;
}

/*****************************************************************************/

AcknowledgeDialog::AcknowledgeDialog (
    const char* title, const char* subtitle
) : BasicDialog(new ButtonState, title, subtitle) {
    Insert(Interior());
    input = new Sensor(noEvents);
    input->Catch(KeyEvent);
}    

void AcknowledgeDialog::Acknowledge () {
    Event e;
    int v = 0;

    state->SetValue(v);
    do {
        Read(e);
        if (e.eventType == KeyEvent) {
            state->SetValue(e.keystring[0]);
        } else {
            Forward(e);
        }
        state->GetValue(v);
    } while (v == 0);
}

Interactor* AcknowledgeDialog::Interior () {
    const int space = round(.5*cm);

    return new MarginFrame(
        new VBox(
            new HBox(_title, new HGlue),
            new HBox(_subtitle, new HGlue),
            new VGlue(space),
            new HBox(
                new HGlue,
                new PushButton("  OK  ", state, 1),
                new HGlue
            )
        ), space, space/2, 0
    );
}

/*****************************************************************************/

ConfirmDialog::ConfirmDialog (
    const char* title, const char* subtitle
) : BasicDialog(new ButtonState, title, subtitle) {
    Insert(Interior());
    input = new Sensor(noEvents);
    input->Catch(KeyEvent);
}    

inline boolean Confirmed (int v) {
    return v == 'y' || v == 'n' || v == '\007';
}

char ConfirmDialog::Confirm () {
    Event e;
    int v = 0;

    state->SetValue(v);
    do {
        Read(e);
        if (e.eventType == KeyEvent) {
            state->SetValue(e.keystring[0]);
        } else {
            Forward(e);
        }
        state->GetValue(v);
    } while (!Confirmed(v));

    return v;
}

Interactor* ConfirmDialog::Interior () {
    const int space = round(.5*cm);

    return new MarginFrame(
        new VBox(
            new HBox(_title, new HGlue),
            new HBox(_subtitle, new HGlue),
            new VGlue(space),
            new HBox(
                new HGlue,
                new PushButton("  Yes  ", state, 'y'),
                new HGlue(space, 0),
                new PushButton("  No  ", state, 'n'),
                new HGlue(space, 0), 
                new PushButton("Cancel", state, '\007'),
                new HGlue
            )
        ), space, space/2, 0
    );
}

/*****************************************************************************/

UChooser::UChooser (
    const char* t, const char* subt, const char* acceptLabel, const char* samp
) : StringChooser(new ButtonState, 10, 24, "") { 
    Init(t, subt);
    Insert(Interior(acceptLabel));
    _sedit->Message(samp);
}

UChooser::UChooser (
    ButtonState* bs, int r, int c, Alignment a
) : StringChooser(bs, a)  {
    StringChooser::Init(new StringEditor(bs, ""), new StringBrowser(bs, r, c));
}

void UChooser::Init (const char* t, const char* subt) {
    if (*t == '\0') {
        _title = new MarginFrame(new VGlue(0, 0));
    } else {
        _title = new MarginFrame(new class Message(t));
    }
    if (*subt == '\0') {
        _subtitle = new MarginFrame(new VGlue(0, 0));
    } else {
        _subtitle = new MarginFrame(new class Message(subt));
    }
}

void UChooser::Clear () { _browser->Clear(); }

void UChooser::Include (const char* s) {
    _browser->Insert(s, Position(s));
}

void UChooser::Exclude (const char* s) { 
    _browser->Remove(_browser->Index(s));
}

void UChooser::SetTitle (const char* name) { ChangeMsg(name, _title); }

void UChooser::SetSubtitle (const char* name) {
    ChangeMsg(name, _subtitle);
}

int UChooser::Position (const char* s) {
    int i;

    for (i = 0; i < _browser->Count(); ++i) {
        if (strcmp(s, _browser->String(i)) < 0) {
            break;
        }
    }
    return i;
}

void UChooser::Reconfig () {
    StringChooser::Reconfig();
    shape->hstretch = shape->vstretch = 0;
}

Interactor* UChooser::AddScroller (Interactor* i) {
    return new HBox(
        new MarginFrame(i, 2),
        new VBorder,
        new VBox(
            new UpMover(i, 1),
            new HBorder,
            new VScroller(i),
            new HBorder,
            new DownMover(i, 1)
        )                
    );
}

Interactor* UChooser::Interior (const char* acptlbl) {
    const int space = round(.5*cm);
    VBox* titleblock = new VBox(
        new HBox(_title, new HGlue),
        new HBox(_subtitle, new HGlue)
    );

    return new MarginFrame(
        new VBox(
            titleblock,
            new VGlue(space, 0),
            new Frame(new MarginFrame(_sedit, 2)),
            new VGlue(space, 0),
            new Frame(AddScroller(_browser)),
            new VGlue(space, 0),
            new HBox(
                new VGlue(space, 0),
                new HGlue,
                new PushButton("Cancel", state, '\007'),
                new HGlue(space, 0),
                new PushButton(acptlbl, state, '\r')
            )
        ), space, space/2, 0
    );
}

/*****************************************************************************/

class PrintBS : public ButtonState {
public:
    PrintBS(PrintDialog*, boolean);
    virtual void Notify();
private:
    PrintDialog* _dialog;
};

PrintBS::PrintBS (PrintDialog* pd, boolean v) : ButtonState(v) {
    _dialog = pd;
}

void PrintBS::Notify () {
    ButtonState::Notify();
    _dialog->ToPrinter(boolean(value));
}

/*****************************************************************************/

static const char* DefaultPrintCmd () {
    static char buf[CHARBUFSIZE];
    const char* print_cmd = getenv("PRINT_CMD");

    if (print_cmd == nil) {
        const char* printer_name = getenv("PRINTER");

        if (printer_name == nil) {
            printer_name = "<printer name>";
        }

        sprintf(buf, "lpr -P%s", printer_name);
        print_cmd = buf;
    }
    return print_cmd;
}

PrintDialog::PrintDialog (boolean to_printer) : FileChooser(
    new ButtonState, ".", 10, 24, Center
) {
    _last_print_cmd = strnew(DefaultPrintCmd());
    _last_file_name = strnew("");
    _to_printer = -1;
    _dest = new PrintBS(this, to_printer);

    FileChooser::Init("", "Generate PostScript and");
    Insert(Interior());

    ToPrinter(to_printer);
}

PrintDialog::~PrintDialog () {
    delete _last_print_cmd;
    delete _last_file_name;
}

void PrintDialog::ToPrinter (boolean to_printer) {
    if (to_printer != _to_printer) {
        _to_printer = to_printer;

        if (to_printer) {
            delete _last_file_name;
            _last_file_name = strnew(Choice());

            browser()->Clear();
            PrintDialog::Message(_last_print_cmd);

        } else {
            delete _last_print_cmd;
            _last_print_cmd = strnew(Choice());

            browser()->Update();
            PrintDialog::Message(_last_file_name);
        }
        SelectMessage();
    }
}

boolean PrintDialog::ToPrinter () { return _to_printer; }

void PrintDialog::UpdateEditor () {
    if (!ToPrinter()) {
        FileChooser::UpdateEditor();
    }
}

void PrintDialog::UpdateBrowser () {
    if (!ToPrinter()) {
        FileChooser::UpdateBrowser();
    }
}

Interactor* PrintDialog::Interior () {
    const int space = round(.5*cm);
    VBox* titleblock = new VBox(
        new HBox(title, new HGlue),
        new HBox(subtitle, new HGlue)
    );

    return new MarginFrame(
        new VBox(
            titleblock,
            new HBox(
                new HGlue(space, 0),
                new VBox(
                    new VGlue(space/3, 0),
                    new RadioButton(
                        "send to printer via a command, or ", _dest, true
                    ),
                    new VGlue(space/3, 0),
#ifdef __DECCXX
		    /* is passing an const unsigned 0 ambiguous here? */
                    new RadioButton("save in file:", _dest, int(false))
#else
                    new RadioButton("save in file:", _dest, false)
#endif
                )
            ),
            new VGlue(space, 0),
            new VBox(
                new Frame(new MarginFrame(_sedit, 2)),
                new VGlue(space, 0),
                new Frame(AddScroller(browser())),
                new VGlue(space, 0)
            ),
            new HBox(
                new HGlue,
                new PushButton("Cancel", state, '\007'),
                new HGlue(space, 0),
                new PushButton("  OK  ", state, '\r')
            )
        ), space, space/2, 0
    );
}

/*****************************************************************************/

GridDialog::GridDialog () : BasicDialog(
    new ButtonState, "", "Enter X and Y grid spacing:"
) {
    _medit = new MatchEditor(state, "9999999999999999999");
    _medit->Message("");
    _medit->Match("%f %f");

    _units = new ButtonState('p');
    input = new Sensor;
    input->Catch(KeyEvent);

    Insert(Interior());
    SelectMessage();
}

void GridDialog::Handle (Event& e) {
    if (e.eventType == KeyEvent) {
        _medit->Handle(e);
    }
}

boolean GridDialog::Accept () {
    Event e;
    int v = 0;

    state->SetValue(0);
    _medit->Edit();
    state->GetValue(v);

    while (v == 0) {
	Read(e);
	Forward(e);
	state->GetValue(v);
    }

    return v == '\r';
}

void GridDialog::SelectMessage () {
    _medit->Select(0, strlen(_medit->Text()));
}

Interactor* GridDialog::Interior () {
    const int space = round(.5*cm);

    VBox* titleblock = new VBox(
        new HBox(_title, new HGlue),
        new HBox(_subtitle, new HGlue)
    );

    HBox* rbuttons = new HBox(
        new RadioButton("Pixels", _units, 'p'),
        new HGlue(space, 0),
        new RadioButton("Points", _units, 'o'),
        new HGlue(space, 0),
        new RadioButton("Centimeters", _units, 'c'),
        new HGlue(space, 0),
        new RadioButton("Inches", _units, 'i')
    );
    rbuttons->Insert(new HGlue);

    return new MarginFrame(
        new VBox(
            titleblock,
            new VGlue(space),
            new Frame(new MarginFrame(_medit, 2)),
            new VGlue(space/2, 0),
	    rbuttons,
            new VGlue(space),
            new HBox(
                new HGlue,
                new PushButton("Cancel", state, '\007'),
		new HGlue(space, 0),
                new PushButton("  OK  ", state, '\r')
            )
        ), space, space/2, 0
    );
}
    
void GridDialog::GetValues (float& xincr, float& yincr) {
    char spacing[CHARBUFSIZE];
    strcpy(spacing, _medit->Text());
    int args = sscanf(spacing, "%f %f", &xincr, &yincr);

    if (args == 0) {
	xincr = yincr = 0.0;
	
    } else {
	if (args == 1) {
	    yincr = xincr;
	}
	int unit;
	_units->GetValue(unit);

	switch (unit) {
	     case 'i':   xincr *= inches; yincr *= inches; break;
	     case 'o':   xincr *= points; yincr *= points; break;
	     case 'c':   xincr *= cm; yincr *= cm; break;
	}
    }
}
