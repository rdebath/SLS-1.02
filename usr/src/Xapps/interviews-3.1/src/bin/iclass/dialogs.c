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
 * Implementation of various dialogs boxes.
 */

#include "dialogs.h"

#include <InterViews/adjuster.h>
#include <InterViews/border.h>
#include <InterViews/box.h>
#include <InterViews/button.h>
#include <InterViews/filebrowser.h>
#include <InterViews/frame.h>
#include <InterViews/glue.h>
#include <InterViews/message.h>
#include <InterViews/scrollbar.h>
#include <InterViews/sensor.h>
#include <InterViews/streditor.h>

#include <stdlib.h>
#include <string.h>
#include <osfcn.h>

#if !defined(AIXV3) && !defined(hpux) && !(defined(sun) && OSMajorVersion >= 5) && !defined(linux)
/* sigh, not all systems have this prototype */
extern "C" {
    char* getcwd(char*, int);
}
#endif

/*****************************************************************************/

static const float fspace = .375;                // space in cm
static const int MAX_PATH_LENGTH = 256;

/*****************************************************************************/

BasicDialog::BasicDialog () : Dialog(new ButtonState, nil) {
    input = new Sensor;
    input->Catch(KeyEvent);
}

boolean BasicDialog::Accept () {
    Event e;
    int v = 0;

    state->SetValue(0);

    do {
	Read(e);
	if (e.target == nil) {
	    break;
	}
        if (!KeyEquiv(e)) {
            Forward(e);
        }
        state->GetValue(v);
    } while (v == 0);

    return v == '\r';
}

void BasicDialog::Forward (Event& e) {
    Coord x = e.x, y = e.y;

    e.target->GetRelative(x, y, this);
    if (x >= 0 && y >= 0 && x <= xmax && y <= ymax) {
	e.target->Handle(e);
    }
}

boolean BasicDialog::KeyEquiv (Event& e) {
    boolean keyEquiv = false;

    if (e.eventType == KeyEvent && e.len > 0) {
        char c = e.keystring[0];

        if (c == '\r' || c == '\004' || c == '\007' || c == '\033') {
            state->SetValue(c);
            keyEquiv = true;
        }
    }
    return keyEquiv;
}

/*****************************************************************************/

AcknowledgeDialog::AcknowledgeDialog (const char* msg, const char* btnLbl) {
    int space = round(fspace*cm);

    Insert(
        new MarginFrame(
            new VBox(
                new Message(msg),
                new VGlue(space),
                new HBox(
                    new HGlue,
                    new PushButton(btnLbl, state, '\r'),
                    new HGlue
                )
            ), space, space, 0
        )
    );
}

void AcknowledgeDialog::Acknowledge () {
    Event e;
    int v = 0;

    state->SetValue(v);

    do {
	Read(e);
	if (e.target == nil) {
	    break;
	}
        if (!KeyEquiv(e)) {
            Forward(e);
        }
	state->GetValue(v);
    } while (v == 0);
}

/*****************************************************************************/

ConfirmDialog::ConfirmDialog (const char* msg) {
    Insert(Interior(msg));
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
	if (e.target == nil) {
	    break;
	}
        if (e.eventType == KeyEvent) {
            state->SetValue(e.keystring[0]);
        } else {
            Forward(e);
        }
        state->GetValue(v);
    } while (!Confirmed(v));

    return v;
}

Interactor* ConfirmDialog::Interior (const char* msg) {
    const int space = round(.5*cm);

    return new MarginFrame(
        new VBox(
            new HBox(new Message(msg), new HGlue),
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

StringDialog::StringDialog (
    const char* msg, const char* sample, const char* confirmLbl
) {
    Init(msg, confirmLbl);
    _sedit->Message(sample);
}

StringDialog::StringDialog (
    const char* msg, int width, const char* confirmLbl
) {
    Init(msg, confirmLbl, width);
}

void StringDialog::Select () {
    _sedit->Select(0, strlen(_sedit->Text()));
}

void StringDialog::Select (int pos) { _sedit->Select(pos); }
void StringDialog::Select (int left, int right) { _sedit->Select(left, right);}

void StringDialog::Init (const char* msg, const char* confirmLbl, int width) {
    int space = round(fspace*cm);
    _sedit = new StringEditor(state, "");
    HBox* framedSedit = new HBox;

    if (width == 0) {
        framedSedit->Insert(_sedit);

    } else {
        framedSedit->Insert(new HGlue);
        framedSedit->Insert(
            new VBox(
                new Frame(new MarginFrame(_sedit, 2)),
                new HGlue(width, 0, 0)
            )
        );
        framedSedit->Insert(new HGlue);
    }

    Insert(
        new MarginFrame(
            new VBox(
                new Message(msg),
                new VGlue(space),
                framedSedit,
                new VGlue(space),
                new HBox(
                    new HGlue,
                    new PushButton(" Cancel ", state, '\007'),
                    new HGlue(space, 0),
                    new PushButton(confirmLbl, state, '\r')
                )
            ), space, space, 0
        )
    );
}

const char* StringDialog::String () { return _sedit->Text(); }

boolean StringDialog::Accept () {
    Event e;
    int v = 0;

    state->SetValue(0);
    Select();
    _sedit->Edit();

    for (;;) {
        state->GetValue(v);
        if (v != 0) {
            break;
        }
	Read(e);
	if (e.target == nil) {
	    break;
	}
        if (!KeyEquiv(e)) {
            Forward(e);
        }
    }

    return v == '\r';
}

/*****************************************************************************/

FileDialog::FileDialog (
    const char* msg, const char* dir, const char* confirmLbl
) {
    int space = round(fspace*cm);
    _browser = new FileBrowser(state, dir, 20, 35, false, Reversed,"\000\007");
    _dirs = new FileBrowser(
        "dirBrowser", state, dir, 20, 24, true, Reversed,"d\007"
    );
    _dirs->SetTextFilter("^$");                 // show directories only
    _dirs->Update();

    _cur_dir = new MarginFrame(new Message("path", FullPath(_dirs)));
    HBox* dirBox = new HBox(
        new Message("Directory: "),
        _cur_dir,
        new HGlue
    );
    dirBox->Propagate(false);

    Insert(
        new MarginFrame(
            new VBox(
                new HBox(
                    new VBox(
                        new Message("Change directory to:", Left, 0, hfil),
                        new Frame(AddScroller(_dirs))
                    ),
                    new HGlue(space, 0),
                    new VBox(
                        new Message(msg, Left, 0, hfil),
                        new Frame(AddScroller(_browser))
                    )
                ),
                new VGlue(space, 0),
                dirBox,
                new VGlue(space, 0),
                new HBox(
                    new HGlue,
                    new PushButton(" Cancel ", state, '\007'),
                    new HGlue(space, 0),
                    new PushButton(confirmLbl, state, '\r')
                )
            ), space, space, 0
        )
    );
}

Interactor* FileDialog::AddScroller (Interactor* i) {
    return new HBox(
        new MarginFrame(i, 2),
        new VBorder,
	new VScrollBar(i)
    );
}

boolean FileDialog::Accept () {
    Event e;
    int v = 0;

    state->SetValue(0);

    do {
	Read(e);
	if (e.target == nil) {
	    break;
	}
        if (!KeyEquiv(e)) {
            Forward(e);
        }
	state->GetValue(v);

	if (v == 'd') {
            if (_dirs->Selections() > 0) {
                int sel = _dirs->Selection();
                char path[MAX_PATH_LENGTH];
                strcpy(path, _dirs->Path(sel));

                _browser->SetDirectory(path);
                _dirs->SetDirectory(path);

                Message* path_msg = new Message("path", FullPath(_dirs));
                _cur_dir->Insert(path_msg);
                _cur_dir->Change(path_msg);
            }
            state->SetValue(0);
            v = 0;
	}
    } while (v == 0);

    return v == '\r';
}

const char* FileDialog::FullPath (const char* relpath) {
    return FullPath(_dirs, relpath);
}

const char* FileDialog::FullPath (FileBrowser* fb, const char* rp) {
    const char* relpath = (rp == nil) ? fb->GetDirectory() : rp;
    char path[MAX_PATH_LENGTH];
    getcwd(path, sizeof(path) - strlen(relpath) - 1);
    strcat(path, "/");
    strcat(path, relpath);
    return fb->Normalize(path);
}
