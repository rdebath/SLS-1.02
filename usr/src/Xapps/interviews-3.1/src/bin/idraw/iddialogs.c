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
 * Implementation of Idraw-specific dialog boxes.
 */

#include "iddialogs.h"

#include <Unidraw/globals.h>

#include <InterViews/box.h>
#include <InterViews/button.h>
#include <InterViews/event.h>
#include <InterViews/frame.h>
#include <InterViews/glue.h>
#include <InterViews/matcheditor.h>
#include <InterViews/sensor.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*****************************************************************************/

MoveDialog::MoveDialog () : BasicDialog(
    new ButtonState, "", "Enter X and Y movement:"
) {
    _medit = new MatchEditor(state, "9999999999999999999");
    _medit->Message("");
    _medit->Match("%f %f", false);

    _units = new ButtonState('p');
    input = new Sensor;
    Ref(input);
    input->Catch(KeyEvent);

    Insert(Interior());
    SelectMessage();
}

void MoveDialog::Handle (Event& e) { _medit->Handle(e); }

boolean MoveDialog::Accept () {
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

void MoveDialog::SelectMessage () {
    _medit->Select(0, strlen(_medit->Text()));
}

Interactor* MoveDialog::Interior () {
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
    
void MoveDialog::GetValues (float& x, float& y) {
    char* movement = nil;
    movement = strnew(_medit->Text());

    if (sscanf(movement,"%f %f",&x, &y) != 2) {
	x = y = 0.0;

    } else {
	int unit;
	_units->GetValue(unit);

	switch (unit) {
	     case 'i':   x *= inches; y *= inches; break;
	     case 'o':   x *= points; y *= points; break;
	     case 'c':   x *= cm; y *= cm; break;
	}
    }
    delete movement;
}

/*****************************************************************************/

ScaleDialog::ScaleDialog () : BasicDialog(
    new ButtonState, "", "Enter X and Y scaling:"
) {
    _medit = new MatchEditor(state, "9999999999999999999");
    _medit->Message("");
    _medit->Match("%f %f", false);

    input = new Sensor;
    Ref(input);
    input->Catch(KeyEvent);

    Insert(Interior());
    SelectMessage();
}

void ScaleDialog::Handle (Event& e) { _medit->Handle(e); }

boolean ScaleDialog::Accept () {
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

void ScaleDialog::SelectMessage () {
    _medit->Select(0, strlen(_medit->Text()));
}

Interactor* ScaleDialog::Interior () {
    const int space = round(.5*cm);

    VBox* titleblock = new VBox(
        new HBox(_title, new HGlue),
        new HBox(_subtitle, new HGlue)
    );

    return new MarginFrame(
        new VBox(
            titleblock,
            new VGlue(space),
            new Frame(new MarginFrame(_medit, 2)),
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
    
void ScaleDialog::GetValues (float& x, float& y) {
    char* movement = nil;
    movement = strnew(_medit->Text());

    if (sscanf(movement,"%f %f",&x, &y) != 2) {
	x = y = 1.0;
    }
    delete movement;
}

/*****************************************************************************/

RotateDialog::RotateDialog () : BasicDialog(
    new ButtonState, "", "Enter rotation in degrees:"
) {
    _medit = new MatchEditor(state, "9999999999999999999");
    _medit->Message("");
    _medit->Match("%f", false);

    input = new Sensor;
    Ref(input);
    input->Catch(KeyEvent);

    Insert(Interior());
    SelectMessage();
}

void RotateDialog::Handle (Event& e) { _medit->Handle(e); }

boolean RotateDialog::Accept () {
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

void RotateDialog::SelectMessage () {
    _medit->Select(0, strlen(_medit->Text()));
}

Interactor* RotateDialog::Interior () {
    const int space = round(.5*cm);

    VBox* titleblock = new VBox(
        new HBox(_title, new HGlue),
        new HBox(_subtitle, new HGlue)
    );

    return new MarginFrame(
        new VBox(
            titleblock,
            new VGlue(space),
            new Frame(new MarginFrame(_medit, 2)),
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
    
void RotateDialog::GetValue (float& angle) {
    char* movement = nil;
    movement = strnew(_medit->Text());

    if (sscanf(movement,"%f",&angle) != 1) {
	angle = 0.0;
    }
    delete movement;
}
