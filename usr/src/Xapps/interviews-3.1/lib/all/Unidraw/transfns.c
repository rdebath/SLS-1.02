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
 * Implementation of transfer function subclasses.
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/statevar.h>
#include <Unidraw/transfns.h>
#include <Unidraw/unidraw.h>
#include <Unidraw/Components/connector.h>

#include <stream.h>

/*****************************************************************************/

TF_2Port::TF_2Port () { }

void TF_2Port::Evaluate (Path* path) {
    Transfer();

    for (int i = 0; i < Outputs(); ++i) {
        if (ChangedOutput(i)) {
            StateVar* output = GetOutput(i);
            Connector* conn = GetBinding(output);
            conn->Transmit(path);
        }
    }
}

StateVar* TF_2Port::GetInput (int) { return nil; }
StateVar* TF_2Port::GetOutput (int) { return nil; }
void TF_2Port::SetInput (StateVar*, int) { }
void TF_2Port::SetOutput (StateVar*, int) { }
void TF_2Port::Transfer () { }
boolean TF_2Port::ChangedOutput (int) { return false; }

int TF_2Port::Inputs () { return 0; }
int TF_2Port::Outputs () { return 0; }

ClassId TF_2Port::GetClassId () { return TF_2PORT; }

boolean TF_2Port::IsA (ClassId id) {
    return TF_2PORT == id || TransferFunct::IsA(id);
}

/*****************************************************************************/

TF_Direct::TF_Direct (StateVar* input, StateVar* output) {
    _input = input;
    _output = output;
    _changed = false;
}

StateVar* TF_Direct::GetInput (int i) { return (i == 0) ? _input : nil; }
StateVar* TF_Direct::GetOutput (int i) { return (i == 0) ? _output : nil; }
int TF_Direct::Inputs () { return 1; }
int TF_Direct::Outputs () { return 1; }

void TF_Direct::SetInput (StateVar* input, int i) {
    if (i == 0) {
        _input = input;
    }
}

void TF_Direct::SetOutput (StateVar* output, int i) {
    if (i == 0) {
        _output = output;
    }
}

ClassId TF_Direct::GetClassId () { return TF_DIRECT; }

boolean TF_Direct::IsA (ClassId id) {
    return TF_DIRECT == id || TF_2Port::IsA(id); 
}

void TF_Direct::Transfer () {
    *_output = *_input;
    _changed = true;
}

boolean TF_Direct::ChangedOutput (int i) { 
    boolean changed = false;

    if (i == 0 && _changed) {
        changed = _changed;
        _changed = false;
    }
    return changed;
}

TransferFunct* TF_Direct::Copy () {
    return new TF_Direct(_input->Copy(), _output->Copy());
}

void TF_Direct::Read (istream& in) {
    TF_2Port::Read(in);
    Catalog* catalog = unidraw->GetCatalog();

    _input = catalog->ReadStateVar(in);
    _output = catalog->ReadStateVar(in);

    int changed;
    in >> changed;
    _changed = changed;
}

void TF_Direct::Write (ostream& out) {
    TF_2Port::Write(out);
    Catalog* catalog = unidraw->GetCatalog();

    catalog->WriteStateVar(_input, out);
    catalog->WriteStateVar(_output, out);
    out << _changed << " ";
}
