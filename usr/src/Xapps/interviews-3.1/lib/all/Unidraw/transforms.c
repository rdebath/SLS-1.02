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
 * Implementation of transformation commands;
 */

#include <Unidraw/catalog.h>
#include <Unidraw/classes.h>
#include <Unidraw/Commands/transforms.h>

#include <stream.h>

/*****************************************************************************/

ClassId MoveCmd::GetClassId () { return MOVE_CMD; }
boolean MoveCmd::IsA (ClassId id) { return MOVE_CMD == id || Command::IsA(id);}

MoveCmd::MoveCmd (ControlInfo* c, float x, float y) : Command(c) {
    _dx = x;
    _dy = y;
}

MoveCmd::MoveCmd (Editor* ed, float x, float y) : Command(ed) {
    _dx = x;
    _dy = y;
}

Command* MoveCmd::Copy () {
    Command* copy = new MoveCmd(CopyControlInfo(), _dx, _dy);
    InitCopy(copy);
    return copy;
}

void MoveCmd::GetMovement (float& x, float& y) {
    x = _dx;
    y = _dy;
}

void MoveCmd::Read (istream& in) {
    Command::Read(in);
    in >> _dx >> _dy;
}

void MoveCmd::Write (ostream& out) {
    Command::Write(out);
    out << _dx << " " << _dy << " ";
}

/*****************************************************************************/

ClassId ScaleCmd::GetClassId () { return SCALE_CMD; }

boolean ScaleCmd::IsA (ClassId id) {
    return SCALE_CMD == id || Command::IsA(id);
}

ScaleCmd::ScaleCmd (
    ControlInfo* c, float x, float y, Alignment a
) : Command(c) {
    _sx = x;
    _sy = y;
    _align = a;
}

ScaleCmd::ScaleCmd (Editor* ed, float x, float y, Alignment a) : Command(ed) {
    _sx = x;
    _sy = y;
    _align = a;
}

Command* ScaleCmd::Copy () {
    Command* copy = new ScaleCmd(CopyControlInfo(), _sx, _sy, _align);
    InitCopy(copy);
    return copy;
}

void ScaleCmd::GetScaling (float& x, float& y) {
    x = _sx;
    y = _sy;
}

void ScaleCmd::Read (istream& in) {
    Command::Read(in);
    long a;
    in >> _sx >> _sy >> a;
    _align = (Alignment) a;
}

void ScaleCmd::Write (ostream& out) {
    Command::Write(out);
    out << _sx << " " << _sy << " " << _align << " ";
}

/*****************************************************************************/

ClassId RotateCmd::GetClassId () { return ROTATE_CMD; }

boolean RotateCmd::IsA (ClassId id) {
    return ROTATE_CMD == id || Command::IsA(id);
}

RotateCmd::RotateCmd (ControlInfo* c, float a) : Command(c) {
    _angle = a;
}

RotateCmd::RotateCmd (Editor* ed, float a) : Command(ed) { _angle = a; }

Command* RotateCmd::Copy () {
    Command* copy = new RotateCmd(CopyControlInfo(), _angle);
    InitCopy(copy);
    return copy;
}

void RotateCmd::Read (istream& in) {
    Command::Read(in);
    in >> _angle;
}

void RotateCmd::Write (ostream& out) {
    Command::Write(out);
    out << _angle << " ";
}
