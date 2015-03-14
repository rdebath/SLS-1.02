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
 * VoidData - stores a void*; its destructor does nothing.
 * MoveData - stores two floating point values for distance moved.
 * ColorData - stores foreground and background colors.
 * GSData - stores a complete set of graphics state.
 * MobilityData - stores mobility information.
 */

#ifndef unidraw_commands_datas_h
#define unidraw_commands_datas_h

#include <Unidraw/globals.h>
#include <Unidraw/Commands/data.h>

class Graphic;
class PSColor;
class FullGraphic;

class VoidData : public Data {
public:
    VoidData(void*);
public:
    void* _void;
};

class MoveData : public Data {
public:
    MoveData(float, float);
public:
    float _dx, _dy;
};

class ColorData : public Data {
public:
    ColorData(PSColor*, PSColor*);
public:
    PSColor* _fg, *_bg;
};

class GSData : public Data {
public:
    GSData(Graphic*);
    virtual ~GSData();
public:
    FullGraphic* _gs;
};

class MobilityData : public Data {
public:
    MobilityData(Mobility, Graphic*);
    virtual ~MobilityData();
public:
    Mobility _mobility;
    FullGraphic* _gs;
};

#endif
