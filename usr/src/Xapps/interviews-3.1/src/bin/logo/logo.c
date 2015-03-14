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
 * Logo object implementation.
 */

#include "logo.h"
#include <InterViews/bitmap.h>
#include <InterViews/color.h>
#include <InterViews/painter.h>
#include <InterViews/pattern.h>
#include <InterViews/sensor.h>
#include <InterViews/shape.h>
#include <InterViews/transformer.h>
#include <InterViews/world.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

Logo::Logo () { 
    SetClassName("Logo");
    input = new Sensor();
    input->Catch(KeyEvent);

    bitmap = nil;
    rainbow = 0;
}

void Logo::Handle (Event& e) {
    if (e.eventType == KeyEvent && e.keystring[0] == 'q') {
	Session::instance()->quit();
    }    
}

void Logo::Reconfig () {
    const char* name = GetAttribute("bitmap");
    rainbow = atoi(GetAttribute("rainbow"));

    if (bitmap == nil && name != nil) {
	bitmap = Bitmap::open(name);
	if (bitmap == nil) {
	    fprintf(stderr, "logo: cannot open bitmap file `%s'\n", name);
	    exit(1);
	}
    }
    if (bitmap != nil) {
	shape->width = bitmap->Width();
	shape->height = bitmap->Height();
    } else {
	shape->width = 65;
	shape->height = 65;
    }
}

void Logo::DrawBitmap () {
    Transformer* oldt = output->GetTransformer();

    Transformer t;
    t.Scale(float(xmax+1)/bitmap->Width(), float(ymax+1)/bitmap->Height());

    output->SetTransformer(&t);
    output->Stencil(
        canvas, -bitmap->Left(), -bitmap->Bottom(), bitmap, bitmap
    );

    output->SetTransformer(oldt);
    output->SetOrigin(0, 0);
}

static Coord poly1_x[] = {   0, 245, 200, 350, 450,  50,  50, 100,   0};
static Coord poly1_y[] = {  71, 314, 350, 350, 450, 450, 310, 310, 210};
static int poly1_count = sizeof(poly1_x)/sizeof(Coord);

static Coord poly2_x[] = {   0, 550, 650,   0};
static Coord poly2_y[] = { 550, 550, 650, 650};
static int poly2_count = sizeof(poly2_x)/sizeof(Coord);

void do_draw (
    Painter* painter, Canvas* canvas, float rotate, float scalex, float scaley
) {
    Transformer* oldt = painter->GetTransformer();
    Transformer t;
    t.Rotate(rotate);
    t.Scale(scalex, scaley);
    painter->SetTransformer(&t);
    painter->FillPolygon(canvas, poly1_x, poly1_y, poly1_count);
    painter->FillPolygon(canvas, poly2_x, poly2_y, poly2_count);
    painter->SetTransformer(oldt);
}

void Logo::DrawPolygon () {
    float scalex = float(xmax+2)/1300;
    float scaley = float(ymax+2)/1300;

    output->SetOrigin(xmax/2, ymax/2);

    do_draw(output, canvas, 0, scalex, scaley);
    do_draw(output, canvas, 90, scalex, -scaley);
    do_draw(output, canvas, 90, scalex, scaley);
    do_draw(output, canvas, 0, scalex, -scaley);
    do_draw(output, canvas, 0, -scalex, -scaley);
    do_draw(output, canvas, 90, -scalex, scaley);
    do_draw(output, canvas, 90, -scalex, -scaley);
    do_draw(output, canvas, 0, -scalex, scaley);

    output->SetOrigin(0, 0);
}

static float red_profile[] =    { 1.0, 1.0, 1.0, 0.0, 0.0, 0.5, 1.0 };
static float green_profile[] =  { 0.0, 0.5, 1.0, 0.8, 0.0, 0.0, 0.0 };
static float blue_profile[] =   { 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0 };
static int profile_count = sizeof(red_profile)/sizeof(float);

static ColorIntensity intensity (float hue, float* profile, int count) {
    int index = min(int(floor(hue * (count-1))), count-1);
    float i = (
        profile[index]
        + (hue * (count-1) - index) * (profile[index+1] - profile[index])
    );
    return i;
}

static Color** colors;

void Logo::Redraw (Coord, Coord, Coord, Coord) {
    if (rainbow > 0) {
        if (colors == nil) {
            colors = new Color* [rainbow];
            for (int c = 0; c < rainbow; ++c) {
                float hue = float(c) / float(rainbow-1);
                colors[c] = new Color(
                    intensity(hue, red_profile, profile_count),
                    intensity(hue, green_profile, profile_count),
                    intensity(hue, blue_profile, profile_count)
                );
                colors[c]->Reference();
            }
        }
        float h = float(ymax+1)/rainbow;
        const Color* oldfg = output->GetFgColor();
        for (int c = 0; c < rainbow; ++c) {
            output->SetColors(colors[c], nil);
            output->FillRect(canvas, 0, Coord(c * h), xmax, Coord((c+1) * h));
        }
        output->SetColors(oldfg, nil);
    } else {
        output->ClearRect(canvas, 0, 0, xmax, ymax);
    }
    if (bitmap == nil) {
        DrawPolygon();
    } else {
        DrawBitmap();
    }
}
