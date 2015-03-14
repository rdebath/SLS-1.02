/*
 * Copyright (c) 1987, 1988, 1989, 1990, 1991 Stanford University
 * Copyright (c) 1991 Silicon Graphics, Inc.
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

/*
 * Border implementation.
 */

#include <IV-2_6/InterViews/border.h>
#include <IV-2_6/InterViews/painter.h>
#include <IV-2_6/InterViews/shape.h>
#include <IV-2_6/_enter.h>

Border::Border(int t) {
    thickness = t;
}

Border::Border(const char* name, int t) {
    SetInstance(name);
    thickness = t;
}

Border::~Border() { }

void Border::Redraw(IntCoord x1, IntCoord y1, IntCoord x2, IntCoord y2) {
    output->FillRect(canvas, x1, y1, x2, y2);
}

void Border::DefaultThickness(int t) {
    if (thickness == -1) {
	thickness = t;
    }
}

HBorder::HBorder(int t) : Border(t) {
    Init();
}

HBorder::HBorder(const char* name, int t) : Border(name, t) {
    Init();
}

HBorder::~HBorder() { }

void HBorder::Init() {
    SetClassName("HBorder");
}

void HBorder::Reconfig() {
    DefaultThickness(1);
    shape->height = thickness;
    shape->Rigid(hfil, hfil, 0, 0);
}

VBorder::VBorder(int t) : Border(t) {
    Init();
}

VBorder::VBorder(const char* name, int t) : Border(name, t) {
    Init();
}

VBorder::~VBorder() { }

void VBorder::Init() {
    SetClassName("VBorder");
}

void VBorder::Reconfig() {
    DefaultThickness(1);
    shape->width = thickness;
    shape->Rigid(0, 0, vfil, vfil);
}
