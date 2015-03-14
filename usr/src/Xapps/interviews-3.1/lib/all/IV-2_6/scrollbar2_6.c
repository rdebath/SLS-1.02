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
 * ScrollBar -- general scrolling interface
 */

#include <IV-2_6/InterViews/adjuster.h>
#include <IV-2_6/InterViews/border.h>
#include <IV-2_6/InterViews/scrollbar.h>
#include <IV-2_6/InterViews/scroller.h>
#include <IV-2_6/InterViews/box.h>

#include <IV-2_6/_enter.h>

/* half-second autorepeat delay on movers */
static const int DELAY = 5;

ScrollBar::ScrollBar(Interactor* i, int n) {
    interactor = i;
    size = n;
}

ScrollBar::ScrollBar(const char* name, Interactor* i, int n) {
    SetInstance(name);
    interactor = i;
    size = n;
}

ScrollBar::~ScrollBar() { }

void ScrollBar::Use(Interactor* i) {
    Insert(i);
    MonoScene::Reconfig();
}

HScrollBar::HScrollBar(Interactor* i, int n) : ScrollBar(i, n) {
    Init();
}

HScrollBar::HScrollBar(
    const char* name, Interactor* i, int n
) : ScrollBar(name, i, n) {
    Init();
}

HScrollBar::~HScrollBar() { }

void HScrollBar::Init() {
    SetClassName("HScrollBar");
}

void HScrollBar::Reconfig() {
    Interactor* i = new HBox(
	new LeftMover(interactor, DELAY),
	new VBorder,
	new HScroller(interactor, size),
	new VBorder,
	new RightMover(interactor, DELAY)
    );
    Use(i);
}

VScrollBar::VScrollBar(Interactor* i, int n) : ScrollBar(i, n) {
    Init();
}

VScrollBar::VScrollBar(
    const char* name, Interactor* i, int n
) : ScrollBar(name, i, n) {
    Init();
}

VScrollBar::~VScrollBar() { }

void VScrollBar::Init () {
    SetClassName("VScrollBar");
}

void VScrollBar::Reconfig() {
    Interactor* i = new VBox(
	new UpMover(interactor, DELAY),
	new HBorder,
	new VScroller(interactor, size),
	new HBorder,
	new DownMover(interactor, DELAY)
    );
    Use(i);
}
