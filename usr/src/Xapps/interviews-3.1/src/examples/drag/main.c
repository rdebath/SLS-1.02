/*
 * Copyright (c) 1992 Redwood Design Automation
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the name of
 * Redwood Design Automation may not be used in any advertising or publicity
 * relating to the software without the specific, prior written permission of
 * Redwood Design Automation.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * IN NO EVENT SHALL REDWOOD DESIGN AUTOMATION BE LIABLE FOR ANY SPECIAL,
 * INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT
 * ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF LIABILITY,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */

#include <iostream.h>
#include <InterViews/background.h>
#include <InterViews/box.h>
#include <InterViews/cursor.h>
#include <InterViews/drag.h>
#include <InterViews/layout.h>
#include <InterViews/session.h>
#include <InterViews/style.h>
#include <InterViews/window.h>
#include <IV-look/kit.h>

///////////////////////////////////////////////
// Glyph that can be dragged to a drag zone. //
///////////////////////////////////////////////

class DragDemo : public Drag {
public:
    DragDemo(WidgetKit* kit, boolean useCursor, boolean useGlyph);

    virtual Cursor* dragCursor();
    virtual Glyph* dragGlyph();
    virtual void dragData(char*& value, int& length);

    virtual Glyph* glyph();
protected:
    WidgetKit* kit_;
    boolean useCursor_;
    boolean useGlyph_;
};

DragDemo::DragDemo(
    WidgetKit* kit, boolean useCursor, boolean useGlyph
) : Drag(nil), kit_(kit), useCursor_(useCursor), useGlyph_(useGlyph) {
    body(glyph());
}

Glyph* DragDemo::glyph() {
    return kit_->outset_frame(kit_->label("drag"));
}

Cursor* DragDemo::dragCursor() {
    return useCursor_ ? crosshairs : nil;
}

Glyph* DragDemo::dragGlyph() {
    return useGlyph_ ? glyph() : nil;
}

void DragDemo::dragData(char*& value, int& length) {
    value = "howdy";
    length = 5;
}

///////////////////////////////////////////////
// Glyph that can have things dropped in it. //
///////////////////////////////////////////////

class DragZoneDemo : public DragZone {
public:
    DragZoneDemo(WidgetKit* kit);

    virtual void enter(Event&, const char* type, int length);
    virtual void leave(Event&);

    virtual void drop(Event&, const char* data, int);

    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void reallocate();
private:
    WidgetKit* kit_;
    Canvas* canvas_;
    Allocation allocation_;
    Extension extension_;
};

DragZoneDemo::DragZoneDemo(WidgetKit* kit) : DragZone(nil), kit_(kit) {
    body(kit->inset_frame(kit->label("zone")));
}

void DragZoneDemo::enter(Event&, const char*, int) {
    body(kit_->outset_frame(kit_->label("zone")));
    reallocate();
}

void DragZoneDemo::leave(Event&) {
    body(kit_->inset_frame(kit_->label("zone")));
    reallocate();
}

void DragZoneDemo::drop(Event& event, const char* data, int) {
    cout << data << endl;
    leave(event);
}

void DragZoneDemo::allocate(Canvas* c, const Allocation& a, Extension& e) {
    DragZone::allocate(c, a, e);
    canvas_ = c;
    allocation_ = a;
    extension_ = e;
}

void DragZoneDemo::reallocate() {
    extension_.clear();
    allocate(canvas_, allocation_, extension_);
    canvas_->damage(extension_);
}

/////////////////////////////////////////////////////////////////////////////
// Main program that creates a box with one object that can be dragged and //
// one place to drag an object to.  You could run this program in parallel //
// and drag from one window to the other.                                  //
//                                                                         //
// There are two options to control the visual part of dragging:           //
//    -useCursor true   (set the cursor to a cross to indicate dragging)   //
//    -useGlyph  true   (drag a glyph in a window to indicate dragging)    //
/////////////////////////////////////////////////////////////////////////////

static OptionDesc options[] = {
    { "-useCursor", "*useCursor",  OptionValueNext },
    { "-useGlyph", "*useGlyph",  OptionValueNext },
    { nil }
};
static PropertyData properties[] = {
    { "*useCursor", "true" },
    { "*useGlyph", "false" },
    { nil }
};

int main(int argc, char** argv) {
    Session* session = new Session("Drag", argc, argv, options, properties);
    Style* style = session->style();
    WidgetKit* kit = WidgetKit::instance();
    boolean useCursor = style->value_is_on("useCursor");
    boolean useGlyph = style->value_is_on("useGlyph");
    ApplicationWindow* window = new ApplicationWindow(
	new DragZoneSink(
	    new Background(
		LayoutKit::instance()->hbox(
		    new DragDemo(kit, useCursor, useGlyph),
		    new DragZoneDemo(kit)
		),
		kit->background()
	    )
	)
    );
    window->map();
    session->run();

    return 0;
}
