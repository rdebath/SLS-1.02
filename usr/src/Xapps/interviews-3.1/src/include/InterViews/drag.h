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

#ifndef iv_drag_h
#define iv_drag_h

#include <InterViews/monoglyph.h>

#include <InterViews/_enter.h>

class Allocation;
class Canvas;
class Cursor;
class DragRep;
class DragZoneRep;
class DragZoneSinkHandler;
class Event;
class Hit;

/*
 * To create a dragable glyph:
 * 1) create a derived class of Drag
 * 2) implement the member functions dragGlyph and/or dragCursor.  dragGlyph
 *    should return a glyph that will be dragged across the screen in a window.
 *    dragCursor should return a cursor that will be dragged across the screen.
 * 3) implement the member function dragData.  dragData should produce a
 *    string to be sent to a drag zone
 * 4) the dragType member function can be implemented to provide a string
 *    that is sent to a drag zone when the drag zone is entered
 * 5) caught, commit, and abort can be implemented to change the default
 *    behavior.  the default behavior is that a middle button press starts a
 *    drag, a chorded button press aborts, and a button release commits a drag.
 * 6) dragOffset can be implemented to calculate a transformed offset from
 *    the left-top corner of the dragGlyph.
 */

class Drag : public MonoGlyph {
public:
    Drag(Glyph* glyph);
    virtual ~Drag();

    virtual void dragable(boolean);
    virtual boolean dragable() const;

    virtual Glyph* dragGlyph() = 0;
    virtual Cursor* dragCursor() = 0;
    virtual void dragData(char*& value, int& length) = 0;
    virtual void dragType(char*& value, int& length);
    virtual void dragOffset(Event& event, int& dx, int& dy);

    virtual boolean caught(const Event&) const;
    virtual boolean commit(const Event&) const;
    virtual boolean abort(const Event&) const;

    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);
protected:
    DragRep* rep() const;
private:
    DragRep* rep_;
};

/*
 * To create a drag zone glyph:
 *   1) create a derived class of DragZone
 *   2) implement the member function drop.  drop will be called when a drag
 *      glyph is dropped in a DragZone glyph.
 *   3) enter, motion, and leave can be implemented to indicate when a drop
 *      would be sent to a drag zone.
 */

class DragZone : public MonoGlyph {
public:
    DragZone(Glyph*);
    virtual ~DragZone();

    virtual void sensitive(boolean);
    virtual boolean sensitive() const;

    virtual void enter(Event&, const char* type, int length);
    virtual void motion(Event&);
    virtual void leave(Event&);

    virtual void drop(Event&, const char* data, int length) = 0;

    virtual void allocate(Canvas*, const Allocation&, Extension&);
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);
protected:
    DragZoneRep* rep() const;
private:
    DragZoneRep* rep_;
};

/*
 * A drag zone sink publishes that a window can accept drag messages.  It
 * also consumes any drag messages that do not fall in a drag zone.  This
 * class could be eliminated and the code moved to xwindow.c.
 */

class DragZoneSink : public DragZone {
public:
    DragZoneSink(Glyph*);
    virtual ~DragZoneSink();

    virtual void drop(Event&, const char* data, int length);

    virtual void draw(Canvas*, const Allocation&) const;
    virtual void pick(Canvas*, const Allocation&, int depth, Hit&);

    virtual boolean event(Event& event);
private:
    boolean dragPublished_;
    DragZoneSinkHandler* target_;
};

#include <InterViews/_leave.h>

#endif
