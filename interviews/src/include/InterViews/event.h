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
 * Input events.
 */

#ifndef iv_event_h
#define iv_event_h

#include <InterViews/coord.h>

#include <InterViews/_enter.h>

class Display;
class EventRep;
class Interactor;
class Handler;
class Window;
class World;

typedef unsigned int EventType;
typedef unsigned int EventButton;

/* anachronism */
enum {
    MotionEvent,	/* mouse moved */
    DownEvent,		/* button pressed */
    UpEvent,		/* button released */
    KeyEvent,		/* key pressed, intepreted as ascii */
    EnterEvent,		/* mouse enters canvas */
    LeaveEvent,		/* mouse leaves canvas */
    FocusInEvent,	/* focus for keyboard events */
    FocusOutEvent 	/* lose keyboard focus */
};

/* mouse button anachronisms */
static const int LEFTMOUSE = 0;
static const int MIDDLEMOUSE = 1;
static const int RIGHTMOUSE = 2;

class Event {
public:
    enum { undefined, motion, down, up, key, other_event };
    enum { none, any, left, middle, right, other_button };

    Event();
    Event(const Event&);
    virtual ~Event();

    virtual Event& operator =(const Event&);

    virtual void display(Display*);
    virtual Display* display() const;

    virtual void window(Window*);
    virtual Window* window() const;

    virtual boolean pending() const;
    virtual void read();
    virtual boolean read(long sec, long usec);
    virtual void unread();
    virtual void poll();

    virtual Handler* handler() const;
    virtual void handle();
    virtual void grab(Handler*) const;
    virtual void ungrab(Handler*) const;
    virtual Handler* grabber() const;
    virtual boolean is_grabbing(Handler*) const;

    virtual EventType type() const;
    virtual unsigned long time() const;
    virtual Coord pointer_x() const;
    virtual Coord pointer_y() const;
    virtual Coord pointer_root_x() const;
    virtual Coord pointer_root_y() const;
    virtual EventButton pointer_button() const;
    virtual unsigned int keymask() const;
    virtual boolean control_is_down() const;
    virtual boolean meta_is_down() const;
    virtual boolean shift_is_down() const;
    virtual boolean capslock_is_down() const;
    virtual boolean left_is_down() const;
    virtual boolean middle_is_down() const;
    virtual boolean right_is_down() const;
    virtual unsigned char keycode() const;
    virtual unsigned long keysym() const;
    virtual unsigned int mapkey(char*, unsigned int len) const;

    EventRep* rep() const;
private:
    EventRep* rep_;
    char free_store_[200];

    void copy_rep(const Event&);

    /*
     * Old members for backward compatibility
     */
public:
    Interactor* target;
    unsigned long timestamp;
    EventType eventType;
    IntCoord x, y;		/* mouse position relative to target */
    boolean control : 1;	/* true if down */
    boolean meta : 1;
    boolean shift : 1;
    boolean shiftlock : 1;
    boolean leftmouse : 1;
    boolean middlemouse : 1;
    boolean rightmouse : 1;
    unsigned char button;	/* button pressed or released, if any */
    unsigned short len;		/* length of ASCII string */
    char* keystring;		/* ASCII interpretation of event, if any */

    void GetAbsolute(IntCoord&, IntCoord&);
    void GetAbsolute(World*&, IntCoord&, IntCoord&);
    EventRep* Rep() const;
private:
    World* w;
    _lib_iv2_6(Coord) wx, wy;
    char keydata[sizeof(int)];

    friend class Interactor;

    void GetInfo();
    void GetMotionInfo();
    void GetButtonInfo(EventType);
    void GetKeyInfo();
    void GetKeyState(unsigned);
    void GetCrossingInfo(EventType);
};

inline EventRep* Event::rep() const { return rep_; }
inline EventRep* Event::Rep() const { return rep(); }

#include <InterViews/_leave.h>

#endif
