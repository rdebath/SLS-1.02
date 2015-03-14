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
 * World -- object representing a display
 */

#ifndef iv_world_h
#define iv_world_h

#include <InterViews/session.h>

#include <InterViews/_enter.h>

class Color;
class Event;
class Font;
class Interactor;
class Sensor;

class World {
protected:
    World();
public:
    World(
	const char*, int& argc, char** argv,
	const OptionDesc* = nil, const PropertyData* = nil
    );
    virtual ~World();

    virtual Session* session() const;
    virtual Display* display() const;

    virtual const char* name() const;
    virtual const char* classname() const;
    virtual int argc() const;
    virtual char** argv() const;

    virtual Style* style() const;
    virtual const char* property_value(const char*) const;
    virtual boolean property_is_on(const char*) const;

    virtual const Font* font() const;
    virtual const Color* foreground() const;
    virtual const Color* background() const;
    virtual boolean shaped_windows() const;
    virtual boolean double_buffered() const;

    virtual void flush();
    virtual void sync();

    virtual Coord width() const;
    virtual Coord height() const;
    virtual unsigned int pwidth() const;
    virtual unsigned int pheight() const;

    virtual void run();
    virtual void quit();
    virtual boolean done() const;

    virtual boolean pending() const;
    virtual void read(Event&);
    virtual boolean read(long sec, long usec, Event&);
    virtual void unread(Event&);
    virtual void poll(Event&);

    virtual void RingBell(int);
    virtual void SetKeyClick(int);
    virtual void SetAutoRepeat(boolean);
    virtual void SetFeedback(int thresh, int scale);

    virtual void SetScreen(int);

    static World* current();
protected:
    void make_current();
private:
    friend class Interactor;

    static World* current_;

    Session* session_;
    Display* display_;

    /*
     * Old functions for backward compatibility
     *
     * We use "unsigned" for Alignment and "6" for BottomLeft to avoid
     * conflicts with new 3.0 names (like Center).
     */
public:
    void InsertApplication(Interactor*);
    void InsertApplication(
	Interactor*, _lib_iv2_6(Coord) x, _lib_iv2_6(Coord) y, unsigned = 6
    );

    void InsertToplevel(Interactor*, Interactor*);
    void InsertToplevel(
	Interactor*, Interactor*, _lib_iv2_6(Coord) x, _lib_iv2_6(Coord) y,
	unsigned = 6
    );

    void InsertPopup(Interactor*);
    void InsertPopup(
	Interactor*, _lib_iv2_6(Coord) x, _lib_iv2_6(Coord) y, unsigned = 6
    );

    void InsertTransient(Interactor*, Interactor*);
    void InsertTransient(
	Interactor*, Interactor*, _lib_iv2_6(Coord) x, _lib_iv2_6(Coord) y,
	unsigned = 6
    );

    void InsertIcon(Interactor*);
    void InsertIcon(
	Interactor*, _lib_iv2_6(Coord) x, _lib_iv2_6(Coord) y, unsigned = 6
    );

    void Insert(Interactor*);
    void Insert(
	Interactor*, _lib_iv2_6(Coord) x, _lib_iv2_6(Coord) y, unsigned = 6
    );

    void Change(Interactor*);
    void Remove(Interactor*);

    void Move(Interactor*, _lib_iv2_6(Coord) x, _lib_iv2_6(Coord) y);
    void Raise(Interactor*);
    void Lower(Interactor*);

    unsigned int Width() const;
    unsigned int Height() const;

    void Run();
    void Flush();
    void Sync();

    /*
     * ParseGeometry return values contains one or more of these bits set.
     */

#   define GeomNoValue 0x00
#   define GeomXValue 0x01
#   define GeomYValue 0x02
#   define GeomWidthValue 0x04
#   define GeomHeightValue 0x08
#   define GeomAllValues 0x0F
#   define GeomXNegative 0x10
#   define GeomYNegative 0x20

    unsigned int ParseGeometry(
	const char*, int&, int&, unsigned int&, unsigned int&
    ) const;

    const char* GetAttribute(const char*) const;

    int Fileno() const;
};

/*
 * Old functions for backward compatibilty
 */

inline void World::Insert(Interactor* i) { InsertToplevel(i, i); }
inline void World::Insert(
    Interactor* i, _lib_iv2_6(Coord) x, _lib_iv2_6(Coord) y, unsigned a
) {
    InsertToplevel(i, i, x, y, a);
}

inline unsigned int World::Width() const { return pwidth(); }
inline unsigned int World::Height() const { return pheight(); }

inline void World::Run() { run(); }
inline void World::Flush() { flush(); }
inline void World::Sync() { sync(); }
inline const char* World::GetAttribute(const char* s) const {
    return property_value(s);
}

#include <InterViews/_leave.h>

#endif
