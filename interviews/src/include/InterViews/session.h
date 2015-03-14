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
 * Session -- coordinate control flow and display management
 */

#ifndef iv_session_h
#define iv_session_h

#include <InterViews/coord.h>

#include <InterViews/_enter.h>

class Display;
class Event;
class Handler;
class SessionRep;
class String;
class Style;
class Window;

class PropertyData {
public:
    const char* path;		/* class/instance(s) property name */
    const char* value;		/* property value */
};

enum OptionStyle {
    OptionPropertyNext,		/* Property and value are in argv[i+1]  */
    OptionValueNext,		/* argv[i+1] */
    OptionValueImplicit,	/* OptionDesc.value */
    OptionValueIsArg,		/* argv[i] */
    OptionValueAfter		/* &argv[i][strlen(OptionDesc.name)] */
};

class OptionDesc {
public:
    const char* name;
    const char* path;
    OptionStyle style;
    const char* value;
};

class Session {
public:
    Session(
	const char*, int& argc, char** argv,
	const OptionDesc* = nil, const PropertyData* = nil
    );
    virtual ~Session();

    const char* name() const;
    const char* classname() const;
    int argc() const;
    char** argv() const;
    Style* style() const;

    void default_display(Display*);
    Display* default_display() const;

    virtual Display* connect(const String&);
    virtual Display* connect(const char*);
    virtual void disconnect(Display*);

    virtual int run();
    virtual int run_window(Window*);
    virtual void quit();
    virtual boolean done() const;

    virtual boolean pending() const;
    virtual void read(Event&);
    virtual boolean read(long sec, long usec, Event&);
    virtual void unread(Event&);
    virtual void poll(Event&);

    static Session* instance();
private:
    SessionRep* rep_;
};

#include <InterViews/_leave.h>

#endif
