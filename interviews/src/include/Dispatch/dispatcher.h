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
 * Wait on multiple file descriptors until a condition occurs.
 */

#ifndef dp_dispatcher_h
#define dp_dispatcher_h

#include <Dispatch/enter-scope.h>

class FdMask;
class IOHandler;
class TimerQueue;
struct timeval;

class Dispatcher {
public:
    enum DispatcherMask {
	ReadMask,
	WriteMask,
	ExceptMask
    };

    Dispatcher();
    virtual ~Dispatcher();

    virtual void link(int fd, DispatcherMask, IOHandler*);
    virtual IOHandler* handler(int fd, DispatcherMask) const;
    virtual void unlink(int fd);

    virtual void startTimer(long sec, long usec, IOHandler*);
    virtual void stopTimer(IOHandler*);

    virtual boolean setReady(int fd, DispatcherMask);
    virtual void dispatch();
    virtual boolean dispatch(long& sec, long& usec);

    static Dispatcher& instance();
    static void instance(Dispatcher*);
protected:
    virtual void attach(int fd, DispatcherMask, IOHandler*);
    virtual void detach(int fd);
    virtual boolean dispatch(timeval*);
    virtual boolean anyReady() const;
    virtual int fillInReady(FdMask&, FdMask&, FdMask&);
    virtual int waitFor(FdMask&, FdMask&, FdMask&, timeval*);
    virtual void notify(int, FdMask&, FdMask&, FdMask&);
    virtual timeval* calculateTimeout(timeval*) const;
    virtual void handleError();
    virtual void checkConnections();
protected:
    int	_nfds;
    FdMask* _rmask;
    FdMask* _wmask;
    FdMask* _emask;
    FdMask* _rmaskready;
    FdMask* _wmaskready;
    FdMask* _emaskready;
    IOHandler** _rtable;
    IOHandler** _wtable;
    IOHandler** _etable;
    TimerQueue* _queue;
private:
    static Dispatcher* _instance;
private:
    /* deny access since member-wise won't work */
    Dispatcher(const Dispatcher&);
    Dispatcher& operator =(const Dispatcher&);
};

#endif
