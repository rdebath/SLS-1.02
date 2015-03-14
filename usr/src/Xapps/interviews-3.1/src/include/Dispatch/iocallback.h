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

#ifndef dp_iocallback_h
#define dp_iocallback_h

/*
 * IOCallback allows a member function to be used as an IO handler
 * instead of requiring derivation of a subclass.
 */

#include <Dispatch/iohandler.h>

#if defined(__STDC__) || defined(__ANSI_CPP__)
#define __IOCallback(T) T##_IOCallback
#define IOCallback(T) __IOCallback(T)
#define __IOReady(T) T##_IOReady
#define IOReady(T) __IOReady(T)
#define __IOTimer(T) T##_IOTimer
#define IOTimer(T) __IOTimer(T)
#else
#define __IOCallback(T) T/**/_IOCallback
#define IOCallback(T) __IOCallback(T)
#define __IOReady(T) T/**/_IOReady
#define IOReady(T) __IOReady(T)
#define __IOTimer(T) T/**/_IOTimer
#define IOTimer(T) __IOTimer(T)
#endif

#define declareIOCallback(T) \
typedef int (T::*IOReady(T))(int fd); \
typedef void (T::*IOTimer(T))(long sec, long usec); \
class IOCallback(T) : public IOHandler { \
public: \
    IOCallback(T)( \
	T*, IOReady(T) in, IOReady(T) out = nil, IOReady(T) ex = nil \
    ); \
    IOCallback(T)( \
	T*, IOTimer(T), \
	IOReady(T) in = nil, IOReady(T) out = nil, IOReady(T) ex = nil \
    ); \
\
    virtual int inputReady(int fd); \
    virtual int outputReady(int fd); \
    virtual int exceptionRaised(int fd); \
    virtual void timerExpired(long sec, long usec); \
private: \
    T* _obj; \
    IOReady(T) _input; \
    IOReady(T) _output; \
    IOReady(T) _except; \
    IOTimer(T) _timer; \
};

#define implementIOCallback(T) \
IOCallback(T)::IOCallback(T)( \
    T* obj, IOReady(T) in, IOReady(T) out, IOReady(T) ex \
) { \
    _obj = obj; _timer = nil; \
    _input = in; _output = out; _except = ex; \
} \
\
IOCallback(T)::IOCallback(T)( \
    T* obj, IOTimer(T) t, IOReady(T) in, IOReady(T) out, IOReady(T) ex \
) { \
    _obj = obj; _timer = t; \
    _input = in; _output = out; _except = ex; \
} \
\
int IOCallback(T)::inputReady(int fd) { return (_obj->*_input)(fd); } \
int IOCallback(T)::outputReady(int fd) { return (_obj->*_output)(fd); } \
int IOCallback(T)::exceptionRaised(int fd) { return (_obj->*_except)(fd); } \
void IOCallback(T)::timerExpired(long s, long u) { (_obj->*_timer)(s, u); }

#endif
