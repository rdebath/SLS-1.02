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

#ifndef dp_rpcbuf_h
#define dp_rpcbuf_h

#include <Dispatch/iostreamb.h>

// Specialize streambuf to sending and receiving RPC requests to and
// from remote machines.

class rpcbuf : public streambuf {
public:
    rpcbuf(iostreamb* = nil);
    virtual ~rpcbuf();

    const char* host();
    int port();
    int fd();
    boolean opened();
    boolean nonblocking();

    enum { anyport = 0 };
    rpcbuf* listen(int port);
    rpcbuf* connect(const char* host, int port);
    rpcbuf* accept(int& fd);
    rpcbuf* attach(int fd);
    rpcbuf* nonblocking(boolean);
    rpcbuf* verbose(boolean);
    rpcbuf* close();
    int start_request();
    int read_request();

    virtual int overflow(int c = EOF);
    virtual int underflow();
    virtual int sync();
#ifdef cplusplus_2_1
    virtual streampos seekoff(streamoff, ios::seek_dir, int);
#else
    virtual streampos seekoff(streamoff, seek_dir, int);
#endif
    virtual streambuf* setbuf(char*, int);
protected:
    virtual int doallocate();
    void finish_request();
    boolean expand_g(int);
    boolean expand_p();
    void error(const char*);
    void sys_error(const char*);
    iostreamb& mystream();
    char* rptr();
    void setr(char*);
    void rbump(int);
protected:
    iostreamb* _mystream;	// reference to the stream that uses me
    char* _rptr;		// beginning of outgoing RPC request
    int _actualWidth;		// width of outgoing RPC request's length field
    const char* _host;		// name of my peer's host or nil if no peer
    int _port;			// my peer's port or my port if I'm a listener
    int _fd;			// my IPC connection's file descriptor
    boolean _opened;		// do I have an open file descriptor?
    boolean _close;		// should I close my file descriptor on exit?
    boolean _nonblocking;	// can I read or write without blocking?
    boolean _verbose;		// should I print system error messages?
};

// Get the stream which will format the length field of RPC requests.

inline iostreamb& rpcbuf::mystream() {
    return *_mystream;
}

// Get and set the pointer to the outgoing RPC request's beginning.

inline char* rpcbuf::rptr() {
    return _rptr;
}

inline void rpcbuf::setr(char* rptr) {
    _rptr = rptr;
}

inline void rpcbuf::rbump(int n) {
    _rptr += n;
}

#endif
