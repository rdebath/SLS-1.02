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

#ifndef dp_rpcstream_h
#define dp_rpcstream_h

#include <Dispatch/rpcbuf.h>

// Modify iostreamb to store a rpcbuf and provide operations on the
// rpcbuf, therefore specializing iostreamb to RPC requests.

class rpcstream : public virtual iostreamb {
public:
    rpcstream();
    ~rpcstream();

    void listen(int port);
    void connect(const char* host, int port);
    int accept();
    void attach(int fd);
    void close();
    void nonblocking(boolean);
    void verbose(boolean);

    rpcbuf* rdbuf();
    boolean incomplete_request();
    void incomplete_request(boolean);
protected:
    void verify(int);
protected:
    rpcbuf _buf;		 // streambuf specialized to RPC requests
    boolean _incomplete_request; // is the incoming request still incomplete?
};

// Return or set protected member variables.

inline rpcbuf* rpcstream::rdbuf() {
    return &_buf;
}

inline boolean rpcstream::incomplete_request() {
    return _incomplete_request;
}

inline void rpcstream::incomplete_request(boolean incomplete_request) {
    _incomplete_request = incomplete_request;
}

#endif
