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

#include <Dispatch/rpcstream.h>

// Specialize this class to RPC requests by initializing an rpcbuf.
// Set incomplete_request to ensure the first attempt to extract an RPC
// request will call underflow.

rpcstream::rpcstream() :
    _buf(this),
    _incomplete_request(true)
{
    init(&_buf);
}

rpcstream::~rpcstream() {}

// Provide operations on the rpcbuf.  Most change the stream's state
// so radically that it's either cleared or failed.

void rpcstream::listen(int port) {
    verify(rdbuf()->listen(port) != nil);
}

void rpcstream::connect(const char* host, int port) {
    verify(rdbuf()->connect(host, port) != nil);
}

int rpcstream::accept() {	// no effect on stream's state
    int fd;
    if (!rdbuf()->accept(fd)) {
	return -1;
    }
    return fd;
}

void rpcstream::attach(int fd) {
    verify(rdbuf()->attach(fd) != nil);
}

void rpcstream::close() {
    verify(rdbuf()->close() != nil);
}

void rpcstream::nonblocking(boolean nonblocking) { // can only fail, not clear
    if (!rdbuf()->nonblocking(nonblocking)) {
	setstate(ios::failbit);
    }
}

void rpcstream::verbose(boolean verbose) { // no effect on stream's state
    rdbuf()->verbose(verbose);
}

// For some of the functions above, success means starting over with a
// clean slate while failure means setting failbit as usual.

void rpcstream::verify(int ok) {
    if (ok) {
	clear();
    } else {
	setstate(ios::failbit);
    }
}
