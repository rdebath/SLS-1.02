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

#ifndef dp_rpcwriter_h
#define dp_rpcwriter_h

#include <Dispatch/enter-scope.h>

class rpcstream;

// Write RPC requests to a server.  Derived classes should add member
// functions corresponding to the RPC service's protocol.

class RpcWriter {
public:
    ~RpcWriter();

    rpcstream& server();
protected:
    RpcWriter(const char* path, boolean fatal, boolean binary);
    RpcWriter(const char* host, int port, boolean fatal, boolean binary);
    RpcWriter(int fd, boolean fatal, boolean binary);
    RpcWriter(rpcstream* server);

    void open(const char* path, boolean fatal, boolean binary);

    rpcstream* _server;		// sink of RPC requests going to server
    boolean _delete;		// should the destructor delete _server?
    char* _host;		// dynamically allocated storage to be deleted
private:
    // deny access since unimplemented and member-wise won't work
    RpcWriter(const RpcWriter&);
    RpcWriter& operator=(const RpcWriter&);
};

// Give this function public access so that programs can test this
// connection's state or attach a RpcReader to it to read as well
// as send RPC requests over the same connection.

inline rpcstream& RpcWriter::server() {
    return *_server;
}

#endif
