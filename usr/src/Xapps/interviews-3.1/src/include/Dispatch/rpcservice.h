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

#ifndef dp_rpcservice_h
#define dp_rpcservice_h

#include <Dispatch/iohandler.h>

class rpcbuf;

// Support communication between a RPC service and its clients.
// Derived classes create readers to read RPC requests from
// connections with clients.

class RpcService : public IOHandler {
public:
    virtual ~RpcService();

    void run();
    void quitRunning();
protected:
    RpcService(int port);
    RpcService(const char* path, int port = 0);

    void startListening();
    void stopListening();
    virtual int inputReady(int);

    virtual void createReader(int fd) = 0;
protected:
    const char* _path;		// my registration's path
    int _port;			// my port's address
    rpcbuf* _service;		// my network socket
    boolean _running;		// am I running my dispatch loop?
private:
    // deny access since unimplemented and member-wise won't work
    RpcService(const RpcService&);
    RpcService& operator=(const RpcService&);
};

#endif
