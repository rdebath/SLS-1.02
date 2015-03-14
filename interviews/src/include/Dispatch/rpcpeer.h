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

#ifndef dp_rpcpeer_h
#define dp_rpcpeer_h

#include <Dispatch/iohandler.h>

class rpcbuf;

// Support bi-directional RPC between two peers.  Derived classes
// create both a reader and a writer so each peer can send RPC
// requests to its opposite over the same connection.

class RpcPeer : public IOHandler {
public:
    virtual ~RpcPeer();

    void run();
    void quitRunning();
protected:
    RpcPeer(
	const char* lPath, int lPort = 0
    );

    void init(const char* rPath);
    void startListening();
    void stopListening();
    virtual int inputReady(int);

    virtual boolean createReaderAndWriter(const char* rHost, int rPort) = 0;
    virtual void createReaderAndWriter(int fd) = 0;
protected:
    const char* _lPath;		// my registration's path
    int _lPort;			// my port's address
    rpcbuf* _service;		// my network socket
    boolean _running;		// am I running my dispatch loop?

    boolean _remote;		// am I connected to a remote RPC service?
    char* _rHost;		// dynamically allocated storage to be freed
private:
    // deny access since unimplemented and member-wise won't work
    RpcPeer(const RpcPeer&);
    RpcPeer& operator=(const RpcPeer&);
};

#endif
