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

#include <Dispatch/dispatcher.h>
#include <Dispatch/rpcbuf.h>
#include <Dispatch/rpcpeer.h>
#include <Dispatch/rpcregistry.h>
#include <stdlib.h>

// Store our parameters for use later since we can't call a derived
// class's virtual function from a base constructor.

RpcPeer::RpcPeer(
    const char* lPath, int lPort
) : IOHandler(),
    _lPath(lPath),
    _lPort(lPort),
    _service(nil),
    _running(false),
    _remote(false),
    _rHost(nil) {}

// Delete our RPC service if we created one.  Free any storage
// allocated by RpcRegistry::find for the remote service's host name.

RpcPeer::~RpcPeer() {
    if (_service) {
	stopListening();
    }
    delete _rHost;
}

// Find the host name and port number registered by the remote RPC
// service and try to open a connection to it.  Unregister the remote
// RPC service to prevent another attempt if the connection could not
// be opened.  If necessary, start up our own RPC service and wait for
// the remote RPC service to contact us.

void RpcPeer::init(const char* rPath) {
    if (_service || _remote) {
	return;
    }

    int rPort;
    _remote = RpcRegistry::find(rPath, _rHost, rPort);

    if (_remote && !createReaderAndWriter(_rHost, rPort)) {
	RpcRegistry::erase(rPath);
	_remote = false;
    }

    if (!_remote) {
	startListening();
    }
}

// Read RPC requests until something tells us to quit the run loop.
// If you're using the InterViews event-reading code, you don't have
// to use this code since that code will also read RPC requests.

void RpcPeer::run() {
    _running = true;
    while (_running) {
	Dispatcher::instance().dispatch();
    }
}

void RpcPeer::quitRunning() {
    _running = false;
}

// Open an RPC service at a port number (zero means any available port
// number).  Store our RPC service's assigned port number in a file to
// allow clients to find our service.  Start listening for connections
// from clients.

void RpcPeer::startListening() {
    _service = new rpcbuf;

    rpcbuf* ok = _service->listen(_lPort);
    _lPort = _service->port();
    if (!ok) {
	abort();
    }

    if (!RpcRegistry::record(_lPath, _lPort)) {
	abort();
    }

    Dispatcher::instance().link(_service->fd(), Dispatcher::ReadMask, this);
}

// Stop listening for connections from clients, unregister our RPC
// service, and delete it.

void RpcPeer::stopListening() {
    Dispatcher::instance().unlink(_service->fd());

    RpcRegistry::erase(_lPath);

    delete _service;
    _service = nil;
}

// Accept a connection from a client.  Ask a derived class to attach a
// reader and a writer which will read RPC requests and send RPC
// requests over the same connection.

int RpcPeer::inputReady(int) {
    int fd;
    rpcbuf* ok = _service->accept(fd);
    if (!ok) {
	abort();
    }

    createReaderAndWriter(fd);
    return 0;
}
