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
#include <Dispatch/rpcregistry.h>
#include <Dispatch/rpcservice.h>
#include <stdlib.h>

// Start up an RPC service.  If a filename is given, store our
// service's assigned port number in it so clients can find us.

RpcService::RpcService(int port) : IOHandler(),
    _path(nil),
    _port(port),
    _service(new rpcbuf),
    _running(false)
{
    startListening();
}

RpcService::RpcService(const char* path, int port) : IOHandler(),
    _path(path),
    _port(port),
    _service(new rpcbuf),
    _running(false)
{
    startListening();
}

// Close our RPC service if it was open.

RpcService::~RpcService() {
    if (_service->opened()) {
	stopListening();
    }
    delete _service;
}

// Read RPC requests until something tells us to quit the run loop.
// If you're using the InterViews event-reading code, you don't have
// to use this code since that code will also read RPC requests.

void RpcService::run() {
    _running = true;
    while (_running) {
	Dispatcher::instance().dispatch();
    }
}

void RpcService::quitRunning() {
    _running = false;
}

// Open an RPC service at a port number (zero means any available port
// number).  If a filename was given, store our service's assigned
// port number in it to allow clients to find us.  Start listening for
// connections from clients.

void RpcService::startListening() {
    rpcbuf* ok = _service->listen(_port);
    _port = _service->port();
    if (!ok) {
	abort();
    }

    if (_path && !RpcRegistry::record(_path, _port)) {
	abort();
    }

    Dispatcher::instance().link(_service->fd(), Dispatcher::ReadMask, this);
}

// Stop listening for connections from clients, unregister our RPC
// service if it was registered, and close it.

void RpcService::stopListening() {
    Dispatcher::instance().unlink(_service->fd());

    if (_path) {
	RpcRegistry::erase(_path);
    }

    _service->close();
}

// Accept a connection from a client.  Ask a derived class to attach a
// reader which will read RPC requests from the connection.

int RpcService::inputReady(int) {
    int fd;
    rpcbuf* ok = _service->accept(fd);
    if (!ok) {
	abort();
    }

    createReader(fd);
    return 0;
}
