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

#include <Dispatch/rpcregistry.h>
#include <Dispatch/rpcstream.h>
#include <Dispatch/rpcwriter.h>
#include <stdlib.h>		/* for abort() */

// Open a connection to an RPC service at its registered host name and
// port number, or give the host name and port number needed to open a
// connection to the RPC service, or give the number of an already
// open file descriptor, or give the address of an already open
// rpcstream (connection).  Negotiate the I/O format if opening a
// connection.  Terminate the program if errors should be fatal.

RpcWriter::RpcWriter(const char* path, boolean fatal, boolean binary) :
    _server(new rpcstream),
    _delete(true),
    _host(nil)
{
    open(path, fatal, binary);
}

RpcWriter::RpcWriter(
    const char* host, int port, boolean fatal, boolean binary
) :
    _server(new rpcstream),
    _delete(true),
    _host(nil)
{
    server().verbose(fatal);
    server().connect(host, port);
    server().negotiate(binary);

    if (!server() && fatal) {
	abort();
    }
}

RpcWriter::RpcWriter(int fd, boolean fatal, boolean binary) :
    _server(new rpcstream),
    _delete(true),
    _host(nil)
{
    server().verbose(fatal);
    server().attach(fd);
    server().negotiate(binary);

    if (!server() && fatal) {
	abort();
    }
}

RpcWriter::RpcWriter(rpcstream* server) :
    _server(server),
    _delete(false),
    _host(nil) {}

// Close the connection to the server, although the file number won't
// be closed if we attached the connection to it.  Free any storage
// allocated by RpcRegistry::find for the host name.

RpcWriter::~RpcWriter() {
    if (_delete) {
	delete _server;
    }
    delete _host;
}

// Use a member function to open a connection to an RPC service at its
// registered host name and port number so that a derived class's
// constructor can retry the attempt if necessary.

void RpcWriter::open(const char* path, boolean fatal, boolean binary) {
    int port;

    if (RpcRegistry::find(path, _host, port)) {
	server().verbose(fatal);
	server().connect(_host, port);
	server().negotiate(binary);
    } else {
	server().clear(ios::failbit | ios::badbit);
    }

    if (!server() && fatal) {
	cerr << "RpcWriter::open: service " << path << " not found" << "\n";
	cerr.flush();
	abort();
    }
}
