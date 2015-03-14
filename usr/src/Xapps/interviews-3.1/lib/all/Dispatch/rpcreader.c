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
#include <Dispatch/rpchdr.h>
#include <Dispatch/rpcreader.h>
#include <Dispatch/rpcstream.h>

// Prepare to read RPC requests from somebody else's connection or
// prepare to handle RPC requests read by somebody else.  Assume the
// I/O format has already been negotiated.  Start listening for RPC
// requests on the connection, taking into account the special case
// where RPC requests have already been buffered.  Zero the function
// array that a derived class will initialize with addresses of static
// member functions to unmarshall RPC requests.

RpcReader::RpcReader(rpcstream* _client, int nfcns) : IOHandler(),
    _nfcns(nfcns),
    _function(new PF[nfcns]),
    _client(_client),
    _delete(false),
    _fd(_client ? _client->rdbuf()->fd() : -1)
{
    if (_client) {
	client().nonblocking(true);
	Dispatcher::instance().link(_fd, Dispatcher::ReadMask, this);
	if (client().rdbuf()->read_request() != EOF) {
	    Dispatcher::instance().setReady(_fd, Dispatcher::ReadMask);
	    client().incomplete_request(false);
	}
    }

    for (int i = 0; i < nfcns; i++) {
	_function[i] = nil;
    }
}

// Connect to a client through an already open file number.  Negotiate
// the I/O format.  Start listening for RPC requests on the
// connection, taking into account the special case where RPC requests
// have already been buffered (when the I/O format was negotiated).
// Zero the function array that a derived class will initialize with
// addresses of static member functions to unmarshall RPC requests.

RpcReader::RpcReader(int fd, int nfcns, boolean binary) : IOHandler(),
    _nfcns(nfcns),
    _function(new PF[nfcns]),
    _client(new rpcstream),
    _delete(true),
    _fd(fd)
{
    client().attach(fd);
    client().negotiate(binary);
    client().nonblocking(true);
    Dispatcher::instance().link(fd, Dispatcher::ReadMask, this);
    if (client().rdbuf()->read_request() != EOF) {
	Dispatcher::instance().setReady(fd, Dispatcher::ReadMask);
	client().incomplete_request(false);
    }

    for (int i = 0; i < nfcns; i++) {
	_function[i] = nil;
    }
}

// Stop listening for RPC requests on the connection if we have one.
// Delete the connection if we created it ourselves.  Note that
// deleting a connection attached to a file number will not close that
// file number.

RpcReader::~RpcReader() {
    if (_fd >= 0) {
	Dispatcher::instance().unlink(_fd);
    }
    if (_delete) {
	delete _client;
    }
    delete[] _function;
}

// Read only one RPC request per call to allow the program to
// interleave RPC requests from multiple clients.  Look up the proper
// reader to execute the request or skip over the request's data if it
// could not be executed.  Ask a derived class to take the appropriate
// action (perhaps closing the file number or deleting ``this'') if no
// more data is available or the data wasn't what we expected.

int RpcReader::inputReady(int fd) {
    RpcHdr hdr;

    client() >> hdr;

    if (client().good() && !client().incomplete_request()) {
	RpcReader* reader = map(hdr.reader());

	if (!execute(reader, hdr)) {
	    client().ignore(hdr.ndata());
	}
    }

    if (client().eof() || client().fail()) {
	connectionClosed(fd);
	return -1;		// don't ever call me again (i.e., detach me)
    } else if (client().incomplete_request()) {
	return 0;		// call me only when more input arrives
    } else {
	return 1;		// call me again as soon as possible
    }
}

// Map the number to the reader that should unmarshall the RPC
// request.  Return this reader itself by default; derived classes
// could return a different reader.

RpcReader* RpcReader::map(unsigned long) {
    return this;
}

// Look up the static member function that will unmarshall the RPC
// request's arguments and execute the request.  Call that function
// with the reader, RPC request header, and connection as arguments.
// Return true if the function was called, false otherwise.

boolean RpcReader::execute(RpcReader* reader, RpcHdr& hdr) {
    if (!reader) {
	return false;
    }

    if (hdr.request() < 0 || hdr.request() >= reader->_nfcns) {
	return false;
    }

    PF func = reader->_function[hdr.request()];
    if (!func) {
	return false;
    }

    (*func)(reader, hdr, client());
    return true;
}
