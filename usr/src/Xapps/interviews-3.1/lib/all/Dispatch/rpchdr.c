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

#include <Dispatch/rpchdr.h>
#include <Dispatch/rpcstream.h>

// Initialize the header for an outgoing RPC request.

RpcHdr::RpcHdr(void* writer, int request) :
    _writer(writer),
    _request(request),
    _ndata(0) {}

// Initialize the header for an incoming RPC request.

RpcHdr::RpcHdr() :
    _reader(0),
    _request(0),
    _ndata(0) {}

// If incomplete_request was set, explicitly call underflow in an
// attempt to complete an incoming RPC request by reading additional
// data and reset the flag.  Set the stream in eof state if no
// additional data was available or in fail state if it was already in
// eof state.  If the stream is still good, call read_request to check
// if the incoming RPC request is complete yet.  If it is, extract its
// header, else set incomplete_request to note so.  Decrement the
// length (which counted the entire request) by the space that the
// header occupied to count only the data following the header.

rpcstream& operator>>(rpcstream& client, RpcHdr& hdr) {
    if (client.good() && client.incomplete_request()) {
	if (client.rdbuf()->underflow() != EOF) {
	    client.incomplete_request(false);
	} else {
	    client.clear(client.rdstate() | ios::eofbit);
	}
    } else if (client.eof()) {
	client.clear(client.rdstate() | ios::failbit);
    }

    if (client.good()) {
	if (client.rdbuf()->read_request() != EOF) {
	    streampos beginning = client.tellg();
	    client >> hdr._ndata >> hdr._reader >> hdr._request;
	    hdr._ndata -= (int)(client.tellg() - beginning);
	} else {
	    client.incomplete_request(true);
	}
    }
    return client;
}

// Store the beginning of a new RPC request.  The rpcbuf automatically
// initializes the length field of the previous RPC request and skips
// the put pointer past the length field of the new RPC request so
// there's no need to examine hdr._ndata or insert its value.

rpcstream& operator<<(rpcstream& server, const RpcHdr& hdr) {
    if (server && server.rdbuf()->start_request() != EOF) {
	server << hdr._reader << hdr._request;
    } else {
	server.clear(server.rdstate() | ios::failbit);
    }
    return server;
}
