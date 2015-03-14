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

#ifndef dp_rpchdr_h
#define dp_rpchdr_h

#include <Dispatch/enter-scope.h>

class rpcstream;

// Insert or extract this header to send or receive a RPC request.

class RpcHdr {
public:
    RpcHdr(void* writer, int request);
    RpcHdr();

    unsigned long reader();
    int request();
    int ndata();
protected:
    friend rpcstream& operator>>(rpcstream&, RpcHdr&);
    friend rpcstream& operator<<(rpcstream&, const RpcHdr&);
protected:
    union {
	void* _writer;		// stores writer sending this RPC request
	unsigned long _reader;	// maps to reader for this RPC request
    };
    int _request;		// maps to member function to be called
    int _ndata;			// gives size (in bytes) of data to extract
};

// Get information about the RPC request.

inline unsigned long RpcHdr::reader() {
    return _reader;
}

inline int RpcHdr::request() {
    return _request;
}

inline int RpcHdr::ndata() {
    return _ndata;
}

#endif
