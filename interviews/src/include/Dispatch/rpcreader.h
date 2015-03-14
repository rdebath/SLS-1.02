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

#ifndef dp_rpcreader_h
#define dp_rpcreader_h

#include <Dispatch/iohandler.h>

class RpcHdr;
class rpcstream;

/*
 * Read RPC requests from a client.  Derived classes initialize the
 * function array with addresses of static member functions to
 * unmarshall RPC requests and implement the virtual function called
 * when the client closes the connection.
 */

class RpcReader : public IOHandler {
public:
    virtual ~RpcReader();
protected:
    RpcReader(rpcstream* client, int nfcns);
    RpcReader(int fd, int nfcns, boolean binary);

    rpcstream& client();
    virtual int inputReady(int);
    virtual RpcReader* map(unsigned long);
    boolean execute(RpcReader*, RpcHdr&);
    virtual void connectionClosed(int fd) = 0;
protected:
    typedef void (*PF)(RpcReader*, RpcHdr&, rpcstream&);
    int _nfcns;			/* size of function array */
    PF* _function;		/* function array indexed by request number */
    rpcstream* _client;		/* source of RPC requests coming from client */
    boolean _delete;		/* should the destructor delete _client? */
    int _fd;			/* file number of connection with client */
private:
    /* deny access since unimplemented and member-wise won't work */
    RpcReader(const RpcReader&);
    RpcReader& operator=(const RpcReader&);
};

inline rpcstream& RpcReader::client() {
    return *_client;
}

#endif
