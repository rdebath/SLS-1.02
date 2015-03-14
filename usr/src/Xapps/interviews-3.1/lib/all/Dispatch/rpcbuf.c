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

#include <Dispatch/rpcbuf.h>
#include <OS/memory.h>
#include <OS/types.h>	/* must come before <netinet/in.h> on some systems */
#include <arpa/inet.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#ifndef __DECCXX
#include <osfcn.h>
#endif
#include "netinet_in.h"
#include <stdlib.h>
#include <stdio.h>
#include <sys/socket.h>

// I need a pointer to an iostreamb so I can insert and extract values
// in the length field of RPC requests.  If I don't have a stream, I
// won't allow you to call start_request().

rpcbuf::rpcbuf(iostreamb* mystream) : streambuf(),
    _mystream(mystream),
    _rptr(nil),
    _actualWidth(0),
    _host(nil),
    _port(0),
    _fd(-1),
    _opened(false),
    _close(false),
    _nonblocking(false),
    _verbose(true) {}

// Free the buffer used to store the put area.  The streambuf
// destructor will free the buffer used to store the get area.

rpcbuf::~rpcbuf() {
    close();

    delete pbase();
    setp(nil, nil);
}

// Return information about the connection.

const char* rpcbuf::host() {
    return _host;
}

int rpcbuf::port() {
    if (!_opened) {
	return 0;
    }

    if (_port) {
	return _port;
    }

    struct sockaddr_in name;
    int name_len = sizeof(name);
    if (getsockname(_fd, (struct sockaddr*)&name, &name_len) < 0) {
	sys_error("rpcbuf::port: getsockname");
	return 0;
    }

    _port = ntohs(name.sin_port);
    return _port;
}

int rpcbuf::fd() {
    return _fd;
}

boolean rpcbuf::opened() {
    return _opened;
}

boolean rpcbuf::nonblocking() {
    return _nonblocking;
}

// Create a socket, bind the socket to a port address, and prepare to
// accept incoming connections.

rpcbuf* rpcbuf::listen(int port) {
    struct sockaddr_in name;
    Memory::zero(&name, sizeof(name));
    name.sin_family = AF_INET;
    name.sin_port = htons(port);
    name.sin_addr.s_addr = htonl(INADDR_ANY);

    if (_opened) {
	error("rpcbuf::listen: already using a file number");
	return nil;
    }

    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
	sys_error("rpcbuf::listen: socket");
	return nil;
    }

    if (bind(fd, (struct sockaddr*)&name, sizeof(name)) < 0) {
	sys_error("rpcbuf::listen: bind");
	::close(fd);
	return nil;
    }

    if (::listen(fd, SOMAXCONN) < 0) {
	sys_error("rpcbuf::listen: listen");
	::close(fd);
	return nil;
    }

    _host = nil;
    _port = port;
    _fd = fd;
    _opened = true;
    _close = true;
    _nonblocking = false;
    return this;
}

// Create a socket and make a connection to another socket.  Do
// retries in case the peer has hit its backlog, but not too many
// retries since the error is indistinguishable from there not being a
// peer listening at all.

rpcbuf* rpcbuf::connect(const char* host, int port) {
    struct sockaddr_in name;
    Memory::zero(&name, sizeof(name));
    name.sin_family = AF_INET;
    name.sin_port = htons(port);

    if (_opened) {
	error("rpcbuf::connect: already using a file number");
	return nil;
    }

    const unsigned long INVALIDADDR = (unsigned long) -1;
    unsigned long hostinetaddr = INVALIDADDR;
    if (isascii(host[0]) && isdigit(host[0])) {
#ifdef AIXV3
	/* cast work-around for AIX */
	hostinetaddr = inet_addr((const int*)host);
#else
	hostinetaddr = inet_addr(host);
#endif
	name.sin_addr.s_addr = hostinetaddr;
    }
    if (hostinetaddr == INVALIDADDR) {
	/* cast to workaround bug in prototype on some systems */
	struct hostent* hp = gethostbyname((char*)host);
	if (hp == nil) {
	    error("rpcbuf::connect: gethostbyname: no such host");
	    return nil;
	}
	if (hp->h_addrtype != AF_INET) {
	    error("rpcbuf::connect: gethostbyname: not an Internet host");
	    return nil;
	}
	Memory::copy(hp->h_addr, &name.sin_addr, sizeof(name.sin_addr));
    }

    int fd = -1;
    int retries = 2;
    do {
	fd = socket(AF_INET, SOCK_STREAM, 0);
	if (fd < 0) {
	    sys_error("rpcbuf::connect: socket");
	    return nil;
	}
	if (::connect(fd, (struct sockaddr*)&name, sizeof(name)) < 0) {
	    if (errno == ECONNREFUSED && retries > 0) {
		// try again since peer's backlog may just be full
		::close(fd);
		sleep(1);
		continue;
	    } else {
		sys_error("rpcbuf::connect: connect");
		::close(fd);
		return nil;
	    }
	}
	break;			// the connection succeeded
    } while (retries-- > 0);

    _host = host;
    _port = port;
    _fd = fd;
    _opened = true;
    _close = true;
    _nonblocking = false;
    return this;
}

// Accept an incoming connection, allocate a new file descriptor for
// it, and return the new file descriptor.

rpcbuf* rpcbuf::accept(int& fd) {
    struct sockaddr_in name;
    int name_len = sizeof(name);

    if (!_opened) {
	error("rpcbuf::accept: not using a file number yet");
	return nil;
    }

    fd = ::accept(_fd, (struct sockaddr*)&name, &name_len);
    if (fd < 0) {
	sys_error("rpcbuf::accept: accept");
	return nil;
    }

    return this;
}

// Attach the streambuf to a file descriptor.  The streambuf cannot
// close the file descriptor because something else might be using it.

rpcbuf* rpcbuf::attach(int fd) {
    if (_opened) {
	error("rpcbuf::attach: already using a file number");
	return nil;
    }

    if (fd < 0) {
	error("rpcbuf::attach: cannot use a negative file number");
	return nil;
    }

    _host = nil;
    _port = 0;
    _fd = fd;
    _opened = true;
    _close = false;
    _nonblocking = false;
    return this;
}

// Empty the get/put areas, close the file descriptor if nothing else
// might use it, and detach the streambuf from the file descriptor.

rpcbuf* rpcbuf::close() {
    if (!_opened) {
	return nil;
    }

    sync();

    int ok = 0;
    if (_close) {
	ok = ::close(_fd);
	if (ok < 0) {
	    sys_error("rpcbuf::close: close");
	}
    }

    _host = nil;
    _port = 0;
    _fd = -1;
    _opened = false;
    _close = false;
    _nonblocking = false;
    return (ok < 0) ? nil : this;
}

// Set or clear non-blocking I/O on the file descriptor.

rpcbuf* rpcbuf::nonblocking(boolean nonblocking) {
    if (!_opened) {
	error("rpcbuf::nonblocking: not using a file number yet");
	return nil;
    }

    if (_nonblocking != nonblocking) {
	int flags = fcntl(_fd, F_GETFL, 0);
	if (flags < 0) {
	    sys_error("rpcbuf::nonblocking: F_GETFL fcntl");
	    return nil;
	}
	if (nonblocking) {
	    flags |= O_NDELAY;
	} else {
	    flags &= ~O_NDELAY;
	}
	if (fcntl(_fd, F_SETFL, flags) < 0) {
	    sys_error("rpcbuf::nonblocking: F_SETFL fcntl");
	    return nil;
	}
    }

    _nonblocking = nonblocking;
    return this;
}

// Set or clear printing of system error messages.

rpcbuf* rpcbuf::verbose(boolean verbose) {
    _verbose = verbose;
    return this;
}

// Finish the current request, if any, and then start a new request.
// The request begins with a length field, so insert a zero with
// enough padding characters (if using formatted I/O) to overwrite the
// zero later with 2**32 - 1, the largest possible length for a
// request (the length must fit in an int).

const int FIELDWIDTH = 11;	// large enough to hold "2147483647 "

int rpcbuf::start_request() {
    if (!_mystream || !_opened || allocate() == EOF) {
	return EOF;
    }

    finish_request();
    setr(pptr());

    const int length = 0;
    mystream().width(FIELDWIDTH);
    mystream() << length;
    _actualWidth = pptr() - rptr();

    return 0;
}

// Finish the current request by inserting its final length in the
// length field at the beginning of the request (overwriting the old
// value).  The length of the request includes its own length field.

void rpcbuf::finish_request() {
    int length = pptr() - rptr();
    if (rptr() && length > 0) {
	pbump(-length);
	mystream().width(FIELDWIDTH);
	mystream() << length;
	if (_actualWidth != pptr() - rptr()) {
	    error("rpcbuf::finish_request: length field's width changed");
	}
	pbump(length - (pptr() - rptr()));
    }
    setr(nil);
}

// Calculate the width of the length field (_actualWidth) and check
// that all of the length field is in the buffer.  If so, extract the
// length, move the get pointer back to the beginning of the request,
// and return 0 if all of the request is in the buffer.  Otherwise,
// return EOF.  By checking for EOF, the caller avoids extracting a
// request before it's completely buffered.  The caller must call
// select() to wait for new input and call rpcbuf::underflow() to
// enqueue it until the complete request is buffered.

int rpcbuf::read_request() {
    if (!_mystream) {
	return EOF;
    }

    if (!_actualWidth) {
	char* orig = pptr();
	const int length = 0;
	mystream().width(FIELDWIDTH);
	mystream() << length;
	_actualWidth = pptr() - orig;
	pbump(orig - pptr());
    }

    int navail = in_avail();
    if (navail < _actualWidth) {
	return EOF;
    }

    char* orig = gptr();
    int length = 0;
    mystream() >> length;
    gbump(orig - gptr());

    if (length <= 0) {
	error("rpcbuf::read_request: zero or negative length");
	return EOF;
    }

    if (length > ebuf() - eback() && !expand_g(length * 2)) {
	error("rpcbuf::read_request: out of memory");
	return EOF;
    }

    if (navail < length) {
	return EOF;
    } else {
	return 0;
    }
}

// Finish the current RPC request if there's nothing to append to it,
// thus allowing flush to send the current RPC request.  Send any
// outgoing data using a loop to safeguard against partial writes.
// Shift any still incomplete RPC request to the beginning of the put
// area to make room for more data.  Append the overflow char if any.

int rpcbuf::overflow(int c) {
    if (!_opened || allocate() == EOF) {
	return EOF;
    }

    if (c == EOF) {
	finish_request();
    }

    if (rptr() == pbase() && pptr() >= epptr() && !expand_p()) {
	error("rpcbuf::overflow: out of memory");
	return EOF;
    }

    int nwrite = (rptr() >= pbase()) ? rptr() - pbase() : out_waiting();
    int count = 0;
    while (count < nwrite) {
	int nsent = write(_fd, pbase() + count, nwrite - count);
	if (nsent < 0) {
	    sys_error("rpcbuf::overflow: write");
	    return EOF;
	}
	count += nsent;
    }
    if (rptr() > pbase()) {
	Memory::copy(rptr(), pbase(), pptr() - rptr());
	rbump(-nwrite);
    }
    pbump(-nwrite);

    if (c != EOF) {
	sputc(c);
    }
    return zapeof(c);
}

// Empty the put area before filling the get area in case the input
// depends on the output just flushed.  Move any unread data between
// gptr() and egptr() to the beginning of the get area.  The get area
// may contain unread data under nonblocking I/O because an incomplete
// RPC request is not extracted until the rest of its data arrives.
// Read as much data as available into the free space between egptr()
// and ebuf() (the get area occupies the entire buffer).  Move egptr()
// to the end of the new data.  Return the first unread character.

int rpcbuf::underflow() {
    if (!_opened || allocate() == EOF) {
	return EOF;
    }

    if (overflow() == EOF) {
	return EOF;
    }

    int nunread = in_avail();
    if (nunread) {
	Memory::copy(gptr(), eback(), nunread);
    }
    setg(eback(), eback(), eback() + nunread);

    int nread = read(_fd, egptr(), ebuf() - egptr());
    if (nread < 0) {
	sys_error("rpcbuf::underflow: read");
	return EOF;
    }
    if (nread == 0) {
	return EOF;
    }

    setg(eback(), gptr(), egptr() + nread);
    return zapeof(*gptr());
}

// Probably called from ios's destructor.  Can't do anything with
// still unread data except discard it, but can flush any outgoing RPC
// requests from the put area.

int rpcbuf::sync() {
    gbump(in_avail());
    return out_waiting() ? overflow() : 0;
}

// Can't seek on a socket, but can return the get pointer's current
// position so that the caller can find out how many bytes he read
// since the get pointer's last position (within the same request).

#ifdef cplusplus_2_1
streampos rpcbuf::seekoff(streamoff offset, ios::seek_dir dir, int mode) {
#else
streampos rpcbuf::seekoff(streamoff offset, seek_dir dir, int mode) {
#endif
    if (!_opened || !gptr()) {
	return EOF;
    }

    if (offset != 0 || dir != ios::cur || mode != ios::in) {
	return EOF;
    }

    return (streampos)gptr();
}

// Refuse any attempt to set the buffers for storing incoming and
// outgoing RPC requests because we need the ability to dynamically
// expand the buffers' sizes.

streambuf* rpcbuf::setbuf(char*, int) {
    return nil;
}

// Dynamically allocate two separate buffers for storing incoming and
// outgoing RPC requests.  Allocating separate buffers for the get and
// put areas makes it easier to expand either area later if necessary.

int rpcbuf::doallocate() {
    const int RPCBUFSIZE = 2032;

    char* get = new char[RPCBUFSIZE];
    if (!get) {
	error("rpcbuf::doallocate: out of memory");
	return EOF;
    }
    setb(get, get + RPCBUFSIZE, true);
    setg(get, get, get);

    char* put = new char[RPCBUFSIZE];
    if (!put) {
	error("rpcbuf::doallocate: out of memory");
	return EOF;
    }
    setp(put, put + RPCBUFSIZE);
    setr(nil);

    return 0;
}

// Expand the get area to make room for a large incoming request.

boolean rpcbuf::expand_g(int newsize) {
    char* get = new char[newsize];
    if (!get) {
	return false;
    }

    int navail = in_avail();
    Memory::copy(gptr(), get, navail);
    delete eback();
    setb(get, get + newsize, true);
    setg(get, get, get + navail);

    return true;
}

// Expand the put area to make room for additional data to be appended
// to an outgoing request.

boolean rpcbuf::expand_p() {
    int newsize = (epptr() - pbase()) * 2;
    char* put = new char[newsize];
    if (!put) {
	return false;
    }

    int nwaiting = out_waiting();
    Memory::copy(pbase(), put, nwaiting);
    delete pbase();
    setp(put, put + newsize);
    pbump(nwaiting);
    setr(put);

    return true;
}

// Print a user error message.

void rpcbuf::error(const char* msg) {
    if (_verbose) {
	cerr << msg << "\n";
	cerr.flush();
    }
}

// Print a system error message.

void rpcbuf::sys_error(const char* msg) {
    if (_verbose) {
	perror(msg);
    }
}
