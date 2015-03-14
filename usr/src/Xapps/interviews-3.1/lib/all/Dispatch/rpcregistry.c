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
#include <OS/host.h>
#include <fstream.h>
#include <errno.h>
#ifndef __DECCXX
#include <osfcn.h>
#endif
#include <stddef.h>
#include <string.h>
#include <sys/param.h>
#if defined(sun) && OSMajorVersion >= 5
#include <sys/utsname.h>
#define MAXHOSTNAMELEN SYS_NMLN
#endif

// Print a short error message describing the last error encountered
// during a call to a system function.

static ostream& perror(ostream& s) {
#if defined(sun) && OSMajorVersion >= 5
    s << ": " << strerror(errno);
#else
    if (errno > 0 && errno < sys_nerr) {
	s << ": " << sys_errlist[errno];
    }
#endif
    s << '\n';
    return s;
}

// Record the RPC service's hostname and port number in the given
// file.  Remove the file before opening it in case it already exists
// and the user has permission to delete it but not permission to
// write data into it.

boolean RpcRegistry::record(const char* path, int port) {
    if (!path) {
	return false;
    }

    unlink(path);

    ofstream registry(path);
    if (!registry) {
	cerr << "RpcRegistry::record: open(" << path << ")" << perror;
	return false;
    }

    registry << Host::name() << ends << port;
    if (!registry) {
	cerr << "RpcRegistry::record: write" << perror;
	return false;
    }

    return true;
}

// Remove the file which stores the RPC service's hostname and port
// number so that no more clients will be able to contact the RPC
// service.

boolean RpcRegistry::erase(const char* path) {
    if (!path) {
	return false;
    }

    if (unlink(path) < 0) {
	cerr << "RpcRegistry::erase: unlink(" << path << ")" << perror;
	return false;
    }

    return true;
}

// Open the file which stores the RPC service's hostname and port
// number.  If the file does not exist, return failure silently.  If
// the file does exist, read the RPC service's hostname and port
// number from it.  If the RPC service and client are on the same
// host, speed up I/O between them by returning "localhost" instead of
// the host's true name.

boolean RpcRegistry::find(const char* path, char*& hostname, int& port) {
    if (!path) {
	return false;
    }

    ifstream registry(path);
    if (!registry) {
	return false;
    }

    if (!hostname) {
	hostname = new char[MAXHOSTNAMELEN];
    }

    registry.getline(hostname, MAXHOSTNAMELEN, '\0');
    registry >> port;
    if (!registry) {
	cerr << "RpcRegistry::find: error reading " << path << '\n';
	return false;
    }

    if (strcmp(hostname, Host::name()) == 0) {
	strcpy(hostname, "localhost");
    }

    return true;
}
