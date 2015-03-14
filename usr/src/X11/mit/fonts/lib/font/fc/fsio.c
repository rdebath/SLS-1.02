/* $Header: /home/x_cvs/mit/fonts/lib/font/fc/fsio.c,v 1.3 1992/07/22 08:46:02 dawes Exp $ */
/* $XConsortium: fsio.c,v 1.23 92/05/14 16:52:27 gildea Exp $ */
/*
 * Copyright 1990 Network Computing Devices
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Network Computing Devices not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Network Computing
 * Devices makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * NETWORK COMPUTING DEVICES DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
 * IN NO EVENT SHALL NETWORK COMPUTING DEVICES BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE
 * OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  	Dave Lemke, Network Computing Devices, Inc
 */
/*
 * font server i/o routines
 */

#include	<X11/Xos.h>

#ifdef NCD
#include	<fcntl.h>
#endif

#include	"FS.h"
#include	"FSproto.h"
#include	<stdio.h>
#include	<signal.h>
#if defined(USG) || defined(SYSV)
#ifndef __TYPES__
#include	<sys/types.h>
#define __TYPES__
#endif
#endif

#if defined(TCPCONN) || defined(UNIXCONN) || defined(DNETCONN)
#define HASSOCKETS
#endif

#ifdef HASSOCKETS
#include	<sys/socket.h>
#endif
/* #include	<netinet/tcp.h> */
#include	<errno.h>
#include	"FSlibos.h"
#include	"fontmisc.h"
#include	"fsio.h"

extern int  errno;

/* check for both EAGAIN and EWOULDBLOCK, because some supposedly POSIX
 * systems are broken and return EWOULDBLOCK when they should return EAGAIN
 */

#if defined(EAGAIN) && defined(EWOULDBLOCK)
#define ETEST(err) (err == EAGAIN || err == EWOULDBLOCK)
#else

#ifdef EAGAIN
#define ETEST(err) (err == EAGAIN)
#else
#define ETEST(err) (err == EWOULDBLOCK)
#endif

#endif

static int  padlength[4] = {0, 3, 2, 1};
unsigned long fs_fd_mask[MSKCNT];

#ifdef HASSOCKETS
static int  _fs_wait_for_readable();

_fs_name_to_address(servername, inaddr)
    char       *servername;
    struct sockaddr_in *inaddr;
{
    int         servernum = 0;
    char        hostname[256];
    char       *sp;
    unsigned long hostinetaddr;
    struct hostent *hp;

    /* XXX - do any service name lookup to get a hostname */

    (void) strncpy(hostname, servername, sizeof(hostname));

    /* get port */
    if ((sp = index(hostname, ':')) == NULL)
	return -1;

    *(sp++) = '\0';

    /* missing server number */
    if (*sp == '\0')
	return -1;
    servernum = atoi(sp);

    /* find host address */
    sp = hostname;
    if (!strncmp(hostname, "tcp/", 4)) {
	sp += 4;
    }
/* XXX -- this is all TCP specific.  other transports need to hack */
    hostinetaddr = inet_addr(sp);
    if (hostinetaddr == -1) {
	if ((hp = gethostbyname(sp)) == NULL) {
	    /* no such host */
	    errno = EINVAL;
	    return -1;
	}
	inaddr->sin_family = hp->h_addrtype;
	bcopy((char *) hp->h_addr, (char *) &inaddr->sin_addr,
	      sizeof(inaddr->sin_addr));
    } else {
	inaddr->sin_addr.s_addr = hostinetaddr;
	inaddr->sin_family = AF_INET;
    }
    inaddr->sin_port = htons(servernum);

    return 1;
}

#ifdef SIGNALRETURNSINT
#define SIGNAL_T int
#else
#define SIGNAL_T void
#endif

/* ARGSUSED */
static      SIGNAL_T
_fs_alarm(foo)
    int         foo;
{
    return;
}

static int
_fs_connect(servername, timeout)
    char       *servername;
    int         timeout;
{
    int         fd;
    struct sockaddr *addr;
    struct sockaddr_in inaddr;
    unsigned    oldTime;

    SIGNAL_T(*oldAlarm) ();
    int         ret;

    if (_fs_name_to_address(servername, &inaddr) < 0)
	return -1;

    addr = (struct sockaddr *) & inaddr;
    if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0)
	return -1;

    oldTime = alarm((unsigned) 0);
    oldAlarm = signal(SIGALRM, _fs_alarm);
    alarm((unsigned) timeout);
    ret = connect(fd, addr, sizeof(struct sockaddr_in));
    alarm((unsigned) 0);
    signal(SIGALRM, oldAlarm);
    alarm(oldTime);
    if (ret == -1) {
	(void) close(fd);
	return -1;
    }
    /* ultrix reads hang on Unix sockets, hpux reads fail */

#if defined(O_NONBLOCK) && (!defined(ultrix) && !defined(hpux))
    (void) fcntl(fd, F_SETFL, O_NONBLOCK);
#else

#ifdef FIOSNBIO
    {
	int         arg = 1;

	ioctl(fd, FIOSNBIO, &arg);
    }
#else
    (void) fcntl(fd, F_SETFL, FNDELAY);
#endif

#endif

    return fd;
}

static int  generationCount;

/* ARGSUSED */
static Bool
_fs_setup_connection(conn, servername, timeout)
    FSFpePtr    conn;
    char       *servername;
    int         timeout;
{
    fsConnClientPrefix prefix;
    fsConnSetup rep;
    int         setuplength;
    fsConnSetupAccept conn_accept;
    int         endian;
    int         i;
    int         alt_len;
    char       *auth_data = NULL,
               *vendor_string = NULL,
               *alt_data = NULL,
               *alt_dst;
    FSFpeAltPtr alts;
    int         nalts;

    conn->fs_fd = _fs_connect(servername, 5);
    if (conn->fs_fd < 0)
	return FALSE;

    conn->generation = ++generationCount;

    /* send setup prefix */
    endian = 1;
    if (*(char *) &endian)
	prefix.byteOrder = 'l';
    else
	prefix.byteOrder = 'B';

    prefix.major_version = FS_PROTOCOL;
    prefix.minor_version = FS_PROTOCOL_MINOR;

/* XXX add some auth info here */
    prefix.num_auths = 0;
    prefix.auth_len = 0;

    if (_fs_write(conn, (char *) &prefix, sizeof(fsConnClientPrefix)) == -1)
	return FALSE;

    /* read setup info */
    if (_fs_read(conn, (char *) &rep, sizeof(fsConnSetup)) == -1)
	return FALSE;

    conn->fsMajorVersion = rep.major_version;
    if (rep.major_version > FS_PROTOCOL)
	return FALSE;

    alts = 0;
    /* parse alternate list */
    if (nalts = rep.num_alternates) {
	setuplength = rep.alternate_len << 2;
	alts = (FSFpeAltPtr) xalloc(nalts * sizeof(FSFpeAltRec) +
				    setuplength);
	if (!alts) {
	    close(conn->fs_fd);
	    errno = ENOMEM;
	    return FALSE;
	}
	alt_data = (char *) (alts + nalts);
	if (_fs_read(conn, (char *) alt_data, setuplength) == -1) {
	    xfree(alts);
	    return FALSE;
	}
	alt_dst = alt_data;
	for (i = 0; i < nalts; i++) {
	    alts[i].subset = alt_data[0];
	    alt_len = alt_data[1];
	    alts[i].name = alt_dst;
	    bcopy(alt_data + 2, alt_dst, alt_len);
	    alt_dst[alt_len] = '\0';
	    alt_dst += (alt_len + 1);
	    alt_data += (2 + alt_len + padlength[(2 + alt_len) & 3]);
	}
    }
    if (conn->alts)
	xfree(conn->alts);
    conn->alts = alts;
    conn->numAlts = nalts;

    setuplength = rep.auth_len << 2;
    if (setuplength &&
	    !(auth_data = (char *) xalloc((unsigned int) setuplength))) {
	close(conn->fs_fd);
	errno = ENOMEM;
	return FALSE;
    }
    if (_fs_read(conn, (char *) auth_data, setuplength) == -1) {
	xfree(auth_data);
	return FALSE;
    }
    if (rep.status != AuthSuccess) {
	xfree(auth_data);
	close(conn->fs_fd);
	errno = EPERM;
	return FALSE;
    }
    /* get rest */
    if (_fs_read(conn, (char *) &conn_accept, (long) sizeof(fsConnSetupAccept)) == -1) {
	xfree(auth_data);
	return FALSE;
    }
    if ((vendor_string = (char *)
	 xalloc((unsigned) conn_accept.vendor_len + 1)) == NULL) {
	xfree(auth_data);
	close(conn->fs_fd);
	errno = ENOMEM;
	return FALSE;
    }
    if (_fs_read_pad(conn, (char *) vendor_string, conn_accept.vendor_len) == -1) {
	xfree(vendor_string);
	xfree(auth_data);
	return FALSE;
    }
    xfree(auth_data);
    xfree(vendor_string);

    conn->servername = (char *) xalloc(strlen(servername) + 1);
    if (conn->servername == NULL)
	return FALSE;
    strcpy(conn->servername, servername);

    return TRUE;
}

static Bool
_fs_try_alternates(conn, timeout)
    FSFpePtr    conn;
    int         timeout;
{
    int         i;

    for (i = 0; i < conn->numAlts; i++)
	if (_fs_setup_connection(conn, conn->alts[i].name, timeout))
	    return TRUE;
    return FALSE;
}
#endif /* HASSOCKETS */

#define FS_OPEN_TIMEOUT	    30
#define FS_REOPEN_TIMEOUT   10

FSFpePtr
_fs_open_server(servername)
    char       *servername;
{
#ifndef HASSOCKETS
    return (FSFpePtr) NULL;
#else
    FSFpePtr    conn;

    conn = (FSFpePtr) xalloc(sizeof(FSFpeRec));
    if (!conn) {
	errno = ENOMEM;
	return (FSFpePtr) NULL;
    }
    bzero((char *) conn, sizeof(FSFpeRec));
    if (!_fs_setup_connection(conn, servername, FS_OPEN_TIMEOUT)) {
	if (!_fs_try_alternates(conn, FS_OPEN_TIMEOUT)) {
	    xfree(conn->alts);
	    xfree(conn);
	    return (FSFpePtr) NULL;
	}
    }
    return conn;
#endif /* HASSOCKETS */
}

Bool
_fs_reopen_server(conn)
    FSFpePtr    conn;
{
#ifdef HASSOCKETS
    if (_fs_setup_connection(conn, conn->servername, FS_REOPEN_TIMEOUT))
	return TRUE;
    if (_fs_try_alternates(conn, FS_REOPEN_TIMEOUT))
	return TRUE;
#endif
    return FALSE;
}

/*
 * expects everything to be here.  *not* to be called when reading huge
 * numbers of replies, but rather to get each chunk
 */
_fs_read(conn, data, size)
    FSFpePtr    conn;
    char       *data;
    unsigned long size;
{
#ifndef HASSOCKETS
    return -1;
#else
    long        bytes_read;

    if (size == 0) {

#ifdef DEBUG
	fprintf(stderr, "tried to read 0 bytes \n");
#endif

	return 0;
    }
    errno = 0;
    while ((bytes_read = read(conn->fs_fd, data, (int) size)) != size) {
	if (bytes_read > 0) {
	    size -= bytes_read;
	    data += bytes_read;
	} else if (ETEST(errno)) {
	    /* in a perfect world, this shouldn't happen */
	    /* ... but then, its less than perfect... */
	    if (_fs_wait_for_readable(conn) == -1) {	/* check for error */
		_fs_connection_died(conn);
		errno = EPIPE;
		return -1;
	    }
	    errno = 0;
	} else if (errno == EINTR) {
	    continue;
	} else {		/* something bad happened */
	    if (conn->fs_fd > 0)
		_fs_connection_died(conn);
	    errno = EPIPE;
	    return -1;
	}
    }
    return 0;
#endif /* HASSOCKETS */
}

_fs_write(conn, data, size)
    FSFpePtr    conn;
    char       *data;
    unsigned long size;
{
#ifndef HASSOCKETS
    return -1;
#else
    long        bytes_written;

    if (size == 0) {

#ifdef DEBUG
	fprintf(stderr, "tried to write 0 bytes \n");
#endif

	return 0;
    }
    errno = 0;
    while ((bytes_written = write(conn->fs_fd, data, (int) size)) != size) {
	if (bytes_written > 0) {
	    size -= bytes_written;
	    data += bytes_written;
	} else if (ETEST(errno)) {
	    /* XXX -- we assume this can't happen */

#ifdef DEBUG
	    fprintf(stderr, "fs_write blocking\n");
#endif
	} else if (errno == EINTR) {
	    continue;
	} else {		/* something bad happened */
	    _fs_connection_died(conn);
	    errno = EPIPE;
	    return -1;
	}
    }
    return 0;
#endif /* HASSOCKETS */
}

_fs_read_pad(conn, data, len)
    FSFpePtr    conn;
    char       *data;
    int         len;
{
#ifndef HASSOCKETS
    return -1;
#else
    char        pad[3];

    if (_fs_read(conn, data, len) == -1)
	return -1;

    /* read the junk */
    if (padlength[len & 3]) {
	return _fs_read(conn, pad, padlength[len & 3]);
    }
    return 0;
#endif /* HASSOCKETS */
}

_fs_write_pad(conn, data, len)
    FSFpePtr    conn;
    char       *data;
    int         len;
{
#ifndef HASSOCKETS
    return -1;
#else
    static char pad[3];

    if (_fs_write(conn, data, len) == -1)
	return -1;

    /* write the pad */
    if (padlength[len & 3]) {
	return _fs_write(conn, pad, padlength[len & 3]);
    }
    return 0;
#endif /* HASSOCKETS */
}

/*
 * returns the amount of data waiting to be read
 */
int
_fs_data_ready(conn)
    FSFpePtr    conn;
{
#ifndef HASSOCKETS
    return -1;
#else
    long        readable;

    if (BytesReadable(conn->fs_fd, &readable) < 0)
	return -1;
    return readable;
#endif /* HASSOCKETS */
}

#ifdef HASSOCKETS
static int
_fs_wait_for_readable(conn)
    FSFpePtr    conn;
{
    unsigned long r_mask[MSKCNT];
    unsigned long e_mask[MSKCNT];
    int         result;

#ifdef DEBUG
    fprintf(stderr, "read would block\n");
#endif

    CLEARBITS(r_mask);
    CLEARBITS(e_mask);
    do {
	BITSET(r_mask, conn->fs_fd);
	BITSET(e_mask, conn->fs_fd);
	result = select(conn->fs_fd + 1, r_mask, NULL, e_mask, NULL);
	if (result == -1) {
	    if (errno != EINTR)
		return -1;
	    else
		continue;
	}
	if (result && _fs_any_bit_set(e_mask))
	    return -1;
    } while (result <= 0);

    return 0;
}
#endif /* HASSOCKETS */

int
_fs_set_bit(mask, fd)
    unsigned long *mask;
    int         fd;
{
    return BITSET(mask, fd);
}

int
_fs_is_bit_set(mask, fd)
    unsigned long *mask;
    int         fd;
{
    return GETBIT(mask, fd);
}

void
_fs_bit_clear(mask, fd)
    unsigned long *mask;
    int         fd;
{
    BITCLEAR(mask, fd);
}

int
_fs_any_bit_set(mask)
    unsigned long *mask;
{

#ifdef ANYSET
    return ANYSET(mask);
#else
    int         i;

    for (i = 0; i < MSKCNT; i++)
	if (mask[i])
	    return (1);
    return (0);
#endif
}

int
_fs_or_bits(dst, m1, m2)
    unsigned long *dst,
               *m1,
               *m2;
{
    ORBITS(dst, m1, m2);
}

_fs_drain_bytes(conn, len)
    FSFpePtr    conn;
    int         len;
{
#ifndef HASSOCKETS
    return -1;
#else
    char        buf[128];

#ifdef DEBUG
    fprintf(stderr, "draining wire\n");
#endif

    while (len > 0) {
	if (_fs_read(conn, buf, (len < 128) ? len : 128) < 0)
	    return -1;
	len -= 128;
    }
    return 0;
#endif /* HASSOCKETS */
}

_fs_drain_bytes_pad(conn, len)
    FSFpePtr    conn;
    int         len;
{
#ifndef HASSOCKETS
    return -1;
#else
    _fs_drain_bytes(conn, len);

    /* read the junk */
    if (padlength[len & 3]) {
	_fs_drain_bytes(conn, padlength[len & 3]);
    }
#endif /* HASSOCKETS */
}

_fs_eat_rest_of_error(conn, err)
    FSFpePtr    conn;
    fsError    *err;
{
#ifndef HASSOCKETS
    return -1;
#else
    int         len = (err->length - (sizeof(fsReplyHeader) >> 2)) << 2;

#ifdef DEBUG
    fprintf(stderr, "clearing error\n");
#endif

    _fs_drain_bytes(conn, len);
#endif /* HASSOCKETS */
}
