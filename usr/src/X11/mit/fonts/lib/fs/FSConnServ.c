/* $Header: /home/x_cvs/mit/fonts/lib/fs/FSConnServ.c,v 1.2 1992/06/18 11:22:51 dawes Exp $ */
/* $XConsortium: FSConnServ.c,v 1.14 91/09/09 18:55:13 rws Exp $ */

/* @(#)FSConnServ.c	4.1	91/05/02
 * Copyright 1990 Network Computing Devices;
 * Portions Copyright 1987 by Digital Equipment Corporation and the
 * Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this protoype software
 * and its documentation to Members and Affiliates of the MIT X Consortium
 * any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the names of Network Computing Devices, Digital or
 * MIT not be used in advertising or publicity pertaining to distribution of
 * the software without specific, written prior permission.
 *
 * NETWORK COMPUTING DEVICES, DIGITAL AND MIT DISCLAIM ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS, IN NO EVENT SHALL NETWORK COMPUTING DEVICES, DIGITAL OR MIT BE
 * LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#if defined(TCPCONN) || defined(UNIXCONN) || defined(DNETCONN)
#define HASSOCKETS
#endif

#include	<stdio.h>
#include	<X11/Xos.h>
#include	"FSlibint.h"
#ifdef HASSOCKETS
#include	<sys/socket.h>
#endif
#include	<sys/uio.h>
#ifdef NCD
#include	<fcntl.h>
#endif

/* font server does not actually support unix domain yet */
#ifdef UNIXCONN
#undef UNIXCONN
#endif

#ifdef DNETCONN
#include <netdnet/dn.h>
#include <netdnet/dnetdb.h>
#endif /* DNETCONN */

#ifndef hpux

#ifndef apollo			/* nest ifndefs because makedepend is broken */
#ifdef TCPCONN
#include <netinet/tcp.h>
#endif
#endif

#endif

#define FamilyLocal (256)
#define FamilyWild  (65535)


/*
 * Attempts to connect to server, given server name. Returns file descriptor
 * (network socket) or -1 if connection fails. The expanded server name
 * of the form hostname:number.screen ("::" if DECnet) is returned in a result
 * parameter. The screen number to use is also returned.
 */
int
_FSConnectServer(server_name, expanded_name)
    char       *server_name;
    char       *expanded_name;	/* return */
{
#ifndef HASSOCKETS
    return (-1);
#else
    char        serverbuf[256];	/* Server string buffer */
    register char *server_ptr;	/* Server string buffer pointer */
    register char *numbuf_ptr;	/* Server number buffer pointer */
    int         server_num;	/* Server number */
    struct sockaddr_in inaddr;	/* INET socket address. */
    unsigned long hostinetaddr;	/* result of inet_addr of arpa addr */

#ifdef UNIXCONN
    struct sockaddr_un unaddr;	/* UNIX socket address. */

#endif
    struct sockaddr *addr;	/* address to connect to */
    struct hostent *host_ptr;
    int         addrlen;	/* length of address */
#ifdef	X_NOT_STDC_ENV
    extern char *getenv();
#endif
    int         fd;		/* Network socket */

#ifdef DNETCONN
    int         dnet = 0;
    char        objname[20];
    extern int  dnet_conn();

#endif
    int         tmp_svr_numlen = 0;
    char       *tmp_svr_num = NULL;
    int         tmp_server_addrlen = 0;
    char       *tmp_server_addr = NULL;

    /*
     * Find the ':' seperator and extract the hostname and the server number.
     * NOTE - if DECnet is to be used, the server name is formatted as
     * "host::number"
     */
    (void) strncpy(serverbuf, server_name, sizeof(serverbuf));
    if ((server_ptr = SearchString(serverbuf, ':')) == NULL)
	return (-1);

#ifdef DNETCONN
    if (*(server_ptr + 1) == ':') {
	dnet++;
	*(server_ptr++) = '\0';
    }
#endif

    *(server_ptr++) = '\0';

    /*
     * serverbuf now contains only a null-terminated host name, and server_ptr
     * points to the server number. If the server number is missing there is
     * an error.
     */

    if (*server_ptr == '\0')
	return (-1);

    tmp_server_addrlen = 0;
    tmp_server_addr = NULL;

    /*
     * Convert the server number string to an integer.
     */
    server_num = atoi(server_ptr);
    tmp_svr_numlen = strlen(server_ptr);
    tmp_svr_num = FSmalloc(tmp_svr_numlen + 1);
    if (!tmp_svr_num) {
	return (-1);
    }
    strcpy(tmp_svr_num, server_ptr);

    /*
     * If the server name is missing, use current host.
     */
    if (serverbuf[0] == '\0')

#ifdef DNETCONN
	if (dnet)
	    (void) strcpy(serverbuf, "0");
	else
#endif

#ifdef UNIXCONN
	    ;			/* Do nothing if UNIX DOMAIN. Will be handled
				 * below. */
#else
	    (void) _FSGetHostname(serverbuf, sizeof serverbuf);
#endif				/* UNIXCONN else TCPCONN (assumed) */

#ifdef DNETCONN
    if (dnet) {
	struct dn_naddr *dnaddrp,
	            dnaddr;
	struct nodeent *np;

	/*
	 * build the target object name.
	 */
	sprintf(objname, "X$X%d", server_num);
	/*
	 * Attempt to open the DECnet connection, return -1 if fails.
	 */
	if ((fd = dnet_conn(serverbuf,
			    objname, SOCK_STREAM, 0, 0, 0, 0)) < 0 ||
	    fd >= OPEN_MAX) {
	    if (tmp_svr_num)
		FSfree(tmp_svr_num);
	    return (-1);	/* errno set by dnet_conn. */
	}
	if (dnaddrp = dnet_addr(serverbuf)) {	/* stolen from xhost */
	    dnaddr = *dnaddrp;
	} else {
	    if ((np = getnodebyname(serverbuf)) == NULL) {
		(void) close(fd);
		if (tmp_svr_num)
		    FSfree(tmp_svr_num);
		return (-1);
	    }
	    dnaddr.a_len = np->n_length;
	    bcopy(np->n_addr, dnaddr.a_addr, np->n_length);
	}
	tmp_server_addrlen = sizeof(struct dn_naddr);
	tmp_server_addr = FSmalloc(tmp_server_addrlen);
	if (!tmp_server_addr) {
	    (void) close(fd);
	    if (tmp_svr_num)
		FSfree(tmp_svr_num);
	    return (-1);
	}
	bcopy((char *) &dnaddr, tmp_server_addr, tmp_server_addrlen);
    } else
#endif

    {

#ifdef UNIXCONN
	if ((serverbuf[0] == '\0') ||
		(strcmp("unix", serverbuf) == 0)) {
	    /* Connect locally using Unix domain. */
	    unaddr.sun_family = AF_UNIX;
	    (void) strcpy(unaddr.sun_path, X_UNIX_PATH);
	    strcat(unaddr.sun_path, server_ptr);
	    addr = (struct sockaddr *) & unaddr;
	    addrlen = strlen(unaddr.sun_path) + 2;
	    /*
	     * Open the network connection.
	     */
	    if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0 ||
	        fd >= OPEN_MAX) {
		if (tmp_svr_num)
		    FSfree(tmp_svr_num);
		return (-1);	/* errno set by system call. */
	    }
	    /*
	     * This is a hack and is not part of the protocol
	     */
	    {
		char        tmpbuf[1024];

		tmp_server_addrlen = _FSGetHostname(tmpbuf, sizeof tmpbuf);
		tmp_server_addr = FSmalloc(tmp_server_addrlen + 1);
		if (!tmp_server_addr) {
		    if (tmp_svr_num)
			FSfree(tmp_svr_num);
		    (void) close(fd);
		    return (-1);
		}
		strcpy(tmp_server_addr, tmpbuf);
	    }
	} else
#endif

	{
	    char	*sp = serverbuf;

	    /* ignore a leading "tcp/" */
	    if (strncmp(sp, "tcp/", 4) == 0)
	    	sp += 4;

	    /* Get the statistics on the specified host. */
	    hostinetaddr = inet_addr(sp);
	    if (hostinetaddr == -1) {
		if ((host_ptr = gethostbyname(sp)) == NULL) {
		    /* No such host! */
		    errno = EINVAL;
		    if (tmp_svr_num)
			FSfree(tmp_svr_num);
		    return (-1);
		}
		/* Check the address type for an internet host. */
		if (host_ptr->h_addrtype != AF_INET) {
		    /* Not an Internet host! */
		    errno = EPROTOTYPE;
		    if (tmp_svr_num)
			FSfree(tmp_svr_num);
		    return (-1);
		}
		/* Set up the socket data. */
		inaddr.sin_family = host_ptr->h_addrtype;

#if defined(CRAY) && defined(OLDTCP)
		/* Only Cray UNICOS3 and UNICOS4 will define this */
		{
		    long        t;

		    bcopy((char *) host_ptr->h_addr,
			  (char *) &t,
			  sizeof(inaddr.sin_addr));
		    inaddr.sin_addr = t;
		}
#else
		bcopy((char *) host_ptr->h_addr,
		      (char *) &inaddr.sin_addr,
		      sizeof(inaddr.sin_addr));
#endif				/* CRAY and OLDTCP */
	    } else {

#if defined(CRAY) && defined(OLDTCP)
		/* Only Cray UNICOS3 and UNICOS4 will define this */
		inaddr.sin_addr = hostinetaddr;
#else
		inaddr.sin_addr.s_addr = hostinetaddr;
#endif				/* CRAY and OLDTCP */

		inaddr.sin_family = AF_INET;
	    }
	    addr = (struct sockaddr *) & inaddr;
	    addrlen = sizeof(struct sockaddr_in);
	    inaddr.sin_port = server_num;
	    inaddr.sin_port = htons(inaddr.sin_port);
	    /*
	     * Open the network connection.
	     */

	    if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0 ||
		fd >= OPEN_MAX) {
		if (tmp_svr_num)
		    FSfree(tmp_svr_num);
		return (-1);	/* errno set by system call. */
	    }
	    /* save address information */
	    {
		char       *cp;
		char        tmpbuf[1024];

#ifdef CRAY
#ifdef OLDTCP
		tmp_server_addrlen = sizeof(inaddr.sin_addr);
#else
		tmp_server_addrlen = SIZEOF_in_addr;
#endif
		cp = (char *) &inaddr.sin_addr;
#else
		tmp_server_addrlen = sizeof(inaddr.sin_addr.s_addr);
		cp = (char *) &inaddr.sin_addr.s_addr;
#endif /* CRAY */

		if ((tmp_server_addrlen == 4) &&
			(cp[0] == 127) && (cp[1] == 0) &&
			(cp[2] == 0) && (cp[3] == 1)) {
		    /*
		     * We are special casing the BSD hack localhost address
		     * 127.0.0.1, since this address shouldn't be copied to
		     * other machines.  So, we convert it to FamilyLocal. This
		     * is a hack and is not part of the protocol
		     */
		    tmp_server_addrlen = _FSGetHostname(tmpbuf, sizeof tmpbuf);
		    cp = tmpbuf;
		}
		tmp_server_addr = FSmalloc(tmp_server_addrlen);
		if (!tmp_server_addr) {
		    (void) close(fd);
		    if (tmp_svr_num)
			FSfree(tmp_svr_num);
		    return (-1);
		}
		bcopy(cp, tmp_server_addr, tmp_server_addrlen);
	    }

	    /* make sure to turn off TCP coalescence */

#ifdef TCP_NODELAY
	    {
		int         mi = 1;

		setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &mi, sizeof(int));
	    }
#endif
	}


	if (connect(fd, addr, addrlen) == -1) {
	    (void) close(fd);
	    if (tmp_svr_num)
		FSfree(tmp_svr_num);
	    if (tmp_server_addr)
		FSfree(tmp_server_addr);
	    return (-1);	/* errno set by system call. */
	}
    }
    /*
     * set it non-blocking.  This is so we can read data when blocked for
     * writing in the library.
     */

    /* ultrix reads hang on Unix sockets, hpux reads fail */
#if defined(O_NONBLOCK) && (!defined(ultrix) && !defined(hpux) && !defined(AIXV3))
    (void) fcntl (fd, F_SETFL, O_NONBLOCK);
#else
#ifdef FIOSNBIO
    {
	int arg = 1;
	ioctl (fd, FIOSNBIO, &arg);
    }
#else
#if defined(AIXV3) && defined(FIONBIO)
    {
	int arg;
	arg = 1;
	ioctl(fd, FIONBIO, &arg);
    }
#else
    (void) fcntl (fd, F_SETFL, FNDELAY);
#endif
#endif
#endif

    /*
     * Return the id if the connection succeeded. Rebuild the expanded spec
     * and return it in the result parameter.
     */
    server_ptr = serverbuf - 1;
    while (*(++server_ptr) != '\0');
    *(server_ptr++) = ':';

#ifdef DNETCONN
    if (dnet)
	*(server_ptr++) = ':';
#endif

    numbuf_ptr = tmp_svr_num;
    while (*numbuf_ptr != '\0')
	*(server_ptr++) = *(numbuf_ptr++);
    *server_ptr = '\0';
    (void) strcpy(expanded_name, serverbuf);
    return (fd);
#endif /* !HASSOCKETS */
}

/*
 * Disconnect from server.
 */

int
_FSDisconnectServer(server)
    int         server;

{
    (void) close(server);
}

#undef NULL
#define NULL ((char *) 0)
/*
 * This is an OS dependent routine which:
 * 1) returns as soon as the connection can be written on....
 * 2) if the connection can be read, must enqueue events and handle errors,
 * until the connection is writable.
 */
_FSWaitForWritable(svr)
    FSServer     *svr;
{
    unsigned long r_mask[MSKCNT];
    unsigned long w_mask[MSKCNT];
    int         nfound;

    CLEARBITS(r_mask);
    CLEARBITS(w_mask);

    while (1) {
	BITSET(r_mask, svr->fd);
	BITSET(w_mask, svr->fd);

	do {
	    nfound = select(svr->fd + 1, r_mask, w_mask, NULL, NULL);
	    if (nfound < 0 && errno != EINTR)
		(*_FSIOErrorFunction) (svr);
	} while (nfound <= 0);

	if (_FSANYSET(r_mask)) {
	    char        buf[BUFSIZE];
	    long        pend_not_register;
	    register long pend;
	    register fsEvent *ev;

	    /* find out how much data can be read */
	    if (BytesReadable(svr->fd, (char *) &pend_not_register) < 0)
		(*_FSIOErrorFunction) (svr);
	    pend = pend_not_register;

	    /*
	     * must read at least one fsEvent; if none is pending, then we'll
	     * just block waiting for it
	     */
	    if (pend < SIZEOF(fsEvent))
		pend = SIZEOF(fsEvent);

	    /* but we won't read more than the max buffer size */
	    if (pend > BUFSIZE)
		pend = BUFSIZE;

	    /* round down to an integral number of FSReps */
	    pend = (pend / SIZEOF(fsEvent)) * SIZEOF(fsEvent);

	    _FSRead(svr, buf, pend);

	    /* no space between comma and type or else macro will die */
	    STARTITERATE(ev, fsEvent, buf, (pend > 0),
			 (pend -= SIZEOF(fsEvent))) {
		if (ev->type == FS_Error)
		    _FSError(svr, (fsError *) ev);
		else		/* it's an event packet; enqueue it */
		    _FSEnq(svr, ev);
	    }
	    ENDITERATE
	}
	if (_FSANYSET(w_mask))
	    return;
    }
}


_FSWaitForReadable(svr)
    FSServer     *svr;
{
    unsigned long r_mask[MSKCNT];
    int         result;

    CLEARBITS(r_mask);
    do {
	BITSET(r_mask, svr->fd);
	result = select(svr->fd + 1, r_mask, NULL, NULL, NULL);
	if (result == -1 && errno != EINTR)
	    (*_FSIOErrorFunction) (svr);
    } while (result <= 0);
}

_FSSendClientPrefix(svr, client)
    FSServer     *svr;
    fsConnClientPrefix *client;
{
    struct iovec iovarray[5],
               *iov = iovarray;
    int         niov = 0;

#define add_to_iov(b,l) \
	  { iov->iov_base = (b); iov->iov_len = (l); iov++, niov++; }

    add_to_iov((caddr_t) client, SIZEOF(fsConnClientPrefix));

#undef add_to_iov

    (void) WritevToServer(svr->fd, iovarray, niov);
    return;
}
