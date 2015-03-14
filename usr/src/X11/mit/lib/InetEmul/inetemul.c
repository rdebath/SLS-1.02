/* $Header: /home/x_cvs/mit/lib/InetEmul/inetemul.c,v 1.1 1992/07/07 11:49:00 dawes Exp $ */
/*
 * Copyright 1990,91 by Thomas Roell, Dinkelscherben, Germany.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Thomas Roell not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  Thomas Roell makes no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 *
 * THOMAS ROELL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THOMAS ROELL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Thomas Roell, roell@informatik.tu-muenchen.de
 *
 * /home/x_cvs/mit/server/ddx/x386/etc/inetemul.c,v 1.1.1.1 1992/05/01 13:15:04 root Exp
 */


/*
 * This is a set of dummy-routines to make shared libs that were compiled for
 * sockets linkable without libinet.a
 *
 * They all return with an error-condition !!!
 */
#include <stdio.h> /* NULL */
#include <errno.h>


unsigned long inet_addr(cp)
     char *cp;
{
  return(-1);
}


void *gethostbyname(name)
     char *name;
{
  return (NULL);
}

int socket(domain, type, protocol)
     int domain, type, protocol;
{
  /* errno here, since they are only defined for TCP/IP ... */
  return(-1);
}



int setsockopt(s, level, optname, optval, optlen)
     int s, level, optname;
     char optval;
     int optlen;
{
  errno = EBADF;
  return (-1);
}     



int connect(s, name, namelen)
     int s;
     void *name;
     int namelen;
{
  errno = EBADF;
  return (-1);
}


int bind(s, name, namelen)
     int s;
     void *name;
     int namelen;
{
  errno = EBADF;
  return (-1);
}



int listen(s, backlog)
     int s, backlog;
{
  errno = EBADF;
  return (-1);
}



int accept(s, addr, addrlen)
     int s;
     void *addr;
     int *addrlen;
{
  errno = EBADF;
  return (-1);
}



int getpeername(s, name, namelen)
     int s;
     void *name;
     int namelen;
{
  errno = EBADF;
  return (-1);
}



int recvfrom(s, buf, len, flags, from, fromlen)
     int s;
     char buf;
     int len, flags;
     void *from;
     int fromlen;
{
  errno = EBADF;
  return (-1);
}



int sendto(s, msg, len, flags, to, tolen)
     int s;
     char msg;
     int len, flags;
     void *to;
     int tolen;
{
  errno = EBADF;
  return (-1);
}

#ifdef ESIX
/* This one is taken from lib/X/Xstream.c */

#include <sys/param.h>
#ifndef NOFILES_MAX
#define NOFILES_MAX 128
#endif

#ifndef OPEN_MAX
#define OPEN_MAX NOFILES_MAX
#endif

#if OPEN_MAX > 256
#undef OPEN_MAX
#define OPEN_MAX 256
#endif

#define MSKCNT ((OPEN_MAX + 31) / 32)

#if (MSKCNT==1)
#define BITMASK(i) (1 << (i))
#define MASKIDX(i) 0
#endif
#if (MSKCNT>1)
#define BITMASK(i) (1 << ((i) & 31))
#define MASKIDX(i) ((i) >> 5)
#endif

#define MASKWORD(buf, i) buf[MASKIDX(i)]
#define BITSET(buf, i) MASKWORD(buf, i) |= BITMASK(i)
#define BITCLEAR(buf, i) MASKWORD(buf, i) &= ~BITMASK(i)
#define GETBIT(buf, i) (MASKWORD(buf, i) & BITMASK(i))

#ifdef DEBUG
#define PRMSG(x,a,b)	fprintf(stderr, x,a,b); fflush(stderr)
#else
#define PRMSG(x,a,b)
#endif

#include <sys/time.h>
#include <sys/poll.h>

#define POLLERROR		(POLLHUP | POLLNVAL | POLLERR)
#define PFD(fds, i, x) { 	if (fds) 		if (ev & (x)) 			BITSET (fds, i); 		else 			BITCLEAR (fds, i); }
#define ERROR(x) { 	errno = x; 	return -1; }
/*
	simulate BSD select system call with SYSV poll system call
	note that efds parameter is not fully supported (or understood)
*/

extern long ulimit();

int
select (nfds, rfds, wfds, efds, timeout)
int nfds;
unsigned long *rfds;
unsigned long *wfds;
unsigned long *efds;
struct timeval *timeout;
{
	int i, rc, ev, timevalue;
	struct pollfd pfds[NOFILES_MAX];
	static long _NOFILE = 0;

	PRMSG("in select\n", 0,0);

	if (_NOFILE == 0) {
		_NOFILE = ulimit(4, (long)0);
		if (_NOFILE > NOFILES_MAX)
			_NOFILE = NOFILES_MAX;
	}

 	if (nfds > _NOFILE)
		nfds = _NOFILE;   /* make poll happy */

	for (i = 0; i < nfds; i++)
	{
		ev = 0;

		if (rfds && GETBIT (rfds, i)) ev |= POLLIN;
		if (wfds && GETBIT (wfds, i)) ev |= POLLOUT;
		if (ev || (efds && GETBIT (efds, i)))
			pfds[i].fd = i;
		else
			pfds[i].fd = -1;
		pfds[i].events = ev;
	}
	if (timeout)
		timevalue = timeout->tv_sec * 1000 + timeout->tv_usec / 1000;
	else
		timevalue = -1;

	while (1)	{
	   rc = poll (pfds, (unsigned long)nfds, timevalue);

	   if(rc<0 && errno == EAGAIN)
		continue;
	   else	break;
	}
	if(rc>0)	{
		if (!efds)
			for (i = 0; i < nfds; ++i)
			{
				ev = pfds[i].revents;
				if (ev & POLLERROR)
					ERROR (EBADF);
			}

		for (i = 0; i < nfds; ++i)
		{
			ev = pfds[i].revents;
			PFD (rfds, i, POLLIN);
			PFD (wfds, i, POLLOUT);
			PFD (efds, i, POLLERROR);
		}
	} 

	if(rc==0)	{
		i = (nfds+ 7)/8;
		if ( rfds != NULL)
			memset((char *) rfds, 0, i);
		if ( wfds != NULL)
			memset((char *) wfds, 0, i);
		if ( efds != NULL)
			memset((char *) efds, 0, i);
	
	}
	
	return rc;
}
#endif /* ESIX */
