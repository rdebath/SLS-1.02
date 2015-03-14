#ifndef NOIDENT
#ident	"@(#)nameserver:starserver.c	1.4"
#endif

/*
 * Copyright 1988, 1989 AT&T, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of AT&T not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  AT&T makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * AT&T DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL AT&T
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
*/

#include <X11/Xos.h>
#include <tiuser.h>
#include <sys/param.h>
#include <sys/utsname.h>
#include <sys/signal.h>
#include <errno.h>
#include <stdio.h>
#include <X11/Xproto.h>
#include "Xstreams.h"				/* in Xlib sources */

#include "osdep.h"


extern	char	*calloc(), *realloc(), *alialloc();
extern  char    *program;
int	network;
int	nextentry;

char	*xalloc();
char	*xrealloc();
char *makePacket();

char    *TheEnd;
char    *inbuf;
int     inlen;
int     dispno;
char	display[64];
int     nhosts;
int	nHosts;
int     flags = 0;

IOBUFFER InputBuffer[1];

main()
{
	ServiceClient();
/*
	sleep(3);
*/
}

SendNull()
{
	char	buf[32];
	char	*ptr;

	ptr = buf;
	*(int *) ptr = 0;
	ptr += sizeof(int);
	*(int *) ptr = 0;
	write(1, buf, 2*sizeof(int));
}

ServiceClient()
{
	register IOBUFFER *iop = &InputBuffer[0];
	int	n,m;
	char	*ptr, *net;

	if((iop->inputbuf = (char *) xalloc(BUFSIZE)) == NULL)
	{
		SendNull();
		return;
	}
	iop->buflen	= BUFSIZE;

	if(!Read(0, iop->inputbuf, HEADERSIZE))
	{
		fprintf(stderr, "Cannot read the HEADERSIZE\n");
		SendNull();
		return(-1);
	}

	iop->bufptr = HEADERSIZE;
	iop->msglen = *(int *) iop->inputbuf;

	if(iop->buflen < iop->msglen)
	{
	   iop->inputbuf = (char *) xrealloc(iop->inputbuf, iop->msglen);
	   if( iop->inputbuf == NULL)
           {
                SendNull();
                return;
           }
	   iop->buflen	= iop->msglen;
	}

	if(!Read(0, &iop->inputbuf[iop->bufptr], iop->msglen - iop->bufptr))
	{
		fprintf(stderr, "Cannot read the rest of the message\n");
		SendNull();
		return(-1);
        }

	ptr = &iop->inputbuf[sizeof(int)];
	m = *(int *) ptr;
	ptr += sizeof(int);

	flags = *(int *) ptr;
	ptr += sizeof(int);

	dispno = *(int *) ptr;

	ptr += sizeof(int);
	n = *(int *) ptr;

	ptr += sizeof(int);
	net  = ptr;

	ptr = &iop->inputbuf[m];
	inlen = *(int *) ptr;

	ptr += sizeof(int);
	nhosts = *(int *) ptr;

	inbuf = ptr + sizeof(int);
	TheEnd = &inbuf[inlen];
#ifdef DEBUG
	write(2, inbuf, inlen);
#endif
        nextentry = ((xHostEntry *) inbuf)->length;
        if((ptr = (char *) makePacket()) != NULL)
	{
#ifdef DEBUG1
                	write(2, ptr, (*(int *) ptr) + 2*sizeof(int));
#endif
                	write(1, ptr, (*(int *) ptr) + 2*sizeof(int));
        }
	return(1);
}


char *
xalloc(n)
int	n;
{
	char	*ptr;

	if((ptr = (char *) malloc(n)) == NULL)
	{
		fprintf(stderr, "malloc failed\n");
		return(NULL);
	}
	return(ptr);
}


char *
xrealloc(buf, n)
char	*buf;
int	n;
{
 	char	*ptr;

        if((ptr = (char *) realloc(buf, n)) == NULL)
	{
         	fprintf(stderr, "realloc failed\n");
		return(NULL);
	}
        return(ptr);
}


int	ConvertStarlanAddress();
int	ConvertStarlanName();
int	MakeStarlanCall();

int	bufsize = 512;

char	*getnextentry();

int
BindStarlanName(pktptr, n, entry, len)
char	**pktptr, *entry;
int	n, len;
{
 	int	entlen;
	int	rndlen;
	char	*ptr;

#ifdef DEBUG
fprintf(stderr, "in ConvertStarlanName %s\n", entry);
#endif
	sprintf(display, "%s%d", entry, dispno);
	entlen = strlen(display) + 1;
        rndlen = ((sizeof(xHostEntry) + entlen + 3) >> 2) << 2;
	if((*pktptr = alialloc(*pktptr, n+rndlen)) == NULL)
                                return(-1);

        ptr = &(*pktptr)[n];
	((xHostEntry *)ptr)->family = FamilyUname;
	((xHostEntry *)ptr)->length = entlen - 1;
	ptr += sizeof(xHostEntry);

        sprintf(ptr, "%s", display);
#ifdef DEBUG
fprintf(stderr, "creating address for host %s, address<%s>\n", entry, ptr);
#endif

        return(n+rndlen);
}

int
ConvertStarlanName(pktptr, n, entry, len)
char	**pktptr, *entry;
int	n, len;
{
	int	entlen = 1 + len ;
	int	rndlen;
	char	*ptr;

#ifdef DEBUG
fprintf(stderr, "in ConvertStarlanName %s\n", entry);
#endif

	rndlen = ((sizeof(xHostEntry) + entlen + 3) >> 2) << 2;
	if((*pktptr = alialloc(*pktptr, n+rndlen)) == NULL)
				return(-1);

	ptr = &(*pktptr)[n];
        ((xHostEntry *)ptr)->family = FamilyUname;
        ((xHostEntry *)ptr)->length = entlen;
        ptr += sizeof(xHostEntry);

	sprintf(ptr, "%s", entry);
#ifdef DEBUG
fprintf(stderr, "creating address for host %s, address<%s>\n", entry, ptr);
#endif
        
	return(n+rndlen);
}

int
ConvertStarCallToName(pktptr, n, entry, len)
char	**pktptr, *entry;
int	n, len;
{
	int	l, rl;
	char	*src, *ptr;
	int	a, o, u;

#ifdef DEBUG
fprintf(stderr, "In ConvertStarCallToName()\n");
#endif

	if(
	    getnextentry(&l) == NULL ||
	    (src = getnextentry(&l)) == NULL
	  ){
		fprintf(stderr,
		"ConvertStarCallToName didn't receive a correct TLI message\n");
		return(-1);
	}

        rl = ((l + sizeof(xHostEntry) + 3) >> 2) << 2;

        if((*pktptr = alialloc(*pktptr, n+rl)) == NULL)
		return(-1);

        ptr = &(*pktptr)[n];
	((xHostEntry *)ptr)->length = l;

	ptr += sizeof(xHostEntry);
	
	bcopy(src, ptr, l);
#ifdef DEBUG
	fprintf(stderr, "ConvertStarCallToName returns %s\n", ptr);
#endif
	return(rl+n);
}


int
MakeStarlanCall(pktptr, n, entry, len)
char	**pktptr, *entry;
int	n, len;
{
	char	*ptr;
	int	rndlen;
	int	ra, ro, ru;
	int	a, o, u;
	struct	utsname machine;

#ifdef DEBUG
fprintf(stderr, "in MakeStarlanCall %s\n", entry);
#endif

	if(uname(&machine) < 0)
		return(-1);

	sprintf(display, "%s%d", entry, dispno);
	a  = strlen(display) + 1;
	o  = 0;
	u  = strlen(machine.nodename) + 1;

	ra = ((a + sizeof(xHostEntry) + 3) >> 2) << 2;
	ro = ((o + sizeof(xHostEntry) + 3) >> 2) << 2;
        ru = ((u + sizeof(xHostEntry) + 3) >> 2) << 2;

        rndlen = ra + ro + ru;

	if((*pktptr = alialloc(*pktptr, n+rndlen)) == NULL)
		return(-1);


	ptr = &(*pktptr)[n];
	((xHostEntry *)ptr)->length = a - 1;
	ptr += sizeof(xHostEntry);
	sprintf(ptr, "%s", display);

#ifdef DEBUG
fprintf(stderr, "creating address for host %s address<%s>\n", entry, ptr);
#endif

	ptr = &(*pktptr)[n+ra];
	((xHostEntry *)ptr)->length = o;


	ptr = &(*pktptr)[n+ra+ro];
	((xHostEntry *)ptr)->length = u;
	ptr += sizeof(xHostEntry);
	sprintf(ptr, "%s", machine.nodename);

	return(n+rndlen);
}


int
ConvertStarlanAddress(pktptr, n, entry, len)
char	**pktptr, *entry;
int	n, len;
{
	char	*ptr;
	int	entlen = len;
	int	rndlen;

	rndlen = ((sizeof(xHostEntry) + entlen + 3) >> 2) << 2;

	if((*pktptr = alialloc(*pktptr, n+rndlen)) == NULL)
		return(-1);

	ptr = &(*pktptr)[n];
	((xHostEntry *)ptr)->family = FamilyUname;
	((xHostEntry *)ptr)->length = entlen;
	ptr += sizeof(xHostEntry);

#ifdef DEBUG
fprintf(stderr, "getting the name for host %s\n", entry);
#endif
	bcopy(entry, ptr, len);

	return(n+rndlen);
}

char	*
getnextentry(plen)
int	*plen;
{
	char	*ptr;
	int	n = nextentry;

#ifdef DEBUG
fprintf(stderr,"In getnextentry()\n");
#endif
	if(inbuf >= TheEnd)
	{
		*plen = -1;
		return(NULL);	
	}

	*plen = nextentry;
        ptr = inbuf + sizeof(xHostEntry);
	inbuf += ((sizeof(xHostEntry) + *plen + 3) >> 2) << 2;
	nextentry = ((xHostEntry *) inbuf)->length;
	ptr[*plen] = '\0';
        nhosts++;
	return(ptr);
}

char *
makePacket()
{
    char *pktptr = NULL, *ptr;
    int	len;
    int	n, m;

    n = sizeof(int) * 2;
    pktptr = (char *) malloc(bufsize);

#ifdef DEBUG
fprintf(stderr,"In makePacket()\n");
#endif

    if(pktptr == NULL)
	return(NULL);
    
    for(nHosts = 0; nHosts < nhosts;)
    {
	ptr = getnextentry(&len);
	if(len < 0)
		break;
	if(len == 0 || ptr == NULL)
		continue;	
	m = addentry(&pktptr, n, ptr, len);
	if(m > n){
		nHosts++;
		n = m;
		}
    }
#ifdef DEBUG
    fprintf(stderr, "packet size is %d\n", n);
#endif

    *(int *) pktptr = n - 2*sizeof(int);
    *(int *) (pktptr+sizeof(int)) = nHosts;
    return(pktptr);
}

int
addentry(pktptr, n, entry, len)
char	**pktptr, *entry;
int	n, len;
{

#ifdef DEBUG
	fprintf(stderr, "in addStarlanEntry %s\n", entry);
#endif

	switch(flags)
	{
		case	ConvertNameToNetAddr:
			return(ConvertStarlanName(pktptr, n, entry, len));
		case	ConvertNetAddrToName:
			return(ConvertStarlanAddress(pktptr, n, entry, len));
		case	ConvertNameToTliCall:
			return(MakeStarlanCall(pktptr, n, entry, len));
		case	ConvertTliCallToName:
			return(ConvertStarCallToName(pktptr, n, entry, len));
		case    ConvertNameToTliBind:
			return(BindStarlanName(pktptr, n, entry, len));
	}
	return(-1);
}

char *
alialloc(ptr, size)
char	*ptr;
int	size;
{
	if(bufsize < size){
		bufsize = size + 512;
		ptr = realloc(ptr, bufsize);
		}
	return(ptr);
}


Read(fd, buf, count)
int	fd, count;
char	*buf;
{
 	int	n, m = 0, t = count;

	while((n = read(fd, buf, count)) > 0)
	{
         	if(n == count)
		{
                 	return(1);
		}
                buf += n;
		count -= n;
		m += n;
	}
	fprintf(stderr, "Trying to read %d but only %d read\n", t, m);
        return(0);
}

