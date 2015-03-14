/* $XConsortium: itserver.c,v 1.3 91/02/17 15:20:18 rws Exp $ */

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

#define _USHORT_H	/* prevent conflicts between BSD sys/types.h and
                           interlan/il_types.h */
#ifdef LACHMAN
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#else
#include <interlan/il_types.h>
#include <interlan/socket.h>
#include <interlan/netdb.h>
#include <interlan/in.h>
#endif
#include <tiuser.h>
#include <sys/param.h>
#include <sys/utsname.h>
#include <sys/signal.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <X11/Xproto.h>
#include "Xstreams.h"				/* in Xlib sources */
#include <X11/X.h>

#include "osdep.h"

#ifdef	LACHMAN
#define	INADDRSIZE 16
#else
#define	INADDRSIZE 8
#endif

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
int	FamilyType;


typedef struct {
    int af, xf;
} FamilyMap;

static FamilyMap familyMap[] = {
#ifdef     AF_DECnet
    {AF_DECnet, FamilyDECnet},
#endif /* AF_DECnet */
#ifdef     AF_CHAOS
    {AF_CHAOS, FamilyChaos},
#endif /* AF_CHAOS */
#ifdef    AF_INET
    {AF_INET, FamilyInternet},
#endif
#ifdef    FamilyUname
    {AF_UNSPEC, FamilyUname}
#endif
};

#define FAMILIES ((sizeof familyMap)/(sizeof familyMap[0]))

static int XFamily(af)
    int af;
{
    int i;
    for (i = 0; i < FAMILIES; i++)
	if (familyMap[i].af == af)
            return familyMap[i].xf;
    return FamilyUname;
}

static int UnixFamily(xf)
    int xf;
{
    int i;
    for (i = 0; i < FAMILIES; i++)
	if (familyMap[i].xf == xf)
            return familyMap[i].af;
    return AF_UNSPEC;
}

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
		fprintf(stderr, "Cannot read HEADERSIZE\n");
		SendNull();
		return(-1);
	}

	iop->bufptr = HEADERSIZE;
	iop->msglen = *(int *) iop->inputbuf;

	if(iop->buflen < iop->msglen)
	{
	   if((iop->inputbuf = (char *) xrealloc(iop->inputbuf, iop->msglen)) == NULL)
	   {
         	SendNull();
		return;
	   }
	   iop->buflen	= iop->msglen;
	}

	if(!Read(0, &iop->inputbuf[iop->bufptr], iop->msglen - iop->bufptr))
	{
		fprintf(stderr, "Cannot read The rest of the message\n");
		SendNull();
		return(-1);
        }

	ptr = &iop->inputbuf[sizeof(int)];
	m = *(int *) ptr;
	ptr += sizeof(int);

	flags = *(int *) ptr;
	ptr += sizeof(int);

	dispno = *(int *) ptr;

	sprintf(display, "%d", dispno);
	ptr += sizeof(int);
	n = *(int *) ptr;

	ptr += sizeof(int);
	net  = ptr;

	if(strcmp(net, "it") != 0)
	{
	}
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
#ifdef DEBUG
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


int	ConvertEthernetName();
int	ConvertEthernetAddress();
int	MakeEthernetCall();

int	bufsize = 512;

char	*getnextentry();


int
ConvertEthernetName(pktptr, n, entry, len)
char	**pktptr, *entry;
int	n, len;
{
 	struct hostent *hp;
	unsigned long	address;
	int	port;
        char    *ptr;
	int	entlen = INADDRSIZE;
	int	rndlen;

#ifdef DEBUG
fprintf(stderr, "in ConvertEthernetName %s\n", entry);
#endif

	rndlen = ((sizeof(xHostEntry) + entlen + 3) >> 2) << 2;

	if (isascii(entry[0]) && isdigit(entry[0]))
	    address = inet_addr (entry);
	else
	    address = (unsigned long)~0L;
	if (address == (unsigned long)~0L) {
	    hp = gethostbyname(entry);
	    if (hp)
		address = *(unsigned long *)hp->h_addr;
	}
	if (address == (unsigned long)~0L) 
	{
		entlen = strlen(display) + len + 2;
		rndlen = ((sizeof(xHostEntry) + entlen + 3) >> 2) << 2;
		if((*pktptr = alialloc(*pktptr, n+rndlen)) == NULL)
					return(-1);
		ptr = &(*pktptr)[n];
		((xHostEntry *)ptr)->family = XFamily(AF_UNSPEC);
		((xHostEntry *)ptr)->length = entlen;
		ptr += sizeof(xHostEntry);

		sprintf(ptr, "%s", entry);
		return(n+rndlen);
	}
	port = atoi(display);
        port += X_TCP_PORT;

	if((*pktptr = alialloc(*pktptr, n+rndlen)) == NULL)
		return(-1);

	ptr = &(*pktptr)[n];
	((xHostEntry *)ptr)->family = XFamily(AF_INET);
	((xHostEntry *)ptr)->length = entlen;
	ptr += sizeof(xHostEntry);
/*
	*(short *) ptr = htons(AF_INET);
	*(short *) (ptr + sizeof(short)) = htons(port);
	*(int *) (ptr + sizeof(short) + sizeof(short)) = address;
*/
	*(int *) ptr = address;

#ifdef DEBUG
fprintf(stderr, "creating address for host %s address<0x%x>\n", entry, address);
#endif

	return(n+rndlen);
}

int
BindEthernetName(pktptr, n, entry, len)
char	**pktptr, *entry;
int	n, len;
{
	struct hostent *hp;
	unsigned long	address;
	int	port;
	char	*ptr;
	int	entlen = INADDRSIZE;
	int	rndlen;

#ifdef DEBUG
fprintf(stderr, "in ConvertEthernetName %s\n", entry);
#endif

	rndlen = ((sizeof(xHostEntry) + entlen + 3) >> 2) << 2;

	if (isascii(entry[0]) && isdigit(entry[0]))
	    address = inet_addr (entry);
	else
	    address = (unsigned long)~0L;
	if (address == (unsigned long)~0L) {
	    hp = gethostbyname(entry);
	    if (hp)
		address = *(unsigned long *)hp->h_addr;
	}
	if (address == (unsigned long)~0L) 
	{
	   return(-1);
	}
	port = atoi(display);
	port += X_TCP_PORT;

	if((*pktptr = alialloc(*pktptr, n+rndlen)) == NULL)
		return(-1);

	ptr = &(*pktptr)[n];
	((xHostEntry *)ptr)->family = AF_INET;
	((xHostEntry *)ptr)->length = entlen;
	ptr += sizeof(xHostEntry);
#ifdef LACHMAN
	*(short *) ptr = AF_INET;
#else
	*(short *) ptr = htons(AF_INET);
#endif
	*(short *) (ptr + sizeof(short)) = htons(port);
	*(int *) (ptr + sizeof(short) + sizeof(short)) = address;

#ifdef DEBUG
fprintf(stderr, "creating address for host %s address<0x%x>\n", entry, address);
#endif

	return(n+rndlen);
}



MakeEthernetCall(pktptr, n, entry, len)
char	**pktptr, *entry;
int	n, len;
{
 	struct hostent *hp;
	unsigned long	address;
	int	port;
	char	*ptr;
	int	rndlen;
	int	ra, ro, ru;
	int	a, o, u;
	struct	utsname machine;

#ifdef DEBUG
fprintf(stderr, "in MakeEthernetCall %s\n", entry);
#endif

        a  = INADDRSIZE;
	o  = 0;
	u  = 0;

        ra = ((a + sizeof(xHostEntry) + 3) >> 2) << 2;
	ro = ((o + sizeof(xHostEntry) + 3) >> 2) << 2;
	ru = ((u + sizeof(xHostEntry) + 3) >> 2) << 2;

        rndlen = ra + ro + ru;

        if((*pktptr = alialloc(*pktptr, n+rndlen)) == NULL)
		return(-1);


	if (isascii(entry[0]) && isdigit(entry[0]))
	    address = inet_addr (entry);
	else
	    address = (unsigned long)~0L;
	if (address == (unsigned long)~0L) {
	    hp = gethostbyname(entry);
	    if (hp)
		address = *(unsigned long *)hp->h_addr;
	}
	if (address == (unsigned long)~0L) 
	{
		return(-1);
	}
        port = atoi(display);
	port += X_TCP_PORT;

        if((*pktptr = alialloc(*pktptr, n+rndlen)) == NULL)
		return(-1);

        ptr = &(*pktptr)[n];
	((xHostEntry *)ptr)->length = a;

	ptr += sizeof(xHostEntry);
#ifdef LACHMAN
	*(short *) ptr = AF_INET;
#else
	*(short *) ptr = htons(AF_INET);
#endif
	*(short *) (ptr + sizeof(short)) = htons(port);
	*(int *) (ptr + sizeof(short) + sizeof(short)) = address;

	ptr = &(*pktptr)[n+ra];
	((xHostEntry *)ptr)->length = o;

	ptr = &(*pktptr)[n+ra+ro];
	((xHostEntry *)ptr)->length = u;

#ifdef DEBUG
fprintf(stderr, "creating address for host %s address<0x%x> and returning +%d\n",
		 entry, address, rndlen);
#endif

	return(n+rndlen);
}

int
ConvertEtherCallToName(pktptr, n, entry, len)
char	**pktptr, *entry;
int	n, len;
{
 	int	l, rl;
	char	*ptr;
	struct hostent *hp;
	unsigned long	address;

        ptr = entry;

/*
  	nf = *(short *) ptr;
	port = *(short *) (ptr + sizeof(short));
*/
  	address = *(int *) (ptr + sizeof(short) + sizeof(short));
	if((hp = gethostbyaddr(&address, sizeof(int), AF_INET)) == NULL){
		fprintf(stderr, "gethostbyaddr() failed \n");
		return(n);
		}


        l = strlen(hp->h_name) + 1;

        rl = ((sizeof(xHostEntry) + l + 3) >> 2) << 2;

        if((*pktptr = alialloc(*pktptr, n+rl)) == NULL)
		return(-1);

        ptr = &(*pktptr)[n];
	((xHostEntry *)ptr)->family = 1;
	((xHostEntry *)ptr)->length = l;

	ptr += sizeof(xHostEntry);

	sprintf(ptr, hp->h_name);

#ifdef DEBUG
fprintf(stderr, "getting the name for host %s\n", hp->h_name);
#endif

        return(rl+n);
}

int
ConvertEthernetAddress(pktptr, n, entry, len)
char	**pktptr, *entry;
int	n, len;
{
	register i;
 	char	*ptr;
        int     entlen; 
        int     rndlen;
 	struct hostent *hp;
	int	address;
	char	*name, buf[32];

/*
	nf = *(short *) entry;
	port = *(short *) (entry + sizeof(short));
	address = *(int *) (entry + sizeof(short) + sizeof(short));
*/
	address = *(int *) entry;
	if((hp = gethostbyaddr(&address, sizeof(int), AF_INET)) == NULL)
	{
/*
		fprintf(stderr, "gethostbyaddr() failed \n");
		return(n);
*/
		if(FamilyType == AF_UNSPEC){
			name = entry;
			entlen = len + 1;
			}
		else {
			sprintf(buf, "%d", address);
			fprintf(stderr, "%d: unknown address\n", address);
			entlen = strlen(buf) +1;
			name = buf;
		}
	}
	else {
		entlen = strlen(hp->h_name) + 1;
		name = hp->h_name;
	}

        rndlen = ((sizeof(xHostEntry) + entlen + 3) >> 2) << 2;

        if((*pktptr = alialloc(*pktptr, n+rndlen)) == NULL)
                return(-1);

        ptr = &(*pktptr)[n];
        ((xHostEntry *)ptr)->family = FamilyType;
        ((xHostEntry *)ptr)->length = entlen;
        ptr += sizeof(xHostEntry);

#ifdef DEBUG
fprintf(stderr, "getting the name for host %s\n", name);
#endif
  	memcpy(ptr, name, entlen);	
	
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
	FamilyType = UnixFamily(((xHostEntry *) inbuf)->family);

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
	fprintf(stderr, "in addentry %s\n", entry);
#endif
	switch(flags)
	{
		case	ConvertNameToNetAddr:
			return(ConvertEthernetName(pktptr, n, entry, len));
		case	ConvertNetAddrToName:
			return(ConvertEthernetAddress(pktptr, n, entry, len));
		case	ConvertNameToTliCall:
			return(MakeEthernetCall(pktptr, n, entry, len));
		case	ConvertTliCallToName:
			return(ConvertEtherCallToName(pktptr, n, entry, len));
		case    ConvertNameToTliBind:
			return(BindEthernetName(pktptr, n, entry, len));
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
 	int	n;
	int	m = 0;
	int	t = count;
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

