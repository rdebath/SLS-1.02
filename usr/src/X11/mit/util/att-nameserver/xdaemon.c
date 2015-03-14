#ifndef NOIDENT
#ident	"@(#)nameserver:xdaemon.c	1.6"
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
#include <errno.h>
#include <sys/param.h>
#include <signal.h>
#include <sys/stream.h>
#include <sys/stropts.h>
#include <stdio.h>
#include "Xstreams.h"				/* in Xlib sources */
#include <sys/utsname.h>
#include <X11/Xproto.h>

#include "osdep.h"

#define	NSECONDS	2	

extern	char	*calloc(), *realloc();
extern	char	*program;
int	network;
int	nextentry;

char	*xalloc();
char	*xrealloc();
char *makePacket();

char	*TheEnd;
char	*inbuf;
int	inlen;
int	dispno;
char	display[64];
int	nhosts;
int	nHosts;
int	flags = 0;

IOBUFFER InputBuffer[MAXSOCKS];

extern int t_errno;


int ListenFd;
int lastfdesc;			/* maximum file descriptor */

long WellKnownConnections;    /* Listener mask */
long AllSockets[mskcnt];	/* select on this */
long AllClients[mskcnt];	      /* available clients */
long LastSelectMask[mskcnt];	      /* mask returned from last select call */
long MaxClients = MAXSOCKS ;

#define SUCCESS		"1"
static char	*ptmx = "/dev/ptmx";

/*
int	sig = -1;
catchit()
{
}
*/


WaitForInput()
{
    int	Quit();
    int i;
    struct timeval waittime, *wt;
    long timeout;
    long readyClients[mskcnt];
    long curclient;
    int selecterr;

    CLEARBITS(readyClients);

    COPYBITS(AllSockets, LastSelectMask);

    wt = NULL;

/* 
    if (!ANYSET(AllClients))
    {
    	wt = &waittime;
	waittime.tv_sec  = 5*60;
	waittime.tv_usec = 0;
    }
    else {
	sleep(1);
        alarm(NSECONDS);
        signal(SIGALRM, catchit);
        while(wait((int *)0) > 0);
    	alarm(0);
    	wt = NULL;
    }

*/
    i = select (MAXSOCKS, LastSelectMask, (int *) NULL, (int *) NULL, wt);

    selecterr = errno;
    
    if (i <= 0) /* An error or timeout occurred */
    {
	if(i == 0)
		Quit();
	if (i < 0) 
		if (selecterr == EBADF)    /* Some client disconnected */
			CheckConnections ();
		else if (selecterr != EINTR)
			fprintf(stderr, "WaitForInput(): select: errno=%d\n",
								selecterr);
    }
    else
    {
	MASKANDSETBITS(readyClients, LastSelectMask, AllClients); 
	if (LastSelectMask[0] & WellKnownConnections) 
		   EstablishNewConnections();
    }
    if (ANYSET(readyClients))
    {
	for (i=0; i<mskcnt; i++)
	{
	    while (readyClients[i])
	    {
		curclient = NextSetBit (readyClients[i]) - 1;
		ServiceClient(curclient);	
		readyClients[i] &= ~(1 << curclient);
	    }
	}	
    }
}

/* Routines for handling local streams (streams-pipes) */

Quit(sig)
int	sig;
{
    fprintf(stderr, "xdeamon: received signal %d\n", sig);

    if(unlink(NAME_SERVER_NODE) < 0 && errno != ENOENT){
		fprintf(stderr, "Cannot unlink %s", NAME_SERVER_NODE);
		perror(" ");
		}
    exit(0);
}

OpenTheListener()
{
	int 	munix, sunix;
	char *	slave;
	char	buf[64];

/*
	signal(SIGHUP, Quit);
*/
	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, Quit);
	signal(SIGQUIT,Quit);
	signal(SIGTERM, Quit);

	sprintf(buf, "%s", NAME_SERVER_NODE);
	if(open(buf, O_RDWR) >= 0){
		fprintf(stderr, "Xdaemon is already running\n");
		return(-1);
		}
	if( (munix = open(ptmx, O_RDWR)) < 0 ){
		fprintf(stderr,"Cannot open %s", ptmx);
		perror(" ");
		return(-1);
	}
	grantpt(munix);
	unlockpt(munix);

	if(unlink(buf) < 0 && errno != ENOENT){
		fprintf(stderr, "Cannot unlink %s", buf);
		perror(" ");
		return(-1);
		}

	slave = (char *) ptsname(munix);
	if( link(slave, buf) <0 ){
		fprintf(stderr, "Cannot link %s to %s", slave, buf);
		perror(" ");
		return(-1);
		}
	if( chmod(buf, 0666) < 0){
		fprintf(stderr, "Cannot chmod %s", buf);
		perror(" ");
		return(-1);
		}

	sunix = open(buf, O_RDWR);
	if(sunix < 0){
		fprintf(stderr, "Cannot open %s", buf);
		perror(" ");
		close(munix);
		return(-1);
		}
	setpgrp();
/*
	if(fcntl(munix, F_SETFL, FNDELAY) < 0)
	{
		fprintf(stderr, "Cannot set nodelay on the nameserver\n");
		exit(1);

	}
*/

	return(munix);
}

ConnectNewClients(lfd, MoreConnections)
int	lfd;
char	* MoreConnections;
{
	
	int fd;
	int read_in;
	char length;
	char buf[64];

	*MoreConnections = 0;

	if( (read_in = read(lfd, &length, 1)) <= 0 ){
		if( !read_in )  /* client closed fd */
			perror("0 bytes read");
		else	perror("Error in reading the local connection msg length");
		return(-1);
		}


	if( (read_in = read(lfd, buf, length)) <= 0 ){
		if( !read_in )  /* client closed fd */
			perror("0 bytes read");
		else	perror("Error in reading the local connection slave name");
		return(-1);
		}

	buf[ length ] = '\0';

	if( (fd = open(buf,O_RDWR|O_SYNC)) < 0 ){
		strcat(buf," open fail, clientfd");
		perror(buf);
		return(-1);
		}

	write(fd,SUCCESS,1);
	InitClientBuffers(fd);	

	return(fd);
}

CreateWellKnownSockets()
{
    CLEARBITS(AllSockets);
    CLEARBITS(AllClients);
    CLEARBITS(LastSelectMask);

    lastfdesc = ulimit(4, (long)0) - 1;  /* Returns total # of FDs available */
    if (lastfdesc > MAXSOCKS)
    {
	lastfdesc = MAXSOCKS;
    }

    WellKnownConnections = 0;
    if ((ListenFd = OpenTheListener()) < 0)
    {
       	if(ListenFd == -1)
        	return(-1);
    }
    else	WellKnownConnections |= (1<<ListenFd);

    if (WellKnownConnections == 0) {
        fprintf(stderr, "No Listeners, nothing to do\n");
        return(-1);
    }
    
    AllSockets[0] = WellKnownConnections;
    return(1);
}

EstablishNewConnections()
{
    long readyconnections;     /* mask of listeners that are ready */
    long newconn;                  /* fd of new client */
    char *reason;
    char MoreConnections;

    if (readyconnections = (LastSelectMask[0] & WellKnownConnections)) 
    {
      MoreConnections = 0;
      do
      {
	newconn = ConnectNewClients(ListenFd,&MoreConnections);
	if (newconn >= 0)
	{
		fcntl (newconn, F_SETFL, O_NDELAY);
		BITSET(AllClients, newconn);
		BITSET(AllSockets, newconn);
	}
      }
      while(MoreConnections);
    }
}

void
CloseDownFileDescriptor(connection)
    long connection;
{
    close(connection);
    BITCLEAR(AllSockets, connection);
    BITCLEAR(AllClients, connection);
}


CheckConnections()
{
    long		rmask[mskcnt]; 
    long		emask[mskcnt]; 
    register int	curclient;
    int			i;

    COPYBITS(AllClients, rmask);
    COPYBITS(AllClients, emask);
    i = select (MAXSOCKS, rmask, (int *) NULL, emask, NULL);
    if (i <= 0)
	return;

    for (i=0; i<mskcnt; i++)
    {
        while (emask[i])
    	{
	    curclient = NextSetBit (emask[i]) - 1 + (i << 5);
#ifdef DEBUG
	    printf("CheckConnection closing %d\n", curclient);
#endif
	    InitClientBuffers(curclient);
            CloseDownFileDescriptor(curclient);
	    BITCLEAR(emask, curclient);
	}
    }	
}

/* Find the first set bit
 * i.e. least signifigant 1 bit:
 * 0 => 0
 * 1 => 1
 * 2 => 2
 * 3 => 1
 * 4 => 3
 */
int NextSetBit(mask)
unsigned int	mask;
{
	register i;

	if ( ! mask ) return 0;
	i = 1;
	while (! (mask & 1)) {
		i++;
		mask >>= 1;
	}
	return i;
}

initBuffers()
{
	int	i;

	for(i=0; i< MAXSOCKS; i++)
	{
		InputBuffer[i].flags	= 0;
		InputBuffer[i].buflen	= 0;
		InputBuffer[i].bufptr	= 0;
		InputBuffer[i].inputbuf	= NULL;
	}
}

char	path[128];

SendNull(fd)
int	fd;
{

	char	*ptr, buf[32];

	ptr = buf;
	*(int *) ptr = 0;
	ptr += sizeof(int);
	*(int *) ptr = 0;
	write(fd, inbuf, 2*sizeof(int));
}

ServiceClient(fd)
int	fd;
{
	register IOBUFFER *iop = &InputBuffer[fd];
	int	n, m;
	char	*ptr, *net;
	int	fds[2];
	int	pid;

	if(iop->inputbuf ==  NULL)
	{
		iop->inputbuf = (char *) xalloc(BUFSIZE);
		iop->buflen	= BUFSIZE;
	}
	if(iop->bufptr < HEADERSIZE)
	{
	   errno = 0;
	   n = read(fd, &iop->inputbuf[iop->bufptr], iop->buflen - iop->bufptr);
	   if(n <= 0)
	   {
		if(errno == EAGAIN)
			return(0);
		else if(errno == EINTR){
			InitClientBuffers(fd);
			SendNull(fd);
		}
		else {
#ifdef DEBUG
                      	fprintf(stderr, "XDEAMON: read error; errno %d\n",
                                                 errno);
#endif
			CloseDownFileDescriptor(fd);
		}
		return(-1);
	   }
	   iop->bufptr += n;
	   if(iop->bufptr < HEADERSIZE)
	   		return(0);
	   iop->msglen = *(int *) iop->inputbuf;
	}

	if(iop->buflen < iop->msglen)
	{
	   iop->inputbuf = (char *) xrealloc(iop->inputbuf, iop->msglen);
	   iop->buflen	= iop->msglen;
	}

	if(iop->bufptr < iop->msglen)
	{
           errno = 0;
	   n = read(fd, &iop->inputbuf[iop->bufptr], iop->msglen - iop->bufptr);
	   if(n <= 0)
	   {
                if(errno == EAGAIN)
                        return(0);
		else if(errno == EINTR){
                        InitClientBuffers(fd);
                        SendNull(fd);
		}
                else {
#ifdef DEBUG
                      	fprintf(stderr, "XDEAMON: read error; errno %d\n",
                                                 errno);
#endif
                        CloseDownFileDescriptor(fd);
		}
		return(-1);
           }
           iop->bufptr += n;
	   if(iop->bufptr < iop->msglen)
                        return(0);
	}

	ptr = &iop->inputbuf[4*sizeof(int)];
	n = *(int *) ptr;
	ptr += sizeof(int);
	net  = ptr;
	if(ptr[n] != '\0')
	{
		fprintf(stderr, "XDEAMON: net name error\n");
                SendNull(fd);
	}	
	else if(pipe(fds) < 0){
		fprintf(stderr, "XDEAMON: pipe failed error\n");
                SendNull(fd);
		}
	else if((pid = fork()) == 0)
	{
		close(0); dup(fds[0]);
		close(1); dup(fd);
		close(fds[0]);
		close(fds[1]);
		sprintf(path, "/usr/X/lib/net/%s/nameserver", net);
		
		execl(path, "nameserver", 0);
		fprintf(stderr, "Cannot exec %s\n", path);
		exit(1);
	}
	else  {
		close(fds[0]);
		if(pid < 0)
		{
			fprintf(stderr, "XDEAMON: fork failed error\n");
                	SendNull(fd);
			close(fds[1]);
		}
		else { 
#ifdef DEBUG
	 		write(2, iop->inputbuf, iop->msglen);
#endif
		 	if(write(fds[1], iop->inputbuf, iop->msglen) != iop->msglen){
				SendNull(fd);
				fprintf(stderr,"XDEAMON: write failed error\n");
				}
			close(fds[1]);
		}
	}
	InitClientBuffers(fd);
	return(1);
}


InitClientBuffers(fd)
int	fd;
{
       	InputBuffer[fd].flags	= 0;
	InputBuffer[fd].bufptr	= 0;
	InputBuffer[fd].msglen	= 0;
}

char *
xalloc(n)
int	n;
{
	char	*ptr;

	if((ptr = (char *) malloc(n)) == NULL)
	{
		fprintf(stderr, "malloc failed\n");
		exit(1);
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
		exit(1);
	}
        return(ptr);
}

char	*program;
main(argc, argv)
int	argc;
char	*argv[];
{
	int	i;
	program = argv[0];
	
	i = CreateWellKnownSockets();
/*
	if(argc == 2 && strcmp(argv[1], "-") == 0){
		sig = SIGALRM;
		}
	if(sig > 0)
		kill(getppid(), sig);
*/

	if(i < 0)
		exit(1);

	signal(SIGCLD, SIG_IGN);

	for(;;)
		WaitForInput();
}

