/* $Header: /home/x_cvs/mit/server/os/connection.c,v 1.12 1992/08/25 07:37:32 dawes Exp $ */
/***********************************************************
Copyright 1987, 1989 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/* $XConsortium: connection.c,v 1.146 92/06/11 10:38:45 rws Exp $ */
/*****************************************************************
 *  Stuff to create connections --- OS dependent
 *
 *      EstablishNewConnections, CreateWellKnownSockets, ResetWellKnownSockets,
 *      CloseDownConnection, CheckConnections, AddEnabledDevice,
 *	RemoveEnabledDevice, OnlyListToOneClient,
 *      ListenToAllClients,
 *
 *      (WaitForSomething is in its own file)
 *
 *      In this implementation, a client socket table is not kept.
 *      Instead, what would be the index into the table is just the
 *      file descriptor of the socket.  This won't work for if the
 *      socket ids aren't small nums (0 - 2^8)
 *
 *****************************************************************/

#include "X.h"
#include "Xproto.h"
#include <sys/param.h>
#include <errno.h>
#include "Xos.h"			/* for strings, file, time */
#ifdef ESIX
#include <lan/socket.h>
#else
#include <sys/socket.h>
#endif

#include <signal.h>
#include <setjmp.h>

#ifdef hpux
#include <sys/utsname.h>
#include <sys/ioctl.h>
#endif

#ifdef SVR4
#include <sys/resource.h>
#endif

#ifdef AIXV3
#include <sys/ioctl.h>
#endif

#ifdef TCPCONN
# include <netinet/in.h>
# ifndef hpux
#  ifdef apollo
#   ifndef NO_TCP_H
#    include <netinet/tcp.h>
#   endif
#  else
#   include <netinet/tcp.h>
#  endif
# endif
#endif

#ifdef UNIXCONN
/*
 * sites should be careful to have separate /tmp directories for diskless nodes
 */
#include <sys/un.h>
#include <sys/stat.h>
static int unixDomainConnection = -1;
#endif

#include <stdio.h>
#include <sys/uio.h>
#include "osstruct.h"
#include "osdep.h"
#include "opaque.h"
#include "dixstruct.h"

#ifdef DNETCONN
#include <netdnet/dn.h>
#endif /* DNETCONN */

#ifdef SIGNALRETURNSINT
#define SIGVAL int
#else
#define SIGVAL void
#endif

typedef long CCID;      /* mask of indices into client socket table */

#ifndef X_UNIX_PATH
#ifdef hpux
#define X_UNIX_DIR	"/usr/spool/sockets/X11"
#define X_UNIX_PATH	"/usr/spool/sockets/X11/"
#define OLD_UNIX_DIR	"/tmp/.X11-unix"
#else
#define X_UNIX_DIR	"/tmp/.X11-unix"
#define X_UNIX_PATH	"/tmp/.X11-unix/X"
#endif
#endif

#ifdef SERVER_LOCALCONN
#include <sys/stream.h>
#include <sys/stropts.h>
#include <sys/utsname.h>
#ifndef UNIXCONN
#include <sys/stat.h>
#endif
static int ptsFd = -1;
static int spxFd = -1;
static int xsFd = -1;
static long AllStreams[mskcnt]; /* keep up, whos on a STREAMS pipe */
/*
 * Why not use the same path as for UNIXCONN ?? 
 * Diskless workstations may have a common /tmp directory. This may cause much
 * trouble. Since every workstation MUST have it's own /dev, so lets use this
 * directory.
 */
#define X_STREAMS_DIR "/dev/X"
#define X_STREAMS_PATH "/dev/X/server."
#define X_XSIGHT_PATH "/dev/X"
#endif

extern char *display;		/* The display number */
int lastfdesc;			/* maximum file descriptor */

long WellKnownConnections;	/* Listener mask */
long EnabledDevices[mskcnt];	/* mask for input devices that are on */
long AllSockets[mskcnt];	/* select on this */
long AllClients[mskcnt];	/* available clients */
long LastSelectMask[mskcnt];	/* mask returned from last select call */
long ClientsWithInput[mskcnt];	/* clients with FULL requests in buffer */
long ClientsWriteBlocked[mskcnt];/* clients who cannot receive output */
long OutputPending[mskcnt];	/* clients with reply/event data ready to go */
long MaxClients = MAXSOCKS ;
long NConnBitArrays = mskcnt;
Bool NewOutputPending;		/* not yet attempted to write some new output */
Bool AnyClientsWriteBlocked;	/* true if some client blocked on write */

Bool RunFromSmartParent;	/* send SIGUSR1 to parent process */
Bool PartialNetwork;		/* continue even if unable to bind all addrs */
static int ParentProcess;

static Bool debug_conns = FALSE;

static int SavedAllClients[mskcnt];
static int SavedAllSockets[mskcnt];
static int SavedClientsWithInput[mskcnt];
int GrabInProgress = 0;

int ConnectionTranslation[MAXSOCKS];
extern ClientPtr NextAvailableClient();

extern SIGVAL AutoResetServer();
extern SIGVAL GiveUp();
extern XID CheckAuthorization();
static void CloseDownFileDescriptor(), ErrorConnMax();
extern void FreeOsBuffers(), ResetOsBuffers();

#ifdef XDMCP
void XdmcpOpenDisplay(), XdmcpInit(), XdmcpReset(), XdmcpCloseDisplay();
#endif

#ifdef TCPCONN
static int
open_tcp_socket ()
{
    struct sockaddr_in insock;
    int request;
    int retry;
#ifdef SVR4
#undef SO_DONTLINGER
#endif
#ifndef SO_DONTLINGER
#ifdef SO_LINGER
    static int linger[2] = { 0, 0 };
#endif /* SO_LINGER */
#endif /* SO_DONTLINGER */

#ifdef AIXV3
#ifndef FORCE_DISPLAY_NUM
    extern int AIXTCPSocket;
    if (AIXTCPSocket>=0) {
        request= AIXTCPSocket;
    } else
#endif /* FORCE_DISPLAY_NUM */
#endif /* AIX && etc. */
    if ((request = socket (AF_INET, SOCK_STREAM, 0)) < 0) 
    {
	Error ("Creating TCP socket");
	return -1;
    } 
#ifdef SO_REUSEADDR
    /* Necesary to restart the server without a reboot */
    {
	int one = 1;
	setsockopt(request, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(int));
    }
#endif /* SO_REUSEADDR */
#ifdef AIXV3
#ifndef FORCE_DISPLAY_NUMBER
    if (AIXTCPSocket<0)
#endif
#endif
    {
    bzero ((char *)&insock, sizeof (insock));
    insock.sin_family = AF_INET;
    insock.sin_port = htons ((unsigned short)(X_TCP_PORT + atoi (display)));
    insock.sin_addr.s_addr = htonl(INADDR_ANY);
    retry = 20;
    while (bind(request, (struct sockaddr *) &insock, sizeof (insock)))
    {
	if (--retry == 0) {
	    Error ("Binding TCP socket");
	    close (request);
	    return -1;
	}
#ifdef SO_REUSEADDR
	sleep (1);
#else
	sleep (10);
#endif /* SO_REUSEDADDR */
    }
    }
#ifdef SO_DONTLINGER
    if(setsockopt (request, SOL_SOCKET, SO_DONTLINGER, (char *)NULL, 0))
	Error ("Setting TCP SO_DONTLINGER");
#else
#ifdef SO_LINGER
    if(setsockopt (request, SOL_SOCKET, SO_LINGER,
		   (char *)linger, sizeof(linger)))
	Error ("Setting TCP SO_LINGER");
#endif /* SO_LINGER */
#endif /* SO_DONTLINGER */
    if (listen (request, 5)) {
	Error ("TCP Listening");
	close (request);
	return -1;
    }
    return request;
}
#endif /* TCPCONN */

#ifdef UNIXCONN

static struct sockaddr_un unsock;

static int
open_unix_socket ()
{
    int oldUmask;
    int request;

    bzero ((char *) &unsock, sizeof (unsock));
    unsock.sun_family = AF_UNIX;
    oldUmask = umask (0);
#ifdef X_UNIX_DIR
    if (!mkdir (X_UNIX_DIR, 0777))
	chmod (X_UNIX_DIR, 0777);
#endif
    strcpy (unsock.sun_path, X_UNIX_PATH);
    strcat (unsock.sun_path, display);
#ifdef hpux
    {  
        /*    The following is for backwards compatibility
         *    with old HP clients. This old scheme predates the use
 	 *    of the /usr/spool/sockets directory, and uses hostname:display
 	 *    in the /tmp/.X11-unix directory
         */
        struct utsname systemName;
	static char oldLinkName[256];

        uname(&systemName);
        strcpy(oldLinkName, OLD_UNIX_DIR);
        if (!mkdir(oldLinkName, 0777))
	    chown(oldLinkName, 2, 3);
        strcat(oldLinkName, "/");
        strcat(oldLinkName, systemName.nodename);
        strcat(oldLinkName, display);
        unlink(oldLinkName);
        symlink(unsock.sun_path, oldLinkName);
    }
#endif	/* hpux */
    unlink (unsock.sun_path);
    if ((request = socket (AF_UNIX, SOCK_STREAM, 0)) < 0) 
    {
	Error ("Creating Unix socket");
	return -1;
    } 
    if (bind(request, (struct sockaddr *)&unsock, strlen(unsock.sun_path)+2))
    {
	Error ("Binding Unix socket");
	close (request);
	return -1;
    }
    if (listen (request, 5))
    {
	Error ("Unix Listening");
	close (request);
	return -1;
    }
    (void)umask(oldUmask);
    return request;
}
#endif /*UNIXCONN */

#ifdef SERVER_LOCALCONN

#if !defined(SVR4) || defined(SVR4_ACP)
static int
connect_spipe(fd1, fd2)
     int fd1, fd2;
{
  long temp;
  struct strfdinsert sbuf;

  sbuf.databuf.maxlen = -1;
  sbuf.databuf.len = -1;
  sbuf.databuf.buf = NULL;
  sbuf.ctlbuf.maxlen = sizeof(long);
  sbuf.ctlbuf.len = sizeof(long);
  sbuf.ctlbuf.buf = (caddr_t)&temp;
  sbuf.offset = 0;
  sbuf.fildes = fd2;
  sbuf.flags = 0;
  if (ioctl(fd1, I_FDINSERT, &sbuf) == -1) return (-1);
  
  return (0);
}

static int
named_spipe(fd, path)
     int fd;
     char *path;
{
  int oldUmask, ret;
  struct stat sbuf;

  oldUmask = umask(0);

  (void) fstat(fd, &sbuf);
  ret = mknod(path, 0020666, sbuf.st_rdev);

  umask(oldUmask);

  return (ret < 0 ? -1 : fd);
}

#ifndef SVR4
static int
open_isc_local ()
{
  int fd = -1,fds = -1;
  long temp;
  struct strfdinsert buf;
  char path[64];

  mkdir (X_UNIX_DIR, 0777);
  chmod (X_UNIX_DIR, 0777);

  strcpy (path, X_UNIX_PATH);
  strcat (path, display);
  
  if(unlink(path) < 0 && errno != ENOENT) {
    ErrorF ("ISC: Server is already running (%s)\n", path);
    return(-1);
  }

  if ((fds = open("/dev/spx", O_RDWR)) >= 0 &&
      (fd  = open("/dev/spx", O_RDWR)) >= 0 )

    if (connect_spipe(fds, fd) != -1 &&
	named_spipe(fds, path) != -1)

      return(fd);
    else
      Error("ISC: Can't set up listener pipes");

  else
    Error("ISC: Cannot open \"/dev/spx\"");

  (void) close(fds);
  (void) close(fd);
  return(-1);
}


static int
accept_isc_local()
{
  struct strrecvfd buf;

  while (ioctl(spxFd, I_RECVFD, &buf) < 0)
    if (errno != EAGAIN) {
      Error("ISC: Can't read fildes");
      return(-1);
    }

  BITSET(AllStreams, buf.fd);
  return(buf.fd);
}
#endif

static int
open_xsight_local()
{
  int fds = -1,fdr = -1;
  char pathS[64], pathR[64];

  sprintf(pathS, "%s%sS",X_XSIGHT_PATH, display);
  sprintf(pathR, "%s%sR",X_XSIGHT_PATH, display);
  
  if((unlink(pathS) < 0 && errno != ENOENT) ||
     (unlink(pathR) < 0 && errno != ENOENT))
    {
      ErrorF ("SCO: Server is already running (%s)\n",pathR);
      return(-1);
    }
  
  if ((fds = open("/dev/spx", O_RDWR)) >= 0 &&
      (fdr = open("/dev/spx", O_RDWR)) >= 0 )

    if (connect_spipe(fds, fdr) != -1 &&
	named_spipe(fds, pathS) != -1 &&
	named_spipe(fdr, pathR) != -1)
      
      return(fds);
    else
      Error("SCO: Can't set up listener pipes");
  
#ifndef SVR4
  /*
   * At this point, most SVR4 versions will fail on this, so leave out the
   * warning
   */
  else
    Error("SCO: Cannot open \"/dev/spx\"");
#endif

  (void) close(fds);
  (void) close(fdr);
  return(-1);
}


static int
accept_xsight_local()
{
  char c;
  int fd;
  long temp;
  struct strfdinsert buf;

  if (read(xsFd, &c, 1) < 0) {
    Error("SCO: Can't read from client");
    return(-1);
  }

  if ((fd = open("/dev/spx", O_RDWR)) < 0) {
    Error("SCO: Can't open \"/dev/spx\"");
    return(-1);
  }

  if (connect_spipe(xsFd, fd) < 0) {
    Error("SCO: Can't connect pipes");
    (void) close(fd);
    return(-1);
  }

  BITSET(AllStreams, fd);
  return(fd);
}
#endif /* SVR4 */

static int
open_att_local ()
{
  char *slave;
  int fd;
  char path[64];

  mkdir (X_STREAMS_DIR, 0777);
  chmod (X_STREAMS_DIR, 0777);
  
  strcpy (path, X_STREAMS_PATH);
  strcat (path, display);
  
  if(open(path, O_RDWR) >= 0 || (unlink(path) < 0 && errno != ENOENT)) {
    ErrorF ("AT&T: Server is already running (%s)\n", path);
    return(-1);
  }
  
  if( (fd = open("/dev/ptmx", O_RDWR)) < 0 ) {
    Error ("AT&T: Cannot open \"/dev/ptmx\"");
    return(-1);
  }
  
  grantpt(fd);
  unlockpt(fd);
  slave = (char *) ptsname(fd);
  if (link(slave, path) < 0 || chmod(path, 0666) < 0) {
    Error("AT&T: Can't set up local listener");
    return(-1);
  }

  if (open(path, O_RDWR) < 0) {
    ErrorF("AT&T: Can't open %s\n", path);
    close(fd);
    return(-1);
  }

  return(fd);
}

static int
accept_att_local()
{
  int newconn;
  int read_in;
  char length;
  char path[64];

  /*
   * first get device-name
   */
  if( (read_in = read(ptsFd, &length, 1)) <= 0 ) {
    Error("AT&T: Can't read slave name length");
    return(-1);
  }

  if( (read_in = read(ptsFd, path, length)) <= 0 ) {
    Error("AT&T: Can't read slave name");
    return(-1);
  }

  path[ length ] = '\0';
      
  if( (newconn = open(path,O_RDWR)) < 0 ) {
    Error("AT&T: Can't open slave");
    return(-1);
  }

  (void) write(newconn, "1", 1); /* send an acknowledge to the client */

  BITSET(AllStreams, newconn);
  return(newconn);
}


#endif /* SERVER_LOCALCONN */

#ifdef SYSV386
int
sysv386_getpeername(fd, from, fromlen)
     int fd;
     struct sockaddr *from;
     int *fromlen;
{
#ifdef SERVER_LOCALCONN
  /*
   * check up whether our fd is really a streams pipe ( /dev/pts??? )
   */
  if (GETBIT(AllStreams, fd))
    {
      from->sa_family = AF_UNSPEC;
      *fromlen = 0;
      return 0;
    }
#endif
#if defined(TCPCONN) || defined(DNETCONN) || defined(UNIXCONN)
  return getpeername(fd, from, fromlen);
#endif
}

int
sysv386_accept (fd, from, fromlen)
int fd;
struct sockaddr *from;
int *fromlen;
{
#ifdef SERVER_LOCALCONN
  if (fd == ptsFd) return accept_att_local();
#ifndef SVR4
  if (fd == spxFd) return accept_isc_local();
#endif
#if !defined(SVR4) || defined(SVR4_ACP)
  if (fd == xsFd)  return accept_xsight_local(); 
#endif /* SVR4 */
#endif
/*
 * else we are handling the normal accept case
 */
#if defined(TCPCONN) || defined(DNETCONN) || defined(UNIXCONN)
  return accept (fd, from, fromlen);
#endif
}

#define	getpeername	sysv386_getpeername
#define	accept		sysv386_accept

#endif /* SYSV386 */

#ifdef hpux
/*
 * hpux returns EOPNOTSUPP when using getpeername on a unix-domain
 * socket.  In this case, smash the socket address with the address
 * used to bind the connection socket and return success.
 */
hpux_getpeername(fd, from, fromlen)
    int	fd;
    struct sockaddr *from;
    int		    *fromlen;
{
    int	    ret;
    int	    len;

    ret = getpeername(fd, from, fromlen);
    if (ret == -1 && errno == EOPNOTSUPP)
    {
	ret = 0;
	len = strlen(unsock.sun_path)+2;
	if (len > *fromlen)
	    len = *fromlen;
	bcopy ((char *) &unsock, (char *) from, len);
	*fromlen = len;
    }
    return ret;
}

#define getpeername(fd, from, fromlen)	hpux_getpeername(fd, from, fromlen)

#endif

#ifdef DNETCONN
static int
open_dnet_socket ()
{
    int request;
    struct sockaddr_dn dnsock;

    if ((request = socket (AF_DECnet, SOCK_STREAM, 0)) < 0) 
    {
	Error ("Creating DECnet socket");
	return -1;
    } 
    bzero ((char *)&dnsock, sizeof (dnsock));
    dnsock.sdn_family = AF_DECnet;
    sprintf(dnsock.sdn_objname, "X$X%d", atoi (display));
    dnsock.sdn_objnamel = strlen(dnsock.sdn_objname);
    if (bind (request, (struct sockaddr *) &dnsock, sizeof (dnsock)))
    {
	Error ("Binding DECnet socket");
	close (request);
	return -1;
    }
    if (listen (request, 5))
    {
	Error ("DECnet Listening");
	close (request);
	return -1;
    }
    return request;
}
#endif /* DNETCONN */

/*****************
 * CreateWellKnownSockets
 *    At initialization, create the sockets to listen on for new clients.
 *****************/

void
CreateWellKnownSockets()
{
    int		request, i;
#ifdef SVR4
    struct rlimit Rlimit;
#endif

    CLEARBITS(AllSockets);
    CLEARBITS(AllClients);
    CLEARBITS(LastSelectMask);
    CLEARBITS(ClientsWithInput);

    for (i=0; i<MAXSOCKS; i++) ConnectionTranslation[i] = 0;
    
#if defined(hpux)
    lastfdesc = _NFILE - 1;
#else   /* hpux */
#ifdef SVR4
    if (getrlimit(RLIMIT_NOFILE, &Rlimit) != 0)
    {
	lastfdesc = _NFILE - 1;
    }
    else
    {
	lastfdesc = Rlimit.rlim_cur;
    }
#else   /* SVR4 */
    lastfdesc = getdtablesize() - 1;
#endif  /* SVR4 */
#endif	/* hpux */

    if (lastfdesc > MAXSOCKS)
    {
	lastfdesc = MAXSOCKS;
	if (debug_conns)
	    ErrorF( "GOT TO END OF SOCKETS %d\n", MAXSOCKS);
    }

    WellKnownConnections = 0;
#ifdef SERVER_LOCALCONN
    CLEARBITS(AllStreams);
    if ((ptsFd = open_att_local ()) != -1) {
	WellKnownConnections |= (1L << ptsFd);
    }
#ifndef SVR4
    if ((spxFd = open_isc_local ()) != -1) {
	WellKnownConnections |= (1L << spxFd);
    }
#endif
#if !defined(SVR4) || defined(SVR4_ACP)
    if ((xsFd = open_xsight_local ()) != -1) {
	WellKnownConnections |= (1L << xsFd);
    } 
#endif
#endif /* SERVER_LOCALCONN */
#ifdef TCPCONN
    if ((request = open_tcp_socket ()) != -1) {
	WellKnownConnections |= (1L << request);
	DefineSelf (request);
    }
    else if (!PartialNetwork) 
    {
	FatalError ("Cannot establish tcp listening socket");
    }
#endif /* TCPCONN */
#ifdef DNETCONN
    if ((request = open_dnet_socket ()) != -1) {
	WellKnownConnections |= (1L << request);
	DefineSelf (request);
    }
    else if (!PartialNetwork) 
    {
	FatalError ("Cannot establish dnet listening socket");
    }
#endif /* DNETCONN */
#ifdef UNIXCONN
    if ((request = open_unix_socket ()) != -1) {
	WellKnownConnections |= (1L << request);
	unixDomainConnection = request;
    }
    else if (!PartialNetwork) 
    {
	FatalError ("Cannot establish unix listening socket");
    }
#endif /* UNIXCONN */
    if (WellKnownConnections == 0)
        FatalError ("Cannot establish any listening sockets");
    signal (SIGPIPE, SIG_IGN);
    signal (SIGHUP, AutoResetServer);
    signal (SIGINT, GiveUp);
    signal (SIGTERM, GiveUp);
    AllSockets[0] = WellKnownConnections;
    ResetHosts(display);
    /*
     * Magic:  If SIGUSR1 was set to SIG_IGN when
     * the server started, assume that either
     *
     *  a- The parent process is ignoring SIGUSR1
     *
     * or
     *
     *  b- The parent process is expecting a SIGUSR1
     *     when the server is ready to accept connections
     *
     * In the first case, the signal will be harmless,
     * in the second case, the signal will be quite
     * useful
     */
    if (signal (SIGUSR1, SIG_IGN) == SIG_IGN)
	RunFromSmartParent = TRUE;
    ParentProcess = getppid ();
    if (RunFromSmartParent) {
	if (ParentProcess > 0) {
	    kill (ParentProcess, SIGUSR1);
	}
    }
#ifdef XDMCP
    XdmcpInit ();
#endif
}

void
ResetWellKnownSockets ()
{
    ResetOsBuffers();
#if defined(UNIXCONN) && !defined(SVR4)
    if (unixDomainConnection != -1)
    {
	/*
	 * see if the unix domain socket has disappeared
	 */
	struct stat	statb;

	if (stat (unsock.sun_path, &statb) == -1 ||
	    (statb.st_mode & S_IFMT) != S_IFSOCK)
	{
	    ErrorF ("Unix domain socket %s trashed, recreating\n",
	    	unsock.sun_path);
	    (void) unlink (unsock.sun_path);
	    (void) close (unixDomainConnection);
	    WellKnownConnections &= ~(1L << unixDomainConnection);
	    unixDomainConnection = open_unix_socket ();
	    if (unixDomainConnection != -1)
		WellKnownConnections |= (1L << unixDomainConnection);
	}
    }
#endif /* UNIXCONN */
#ifdef SERVER_LOCALCONN
    CLEARBITS(AllStreams);
#endif /* SERVER_LOCALCONN */
    ResetAuthorization ();
    ResetHosts(display);
    /*
     * See above in CreateWellKnownSockets about SIGUSR1
     */
    if (RunFromSmartParent) {
	if (ParentProcess > 0) {
	    kill (ParentProcess, SIGUSR1);
	}
    }
    /*
     * restart XDMCP
     */
#ifdef XDMCP
    XdmcpReset ();
#endif
}

/*****************************************************************
 * ClientAuthorized
 *
 *    Sent by the client at connection setup:
 *                typedef struct _xConnClientPrefix {
 *                   CARD8	byteOrder;
 *                   BYTE	pad;
 *                   CARD16	majorVersion, minorVersion;
 *                   CARD16	nbytesAuthProto;    
 *                   CARD16	nbytesAuthString;   
 *                 } xConnClientPrefix;
 *
 *     	It is hoped that eventually one protocol will be agreed upon.  In the
 *        mean time, a server that implements a different protocol than the
 *        client expects, or a server that only implements the host-based
 *        mechanism, will simply ignore this information.
 *
 *****************************************************************/

char * 
ClientAuthorized(client, proto_n, auth_proto, string_n, auth_string)
    ClientPtr client;
    char *auth_proto, *auth_string;
    unsigned short proto_n, string_n;
{
    register OsCommPtr priv;
    union {
	struct sockaddr sa;
#ifdef UNIXCONN
	struct sockaddr_un un;
#endif /* UNIXCONN */
#ifdef TCPCONN
	struct sockaddr_in in;
#endif /* TCPCONN */
#ifdef DNETCONN
	struct sockaddr_dn dn;
#endif /* DNETCONN */
    } from;
    int	fromlen = sizeof (from);
    XID	 auth_id;

    auth_id = CheckAuthorization (proto_n, auth_proto,
				  string_n, auth_string);

    priv = (OsCommPtr)client->osPrivate;
    if (auth_id == (XID) ~0L && 
   	getpeername (priv->fd, &from.sa, &fromlen) != -1 &&
        !InvalidHost (&from.sa, fromlen))
    {
	auth_id = (XID) 0;
    }

    if (auth_id == (XID) ~0L)
	return "Client is not authorized to connect to Server";

    priv->auth_id = auth_id;
    priv->conn_time = 0;

#ifdef XDMCP
    /* indicate to Xdmcp protocol that we've opened new client */
    XdmcpOpenDisplay(priv->fd);
#endif /* XDMCP */
    /* At this point, if the client is authorized to change the access control
     * list, we should getpeername() information, and add the client to
     * the selfhosts list.  It's not really the host machine, but the
     * true purpose of the selfhosts list is to see who may change the
     * access control list.
     */
    return((char *)NULL);
}

/*****************
 * EstablishNewConnections
 *    If anyone is waiting on listened sockets, accept them.
 *    Returns a mask with indices of new clients.  Updates AllClients
 *    and AllSockets.
 *****************/

/*ARGSUSED*/
Bool
EstablishNewConnections(clientUnused, closure)
    ClientPtr clientUnused;
    pointer closure;
{
    long readyconnections;     /* mask of listeners that are ready */
    int curconn;                  /* fd of listener that's ready */
    register int newconn;         /* fd of new client */
    long connect_time;
    register int i;
    register ClientPtr client;
    register OsCommPtr oc;

#ifdef TCP_NODELAY
    union {
	struct sockaddr sa;
#ifdef UNIXCONN
	struct sockaddr_un un;
#endif /* UNIXCONN */
#ifdef TCPCONN
	struct sockaddr_in in;
#endif /* TCPCONN */
#ifdef DNETCONN
	struct sockaddr_dn dn;
#endif /* DNETCONN */
    } from;
    int	fromlen;
#endif /* TCP_NODELAY */

    readyconnections = (((long)closure) & WellKnownConnections);
    if (!readyconnections)
	return TRUE;
    connect_time = GetTimeInMillis();
    /* kill off stragglers */
    for (i=1; i<currentMaxClients; i++)
    {
	if (client = clients[i])
	{
	    oc = (OsCommPtr)(client->osPrivate);
	    if (oc && (oc->conn_time != 0) &&
		(connect_time - oc->conn_time) >= TimeOutValue)
		CloseDownClient(client);     
	}
    }
    while (readyconnections) 
    {
	curconn = ffs (readyconnections) - 1;
	readyconnections &= ~(1 << curconn);
	if ((newconn = accept (curconn,
			      (struct sockaddr *) NULL, 
			      (int *)NULL)) < 0) 
	    continue;
#ifdef TCP_NODELAY
	fromlen = sizeof (from);
	if (!getpeername (newconn, &from.sa, &fromlen))
	{
	    if (fromlen && (from.sa.sa_family == AF_INET)) 
	    {
		int mi = 1;
		setsockopt (newconn, IPPROTO_TCP, TCP_NODELAY,
			   (char *)&mi, sizeof (int));
	    }
	}
#endif /* TCP_NODELAY */
    /* ultrix reads hang on Unix sockets, hpux reads fail, AIX fails too */
#if defined(O_NONBLOCK) && (!defined(ultrix) && !defined(hpux) && !defined(AIXV3))
	(void) fcntl (newconn, F_SETFL, O_NONBLOCK);
#else
#ifdef FIOSNBIO
	{
	    int	arg;
	    arg = 1;
	    ioctl(newconn, FIOSNBIO, &arg);
	}
#else
#if defined(AIXV3) && defined(FIONBIO)
	{
	    int arg;
	    arg = 1;
	    ioctl(newconn, FIONBIO, &arg);
	}
#else
	fcntl (newconn, F_SETFL, FNDELAY);
#endif
#endif
#endif
	oc = (OsCommPtr)xalloc(sizeof(OsCommRec));
	if (!oc)
	{
	    ErrorConnMax(newconn);
	    close(newconn);
	    continue;
	}
	if (GrabInProgress)
	{
	    BITSET(SavedAllClients, newconn);
	    BITSET(SavedAllSockets, newconn);
	}
	else
	{
	    BITSET(AllClients, newconn);
	    BITSET(AllSockets, newconn);
	}
	oc->fd = newconn;
	oc->input = (ConnectionInputPtr)NULL;
	oc->output = (ConnectionOutputPtr)NULL;
	oc->conn_time = connect_time;
	if ((newconn < lastfdesc) &&
	    (client = NextAvailableClient((pointer)oc)))
	{
	    ConnectionTranslation[newconn] = client->index;
	}
	else
	{
	    ErrorConnMax(newconn);
	    CloseDownFileDescriptor(oc);
	}
    }
    return TRUE;
}

#define NOROOM "Maximum number of clients reached"

/************
 *   ErrorConnMax
 *     Fail a connection due to lack of client or file descriptor space
 ************/

static void
ErrorConnMax(fd)
    register int fd;
{
    xConnSetupPrefix csp;
    char pad[3];
    struct iovec iov[3];
    char byteOrder = 0;
    int whichbyte = 1;
    struct timeval waittime;
    long mask[mskcnt];

    /* if these seems like a lot of trouble to go to, it probably is */
    waittime.tv_sec = BOTIMEOUT / MILLI_PER_SECOND;
    waittime.tv_usec = (BOTIMEOUT % MILLI_PER_SECOND) *
		       (1000000 / MILLI_PER_SECOND);
    CLEARBITS(mask);
    BITSET(mask, fd);
    (void)select(fd + 1, (int *) mask, (int *) NULL, (int *) NULL, &waittime);
    /* try to read the byte-order of the connection */
    (void)read(fd, &byteOrder, 1);
    if ((byteOrder == 'l') || (byteOrder == 'B'))
    {
	csp.success = xFalse;
	csp.lengthReason = sizeof(NOROOM) - 1;
	csp.length = (sizeof(NOROOM) + 2) >> 2;
	csp.majorVersion = X_PROTOCOL;
	csp.minorVersion = X_PROTOCOL_REVISION;
	if (((*(char *) &whichbyte) && (byteOrder == 'B')) ||
	    (!(*(char *) &whichbyte) && (byteOrder == 'l')))
	{
	    swaps(&csp.majorVersion, whichbyte);
	    swaps(&csp.minorVersion, whichbyte);
	    swaps(&csp.length, whichbyte);
	}
	iov[0].iov_len = sz_xConnSetupPrefix;
	iov[0].iov_base = (char *) &csp;
	iov[1].iov_len = csp.lengthReason;
	iov[1].iov_base = NOROOM;
	iov[2].iov_len = (4 - (csp.lengthReason & 3)) & 3;
	iov[2].iov_base = pad;
	(void)writev(fd, iov, 3);
    }
}

/************
 *   CloseDownFileDescriptor:
 *     Remove this file descriptor and it's I/O buffers, etc.
 ************/

static void
CloseDownFileDescriptor(oc)
    register OsCommPtr oc;
{
    int connection = oc->fd;

    close(connection);
    FreeOsBuffers(oc);
    BITCLEAR(AllSockets, connection);
    BITCLEAR(AllClients, connection);
#ifdef SERVER_LOCALCONN
    BITCLEAR(AllStreams, connection);
#endif
    BITCLEAR(ClientsWithInput, connection);
    if (GrabInProgress)
    {
	BITCLEAR(SavedAllSockets, connection);
	BITCLEAR(SavedAllClients, connection);
	BITCLEAR(SavedClientsWithInput, connection);
    }
    BITCLEAR(ClientsWriteBlocked, connection);
    if (!ANYSET(ClientsWriteBlocked))
    	AnyClientsWriteBlocked = FALSE;
    BITCLEAR(OutputPending, connection);
    xfree(oc);
}

/*****************
 * CheckConections
 *    Some connection has died, go find which one and shut it down 
 *    The file descriptor has been closed, but is still in AllClients.
 *    If would truly be wonderful if select() would put the bogus
 *    file descriptors in the exception mask, but nooooo.  So we have
 *    to check each and every socket individually.
 *****************/

void
CheckConnections()
{
    long		mask;
    long		tmask[mskcnt]; 
    register int	curclient, curoff;
    int			i;
    struct timeval	notime;
    int r;

    notime.tv_sec = 0;
    notime.tv_usec = 0;

    for (i=0; i<mskcnt; i++)
    {
	mask = AllClients[i];
        while (mask)
    	{
	    curoff = ffs (mask) - 1;
 	    curclient = curoff + (i << 5);
            CLEARBITS(tmask);
            BITSET(tmask, curclient);
            r = select (curclient + 1, (int *)tmask, (int *)NULL, (int *)NULL, 
			&notime);
            if (r < 0)
		CloseDownClient(clients[ConnectionTranslation[curclient]]);
	    mask &= ~(1 << curoff);
	}
    }	
}


/*****************
 * CloseDownConnection
 *    Delete client from AllClients and free resources 
 *****************/

CloseDownConnection(client)
    ClientPtr client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;

    if (oc->output && oc->output->count)
	FlushClient(client, oc, (char *)NULL, 0);
    ConnectionTranslation[oc->fd] = 0;
#ifdef XDMCP
    XdmcpCloseDisplay(oc->fd);
#endif
    CloseDownFileDescriptor(oc);
    client->osPrivate = (pointer)NULL;
}


AddEnabledDevice(fd)
    int fd;
{
    BITSET(EnabledDevices, fd);
    BITSET(AllSockets, fd);
}


RemoveEnabledDevice(fd)
    int fd;
{
    BITCLEAR(EnabledDevices, fd);
    BITCLEAR(AllSockets, fd);
}

/*****************
 * OnlyListenToOneClient:
 *    Only accept requests from  one client.  Continue to handle new
 *    connections, but don't take any protocol requests from the new
 *    ones.  Note that if GrabInProgress is set, EstablishNewConnections
 *    needs to put new clients into SavedAllSockets and SavedAllClients.
 *    Note also that there is no timeout for this in the protocol.
 *    This routine is "undone" by ListenToAllClients()
 *****************/

OnlyListenToOneClient(client)
    ClientPtr client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    int connection = oc->fd;

    if (! GrabInProgress)
    {
	COPYBITS (ClientsWithInput, SavedClientsWithInput);
        BITCLEAR (SavedClientsWithInput, connection);
	if (GETBIT(ClientsWithInput, connection))
	{
	    CLEARBITS(ClientsWithInput);	    
	    BITSET(ClientsWithInput, connection);
	}
	else
        {
	    CLEARBITS(ClientsWithInput);	    
	}
	COPYBITS(AllSockets, SavedAllSockets);
	COPYBITS(AllClients, SavedAllClients);

	UNSETBITS(AllSockets, AllClients);
	BITSET(AllSockets, connection);
	CLEARBITS(AllClients);
	BITSET(AllClients, connection);
	GrabInProgress = client->index;
    }
}

/****************
 * ListenToAllClients:
 *    Undoes OnlyListentToOneClient()
 ****************/

ListenToAllClients()
{
    if (GrabInProgress)
    {
	ORBITS(AllSockets, AllSockets, SavedAllSockets);
	ORBITS(AllClients, AllClients, SavedAllClients);
	ORBITS(ClientsWithInput, ClientsWithInput, SavedClientsWithInput);
	GrabInProgress = 0;
    }	
}

/****************
 * IgnoreClient
 *    Removes one client from input masks.
 *    Must have cooresponding call to AttendClient.
 ****************/

static long IgnoredClientsWithInput[mskcnt];

IgnoreClient (client)
    ClientPtr	client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    int connection = oc->fd;

    if (!GrabInProgress || GrabInProgress == client->index)
    {
    	if (GETBIT (ClientsWithInput, connection))
	    BITSET(IgnoredClientsWithInput, connection);
    	else
	    BITCLEAR(IgnoredClientsWithInput, connection);
    	BITCLEAR(ClientsWithInput, connection);
    	BITCLEAR(AllSockets, connection);
    	BITCLEAR(AllClients, connection);
	BITCLEAR(LastSelectMask, connection);
    }
    else
    {
    	if (GETBIT (SavedClientsWithInput, connection))
	    BITSET(IgnoredClientsWithInput, connection);
    	else
	    BITCLEAR(IgnoredClientsWithInput, connection);
	BITCLEAR(SavedClientsWithInput, connection);
	BITCLEAR(SavedAllSockets, connection);
	BITCLEAR(SavedAllClients, connection);
    }
    isItTimeToYield = TRUE;
}

/****************
 * AttendClient
 *    Adds one client back into the input masks.
 ****************/

AttendClient (client)
    ClientPtr	client;
{
    OsCommPtr oc = (OsCommPtr)client->osPrivate;
    int connection = oc->fd;

    if (!GrabInProgress || GrabInProgress == client->index)
    {
    	BITSET(AllClients, connection);
    	BITSET(AllSockets, connection);
	BITSET(LastSelectMask, connection);
    	if (GETBIT (IgnoredClientsWithInput, connection))
	    BITSET(ClientsWithInput, connection);
    }
    else
    {
	BITSET(SavedAllClients, connection);
	BITSET(SavedAllSockets, connection);
	if (GETBIT(IgnoredClientsWithInput, connection))
	    BITSET(SavedClientsWithInput, connection);
    }
}

#ifdef AIXV3

static int grabbingClient;
static int reallyGrabbed;

/****************
* DontListenToAnybody:
*   Don't listen to requests from any clients. Continue to handle new
*   connections, but don't take any protocol requests from anybody.
*   We have to take care if there is already a grab in progress, though.
*   Undone by PayAttentionToClientsAgain. We also have to be careful
*   not to accept any more input from the currently dispatched client.
*   we do this be telling dispatch it is time to yield.

*   We call this when the server loses access to the glass
*   (user hot-keys away).  This looks like a grab by the 
*   server itself, but gets a little tricky if there is already
*   a grab in progress.
******************/

void
DontListenToAnybody()
{
    if (!GrabInProgress) {
	COPYBITS(ClientsWithInput, SavedClientsWithInput);
	COPYBITS(AllSockets, SavedAllSockets);
	COPYBITS(AllClients, SavedAllClients);
	GrabInProgress = TRUE;
	reallyGrabbed = FALSE;
    } else {
	grabbingClient = ((OsCommPtr)clients[GrabInProgress]->osPrivate)->fd;
	reallyGrabbed = TRUE;
    }
    CLEARBITS(ClientsWithInput);
    UNSETBITS(AllSockets, AllClients);
    CLEARBITS(AllClients);
    isItTimeToYield = TRUE;
}

void
PayAttentionToClientsAgain()
{
    if (reallyGrabbed) {
	BITSET(AllSockets, grabbingClient);
	BITSET(AllClients, grabbingClient);
    } else {
	ListenToAllClients();
    }
    reallyGrabbed = FALSE;
}

#endif
