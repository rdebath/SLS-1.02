/* Copyright    Massachusetts Institute of Technology    1988	*/
/*
 * THIS IS AN OS DEPENDENT FILE! It should work on 4.2BSD derived
 * systems.  VMS and System V should plan to have their own version.
 *
 * This code was cribbed from lib/X/XConnDis.c.
 * Compile using   
 *                    % cc -c socket.c -DUNIXCONN
 */

#include <stdio.h>
#ifndef COMPILE_ONLY
#include <Xos.h>
#include <Xproto.h>
#else
#include <sys/types.h>
#endif
#include <errno.h>
#include <netinet/in.h>
#include <sys/ioctl.h>
#include <netdb.h> 
#include <sys/socket.h>
#ifndef hpux
#include <netinet/tcp.h>
#endif
#include <sys/types.h>
#include <sys/time.h>

static int connect_to_server();
static void byte();
static void word();
static void lword();
static void nBytes();
static u_char getByte();
static u_short getWord();
static u_long getLword();
static int waitForSocket();

extern int errno;		/* Certain (broken) OS's don't have this */
				/* decl in errno.h */

#ifdef UNIXCONN
#include <sys/un.h>
#ifndef X_UNIX_PATH
#define X_UNIX_PATH "/tmp/.X11-unix/X"
#endif /* X_UNIX_PATH */
#endif /* UNIXCONN */
void bcopy();

void defineSocket()
{
  defineCFunc("connectToServer", connect_to_server);
  defineCFunc("byte", getByte);
  defineCFunc("word", getWord);
  defineCFunc("long", getLword);
  defineCFunc("putByte", byte);
  defineCFunc("putWord", word);
  defineCFunc("putLong", lword);
  defineCFunc("putBytes", nBytes);
  defineCFunc("waitForSocket", waitForSocket);
}

static void byte(s, v)
int s;
u_char v;
{
  if (write(s, &v, 1) < 0) {
    perror("write byte");
  }
}

static void word(s, v)
int s;
u_short v;
{
  if (write(s, &v, 2) < 0) {
    perror("write word");
  }
}

static void lword(s, v)
int s;
u_long v;
{
  if (write(s, &v, 4) < 0) {
    perror("write long");
  }
}

static void nBytes(s, n, byteStr)
int s;
int n;
char *byteStr;
{
  if (write(s, byteStr, n) < 0) {
    perror("write bytes");
  }
}

static u_char getByte(s)
{
  u_char v;
  if (read(s, &v, 1) < 0) {
    perror("read byte");
  }

  return (v);
}

static u_short getWord(s)
{
  u_short v;
  if (read(s, &v, 2) < 0) {
    perror("read short");
  }

  return (v);
}

static u_long getLword(s)
{
  u_long v;
  if (read(s, &v, 4) < 0) {
    perror("read long");
  }

  return (v);
}


/* 
 * Attempts to connect to server, given host and display. Returns file 
 * descriptor (network socket) or 0 if connection fails.
 */

static int connect_to_server (host, display)
     char *host;
     int display;
{
  struct sockaddr_in inaddr;	/* INET socket address. */
  struct sockaddr *addr;		/* address to connect to */
  struct hostent *host_ptr;
  int addrlen;			/* length of address */
#ifdef UNIXCONN
  struct sockaddr_un unaddr;	/* UNIX socket address. */
#endif
  extern char *getenv();
  extern struct hostent *gethostbyname();
  int fd;				/* Network socket */

#ifndef COMPILE_ONLY
  {
#ifdef UNIXCONN
    if ((host[0] == '\0') || 
	(strcmp("unix", host) == 0)) {
	/* Connect locally using Unix domain. */
	unaddr.sun_family = AF_UNIX;
	(void) strcpy(unaddr.sun_path, X_UNIX_PATH);
	sprintf(&unaddr.sun_path[strlen(unaddr.sun_path)], "%d", display);
	addr = (struct sockaddr *) &unaddr;
	addrlen = strlen(unaddr.sun_path) + 2;
	/*
	 * Open the network connection.
	 */
	if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0)
	    return(-1);	    /* errno set by system call. */
    } else
#endif
    {
      /* Get the statistics on the specified host. */
      if ((inaddr.sin_addr.s_addr = inet_addr(host)) == -1) 
	{
	  if ((host_ptr = gethostbyname(host)) == NULL) 
	    {
	      /* No such host! */
	      errno = EINVAL;
	      return(-1);
	    }
	  /* Check the address type for an internet host. */
	  if (host_ptr->h_addrtype != AF_INET) 
	    {
	      /* Not an Internet host! */
	      errno = EPROTOTYPE;
	      return(-1);
	    }
	  /* Set up the socket data. */
	  inaddr.sin_family = host_ptr->h_addrtype;
	  bcopy((char *)host_ptr->h_addr, 
		(char *)&inaddr.sin_addr, 
		sizeof(inaddr.sin_addr));
	} 
      else 
	{
	  inaddr.sin_family = AF_INET;
	}
      addr = (struct sockaddr *) &inaddr;
      addrlen = sizeof (struct sockaddr_in);
      inaddr.sin_port = display + X_TCP_PORT;
      inaddr.sin_port = htons(inaddr.sin_port);
      /*
       * Open the network connection.
       */
      if ((fd = socket((int) addr->sa_family, SOCK_STREAM, 0)) < 0){
	return(-1);	    /* errno set by system call. */}
      /* make sure to turn off TCP coalescence */
#ifdef TCP_NODELAY
      {
	int mi = 1;
	setsockopt (fd, IPPROTO_TCP, TCP_NODELAY, &mi, sizeof (int));
      }
#endif
    }

    /*
     * Changed 9/89 to retry connection if system call was interrupted.  This
     * is necessary for multiprocessing implementations that use timers,
     * since the timer results in a SIGALRM.	-- jdi
     */
    while (connect(fd, addr, addrlen) == -1) {
	if (errno != EINTR) {
  	    (void) close (fd);
  	    return(-1); 	    /* errno set by system call. */
	}
      }
  }
  /*
   * Return the id if the connection succeeded.
   */
#endif
  return(fd);
}

/*
 *	static int waitForSocket(fd, timeout)
 *
 * Description
 *
 *	Waits for some data to be available on the given socket.
 *
 * Inputs
 *
 *	fd    : File descriptor for socket to wait on
 *	timeout: 
 *		Value in milliseconds to wait before timing out.  If zero, wait
 *		until there's input with no timeout.
 *
 * Outputs
 *
 *	True if there was input available, false if it timed out.
 */
static int waitForSocket(fd, timeout)
int	fd, timeout;
{
  struct timeval time, *timePtr;
#ifdef SUNOS4
  fd_set	fds;

  FD_ZERO(&fds);
  FD_SET(fd, &fds);
#else
  int		fds;

  fds = 1 << (fd - 1);
#endif

  if (timeout) {
    time.tv_sec = timeout / 1000;
    time.tv_usec = (timeout % 1000) * 1000;
    timePtr = &time;
  } else {
    timePtr = NULL;
  }

  if (select(getdtablesize(), &fds, NULL, NULL, timePtr)) {
    return (1);
  } else {
    return (0);
  }
}
