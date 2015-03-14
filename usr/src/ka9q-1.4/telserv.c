#include <stdio.h>
#include "global.h"
#include "config.h"
#include "mbuf.h"
#include "timer.h"
#include "internet.h"
#include "icmp.h"
#include "netuser.h"
#include "tcp.h"
#include "ftp.h"
#include "telnet.h"
#include "iface.h"
#include "ax25.h"
#include "lapb.h"
#include "session.h"
#include "nr4.h"
#ifdef	UNIX
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <fcntl.h>
#include <errno.h>
#endif

#if (defined(BSD) || defined(SUNOS4))
char *sprintf();
#endif

extern unsigned long telmask;
extern unsigned long selmask;
int telservsock;

/*
 * This is a server that talks to the separate "telnet" program.
 * Those programs use a Unix-domain socket to talk to this code.
 * They pass a command line consisting of null-terminated
 * strings.  The arguments are 
 *   telnet tty termtype host [port]
 */

struct telnet *telserv_tn[32];

/*
 * turn on listening for telnet clients on Unix domain socket
 */

telserv1(argc, argv)
int	argc;
char	*argv[];
{
	struct sockaddr_un unaddr;		/* UNIX socket data block */
	struct sockaddr *addr;		/* generic socket pointer */
	int addrlen;			/* length of addr */
	int fd;

	if (telservsock)
		return;

	unaddr.sun_family = AF_UNIX;
	if(argc < 2)
		sprintf (unaddr.sun_path, "%s%d", "/tmp/telnet", 0);
	else
		sprintf (unaddr.sun_path, "%s-%s", "/tmp/telnet", argv[1]);
	unlink(unaddr.sun_path);
	addr = (struct sockaddr *) &unaddr;
	addrlen = strlen(unaddr.sun_path) + sizeof(unaddr.sun_family);

	/*
	 * Open the network connection.
	 */

	if ((fd = socket ((int) addr->sa_family, SOCK_STREAM, 0))
	    < 0) {
	  printf("telnet server socket failed\r\n");
	  fflush(stdout);
	  return;
	}
	if (bind (fd, addr, addrlen) < 0) {
	  printf("telnet server bind failed\r\n");
	  fflush(stdout);
	  return;
	}
	if (listen (fd, 2) < 0) {
	  printf("telnet server listen failed\r\n");
	  fflush(stdout);
	  return;
	}
	telservsock = fd;
	telmask |= 1 << fd;
	selmask |= 1 << fd;
	return;
}

/*
 * turn off listening for telnet clients on Unix domain socket
 */

telserv0()
{
	if (telservsock) {
		telmask &= ~(1 << telservsock);
		selmask &= ~(1 << telservsock);
		close(telservsock);
		telservsock = 0;
	}
	return;
}

/*
 * called from main select code when there's input.  The select mask
 * includes both the main Unix domain socket (i.e. another client has
 * started up) and the tty's (i.e. a user has typed a character).
 */

telservready(mask)
     unsigned long mask;
{

  int fd;

  for (fd = 0; mask; fd++, mask >>= 1)
    if (mask & 1) {
      if (fd == telservsock)
	telserv_newconn();
      else
	telserv_input(fd);
    }
}

/* handle new connection */

telserv_newconn()
{
  char buffer[1024];
  int len;
  int fd;
  int argc;
  char *argv[5], *cp;
  struct telnet *tn;

  len = sizeof(buffer);
  fd = accept(telservsock, buffer, &len);

  if (fd < 0) {
    printf("telnet server accept failed %d\r\n", errno);
    fflush(stdout);
    return;
  }

  len = read(fd, buffer, sizeof(buffer));
	      
  /* parse the arguments passed from the telnet client */

  for (argc = 0, cp = buffer, argv[0] = buffer; 
       len > 0 && argc < 5;
       len--, cp++)
    if (*cp == 0) {
      argc++;
      argv[argc] = cp+1;
    }

  if (argc < 4) {
    printf("telnet insufficient arguments\r\n");
    close(fd);
    return;
  }
	  
  /* start the telnet connection */

  tn = dotelnet_subr(argc-2,argv+2,argv[1]);
  if (! tn) {
    close(fd);
    return;
  }

  tn->client = fd;
  if (*(argv[2]))  /* is there a non-null value? */
    tn->termtype = strdup(argv[2]);

  /* put the tty's fd into the select mask */

  telserv_tn[tn->fd] = tn;
  telmask |= 1 << tn->fd;
  selmask |= 1 << tn->fd;
}

telserv_close(tn)
  struct telnet *tn;
{
  int fd;
  char c;

  fd = tn->fd;
  c = 0;
 
  /* tell client we're done */
  close(tn->client);  /* pipe to client */
  if (tn->termtype)
    free(tn->termtype);

  /* remove tty from mask -- tty will be closed by ttydriv */
  telmask &= ~(1 << tn->fd);
  selmask &= ~(1 << tn->fd);
  telserv_tn[fd] = NULL;
}  

/*
 * process input from a tty that's being used for telnet.  This is
 * called from the main select, though telserv_ready
 * Note that ttydriv takes a character, does any character editing,
 * and may return anything between nothing and a whole line, depending
 * upon mode and what the user typed.  The parse routine is the main
 * entry into the telnet code to handle an edited input character or line.
 */

telserv_input(fd)
  int fd;
{
  struct telnet *tn;
  int cnt;
  char buf;
  char *bufp;
  char c;

  tn = telserv_tn[fd];
  if (!tn)
    return;

  cnt = read(fd, &buf, 1);
  if (cnt > 0) {
    /* if ttydriv returns 0, we don't want to treat that as an error */
    errno = EINTR;
    cnt = ttydriv(tn->inbuf,buf,&bufp);
  }
  if (cnt > 0)
    tn->parse(bufp, cnt, tn);
  else if (cnt < 0 || errno != EINTR) {
    /* if the telnet process goes away, we get cnt == 0, EAGAIN. */
    /*
     * User has requested us to close the connection.
     * This is slightly hairy.  We can't just close the connection
     * and wait for the protocol to cycle, because that won't
     * happen until time_wait expires.  We have to do something,
     * or the client's tty won't be freed up.  It turns out that
     * things are so intertwined that nothing short of free_telnet
     * will work.  I believe that's safe.  We have to set user to NULL
     * before calling close_tcp.  If the connection is not yet fully
     * open, close_tcp will deallocated the tcb at that point.  It
     * would normally called free_telnet.  It's not so good for us
     * to call it again.  I can't see any way to know for sure
     * that close_tcp has really called it.  So I clear the upcall
     * and call it myself.
     */
    tn->tcb->user = NULL;  /* disable the upcalls */
    close_tcp(tn->tcb);
    free_telnet(tn);
    telserv_tn[fd] = NULL;
  }
}



