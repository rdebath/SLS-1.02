/*
 * Copyright 1989 by Georgia Tech Research Corporation, Atlanta, GA.
 * Copyright 1988, 1989 by Robert Viduya.
 * Copyright 1990 Jeff Sparkes.
 *
 *                         All Rights Reserved
 */

/*
 *	telnet.c
 *		This module initializes and manages a telnet socket to
 *		the given IBM host.
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
/* #ifndef hpux */
#include <arpa/telnet.h>
/* #else
#include "telnet.h"
#endif */
#include <netdb.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>

#define BUFSZ	4096

u_char		myopts[256], hisopts[256];	/* telnet option flags */
u_char		ibuf[4*BUFSZ], *ibptr;		/* 3270 input buffer */
u_char		obuf[4*BUFSZ], *obptr;		/* 3270 output buffer */
u_char		netrbuf[BUFSZ];			/* network input buffer */
u_char		sbbuf[1024], *sbptr;		/* telnet sub-option buffer */
extern		int model_num;			/* 3270 model number */

/* telnet states */
#define TNS_DATA	0	/* receiving data */
#define TNS_IAC		1	/* got an IAC */
#define TNS_WILL	2	/* got an IAC WILL */
#define TNS_WONT	3	/* got an IAC WONT */
#define TNS_DO		4	/* got an IAC DO */
#define TNS_DONT	5	/* got an IAC DONT */
#define TNS_SB		6	/* got an IAC SB */
#define TNS_SB_IAC	7	/* got an IAC after an IAC SB */
static u_char	telnet_state;

/* telnet predefined messages */
static u_char	do_opt[]	= { IAC, DO, '_' };
static u_char	dont_opt[]	= { IAC, DONT, '_' };
static u_char	will_opt[]	= { IAC, WILL, '_' };
static u_char	wont_opt[]	= { IAC, WONT, '_' };
static u_char	ttype_opt[]	= { IAC, SB, TELOPT_TTYPE, TELQUAL_IS,
				    'I', 'B', 'M', '-', '3', '2', '7', '8', '-', '4',
				    IAC, SE };

extern u_long	inet_addr ();


/*
 * connect_net
 *	Establish a telnet socket to the given host passed as an argument.
 *	Called only once and is responsible for setting up the telnet
 *	variables.  Returns the file descriptor of the connected socket.
 */
int
connect_net (host)
char	*host;
{
    struct servent	*sp;
    struct hostent	*hp;
    struct sockaddr_in	sin;
    int			net, i;

    /* initialize the telnet variables */
    bzero ((char *) myopts, sizeof (myopts));
    bzero ((char *) hisopts, sizeof (hisopts));
    telnet_state = TNS_DATA;
    ibptr = &ibuf[0];
    sbptr = &sbbuf[0];

    /* get the tcp/ip service (telnet) */
    if ((sp = getservbyname( "telnet","tcp" )) ==  0) {
	(void) fprintf (stderr, "x3270: telnet/tcp - unknown service\n");
	exit (1);
    }

    /* fill in the socket address of the given host */
    bzero ((char *) &sin, sizeof (sin));
    if ((hp = gethostbyname (host)) == (struct hostent *) 0) {
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = inet_addr (host);
	if (sin.sin_addr.s_addr == -1) {
	    (void) fprintf (stderr, "x3270: unknown host (%s)\n", host);
	    exit (1);
	}
    }
    else {
	sin.sin_family = hp->h_addrtype;
	bcopy ((char *) hp->h_addr, (char *) &sin.sin_addr, hp->h_length);
    }
    sin.sin_port = sp->s_port;

    /* create and establish a connection on the socket */
    if ((net = socket (AF_INET, SOCK_STREAM, 0)) == -1) {
	perror ("x3270: socket");
	exit (1);
    }
    if (connect (net, (struct sockaddr *) &sin, sizeof (sin)) == -1) {
	perror ("x3270: connect");
	exit (1);
    }

    /* set the socket to be asynchronous and non-delaying */
#ifndef hpux
    i = FNDELAY | FASYNC;
    if (fcntl (net, F_SETFL, i) == -1) {
	perror ("x3270: fcntl");
	exit (1);
    }
#else  /* hpux */
    i = O_NDELAY;
    if (fcntl (net, F_SETFL, i) == -1) {
	perror ("x3270: fcntl");
	exit (1);
    }
    i = 1;
    if (ioctl (net, FIOASYNC, (char *)&i) == -1) {
	perror ("x3270: fcntl");
	exit (1);
    }
#endif /* hpux */
    i = -getpid ();
    if (ioctl (net, SIOCSPGRP, &i) == -1) {
	perror ("x3270: ioctl");
	exit (1);
    }

    /* all done */
    return (net);
}


/*
 * net_input
 *	Called by the notifier whenever there is input available on the
 *	socket.  Reads the data, processes the special telnet commands
 *	and calls net_process to process the 3270 data stream.
 */
/*ARGSUSED*/
net_input (fd)
int		fd;
{
    register u_char	*cp;
    int			nr;
    int			br;

    br = 0;
    do {	/* until no more data available */
	nr = read (fd, (char *) netrbuf, BUFSZ);
	if (nr > 0) {
	    br += nr;
	    for (cp = netrbuf; cp < (netrbuf + nr); cp++) {
		switch (telnet_state) {
		    case TNS_DATA:	/* normal data processing */
			if (*cp == IAC)	/* got a telnet command */
			    telnet_state = TNS_IAC;
			else
			    *ibptr++ = *cp;
			break;
		    case TNS_IAC:	/* process a telnet command */
			switch (*cp) {
			    case IAC:	/* escaped IAC, insert it */
				*ibptr++ = *cp;
				telnet_state = TNS_DATA;
				break;
			    case EOR:	/* eor, process accumulated input */
				net_process (ibuf, ibptr - ibuf);
				ibptr = ibuf;
				telnet_state = TNS_DATA;
				break;
			    case WILL:
				telnet_state = TNS_WILL;
				break;
			    case WONT:
				telnet_state = TNS_WONT;
				break;
			    case DO:
				telnet_state = TNS_DO;
				break;
			    case DONT:
				telnet_state = TNS_DONT;
				break;
			    case SB:
				telnet_state = TNS_SB;
				sbptr = sbbuf;
				break;
			}
			break;
		    case TNS_WILL:	/* telnet WILL DO OPTION command */
			switch (*cp) {
			    case TELOPT_BINARY:
			    case TELOPT_EOR:
			    case TELOPT_TTYPE:
				if (!hisopts[*cp]) {
				    hisopts[*cp] = 1;
				    do_opt[2] = *cp;
				    (void) write (fd, (char *) do_opt, sizeof (do_opt));
				}
				break;
			    default:
				dont_opt[2] = *cp;
				(void) write (fd, (char *) dont_opt, sizeof (dont_opt));
				break;
			}
			telnet_state = TNS_DATA;
			break;
		    case TNS_WONT:	/* telnet WONT DO OPTION command */
			switch (*cp) {
			    case TELOPT_BINARY:
			    case TELOPT_EOR:
			    case TELOPT_TTYPE:
				(void) fprintf (stderr, "x3270: Remote host won't do option 0%03o.\n", *cp);
				(void) fprintf (stderr, "x3270: Are you sure it's an IBM?\n");
				exit(0);
				break;
			    default:
				hisopts[*cp] = 0;
				break;
			}
			telnet_state = TNS_DATA;
			break;
		    case TNS_DO:	/* telnet PLEASE DO OPTION command */
			switch (*cp) {
			    case TELOPT_BINARY:
			    case TELOPT_EOR:
			    case TELOPT_TTYPE:
				if (!myopts[*cp]) {
				    myopts[*cp] = 1;
				    will_opt[2] = *cp;
				    (void) write (fd, (char *) will_opt, sizeof (will_opt));
				}
				break;
			    default:
				wont_opt[2] = *cp;
				(void) write (fd, (char *) wont_opt, sizeof (wont_opt));
				break;
			}
			telnet_state = TNS_DATA;
			break;
		    case TNS_DONT:	/* telnet PLEASE DON'T DO OPTION command */
			switch (*cp) {
			    case TELOPT_BINARY:
			    case TELOPT_EOR:
			    case TELOPT_TTYPE:
				(void) fprintf (stderr, "x3270: Remote host says don't do option 0%03o.\n", *cp);
				(void) fprintf (stderr, "x3270: Are you sure it's an IBM?\n");
				exit(0);
				break;
			    default:
				myopts[*cp] = 0;
				break;
			}
			telnet_state = TNS_DATA;
			break;
		    case TNS_SB:	/* telnet sub-option string command */
			if (*cp == IAC)
			    telnet_state = TNS_SB_IAC;
			else
			    *sbptr++ = *cp;
			break;
		    case TNS_SB_IAC:	/* telnet sub-option string command */
			if (*cp == SE) {
			    telnet_state = TNS_DATA;
			    if (sbbuf[0] == TELOPT_TTYPE && sbbuf[1] == TELQUAL_SEND) {
				ttype_opt[13] = model_num + '0';
				(void) write (fd, (char *) ttype_opt, sizeof (ttype_opt));
			    }
			}
			else {
			    *sbptr = *cp;	/* just stuff it */
			    telnet_state = TNS_SB;
			}
			break;
		}
	    }
	}
	else if (nr < 0 && errno != EWOULDBLOCK) {	/* got an error */
	    perror ("x3270: read(net)");
	    exit(0);
	}
    } while (nr > 0);
    if (br == 0)
	exit(0);
    return (0);
}


/*
 * net_output
 *	Called to send output over the network.  Tacks on the necessary
 *	telnet end-of-record command before doing a standard write.
 */
net_output (buf, buflen)
u_char	buf[];
int	buflen;
{
    extern int	net_sock;

    buf[buflen++] = IAC;
    buf[buflen++] = EOR;
    if (write (net_sock, (char *) buf, buflen) != buflen) {
	perror ("x3270: write(net)");
	exit(0);
    }
}
