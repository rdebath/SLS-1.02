/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : nntplib.c
 *  Author    : S.Barber & I.Lea
 *  Created   : 12-01-91
 *  Updated   : 07-11-92
 *  Notes     : NNTP client routines taken from clientlib.c 1.5.11 (10-02-91)
 *  Copyright : (c) Copyright 1991-92 by Stan Barber & Iain Lea
 *              Permission is hereby granted to copy, reproduce, redistribute
 *              or otherwise use this software  as long as: there is no
 *              monetary  profit  gained  specifically  from the use or
 *              reproduction or this software, it is not  sold, rented,
 *              traded or otherwise marketed, and this copyright notice
 *              is included prominently in any copy made. 
 */

#include "tin.h"

#ifdef NNTP_ONLY
#	ifndef NNTP_ABLE
#		define	NNTP_ABLE
#	endif
#endif

#ifndef CDROM_ABLE

FILE	*nntp_rd_fp = NULL;
FILE	*nntp_wr_fp = NULL;

extern	int errno;

#ifdef NNTP_ABLE
#	ifdef TLI
#		include	<fcntl.h>
#		include	<tiuser.h>
#		include	<stropts.h>
#		include	<sys/socket.h>
#		ifdef WIN_TCP
#			include	<sys/in.h>
#		else
#			include	<netinet/in.h>
#		endif
#		define	IPPORT_NNTP	((unsigned short) 119)
#		include 	<netdb.h>	/* All TLI implementations may not have this */
#	else
#		ifdef apollo
#			include </bsd4.3/usr/include/sys/socket.h>
#			include </bsd4.3/usr/include/netinet/in.h>
#			include </bsd4.3/usr/include/netdb.h>
#		else
#			include <sys/socket.h>
#			include <netinet/in.h>
#			ifndef EXCELAN
#				include <netdb.h>
#			endif
#		endif
#	endif /* !TLI */

#	ifndef BSD
#		define	index(a,b)	strchr(a,b)
#		define	bcopy(a,b,c)	memcpy(b,a,c)
#		define	bzero(a,b)	memset(a,'\0',b)
#	endif

#	ifdef EXCELAN
#		if __STDC__
			int connect (int, struct sockaddr *);
			unsigned short htons (unsigned short);
			unsigned long rhost (char **);
			int rresvport (int);
			int socket (int, struct sockproto *, struct sockaddr_in *, int);
#		endif
#	endif

#	ifdef DECNET
#		include <netdnet/dn.h>
#		include <netdnet/dnetdb.h>
#	endif

#endif /* NNTP_ABLE */

/*
 * getserverbyfile	Get the name of a server from a named file.
 *			Handle white space and comments.
 *			Use NNTPSERVER environment variable if set.
 *
 *	Parameters:	"file" is the name of the file to read.
 *
 *	Returns:	Pointer to static data area containing the
 *			first non-ws/comment line in the file.
 *			NULL on error (or lack of entry in file).
 *
 *	Side effects:	None.
 */

char *getserverbyfile (file)
	char	*file;
{
#ifdef NNTP_ABLE
	register FILE	*fp;
	register char	*cp;
	static char	buf[256];

	if (cp = (char *) getenv ("NNTPSERVER")) {
		(void) strcpy (buf, cp);
		return (buf);
	}

	if (file == NULL)
		return (NULL);

	if ((fp = fopen (file, "r")) == NULL)
		return (NULL);

	while (fgets (buf, sizeof (buf), fp) != NULL) {
		if (*buf == '\n' || *buf == '#') {
			continue;
		}
		cp = (char *) index(buf, '\n');
		if (cp) {
			*cp = '\0';
		}
		(void) fclose (fp);
		return (buf);
	}

	(void) fclose (fp);
#endif /* NNTP_ABLE */
	return (NULL);			 /* No entry */
}

/*
 * server_init  Get a connection to the remote server.
 *
 *	Parameters:	"machine" is the machine to connect to.
 *			"service" is the service to connect to on the machine.
 *			"port" is the servive port to connect to.
 *
 *	Returns:	-1 on error
 *			server's initial response code on success.
 *
 *	Side effects:	Connects to server.
 *			"nntp_rd_fp" and "nntp_wr_fp" are fp's
 *			for reading and writing to server.
 */

int server_init (machine, service, port)
	char	*machine;
	char	*service;
	unsigned short port;
{
#ifdef NNTP_ABLE
	int	sockt_rd, sockt_wr;
	char	line[256];
#ifdef DECNET
	char	*cp;

	cp = (char *) index(machine, ':');

	if (cp && cp[1] == ':') {
		*cp = '\0';
		sockt_rd = get_dnet_socket (machine, service);
	} else {
		sockt_rd = get_tcp_socket (machine, service, port);
	}
#else
	sockt_rd = get_tcp_socket (machine, service, port);
#endif

	if (sockt_rd < 0)
		return (-1);

	/*
	 * Now we'll make file pointers (i.e., buffered I/O) out of
	 * the socket file descriptor.  Note that we can't just
	 * open a fp for reading and writing -- we have to open
	 * up two separate fp's, one for reading, one for writing.
	 */

	if ((nntp_rd_fp = (FILE *) fdopen (sockt_rd, "r")) == NULL) {
		perror ("server_init: fdopen #1");
		return (-1);
	}

	sockt_wr = dup (sockt_rd);
#ifdef TLI
	if (t_sync (sockt_rd) < 0) {	/* Sync up new fd with TLI */
    		t_error ("server_init: t_sync");
		nntp_rd_fp = NULL;		/* from above */
		return (-1);
	}
#endif
	if ((nntp_wr_fp = (FILE *) fdopen (sockt_wr, "w")) == NULL) {
		perror ("server_init: fdopen #2");
		nntp_rd_fp = NULL;		/* from above */
		return (-1);
	}

	/*
	 * Now get the server's signon message
	 */

	(void) get_server (line, sizeof (line));
	return (atoi (line));
#else
	return (-1);
#endif /* NNTP_ABLE */
}

/*
 * get_tcp_socket -- get us a socket connected to the specified server.
 *
 *	Parameters:	"machine" is the machine the server is running on.
 *			"service" is the service to connect to on the server.
 *			"port" is the port to connect to on the server.
 *
 *	Returns:	Socket connected to the server if
 *			all is ok, else -1 on error.
 *
 *	Side effects:	Connects to server.
 *
 *	Errors:		Printed via perror.
 */

int get_tcp_socket (machine, service, port)
	char	*machine;	/* remote host */
	char	*service;	/* nttp/smtp etc. */
	unsigned short port;	/* tcp port number */
{
#ifdef NNTP_ABLE
	int	s = -1;
	struct	sockaddr_in sin;
#ifdef TLI 
	struct	hostent *gethostbyname (), *hp;
	struct	t_call	*callptr;

	/*
	 * Create a TCP transport endpoint.
	 */
	if ((s = t_open ("/dev/tcp", O_RDWR, (struct t_info*) 0)) < 0){
		t_error ("t_open: can't t_open /dev/tcp");
		return (-1);
	}
	if (t_bind (s, (struct t_bind *) 0, (struct t_bind *) 0) < 0) {
	   	t_error ("t_bind");
		t_close (s);
		return (-1);
	}
	bzero((char *) &sin, sizeof (sin));	
	sin.sin_family = AF_INET;
	sin.sin_port = htons (port);
	if (!isdigit(*machine) ||
	    (long)(sin.sin_addr.s_addr = inet_addr (machine)) == -1) {
		if((hp = gethostbyname (machine)) == NULL) {
			fprintf (stderr, "gethostbyname: %s: host unknown\n", machine);
			t_close (s);
			return (-1);
		}
		bcopy(hp->h_addr, (char *) &sin.sin_addr, hp->h_length);
	}
	
	/*
	 * Allocate a t_call structure and initialize it.
	 * Let t_alloc() initialize the addr structure of the t_call structure.
	 */
	if ((callptr = (struct t_call *) t_alloc (s,T_CALL,T_ADDR)) == NULL){
		t_error ("t_alloc");
		t_close (s);
		return (-1);
	}

	callptr->addr.maxlen = sizeof (sin);
	callptr->addr.len = sizeof (sin);
	callptr->addr.buf = (char *) &sin;
	callptr->opt.len = 0;			/* no options */
	callptr->udata.len = 0;			/* no user data with connect */

	/*
	 * Connect to the server.
	 */
	if (t_connect (s, callptr, (struct t_call *) 0) < 0) {
		t_error ("t_connect");
		t_close (s);
		return (-1);
	}

	/*
	 * Now replace the timod module with the tirdwr module so that
	 * standard read() and write() system calls can be used on the
	 * descriptor.
	 */

	if (ioctl (s,  I_POP,  (char *) 0) < 0) {
		perror ("I_POP(timod)");
		t_close (s);
		return (-1);
	}

	if (ioctl (s,  I_PUSH, "tirdwr") < 0) {
		perror ("I_PUSH(tirdwr)");
		t_close (s);
		return (-1);
	}
	
#else /* !TLI */
#ifndef EXCELAN
	struct	servent *getservbyname(), *sp;
	struct	hostent *gethostbyname(), *hp;
#ifdef h_addr
	int	x = 0;
	register char **cp;
	static char *alist[1];
#endif /* h_addr */
	unsigned long inet_addr();
	static struct hostent def;
	static struct in_addr defaddr;
	static char namebuf[256];

	if ((sp = getservbyname (service, "tcp")) ==  NULL) {
		fprintf (stderr, "%s/tcp: Unknown service.\n", service);
		return (-1);
	}
	/* If not a raw ip address, try nameserver */
	if (!isdigit(*machine) ||
	    (long)(defaddr.s_addr = inet_addr (machine)) == -1)
		hp = gethostbyname (machine);
	else {
		/* Raw ip address, fake  */
		(void) strcpy (namebuf, machine);
		def.h_name = namebuf;
#ifdef h_addr
		def.h_addr_list = alist;
#endif
		def.h_addr = (char *) &defaddr;
		def.h_length = sizeof (struct in_addr);
		def.h_addrtype = AF_INET;
		def.h_aliases = 0;
		hp = &def;
	}
	if (hp == NULL) {
		fprintf (stderr, "\n%s: Unknown host.\n", machine);
		return (-1);
	}

	bzero((char *) &sin, sizeof (sin));
	sin.sin_family = hp->h_addrtype;
	sin.sin_port = sp->s_port;
#else /* EXCELAN */
	bzero((char *) &sin, sizeof (sin));
	sin.sin_family = AF_INET;
#endif /* EXCELAN */

	/*
	 * The following is kinda gross.  The name server under 4.3
	 * returns a list of addresses, each of which should be tried
	 * in turn if the previous one fails.  However, 4.2 hostent
	 * structure doesn't have this list of addresses.
	 * Under 4.3, h_addr is a #define to h_addr_list[0].
	 * We use this to figure out whether to include the NS specific
	 * code...
	 */

#ifdef h_addr
	/*
	 * get a socket and initiate connection -- use multiple addresses
	 */

	for (cp = hp->h_addr_list; cp && *cp; cp++) {
		s = socket (hp->h_addrtype, SOCK_STREAM, 0);
		if (s < 0) {
			perror ("socket");
			return (-1);
		}
		bcopy(*cp, (char *) &sin.sin_addr, hp->h_length);
		
		if (x < 0) {
			fprintf (stderr, "Trying %s", (char *) inet_ntoa (sin.sin_addr));
		}
		x = connect (s, (struct sockaddr *) &sin, sizeof (sin));
		if (x == 0) {
			break;
		}
		fprintf (stderr, "\nConnection to %s: ", (char *) inet_ntoa (sin.sin_addr));
		perror ("");
		(void) close (s);
	}
	if (x < 0) {
		fprintf (stderr, "Giving up...\n");
		return (-1);
	}
#else	/* no name server */
#ifdef EXCELAN
	if ((s = socket (SOCK_STREAM,(struct sockproto *)NULL,&sin,SO_KEEPALIVE)) < 0) {
		/* Get the socket */
		perror ("socket");
		return (-1);
	}
	bzero((char *) &sin, sizeof (sin));
	sin.sin_family = AF_INET;
	sin.sin_port = htons (IPPORT_NNTP);
	/* set up addr for the connect */

	if ((sin.sin_addr.s_addr = rhost (&machine)) == -1) {
		fprintf (stderr, "\n%s: Unknown host.\n", machine);
		return (-1);
	}
	/* And then connect */

	if (connect (s, (struct sockaddr *)&sin) < 0) {
		perror ("connect");
		(void) close (s);
		return (-1);
	}
#else /* not EXCELAN */
	if ((s = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
		perror ("socket");
		return (-1);
	}

	/* And then connect */

	bcopy (hp->h_addr, (char *) &sin.sin_addr, hp->h_length);
	if (connect (s, (struct sockaddr *) &sin, sizeof (sin)) < 0) {
		perror ("connect");
		(void) close (s);
		return (-1);
	}

#endif /* !EXCELAN */
#endif /* !h_addr */
#endif /* !TLI */
	return (s);
#else
	return (-1);
#endif /* NNTP_ABLE */
}

#ifdef DECNET
/*
 * get_dnet_socket -- get us a socket connected to the server.
 *
 *	Parameters:	"machine" is the machine the server is running on.
 *			"service" is the name of the service to connect to.
 *
 *	Returns:	Socket connected to the news server if
 *			all is ok, else -1 on error.
 *
 *	Side effects:	Connects to server.
 *
 *	Errors:		Printed via nerror.
 */

int get_dnet_socket (machine, service)
	char	*machine;
	char	*service;
{
#ifdef NNTP_ABLE
	int	s, area, node;
	struct	sockaddr_dn sdn;
	struct	nodeent *getnodebyname(), *np;

	bzero((char *) &sdn, sizeof (sdn));

	switch (s = sscanf (machine, "%d%*[.]%d", &area, &node)) {
		case 1: 
			node = area;
			area = 0;
		case 2: 
			node += area*1024;
			sdn.sdn_add.a_len = 2;
			sdn.sdn_family = AF_DECnet;
			sdn.sdn_add.a_addr[0] = node % 256;
			sdn.sdn_add.a_addr[1] = node / 256;
			break;
		default:
			if ((np = getnodebyname (machine)) == NULL) {
				fprintf (stderr, "%s: Unknown host.\n", machine);
				return (-1);
			} else {
				bcopy(np->n_addr, (char *) sdn.sdn_add.a_addr, np->n_length);
				sdn.sdn_add.a_len = np->n_length;
				sdn.sdn_family = np->n_addrtype;
			}
			break;
	}
	sdn.sdn_objnum = 0;
	sdn.sdn_flags = 0;
	sdn.sdn_objnamel = strlen ("NNTP");
	bcopy("NNTP", &sdn.sdn_objname[0], sdn.sdn_objnamel);

	if ((s = socket (AF_DECnet, SOCK_STREAM, 0)) < 0) {
		nerror ("socket");
		return (-1);
	}

	/* And then connect */

	if (connect (s, (struct sockaddr *) &sdn, sizeof (sdn)) < 0) {
		nerror ("connect");
		close (s);
		return (-1);
	}

	return (s);
#else
	return (-1);
#endif /* NNTP_ABLE */
}
#endif

/*
 * handle_server_response
 *
 *	Print some informative messages based on the server's initial
 *	response code.  This is here so inews, rn, etc. can share
 *	the code.
 *
 *	Parameters:	"response" is the response code which the
 *			server sent us, presumably from "server_init",
 *			above.
 *			"nntpserver" is the news server we got the
 *			response code from.
 *
 *	Returns:	-1 if the error is fatal (and we should exit).
 *			0 otherwise.
 *
 *	Side effects:	None.
 */

int handle_server_response (response, nntpserver)
	int	response;
	char	*nntpserver;
{
#ifdef NNTP_ABLE
	switch (response) {
		case OK_NOPOST:		/* fall through */
   		 	printf ("NOTE: This machine does not have permission to post articles.\n");
			printf ("      Please don't waste your time trying.\n\n");

		case OK_CANPOST:
			return (0);
			break;

		case ERR_ACCESS:
			printf ("This machine does not have permission to use the %s news server.\n", nntpserver);
			return (-1);
			break;

		default:
			printf ("Unexpected response code from %s news server: %d\n",
				nntpserver, response);
			return (-1);
			break;
    }
	/*NOTREACHED*/
#else
	return (-1);
#endif /* NNTP_ABLE */
}

/*
 * put_server -- send a line of text to the server, terminating it
 * with CR and LF, as per ARPA standard.
 *
 *	Parameters:	"string" is the string to be sent to the
 *			server.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	Talks to the server.
 *
 *	Note:	This routine flushes the buffer each time
 *			it is called.  For large transmissions
 *			(i.e., posting news) don't use it.  Instead,
 *			do the fprintf's yourself, and then a final
 *			fflush.
 */

void put_server (string)
	char *string;
{
#ifdef NNTP_ABLE
	fprintf (nntp_wr_fp, "%s\r\n", string);
	(void) fflush (nntp_wr_fp);
#endif /* NNTP_ABLE */
}

/*
 * get_server -- get a line of text from the server.  Strips
 * CR's and LF's.
 *
 *	Parameters:	"string" has the buffer space for the
 *			line received.
 *			"size" is the size of the buffer.
 *
 *	Returns:	-1 on error, 0 otherwise.
 *
 *	Side effects:	Talks to server, changes contents of "string".
 */

int get_server (string, size)
	char	*string;
	int	size;
{
#ifdef NNTP_ABLE
	register char *cp;

	while (fgets (string, size, nntp_rd_fp) == NULL) {
		if (errno != EINTR) {
			return (-1);
		}	
	}

	if ((cp = (char *) index(string, '\r')) != NULL) {
		*cp = '\0';
	} else if ((cp = (char *) index(string, '\n')) != NULL) {
		*cp = '\0';
	}

	return (0);
#else
	return (-1);
#endif /* NNTP_ABLE */
}

/*
 * close_server -- close the connection to the server, after sending
 *		the "quit" command.
 *
 *	Parameters:	None.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	Closes the connection with the server.
 *			You can't use "put_server" or "get_server"
 *			after this routine is called.
 */

void close_server ()
{
#ifdef NNTP_ABLE
	char	ser_line[256];

	if (nntp_wr_fp == NULL || nntp_rd_fp == NULL)
		return;

	put_server ("QUIT");
	(void) get_server (ser_line, sizeof (ser_line));

	(void) fclose (nntp_wr_fp);
	(void) fclose (nntp_rd_fp);
#endif /* NNTP_ABLE */
}

#endif /* CDROM_ABLE */

