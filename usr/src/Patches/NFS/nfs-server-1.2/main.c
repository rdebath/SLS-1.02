#include <stdio.h>
#include <syslog.h>
#include <sys/ioctl.h>
#include <rpc/rpc.h>
#include <getopt.h>
#ifdef DEBUG
#include <signal.h>
#endif /* DEBUG */

#include "nfsd.h"
#include "libc.h"
#include "fh.h"			/* Just for fh_init() */
#include "auth.h"

extern void toggle_logging(int sig);
extern void nfs_dispatch(struct svc_req *rqstp, SVCXPRT *transp);

/* No prototypes. */
extern int pmap_unset();


char	*log_file = NULL;

#define _RPCSVC_CLOSEDOWN 120
int	_rpcpmstart = 0;
int	_rpcsvcdirty = 0;
int	_rpcfdtype = 0;

static void closedown(int sig);

static int makesock(int port, int proto, int socksz);

void
main(argc, argv)
int	argc;
char	**argv;
{
    int c;
    int log_flag = 0;
    
    struct sockaddr_in saddr;
    int addr_size;

    SVCXPRT *transp;
    int nfs_socket;
    char *auth_file = NULL;

#ifdef LOG_DAEMON
    openlog("nfsd", LOG_PID, LOG_DAEMON);
#else
    openlog("nfsd", LOG_PID|LOG_TIME);
#define LOG_DAEMON 0		/* Ignore it if it's not there */
#endif /* LOG_DAEMON */

    /* This code uses the RPC library functions in exactly the same way
       a regular RPC application would. */

    nfs_socket = 0;
    _rpcfdtype = 0;
    if (getsockname(0, (struct sockaddr *)&saddr, &addr_size) == 0) {
	int ssize = sizeof (int);

	if (saddr.sin_family != AF_INET)
	    exit(1);
	if (getsockopt(0, SOL_SOCKET, SO_TYPE, (char *) &_rpcfdtype, &ssize) < 0)
	    exit(1);
	_rpcpmstart = 1;
    }
    else
	pmap_unset(NFS_PROGRAM, NFS_VERSION);

    if (_rpcfdtype == 0 || _rpcfdtype == SOCK_DGRAM) {
	if (_rpcfdtype == 0 && (nfs_socket = makesock(NFS_PORT, IPPROTO_UDP, NFS_MAXDATA)) < 0) {
	    fprintf(stderr, "Could not make a UDP socket.\n");
	    exit(1);
	}
	transp = svcudp_create(nfs_socket);
	if (transp == NULL) {
	    fprintf(stderr, "Cannot create UDP service.\n");
	    exit(1);
	}
	if (!svc_register(transp, NFS_PROGRAM, NFS_VERSION, nfs_dispatch,
			  IPPROTO_UDP)) {
	    fprintf(stderr,
		    "unable to register (NFS_PROGRAM, NFS_VERSION, UDP).\n");
	    exit(1);
	}
    }

    if (_rpcfdtype == 0 || _rpcfdtype == SOCK_STREAM) {
	if (_rpcfdtype == 0 && (nfs_socket = makesock(NFS_PORT, IPPROTO_TCP, NFS_MAXDATA)) < 0) {
	    fprintf(stderr, "Could not make a TCP socket.\n");
	    exit(1);
	}
	transp = svctcp_create(nfs_socket, 0, 0);
	if (transp == NULL) {
	    fprintf(stderr, "Cannot create TCP service.\n");
	    exit(1);
	}
	if (!svc_register(transp, NFS_PROGRAM, NFS_VERSION, nfs_dispatch,
			  IPPROTO_TCP)) {
	    fprintf(stderr,
		    "unable to register (NFS_PROGRAM, NFS_VERSION, TCP).\n");
	    exit(1);
	}
    }

    /* How can something as ugly as getopt() be considered good coding
       style?  Now if we had an option-getter that included option
       documentation that could be passed to a shell, that would be
       something... */
    while ((c = getopt(argc, argv, "df:l:p")) != EOF)
	switch (c) {
	case 'd':
	    log_flag++;
	    break;
	case 'f':
	    auth_file = optarg;
	    break;
	case 'l':
	    log_file = optarg;
	    break;
	case 'p':
	    promiscuous = 1;
	    break;
	case '?':
	    syslog(LOG_DAEMON|LOG_ERR, "Unknown option '-%c'", c);
	}
    if (argv[optind] != NULL)
	    syslog(LOG_DAEMON|LOG_ERR, "Bad argument -- %s", argv[optind]);
    /* We have to deal with the grunge of real/effective/saved uid.
       First we make both both the real and effective uid to root.  We shouldn't
       really need to change the real uid, but it occasionally has problems with
       losing root permissions if we don't.  ( ??? yeah -- I should track it down) */
    if (setreuid(0, 0) < 0) {
	fprintf(stderr, "%s:Unable to reset my reuids to root: %s.\n", argv[0],
		sys_errlist[errno]);
	exit(1);
    }

    if (log_flag)
	toggle_logging(0);

#ifndef RPC_SVC_FG
    if (_rpcfdtype == 0) {
	int fd;
	
	if (fork())
	    exit(0);
	close(0);
	close(1);
	close(2);
	if ((fd = open("/dev/tty", 2, 0)) >= 0) {
	    ioctl(fd, TIOCNOTTY, (char *)0);
	    (void) close(fd);
	}
    }
#endif /* NO_FORK */

    fh_init();
    init_auth(auth_file);
#ifdef DEBUG
    signal(SIGUSR1, toggle_logging);
#endif /* DEBUG */

     if (_rpcpmstart) {
	signal(SIGALRM, closedown);
	alarm(_RPCSVC_CLOSEDOWN);
     }

    svc_run();
    syslog(LOG_DAEMON|LOG_ERR, "Oh no Mr. Bill... svc_run returned!\n");
    exit(1);
}


static int makesock(int port, int proto, int socksz)
{
    struct sockaddr_in	sin;
    int			s;
    int			sock_type;

    sock_type = (proto == IPPROTO_UDP) ? SOCK_DGRAM : SOCK_STREAM;
    s = socket(AF_INET, sock_type, proto);
    if (s < 0) {
	syslog(LOG_DAEMON|LOG_ERR, "Could not make a socket: %m");
	return -1;
    }
    bzero((char *)&sin, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = INADDR_ANY;
    sin.sin_port = htons(port);
    
#ifdef SO_SNDBUF
    {
	int	sblen, rblen;

	sblen = rblen = socksz + 1024;
	/* 1024 for rpc & transport overheads */
	if ( setsockopt(s, SOL_SOCKET, SO_SNDBUF, &sblen, sizeof sblen) < 0
	    || setsockopt(s, SOL_SOCKET, SO_RCVBUF, &rblen, sizeof rblen) < 0)
	    syslog(LOG_DAEMON|LOG_ERR, "setsockopt failed: %m");
    }
#endif /* SO_SNDBUF */
    
    if (bind(s, (struct sockaddr *) &sin, sizeof(sin)) == -1) {
	syslog(LOG_DAEMON|LOG_ERR, "Could not bind name to socket: %m");
	return -1;
    }
    
    return s;
}

/* this is taken from an rpcgen generated service file */

static void
closedown(int sig)
{
	(void) signal(sig, closedown);
	if (_rpcsvcdirty == 0) {
		extern fd_set svc_fdset;
		static int size;
		int i, openfd;

		if (_rpcfdtype == SOCK_DGRAM)
			exit(0);
		if (size == 0) {
			size = getdtablesize();
		}
		for (i = 0, openfd = 0; i < size && openfd < 2; i++)
			if (FD_ISSET(i, &svc_fdset))
				openfd++;
		if (openfd <= 1)
			exit(0);
	}
	(void) alarm(_RPCSVC_CLOSEDOWN);
}

