/*
 * xdm - display manager daemon
 *
 * $XConsortium: dm.h,v 1.27 89/12/15 20:12:23 keith Exp $
 *
 * Copyright 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:  Keith Packard, MIT X Consortium
 */

/*
 * dm.h
 *
 * public interfaces for greet/verify functionality
 */

# include	<X11/Xos.h>
# include	<X11/Xmd.h>
# include	<X11/Xauth.h>
# include	<X11/Xdmcp.h>

# include	<sys/param.h>	/* for NGROUPS */

#ifdef pegasus
#undef dirty		/* Some bozo put a macro called dirty in sys/param.h */
#endif /* pegasus */

#ifdef SYSV
# define waitCode(w)	(((w) >> 8) & 0xff)
# define waitSig(w)	((w) & 0xff)
typedef int		waitType;
#else
# include	<sys/wait.h>
# define waitCode(w)	((w).w_T.w_Retcode)
# define waitSig(w)	((w).w_T.w_Termsig)
typedef union wait	waitType;
#endif

#ifdef UDP_SOCKET
#include	<sys/types.h>
#include	<netinet/in.h>
#endif

# define waitVal(w)	(waitSig(w) ? (waitSig(w) * 256 + 1) : waitCode (w))

typedef enum displayStatus { running, notRunning, zombie, phoenix } DisplayStatus;

#ifndef FD_ZERO
typedef	struct	my_fd_set { int fds_bits[1]; } my_fd_set;
# define FD_ZERO(fdp)	bzero ((fdp), sizeof (*(fdp)))
# define FD_SET(f,fdp)	((fdp)->fds_bits[(f) / (sizeof (int) * 8)] |=  (1 << ((f) % (sizeof (int) * 8))))
# define FD_CLR(f,fdp)	((fdp)->fds_bits[(f) / (sizeof (int) * 8)] &= ~(1 << ((f) % (sizeof (int) * 8))))
# define FD_ISSET(f,fdp)	((fdp)->fds_bits[(f) / (sizeof (int) * 8)] & (1 << ((f) % (sizeof (int) * 8))))
# define FD_TYPE	my_fd_set
#else
# define FD_TYPE	fd_set
#endif

/*
 * local     - server runs on local host
 * foreign   - server runs on remote host
 * permanent - session restarted when it exits
 * transient - session not restarted when it exits
 * fromFile  - started via entry in servers file
 * fromXDMCP - started with XDMCP
 */

typedef struct displayType {
	unsigned int	location:1;
	unsigned int	lifetime:1;
	unsigned int	origin:1;
} DisplayType;

# define Local		1
# define Foreign	0

# define Permanent	1
# define Transient	0

# define FromFile	1
# define FromXDMCP	0

extern DisplayType parseDisplayType ();

typedef enum fileState { NewEntry, OldEntry, MissingEntry } FileState;

struct display {
	struct display	*next;
	char		*name;		/* DISPLAY name */
	char		*class;		/* display class (may be NULL) */
	char		**argv;		/* program name and arguments */
	DisplayStatus	status;		/* current status */
	int		pid;		/* process id of child */
	int		serverPid;	/* process id of server (-1 if none) */
	FileState	state;		/* state during HUP processing */
	char		*resources;	/* resource file */
	char		*xrdb;		/* xrdb program */
	char		*cpp;		/* cpp program */
	char		*startup;	/* Xstartup program */
	char		*reset;		/* Xreset program */
	char		*session;	/* Xsession program */
	char		*userPath;	/* path set for session */
	char		*systemPath;	/* path set for startup/reset */
	char		*systemShell;	/* interpreter for startup/reset */
	char		*failsafeClient;/* a client to start when the session fails */
	int		authorize;	/* enable authorization */
	Xauth		*authorization;	/* authorization data */
	char		*authFile;	/* file to store authorization in */
	char		*userAuthDir;	/* backup directory for tickets */
	char		*authName;	/* authorization protocol name */
	unsigned short	authNameLen;	/* authorization protocol name len */
	int		openDelay;	/* open delay time */
	int		openRepeat;	/* open attempts to make */
	int		openTimeout;	/* abort open attempt timeout */
	int		startAttempts;	/* number of attempts at starting */
	int		startTries;	/* current start try */
	int		pingInterval;	/* interval between XSync */
	int		pingTimeout;	/* timeout for XSync */
	int		terminateServer;/* restart for each session */
	int		grabServer;	/* keep server grabbed for Login */
	int		grabTimeout;	/* time to wait for grab */
	int		resetForAuth;	/* server reads auth file at reset */
	DisplayType	displayType;	/* method to handle with */
	CARD32		sessionID;	/* ID of active session */
	struct sockaddr	*peer;		/* sockaddr of display peer */
	int		peerlen;	/* length of peer name */
	struct sockaddr	*from;		/* XDMCP port of display */
	int		fromlen;
	CARD16		displayNumber;
};

#define PROTO_TIMEOUT	(30 * 60)   /* 30 minutes should be long enough */

struct protoDisplay {
	struct protoDisplay	*next;
	struct sockaddr		*address;   /* UDP address */
	int			addrlen;    /* UDP address length */
	unsigned long		date;	    /* creation date */
	CARD16			displayNumber;
	CARD16			connectionType;
	ARRAY8			connectionAddress;
	CARD32			sessionID;
	Xauth			*fileAuthorization;
	Xauth			*xdmcpAuthorization;
	ARRAY8			authenticationName;
	ARRAY8			authenticationData;
	XdmAuthKeyRec		key;
};

struct greet_info {
	char		*name;		/* user name */
	char		*password;	/* user password */
	char		*string;	/* random string */
};

struct verify_info {
	int		uid;		/* user id */
#ifdef NGROUPS
	int		groups[NGROUPS];/* group list */
	int		ngroups;	/* number of elements in groups */
#else
	int		gid;		/* group id */
#endif
	char		**argv;		/* arguments to session */
	char		**userEnviron;	/* environment for session */
	char		**systemEnviron;/* environment for startup/reset */
};

/* display manager exit status definitions */

# define OBEYSESS_DISPLAY	0	/* obey multipleSessions resource */
# define REMANAGE_DISPLAY	1	/* force remanage */
# define UNMANAGE_DISPLAY	2	/* force deletion */
# define RESERVER_DISPLAY	3	/* force server termination */
# define OPENFAILED_DISPLAY	4	/* XOpenDisplay failed, retry */

extern char	*config;

extern char	*servers;
extern int	request_port;
extern int	debugLevel;
extern char	*errorLogFile;
extern int	daemonMode;
extern char	*pidFile;
extern int	lockPidFile;
extern char	*remoteAuthDir;
extern int	autoRescan;
extern int	removeDomainname;
extern char	*keyFile;

extern struct display	*FindDisplayByName (),
			*FindDisplayBySessionID (),
			*FindDisplayByAddress (),
			*FindDisplayByPid (),
			*FindDisplayByServerPid (),
			*NewDisplay ();

extern struct protoDisplay	*FindProtoDisplay (),
				*NewProtoDisplay ();

/*
 * CloseOnFork flags
 */

# define CLOSE_ALWAYS	    0
# define LEAVE_FOR_DISPLAY  1

extern char	*malloc (), *realloc (), *strcpy ();

#ifdef SIGNALRETURNSINT
#define SIGVAL int
#else
#define SIGVAL void
#endif
