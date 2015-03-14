/*
 * xdm - display manager daemon
 *
 * $Header: /home/x_cvs/mit/clients/xdm/auth.c,v 1.6 1992/09/16 15:09:10 dawes Exp $
 * $XConsortium: auth.c,v 1.47 91/11/08 15:18:18 eswu Exp $
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
 * auth.c
 *
 * maintain the authorization generation daemon
 */

#if defined(TCPCONN) || defined(DNETCONN) || defined(UNIXCONN)
# define HASSOCKETS
#endif

#include "dm.h"
#include <X11/X.h>
#if defined(USG) || defined(SYSV)
# ifndef __TYPES__
#  include <sys/types.h>
#  define __TYPES__
# endif
#endif
#include <sys/stat.h>
#ifdef HASSOCKETS
# include <sys/socket.h>
#endif /* HASSOCKETS */
# include <sys/ioctl.h>

#ifdef TCPCONN
# include <netinet/in.h>
#endif
#ifdef DNETCONN
# include <netdnet/dn.h>
# include <netdnet/dnetdb.h>
#endif

#if (defined(_POSIX_SOURCE) && !defined(AIXV3)) || defined(hpux) || defined(USG) || defined(SYSV) || defined(SVR4)
#define NEED_UTSNAME
#include <sys/utsname.h>
#endif

#if defined(SYSV) && defined(SYSV386)
# include <sys/stream.h>
# ifdef ISC
#  include <sys/sioctl.h>
# endif /* ISC */
#if 0
# ifdef ESIX
#  include <lan/net_ioctl.h>
# endif /* ESIX */
#endif
#endif /* SYSV386 */

#ifdef SVR4
# include <netdb.h>
# include <sys/sockio.h>
#endif
#ifdef __convex__
# include <sync/queue.h>
# include <sync/sema.h>
#endif
#ifdef HASSOCKETS
# include <net/if.h>
#endif /* HASSOCKETS */

extern int	MitInitAuth ();
extern Xauth	*MitGetAuth ();

#ifdef HASXDMAUTH
extern int	XdmInitAuth ();
extern Xauth	*XdmGetAuth ();
#ifdef XDMCP
extern int	XdmGetXdmcpAuth ();
#else
#define XdmGetXdmcpAuth NULL
#endif
#endif

#ifdef SECURE_RPC
extern int	SecureRPCInitAuth ();
extern Xauth	*SecureRPCGetAuth ();
#endif

struct AuthProtocol {
    unsigned short  name_length;
    char	    *name;
    int		    (*InitAuth)();
    Xauth	    *(*GetAuth)();
    int		    (*GetXdmcpAuth)();
    int		    inited;
};

static struct AuthProtocol AuthProtocols[] = {
{ (unsigned short) 18,	"MIT-MAGIC-COOKIE-1",
    MitInitAuth, MitGetAuth, NULL
},
#ifdef HASXDMAUTH
{ (unsigned short) 19,	"XDM-AUTHORIZATION-1",
    XdmInitAuth, XdmGetAuth, XdmGetXdmcpAuth,
},
#endif
#ifdef SECURE_RPC
{ (unsigned short) 9, "SUN-DES-1",
    SecureRPCInitAuth, SecureRPCGetAuth, NULL,
},
#endif
};

#define NUM_AUTHORIZATION (sizeof (AuthProtocols) / sizeof (AuthProtocols[0]))

static struct AuthProtocol *
findProtocol (name_length, name)
    unsigned short  name_length;
    char	    *name;
{
    int	i;

    for (i = 0; i < NUM_AUTHORIZATION; i++)
	if (AuthProtocols[i].name_length == name_length &&
	    bcmp (AuthProtocols[i].name, name, name_length) == 0)
	{
	    return &AuthProtocols[i];
	}
    return (struct AuthProtocol *) 0;
}

ValidAuthorization (name_length, name)
    unsigned short  name_length;
    char	    *name;
{
    if (findProtocol (name_length, name))
	return TRUE;
    return FALSE;
}

static Xauth *
GenerateAuthorization (name_length, name)
unsigned short	name_length;
char		*name;
{
    struct AuthProtocol	*a;
    Xauth   *auth = 0;
    int	    i;

    Debug ("GenerateAuthorization %*.*s\n",
	    name_length, name_length, name);
    a = findProtocol (name_length, name);
    if (a)
    {
	if (!a->inited)
	{
	    (*a->InitAuth) (name_length, name);
	    a->inited = TRUE;
	}
	auth = (*a->GetAuth) (name_length, name);
	if (auth)
	{
	    Debug ("Got 0x%x (%d %*.*s) ", auth,
		auth->name_length, auth->name_length,
 		auth->name_length, auth->name);
	    for (i = 0; i < (int)auth->data_length; i++)
		Debug (" %02x", auth->data[i] & 0xff);
	    Debug ("\n");
	}
	else
	    Debug ("Got (null)\n");
    }
    else
    {
	Debug ("Unknown authorization %*.*s\n", name_length, name_length, name);
    }
    return auth;
}

#ifdef XDMCP

SetProtoDisplayAuthorization (pdpy,
    authorizationNameLen, authorizationName)
    struct protoDisplay	*pdpy;
    unsigned short	authorizationNameLen;
    char		*authorizationName;
{
    struct AuthProtocol	*a;
    Xauth   *auth;

    a = findProtocol (authorizationNameLen, authorizationName);
    pdpy->xdmcpAuthorization = pdpy->fileAuthorization = 0;
    if (a)
    {
	if (!a->inited)
	{
	    (*a->InitAuth) (authorizationNameLen, authorizationName);
	    a->inited = TRUE;
	}
	if (a->GetXdmcpAuth)
	{
	    (*a->GetXdmcpAuth) (pdpy, authorizationNameLen, authorizationName);
	    auth = pdpy->xdmcpAuthorization;
	}
	else
	{
	    auth = (*a->GetAuth) (authorizationNameLen, authorizationName);
	    pdpy->fileAuthorization = auth;
	    pdpy->xdmcpAuthorization = 0;
	}
	if (auth)
	    Debug ("Got 0x%x (%d %*.*s)\n", auth,
		auth->name_length, auth->name_length,
 		auth->name_length, auth->name);
	else
	    Debug ("Got (null)\n");
    }
}

#endif /* XDMCP */

static
CleanUpFileName (src, dst, len)
char	*src, *dst;
int	len;
{
    while (*src) {
	if (--len <= 0)
		break;
	switch (*src & 0x7f)
	{
	case '/':
	    *dst++ = '_';
	    break;
	case '-':
	    *dst++ = '.';
	    break;
	default:
	    *dst++ = (*src & 0x7f);
	}
	++src;
    }
    *dst = '\0';
}

static
MakeServerAuthFile (d)
    struct display  *d;
{
    int len;
#ifdef SYSV
#define NAMELEN	14
#else
#define NAMELEN	255
#endif
    char    cleanname[NAMELEN];

    if (d->clientAuthFile && *d->clientAuthFile)
	len = strlen (d->clientAuthFile) + 1;
    else
    {
    	CleanUpFileName (d->name, cleanname, NAMELEN - 8);
    	len = strlen (authDir) + strlen (cleanname) + 12;
    }
    if (d->authFile)
	free (d->authFile);
    d->authFile = malloc ((unsigned) len);
    if (!d->authFile)
	return FALSE;
    if (d->clientAuthFile && *d->clientAuthFile)
	strcpy (d->authFile, d->clientAuthFile);
    else
    {
    	sprintf (d->authFile, "%s/A%s-XXXXXX", authDir, cleanname);
    	(void) mktemp (d->authFile);
    }
    return TRUE;
}

SaveServerAuthorizations (d, auths, count)
    struct display  *d;
    Xauth	    **auths;
    int		    count;
{
    FILE	*auth_file;
    int		mask;
    int		ret;
    int		i;

    mask = umask (0077);
    if (!d->authFile && !MakeServerAuthFile (d))
	return FALSE;
    (void) unlink (d->authFile);
    auth_file = fopen (d->authFile, "w");
    umask (mask);
    if (!auth_file) {
	Debug ("Can't creat auth file %s\n", d->authFile);
	LogError ("Cannot open server authorization file %s\n", d->authFile);
	free (d->authFile);
	d->authFile = NULL;
	ret = FALSE;
    }
    else
    {
    	Debug ("File: %s auth: %x\n", d->authFile, auths);
	ret = TRUE;
	for (i = 0; i < count; i++)
	{
	    if (!XauWriteAuth (auth_file, auths[i]) ||
		fflush (auth_file) == EOF)
	    {
		LogError ("Cannot write server authorization file %s\n",
			  d->authFile);
		ret = FALSE;
		free (d->authFile);
		d->authFile = NULL;
	    }
    	}
	fclose (auth_file);
    }
    return ret;
}

SetLocalAuthorization (d)
    struct display	*d;
{
    Xauth	*auth, **auths;
    int		i, j;

    if (d->authorizations)
    {
	for (i = 0; i < d->authNum; i++)
	    XauDisposeAuth (d->authorizations[i]);
	free ((char *) d->authorizations);
	d->authorizations = (Xauth **) NULL;
	d->authNum = 0;
    }
    if (!d->authNames)
	return;
    for (i = 0; d->authNames[i]; i++)
	;
    d->authNameNum = i;
    if (d->authNameLens)
	free ((char *) d->authNameLens);
    d->authNameLens = (unsigned short *) malloc
				(d->authNameNum * sizeof (unsigned short));
    if (!d->authNameLens)
	return;
    for (i = 0; i < d->authNameNum; i++)
	d->authNameLens[i] = strlen (d->authNames[i]);
    auths = (Xauth **) malloc (d->authNameNum * sizeof (Xauth *));
    if (!auths)
	return;
    j = 0;
    for (i = 0; i < d->authNameNum; i++)
    {
	auth = GenerateAuthorization (d->authNameLens[i], d->authNames[i]);
	if (auth)
	    auths[j++] = auth;
    }
    if (SaveServerAuthorizations (d, auths, j))
    {
	d->authorizations = auths;
	d->authNum = j;
    }
    else
    {
	for (i = 0; i < j; i++)
	    XauDisposeAuth (auths[i]);
	free ((char *) auths);
    }
}

SetAuthorization (d)
    struct display  *d;
{
    Xauth   **auth;

    auth = d->authorizations;
    if (auth && *auth)
	XSetAuthorization ((*auth)->name, (int) (*auth)->name_length,
			   (*auth)->data, (int) (*auth)->data_length);
}

static
openFiles (name, new_name, oldp, newp)
char	*name, *new_name;
FILE	**oldp, **newp;
{
	int	mask;

	strcpy (new_name, name);
	strcat (new_name, "-n");
	mask = umask (0077);
	(void) unlink (new_name);
	*newp = fopen (new_name, "w");
	(void) umask (mask);
	if (!*newp) {
		Debug ("can't open new file %s\n", new_name);
		return 0;
	}
	*oldp = fopen (name, "r");
	Debug ("opens succeeded %s %s\n", name, new_name);
	return 1;
}

static
binaryEqual (a, b, len)
char	*a, *b;
unsigned short	len;
{
	while (len-- > 0)
		if (*a++ != *b++)
			return 0;
	return 1;
}

static
dumpBytes (len, data)
unsigned short	len;
char	*data;
{
	unsigned short	i;

	Debug ("%d: ", len);
	for (i = 0; i < len; i++)
		Debug ("%02x ", data[i] & 0377);
	Debug ("\n");
}

static
dumpAuth (auth)
    Xauth	*auth;
{
	Debug ("family: %d\n", auth->family);
	Debug ("addr:   ");
	dumpBytes (auth->address_length, auth->address);
	Debug ("number: ");
	dumpBytes (auth->number_length, auth->number);
	Debug ("name:   ");
	dumpBytes (auth->name_length, auth->name);
	Debug ("data:   ");
	dumpBytes (auth->data_length, auth->data);
}

struct addrList {
	unsigned short	family;
	unsigned short	address_length;
	char	*address;
	unsigned short	number_length;
	char	*number;
	unsigned short	name_length;
	char	*name;
	struct addrList	*next;
};

static struct addrList	*addrs;

static
initAddrs ()
{
	addrs = 0;
}

static
doneAddrs ()
{
	struct addrList	*a, *n;
	for (a = addrs; a; a = n) {
		n = a->next;
		if (a->address)
			free (a->address);
		if (a->number)
			free (a->number);
		free ((char *) a);
	}
}

static checkEntry ();

static
saveEntry (auth)
    Xauth	*auth;
{
	struct addrList	*new;

	new = (struct addrList *) malloc (sizeof (struct addrList));
	if (!new) {
		LogOutOfMem ("saveEntry");
		return;
	}
	if ((new->address_length = auth->address_length) > 0) {
		new->address = malloc (auth->address_length);
		if (!new->address) {
			LogOutOfMem ("saveEntry");
			free ((char *) new);
			return;
		}
		bcopy (auth->address, new->address, (int) auth->address_length);
	} else
		new->address = 0;
	if ((new->number_length = auth->number_length) > 0) {
		new->number = malloc (auth->number_length);
		if (!new->number) {
			LogOutOfMem ("saveEntry");
			free (new->address);
			free ((char *) new);
			return;
		}
		bcopy (auth->number, new->number, (int) auth->number_length);
	} else
		new->number = 0;
	if ((new->name_length = auth->name_length) > 0) {
		new->name = malloc (auth->name_length);
		if (!new->name) {
			LogOutOfMem ("saveEntry");
			free (new->number);
			free (new->address);
			free ((char *) new);
			return;
		}
		bcopy (auth->name, new->name, (int) auth->name_length);
	} else
		new->name = 0;
	new->family = auth->family;
	new->next = addrs;
	addrs = new;
}

static
checkEntry (auth)
    Xauth	*auth;
{
	struct addrList	*a;

	for (a = addrs; a; a = a->next) {
		if (a->family == auth->family &&
		    a->address_length == auth->address_length &&
 		    binaryEqual (a->address, auth->address, auth->address_length) &&
		    a->number_length == auth->number_length &&
 		    binaryEqual (a->number, auth->number, auth->number_length) &&
		    a->name_length == auth->name_length &&
		    binaryEqual (a->name, auth->name, auth->name_length))
		{
			return 1;
		}
	}
	return 0;
}

static int  doWrite;

static
writeAuth (file, auth)
    FILE	*file;
    Xauth	*auth;
{
        Debug ("writeAuth: doWrite = %d\n", doWrite);
	dumpAuth (auth);	/* does Debug only */
	if (doWrite)
	    XauWriteAuth (file, auth);
}

static
writeAddr (family, addr_length, addr, file, auth)
    int		family;
    int		addr_length;
    char	*addr;
    FILE	*file;
    Xauth	*auth;
{
	auth->family = (unsigned short) family;
	auth->address_length = addr_length;
	auth->address = addr;
	Debug ("writeAddr: writing and saving an entry\n");
	writeAuth (file, auth);
	saveEntry (auth);
}

static
DefineLocal (file, auth)
    FILE	*file;
    Xauth	*auth;
{
	char	displayname[100];

	/* stolen from xinit.c */

/* Make sure this produces the same string as _XGetHostname in lib/X/XlibInt.c.
 * Otherwise, Xau will not be able to find your cookies in the Xauthority file.
 *
 * Note: POSIX says that the ``nodename'' member of utsname does _not_ have
 *       to have sufficient information for interfacing to the network,
 *       and so, you may be better off using gethostname (if it exists).
 */

#ifdef NEED_UTSNAME

	/* hpux:
	 * Why not use gethostname()?  Well, at least on my system, I've had to
	 * make an ugly kernel patch to get a name longer than 8 characters, and
	 * uname() lets me access to the whole string (it smashes release, you
	 * see), whereas gethostname() kindly truncates it for me.
	 */
	{
	struct utsname name;

	uname(&name);
	strcpy(displayname, name.nodename);
	}
#else
        /* AIXV3:
	 * In AIXV3, _POSIX_SOURCE is defined, but uname gives only first
	 * field of hostname. Thus, we use gethostname instead.
	 */

	gethostname(displayname, sizeof(displayname));
#endif
	writeAddr (FamilyLocal, strlen (displayname), displayname, file, auth);
}

#ifdef STREAMSCONN

#include <tiuser.h>

/* Define this host for access control.  Find all the hosts the OS knows about 
 * for this fd and add them to the selfhosts list.
 * TLI version, written without sufficient documentation.
 */
static
DefineSelf (fd, file, auth)
    int fd;
    FILE	*file;
    Xauth	*auth;
{
    struct netbuf	netb;
    char		addrret[1024]; /* easier than t_alloc */
    
    netb.maxlen = sizeof(addrret);
    netb.buf = addrret;
    if (t_getname (fd, &netb, LOCALNAME) == -1)
	t_error ("t_getname");
    /* what a kludge */
    writeAddr (FamilyInternet, 4, netb.buf+4, file, auth);
}

#else /* STREAMSCONN */
#if defined(SIOCGIFCONF) && defined(HASSOCKETS)

/* Define this host for access control.  Find all the hosts the OS knows about 
 * for this fd and add them to the selfhosts list.
 */
static
DefineSelf (fd, file, auth)
    int fd;
    FILE	*file;
    Xauth	*auth;
{
    char		buf[2048];
    struct ifconf	ifc;
    register int	n;
    int 		len;
    char 		*addr;
    int 		family;
    register struct ifreq *ifr;
    
    ifc.ifc_len = sizeof (buf);
    ifc.ifc_buf = buf;
    if (ioctl (fd, SIOCGIFCONF, (char *) &ifc) < 0)
        LogError ("Trouble getting network interface configuration");
    for (ifr = ifc.ifc_req, n = ifc.ifc_len / sizeof (struct ifreq); --n >= 0;
     ifr++)
    {
#ifdef DNETCONN
	/*
	 * this is ugly but SIOCGIFCONF returns decnet addresses in
	 * a different form from other decnet calls
	 */
	if (ifr->ifr_addr.sa_family == AF_DECnet) {
		len = sizeof (struct dn_naddr);
		addr = (char *)ifr->ifr_addr.sa_data;
		family = FamilyDECnet;
	} else
#endif
	{
	    if (ConvertAddr (&ifr->ifr_addr, &len, &addr) < 0)
		continue;
	    if (len == 0)
 	    {
		Debug ("Skipping zero length address\n");
		continue;
	    }
	    /*
	     * don't write out 'localhost' entries, as
	     * they may conflict with other local entries.
	     * DefineLocal will always be called to add
	     * the local entry anyway, so this one can
	     * be tossed.
	     */
	    if (len == 4 &&
		addr[0] == 127 && addr[1] == 0 &&
		addr[2] == 0 && addr[3] == 1)
	    {
		    Debug ("Skipping localhost address\n");
		    continue;
	    }
	    family = FamilyInternet;
	}
	Debug ("DefineSelf: write network address, length %d\n", len);
	writeAddr (family, len, addr, file, auth);
    }
}

#else /* SIOCGIFCONF */

/* Define this host for access control.  Find all the hosts the OS knows about 
 * for this fd and add them to the selfhosts list.
 */
static
DefineSelf (fd, file, auth)
    int fd;
{
#ifdef linux
	return -1;
#else /* !linux */
#ifndef HASSOCKETS
    return -1;
#else
    register int n;
    int	len;
    caddr_t	addr;
    int		family;

    struct utsname name;
    register struct hostent  *hp;

    union {
	struct  sockaddr   sa;
	struct  sockaddr_in  in;
    } saddr;
	
    struct	sockaddr_in	*inetaddr;

    /* hpux:
     * Why not use gethostname()?  Well, at least on my system, I've had to
     * make an ugly kernel patch to get a name longer than 8 characters, and
     * uname() lets me access to the whole string (it smashes release, you
     * see), whereas gethostname() kindly truncates it for me.
     */
    uname(&name);
    hp = gethostbyname (name.nodename);
    if (hp != NULL) {
	saddr.sa.sa_family = hp->h_addrtype;
	inetaddr = (struct sockaddr_in *) (&(saddr.sa));
	bcopy ( (char *) hp->h_addr, (char *) &(inetaddr->sin_addr), (int) hp->h_length);
	family = ConvertAddr ( &(saddr.sa), &len, &addr);
	if ( family >= 0) {
	    writeAddr (FamilyInternet, sizeof (inetaddr->sin_addr),
			(char *) (&inetaddr->sin_addr), file, auth);
	}
    }
#endif /* HASSOCKETS */
#endif /* else !linux */
}

#endif /* SIOCGIFCONF else */
#endif /* STREAMSCONN else */

static
setAuthNumber (auth, name)
    Xauth   *auth;
    char    *name;
{
    char	*colon;
    char	*dot, *number;

    Debug ("setAuthNumber %s\n", name);
    colon = rindex (name, ':');
    if (colon) {
	++colon;
	dot = index (colon, '.');
	if (dot)
	    auth->number_length = dot - colon;
	else
	    auth->number_length = strlen (colon);
	number = malloc (auth->number_length + 1);
	if (number) {
	    strncpy (number, colon, auth->number_length);
	    number[auth->number_length] = '\0';
	} else {
	    LogOutOfMem ("setAuthNumber");
	    auth->number_length = 0;
	}
	auth->number = number;
	Debug ("setAuthNumber: %s\n", number);
    }
}

static
writeLocalAuth (file, auth, name)
    FILE	*file;
    Xauth	*auth;
    char	*name;
{
    int	fd;

    Debug ("writeLocalAuth: %s %.*s\n", name, auth->name_length, auth->name);
    setAuthNumber (auth, name);
#ifdef TCPCONN
    fd = socket (AF_INET, SOCK_STREAM, 0);
    DefineSelf (fd, file, auth);
    close (fd);
#endif
#ifdef DNETCONN
    fd = socket (AF_DECnet, SOCK_STREAM, 0);
    DefineSelf (fd, file, auth);
    close (fd);
#endif
    DefineLocal (file, auth);
}

#ifdef XDMCP

static
writeRemoteAuth (file, auth, peer, peerlen, name)
    FILE	    *file;
    Xauth	    *auth;
    struct sockaddr *peer;
    int		    peerlen;
    char	    *name;
{
    int	    family = FamilyLocal;
    char    *addr;
    
    Debug ("writeRemoteAuth: %s %.*s\n", name, auth->name_length, auth->name);
    if (!peer || peerlen < 2)
	return;
    setAuthNumber (auth, name);
    family = ConvertAddr (peer, &peerlen, &addr);
    Debug ("writeRemoteAuth: family %d\n", family);
    if (family != FamilyLocal)
    {
	Debug ("writeRemoteAuth: %d, %d, %x\n",
		family, peerlen, *(int *)addr);
	writeAddr (family, peerlen, addr, file, auth);
    }
    else
    {
	writeLocalAuth (file, auth, name);
    }
}

#endif /* XDMCP */

SetUserAuthorization (d, verify)
struct display		*d;
struct verify_info	*verify;
{
    FILE	*old, *new;
    char	home_name[1024], backup_name[1024], new_name[1024];
    char	*name;
    char	*home;
    char	*envname = 0;
    int	lockStatus;
    Xauth	*entry, **auths;
    int	setenv;
    char	**setEnv (), *getEnv ();
    struct stat	statb;
    int		i;
    int		magicCookie;

    Debug ("SetUserAuthorization\n");
    auths = d->authorizations;
    if (auths) {
	home = getEnv (verify->userEnviron, "HOME");
	lockStatus = LOCK_ERROR;
	if (home) {
	    strcpy (home_name, home);
	    if (home[strlen(home) - 1] != '/')
		strcat (home_name, "/");
	    strcat (home_name, ".Xauthority");
	    Debug ("XauLockAuth %s\n", home_name);
	    lockStatus = XauLockAuth (home_name, 1, 2, 10);
	    Debug ("Lock is %d\n", lockStatus);
	    if (lockStatus == LOCK_SUCCESS) {
		if (openFiles (home_name, new_name, &old, &new)) {
		    name = home_name;
		    setenv = 0;
		} else {
		    Debug ("openFiles failed\n");
		    XauUnlockAuth (home_name);
		    lockStatus = LOCK_ERROR;
		}	
	    }
	}
	if (lockStatus != LOCK_SUCCESS) {
	    sprintf (backup_name, "%s/.XauthXXXXXX", d->userAuthDir);
	    (void) mktemp (backup_name);
	    lockStatus = XauLockAuth (backup_name, 1, 2, 10);
	    Debug ("backup lock is %d\n", lockStatus);
	    if (lockStatus == LOCK_SUCCESS) {
		if (openFiles (backup_name, new_name, &old, &new)) {
		    name = backup_name;
		    setenv = 1;
		} else {
		    XauUnlockAuth (backup_name);
		    lockStatus = LOCK_ERROR;
		}	
	    }
	}
	if (lockStatus != LOCK_SUCCESS) {
	    Debug ("can't lock auth file %s or backup %s\n",
			    home_name, backup_name);
	    LogError ("can't lock authorization file %s or backup %s\n",
			    home_name, backup_name);
	    return;
	}
	initAddrs ();
	doWrite = 1;
	Debug ("%d authorization protocols for %s\n", d->authNum, d->name);
	/*
	 * Write MIT-MAGIC-COOKIE-1 authorization first, so that
	 * R4 clients which only knew that, and used the first
	 * matching entry will continue to function
	 */
	magicCookie = -1;
	for (i = 0; i < d->authNum; i++)
	{
	    if (auths[i]->name_length == 18 &&
		!strncmp (auths[i]->name, "MIT-MAGIC-COOKIE-1", 18))
	    {
		magicCookie = i;
	    	if (d->displayType.location == Local)
	    	    writeLocalAuth (new, auths[i], d->name);
#ifdef XDMCP
	    	else
	    	    writeRemoteAuth (new, auths[i], d->peer, d->peerlen, d->name);
#endif
		break;
	    }
	}
	/* now write other authorizations */
	for (i = 0; i < d->authNum; i++)
	{
	    if (i != magicCookie)
	    {
	    	if (d->displayType.location == Local)
	    	    writeLocalAuth (new, auths[i], d->name);
#ifdef XDMCP
	    	else
	    	    writeRemoteAuth (new, auths[i], d->peer, d->peerlen, d->name);
#endif
	    }
	}
	if (old) {
	    if (fstat (fileno (old), &statb) != -1)
		chmod (new_name, (int) (statb.st_mode & 0777));
	    /*SUPPRESS 560*/
	    while (entry = XauReadAuth (old)) {
		if (!checkEntry (entry))
		{
		    Debug ("Writing an entry\n");
		    writeAuth (new, entry);
		}
		XauDisposeAuth (entry);
	    }
	    fclose (old);
	}
	doneAddrs ();
	fclose (new);
	if (unlink (name) == -1)
	    Debug ("unlink %s failed\n", name);
	envname = name;
	if (link (new_name, name) == -1) {
	    Debug ("link failed %s %s\n", new_name, name);
	    LogError ("Can't move authorization into place\n");
	    setenv = 1;
	    envname = new_name;
	} else {
	    Debug ("new is in place, go for it!\n");
	    unlink (new_name);
	}
	if (setenv) {
	    verify->userEnviron = setEnv (verify->userEnviron,
				    "XAUTHORITY", envname);
	    verify->systemEnviron = setEnv (verify->systemEnviron,
				    "XAUTHORITY", envname);
	}
	XauUnlockAuth (name);
	if (envname) {
#ifdef NGROUPS_MAX
	    chown (envname, verify->uid, verify->groups[0]);
#else
	    chown (envname, verify->uid, verify->gid);
#endif
	}
    }
    Debug ("done SetUserAuthorization\n");
}

RemoveUserAuthorization (d, verify)
    struct display	*d;
    struct verify_info	*verify;
{
    char    *home;
    Xauth   **auths, *entry;
    char    name[1024], new_name[1024];
    int	    lockStatus;
    FILE    *old, *new;
    struct stat	statb;
    int	    i;
    char    *getEnv ();

    if (!(auths = d->authorizations))
	return;
    home = getEnv (verify->userEnviron, "HOME");
    if (!home)
	return;
    Debug ("RemoveUserAuthorization\n");
    strcpy (name, home);
    if (home[strlen(home) - 1] != '/')
	strcat (name, "/");
    strcat (name, ".Xauthority");
    Debug ("XauLockAuth %s\n", name);
    lockStatus = XauLockAuth (name, 1, 2, 10);
    Debug ("Lock is %d\n", lockStatus);
    if (lockStatus != LOCK_SUCCESS)
	return;
    if (openFiles (name, new_name, &old, &new))
    {
	initAddrs ();
	doWrite = 0;
	for (i = 0; i < d->authNum; i++)
	{
	    if (d->displayType.location == Local)
	    	writeLocalAuth (new, auths[i], d->name);
#ifdef XDMCP
	    else
	    	writeRemoteAuth (new, auths[i], d->peer, d->peerlen, d->name);
#endif
	}
	doWrite = 1;
	if (old) {
	    if (fstat (fileno (old), &statb) != -1)
		chmod (new_name, (int) (statb.st_mode & 0777));
	    /*SUPPRESS 560*/
	    while (entry = XauReadAuth (old)) {
		if (!checkEntry (entry))
		{
		    Debug ("Writing an entry\n");
		    writeAuth (new, entry);
		}
		XauDisposeAuth (entry);
	    }
	    fclose (old);
	}
	doneAddrs ();
	fclose (new);
	if (unlink (name) == -1)
	    Debug ("unlink %s failed\n", name);
	if (link (new_name, name) == -1) {
	    Debug ("link failed %s %s\n", new_name, name);
	    LogError ("Can't move authorization into place\n");
	} else {
	    Debug ("new is in place, go for it!\n");
	    unlink (new_name);
	}
    }
    XauUnlockAuth (name);
}
