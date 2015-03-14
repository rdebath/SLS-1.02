/*
 * xdm - display manager daemon
 *
 * $XConsortium: auth.c,v 1.17 89/12/14 09:42:18 rws Exp $
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

# include   "dm.h"
# include   <sys/signal.h>
# include   <setjmp.h>
# include   <sys/types.h>
# include   <sys/ioctl.h>
# include   <sys/socket.h>
# include   <sys/stat.h>
#ifdef hpux
# include   <sys/utsname.h>
# include   <netdb.h>
#else
# include    <net/if.h>
#endif
#ifdef TCPCONN
# include   <netinet/in.h>
#endif
#ifdef DNETCONN
# include    <netdnet/dn.h>
# include    <netdnet/dnetdb.h>
#endif
# include    <X11/X.h>

extern void	exit (), bcopy (), free ();
extern char	*mktemp ();

extern int	MitInitAuth ();
extern Xauth	*MitGetAuth ();

#ifdef HASDES
extern int	XdmInitAuth ();
extern Xauth	*XdmGetAuth ();
extern int	XdmGetXdmcpAuth ();
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
    MitInitAuth, MitGetAuth, NULL,
},
#ifdef HASDES
{ (unsigned short) 19,	"XDM-AUTHORIZATION-1",
    XdmInitAuth, XdmGetAuth, XdmGetXdmcpAuth,
}
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

Xauth *
GenerateAuthorization (name_length, name)
unsigned short	name_length;
char		*name;
{
    struct AuthProtocol	*a;
    Xauth   *auth = 0;

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
	    Debug ("Got 0x%x (%d %*.*s)\n", auth,
		auth->name_length, auth->name_length,
 		auth->name_length, auth->name);
	else
	    Debug ("Got (null)\n");
    }
    return auth;
}

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

SaveServerAuthorization (d, auth)
    struct display  *d;
    Xauth	    *auth;
{
    FILE	*auth_file;
    int		mask;
    int		ret;

    mask = umask (0077);
    (void) unlink (d->authFile);
    auth_file = fopen (d->authFile, "w");
    umask (mask);
    if (!auth_file) {
	LogError ("Cannot open server authorization file %s\n", d->authFile);
	ret = FALSE;
    }
    else
    {
    	Debug ("File: %s auth: %x\n", d->authFile, auth);
    	if (!XauWriteAuth (auth_file, auth) || fflush (auth_file) == EOF)
    	{
	    LogError ("Cannot write server authorization file %s\n",
		       d->authFile);
	    ret = FALSE;
    	}
	else
	    ret = TRUE;
	fclose (auth_file);
    }
    return ret;
}

SetLocalAuthorization (d)
    struct display	*d;
{
    Xauth	*auth;

    if (d->authorization)
    {
	XauDisposeAuth (d->authorization);
	d->authorization = (Xauth *) NULL;
    }
    if (d->authName && !d->authNameLen)
    	d->authNameLen = strlen (d->authName);
    auth = GenerateAuthorization (d->authNameLen, d->authName);
    if (!auth)
	return;
    if (SaveServerAuthorization (d, auth))
	d->authorization = auth;
    else
	XauDisposeAuth (auth);
}

SetAuthorization (d)
    struct display  *d;
{
    Xauth   *auth;

    auth = d->authorization;
    if (auth)
	XSetAuthorization (auth->name, (int) auth->name_length,
			   auth->data, (int) auth->data_length);
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

binaryEqual (a, b, len)
char	*a, *b;
unsigned short	len;
{
	while (len-- > 0)
		if (*a++ != *b++)
			return 0;
	return 1;
}

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
	struct addrList	*next;
};

static struct addrList	*addrs;

initAddrs ()
{
	addrs = 0;
}

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

saveAddr (family, address_length, address, number_length, number)
unsigned short	family;
unsigned short	address_length, number_length;
char	*address, *number;
{
	struct addrList	*new;
	char		*malloc ();

	if (checkAddr (family, address_length, address, number_length, number))
		return;
	new = (struct addrList *) malloc (sizeof (struct addrList));
	if (!new) {
		LogOutOfMem ("saveAddr");
		return;
	}
	if ((new->address_length = address_length) > 0) {
		new->address = malloc (address_length);
		if (!new->address) {
			LogOutOfMem ("saveAddr");
			free ((char *) new);
			return;
		}
		bcopy (address, new->address, (int) address_length);
	} else
		new->address = 0;
	if ((new->number_length = number_length) > 0) {
		new->number = malloc (number_length);
		if (!new->number) {
			LogOutOfMem ("saveAddr");
			free (new->address);
			free ((char *) new);
			return;
		}
		bcopy (number, new->number, (int) number_length);
	} else
		new->number = 0;
	new->family = family;
	new->next = addrs;
	addrs = new;
}

checkAddr (family, address_length, address, number_length, number)
unsigned short	family;
unsigned short	address_length, number_length;
char	*address, *number;
{
	struct addrList	*a;

	for (a = addrs; a; a = a->next) {
		if (a->family == family &&
		    a->address_length == address_length &&
 		    binaryEqual (a->address, address, address_length) &&
		    a->number_length == number_length &&
 		    binaryEqual (a->number, number, number_length))
		{
			return 1;
		}
	}
	return 0;
}

writeAuth (file, auth)
FILE	*file;
Xauth	*auth;
{
	saveAddr (auth->family, auth->address_length, auth->address,
				auth->number_length,  auth->number);
	XauWriteAuth (file, auth);
}

writeAddr (family, addr_length, addr, file, auth)
int	family;
int	addr_length;
char	*addr;
FILE	*file;
Xauth	*auth;
{
	Debug ("writeAddr\n");
	auth->family = (unsigned short) family;
	auth->address_length = addr_length;
	auth->address = addr;
	dumpAuth (auth);
	writeAuth (file, auth);
}

DefineLocal (file, auth)
FILE	*file;
Xauth	*auth;
{
	char	displayname[100];

	/* stolen from xinit.c */
#ifdef hpux
	/* Why not use gethostname()?  Well, at least on my system, I've had to
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
	gethostname(displayname, sizeof(displayname));
#endif
	writeAddr (FamilyLocal, strlen (displayname), displayname, file, auth);
}

/* code stolen from server/os/4.2bsd/access.c */

ConvertAddr (saddr, len, addr)
    register struct sockaddr	*saddr;
    int				*len;
    char			**addr;
{
    if (len == 0)
        return (0);
    switch (saddr->sa_family)
    {
      case AF_UNSPEC:
#ifndef hpux
      case AF_UNIX:
#endif
        return (0);
#ifdef TCPCONN
      case AF_INET:
        *len = sizeof (struct in_addr);
        *addr = (char *) &(((struct sockaddr_in *) saddr)->sin_addr);
        return (AF_INET);
#endif

#ifdef DNETCONN
      case AF_DECnet:
        *len = sizeof (struct dn_naddr);
        *addr = (char *) &(((struct sockaddr_dn *) saddr)->sdn_add);
        return (AF_DECnet);
#endif
      default:
        break;
    }
    return (-1);
}

#ifdef hpux
/* Define this host for access control.  Find all the hosts the OS knows about 
 * for this fd and add them to the selfhosts list.
 * HPUX version - hpux does not have SIOCGIFCONF ioctl;
 */
DefineSelf (fd, file, auth)
    int fd;
{
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

    /* Why not use gethostname()?  Well, at least on my system, I've had to
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
	if ( family > 0) {
	    writeAddr (FamilyInternet, sizeof (inetaddr->sin_addr),
			(char *) (&inetaddr->sin_addr), file, auth);
	}
    }
}

#else

/* Define this host for access control.  Find all the hosts the OS knows about 
 * for this fd and add them to the selfhosts list.
 */
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
        	if (ConvertAddr (&ifr->ifr_addr, &len, &addr) <= 0)
	    	    continue;
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
#endif /* hpux */

setAuthNumber (auth, name)
    Xauth   *auth;
    char    *name;
{
    char	*colon, *malloc ();
    char	*dot, *number;

    Debug ("setAuthNumber %s\n", name);
    colon = rindex (name, ':');
    if (colon) {
	++colon;
	if (dot = index (colon, '.'))
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

writeLocalAuth (file, auth, name)
FILE	*file;
Xauth	*auth;
char	*name;
{
    int	fd;
    Debug ("writeLocalAuth\n");
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
    
    if (!peer || peerlen < 2)
	return;
    setAuthNumber (auth, name);
    switch (ConvertAddr (peer, &peerlen, &addr))
    {
#ifdef AF_UNIX
    case AF_UNIX:
	family = FamilyLocal;
	break;
#endif
#ifdef AF_INET
    case AF_INET:
	family = FamilyInternet;
	break;
#endif
#ifdef AF_DECnet
    case AF_DECnet:
	family = FamilyDECnet;
	break;
#endif
#ifdef AF_CHAOS
    case AF_CHAOS:
	family = FamilyChaos;
	break;
#endif
    }
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
    Xauth	*entry, *auth;
    int	setenv;
    char	**setEnv (), *getEnv ();
    struct stat	statb;

    Debug ("SetUserAuthorization\n");
    if (auth = d->authorization) {
	home = getEnv (verify->userEnviron, "HOME");
	lockStatus = LOCK_ERROR;
	if (home) {
	    sprintf (home_name, "%s/.Xauthority", home);
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
	    mktemp (backup_name);
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
	if (d->displayType.location == Local)
	    writeLocalAuth (new, auth, d->name);
	else
	    writeRemoteAuth (new, auth, d->peer, d->peerlen, d->name);
	if (old) {
	    if (fstat (fileno (old), &statb) != -1)
		chmod (new_name, (int) (statb.st_mode & 0777));
	    while (entry = XauReadAuth (old)) {
		if (!checkAddr (entry->family,
			       entry->address_length, entry->address,
			       entry->number_length, entry->number))
		{
		    Debug ("Saving an entry\n");
		    dumpAuth (entry);
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
#ifdef NGROUPS
	    chown (envname, verify->uid, verify->groups[0]);
#else
	    chown (envname, verify->uid, verify->gid);
#endif
	}
    }
    Debug ("done SetUserAuthorization\n");
}
