/* $XConsortium: xhost.c,v 11.48 91/07/19 18:41:15 rws Exp $ */
 
/*

Copyright 1985, 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.
M.I.T. makes no representations about the suitability of
this software for any purpose.  It is provided "as is"
without express or implied warranty.

*/

/* sorry, streams support does not really work yet */
#if defined(STREAMSCONN) && defined(SVR4)
#undef STREAMSCONN
#define TCPCONN
#endif

#ifdef TCPCONN
#define NEEDSOCKETS
#endif
#ifdef UNIXCONN
#define NEEDSOCKETS
#endif
#ifdef DNETCONN
#define NEEDSOCKETS
#endif

#include <X11/Xlib.h>
#include <X11/Xos.h>
#include <X11/Xproto.h>
#include <X11/Xfuncs.h>
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <ctype.h>
#include <X11/Xauth.h>
#include <X11/Xmu/Error.h>

#ifdef NEEDSOCKETS
#ifdef att
typedef unsigned short unsign16;
typedef unsigned long unsign32;
typedef short sign16;
typedef long sign32;
#include <interlan/socket.h>
#include <interlan/netdb.h>
#include <interlan/in.h>
#else
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#endif
#endif /* NEEDSOCKETS */

#ifdef notdef
#include <arpa/inet.h>
	bogus definition of inet_makeaddr() in BSD 4.2 and Ultrix
#else
#ifndef hpux
extern unsigned long inet_makeaddr();
#endif
#endif
#ifdef DNETCONN
#include <netdnet/dn.h>
#include <netdnet/dnetdb.h>
#endif
#ifdef STREAMSCONN
#include <Xstreams.h>
extern char _XsTypeOfStream[];
#endif /* STREAMSCONN */

#ifdef SECURE_RPC
#include <pwd.h>
#include <rpc/rpc.h>
#ifndef X_NOT_POSIX
#ifdef _POSIX_SOURCE
#include <limits.h>
#else
#define _POSIX_SOURCE
#include <limits.h>
#undef _POSIX_SOURCE
#endif
#endif
#ifndef NGROUPS_MAX
#include <sys/param.h>
#define NGROUPS_MAX NGROUPS
#endif
#endif
 
static int local_xerror();
static char *get_hostname();
#ifdef STREAMSCONN
static char *get_streams_hostname();
static Bool get_streams_address();
#endif

#ifdef SIGNALRETURNSINT
#define signal_t int
#else
#define signal_t void
#endif
static signal_t nameserver_lost();

#define NAMESERVER_TIMEOUT 5	/* time to wait for nameserver */

int nameserver_timedout;
 
char *ProgramName;

#ifdef NEEDSOCKETS
static int XFamily(af)
    int af;
{
    int i;
    static struct _familyMap {
	int af, xf;
    } familyMap[] = {
#ifdef	AF_DECnet
        { AF_DECnet, FamilyDECnet },
#endif
#ifdef	AF_CHAOS
        { AF_CHAOS, FamilyChaos },
#endif
#ifdef	AF_INET
        { AF_INET, FamilyInternet },
#endif
};

#define FAMILIES ((sizeof familyMap)/(sizeof familyMap[0]))

    for (i = 0; i < FAMILIES; i++)
	if (familyMap[i].af == af) return familyMap[i].xf;
    return -1;
}
#endif /* NEEDSOCKETS */

Display *dpy;

main(argc, argv)
	int argc;
	char **argv;
{
	register char *arg;
	int i, nhosts;
	char *hostname;
	XHostAddress *list;
	Bool enabled = False;
#ifdef DNETCONN
	char *dnet_htoa();
	struct nodeent *np;
	struct dn_naddr *nlist, dnaddr, *dnaddrp, *dnet_addr();
	char *cp;
#endif
 
	ProgramName = argv[0];

	if ((dpy = XOpenDisplay(NULL)) == NULL) {
	    fprintf(stderr, "%s:  unable to open display \"%s\"\n",
		    ProgramName, XDisplayName (NULL));
	    exit(1);
	}

	XSetErrorHandler(local_xerror);
 
 
	if (argc == 1) {
#ifdef DNETCONN
		setnodeent(1); /* keep the database accessed */
#endif
		sethostent(1); /* don't close the data base each time */
		list = XListHosts(dpy, &nhosts, &enabled);
		if (enabled)
		    printf ("access control enabled, only authorized clients can connect\n");
		else
		    printf ("access control disabled, clients can connect from any host\n");

		if (nhosts != 0) {
		    for (i = 0; i < nhosts; i++ )  {
		      hostname = get_hostname(&list[i]);
		      if (hostname) {
			  printf ("%s", hostname);
		      } else {
#ifdef STREAMSCONN
			  print_streams_hostnames (list, nhosts);
#else
			  printf ("<unknown address in family %d>",
				  list[i].family);
#endif
		      }
		      if (nameserver_timedout)
			printf("\t(no nameserver response within %d seconds)\n",
			        NAMESERVER_TIMEOUT);
		      else printf("\n");
		    }
		    free(list);
		    endhostent();
		}
		exit(0);
	}
 
	if (argc == 2 && !strcmp(argv[1], "-help")) {
	    fprintf(stderr, "usage: %s [[+-]hostname ...]\n", argv[0]);
	    exit(1);
	}

	for (i = 1; i < argc; i++) {
	    arg = argv[i];
	    if (*arg == '-') {
	    
	        if (!argv[i][1] && ((i+1) == argc)) {
		    printf ("access control enabled, only authorized clients can connect\n");
		    XEnableAccessControl(dpy);
		} else {
		    arg = argv[i][1]? &argv[i][1] : argv[++i];
		    if (!change_host (dpy, arg, False)) {
			fprintf (stderr, "%s:  bad hostname \"%s\"\n",
				 ProgramName, arg);
		    }
		}
	    } else {
	        if (*arg == '+' && !argv[i][1] && ((i+1) == argc)) {
		    printf ("access control disabled, clients can connect from any host\n");
		    XDisableAccessControl(dpy);
		} else {
		    if (*arg == '+') {
		      arg = argv[i][1]? &argv[i][1] : argv[++i];
		    }
		    if (!change_host (dpy, arg, True)) {
			fprintf (stderr, "%s:  bad hostname \"%s\"\n",
				 ProgramName, arg);
		    }
		}
	    }
	}
	XCloseDisplay (dpy);  /* does an XSync first */
	exit(0);
}

 

/*
 * change_host - edit the list of hosts that may connect to the server;
 * it parses DECnet names (expo::), Internet addresses (18.30.0.212), or
 * Internet names (expo.lcs.mit.edu); if 4.3bsd macro h_addr is defined
 * (from <netdb.h>), it will add or remove all addresses with the given
 * address.
 */

int change_host (dpy, name, add)
    Display *dpy;
    char *name;
    Bool add;
{
  struct hostent *hp;
  XHostAddress ha;
#ifdef NEEDSOCKETS
  static struct in_addr addr;	/* so we can point at it */
#endif
  char *cp;
#ifdef DNETCONN
  struct dn_naddr *dnaddrp;
  struct nodeent *np;
  static struct dn_naddr dnaddr;
#endif /* DNETCONN */
  static char *add_msg = "being added to access control list";
  static char *remove_msg = "being removed from access control list";

#ifdef DNETCONN
  if ((cp = index (name, ':')) && (*(cp + 1) == ':')) {
    *cp = '\0';
    ha.family = FamilyDECnet;
    if (dnaddrp = dnet_addr(name)) {
      dnaddr = *dnaddrp;
    } else {
      if ((np = getnodebyname (name)) == NULL) {
	  fprintf (stderr, "%s:  unble to get node name for \"%s::\"\n",
		   ProgramName, name);
	  return 0;
      }
      dnaddr.a_len = np->n_length;
      bcopy (np->n_addr, dnaddr.a_addr, np->n_length);
    }
    ha.length = sizeof(struct dn_naddr);
    ha.address = (char *)&dnaddr;
    if (add) {
	XAddHost (dpy, &ha);
	printf ("%s:: %s\n", name, add_msg);
    } else {
	XRemoveHost (dpy, &ha);
	printf ("%s:: %s\n", name, remove_msg);
    }
    return 1;
  }
#endif /* DNETCONN */
    /*
     * If it has an '@',  its a netname
     */
    if (cp = index(name, '@')) {
	char *netname = name;
#ifdef SECURE_RPC
	static char username[MAXNETNAMELEN];

	if (!cp[1]) {
	    struct passwd *pwd;
	    static char domainname[128];

	    *cp = '\0';
	    pwd = getpwnam(name);
	    if (!pwd) {
		fprintf(stderr, "no such user \"%s\"\n", name);
		return 0;
	    }
	    getdomainname(domainname, sizeof(domainname));
	    if (!user2netname(username, pwd->pw_uid, domainname)) {
		fprintf(stderr, "failed to get netname for \"%s\"\n", name);
		return 0;
	    }
	    netname = username;
	}
#endif
	ha.family = FamilyNetname;
	ha.length = strlen(netname);
	ha.address = netname;
	if (add)
	    XAddHost (dpy, &ha);
	else
	    XRemoveHost (dpy, &ha);
	if (netname != name)
	    printf ("%s@ (%s) %s\n", name, netname, add ? add_msg : remove_msg);
	else
	    printf ("%s %s\n", netname, add ? add_msg : remove_msg);
        return 1;
    }
#ifdef STREAMSCONN
  if (get_streams_address (name, &ha)) {
    if (add) {
	XAddHost (dpy, &ha);
	printf ("%s %s\n", name, add_msg);
    } else {
	XRemoveHost (dpy, &ha);
	printf ("%s %s\n", name, remove_msg);
    }
    return 1;
  }
#endif
#ifdef NEEDSOCKETS
  /*
   * First see if inet_addr() can grok the name; if so, then use it.
   */
  if ((addr.s_addr = inet_addr(name)) != -1) {
    ha.family = FamilyInternet;
    ha.length = sizeof(addr.s_addr);
    ha.address = (char *)&addr.s_addr;
    if (add) {
	XAddHost (dpy, &ha);
	printf ("%s %s\n", name, add_msg);
    } else {
	XRemoveHost (dpy, &ha);
	printf ("%s %s\n", name, remove_msg);
    }
    return 1;
  } 
  /*
   * Is it in the namespace?
   */
  else if (((hp = gethostbyname(name)) == (struct hostent *)NULL)
       || hp->h_addrtype != AF_INET) {
    return 0;
  } else {
    ha.family = XFamily(hp->h_addrtype);
    ha.length = hp->h_length;
#ifdef h_addr				/* new 4.3bsd version of gethostent */
    {
	char **list;

	/* iterate over the hosts */
	for (list = hp->h_addr_list; *list; list++) {
	    ha.address = *list;
	    if (add) {
		XAddHost (dpy, &ha);
	    } else {
		XRemoveHost (dpy, &ha);
	    }
	}
    }
#else
    ha.address = hp->h_addr;
    if (add) {
	XAddHost (dpy, &ha);
    } else {
	XRemoveHost (dpy, &ha);
    }
#endif
    printf ("%s %s\n", name, add ? add_msg : remove_msg);
    return 1;
  }
#endif /* NEEDSOCKETS */
}


/*
 * get_hostname - Given an internet address, return a name (CHARON.MIT.EDU)
 * or a string representing the address (18.58.0.13) if the name cannot
 * be found.
 */

jmp_buf env;

static char *get_hostname (ha)
    XHostAddress *ha;
{
#ifdef TCPCONN
  struct hostent *hp = NULL;
  char *inet_ntoa();
#endif
#ifdef DNETCONN
  struct nodeent *np;
  static char nodeaddr[16];
#endif /* DNETCONN */

#ifdef TCPCONN
  if (ha->family == FamilyInternet) {
    /* gethostbyaddr can take a LONG time if the host does not exist.
       Assume that if it does not respond in NAMESERVER_TIMEOUT seconds
       that something is wrong and do not make the user wait.
       gethostbyaddr will continue after a signal, so we have to
       jump out of it. 
       */
    nameserver_timedout = 0;
    signal(SIGALRM, nameserver_lost);
    alarm(4);
    if (setjmp(env) == 0) {
      hp = gethostbyaddr (ha->address, ha->length, AF_INET);
    }
    alarm(0);
    if (hp)
      return (hp->h_name);
    else return (inet_ntoa(*((struct in_addr *)(ha->address))));
  }
#endif
  if (ha->family == FamilyNetname) {
    static char netname[512];
    int len;
#ifdef SECURE_RPC
    int uid, gid, gidlen, gidlist[NGROUPS_MAX];
#endif

    if (ha->length < sizeof(netname) - 1)
        len = ha->length;
    else
        len = sizeof(netname) - 1;
    bcopy(ha->address, netname, len);
    netname[len] = '\0';
#ifdef SECURE_RPC
    if (netname2user(netname, &uid, &gid, &gidlen, gidlist)) {
	struct passwd *pwd;
	char *cp;

	pwd = getpwuid(uid);
	if (pwd)
	    sprintf(netname, "%s@ (%*.*s)", pwd->pw_name,
		    ha->length, ha->length, ha->address);
    }
#endif
    return (netname);
  }
#ifdef DNETCONN
  if (ha->family == FamilyDECnet) {
    if (np = getnodebyaddr(ha->address, ha->length, AF_DECnet)) {
      sprintf(nodeaddr, "%s::", np->n_name);
    } else {
      sprintf(nodeaddr, "%s::", dnet_htoa(ha->address));
    }
    return(nodeaddr);
  }
#endif
#ifdef STREAMSCONN
  return get_streams_hostname (ha);
#else
  return (NULL);
#endif
}

static signal_t nameserver_lost()
{
  nameserver_timedout = 1;
  longjmp(env, -1);
}

/*
 * local_xerror - local non-fatal error handling routine. If the error was
 * that an X_GetHosts request for an unknown address format was received, just
 * return, otherwise print the normal error message and continue.
 */
static int local_xerror (dpy, rep)
    Display *dpy;
    XErrorEvent *rep;
{
    if ((rep->error_code == BadAccess) && (rep->request_code == X_ChangeHosts)) {
	fprintf (stderr, 
		 "%s:  must be on local machine to add or remove hosts.\n",
		 ProgramName);
	return 1;
    } else if ((rep->error_code == BadAccess) && 
	       (rep->request_code == X_SetAccessControl)) {
	fprintf (stderr, 
	"%s:  must be on local machine to enable or disable access control.\n",
		 ProgramName);
	return 1;
    } else if ((rep->error_code == BadValue) && 
	       (rep->request_code == X_ListHosts)) {
	return 1;
    }

    XmuPrintDefaultErrorMessage (dpy, rep, stderr);
    return 0;
}


#ifdef STREAMSCONN
static Bool get_streams_address (name, hap) 
    char *name;
    XHostAddress *hap;
{
  static char buf[128];
  char	 *ptr, *packet, *retptr, pktbuf[128];
  int	 n;


  if(_XsTypeOfStream[ConnectionNumber(dpy)]  == X_LOCAL_STREAM)
  {
	hap->family = FamilyUname;
	hap->length = strlen(name) +1;
	hap->address = name;
	return True;
  }

  packet = pktbuf;
  ptr = &packet[2*sizeof(int)];

  n = strlen(name) + 1;
  ((xHostEntry *) ptr)->length = n;
  ptr += sizeof(xHostEntry);
  memcpy(ptr, name, n);

  retptr = packet;
   *(int *) retptr = n+sizeof(xHostEntry);
   *(int *) (retptr + sizeof(int)) = 1;

  if(GetNetworkInfo (ConnectionNumber(dpy), NULL, ConvertNameToNetAddr, &packet, &retptr, NULL)<0)
           {
		return False;
           }
   hap->family = ((xHostEntry *) retptr)->family;
   hap->length = ((xHostEntry *) retptr)->length;
   hap->address = buf;
  
   if(hap->length > 127)
   	hap->length = 127;

   /* trim internet address to four */
   if (hap->family == FamilyInternet)
	hap->length = 4;

   ptr = &retptr[sizeof(xHostEntry)];
   memcpy(buf, ptr, hap->length);
   buf[hap->length] = '\0';
   return True;
}


print_streams_hostnames (list, nhosts)
    XHostAddress *list;
    int nhosts;
{
    int  m, n, i;
    char *ptr, *retptr;
    static char *packet = NULL;
    static int buflen = 0;
 
    if(buflen == 0)
		buflen = 512;

    m = 2 * sizeof(int);
    packet = (char *) malloc (buflen);
    if(packet == NULL){
	fprintf(stderr, "Cannot malloc %d chars \n", buflen);
	return;
	}
    ptr = &packet[m];

    for (i=0; i< nhosts; i++)
    {
	n = (((list[i].length + 3) >> 2) << 2) + sizeof(xHostEntry);
	m += n;
	if(m > buflen){
		buflen = m + 128;
		packet = (char *) realloc(packet, buflen);
    		if(packet == NULL){
			fprintf(stderr, "Cannot realloc %d chars \n", buflen);
			return;
			}
		}
	ptr = &packet[m - n];
	((xHostEntry *) ptr)->length  = list[i].length;
	((xHostEntry *) ptr)->family  = list[i].family;
	ptr += sizeof(xHostEntry);
	bcopy (list[i].address, ptr, list[i].length);
    }
    *(int *) packet = m;
    *(int *) (packet + sizeof(int)) = nhosts;
    if(_XsTypeOfStream[ConnectionNumber(dpy)] != X_LOCAL_STREAM){
    	n =
 GetNetworkInfo (ConnectionNumber(dpy), NULL,ConvertNetAddrToName, &packet, &retptr, &nhosts);
	if( n <= 0){
		fprintf(stderr, "No reply from the nameserver\n");
		return;
		}
	}
  	else retptr = &packet[2*sizeof(int)];
     m = 0;
     for(i=0; i<nhosts; i++){
	ptr = &retptr[m];
     	n = ((xHostEntry *) ptr)->length;
	n = (((n + 3) >> 2) << 2) + sizeof(xHostEntry);
	m += n;
     	ptr += sizeof(xHostEntry);
 	fprintf(stderr, "%s\n", ptr);	
 	}		
     free(retptr);
}

static char *get_streams_hostname (ha)
    XHostAddress *ha;
{
  static char buf[128];
  char	 *ptr, *packet, pktbuf[128], *retptr;
  int	 n, len;

   if(_XsTypeOfStream[ConnectionNumber(dpy)] == X_LOCAL_STREAM || ha->family == FamilyUname){
	return(ha->address);
  }

  packet = pktbuf;
  ptr = &packet[2*sizeof(int)];

  ((xHostEntry *) ptr)->length = ha->length;
  ((xHostEntry *) ptr)->family = ha->family;

  ptr += sizeof(xHostEntry);
  memcpy(ptr, ha->address, ha->length);

   retptr = packet;
   *(int *) retptr = ha->length+sizeof(xHostEntry);
   *(int *) (retptr + sizeof(int)) = 1;

  if(GetNetworkInfo (ConnectionNumber(dpy), NULL, ConvertNetAddrToName, &packet, &retptr, NULL)<0)
           {
		ha->address[ha->length] = '\0';
		return(ha->address);
           }
   ptr = &retptr[sizeof(xHostEntry)];
   len = ((xHostEntry *) retptr)->length;
   memcpy(buf, ptr, len);
   buf[len] = '\0';
   return(buf);
}

#endif /* STREAMSCONN */
