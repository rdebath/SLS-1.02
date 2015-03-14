/*****************************************************************************\
*									      *
*	File:     auth_clnt.c						      *
*	Author:   Don Becker						      *
*	Created:  Fri Jun  9 17:31:20 1989				      *
*	Contents: Client authentication routines for the NFS server	      *
*		Someday this should be update to encompass explicit Kerberos  *
*		authentication, in addition to the currently implemented      *
*		RPC-level scheme and the new "secure RPC" authentication.     *
*									      *
******************************************************************************/

/* extended to handle hostname patterns and enforce pathnames - rick sladkey */

#include <netdb.h>
#include <syslog.h>
#include <arpa/inet.h>
#include "nfsd.h"
#include "libc.h"
#include "auth.h"

extern clnt_param *clients, *unknown_clients, *default_client;

extern int hostmatch(struct hostent *hent, char *pattern);
extern int pathmatch(char *path, char *pattern);

clnt_param *
auth_clnt(struct svc_req *rqstp, char *path)
{
    clnt_param	**cpp, *cp, *ncp;
    long addr = svc_getcaller(rqstp->rq_xprt)->sin_addr.s_addr;
    int known_client = 0;
    
    /* find host parameter struct */
    for (cpp = &clients; *cpp != NULL; ) {
	if ((*cpp)->clnt_addr.s_addr == addr) {
	    cp = *cpp;
	    if (cp != clients) {
		/* Move to front */
		*cpp = cp->next;
		cp->next = clients;
		clients = cp;
	    }
	    if (path == NULL || pathmatch(path, cp->mount_point))
		goto found_it;
	    known_client = 1;
	}
	else
	    cpp = &((*cpp)->next);
    }
    
    /* Check the list of patterns or clients we didn't know at start-up. */
    for (cpp = &unknown_clients; (cp = *cpp) != NULL; ) {
	struct hostent *hent;
	
	if (!pathmatch(path, cp->mount_point)) {
	    cpp = &(cp->next);		/* The normal iteration. */
	    continue;
	}
	if (cp->clnt_name == NULL) {
	    if ((ncp = malloc(sizeof *ncp)) == NULL)
		mallocfailed();
	    *ncp = *cp;
	    ncp->clnt_addr = svc_getcaller(rqstp->rq_xprt)->sin_addr;
	    ncp->next = clients;
	    clients = ncp;
#ifdef AUTH_DEBUG
	    syslog(LOG_DAEMON|LOG_DEBUG,
		"anonymous NFS request from %s",
		inet_ntoa(*(struct in_addr *)&addr));
#endif
	    goto found_it;
	}
	else if (strchr(cp->clnt_name, '*') || strchr(cp->clnt_name, '?')) {
	    if ((hent = gethostbyaddr((char *) &addr,  sizeof addr, AF_INET))
		&& hostmatch(hent, cp->clnt_name)) {
		/* resolved client entry inherits properties from pattern */
		if ((ncp = malloc(sizeof *ncp)) == NULL)
			mallocfailed();
		*ncp = *cp;
		ncp->clnt_addr = *((struct in_addr *)hent->h_addr);
		ncp->clnt_name = strdup(hent->h_name);
		ncp->next = clients;
		clients = ncp;
#ifdef AUTH_DEBUG
		syslog(LOG_DAEMON|LOG_DEBUG,
			"requestor %s matched pattern %s",
			ncp->clnt_name, cp->clnt_name);
#endif
		goto found_it;
	    }
	    cpp = &(cp->next);		/* The normal iteration. */
	}
	else if ((hent = gethostbyname(cp->clnt_name)) != NULL) {
	    cp->clnt_addr = *((struct in_addr *)hent->h_addr);

	    *cpp = cp->next;		/* Remove from unknown_clients list. */
	    cp->next = clients;		/* Add to the front of client list. */
	    clients = cp;
		
#ifdef AUTH_DEBUG
	    syslog(LOG_DAEMON|LOG_DEBUG,
		   "Found previously unknown host %s.", cp->clnt_name);
#endif
	    if (cp->clnt_addr.s_addr == addr)
		goto found_it;
	} else
	    cpp = &(cp->next);		/* The normal iteration. */
    }
    
    if (default_client != NULL && pathmatch(path, default_client->mount_point))
	cp = default_client;
    else if (!known_client) {
	syslog(LOG_DAEMON|LOG_CRIT, "Access attempt by unknown client: %s", 
	       inet_ntoa(*(struct in_addr *)&addr));
	return NULL;
    }
    else
	return NULL;
 found_it:
    /* check request originated on a privileged port */
    if (!allow_non_root && cp->o.secure_port &&
	ntohs(svc_getcaller(rqstp->rq_xprt)->sin_port) >= IPPORT_RESERVED) {
	syslog(LOG_DAEMON|LOG_CRIT,
	       "NFS request from %s originated on insecure port, "
	       "psychoanalysis suggested",
	       inet_ntoa(*(struct in_addr *)&addr));
	return NULL;
    }
    return cp;
}

int
hostmatch(struct hostent *hent, char *pattern)
{
    const char *name = hent->h_name;
    int seen_dot = 0;

#ifdef AUTH_DEBUG
	syslog(LOG_DAEMON|LOG_DEBUG, "host matching %s to %s", name, pattern);
#endif
    for (;;) {
	if (*name == '\0' || *pattern == '\0')
	    return *name == *pattern;
	switch (*pattern) {
	case '*':
	    while (*name != '.' && *name != '\0')
		name++;
	    seen_dot = 1;
	    pattern++;
	    break;
	case '?':
	    if (*name == '.')
		return 0;
	    name++;
	    pattern++;
	    break;
	default:
	    if (seen_dot) {
		if (tolower(*name) != tolower(*pattern))
		    return 0;
	    }
	    else if (*name != *pattern)
		return 0;
	    if (*pattern == '.')
		seen_dot = 1;
	    name++;
	    pattern++;
	    break;
	}
    }
}

int
pathmatch(char *path, char *pattern)
{
	int len, c;

	if (!path || !pattern)
		return 1;
#ifdef AUTH_DEBUG
	syslog(LOG_DAEMON|LOG_DEBUG, "path matching %s to %s", path, pattern);
#endif
	len = strlen(pattern);
	if (strncmp(path, pattern, len) == 0
	    && ((len && pattern[len - 1] == '/')
		|| (c = path[len] == '/') || c == '\0'))
		return 1;
	return 0;
}

