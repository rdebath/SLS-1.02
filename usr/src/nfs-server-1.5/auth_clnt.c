/*
 * auth		This module takes care of request authorization.
 *
 * Version:	@(#)auth_clnt.c	1.5	93/04/10
 *
 * Authors:	Don Becker, <becker@super.org>
 *		Rick Sladkey, <jrs@world.std.com>
 *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *
 *		This software maybe be used for any purpose provided
 *		the above copyright notice is retained.  It is supplied
 *		as is, with no warranty expressed or implied.
 */
#include "nfsd.h"


static _PRO( int hostmatch, (struct hostent *, char *)			);
static _PRO( int pathmatch, (char *, char *)				);


static int hostmatch(hent, pattern)
struct hostent *hent;
char *pattern;
{
  const char *name = hent->h_name;
  int seen_dot = 0;

#ifdef AUTH_DEBUG
  dprintf(1, "host matching %s to %s\n", name, pattern);
#endif

  for (;;) {
	if (*name == '\0' || *pattern == '\0') return(*name == *pattern);
	switch (*pattern) {
		case '*':
			while (*name != '.' && *name != '\0')
			name++;
			seen_dot = 1;
			pattern++;
			break;
		case '?':
			if (*name == '.') return(0);
			name++;
			pattern++;
			break;
		default:
			if (seen_dot) {
				if (tolower(*name) != tolower(*pattern))
								return(0);
			} else if (*name != *pattern) return(0);
			if (*pattern == '.') seen_dot = 1;
			name++;
			pattern++; break;
	}
  }
}


static int pathmatch(path, pattern)
char *path;
char *pattern;
{
  int len, c;

  if (!path || !pattern) return(1);
#ifdef AUTH_DEBUG
  dprintf(1, "path matching %s to %s\n", path, pattern);
#endif
  len = strlen(pattern);
  if (strncmp(path, pattern, len) == 0 &&
      ((len && pattern[len - 1] == '/') ||
       (c = path[len] == '/') || c == '\0')) return(1);
  return(0);
}


clnt_param *auth_clnt(struct svc_req *rqstp, char *path)
{
  clnt_param **cpp, *cp, *ncp;
  long addr = svc_getcaller(rqstp->rq_xprt)->sin_addr.s_addr;
  int known_client = 0;
	
  /* Find host parameter struct. */
  for (cpp = &clients; *cpp != NULL; ) {
	if ((*cpp)->clnt_addr.s_addr == addr) {
		cp = *cpp;
		if (cp != clients) {
			/* Move to front. */
			*cpp = cp->next;
			cp->next = clients;
			clients = cp;
		}
		if (path == NULL || pathmatch(path, cp->mount_point))
							goto found_it;
		known_client = 1;
	} else
		cpp = &((*cpp)->next);
  }
	
  /* Check the list of patterns or clients we didn't know at start-up. */
  for (cpp = &unknown_clients; (cp = *cpp) != NULL; ) {
	struct hostent *hent;

	if (!pathmatch(path, cp->mount_point)) {
		cpp = &(cp->next);		/* the normal iteration	*/
		continue;
	}
	if (cp->clnt_name == NULL) {
		if ((ncp = malloc(sizeof *ncp)) == NULL) mallocfailed();
		*ncp = *cp;
		ncp->clnt_addr = svc_getcaller(rqstp->rq_xprt)->sin_addr;
		ncp->next = clients;
		clients = ncp;
#ifdef AUTH_DEBUG
		dprintf(1, "anonymous NFS request from %s\n",
				inet_ntoa(*(struct in_addr *)&addr));
#endif
		goto found_it;
	} else {
		if (strchr(cp->clnt_name, '*') || strchr(cp->clnt_name, '?')) {
			hent = gethostbyaddr((char *) &addr, sizeof addr,
									AF_INET);
			if (hent && hostmatch(hent, cp->clnt_name)) {
				/*
				 * Resolved client entry inherits properties
				 * from pattern.
				 */
				if ((ncp = malloc(sizeof *ncp)) == NULL)
								mallocfailed();
				*ncp = *cp;
				ncp->clnt_addr = *((struct in_addr *)
								hent->h_addr);
				ncp->clnt_name = strdup(hent->h_name);
				ncp->next = clients;
				clients = ncp;
#ifdef AUTH_DEBUG
				dprintf(1, "requester %s matched pattern %s\n",
						ncp->clnt_name, cp->clnt_name);
#endif
				goto found_it;
			}
			cpp = &(cp->next);	/* the normal iteration	*/
		} else if ((hent = gethostbyname(cp->clnt_name)) != NULL) {
			cp->clnt_addr = *((struct in_addr *) hent->h_addr);

			/* Remove from "unknown clients" list. */
			*cpp = cp->next;

			/* Add to the front of "known clients" list. */
			cp->next = clients;
			clients = cp;
		
#ifdef AUTH_DEBUG
			dprintf(1, "Found previously unknown host %s.\n",
								cp->clnt_name);
#endif
			if (cp->clnt_addr.s_addr == addr)
							goto found_it;
		} else
			cpp = &(cp->next);	/* normal iteration	*/
	}
	
	if (default_client != NULL &&
	    pathmatch(path, default_client->mount_point)) {
		cp = default_client;
	} else if (!known_client) {
		dprintf(0, "Access attempt by unknown client: %s\n", 
	   			inet_ntoa(*(struct in_addr *)&addr));
		return(NULL);
	} else
		return(NULL);
  }

 found_it:
  /* Check request originated on a privileged port. */
  if (!allow_non_root && cp->o.secure_port &&
      ntohs(svc_getcaller(rqstp->rq_xprt)->sin_port) >= IPPORT_RESERVED) {
	dprintf(0, "NFS request from %s originated on insecure port, %s\n",
		inet_ntoa(*(struct in_addr *)&addr),
				"psychoanalysis suggested");
	return(NULL);
  }
  return(cp);
}
