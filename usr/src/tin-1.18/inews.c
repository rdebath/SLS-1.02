/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : inews.c
 *  Author    : I.Lea
 *  Created   : 17-03-92
 *  Updated   : 06-12-92
 *  Notes     : NNTP builtin version of inews
 *  Copyright : (c) Copyright 1991-92 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

#ifdef HAVE_NETDB_H
#	ifdef apollo
#		include	</bsd4.3/usr/include/netdb.h>
#	else
#		include	<netdb.h>
#	endif
#endif


int submit_inews (name)
	char *name;
{
	int	ret_code = FALSE;

#if !defined(INDEX_DAEMON) && !defined(XSPOOLDIR)

#ifdef NNTP_INEWS
	char	from_name[PATH_LEN];
	char	host_name[PATH_LEN];
	char	full_name[128];
	char	user_name[128];
	char	line[NNTP_STRLEN];
	FILE	*fp;
	int	len = 0;
	int	respcode;

	if ((fp = fopen (name, "r")) == NULL) {
		return (ret_code);
	}

	/*
	 * Send POST command to NNTP server
	 */
	put_server ("post");

	/*
	 * Receive CONT_POST or ERROR response code from NNTP server
	 */
	if ((respcode = get_respcode ()) != CONT_POST) {
		error_message ("%s", nntp_respcode (respcode));
		debug_nntp ("submit_inews", nntp_respcode (respcode));
		return (ret_code);
	}

	get_host_name (host_name);	
	get_user_info (user_name, full_name);
	get_from_name (user_name, host_name, full_name, from_name);
	
	/*
	 * Send Path: and From: article headers
	 */
#ifdef NNTP_INEWS_GATEWAY
	if (*(NNTP_INEWS_GATEWAY)) {
		sprintf (line, "Path: %s", user_name);
	} else {	
		sprintf (line, "Path: %s!%s", host_name, user_name);
	}
#else
	sprintf (line, "Path: %s!%s", host_name, user_name);
#endif	
	put_server (line);
	sprintf (line, "From: %s", from_name);
	put_server (line);

	/*
	 * Send article 1 line at a time ending with "."
	 */
	while (fgets (line, sizeof (line), fp) != NULL) {
		len = strlen (line);
		line[len-1] = '\0';
		fprintf (nntp_wr_fp, "%s\r\n", line);
	}
	
	put_server (".");

	/*
	 * Receive OK_POSTED or ERROR response code from NNTP server
	 */
	if ((respcode = get_respcode ()) != OK_POSTED) {
		error_message ("%s", nntp_respcode (respcode));
		debug_nntp ("submit_inews", nntp_respcode (respcode));
		return (ret_code);
  	}
  	
	ret_code = TRUE;

#endif /* NNTP_ABLE */

#endif /* INDEX_DAEMON */

	return (ret_code);
}

/*
 * Find real hostname / substitute hostname if news gateway name 
 */
 
void get_host_name (host_name)
	char *host_name;
{
#ifndef INDEX_DAEMON

	char *ptr, host[PATH_LEN];
	char nntp_inews_gateway[PATH_LEN];
	FILE *fp;

	host_name[0] = '\0';
	nntp_inews_gateway[0] = '\0';
	
#ifdef NNTP_INEWS_GATEWAY
	if (*(NNTP_INEWS_GATEWAY)) {
		strcpy (nntp_inews_gateway, NNTP_INEWS_GATEWAY);
	}
#endif
	
	if (nntp_inews_gateway[0]) {
		/*
		 * If 1st letter is '$' read gateway name from shell variable
		 */
		if (nntp_inews_gateway[0] == '$' && nntp_inews_gateway[1]) {
			ptr = (char *) getenv (&nntp_inews_gateway[1]);
			if (ptr != (char *) 0) {
				strncpy (nntp_inews_gateway, ptr, sizeof (nntp_inews_gateway));
			}
		}
		/*
		 * If 1st letter is '/' read gateway name from specified file
		 */
		if (nntp_inews_gateway[0] == '/') {
			if ((fp = fopen (nntp_inews_gateway, "r")) != (FILE *) 0) {
				if (fgets (host, sizeof (host), fp) != (char *) 0) {
					strcpy (host_name, host);
					ptr = (char *) strchr (host_name, '\n');
					if (ptr != (char *) 0) {
						*ptr = '\0';
					}
				}	
				fclose (fp);
			}
			if (! host_name[0]) {
				strcpy (host_name, "PROBLEM_WITH_INEWS_GATEWAY_FILE");
			}
		} else {
			strcpy (host_name, nntp_inews_gateway);
		}	
	} else {
#		ifdef HAVE_GETHOSTBYNAME
		{
			struct hostent *host_entry;

			gethostname (host, sizeof (host)); 
			host_entry = gethostbyname (host);
			my_strncpy (host, host_entry->h_name, sizeof (host)); 
		}	
#		else	
#			ifdef AMIGA
			my_strncpy (host, get_val ("NodeName", "PROBLEM_WITH_NODE_NAME"), sizeof (host)); 
#			else
		{
			struct utsname uts_name;

			uname (&uts_name);
			my_strncpy (host, uts_name.nodename, sizeof (host));
		}
#			endif
#		endif

		strcpy (host_name, host);
	}	
	
#endif /* INDEX_DAEMON */
}

/*
 * Find username & fullname
 */

void get_user_info (user_name, full_name)
	char *user_name;
	char *full_name;
{
#ifndef INDEX_DAEMON

	char *ptr;
	
#ifdef AMIGA
	ptr = (char *) get_val ("REALNAME", "Unknown");
	my_strncpy (full_name, ptr, 128);
	strcpy (user_name, userid);
#else		
	if ((ptr = (char *) getenv ("NAME")) != (char *) 0) {
		my_strncpy (full_name, ptr, 128);
	} else {	
		my_strncpy (full_name, myentry->pw_gecos, 128);
		ptr = (char *) strchr (full_name, ',');
		if (ptr != (char *) 0) {
			*ptr = '\0';			
		}
	}
	ptr = get_val ("USER", userid);
	my_strncpy (user_name, ptr, 128);
#endif

#endif /* INDEX_DAEMON */
}

/*
 * Find full From: name in 'user@host (name)' format
 */

void get_from_name (user_name, host_name, full_name, from_name)
	char *user_name;
	char *host_name;
	char *full_name;
	char *from_name;
{
#ifndef INDEX_DAEMON

	char domain[PATH_LEN];
	char nntp_inews_domain[PATH_LEN];
	char *ptr;
	FILE *fp;
	
	domain[0] = '\0';
	nntp_inews_domain[0] = '\0';

#ifdef NNTP_INEWS_DOMAIN
	if (*(NNTP_INEWS_DOMAIN)) {
		strcpy (nntp_inews_domain, NNTP_INEWS_DOMAIN);
	}
#endif

	if (nntp_inews_domain[0]) {
		/*
		 * If 1st letter is '$' read domain name from shell variable
		 */
		if (nntp_inews_domain[0] == '$' && nntp_inews_domain[1]) {
			ptr = (char *) getenv (&nntp_inews_domain[1]);
			if (ptr != (char *) 0) {
				strncpy (nntp_inews_domain, ptr, sizeof (nntp_inews_domain));
			}
		}
		/*
		 * If 1st letter is '/' read domain name from specified file
		 */
		if (nntp_inews_domain[0] == '/') {
			if ((fp = fopen (nntp_inews_domain, "r")) != (FILE *) 0) {
				if (fgets (domain, sizeof (domain), fp) != (char *) 0) {
					ptr = (char *) strchr (domain, '\n');
					if (ptr != (char *) 0) {
						*ptr = '\0';
					}
				}
				fclose (fp);
			}
			if (! domain[0]) {
				strcpy (domain, "PROBLEM_WITH_INEWS_DOMAIN_FILE");
			}
		} else {
			my_strncpy (domain, nntp_inews_domain, sizeof (domain));
		}
	
		if (domain[0] == '.') {
			sprintf (from_name, "%s@%s%s (%s)",
				user_name, host_name, domain, full_name);
		} else {	
			sprintf (from_name, "%s@%s (%s)", user_name, domain, full_name);
		}		
	} else {	
		if (host_name[0] == '%') {
			sprintf (from_name, "%s%s (%s)", user_name, host_name, full_name);
		} else {
			sprintf (from_name, "%s@%s (%s)", user_name, host_name, full_name);
		}
	}

	if (debug == 2) {
		sprintf (msg, "FROM=[%s] USER=[%s] HOST=[%s] NAME=[%s]", 
			from_name, user_name, host_name, full_name);
		error_message (msg, "");
	}	

#endif /* INDEX_DAEMON */
}
