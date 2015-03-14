/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : open.c
 *  Author    : I.Lea & R.Skrenta
 *  Created   : 01-04-91
 *  Updated   : 18-11-92
 *  Notes     : Routines to make reading news locally (ie. /usr/spool/news) 
 *              or via NNTP transparent
 *  Copyright : (c) Copyright 1991-92 by Iain Lea & Rich Skrenta
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

int nntp_codeno = 0;

#ifdef NNTP_ABLE
int compiled_with_nntp = TRUE;		/* used in mail_bug_report() info */
#else
int compiled_with_nntp = FALSE;
#endif

#ifdef NO_POSTING
int	can_post = FALSE;
#else
int	can_post = TRUE;
#endif

char *nntp_server;

void nntp_open ()
{
#ifdef NNTP_ABLE	
	int ret;

	if (read_news_via_nntp) {
		debug_nntp ("nntp_open", "BEGIN");

		nntp_server = getserverbyfile (NNTP_SERVER_FILE);

		if (nntp_server == (char *) 0) {
			error_message (txt_cannot_get_nntp_server_name, "");
			error_message (txt_server_name_in_file_env_var, NNTP_SERVER_FILE);
			exit (1);
		}

		if (update == FALSE) {
			sprintf (msg, txt_connecting, nntp_server);
			wait_message (msg);
		}
		
		debug_nntp ("nntp_open", nntp_server);

		ret = server_init (nntp_server, NNTP_TCP_NAME, NNTP_TCP_PORT);
		if (update == FALSE && ret != -1) {
			fputc ('\n', stdout);
		}

		debug_nntp_respcode (ret);

		switch (ret) {
		case OK_CANPOST:
#ifndef NO_POSTING		
			can_post = TRUE;
#endif			
			break;

		case OK_NOPOST:
			can_post = FALSE;
			break;	

		case -1:
			error_message (txt_failed_to_connect_to_server, nntp_server);
			exit (1);

		default:
			sprintf (msg, "%s: %s", progname, nntp_respcode (ret));
			error_message (msg, "");
			exit (1);
		}

		/*
		 * Check if NNTP supports my XINDEX & XUSER commands
		 */
#ifndef DONT_HAVE_NNTP_EXTS	 
		debug_nntp ("nntp_open", "xindex");
		put_server ("xindex");	
		if (get_respcode () != ERR_COMMAND) {
			xindex_supported = TRUE;
		}
		debug_nntp ("nntp_open", "xuser");
		put_server ("xuser");	
		if (get_respcode () != ERR_COMMAND) {
			xuser_supported = TRUE;
		}
#endif	/* DONT_HAVE_NNTP_EXTS */		
		
		/*
		 * Check if NNTP server expects user authorization
		 */
		authorization (nntp_server, userid);
		 
		/*
		 * If INN NNTP & XINDEX not supported switch to mode reader
		 */
		if (! xindex_supported) {
			debug_nntp ("nntp_open", "mode reader");
			put_server ("mode reader");	
			if (get_respcode () != ERR_COMMAND) {
				inn_nntp_server = TRUE;
			}
		}
	}
#ifndef DONT_HAVE_NNTP_EXTS
	/*
	 * Find out if NNTP supports SPOOLDIR command
	 */
	get_spooldir ();
#endif	/* DONT_HAVE_NNTP_EXTS */		

#endif	
}


void nntp_close ()
{
#ifdef NNTP_ABLE
	if (read_news_via_nntp) {
		debug_nntp ("nntp_close", "END");
		close_server ();
	}
#endif	
}

/*
 * Open the mail active file locally
 */

FILE *open_mail_active_fp (mode)
	char *mode;
{
	return fopen (mail_active_file, mode);
}

/*
 * Open the news active file locally or send the LIST command via NNTP
 */

FILE *open_news_active_fp ()
{
	int respcode;
	
	if (read_news_via_nntp) {
#ifdef NNTP_ABLE
		put_server ("list");
		if ((respcode = get_respcode ()) != OK_GROUPS) {
			debug_nntp ("open_news_active_fp", "NOT_OK");
			error_message ("%s", nntp_respcode (respcode));
			return (FILE *) 0;
		}
		debug_nntp ("open_news_active_fp", "OK");
		return nntp_to_fp ();
#else
		return (FILE *) 0;
#endif		
	} else {
		return fopen (news_active_file, "r");
	}
}

/*
 * Open the ~/.tin/active file locally or send the NEWGROUPS command via NNTP
 *
 * NEWGROUPS 311299 235959
 */

FILE *open_newgroups_fp (index)
	int index;
{
	char line[NNTP_STRLEN];
	
	if (read_news_via_nntp) {
#ifdef NNTP_ABLE
		if (index == -1) {
			return (FILE *) 0;
		}
		sprintf (line, "newgroups %s", active_size[index].attribute);
		debug_nntp ("open_newgroups_fp", line);
		put_server (line);
		if (get_respcode () != OK_NEWGROUPS) {
			debug_nntp ("open_newgroups_fp", "NOT_OK");
			return (FILE *) 0;
		}
		debug_nntp ("open_newgroups_fp", "OK");
		return nntp_to_fp ();
#else
		return (FILE *) 0;
#endif		
	} else {
		joinpath (line, rcdir, ACTIVE);
		return fopen (line, "r");
	}
}

/*
 * Open the news motd file locally or on the NNTP server
 *
 * XMOTD 311299 235959 [GMT]
 */
 
FILE *open_motd_fp (motd_file_date)
	char *motd_file_date; 
{
	char line[NNTP_STRLEN];
	
	if (read_news_via_nntp) {
#if defined(NNTP_ABLE) && !defined(DONT_HAVE_NNTP_EXTS)
		sprintf (line, "xmotd %s", motd_file_date);
		debug_nntp ("open_motd_fp", line);
		put_server (line);
		if (get_respcode () != OK_XMOTD) {
			debug_nntp ("open_motd_fp", "NOT_OK");
			return (FILE *) 0;
		}
		debug_nntp ("open_motd_fp", "OK");
		return nntp_to_fp ();
#else
		return (FILE *) 0;
#endif		
	} else {
		return fopen (motd_file, "r");
	}
}


FILE *open_subscription_fp ()
{
	if (read_news_via_nntp) {
#ifdef NNTP_ABLE
		put_server ("list subscriptions");
		if (get_respcode () != OK_GROUPS) {
			debug_nntp ("open_subscription_fp", "NOT_OK");
			return (FILE *) 0;
		}
		debug_nntp ("open_subscription_fp", "OK");
		return nntp_to_fp ();
#else
		return (FILE *) 0;
#endif		
	} else {
		return fopen (subscriptions_file, "r");
	}
}

/*
 *  Open mail groups description file.
 */
 
FILE *open_mailgroups_fp ()
{
	return fopen (mailgroups_file, "r");
}


/*
 * If reading via NNTP the newsgroups file will be saved to ~/.tin/newsgroups
 * so that any subsequent rereads on the active file will not have to waste
 * net bandwidth and the local copy of the newsgroups file can be accessed.
 */
 
FILE *open_newsgroups_fp ()
{
	if (read_news_via_nntp) {
#ifdef NNTP_ABLE
		if (read_local_newsgroups_file) {
			debug_nntp ("open_newsgroups_fp", "Using local copy of newsgroups file");
			return fopen (local_newsgroups_file, "r");
		} else {
			put_server ("list newsgroups");
			if (get_respcode () != OK_GROUPS) {
				debug_nntp ("open_newsgroups_fp", "NOT_OK");
				return (FILE *) 0;
			}
			debug_nntp ("open_newsgroups_fp", "OK");
			return nntp_to_fp ();
		}
#else
		return (FILE *) 0;
#endif		
	} else {
		return fopen (newsgroups_file, "r");
	}
}

/*
 * Open a group index file
 */
 
FILE *open_index_fp (group_name)
	char *group_name;
{
	extern char index_file[PATH_LEN];
	char line[NNTP_STRLEN];
	int group_type;
	
	group_type = find_index_file (group_name);
	if (group_type == -1) {
		return (FILE *) 0;
	}

	if (debug == 2) {
		error_message ("INDEX file=[%s]", index_file);
	}
	
	if (read_news_via_nntp && xindex_supported && 
	    group_type == GROUP_TYPE_NEWS) {
#ifdef NNTP_ABLE
		sprintf (line, "xindex %s", group_name);
		debug_nntp ("open_index_fp", line);
		put_server (line);
		if (get_respcode () != OK_XINDEX) {
			debug_nntp ("open_index_fp", "NOT_OK");
			return (FILE *) 0;
		}
		debug_nntp ("open_index_fp", "OK");
		return nntp_to_fp ();
#else
		return (FILE *) 0;
#endif
	} else {
		return fopen (index_file, "r");
	}
}

/*
 * Stat a mail/news article to see if it still exists
 */
 
int stat_article (art, group_path)
	long art;
	char *group_path;
{
	char buf[NNTP_STRLEN];
	int i, respcode;
	int art_exists = TRUE;
	struct stat sb;

	i = my_group[cur_groupnum];
	
	if (read_news_via_nntp && active[i].type == GROUP_TYPE_NEWS) {
#ifdef NNTP_ABLE
		sprintf (buf, "stat %ld", art);
		debug_nntp ("stat_article", buf);
		put_server (buf);
		if ((respcode = get_respcode ()) != OK_NOTEXT) {
			art_exists = FALSE;
		}
#endif
	} else {
		joinpath (buf, active[i].spooldir, group_path);
		sprintf (&buf[strlen (buf)], "/%ld", art);

		if (stat (buf, &sb) == -1) {
			art_exists = FALSE;
		}
	}

	return art_exists;
}

/*
 * Open a mail/news article
 */
 
FILE *open_art_fp (group_path, art)
	char *group_path;
	long art;
{
	char buf[NNTP_STRLEN];
	int i, respcode;
	struct stat sb;
	extern long note_size;

	i = my_group[cur_groupnum];
	
	if (read_news_via_nntp && active[i].type == GROUP_TYPE_NEWS) {
#ifdef NNTP_ABLE
		sprintf (buf, "article %ld", art);
		debug_nntp ("open_art_fp", buf);
		put_server (buf);
		if ((respcode = get_respcode ()) != OK_ARTICLE) {
			if (debug == 2) {
				error_message ("%s", nntp_respcode (respcode));
			}
			debug_nntp ("open_art_fp", "NOT OK");
			return (FILE *) 0;
		}

		debug_nntp ("open_art_fp", "OK");

		return nntp_to_fp ();
#else
		return (FILE *) 0;
#endif
	} else {
		joinpath (buf, active[i].spooldir, group_path);
		sprintf (&buf[strlen (buf)], "/%ld", art);

		if (debug == 2) {
			error_message ("ART=[%s]", buf);
		}

		if (stat (buf, &sb) == -1) {
			note_size = 0;
		} else {
			note_size = sb.st_size;
		}
		return fopen (buf, "r");
	}
}


FILE *open_header_fp (art)
	long art;
{
	char buf[NNTP_STRLEN];

	if (read_news_via_nntp &&
	    active[my_group[cur_groupnum]].type == GROUP_TYPE_NEWS) {
#ifdef NNTP_ABLE	
		sprintf(buf, "head %ld", art);
		
		debug_nntp ("open_header_fp", buf);

		put_server (buf);
		if (get_respcode () != OK_HEAD) {
			debug_nntp ("open_header_fp", "NOT_OK_HEAD");
			return (FILE *) 0;
		}

		debug_nntp ("open_header_fp", "OK_HEAD");

		return nntp_to_fp ();
#else
		return (FILE *) 0;
#endif		
	} else {
		sprintf (buf, "%ld", art);
		return fopen (buf, "r");
	}
}

/*
 *  Longword comparison routine for the qsort()
 */

int base_comp (p1, p2)
	char *p1;
	char *p2;
{
	long *a = (long *) p1;
	long *b = (long *) p2;

	if (*a < *b)
		return -1;
	if (*a > *b)
		return 1;
	return 0;
}


/*
 *  Read the article numbers existing in a group's spool directory
 *  into base[] and sort them.  top_base is one past top.
 */

void setup_base (group, group_path)
	char *group;
	char *group_path;
{
	char buf[NNTP_STRLEN];
#ifdef NNTP_ABLE
	char line[NNTP_STRLEN];
#endif
	DIR *d;
	DIR_BUF *e;
	int i;
	long art, start, last, dummy, count;

	top_base = 0;
		
	i = my_group[cur_groupnum];

	if (read_news_via_nntp && active[i].type == GROUP_TYPE_NEWS) {
#ifdef NNTP_ABLE
		sprintf (buf, "group %s", group);

		debug_nntp ("setup_base", buf);
		
		put_server (buf);

		if (get_server (line, NNTP_STRLEN) == -1) {
			error_message (txt_connection_to_server_broken, "");
			tin_done (1);
		}

		if (atoi(line) != OK_GROUP) {
			debug_nntp ("setup_base", "NOT_OK");
			return;
		}

		debug_nntp ("setup_base", line);

		sscanf (line,"%ld %ld %ld %ld", &dummy, &count, &start, &last);
		if (last - count > start) {
			start = last - count;
		}

		while (start <= last) {
			if (top_base >= max_art) {
				expand_art();
			}
			base[top_base++] = start++;
		}
#else
		return; 
#endif
	} else {
		joinpath (buf, active[i].spooldir, group_path);

		if (access (buf, 4) != 0) {
			return;
		}

		d = opendir (buf);
		if (d != NULL) {
			while ((e = readdir (d)) != NULL) {
				art = my_atol (e->d_name, (int) e->D_LENGTH);
				if (art >= 0) {
					if (top_base >= max_art)
						expand_art ();
					base[top_base++] = art;
				}
			}
			closedir (d);
			qsort ((char *) base, top_base, sizeof (long), base_comp);
		}
	}
}

/*
 *  get a response code from the server and return it to the caller
 */

int get_respcode ()
{
#ifdef NNTP_ABLE
	char line[NNTP_STRLEN];

	if (get_server (line, NNTP_STRLEN) == -1) {
		error_message (txt_connection_to_server_broken, "");
		tin_done (1);
	}

	debug_nntp ("get_respcode", line);
	
	return atoi (line);
#else
	return (0);
#endif
}


int stuff_nntp (fnam)
	char *fnam;
{
#ifdef NNTP_ABLE
	FILE *fp;
	char line[NNTP_STRLEN];
	extern char *mktemp ();
	struct stat sb;
	extern long note_size;

	sprintf (fnam, "%stin_nntpXXXXXX", TMPDIR);
	mktemp (fnam);

	if ((fp = fopen (fnam, "w")) == (FILE *) 0) {
		perror_message (txt_stuff_nntp_cannot_open, fnam);
		return FALSE;
	}

	while (1) {
		if (get_server (line, NNTP_STRLEN) == -1) {
			error_message (txt_connection_to_server_broken, "");
			tin_done (1);
		}

		debug_nntp ("stuff_nntp", line);
		
		if (strcmp (line, ".") == 0)
			break;			/* end of text */
		strcat (line, "\n");
		if (line[0] == '.')		/* reduce leading .'s */
			fputs (&line[1], fp);
		else
			fputs (line, fp);
	}
	fclose (fp);

	if (stat (fnam, &sb) < 0)
		note_size = 0;
	else
		note_size = sb.st_size;

	return TRUE;
#else
	return TRUE;
#endif
}


FILE *nntp_to_fp ()
{
#ifdef NNTP_ABLE
	char fnam[PATH_LEN];
	FILE *fp = (FILE *) 0;
	
	if (! stuff_nntp (fnam)) {
		debug_nntp ("nntp_to_fp", "! stuff_nntp()");
		return (FILE *) 0;
	}

	if ((fp = fopen (fnam, "r")) == (FILE *) 0) {
		perror_message (txt_nntp_to_fp_cannot_reopen, fnam);
		return (FILE *) 0;
	}
	
	unlink (fnam);
	return fp;
#else
	return (FILE *) 0;
#endif
}

/*
 * Log user info to local file or NNTP logfile
 */

void log_user ()
{
	char log_file[PATH_LEN];
	char buf[32], *ptr;
	char line[NNTP_STRLEN];
#ifndef DONT_LOG_USER
	FILE *fp;
	long epoch;
#endif
#ifndef AMIGA
	extern struct passwd *myentry;

	my_strncpy (buf, myentry->pw_gecos, sizeof (buf)-1);

	if (read_news_via_nntp && xuser_supported) {
		if ((ptr = (char *) strchr(buf, ','))) {
			*ptr = '\0';
		}
		sprintf (line, "xuser %s (%s)", myentry->pw_name, buf);

		debug_nntp ("log_user", line);
		put_server (line);
	} else
#endif	/* AMIGA */
	{
#ifndef DONT_LOG_USER
		joinpath (log_file, TMPDIR, LOG_USER_FILE);
		
		if ((fp = fopen (log_file, "a+")) != (FILE *) 0) {
			time (&epoch);
			fprintf (fp, "%s%d: %-32s (%-8s) %s", 
				VERSION, PATCHLEVEL, buf, 
#ifdef AMIGA
				get_val ("USERNAME", "Unknown"),
#else
				myentry->pw_name, 
#endif
				ctime (&epoch));
			fclose (fp);
			chmod (log_file, 0666);
		}	
#endif	/* DONT_LOG_USER */
	}
}

/*
 * NNTP user authorization. Password read from ~/.newsauth
 * The ~/.newsauth authorization file has the format:  
 *   nntpserver1 password
 *   nntpserver2 password
 *   etc.
 */
 
void authorization (server, authuser)
	char *server;
	char *authuser;
{
	char authfile[PATH_LEN];
	char authpass[PATH_LEN];
	char line[NNTP_STRLEN];
	char buf[PATH_LEN], *ptr;
	int found = FALSE;
	FILE *fp;

	/*
	 * Check if running via NNTP
	 */
	if (! read_news_via_nntp) {
		return;
	}

	/*
	 * Lets check if the NNTP supports authorization
	 */
	debug_nntp ("authorization", "authinfo");
	put_server ("authinfo");
	if (get_respcode () == ERR_COMMAND) {
		return;
	}

	joinpath (authfile, homedir, ".newsauth");

	if ((fp = fopen (authfile,"r")) != (FILE *) 0) {
		/*
		 * Search through authorization file for correct NNTP server
		 * File has format:  'nntp-server' 'password'
		 */
		while (fgets (buf, sizeof (buf), fp) != (char *) 0) {
			/*
			 * Get server from 1st part of the line
			 */
			strcpy (line, buf); 
			ptr = (char *) strchr (line, ' ');
			if (ptr != (char *) 0) {
				*ptr = '\0';
			}

			if (strncmp (line, server, sizeof (server)) == 0) {
				/*
				 * Get passwdord from 2nd part of the line
				 */
				ptr = (char *) strrchr (buf, ' ');
				if (ptr != (char *) 0 && ++ptr != (char *) 0) {
					strcpy (authpass, ptr); 
					ptr = (char *) strchr (authpass, '\n');
					if (ptr != (char *) 0) {
						*ptr = '\0';
					}
					found = TRUE;
				}
				break;
			}
		}
		fclose (fp); 

		if (! found) {
			error_message (txt_nntp_authorization_failed, authuser);
		} else {
			sprintf (line, "authinfo user %s", authuser);
			put_server (line);
			get_respcode ();

			sprintf (line, "authinfo pass %s", authpass);
			put_server (line);
			get_respcode ();
		}
	}
}

/*
 * NNTP strings for get_respcode()
 */

char *nntp_respcode (respcode)
	int respcode;
{
#ifdef NNTP_ABLE

	static char *text;
	
	switch (respcode) {
		case 0:
			text = "";
			break;
		case INF_HELP:
			text = "100  Help text on way";
			break;
		case INF_AUTH:
			text = "180  Authorization capabilities";
			break;
		case INF_DEBUG:
			text = "199  Debug output";
			break;
		case OK_CANPOST:
			text = "200  Hello; you can post";
			break;
		case OK_NOPOST:
			text = "201  Hello; you can't post";
			break;
		case OK_SLAVE:
			text = "202  Slave status noted";
			break;
		case OK_GOODBYE:
			text = "205  Closing connection";
			break;
		case OK_GROUP:
			text = "211  Group selected";
			break;
		case OK_GROUPS:
			text = "215  Newsgroups follow";
			break;
		case OK_XMOTD:
			text = "217  News motd file follows";
			break;
		case OK_XINDEX:
			text = "218  Group index file follows";
			break;
		case OK_ARTICLE:
			text = "220  Article (head & body) follows";
			break;
		case OK_HEAD:
			text = "221  Head follows";
			break;
		case OK_BODY:
			text = "222  Body follows";
			break;
		case OK_NOTEXT:
			text = "223  No text sent -- stat, next, last";
			break;
		case OK_NEWNEWS:
			text = "230  New articles by message-id follow";
			break;
		case OK_NEWGROUPS:
			text = "231  New newsgroups follow";
			break;
		case OK_XFERED:
			text = "235  Article transferred successfully";
			break;
		case OK_POSTED:
			text = "240  Article posted successfully";
			break;
		case OK_AUTHSYS:
			text = "280  Authorization system ok";
			break;
		case OK_AUTH:
			text = "281  Authorization (user/pass) ok";
			break;
		case OK_BIN:
			text = "282  binary data follows";
			break;
		case OK_SPLIST:
			text = "283  spooldir list follows";
			break;
		case OK_SPSWITCH:
			text = "284  Switching to a different spooldir";
			break;
		case OK_SPNOCHANGE:
			text = "285  Still using same spooldir";
			break;
		case OK_SPLDIRCUR:
			text = "286  Current spooldir";
			break;
		case OK_SPLDIRAVL:
			text = "287  Available spooldir";
			break;
		case OK_SPLDIRERR:
			text = "288  Unavailable spooldir or invalid entry";
			break;
		case CONT_XFER:
			text = "335  Continue to send article";
			break;
		case CONT_POST:
			text = "340  Continue to post article";
			break;
		case NEED_AUTHINFO:
			text = "380  authorization is required";
			break;
		case NEED_AUTHDATA:
			text = "381  <type> authorization data required";
			break;
		case ERR_GOODBYE:
			text = "400  Have to hang up for some reason";
			break;
		case ERR_NOGROUP:
			text = "411  No such newsgroup";
			break;
		case ERR_NCING:
			text = "412  Not currently in newsgroup";
			break;
		case ERR_XMOTD:
			text = "417  No news motd file";
			break;
		case ERR_XINDEX:
			text = "418  No index file for this group";
			break;
		case ERR_NOCRNT:
			text = "420  No current article selected";
			break;
		case ERR_NONEXT:
			text = "421  No next article in this group";
			break;
		case ERR_NOPREV:
			text = "422  No previous article in this group";
			break;
		case ERR_NOARTIG:
			text = "423  No such article in this group";
			break;
		case ERR_NOART:
			text = "430  No such article at all";
			break;
		case ERR_GOTIT:
			text = "435  Already got that article, don't send";
			break;
		case ERR_XFERFAIL:
			text = "436  Transfer failed";
			break;
		case ERR_XFERRJCT:
			text = "437  Article rejected, don't resend";
			break;
		case ERR_NOPOST:
			text = "440  Posting not allowed";
			break;
		case ERR_POSTFAIL:
			text = "441  Posting failed";
			break;
		case ERR_NOAUTH:
			text = "480  authorization required for command";
			break;
		case ERR_AUTHSYS:
			text = "481  Authorization system invalid";
			break;
		case ERR_AUTHREJ:
			text = "482  Authorization data rejected";
			break;
		case ERR_INVALIAS:
			text = "483  Invalid alias on spooldir cmd";
			break;
		case ERR_INVNOSPDIR:
			text = "484  No spooldir file found";
			break;
		case ERR_COMMAND:
			text = "500  Command not recognized";
			break;
		case ERR_CMDSYN:
			text = "501  Command syntax error";
			break;
		case ERR_ACCESS:
			text = "502  Access to server denied";
			break;
		case ERR_FAULT:
			text = "503  Program fault, command not performed";
			break;
		case ERR_AUTHBAD:
			text = "580  Authorization Failed";
			break;
		default:
			text = "Unknown NNTP response code";
			break;
	}
	return (text);
#else
	return ("");
#endif
}

