/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : init.c
 *  Author    : I.Lea
 *  Created   : 01-04-91
 *  Updated   : 05-12-92
 *  Notes     :
 *  Copyright : (c) Copyright 1991-92 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

char active_times_file[PATH_LEN];
char attributes_file[PATH_LEN];
char add_addr[LEN];		/* address to add to rR reply to author with mail */
char article[PATH_LEN];		/* ~/.article file */
char bug_addr[LEN];		/* address to add send bug reports to */
char cmd_line_printer[PATH_LEN];	/* printer program specified on cmd line */
char cvers[LEN];
char dead_article[PATH_LEN];	/* ~/dead.article file */
char default_maildir[PATH_LEN];	/* mailbox dir where = saves are stored */
char default_organization[PATH_LEN];	/* Organization: */
char default_post_newsgroups[PATH_LEN];
char default_post_subject[PATH_LEN];
char default_select_pattern[LEN];
char default_sigfile[PATH_LEN];
char default_signature[PATH_LEN];
char default_shell_command[LEN];	/* offers user default choice */
char default_savedir[PATH_LEN];		/* directory to save articles to */
char delgroups[LEN];
char homedir[PATH_LEN];
char index_maildir[PATH_LEN];
char index_newsdir[PATH_LEN];
char killfile[PATH_LEN];
char killsubj[LEN];		/* contains Subject:'s not to be shown */
char killfrom[LEN];		/* contains From:'s not to be shown */
char lock_file[PATH_LEN];	/* contains name of index lock file */
char local_newsgroups_file[PATH_LEN];	/* local copy of NNTP newsgroups file */
char mail_news_user[LEN];	/* mail new news to this user address */
char mail_quote_format[PATH_LEN];
char mail_active_file[PATH_LEN];
char mailbox[PATH_LEN];		/* system mailbox for each user */
char mailer[PATH_LEN];		/* mail program */
char motd_file[PATH_LEN];	/* news motd file for newsadmin purposes */
char motd_file_info[PATH_LEN];	/* date of last time news motd file read */
char my_distribution[LEN];	/* Distribution: */
char news_active_file[PATH_LEN];
char news_quote_format[PATH_LEN];
char mailgroups_file[PATH_LEN];
char newsgroups_file[PATH_LEN];
char newsrc[PATH_LEN];
char newnewsrc[PATH_LEN];
char page_header[LEN];		/* page header of pgm name and version */
char postfile[PATH_LEN];
char default_printer[LEN];	/* printer program specified from tinrc */
char progname[PATH_LEN];	/* program name */
char quote_chars[PATH_LEN];	/* quote chars for posting/mails ": " */
char rcdir[PATH_LEN];
char rcfile[PATH_LEN];
char reply_to[LEN];		/* Reply-To: address */
char save_active_file[PATH_LEN];
char spooldir[PATH_LEN];	/* directory where news is */
char spooldir_alias[PATH_LEN];	/* alias of spooldir being used */
char subscriptions_file[PATH_LEN];
char txt_help_bug_report[LEN];	/* address to add send bug reports to */
char userid[PATH_LEN];


int unread_art_mark;
int hot_art_mark;
int return_art_mark;
int xindex_supported = FALSE;
int xuser_supported = FALSE;
int xspooldir_supported = FALSE;
int NOTESLINES;			/* set in set_win_size () */
int RIGHT_POS;			/* set in set_win_size () */
int MORE_POS;			/* set in set_win_size () */
int confirm_action;
int max_subj = 0;
int max_from = 0;
int group_top;			/* one past top of my_group */
int groupname_len = 0;		/* one past top of my_group */
int catchup = FALSE;		/* mark all arts read in all subscribed groups */
int update_fork = FALSE;	/* update index files by forked tin -u */
int verbose = FALSE;		/* update index files only mode */
int start_line_offset;		/* used by invoke_editor for line no. */
int inn_nntp_server = FALSE;	/* read news via INN NNTP */
int read_news_via_nntp = FALSE;	/* read news locally or via NNTP */
int local_index;		/* do private indexing? */
int real_gid;
int real_uid;
int real_umask;
int show_description;
int slow_speed_terminal;
int start_editor_offset;
int tin_uid;
int tin_gid;
int top = 0;
int top_base;
int check_any_unread = FALSE;
int start_any_unread = FALSE;

int beginner_level;		/* beginner level (shows mini help a la elm) */
int catchup_read_groups;	/* ask if read groups are to be marked read */
int cmd_line;			/* batch / interactive mode */
int check_for_new_newsgroups;	/* don't check for new newsgroups */
int created_rcdir;		/* checks if first time tin is started */
int default_auto_save;		/* save thread with name from Archive-name: field */
int default_batch_save;		/* save arts if -M/-S command line switch specified */
int default_show_author;	/* show_author value from 'M' menu in tinrc */
int default_show_only_unread;	/* show only new/unread arts or all arts */
int default_sort_art_type;	/* sort arts[] array by subject,from or date field */
int default_thread_arts;	/* thread/unthread articles for viewing */
int display_reading_prompt;	/* display 'Reading...' when fetching art via NNTP */
int draw_arrow_mark;		/* draw -> or highlighted bar */
int force_screen_redraw;	/* force screen redraw after external (shell) commands */
int full_page_scroll;		/* page half/full screen of articles/groups */
int groupname_max_length;	/* max len of group names to display on screen */
int use_keypad;			/* enables/disables scroll keys on supported terminals */
int killed_articles;		/* killed / auto-selected hot articles */
int mark_saved_read;		/* mark saved article/thread as read */
int num_of_hot_arts;
int num_of_killed_arts;
int num_of_tagged_arts;
int process_id;
int pos_first_unread;		/* position cursor at first/last unread article */
int default_post_proc_type;	/* type of post processing to be performed */
int post_article_and_exit;	/* quick post of an article then exit (elm like) */
int print_header;		/* print all of mail header or just Subject: & From lines */
int purge_index_files;		/* stat all articles to see if they still exist */
int reread_active_file_secs;	/* reread active file interval in seconds */
int read_local_newsgroups_file;	/* read newsgroups file locally or via NNTP */
int mail_news;			/* mail all arts to specified user */
int save_news;			/* save all arts to savedir structure */
int save_to_mmdf_mailbox;	/* save mail to MMDF/mbox format mailbox */
int show_author;
int show_last_line_prev_page;	/* set TRUE to see last line of prev page (ala nn) */
int show_only_unread_groups;	/* set TRUE to see only subscribed groups with new news */
int spooldir_is_active;		/* set TRUE if current spooldir is active news feed */
int system_status;
int tab_after_X_selection;	/* set TRUE if you want auto TAB after X */
int update;			/* update index files only mode */
int use_builtin_inews;

struct passwd *myentry;


/*
 * Get users home directory, userid, and a bunch of other stuff!
 */

void init_selfinfo ()
{
	extern char *getlogin ();
	extern struct passwd *getpwnam ();
	char nam[LEN];
	char *p;
	FILE *fp;
	struct stat sb;

#ifdef AMIGA
	/* 
	 * use the task address for pid which is unique 
	 * TIND env. var is used to specify 'setuid'.
	 */
	process_id = (long) FindTask (0L) >> 2;
	
	tin_uid = tin_gid = 0;
	real_uid = real_gid = (getenv ("TIND") ? 1 : 0);
#else
	process_id = getpid ();
	tin_uid = geteuid ();
	tin_gid = getegid ();
	real_uid = getuid ();
	real_gid = getgid ();

	real_umask = umask (0);
	umask (real_umask);
#endif	/* AMIGA */
	
#ifdef HAVE_SETLOCALE
	setlocale (LC_ALL, "");
#endif
		
	/*
	 * we're setuid, so index in /usr/spool/news even if user root
	 * This is quite essential if non local index files are 
	 * to be updated during the night from crontab by root.
	 */
	if (tin_uid != real_uid) {
		local_index = FALSE;
		set_real_uid_gid ();

	} else {	/* index in users home directory ~/.tin/.index */
		local_index = TRUE;
	}

#ifdef AMIGA
	if ((p = (char *) getenv ("USERNAME")) != (char *) 0) {
		my_strncpy (userid, p, sizeof (userid));
	} else {
		error_message (txt_env_var_not_found, "USERNAME");
		tin_done (1);
	}
	if ((p = (char *) getenv ("HOME")) != (char *) 0) {
		my_strncpy (homedir, p, sizeof (homedir));
	} else {
		error_message (txt_env_var_not_found, "HOME");
		tin_done (1);
	}
#else
	myentry = (struct passwd *) 0;
	if (((p = getlogin ()) != (char *) 0) && strlen (p)) {
		myentry = getpwnam (p);
	} else {
		myentry = getpwuid (getuid ());
	}

	strcpy (userid, myentry->pw_name);

	if ((p = (char *) getenv ("TINDIR")) != (char *) 0) {
		strcpy (homedir, p);
	} else if ((p = (char *) getenv ("HOME")) != (char *) 0) {
		strcpy (homedir, p);
	} else {
		strcpy (homedir, myentry->pw_dir);
	}
#endif	/* AMIGA */

	beginner_level = TRUE;
	catchup_read_groups = FALSE;
	confirm_action = TRUE;
	created_rcdir = FALSE;
#ifdef USE_INVERSE_HACK
	inverse_okay = FALSE;
	draw_arrow_mark = TRUE;
#else
	inverse_okay = TRUE;
	draw_arrow_mark = FALSE;
#endif
	default_auto_save = TRUE;
	default_batch_save = FALSE;
	default_move_group = 0;
	default_post_proc_type = POST_PROC_NONE;
	default_show_author = SHOW_FROM_NAME;
	default_show_only_unread = FALSE;
	default_sort_art_type = SORT_BY_DATE_ASCEND;
	default_thread_arts = TRUE;
#ifdef SLOW_SCREEN_UPDATE
	display_reading_prompt = FALSE;
#else
	display_reading_prompt = TRUE;
#endif
	force_screen_redraw = FALSE;
	full_page_scroll = TRUE;
	groupname_max_length = 132;
	hot_art_mark = HOT_ART_MARK;
	killed_articles = FALSE;
	mark_saved_read = TRUE;
	num_of_hot_arts = 0;
	num_of_killed_arts = 0;
	num_of_tagged_arts = 0;
	pos_first_unread = TRUE;
	post_article_and_exit = FALSE;
	print_header = FALSE;
	purge_index_files = FALSE;
	reread_active_file_secs = REREAD_ACTIVE_FILE_SECS;
	return_art_mark = RETURN_ART_MARK;
	save_news = FALSE;
#ifdef HAVE_MMDF_MAILER
	save_to_mmdf_mailbox = TRUE;
#else
	save_to_mmdf_mailbox = FALSE;
#endif
	show_last_line_prev_page = FALSE;
	show_description = TRUE;
	show_only_unread_groups = FALSE;
	slow_speed_terminal = FALSE;
#ifdef AMIGA
	start_editor_offset = FALSE;
#else	
	start_editor_offset = TRUE;
#endif
	tab_after_X_selection = FALSE;
#ifdef INDEX_DAEMON
	check_for_new_newsgroups = FALSE;
	update = TRUE;
#else
	check_for_new_newsgroups = TRUE;
	update = FALSE;
#endif
	unread_art_mark = UNREAD_ART_MARK;
	use_builtin_inews = TRUE;
	use_keypad = FALSE;

	newsrc[0] = '\0';
	
	killsubj[0] = '\0';
	killfrom[0] = '\0';
	
	strncpy (mail_quote_format, txt_mail_quote, sizeof (mail_quote_format));
	strncpy (news_quote_format, txt_news_quote, sizeof (news_quote_format));

	cmd_line_printer[0] = '\0';
	default_art_search[0] = '\0';
	default_author_search[0] = '\0';
	default_crosspost_group[0] = '\0';
	default_goto_group[0] = '\0';
	default_group_search[0] = '\0';
	default_mail_address[0] = '\0';
	default_organization[0] = '\0';
	default_pipe_command[0] = '\0';
	default_post_newsgroups[0] = '\0';
	default_post_subject[0] = '\0';
	default_regex_pattern[0] = '\0';
	default_save_file[0] = '\0';
	default_select_pattern[0] = '\0';
	default_shell_command[0] = '\0';
	default_subject_search[0] = '\0';
	proc_ch_default = 'n';

	/*
	 * set start spooldir to active newsfeed
	 */
	strcpy (spooldir_alias, "news");
	strcpy (spooldir, SPOOLDIR);
	strcpy (mailer, get_val ("MAILER", DEFAULT_MAILER));
	strcpy (default_printer, DEFAULT_PRINTER);
	strcpy (quote_chars, DEFAULT_COMMENT);
	strcpy (bug_addr, BUG_REPORT_ADDRESS);

	set_tindir ();	

	/* 
	 * Amiga uses assigns which end in a ':' and won't work with a '/'
	 * tacked on after them: e.g. we want UULIB:active, and not 
	 * UULIB:/active. For this reason I have changed the sprintf calls
	 * to joinpath. This is defined to sprintf(result,"%s/%s",dir,file)
	 * on all UNIX systems.
	 */

	joinpath (mail_active_file, rcdir, ACTIVE_MAIL);
	joinpath (save_active_file, rcdir, ACTIVE_SAVE);
	joinpath (news_active_file, LIBDIR, get_val ("TIN_ACTIVE", "active"));
	joinpath (attributes_file, rcdir, "attributes");
	joinpath (article, homedir, ".article");
	joinpath (dead_article, homedir, "dead.article");
	joinpath (delgroups, homedir, ".delgroups");
	joinpath (mailbox, DEFAULT_MAILBOX, userid);
	joinpath (default_maildir, "~", DEFAULT_MAILDIR);
	joinpath (default_savedir, "~", DEFAULT_SAVEDIR);
	joinpath (default_sigfile, "~", ".Sig");
	joinpath (default_signature, homedir, ".signature");
	joinpath (motd_file, LIBDIR, MOTD_FILE);
	joinpath (mailgroups_file, rcdir, MAILGROUPS_FILE);
	joinpath (newsgroups_file, LIBDIR, NEWSGROUPS_FILE);
	joinpath (subscriptions_file, LIBDIR, SUBSCRIPTIONS_FILE);

#ifdef INDEX_DAEMON
	joinpath  (lock_file, TMPDIR, LOCK_FILE);	
	strcpy (newsrc, news_active_file);	/* default so all groups are indexed */
	joinpath (active_times_file, rcdir, "active.times");
	joinpath (index_newsdir, get_val ("TIN_INDEX", spooldir), INDEX_NEWSDIR);

	if (stat (index_newsdir, &sb) == -1) {
		mkdir (index_newsdir, 0777);
	}
#else
#	ifdef HAVE_LONG_FILENAMES
		sprintf (lock_file, "%stin.%s.LCK", TMPDIR, userid);	
#	else
		sprintf (lock_file, "%s%s.LCK", TMPDIR, userid);	
#endif

	if (stat (rcdir, &sb) == -1) {
		created_rcdir = TRUE;
		mkdir (rcdir, 0755);
	}
	if (tin_uid != real_uid) {
		joinpath (index_newsdir, get_val ("TIN_INDEX", spooldir), INDEX_NEWSDIR);

		set_tin_uid_gid ();
		if (stat (index_newsdir, &sb) == -1) {
			mkdir (index_newsdir, 0777);
		}
		set_real_uid_gid ();
	} else if (stat (index_newsdir, &sb) == -1) {
		mkdir (index_newsdir, 0755);
	}
	if (stat (postfile, &sb) == -1) {
		if ((fp = fopen (postfile, "w")) != NULL) {
			fclose (fp);
		}
	}
	if (stat (attributes_file, &sb) == -1) {
		write_attributes_file ();
	}

	/*
	 * Read user config file ~/.tin/tinrc
	 */
	read_rcfile ();

#endif /* INDEX_DAEMON */	

	if (stat (news_active_file, &sb) >= 0)
		goto got_active;

	/*
	 *  I hate forgetting to define LIBDIR correctly.  Guess a couple
	 *  of the likely places if it's not where LIBDIR says it is.
	 */

	strcpy (news_active_file, "/usr/lib/news/active");
	if (stat (news_active_file, &sb) >= 0)
		goto got_active;

	strcpy (news_active_file, "/usr/local/lib/news/active");
	if (stat (news_active_file, &sb) >= 0)
		goto got_active;

	strcpy (news_active_file, "/usr/public/lib/news/active");
	if (stat (news_active_file, &sb) >= 0)
		goto got_active;

	/*
	 *  Oh well. Revert to what LIBDIR says it is to produce a useful
	 *  error message when read_news_active_file () fails later.
	 */

	joinpath (news_active_file, LIBDIR, "active");

got_active:

	/*
	 *  check enviroment for ORGANIZATION / NEWSORG
	 */
#ifdef apollo
	if ((p = (char *) getenv ("NEWSORG")) != NULL) {
#else	
	if ((p = (char *) getenv ("ORGANIZATION")) != NULL) {
#endif
		my_strncpy (default_organization, p, sizeof (default_organization));
		goto got_org;
	}

	/*
	 *  check ~/.tin/organization for organization
	 */
	joinpath (nam, rcdir, "organization");
	fp = fopen (nam, "r");

	/*
	 *  check LIBDIR/organization for system wide organization
	 */
	if (fp == NULL) {
		joinpath (nam, LIBDIR, "organization");
		fp = fopen (nam, "r");
	}

#ifndef AMIGA
	if (fp == NULL) {
		sprintf (nam, "/usr/lib/news/organization");
		fp = fopen (nam, "r");
	}

	if (fp == NULL) {
		sprintf (nam, "/usr/local/lib/news/organization");
		fp = fopen (nam, "r");
	}

	if (fp == NULL) {
		sprintf (nam, "/usr/public/lib/news/organization");
		fp = fopen (nam, "r");
	}

	if (fp == NULL) {
		sprintf (nam, "/etc/organization");
		fp = fopen (nam, "r");
	}
#endif	/* AMIGA */

	if (fp != NULL) {
		if (fgets (default_organization, sizeof (default_organization), fp) != NULL) {
			for (p = default_organization; *p && *p != '\n'; p++)
				continue;
			*p = '\0';
		}
		fclose (fp);
	}

got_org:;

	/*
	 *  check enviroment for REPLYTO
	 */
	reply_to[0] = '\0';
	if ((p = (char *) getenv ("REPLYTO")) != NULL) {
		my_strncpy (reply_to, p, sizeof (reply_to));
		goto got_reply;
	}

	joinpath (nam, rcdir, "replyto");
	if ((fp = fopen (nam, "r")) != NULL) {
		if (fgets (reply_to, sizeof (reply_to), fp) != NULL) {
			reply_to[strlen (reply_to)-1] = '\0';
		}
		fclose (fp);
	}

got_reply:;

	/*
	 *  check enviroment for DISTRIBUTION
	 */
	my_distribution[0] = '\0';
	if ((p = (char *) getenv ("DISTRIBUTION")) != NULL) {
		my_strncpy (my_distribution, p, sizeof (my_distribution));
	}

	/*
	 *  check enviroment for ADD_ADDRESS
 	 */
	add_addr[0] = '\0';
	if ((p = (char *) getenv ("ADD_ADDRESS")) != NULL) {
		my_strncpy (add_addr, p, sizeof (add_addr));
		goto got_add_addr;
	}

	joinpath (nam, rcdir, "add_address");
	if ((fp = fopen (nam, "r")) != NULL) {
		if (fgets (add_addr, sizeof (add_addr), fp) != NULL) {
			add_addr[strlen (add_addr)-1] = '\0';
		}
		fclose (fp);
	}

got_add_addr:;

	/*
	 *  check enviroment for BUG_ADDRESS
	 */
	if ((p = (char *) getenv ("BUG_ADDRESS")) != NULL) {
		my_strncpy (bug_addr, p, sizeof (bug_addr));
		goto got_bug_addr;
	}

	joinpath (nam, rcdir, "bug_address");
	if ((fp = fopen (nam, "r")) != NULL) {
		if (fgets (bug_addr, sizeof (bug_addr), fp) != NULL) {
			bug_addr[strlen (bug_addr)-1] = '\0';
		}
		fclose (fp);
	}

got_bug_addr:;
	sprintf (txt_help_bug_report, txt_help_bug, bug_addr);
}

/*
 * Set up ~/.tin directory & support files depending on where the news
 * is being read from (ie. active news / CD-ROM spooldir).  Note that
 * any control files which may be specific to a given spooldir (various
 * CD issues versus live news) should be under the spooldir_alias 
 * subdirectory also.
 */
 
void set_tindir ()
{
	struct stat sb;
	
	joinpath (rcdir, homedir, RCDIR);
	if (stat (rcdir, &sb) == -1) {
		created_rcdir = TRUE;
		mkdir (rcdir, 0755);
	}

	if (strcmp (spooldir_alias, "news") != 0) {
		joinpath (rcdir, rcdir, spooldir_alias); 
		if (stat (rcdir, &sb) == -1) {
			created_rcdir = TRUE;
			mkdir (rcdir, 0755);
		}
		/*
		 * Use a separate .newsrc file for every spooldir 
		 */
		joinpath (newsrc, rcdir, ".newsrc");
		joinpath (newnewsrc, rcdir, ".newnewsrc");

		spooldir_is_active = FALSE;
		reread_active_file = FALSE;
#ifndef DONT_REREAD_ACTIVE_FILE
		alarm (0);
#endif		
	} else {
		joinpath (rcfile, rcdir, RCFILE);
		joinpath (killfile, rcdir, KILLFILE);
		joinpath (postfile, rcdir, POSTFILE);
		if (newsrc[0] == '\0') {
			joinpath (newsrc, homedir, ".newsrc");
			joinpath (newnewsrc, homedir, ".newnewsrc");
		}
		spooldir_is_active = TRUE;
		reread_active_file = TRUE;
	}

	read_local_newsgroups_file = FALSE;
	joinpath (local_newsgroups_file, rcdir, NEWSGROUPS_FILE);

	joinpath (index_maildir, rcdir, INDEX_MAILDIR);
	joinpath (index_newsdir, rcdir, INDEX_NEWSDIR);
	if (stat (index_maildir, &sb) == -1) {
		mkdir (index_maildir, 0755);
	}
	if (stat (index_newsdir, &sb) == -1) {
		mkdir (index_newsdir, 0755);
	}
}

/*
 * Create default mail & save directories if they do not exist
 */
 
int create_mail_save_dirs ()
{
	int created = FALSE;
#ifndef INDEX_DAEMON
	char path[PATH_LEN];
	struct stat sb;

		
	if (! strfpath (default_maildir, path, sizeof (path), 
	    homedir, (char *) 0, (char *) 0, (char *) 0)) {
		joinpath (path, homedir, DEFAULT_MAILDIR);
	}
	if (stat (path, &sb) == -1) {
		mkdir (path, 0755);
		created = TRUE;
	}

	if (! strfpath (default_savedir, path, sizeof (path), 
	    homedir, (char *) 0, (char *) 0, (char *) 0)) {
		joinpath (path, homedir, DEFAULT_SAVEDIR);
	}
	if (stat (path, &sb) == -1) {
		mkdir (path, 0755);
		created = TRUE;
	}

#endif	/* INDEX_DAEMON */
	
	return (created);
}
