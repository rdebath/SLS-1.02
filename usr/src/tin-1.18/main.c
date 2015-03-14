/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : main.c
 *  Author    : I.Lea & R.Skrenta
 *  Created   : 01-04-91
 *  Updated   : 05-12-92
 *  Notes     :
 *  Copyright : (c) Copyright 1991-92 by Iain Lea & Rich Skrenta
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"


static char **cmdargs;
static int num_cmdargs;
static int max_cmdargs;

/*
 *  OK lets start the ball rolling...
 */
 
void main (argc, argv)
	int argc;	
	char *argv[];
{
	int start_groupnum = 0;

	cmd_line = TRUE;
	debug = 0;	/* debug OFF */

	set_signal_handlers ();

	basename (argv[0], progname);

	sprintf (page_header, "%s %s PL%d", progname, VERSION, PATCHLEVEL); 	
	sprintf (cvers, txt_copyright_notice, page_header);

#if defined(NNTP_ONLY) || defined(CDROM_ONLY)
	read_news_via_nntp = TRUE;
#else
	/* 
	 *  rtin/cdtin so read news remotely via NNTP 
	 */
	if (progname[0] == 'r' || (progname[0] == 'c' && progname[1] == 'd' )) {
#		ifdef NNTP_ABLE			
			read_news_via_nntp = TRUE;
#		else
			error_message (txt_option_not_enabled, "-DNNTP_ABLE");
			exit (1);
#		endif
	}
#endif

	/*
	 *  Set up initial array sizes, char *'s: homedir, newsrc, etc. 
	 */
	init_alloc ();
	hash_init ();
	init_selfinfo ();

	/*
	 *  Process envargs & command line options
	 */
	read_cmd_line_options (argc, argv);
	if (update_fork || (update && verbose) || !update) {
		error_message (cvers, "");
	}

	/*
	 *  If specified connect to (cdrom pseudo) nntp server
	 */
	nntp_open ();

	/*
	 *  Log username info to local/central logfile (NNTP XUSER)
	 */
	log_user ();

	/*
	 *  Read message of the day file from newsadmin
	 */
	read_motd_file ();

	/*
	 *  Load the mail & news active files into active[]
	 */
	read_mail_active_file ();
	read_news_active_file ();

	/*
	 *  Load the group specific attributes file into active[]
	 */
	read_attributes_file ();

	/*
	 *  Quick post an article & exit if -w specified
	 */
	if (post_article_and_exit) {
		setup_screen ();
		quick_post_article ();
		tin_done (0);
	}
	
	/*
	 *  Read text descriptions for mail & news groups from 
	 *  ~/.tin/mailgroups & LIBDIR/newsgroups respectively
	 */
	read_mailgroups_file ();
	read_newsgroups_file ();
	debug_print_active ();
	
	if (create_mail_save_dirs ()) {
		write_rcfile ();
	}	

	if (! read_cmd_line_groups ()) {
		backup_newsrc ();
		read_newsrc (TRUE);
		toggle_my_groups (show_only_unread_groups, "");
	}

	/*
	 *  Read in users kill/auto-select (hot) file
	 */
	killed_articles = read_kill_file ();
	
	/*
	 *  Check/start if any new/unread articles
	 */
	start_groupnum = check_for_any_new_news (check_any_unread, start_any_unread);

	/*
	 *  Mail any new articles to specified user
	 *  or
	 *  Save any new articles to savedir structure for later reading
	 */
	save_or_mail_new_news ();
	
	/*
	 *  Update index files
	 */
	update_index_files ();
	
	/*
	 *  Set up screen and switch to raw mode
	 */
	if (! InitScreen ()) {
		error_message (txt_screen_init_failed, progname);
		exit (1);
	}
	setup_screen ();

	/*
	 *  If first time print welcome screen and auto-subscribe
	 *  to groups specified in /usr/lib/news/subscribe locally
	 *  or via NNTP if reading news remotely (LIST SUBSCRIBE)
	 */
	if (created_rcdir && !update) {
		show_intro_page ();
	}
	
	/*
	 *  Work loop
	 */
	selection_index (start_groupnum);
}

/*
 * process command line options
 */

void read_cmd_line_options (argc, argv)
	int argc;
	char *argv[];
{
	int ch;

	envargs (&argc, &argv, "TINRC");
	
#ifdef INDEX_DAEMON
	while ((ch = getopt (argc, argv, "D:f:hI:PvV")) != EOF) {
#else
	while ((ch = getopt (argc, argv, "cD:f:hHI:m:M:p:PqrRs:SuUvVwzZ")) != EOF) {
#endif
		switch (ch) {
			case 'c':
				catchup = TRUE;
				update = TRUE;
				break;
				
			case 'D':		/* debug mode 1=NNTP 2=ALL */
#ifdef DEBUG			
				debug = atoi (optarg);
#else
				error_message (txt_option_not_enabled, "-DDEBUG");
				exit (1);
#endif
				break;

			case 'f':	/* active (tind) / newsrc (tin) file */
#ifdef INDEX_DAEMON
				my_strncpy (news_active_file, optarg, sizeof (news_active_file));
#else
				my_strncpy (newsrc, optarg, sizeof (newsrc));
#endif
				break;

			case 'H':
				show_intro_page ();
				exit (1);
				break;

#if !defined(NNTP_ONLY) || !defined(NNTP_XINDEX)
			case 'I':
				my_strncpy (index_newsdir, optarg, sizeof (index_newsdir));
				mkdir (index_newsdir, 0777);
				break;
#endif
			case 'm':
				my_strncpy (default_maildir, optarg, sizeof (default_maildir));
				break;

			case 'M':	/* mail new news to specified user */
				my_strncpy (mail_news_user, optarg, sizeof (mail_news_user));
				mail_news = TRUE;
				update = TRUE;
				catchup = TRUE;
				break;

			case 'p':
				my_strncpy (cmd_line_printer, optarg, sizeof (cmd_line_printer));
				break;

			case 'P':	/* stat every art for a through purge */
				purge_index_files = TRUE;
				break;

			case 'q':
				check_for_new_newsgroups = FALSE;
				break;

			case 'r':	/* read news remotely from default NNTP server */
#ifdef NNTP_ABLE			
				read_news_via_nntp = TRUE;
#else
				error_message (txt_option_not_enabled, "-DNNTP_ABLE");
				exit (1);
#endif
				break;

			case 'R':	/* read news saved by -S option */
				error_message ("%s: Option -R not yet implemented.", progname);
				exit (1);
				break;

			case 's':
				my_strncpy (default_savedir, optarg, sizeof (default_savedir));
				break;

			case 'S':	/* save new news to dir structure */
				save_news = TRUE;
				update = TRUE;
				break;

			case 'u':	/* update index files */
				update = TRUE;
				show_description = FALSE;
				break;

			case 'U':	/* update index files in background */
				update_fork = TRUE;
				update = TRUE;
				break;

			case 'v':	/* verbose mode */
				verbose = TRUE;
				break;

			case 'V':
#if defined(__DATE__) && defined(__TIME__)			
				sprintf (msg, "Version: %s PL%d  %s %s",
					VERSION, PATCHLEVEL, __DATE__, __TIME__);
#else
				sprintf (msg, "Version: %s PL%d",
					VERSION, PATCHLEVEL);
#endif					
				error_message (msg, "");
				exit (1);
				break;

			case 'w':	/* post article & exit */
				post_article_and_exit = TRUE;
				break;

			case 'z':
				start_any_unread = TRUE;
				update = TRUE;
				break;

			case 'Z':
				check_any_unread = TRUE;
				update = TRUE;
				break;

			case 'h':
			case '?':
			default:
				usage (progname);
				exit (1);
		}
	}
	cmdargs = argv;
	num_cmdargs = optind;
	max_cmdargs = argc;
}

/*
 * usage
 */

void usage (progname)
	char *progname;
{
#ifndef INDEX_DAEMON
	error_message ("%s A threaded Netnews reader.\n", cvers);
#else
	error_message ("%s Tin index file daemon.\n", cvers);
#endif
	error_message ("Usage: %s [options] [newsgroups]", progname);
#ifndef INDEX_DAEMON
	error_message ("  -c       mark all news as read in subscribed newsgroups (batch mode)", "");
	error_message ("  -f file  subscribed to newsgroups file [default=%s]", newsrc);
#else
	error_message ("  -f file  active newsgroups file [default=%s]", newsrc);
#endif
	error_message ("  -h       help", "");
#ifndef INDEX_DAEMON
	error_message ("  -H       help information about %s", progname);
#endif
	error_message ("  -I dir   news index file directory [default=%s]", index_newsdir);
#ifndef INDEX_DAEMON
	error_message ("  -m dir   mailbox directory [default=%s]", default_maildir);
	error_message ("  -M user  mail new news to specified user (batch mode)", "");
	error_message ("  -p file  print program with options [default=%s]", DEFAULT_PRINTER);
	error_message ("  -P       purge any expired articles from index files", "");
	error_message ("  -q       quick start by not checking for new newsgroups", "");
#  if defined(NNTP_ABLE) && !defined(NNTP_ONLY)
	if (! read_news_via_nntp) {
		error_message ("  -r       read news remotely from default NNTP server", "");
	}
#  endif /* NNTP_ABLE */	
	error_message ("  -R       read news saved by -S option", "");
	error_message ("  -s dir   save news directory [default=%s]", default_savedir);
	error_message ("  -S       save new news for later reading (batch mode)", "");
#  if !defined(NNTP_ONLY) || !defined(NNTP_XINDEX)
	error_message ("  -u       update index files (batch mode)", "");
	error_message ("  -U       update index files in the background while reading news", "");
#  endif /* NNTP_XINDEX */
#else
	error_message ("  -P       purge any expired articles from index files", "");
#endif /* INDEX_DAEMON */
	error_message ("  -v       verbose output for batch mode options", "");
#ifndef INDEX_DAEMON
	error_message ("  -w       post an article and exit", "");
	error_message ("  -z       start if any unread news", "");
	error_message ("  -Z       return status indicating if any unread news (batch mode)", "");
#endif
	error_message ("\nMail bug reports/comments to %s", BUG_REPORT_ADDRESS);
}

/*
 *  check/start if any new/unread articles
 */

int check_for_any_new_news (check_any_unread, start_any_unread)
	int check_any_unread;
	int start_any_unread;
{
	int i = 0;
	
	if (check_any_unread) {
		i = check_start_save_any_news (CHECK_ANY_NEWS);
		exit (i);
	}
	
	if (start_any_unread) {
		i = check_start_save_any_news (START_ANY_NEWS);
		if (i == -1) {		/* no new/unread news so exit */
			exit (0);
		}
		update = FALSE;
	}
	return (i);
}

/*
 *  mail any new articles to specified user
 *  or
 *  save any new articles to savedir structure for later reading
 */

void save_or_mail_new_news ()
{
	int i;
	
	if (mail_news || save_news) {
		i = catchup;			/* set catchup to FALSE */
		catchup = FALSE;
		do_update ();
		catchup = i;			/* set catchup to previous value */
		if (mail_news) {
			check_start_save_any_news (MAIL_ANY_NEWS);
		} else {
			check_start_save_any_news (SAVE_ANY_NEWS);
		}
		tin_done (0);
	}
}

/*
 *  update index files
 */

void update_index_files ()
{
	if (update || update_fork) {
		if (!catchup && (read_news_via_nntp && xindex_supported)) {
			error_message ("%s: Updating of index files not supported", progname);
			tin_done (1);
		}

		COLS = 132;					/* set because curses has not started */ 
#ifndef AMIGA
		if (update_fork) {
			catchup = FALSE;		/* turn off msgs when running forked */ 
			verbose = FALSE;
			switch (fork ()) {		/* fork child to update indexes in background */
				case -1:	/* error forking */	
					perror_message ("Failed to start background indexing process", "");
					break;
				case 0:		/* child process */	
					create_index_lock_file (lock_file);
					process_id = getpid ();
#ifdef BSD
					setpgrp (0, process_id);	/* reset process group leader to this process */
#	ifdef TIOCNOTTY
					{
						int fd;
	
						if ((fd = open ("/dev/tty", O_RDWR)) >= 0) {
							ioctl (fd, TIOCNOTTY, (char *) NULL);
							close (fd);
						}	
					}	
#	endif
#else
					setpgrp ();
					signal (SIGHUP, SIG_IGN);	/* make immune from process group leader death */
#endif
					signal (SIGQUIT, SIG_IGN);	/* stop indexing being interrupted */			
					signal (SIGALRM, SIG_IGN);	/* stop indexing resyning active file */			
					nntp_open ();			/* connect server if we are using nntp */
					default_thread_arts = FALSE;	/* stop threading to run faster */
					do_update ();
					tin_done (0);
					break;
				default:	/* parent process*/
					break;					
			}	
			update = FALSE;
		} else 
#endif	/* AMIGA */
		{
			create_index_lock_file (lock_file);
			default_thread_arts = FALSE;	/* stop threading to run faster */
			do_update ();
			tin_done (0);
		}
	}
	
}

/*
 *  display page of general info. for first time user.
 */

void show_intro_page ()
{
	if (cmd_line) {
		wait_message (cvers); 	
	} else {
		ClearScreen ();
		center_line (0, TRUE, cvers); 
		Raw (FALSE);	
	}

	printf ("\n\nWelcome to tin, a full screen threaded Netnews reader. It can read news locally\n");
	printf ("(ie. <spool>/news) or remotely (-r option) from a NNTP  (Network News Transport\n");
	printf ("Protocol) server. tin -h lists the available command line options.\n\n");

	printf ("Tin has five  newsreading levels,  the newsgroup  selection page,  the spooldir\n");
	printf ("selection page,  the group index page,  the thread listing page and the article\n");
	printf ("viewer. Help is available at each level by pressing the 'h' command.\n\n");

	printf ("Move up/down by using the terminal arrow keys or 'j' and 'k'.  Use PgUp/PgDn or\n");
	printf ("Ctrl-U and Ctrl-D to page up/down. Enter a newsgroup by pressing RETURN.\n\n");

	printf ("Articles, threads, tagged articles or articles matching a pattern can be mailed\n");
	printf ("('m' command), printed ('o' command), saved ('s' command), piped ('|' command).\n");
	printf ("Use the 'w' command  to post  a news  article,  the 'f'/'F' commands to  post a\n");
	printf ("follow-up  to  an existing  news article and the 'r'/'R' commands to  reply via\n");
	printf ("mail to an existing news articles author.  The 'M' command allows the operation\n");
	printf ("of tin to be configured via a menu.\n\n");

	printf ("For more information read the manual page, README, INSTALL, TODO and FTP files.\n");
	printf ("Please send bug reports/comments to the programs author with the 'B' command.\n");
	fflush (stdout);

	if (! cmd_line) {
		Raw (TRUE);	
		continue_prompt ();
	}
}


int read_cmd_line_groups ()
{
	char buf[PATH_LEN];
	int matched = FALSE;
	int num = num_cmdargs;
	register int i;
		
	if  (num < max_cmdargs) {
		group_top = 0;	
	
		while (num < max_cmdargs) {
			sprintf (buf, "Matching %s groups...", cmdargs[num]);
			wait_message (buf);
			
			for (i = 0 ; i < num_active ; i++) {
				if (wildmat (active[i].name, cmdargs[num])) {		
					if (add_group (active[i].name, TRUE) < 0) {
						error_message (txt_not_in_active_file, active[i].name);
					}
				}	
			}	
			num++;
		}
		matched = TRUE;
	}
	
	return (matched);
}
