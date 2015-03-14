/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : save.c
 *  Author    : I.Lea & R.Skrenta
 *  Created   : 01-04-91
 *  Updated   : 23-11-92
 *  Notes     :
 *  Copyright : (c) Copyright 1991-92 by Iain Lea & Rich Skrenta
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

#define	INITIAL		1
#define MIDDLE		2
#define OFF		3
#define END		4

extern int errno;

int create_subdir = TRUE;


/*
 * types of archive programs
 */
 
struct archiver_t { 
	char *name;
	char *ext;
	char *test;
	char *list;
	char *extract;
} archiver[] = {
	{ "",		"",		"",		"",		""	},
	{ "",		"",		"",		"",		""	},
	{ "",		"",		"",		"",		""	},
#ifdef AMIGA
	{ "lha",	"lha",		"t",		"l",		"x" },
#else
	{ "zoo",	"zoo",		"-test",	"-list",	"-extract" },
#endif	
	{ "unzip",	"zip",		"-t",		"-l",		"-o"	},
	{ (char *) 0,	(char *) 0,	(char *) 0,	(char *) 0,	(char *) 0 }
};

extern char *glob_group;
extern char note_h_path[LEN];		/* Path:	*/
extern char note_h_date[PATH_LEN];	/* Date:	*/
extern FILE	*note_fp;		/* the body of the current article */
extern int index_point;
extern int note_end;
extern int note_page;
extern long note_mark[MAX_PAGES];


/*
 *  Check for articles and say how many new/unread in each group.
 *  or
 *  Start if new/unread articles and return first group with new/unread.
 *  or
 *  Save any new articles to savedir and mark arts read and mail user
 *  and inform how many arts in which groups were saved.
 *  or
 *  Mail any new articles to specified user and mark arts read and mail
 *  user and inform how many arts in which groups were mailed.
 */

int check_start_save_any_news (check_start_save)
	int check_start_save;
{
#ifndef INDEX_DAEMON

	char buf[LEN], logfile[LEN];
	char group_path[PATH_LEN];
	char savefile[PATH_LEN];
	char path[PATH_LEN];
	extern FILE *note_fp;
	FILE *fp = (FILE *) 0; 
	FILE *fp_log = (FILE *) 0;
	int i, j, k, print_group;
	int check_arts = 0;
	int log_opened = TRUE;
	int print_first = TRUE;
	int saved_arts = 0;
	int saved_groups = 0;
	int unread_news = FALSE;	
	long epoch;

	switch (check_start_save) {
		case CHECK_ANY_NEWS:
			if (verbose) {
				wait_message (txt_checking_for_news);
			}
			break;
		case START_ANY_NEWS:
			wait_message (txt_checking_for_news);
			break;
		case MAIL_ANY_NEWS:
		case SAVE_ANY_NEWS:
			sprintf (logfile, "%s/log", rcdir);
			if ((fp_log = fopen (logfile, "w")) == NULL) {
				perror_message (txt_cannot_open, logfile);
				fp_log = stdout;
				verbose = FALSE;
				log_opened = FALSE;
			}
			time (&epoch);
			fprintf (fp_log, "To: %s\n", userid);
			fprintf (fp_log, "Subject: NEWS LOG %s\n", ctime (&epoch));
			break;
	}
	
	for (i = 0; i < group_top; i++) {
		make_group_path (active[my_group[i]].name, group_path);
		if (! index_group (active[my_group[i]].name, group_path)) {
			continue;
		}
		read_newsrc_line (active[my_group[i]].name);
		print_group = TRUE;
		check_arts = 0;

		for (j = 0; j < top; j++) {
			if (arts[j].unread == ART_UNREAD)  {
				switch (check_start_save) {
					case CHECK_ANY_NEWS:
						if (print_first && verbose) {
							fputc ('\n', stdout);
							print_first = FALSE;
						}
						check_arts++;
						break;
					case START_ANY_NEWS:
						return i;	/* return first group with unread news */ 
						/* NOTREACHED */
					case MAIL_ANY_NEWS:
					case SAVE_ANY_NEWS:
						if (print_group) {	
							sprintf (buf, "Saved %s...\n", active[my_group[i]].name);
							fprintf (fp_log, "%s", buf);
							if (verbose) {
								wait_message (buf);
							}
							print_group = FALSE;
							saved_groups++;
							if (check_start_save == SAVE_ANY_NEWS) {
								sprintf (buf, "%s/dummy", group_path);
								create_path (buf);
							}
						}

						if (check_start_save == MAIL_ANY_NEWS) {
							sprintf (savefile, "%stin.%d", TMPDIR, process_id);
						} else {
							k = my_group[cur_groupnum];
							if (! strfpath (active[k].attribute.savedir,
							    path, sizeof (path), homedir, (char *) 0,
							    (char *) 0, active[k].name)) {
								joinpath (path, homedir, DEFAULT_SAVEDIR);
							}
							sprintf (savefile, "%s/%s/%ld", path, group_path, arts[j].artnum);
						}

						note_page = art_open (arts[j].artnum, group_path);	
						if (note_page == ART_UNAVAILABLE) {
							continue;
						}

						if ((fp = fopen (savefile, "w")) == NULL) {
							fprintf (fp_log, txt_cannot_open, savefile);
							if (verbose) {
								perror_message (txt_cannot_open, savefile);
							}
							continue;
						}

						if (check_start_save == MAIL_ANY_NEWS) {
							fprintf (fp, "To: %s\n", mail_news_user);
						}

						sprintf (buf, "[%5ld]  %s\n", arts[j].artnum, arts[j].subject);
						fprintf (fp_log, "%s", buf);
						if (verbose) {
							wait_message (buf);
						}
						fseek (note_fp, 0L, 0);
						copy_fp (note_fp, fp, "");
						art_close ();
						fclose (fp);
						saved_arts++;

						if (check_start_save == MAIL_ANY_NEWS) {
#ifdef AMIGA
							sprintf (buf, "%s <%s -f %s", 
								mailer, savefile, userid);
#else
							sprintf (buf, "%s \"%s\" < %s", mailer,
								mail_news_user, savefile);
#endif	/* AMIGA */
							if (! invoke_cmd (buf)) {
								error_message (txt_command_failed_s, buf);
							}
							unlink (savefile);
						}
						if (catchup) {
							arts[j].unread = ART_READ;
						}
						break;
				}
			}
		}
		
		if (check_start_save == MAIL_ANY_NEWS ||
			check_start_save == SAVE_ANY_NEWS) {
			if (catchup) {
				update_newsrc (active[my_group[i]].name, my_group[i], FALSE);
			}
		} else {
			if (check_arts) {
				if (verbose) {
					sprintf (buf, "%4d unread articles in %s\n",
						check_arts, active[my_group[i]].name);
					wait_message (buf); 	
				}
				unread_news = TRUE;	
			}
		}
	}
	switch (check_start_save) {
		case CHECK_ANY_NEWS:
			if (unread_news) {
				return 2;
			} else {
				if (verbose) {
					wait_message (txt_there_is_no_news);
				}
				return 0;
			}
			/* NOTREACHED */ 
		case START_ANY_NEWS:
			wait_message (txt_there_is_no_news);
			return -1;
			/* NOTREACHED */ 
		case MAIL_ANY_NEWS:
		case SAVE_ANY_NEWS:
			sprintf (buf, "\n%s %d article(s) from %d group(s)\n", 
				(check_start_save == MAIL_ANY_NEWS ? "Mailed" : "Saved"),
				saved_arts, saved_groups);
			fprintf (fp_log, "%s", buf);
			if (verbose) {
				wait_message (buf);
			}
			if (log_opened) {
				fclose (fp_log);
				if (verbose) {
					sprintf (buf, "Mailing log to %s\n",
						(check_start_save == MAIL_ANY_NEWS ? mail_news_user : userid));
					wait_message (buf);
				}
#ifdef AMIGA
				sprintf (buf, "%s <%s -f %s", mailer, logfile, userid);
#else
				sprintf (buf, "%s \"%s\" < %s", mailer,
					(check_start_save == MAIL_ANY_NEWS ? mail_news_user : userid),
					logfile);
#endif	/* AMIGA */
				if (! invoke_cmd (buf)) {
					error_message (txt_command_failed_s, buf);
				}
			}
			break;
	}

#endif /* INDEX_DAEMON */

	return 0;
}


int save_art_to_file (respnum, index, mailbox, filename)
	int respnum;
	int index;
	int mailbox;
	char *filename;
{
#ifndef INDEX_DAEMON

	char file[PATH_LEN];
	char save_art_info[LEN];
	FILE *fp;
	int is_mailbox = FALSE;
	int i = 0, ret_code = FALSE;
	long epoch;
	
	if (debug == 2) {
		sprintf (msg, "Save respnum=[%d] index=[%d] mbox=[%d] file=[%s]", 
			respnum, index, mailbox, filename);
		error_message (msg, "");
	}

	if (strlen (filename)) {
		my_strncpy (file, filename, sizeof (file));
		is_mailbox = mailbox;
		i = index;
	} else if (default_auto_save && arts[respnum].archive) {
		my_strncpy (file, arts[respnum].archive, sizeof (file));
	}

	if (! append_to_existing_file (i)) {
		save[i].saved = FALSE;
		info_message (txt_art_not_saved);
		sleep (1);
		return (ret_code);
	}

	if (debug == 2) {
		error_message ("Save file=[%s]", save_filename (i));
		sleep (3);
	}

	if ((fp = fopen (save_filename (i), "a+")) == NULL) {
		save[i].saved = FALSE;
		info_message (txt_art_not_saved);
		return (ret_code);
	}

 	time (&epoch);
 	fprintf (fp, "From %s %s", note_h_path, ctime (&epoch));

	if (fseek (note_fp, 0L, 0) == -1) {
		perror_message ("fseek() error on [%s]", arts[respnum].subject);
	}
	copy_fp (note_fp, fp, "");

	print_art_seperator_line (fp, mailbox);

	fclose (fp);
	fseek (note_fp, note_mark[note_page], 0);

	save[i].saved = TRUE;

	if (filename == (char *) 0) {
		if (is_mailbox) {
			sprintf (save_art_info, txt_saved_to_mailbox, get_first_savefile ());
		} else {
			sprintf (save_art_info, txt_art_saved_to, get_first_savefile ());
		}
		info_message (save_art_info);
	}

#endif /* INDEX_DAEMON */

	return TRUE;
}


int save_thread_to_file (is_mailbox, group_path)
	int is_mailbox;
	char *group_path;
{
#ifndef INDEX_DAEMON

	char file[PATH_LEN];
	char save_thread_info[LEN];
	char *first_savefile;
	int count = 0;
	int i, ret_code = FALSE;

	for (i=0 ; i < num_save ; i++) {
		sprintf (msg, "%s%d", txt_saving, ++count);
		wait_message (msg);

		if (is_mailbox) {
			file[0] = 0;
		}else {
			sprintf (file, "%s.%02d", save[i].file, i+1);
		}

		note_page = art_open (arts[save[i].index].artnum, group_path);
		if (note_page != ART_UNAVAILABLE) {
			ret_code = save_art_to_file (save[i].index, i, is_mailbox, file);
			art_close ();			
		}
	}
	
	first_savefile = get_first_savefile ();

	if (first_savefile == (char *) 0) {
		info_message (txt_thread_not_saved);
	} else {
		if (is_mailbox) {
			sprintf (save_thread_info, txt_saved_to_mailbox, first_savefile);
		} else {
			if (num_save == 1) {
				sprintf (save_thread_info, txt_art_saved_to, first_savefile);
			} else {
				sprintf (save_thread_info, txt_thread_saved_to_many,
					first_savefile, get_last_savefile ());
			}
			if (first_savefile != (char *) 0) {
				free (first_savefile);
				first_savefile = (char *) 0;
			}
		}
		info_message (save_thread_info);
		sleep (2);
	}

#endif /* INDEX_DAEMON */

	return TRUE;
}


int save_regex_arts (is_mailbox, group_path)
	int is_mailbox;
	char *group_path;
{
#ifndef INDEX_DAEMON

	char buf[PATH_LEN];
	int i, ret_code = FALSE; 	
	
	for (i=0 ; i < num_save ; i++) {
		sprintf(msg, "%s%d", txt_saving, i+1);
		wait_message (msg);

		if (is_mailbox) {
			buf[0] = 0;
		}else {
			sprintf (buf, "%s.%02d", save[i].file, i+1);
		}

		note_page = art_open (arts[save[i].index].artnum, group_path);
		if (note_page != ART_UNAVAILABLE) {
			ret_code = save_art_to_file (save[i].index, i, is_mailbox, buf);
			art_close ();			
		}
	}

	if (! num_save) {	
		info_message (txt_no_match);
	} else {
		if (is_mailbox) {
			sprintf (buf, txt_saved_to_mailbox, get_first_savefile ());
		} else {
			sprintf (buf,txt_saved_pattern_to,
				get_first_savefile (), get_last_savefile ());
		}
		info_message (buf);
	}

	return (ret_code);

#else

	return (FALSE);
	
#endif /* INDEX_DAEMON */
}


int append_to_existing_file (i)
	int i;
{
#ifndef INDEX_DAEMON

	char buf[LEN];
	char *file;
	struct stat st;

	if (! save[i].is_mailbox) {
		file = save_filename (i);
		if (stat(file, &st) != -1) {	
			sprintf (buf, txt_append_to_file, file); 
			if (! prompt_yn (LINES, buf, 'n')) {
				if (file != (char *) 0) {
					free (file);
					file = (char *) 0;
				}
				return FALSE;
			}
		}
		if (file != (char *) 0) {
			free (file);
			file = (char *) 0;
		}
	}

#endif /* INDEX_DAEMON */
	
	return TRUE;
}


int create_path (path)
	char *path;
{
	int mbox_format = FALSE;

#ifndef INDEX_DAEMON

	char tmp[PATH_LEN];
	char buf[PATH_LEN];
	int i, j, len = 0;
	struct stat st;
	
	i = my_group[cur_groupnum];

	/*
	 * expand "$var..." first, so variables starting with 
	 * '+', '$' or '=' will be processed correctly later
	 */
	if (path[0] == '$') {
		if (strfpath (path, buf, sizeof (buf), homedir,
		    (char *) 0, (char *) 0, active[i].name)) {
			my_strncpy (path, buf, PATH_LEN);
		}
	}

	/*
	 * save in mailbox format to ~/Mail/<group.name> or
	 * attribute.maildir for current group
	 */
	if (path[0] == '=') {
		mbox_format = TRUE;
		strcpy (tmp, path);
		if (! strfpath (active[i].attribute.maildir, buf, sizeof (buf), 
		    homedir, (char *) 0, (char *) 0, active[i].name)) {
			joinpath (buf, homedir, DEFAULT_MAILDIR);
		}
		sprintf (path, "%s/dummy", buf);
	} else {
		if (! strchr ("~$=+/.", path[0])) {
			if (! strfpath (active[i].attribute.savedir, buf, sizeof (buf), 
			    homedir, (char *) 0, (char *) 0, active[i].name)) {
				joinpath (buf, homedir, DEFAULT_SAVEDIR);
			}
			sprintf (tmp, "%s/%s", buf, path);
			my_strncpy (path, tmp, PATH_LEN);
		}
		if (strfpath (path, buf, sizeof (buf), homedir,
		    (char *) 0, active[i].attribute.savedir, active[i].name)) {
		    	my_strncpy (path, buf, PATH_LEN);
		}
	}

	/*
	 *  create any directories, otherwise check
	 *  errno and give appropiate error message
	 */
	len = (int) strlen (path);
	
	for (i=0, j=0 ; i < len ; i++, j++) {
		buf[j] = path[i];
		if (i+1 < len && path[i+1] == '/') {
			buf[j+1] = '\0';
			if (stat (buf, &st) == -1) {
				if (mkdir (buf, 0755) == -1) {
					if (errno != EEXIST) {
						perror_message ("Cannot create %s", buf);
						return FALSE;
					}
				}
			}
		}
	}

	if (mbox_format) {
		strcpy (path, tmp);
	}
	
#endif /* INDEX_DAEMON */
	
	return (mbox_format);
}


int create_sub_dir (i)
	int i;
{
#ifndef INDEX_DAEMON

	char dir[LEN];
	struct stat st;

	if (! save[i].is_mailbox && save[i].archive) {
		joinpath (dir, save[i].dir, save[i].archive);
		if (stat (dir, &st) == -1) {
			mkdir (dir, 0755);
			return TRUE;
		}
#ifdef AMIGA		
		if (st.st_attr & ST_DIRECT) {
#else
		if ((st.st_mode & S_IFMT) == S_IFDIR) {
#endif
			return TRUE;
		} else {
			return FALSE;
		}
	}

#endif /* INDEX_DAEMON */
	
	return FALSE;
}

/*
 *  add files to be saved to save array
 */

void add_to_save_list (index, article, is_mailbox, archive_save, path)
	int index;
	struct article_t *article;
	int is_mailbox;
	int archive_save;
	char *path;
{
#ifndef INDEX_DAEMON
	char tmp[PATH_LEN];
	char dir[PATH_LEN];
	char file[PATH_LEN];
	int i;
	
	dir[0] = '\0';
	file[0] = '\0';

	if (num_save == max_save-1) {
		expand_save ();
	}

	save[num_save].index   = index;
	save[num_save].saved   = FALSE;
	save[num_save].is_mailbox = is_mailbox;
	save[num_save].dir     = (char *) 0;
	save[num_save].file    = (char *) 0;
	save[num_save].archive = (char *) 0;
	save[num_save].part    = (char *) 0;
	save[num_save].patch   = (char *) 0;

	save[num_save].subject = str_dup (article->subject);
	if (archive_save && article->archive) {
		save[num_save].archive = str_dup (article->archive);
		if (article->part) {
			save[num_save].part = str_dup (article->part);
		}
		if (article->patch) {
			save[num_save].patch = str_dup (article->patch);
		}
	}

	if (is_mailbox) {
		if ((int) strlen (path) > 1) {
			if (path[0] == '=') {
				my_strncpy (file, path+1, sizeof (file));
			} else {
				my_strncpy (file, path, sizeof (file));
			}
		} else {
			my_strncpy (file, glob_group, sizeof (file));
		}

		i = my_group[cur_groupnum];
		if (! strfpath (active[i].attribute.maildir, tmp, sizeof (tmp), 
		    homedir, (char *) 0, (char *) 0, active[i].name)) {
			joinpath (tmp, homedir, DEFAULT_MAILDIR);
		}
		save[num_save].dir = str_dup (tmp);
		save[num_save].file = str_dup (file);
	} else {
		if (path[0]) {
			for (i=strlen (path) ; i ; i--) {
				if (path[i] == '/') {
					strncpy (dir, path, i);
					dir[i] = '\0';
					strcpy (file, path+i+1);
					break;
				}
			}
		}
		
		if (dir[0]) {
			save[num_save].dir = str_dup (dir);
		} else {
			i = my_group[cur_groupnum];
			if (! strfpath (active[i].attribute.savedir, tmp, sizeof (tmp), 
			    homedir, (char *) 0, (char *) 0, active[i].name)) {
				joinpath (tmp, homedir, DEFAULT_SAVEDIR);
			}	
			save[num_save].dir = str_dup (tmp);
		}

		if (file[0]) {
			save[num_save].file = str_dup (file);
		} else {
			if (path[0]) {
				save[num_save].file = str_dup (path);
			} else {
				save[num_save].file = str_dup (save[num_save].archive);
			}
		}
	}
	num_save++;

#endif /* INDEX_DAEMON */
}

/*
 *  print save array of files to be saved
 */

void sort_save_list ()
{
	qsort ((char *) save, num_save, sizeof (struct save_t), save_comp);
	debug_save_comp ();
}

/*
 *  string comparison routine for the qsort()
 *  ie. qsort(array, 5, 32, save_comp);
 */

int save_comp (p1, p2)
	char *p1;
	char *p2;
{
	struct save_t *s1 = (struct save_t *) p1;
	struct save_t *s2 = (struct save_t *) p2;

	/*
	 * Sort on Archive-name: part & patch otherwise Subject: 
	 */
	if (s1->archive != (char *) 0) {
		if (s1->part != (char *) 0) {
			if (s2->part != (char *) 0) {
				if (strcmp (s1->part, s2->part) < 0) {
					return -1;
				}
				if (strcmp (s1->part, s2->part) > 0) {
					return 1;
				}
			} else {
				return 0;
			}	
		} else if (s1->patch != (char *) 0) {
			if (s2->patch != (char *) 0) {
				if (strcmp (s1->patch, s2->patch) < 0) {
					return -1;
				}
				if (strcmp (s1->patch, s2->patch) > 0) {
					return 1;
				}
			} else {
				return 0;
			}	
		}	
#if 0
		if (s1->part != (char *) 0) {
			if (strcmp (s1->part, s2->part) < 0) {
				return -1;
			}
			if (strcmp (s1->part, s2->part) > 0) {
				return 1;
			}
		} else {
			if (strcmp (s1->patch, s2->patch) < 0) {
				return -1;
			}
			if (strcmp (s1->patch, s2->patch) > 0) {
				return 1;
			}
		}	
#endif
	} else {
		if (strcmp (s1->subject, s2->subject) < 0) {
			return -1;
		}
		if (strcmp (s1->subject, s2->subject) > 0) {
			return 1;
		}
	}
	
	return 0;
}


char *save_filename (i)
	int i;
{
	char *filename;

	filename = (char *) my_malloc (PATH_LEN);

	if (save[i].is_mailbox) {
		sprintf (filename, "%s/%s", save[i].dir, save[i].file);
		return (filename);
	}
	
	if (! default_auto_save || (! save[i].part && ! save[i].patch)) {
		if (num_save == 1) {
			sprintf (filename, "%s/%s", save[i].dir, save[i].file);
		} else {
			sprintf (filename, "%s/%s.%02d", save[i].dir, save[i].file, i+1);
		}
	} else {
		if (save[i].part) {
			if (create_sub_dir (i)) {
				sprintf (filename, "%s/%s/%s.%s%s", save[i].dir, save[i].archive, save[i].archive, LONG_PATH_PART, save[i].part);
			} else {
				sprintf (filename, "%s/%s.%s%s", save[i].dir, save[i].archive, LONG_PATH_PART, save[i].part);
			}
		} else {
			if (save[i].patch) {
				if (create_sub_dir (i)) {
					sprintf (filename, "%s/%s/%s.%s%s", save[i].dir, save[i].archive, save[i].archive, LONG_PATH_PATCH, save[i].patch);
				} else {
					sprintf (filename, "%s/%s.%s%s", save[i].dir, save[i].archive, LONG_PATH_PATCH, save[i].patch);
				}
			} else {
	 	 		sprintf (filename, "%s/%s", save[i].dir, save[i].file);
			}
		}
	}

	return (filename);
}


char *get_first_savefile ()
{
	char *file;
	int i;

	for (i=0 ; i < num_save ; i++) {
		if (save[i].saved) {
			file = (char *) my_malloc (PATH_LEN);
			if (save[i].is_mailbox) {
				sprintf (file, "%s/%s", save[i].dir, save[i].file);
				return (file);
			} else {
				if (save[i].archive && default_auto_save) {
					if (save[i].part) {
						if (create_subdir) {
							sprintf (file, "%s/%s.%s%s", save[i].archive, save[i].archive, LONG_PATH_PART, save[i].part);
						} else {
							sprintf (file, "%s.%s%s", save[i].archive, LONG_PATH_PART, save[i].part);
						}
					} else {
						if (create_subdir) {
							sprintf (file, "%s/%s.%s%s", save[i].archive, save[i].archive, LONG_PATH_PATCH, save[i].patch);
						} else {
							sprintf (file, "%s.%s%s", save[i].archive, LONG_PATH_PATCH, save[i].patch);
						}
					}
				} else {
					if (num_save == 1) {
						sprintf (file, "%s", save[i].file);
					} else {
						sprintf (file, "%s.%02d", save[i].file, i+1);
					}
				}
				return (file);
			}
		}
	}
	return ((char *) 0);
}


char *get_last_savefile ()
{
	char *file;
	int i;
	
	for (i=num_save-1 ; i >= 0 ; i--) {
		if (save[i].saved) {
			file = (char *) my_malloc (PATH_LEN);
			if (save[i].is_mailbox) {
				sprintf (file, "%s/%s", save[i].dir, save[i].file);
				return (file);
			} else {
				if (save[i].archive && default_auto_save) {
					if (save[i].part) {
						if (create_subdir) {
							sprintf (file, "%s/%s.%s%s", save[i].archive, save[i].archive, LONG_PATH_PART, save[i].part);
						} else {
							sprintf (file, "%s.%s%s", save[i].archive, LONG_PATH_PART, save[i].part);
						}
					} else {
						if (create_subdir) {
							sprintf (file, "%s/%s.%s%s", save[i].archive, save[i].archive, LONG_PATH_PATCH, save[i].patch);
						} else {
							sprintf (file, "%s.%s%s", save[i].archive, LONG_PATH_PATCH, save[i].patch);
						}
					}
				} else {
					if (num_save == 1) {
						sprintf (file, "%s", save[i].file);
					} else {
						sprintf (file, "%s.%02d", save[i].file, i+1);
					}
				}
				return (file);
			}
		}
	}
	return ((char *) 0);
}


int post_process_files (proc_type_ch)
	int proc_type_ch;
{
	if (num_save) {
		wait_message (txt_post_processing);

		switch (proc_type_ch) {
			case 's':
				post_process_sh ();
				break;
			case 'u':
				post_process_uud (POST_PROC_UUDECODE);
				break;
			case 'l':
				post_process_uud (POST_PROC_UUD_LST_ZOO);
				break;
			case 'e':
				post_process_uud (POST_PROC_UUD_EXT_ZOO);
				break;
			case 'L':
				post_process_uud (POST_PROC_UUD_LST_ZIP);
				break;
			case 'E':
				post_process_uud (POST_PROC_UUD_EXT_ZIP);
				break;
		}

		info_message (txt_post_processing_finished);
		sleep (1);
		return TRUE;
	}
	return FALSE;
}


void post_process_uud (pp)
	int pp;
{
#ifndef INDEX_DAEMON

	char s[LEN], t[LEN], u[LEN];
	char buf[LEN], *file;
	char file_out[PATH_LEN];
	char file_out_dir[PATH_LEN];
	FILE *fp_in, *fp_out;
	int i, state = INITIAL;
	int file_size = 0;
	struct stat st;
	
	t[0] = '\0';
	u[0] = '\0';

	my_strncpy (file_out_dir, save_filename (0), 
		sizeof (file_out_dir));
	for (i=strlen(file_out_dir) ; i > 0 ; i--) {
		if (file_out_dir[i] == '/') {
			file_out_dir[i] = '\0';
			break;
		}
	}

	sprintf (file_out, "%s/tin.%05d", file_out_dir, process_id);
	
	if ((fp_out = fopen (file_out, "a+")) == NULL) {
		perror_message (txt_cannot_open, file_out);
	}


	for (i=0 ; i < num_save ; i++) {
		my_strncpy (buf, save_filename (i), sizeof (buf));

		if ((fp_in = fopen (buf, "r")) != NULL) {
			if (fgets (s, sizeof s, fp_in) == NULL) {
				fclose (fp_in);
				continue;
			}
			while (state != END) { 
				switch (state) {
					case INITIAL:
						if (! strncmp ("begin", s, 5)) {
							state = MIDDLE;
							fprintf (fp_out, "%s", s);
						}
						break;

					case MIDDLE:
						if (s[0] == 'M') {
							fprintf (fp_out, "%s", s);
						} else if (strncmp("end", s, 3)) {
							state = OFF;
						} else { /* end */
							state = END;
							if (u[0] != 'M') {
								fprintf (fp_out, "%s", u);
							}
							if (t[0] != 'M') {
							    fprintf (fp_out, "%s", t);
							}
							fprintf (fp_out, "%s\n", s);
						}
						break;

					case OFF:
						if ((s[0] == 'M') && (t[0] == 'M') && (u[0] == 'M')) {
							fprintf (fp_out, "%s", u);
							fprintf (fp_out, "%s", t);
							fprintf (fp_out, "%s", s);
							state = MIDDLE;
						} else if (! strncmp ("end", s, 3)) {
							state = END;
							if (u[0] != 'M') {
							    fprintf (fp_out, "%s", u);
							}
							if (t[0] != 'M') {
							    fprintf (fp_out, "%s", t);
							}
							fprintf (fp_out, "%s\n", s);
						}
						break;

					case END:
						break;

					default:
						fprintf (stderr, "\r\nerror: ASSERT - default state\n");
						fclose (fp_in);
						fclose (fp_out);
						unlink (file_out);
						return;
				}
				strcpy (u,t);
				strcpy (t,s);
				/*
				 *  read next line & if error goto next file in save array
				 */
				if (fgets (s, sizeof s, fp_in) == NULL) {
					break;
				}
			}
			fclose (fp_in);
		}
	}
	fclose (fp_out);

	/*
	 *  uudecode file
	 */
	wait_message (txt_uudecoding);
	
	sprintf (buf, "cd %s; uudecode %s", file_out_dir, file_out); 
	if (invoke_cmd (buf)) {
		/*
		 *  Sum file
		 */
		if ((file = get_archive_file (file_out_dir, "*")) != (char *) 0) { 
			sprintf (buf, "%s %s", DEFAULT_SUM, file); 
			printf (txt_checksum_of_file, file); 
			fflush (stdout);
			if ((fp_in = (FILE *) popen (buf, "r")) == NULL) {
				printf ("Cannot execute %s\r\n", buf); 
				fflush (stdout);
			} else {
				if (stat (file, &st) != -1) {
					file_size = (int) st.st_size;
				}
				if (fgets (buf, sizeof buf, fp_in) != NULL) {
					buf[strlen (buf)-1] = '\0';
				}
				pclose (fp_in);
				printf ("%s  %8d bytes\r\n", buf, file_size); 
				fflush (stdout);
			}

			if (pp > POST_PROC_UUDECODE) {
				/*
				 *  Test archive integrity
				 */
				if (pp > POST_PROC_UUDECODE && archiver[pp].test != (char *) 0) {
					i = (pp == POST_PROC_UUD_LST_ZOO || pp == POST_PROC_UUD_EXT_ZOO ? 3 : 4);
					sprintf (buf, "cd %s; %s %s %s", file_out_dir,
						archiver[i].name, archiver[i].test, file);
					printf (txt_testing_archive, file); 
					fflush (stdout);
					if (! invoke_cmd (buf)) {
						error_message (txt_post_processing_failed, "");
					}
				}
				/*
				 *  List archive
				 */
				if (pp == POST_PROC_UUD_LST_ZOO || pp == POST_PROC_UUD_LST_ZIP) {
					i = (pp == POST_PROC_UUD_LST_ZOO ? 3 : 4);
					sprintf (buf, "cd %s; %s %s %s", file_out_dir,
						archiver[i].name, archiver[i].list, file);
					printf (txt_listing_archive, file); 
					fflush (stdout);
					if (! invoke_cmd (buf)) {
						error_message (txt_post_processing_failed, "");
					}
					sleep (3);
				}
				/*
				 *  Extract archive
				 */
				if (pp == POST_PROC_UUD_EXT_ZOO || pp == POST_PROC_UUD_EXT_ZIP) {
					i = (pp == POST_PROC_UUD_EXT_ZOO ? 3 : 4);
					sprintf (buf, "cd %s; %s %s %s", file_out_dir,
						archiver[i].name, archiver[i].extract, file);
					printf (txt_extracting_archive, file);
					fflush (stdout);
					if (! invoke_cmd (buf)) {
						error_message (txt_post_processing_failed, "");
					}
					sleep (3);
				}
	
				if (file != (char *) 0) {
					free (file);
					file = (char *) 0;
				}
			}
		}
	}

	delete_processed_files ();

	unlink (file_out);

#endif /* INDEX_DAEMON */
}

/*
 *  Unpack /bin/sh archives
 */
 
void post_process_sh ()
{
#ifndef INDEX_DAEMON

	char buf[LEN];
	char file_in[PATH_LEN];
	char file_out[PATH_LEN];
	char file_out_dir[PATH_LEN];
	char *ptr1, *ptr2;
	char sh_pattern_1[16];
	char sh_pattern_2[16];
	FILE *fp_in, *fp_out;
	int found_header;
	int i, j;
	int patlen1, patlen2;

	strcpy (sh_pattern_1, "#! /bin/sh");
	strcpy (sh_pattern_2, "#!/bin/sh");

	my_strncpy (file_out_dir, save_filename (0), sizeof (file_out_dir));
	for (i=strlen(file_out_dir) ; i > 0 ; i--) {
		if (file_out_dir[i] == '/') {
			file_out_dir[i] = '\0';
			break;
		}
	}

	sprintf (file_out, "%s/tin.%05d", file_out_dir, process_id);

	for (j=0 ; j < num_save ; j++) {
		my_strncpy (file_in, save_filename (j), sizeof (file_in));

		printf (txt_extracting_shar, file_in);
		fflush (stdout);

		found_header = FALSE;
		
		if ((fp_out = fopen (file_out, "w")) != NULL) {
			if ((fp_in = fopen (file_in, "r")) != NULL) {
				ptr1 = sh_pattern_1;
				ptr2 = sh_pattern_2;
				patlen1 = strlen (sh_pattern_1);
				patlen2 = strlen (sh_pattern_2);
				while (! feof (fp_in)) {
					if (fgets (buf, sizeof buf, fp_in)) {
						/*
						 *  find #!/bin/sh or #! /bin/sh pattern
						 */
						if (!found_header) {
							if (str_str (buf, ptr1, patlen1) != 0 ||
								str_str (buf, ptr2, patlen2) != 0) {
								found_header = TRUE;
							}
						}
					
						/*
						 *  Write to temp file
						 */
						if (found_header) {
							fputs (buf, fp_out);
						}
					}
				}
				fclose (fp_in);
			}
			fclose (fp_out);

			sprintf (buf, "cd %s; sh %s", file_out_dir, file_out); 
			fputs ("\r\n", stdout);
			fflush (stdout);
			Raw (FALSE);
			invoke_cmd (buf);
			Raw (TRUE);
			unlink (file_out);
		}
	}
	delete_processed_files ();

#endif /* INDEX_DAEMON */
}


char *get_archive_file (dir, ext)
	char *dir;
	char *ext;
{
	char buf[LEN];
	char *file = (char *) 0;
	FILE *fp;
	
	sprintf (buf, "ls -t %s/%s", dir, ext);

	if ((fp = (FILE *) popen (buf, "r")) == NULL) {
		return (char *) 0;
	}

	if (fgets (buf, sizeof buf, fp) != NULL) {
		file = str_dup (buf);
		file[strlen (file)-1] = '\0';
	}
	
	pclose (fp);

	return (file);
}


void delete_processed_files ()
{
#ifndef INDEX_DAEMON

	int delete = FALSE;
	int i;

	if (active[my_group[cur_groupnum]].attribute.delete_tmp_files) {
		delete = TRUE;
	} else if (prompt_yn (LINES, txt_delete_processed_files, 'y')) {
		delete = TRUE;
	}
	
	if (delete) {
		wait_message ("\r\n");
		wait_message (txt_deleting);

		for (i=0 ; i < num_save ; i++) {
			unlink (save_filename (i));
		}
	}
	
#endif	/* INDEX_DAEMON */
}


void print_art_seperator_line (fp, mailbox)
	FILE *fp;
	int mailbox;
{
	int sep = 0x01;	/* Ctrl-A */

	if (debug == 2) {	
		sprintf (msg, "Mailbox=[%d]  MMDF=[%d]", mailbox, save_to_mmdf_mailbox);
		error_message (msg, "");
	}
	
	if (mailbox && save_to_mmdf_mailbox) {
		fprintf (fp, "%c%c%c%c\n", sep, sep, sep, sep);
	} else {
		fputc ('\n', fp);
	}
}
