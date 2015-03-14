/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : active.c
 *  Author    : I.Lea
 *  Created   : 16-02-92
 *  Updated   : 05-12-92
 *  Notes     :
 *  Copyright : (c) Copyright 1991-92 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

char new_active_file_server[PATH_LEN];
char new_active_file_attribute[32];
int group_hash[TABLE_SIZE];			/* group name --> active[] */
int reread_active_file = FALSE;
int active_index = -1;

/*
 *  Resync active file when SIGALRM signal received that
 *  is triggered by alarm (reread_active_file_secs) call.
 */

void resync_active_file ()
{
	char old_group[PATH_LEN];

	if (reread_active_file) {
		if (cur_groupnum >= 0 && group_top) {
			strcpy (old_group, active[my_group[cur_groupnum]].name);
		} else {
			old_group[0] = '\0';
		}
		free_active_arrays ();
		max_active = DEFAULT_ACTIVE_NUM;
		expand_active ();
		read_mail_active_file ();
		read_news_active_file ();
		read_attributes_file ();
		read_mailgroups_file ();
		read_newsgroups_file ();
		if (! read_cmd_line_groups ()) {
			read_newsrc (TRUE);
			toggle_my_groups (show_only_unread_groups, old_group);
		}
		set_groupname_len (FALSE);
		set_alarm_signal ();
		mail_setup ();
		group_selection_page ();
	}
}

/*
 *  Find group name in active[] array and return index otherwise -1
 */
 
int find_group_index (group)
	char *group;
{
	int i = -1;
	long h;

	h = hash_groupname (group);

	i = group_hash[h];
	
	/* 
	 * hash linked list chaining 
	 */
	while (i >= 0) {
		if (strncmp (group, active[i].name, strlen (active[i].name)) == 0) {
			return (i);
		}
		i = active[i].next;
	}

	return (-1);
}

/*
 * parse line from news or mail active files
 */
 
int parse_active_line (line, max, min, moderated)
	char *line;
	long *max;
	long *min;
	char *moderated;
{
	char *p, *q, *r;
	
	if (line[0] == '#' || line[0] == '\n') {
		return FALSE;
	}
	
	for (p = line; *p && *p != ' ' && *p != '\n'; p++) {
		continue;
	}
	if (*p != ' ') {
		error_message (txt_bad_active_file, line);
		return FALSE;
	}
	*p++ = '\0';

	for (q = p; *q && *q != ' '; q++) {
		continue;
	}
	if (*q != ' ') {
		error_message (txt_bad_active_file, line);
		return FALSE;;
	}
	*q++ = '\0';

	for (r = q; *r && *r != ' '; r++) {
		continue;
	}
	if (*r != ' ') {
		error_message (txt_bad_active_file, line);
		return FALSE;;
	}
	*r++ = '\0';

	if (*r) {
		strcpy (moderated, r);
		r = (char *) strchr (moderated, '\n');
		if (*r) {
			*r = '\0';
		}
	}

	*max = (long) atol (p);
	*min = (long) atol (q);

	return TRUE;
}

/*
 *  Load the news active file into active[] and create copy 
 *  of active ~/.tin/active
 */

void read_news_active_file ()
{
	FILE *fp;
	char buf[LEN];
	char moderated[PATH_LEN];
	int i;
	long h, min, max;
		
	if ((update && update_fork) || ! update) {
		wait_message (txt_reading_news_active_file);
	}

	if ((fp = open_news_active_fp ()) == NULL) {
		if (compiled_with_nntp) {
			sprintf (msg, txt_cannot_open_active_file, news_active_file, progname);
			error_message (msg, "");
		} else {
			if (cmd_line) {
				fputc ('\n', stderr);
			}
			error_message (txt_cannot_open, news_active_file);
		}
		tin_done (1);
	}

	if (num_active == -1) {
		num_active = 0;
		for (i = 0; i < TABLE_SIZE; i++) {
			group_hash[i] = -1;
		}
	}
	strcpy (moderated, "y");

	while (fgets (buf, sizeof (buf), fp) != NULL) {
		if (! parse_active_line (buf, &max, &min, moderated)) {
			continue;
		}

		/*
		 * Don't load group into active[] from active file if 
		 * 'x'  junked group
		 * '='  aliased group
		 */
		if (moderated[0] != 'x' && moderated[0] != '=') {
			/*
			 * Load group into group hash table
			 */
			if (num_active >= max_active) {
				expand_active ();
			}

			h = hash_groupname (buf);

			if (group_hash[h] == -1) {
				group_hash[h] = num_active;
			} else {	/* hash linked list chaining */
				for (i=group_hash[h]; active[i].next >= 0; i=active[i].next) {
					if (strcmp(active[i].name, buf) == 0) {
						goto read_news_active_continue;	/* kill dups */
					}
				}
				if (strcmp(active[i].name, buf) == 0)
					goto read_news_active_continue;
				active[i].next = num_active;
			}

			/*
			 * Load group info.
			 */
			active[num_active].type = GROUP_TYPE_NEWS;
			active[num_active].name = str_dup (buf);
			active[num_active].spooldir = spooldir;
			active[num_active].description = (char *) 0;
			active[num_active].max = max;
			active[num_active].min = min;
			active[num_active].moderated = moderated[0];
			active[num_active].next = -1;			/* hash chaining */
			active[num_active].my_group = UNSUBSCRIBED;	/* not in my_group[] yet */
			active[num_active].unread = 0;
#ifdef INDEX_DAEMON			
			active[num_active].last_updated_time = 0L;
#endif
			num_active++;
		}
read_news_active_continue:;
	}
	fclose (fp);

	/*
	 *  exit if active file is empty
	 */
	if (! num_active) {
		error_message (txt_active_file_is_empty, news_active_file);
		tin_done (1);
	}

	if ((cmd_line && ! update && ! verbose) || (update && update_fork)) {
		wait_message ("\n");
	}

	check_for_any_new_groups ();
}

/*
 *  create ~/.tin/active if it does not exist (local news only) 
 */

void backup_active (create)
	int create;
{
	char buf[PATH_LEN];
	FILE *fp;
	register int i;
	struct stat sb;
	
	joinpath (buf, rcdir, "active");
	
	if (create) {
		if (stat (buf, &sb) != -1) {
			return;
		}
	}
	
	if ((fp = fopen (buf, "w")) != (FILE *) 0) {
		for (i = 0; i < num_active ; i++) {	/* for each group */
			fprintf (fp, "%s\n", active[i].name);
		}
		fclose (fp);
		chmod (buf, 0644);
	}
}

/*
 *  check for any newly created newsgroups.
 *
 * If reading news locally stat() the active file to get its
 * size otherwise do a LIST NEWGROUPS to the NNTP server
 */

void check_for_any_new_groups ()
{
	char old_active_file_server[LEN];
	char old_active_file_attribute[LEN];
	char buf[LEN];
	FILE *fp = (FILE *) 0;
	int group_not_found;
	int ch, num = 0;
	int ch_default = 'n';
	int update_old_active = FALSE;
	int max_old_active;
	int new_active_size;
	int old_active_size;
	long epoch;
	register int i, j;
	struct stat sb;
	struct tm *tm;
	struct notify_t {
		char name[PATH_LEN];
		int len;
		int visited;
	} *old_active = (struct notify_t *) 0;

	if ((! check_for_new_newsgroups || (update && ! update_fork))) {
		return;
	}
	
	/*
	 * reading news locally (local) or via NNTP (server name)
	 */	
	if (read_news_via_nntp) {
		time (&epoch);
		tm = localtime (&epoch);
		sprintf (new_active_file_attribute, "%02d%02d%02d %02d%02d%02d",
			tm->tm_year, tm->tm_mon+1, tm->tm_mday, 
			tm->tm_hour, tm->tm_min, tm->tm_sec);
		strcpy (new_active_file_server, nntp_server);
		max_old_active = 16;
	} else {
		backup_active (TRUE);
		strcpy (new_active_file_server, "local");
		max_old_active = num_active;
		if (stat (news_active_file, &sb) >= 0) {
			sprintf (new_active_file_attribute, "%d", sb.st_size);
		}
	}
	
	/*
	 * find out if we have read news from here before otherwise -1
	 */
	active_index = find_active_size_index (new_active_file_server);
		
	if (debug == 2) {
		if (active_index >= 0) {	
			strcpy (old_active_file_server, active_size[active_index].server); 
			strcpy (old_active_file_attribute, active_size[active_index].attribute);
		} else {
			strcpy (old_active_file_server, "UNKNOWN"); 
			strcpy (old_active_file_attribute, "UNKNOWN");
		}	
		sprintf (msg, "Active size index=[%d]  old=[%s %s]  new=[%s %s]", 
			active_index,
			old_active_file_server, old_active_file_attribute,
			new_active_file_server, new_active_file_attribute);
		error_message (msg, "");						
		sleep (2);
	}
			
	if (! read_news_via_nntp && active_index >= 0) {
		new_active_size = atoi (new_active_file_attribute);
		old_active_size = atoi (active_size[active_index].attribute);
		if (new_active_size <= old_active_size) {
			goto notify_groups_done;
		}
	}

	/*
	 * Only check if spooldir is active news feed
	 */
	if (strcmp (spooldir_alias, "news") != 0) {
		goto notify_groups_done;
	}
	
	if ((fp = open_newgroups_fp (active_index)) == (FILE *) 0) {
		goto notify_groups_done;
	}

	Raw (TRUE);

	old_active = (struct notify_t *) my_malloc ((unsigned) sizeof (struct notify_t) * max_old_active);
	if (old_active == (struct notify_t *) 0) {
		error_message (txt_out_of_memory, progname);
		goto notify_groups_done;
	}
	
	while (fgets (buf, sizeof (buf), fp) != NULL) {
		if (buf[0] == '.') {
			break;
		}
		strncpy (old_active[num].name, buf, sizeof (old_active[num].name));		
		old_active[num].len = strlen (old_active[num].name)-1;
		old_active[num].name[old_active[num].len] = '\0';
		old_active[num].visited = FALSE;
		num++;
		if (num >= max_old_active) {
			max_old_active= max_old_active + (max_old_active / 2);
			old_active= (struct notify_t*) my_realloc(
				(char *) old_active, 
				(unsigned) sizeof(struct notify_t) * max_old_active);
			if (old_active == (struct notify_t *) 0) {
				error_message (txt_out_of_memory, progname);
				goto notify_groups_done;
			}
		}
	}


	if (read_news_via_nntp) {
		for (i = 0 ; i < num ; i++) {
			if (find_group_index (old_active[i].name) == -1) {
				continue;
			}
			if (! prompt_subscribe_group (old_active[i].name)) {
				if (cmd_line) {
					printf ("\r\n");
					fflush (stdout);
				}
				goto notify_groups_done;
			}
		}
	} else {	
		wait_message (txt_checking_active_file);
		for (i = 0 ; i < num_active ; i++) {	
			group_not_found = TRUE;
			for (j=0; j < num ; j++) {
				if (strcmp (old_active[j].name, active[i].name) == 0) {
					group_not_found = FALSE;	/* found it so read in next group */
					old_active[j].visited = TRUE;
					break;
				}
			}

			if (group_not_found == FALSE) {
				continue;
			}	
	
			update_old_active = TRUE;
	
			if (! prompt_subscribe_group (active[i].name)) {
				if (cmd_line) {
					printf ("\r\n");
					fflush (stdout);
				}
				goto notify_groups_done;
			}
		}
	}
	if (cmd_line) {
		fputc ('\r', stdout);
		fflush (stdout);
	}
	CleartoEOLN();

	if (! read_news_via_nntp) {
		/*
		 * Look for bogus groups 
		 */
		ch_default = 'y';
		for (j = 0 ; j < num ; j++)  {
			if (old_active[j].visited) {
				continue;
			}
			do {	
				update_old_active= 1;
				fputc ('\r', stdout);
				CleartoEOLN ();
				printf (txt_delete_bogus_group, old_active[j].name, ch_default);
				fflush (stdout);
				ch = ReadCh ();
				if (ch == CR) {
					ch = ch_default;
				}
			} while (! strchr ("nqy\033", ch));
	
			switch (ch) {
				case 'y':
					delete_group (old_active[j].name);
					break;	   
				case 'q':
				case ESC:
					goto notify_groups_done;	   
				case 'n':
				default:
					break;	   
			}		
			printf ("\r\n");
			fflush (stdout);
		}
	}
	
	/*
	 *  write active[] to ~/.tin/active (local spooldir)
	 */
	if (! read_news_via_nntp && update_old_active) {
		backup_active (FALSE);
	}

notify_groups_done:
	
	if (fp != (FILE *) 0) {
		fclose (fp);
	}
	
	/*
	 * update attribute field/create new entry with new size/date
	 */
	if (active_index >= 0) {
		if (active_size[active_index].attribute != (char *) 0) {
			free (active_size[active_index].attribute);
			active_size[active_index].attribute = (char *) 0;
		}
		active_size[active_index].attribute = 
			str_dup (new_active_file_attribute);
	} else {
		sprintf (buf, "%s[%s]", new_active_file_server, new_active_file_attribute);
		load_active_size_info (buf);
	}

	if (old_active != (struct notify_t *) 0) {
		free ((char *) old_active);
		old_active = (struct notify_t *) 0;
	}

	if (cmd_line) {
		Raw (FALSE);
	}
}

/*
 * prompt user if new group should be subscribed to
 */
 
int prompt_subscribe_group (group)
	char *group;
{
	int ch, ch_default = 'n';
	int idx;
	
	do {
		if (cmd_line) {
			fputc ('\r', stdout);
			CleartoEOLN ();
		} else {
			clear_message ();
		}
		printf (txt_subscribe_to_new_group, group, ch_default);
		fflush (stdout);
		ch = ReadCh ();
		if (ch == CR) {
			ch = ch_default;
		}
	} while (! strchr ("nqy\033", ch));
		
	fputc (ch, stdout);
	fflush (stdout);
	
	switch (ch) {
		case 'y':
			idx = add_group (group, TRUE);
if (debug == 2) {
	printf ("idx=[%d] group_top=[%d]\n", idx, group_top);
	fflush (stdout);
}
			subscribe (active[my_group[idx]].name, ':',
				   my_group[idx], FALSE);
			break;	   
		case 'q':
		case ESC:
			return FALSE;			   
		case 'n':
		default:
			break;	   
	}	
		
	if (cmd_line) {
		printf ("\r\n%s", txt_checking);
		fflush (stdout);
	} else {	
		wait_message (txt_checking);
	}

	return TRUE;			   
}

/*
 * Per group attributes
 */
 
void set_default_attributes ()
{
#ifndef INDEX_DAEMON
	register int i;
	
	for (i = 0; i < num_active ; i++) {
		active[i].attribute.maildir = default_maildir;
		active[i].attribute.savedir = default_savedir;
		active[i].attribute.sigfile = default_sigfile;
		active[i].attribute.organization = 
			(default_organization[0] ? default_organization : (char *) 0);
		active[i].attribute.followup_to = (char *) 0;
		active[i].attribute.printer = default_printer;
		active[i].attribute.read_during_session = FALSE;
		active[i].attribute.show_only_unread = default_show_only_unread;
		active[i].attribute.thread_arts = default_thread_arts;
		active[i].attribute.sort_art_type = default_sort_art_type;
		active[i].attribute.show_author  = default_show_author;
		active[i].attribute.auto_save= default_auto_save;
		active[i].attribute.batch_save = default_batch_save;
		active[i].attribute.delete_tmp_files = FALSE;
		active[i].attribute.post_proc_type = default_post_proc_type; 
	}
#endif	/* INDEX_DAEMON */
}

/*
 *  Load the group attributes into active[].attribute from ~/.tin/attributes 
 *
 *  attribute.maildir          = STRING
 *  attribute.savedir          = STRING
 *  attribute.organization     = STRING
 *  attribute.sigfile          = STRING
 *  attribute.followup_to      = STRING
 *  attribute.printer          = STRING
 *  attribute.auto_save        = ON/OFF
 *  attribute.batch_save       = ON/OFF
 *  attribute.delete_tmp_files = ON/OFF
 *  attribute.show_only_unread = ON/OFF
 *  attribute.thread_arts      = ON/OFF
 *  attribute.show_author      = NUM
 *    0=none, 1=name, 2=addr, 3=both
 *  attribute.sort_art_type    = NUM
 *    0=none, 1=subj descend, 2=subj ascend
 *    3=from descend, 4=from ascend
 *    5=date descend, 6=date ascend
 *  attribute.post_proc_type   = NUM
 *    0=none, 1=unshar, 2=uudecode
 *    3=uudecode & list zoo (unix) / lha (AmigaDOS) archive 
 *    4=uudecode & extract zoo (unix) / lha (AmigaDOS) archive
 *    5=uudecode & list zip archive
 *    6=uudecode & extract zip archive
 */

void read_attributes_file ()
{
#ifndef INDEX_DAEMON

	char buf[PATH_LEN];
	char line[PATH_LEN];
	FILE *fp;
	int num;
	int index = -1;
		
	set_default_attributes ();

	if ((fp = fopen (attributes_file, "r")) == (FILE *) 0) {
		return;
	}

	if ((update && update_fork) || ! update) {
		wait_message (txt_reading_attributes_file);
	}

	while (fgets (line, sizeof (line), fp) != NULL) {
		if (line[0] == '#' || line[0] == '\n') {
			continue;
		}
		if (match_string (line, "newsgroup=", buf, sizeof (buf))) {
if (debug == 2) {
	error_message("group=[%s]",buf);
}
			index = find_group_index (buf);
			continue;
		}
		if (match_string (line, "maildir=", buf, sizeof (buf))) {
			if (index >= 0) {
				active[index].attribute.maildir = str_dup (buf);
if (debug == 2) {
	sprintf (msg, "maildir=[%s]", active[index].attribute.maildir);
	error_message (msg, "");
}
			}
			continue;
		}
		if (match_string (line, "savedir=", buf, sizeof (buf))) {
			if (index >= 0) {
				active[index].attribute.savedir = str_dup (buf);
if (debug == 2) {
	sprintf (msg, "savedir=[%s]", active[index].attribute.savedir);
	error_message (msg, "");
}
			}
			continue;
		}
		if (match_string (line, "sigfile=", buf, sizeof (buf))) {
			if (index >= 0) {
				active[index].attribute.sigfile = str_dup (buf);
if (debug == 2) {
	sprintf (msg, "sigfile=[%s]", active[index].attribute.sigfile);
	error_message (msg, "");
}
			}
			continue;
		}
		if (match_string (line, "organization=", buf, sizeof (buf))) {
			if (index >= 0) {
				active[index].attribute.organization = str_dup (buf);
if (debug == 2) {
	error_message("organization=[%s]",active[index].attribute.organization);
}
			}
			continue;
		}
		if (match_string (line, "followup_to=", buf, sizeof (buf))) {
			if (index >= 0) {
				active[index].attribute.followup_to = str_dup (buf);
if (debug == 2) {
	error_message("followup_to=[%s]",active[index].attribute.followup_to);
}
			}
			continue;
		}
		if (match_string (line, "printer=", buf, sizeof (buf))) {
			if (index >= 0) {
				active[index].attribute.printer = str_dup (buf);
if (debug == 2) {
	error_message("printer=[%s]",active[index].attribute.printer);
}
			}
			continue;
		}
		if (match_boolean (line, "show_only_unread=", &num)) {
			if (index >= 0) {
				active[index].attribute.show_only_unread = num;
			}
			continue;
		}
		if (match_boolean (line, "thread_arts=", &num)) {
			if (index >= 0) {
				active[index].attribute.thread_arts = num;
			}
			continue;
		}
		if (match_boolean (line, "auto_save=", &num)) {
			if (index >= 0) {
				active[index].attribute.auto_save = num;
			}
			continue;
		}
		if (match_boolean (line, "batch_save=", &num)) {
			if (index >= 0) {
				active[index].attribute.batch_save = num;
			}
			continue;
		}
		if (match_boolean (line, "delete_tmp_files=", &num)) {
			if (index >= 0) {
				active[index].attribute.delete_tmp_files = num;
			}
			continue;
		}
		if (match_number (line, "sort_art_type=", &num)) {
			if (index >= 0) {
				active[index].attribute.sort_art_type = num;
			}
			continue;
		}
		if (match_number (line, "show_author=", &num)) {
			if (index >= 0) {
				active[index].attribute.show_author = num;
			}
			continue;
		}
		if (match_number (line, "post_proc_type=", &num)) {
			if (index >= 0) {
				active[index].attribute.post_proc_type = num;
			}
			continue;
		}
	}

	fclose (fp);

	if ((cmd_line && ! update && ! verbose) || (update && update_fork)) {
		wait_message ("\n");
	}

#endif	/* INDEX_DAEMON */
}

/*
 *  Save the group attributes from active[].attribute to ~/.tin/attributes 
 */

void write_attributes_file ()
{
#ifndef INDEX_DAEMON
	FILE *fp;
	register int i;
		
	if ((fp = fopen (attributes_file, "w")) == (FILE *) 0) {
		return;
	}

	if (! cmd_line) {
		if ((update && update_fork) || ! update) {
			wait_message (txt_writing_attributes_file);
		}
	}

	fprintf (fp, "# Group attributes file\n#\n");
	fprintf (fp, "#  newsgroup=STRING (ie. alt.sources) [mandatory]\n#\n");
	fprintf (fp, "#  maildir=STRING (ie. ~/Mail)\n");
	fprintf (fp, "#  savedir=STRING (ie. ~user/News)\n");
	fprintf (fp, "#  organization=STRING\n");
	fprintf (fp, "#  sigfile=STRING (ie. $var/sig)\n");
	fprintf (fp, "#  followup_to=STRING\n");
	fprintf (fp, "#  printer=STRING\n");
	fprintf (fp, "#  auto_save=ON/OFF\n");
	fprintf (fp, "#  batch_save=ON/OFF\n");
	fprintf (fp, "#  delete_tmp_files=ON/OFF\n");
	fprintf (fp, "#  show_only_unread=ON/OFF\n");
	fprintf (fp, "#  thread_arts=ON/OFF\n#\n");
	fprintf (fp, "#  show_author=NUM\n");
	fprintf (fp, "#    0=none, 1=name, 2=addr, 3=both\n#\n");
	fprintf (fp, "#  sort_art_type=NUM\n");
	fprintf (fp, "#    0=none, 1=subj descend, 2=subj ascend,\n"); 
	fprintf (fp, "#    3=from descend, 4=from ascend,\n");
	fprintf (fp, "#    5=date descend, 6=date ascend\n#\n");
	fprintf (fp, "#  post_proc_type=NUM\n");
	fprintf (fp, "#    0=none, 1=unshar, 2=uudecode,\n"); 
#ifdef AMIGA
	fprintf (fp, "#    3=uudecode & list lha archive,\n"); 
	fprintf (fp, "#    4=uudecode & extract lha archive\n");
#else
	fprintf (fp, "#    3=uudecode & list zoo archive,\n"); 
	fprintf (fp, "#    4=uudecode & extract zoo archive\n");
#endif
	fprintf (fp, "#    5=uudecode & list zip archive,\n"); 
	fprintf (fp, "#    6=uudecode & extract zip archive\n\n");

	for (i = 0 ; i < num_active ; i++) {
		fprintf (fp, "newsgroup=%s\n", active[i].name);
		fprintf (fp, "maildir=%s\n", active[i].attribute.maildir);
		fprintf (fp, "savedir=%s\n", active[i].attribute.savedir);
		fprintf (fp, "organization=%s\n", active[i].attribute.organization);
		fprintf (fp, "sigfile=%s\n", active[i].attribute.sigfile);
		fprintf (fp, "followup_to=%s\n", active[i].attribute.followup_to);
		fprintf (fp, "printer=%s\n", active[i].attribute.printer);
		fprintf (fp, "show_only_unread=%s\n", 
			(active[i].attribute.show_only_unread ? "ON" : "OFF"));
		fprintf (fp, "thread_arts=%s\n", 
			(active[i].attribute.thread_arts ? "ON" : "OFF"));
		fprintf (fp, "auto_save=%s\n", 
			(active[i].attribute.auto_save ? "ON" : "OFF"));
		fprintf (fp, "batch_save=%s\n", 
			(active[i].attribute.batch_save ? "ON" : "OFF"));
		fprintf (fp, "delete_tmp_files=%s\n", 
			(active[i].attribute.delete_tmp_files ? "ON" : "OFF"));
		fprintf (fp, "sort_art_type=%d\n", active[i].attribute.sort_art_type);
		fprintf (fp, "show_author=%d\n", active[i].attribute.show_author);
		fprintf (fp, "post_proc_type=%d\n", active[i].attribute.post_proc_type);
	}

	fclose (fp);

#endif	/* INDEX_DAEMON */
}

/*
 *  Load the last updated time for each group in the active file so that
 *  tind is more efficient and only has to stat the group dir and compare
 *  the last changed time with the time read from the ~/.tin/active.times
 *  file to determine if the group needs updating.
 *
 *  alt.sources 71234589
 *  comp.archives 71234890
 */

void read_active_times_file ()
{
#ifdef INDEX_DAEMON

	char *p, *q;
	char buf[LEN];
	char group[PATH_LEN];
	FILE *fp;
	int i;
	long updated_time;
	
	if ((fp = fopen (active_times_file, "r")) == (FILE *) 0) {
		return;
	}

	while (fgets (buf, sizeof (buf), fp) != NULL) {
		/*
		 * read the group name
		 */
		for (p = buf, q = group ; *p && *p != ' ' && *p != '\t' ; p++, q++) {
			*q = *p;
		}
		*q = '\0';
		
		/*
		 * read the last updated time
		 */
		updated_time = atol (p);
		
		/*
		 * find the group in active[] and set updated time 
		 */
		i = find_group_index (group);
		 
		if (i >= 0) {
			active[i].last_updated_time = updated_time;
		}

if (debug == 2) {
	printf ("group=[%-40.40s]  [%ld]\n",
		active[i].name, active[i].last_updated_time);
}		
	}
	fclose (fp);

#endif	/* INDEX_DAEMON */
}

/*
 *  Save the last updated time for each group to ~/.tin/active.times
 */

void write_active_times_file ()
{
#ifdef INDEX_DAEMON

	char buf[LEN];
	char group[PATH_LEN];
	FILE *fp;
	register int i;
	
	if ((fp = fopen (active_times_file, "w")) == (FILE *) 0) {
		return;
	}

	for (i = 0 ; i < num_active ; i++) {
		fprintf (fp, "%s %ld\n", active[i].name, active[i].last_updated_time);
	}
	fclose (fp);

#endif	/* INDEX_DAEMON */
}


void load_active_size_info (info)
	char *info;
{
	char *ptr_name;
	char *ptr_size;
	char buf[PATH_LEN];
	int i;
		
	/*
	 * initialize active_size[] if no entries
	 */
	if (! num_active_size) {
		for (i = 0 ; i < max_active_size ; i++) {
			active_size[i].server = (char *) 0;
			active_size[i].attribute = (char *) 0;
		}
	}

	my_strncpy (buf, info, sizeof (buf));
	
	ptr_name = buf;
	ptr_name = (char *) strchr (buf, ']'); 
	if (ptr_name != (char *) 0) {
		*ptr_name = '\0';
	}
	
	ptr_name = buf;
	ptr_name = (char *) strchr (buf, '['); 
	if (ptr_name != (char *) 0) {
		ptr_size = ptr_name;
		*ptr_name = '\0';
		if (num_active_size > max_active_size) {
			expand_active_size ();
		}
		active_size[num_active_size].server = str_dup (buf);
		active_size[num_active_size].attribute = str_dup (++ptr_size);
		if (debug == 2) {
			sprintf (buf, "ACTIVE server=[%s] attribute=[%s]", 
				active_size[num_active_size].server,
				active_size[num_active_size].attribute);
			error_message ("%s", buf);
		}	
		num_active_size++;
	}
}


int find_active_size_index (cur_active_server)
	char *cur_active_server;
{
	int i, found = FALSE;
		
	for (i = 0 ; i < num_active_size ; i++) {
		if (strcmp (cur_active_server, active_size[i].server) == 0) {
			found = TRUE;
			break;
		}
  	}

	return (found ? i : -1);
}

/*
 *  check for message of the day (motd) file
 *
 * If reading news locally stat() the active file to get its
 * mtime otherwise do a XMOTD command to the NNTP server
 */

void read_motd_file ()
{
#ifndef INDEX_DAEMON

	char buf[LEN];
	char motd_file_date[32];
	FILE *fp = (FILE *) 0;
	int lineno = 0;
	long new_motd_date = 0L;
	long old_motd_date = 0L;
	struct stat sb;
	struct tm *tm;

	if (update && ! update_fork) {
		return;
	}
	
	old_motd_date = atol (motd_file_info);

	/*
	 * reading news locally (local) or via NNTP (server name)
	 */	
	if (read_news_via_nntp) {
		time (&new_motd_date);
		tm = localtime (&new_motd_date);
		if (! old_motd_date) {
			strcpy (motd_file_date, "920101 000000");
		} else {
			sprintf (motd_file_date, "%02d%02d%02d %02d%02d%02d",
				tm->tm_year, tm->tm_mon+1, tm->tm_mday, 
				tm->tm_hour, tm->tm_min, tm->tm_sec);
		}
	} else {
		if (stat (motd_file, &sb) >=0) {
			new_motd_date = sb.st_mtime;
		}
	}
	
	if (old_motd_date && new_motd_date <= old_motd_date) {
		goto read_motd_done;
	}

	/*
	 * Only check if spooldir is active news feed
	 */
	if (strcmp (spooldir_alias, "news") != 0) {
		goto read_motd_done;
	}
	
	if ((fp = open_motd_fp (motd_file_date)) != (FILE *) 0) {
		while (fgets (buf, sizeof (buf), fp) != NULL) {
			if (buf[0] == '.') {
				break;
			}
			printf ("%s", buf);
			lineno++;
		}
		fclose (fp);
		
		if (lineno) {
			wait_message (txt_cmdline_hit_any_key);
			Raw (TRUE);
			ReadCh ();	
			Raw (FALSE);
			wait_message ("\n");
		}
	}

read_motd_done:
	
	/*
	 * update motd tinrc entry with new date
	 */
	sprintf (motd_file_info, "%ld", new_motd_date);

#endif	/* INDEX_DAEMON */
}

