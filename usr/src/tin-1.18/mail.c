/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : mail.c
 *  Author    : I.Lea
 *  Created   : 02-10-92
 *  Updated   : 11-10-92
 *  Notes     : Mail handling routines for creating pseudo newsgroups
 *  Copyright : (c) Copyright 1991-92 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

/*
 *  Load the mail active file into active[]
 */

void read_mail_active_file ()
{
#if !defined(INDEX_DAEMON) && defined(HAVE_MAIL_HANDLING)
	FILE *fp;
	char buf[LEN];
	char spooldir[PATH_LEN];
	int i;
	long h, min, max;
		
	if ((update && update_fork) || ! update) {
		wait_message (txt_reading_mail_active_file);
	}

	/*
	 * Open the mail active file & if we can't create an empty one
	 */
	if ((fp = open_mail_active_fp ("r")) == (FILE *) 0) {
		if (cmd_line) {
			fputc ('\n', stderr);
		}
		error_message (txt_cannot_open, mail_active_file);
		write_mail_active_file ();
		return;	
	}

	if (num_active == -1) {
		num_active = 0;
		for (i = 0; i < TABLE_SIZE; i++) {
			group_hash[i] = -1;
		}
	}

	while (fgets (buf, sizeof (buf), fp) != NULL) {
		if (! parse_active_line (buf, &max, &min, spooldir)) {
			continue;
		}

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
				if (strcmp (active[i].name, buf) == 0) {
					goto read_mail_active_continue;	/* kill dups */
				}
			}
			if (strcmp (active[i].name, buf) == 0)
				goto read_mail_active_continue;
			active[i].next = num_active;
		}
		/*
		 * Load group info.
		 */
		active[num_active].type = GROUP_TYPE_MAIL;
		active[num_active].name = str_dup (buf);
		active[num_active].spooldir = str_dup (spooldir);
		active[num_active].description = (char *) 0;
		active[num_active].max = max;
		active[num_active].min = min;
		active[num_active].moderated = 'y';
		active[num_active].next = -1;			/* hash chaining */
		active[num_active].my_group = UNSUBSCRIBED;	/* not in my_group[] yet */
		active[num_active].unread = 0;
		num_active++;

read_mail_active_continue:;

	}
	fclose (fp);

	if ((cmd_line && ! update && ! verbose) || (update && update_fork)) {
		wait_message ("\n");
	}

#endif	/* INDEX_DAEMON	*/
}

/*
 *  Write the mail groups from active[] to ~/.tin/mactive
 */

void write_mail_active_file ()
{
#if !defined(INDEX_DAEMON) && defined(HAVE_MAIL_HANDLING)
	FILE *fp;
	register int i;
		
	if ((fp = open_mail_active_fp ("w")) == (FILE *) 0) {
		return;
	}

	/*
	 * Load group into group hash table
	 */
	fprintf (fp, "# Mail active file. Format is like news active file:\n");
	fprintf (fp, "#   groupname  max.artnum  min.artnum  /maildir\n#\n");
	for (i = 0 ; i < num_active ; i++) {
		if (active[i].type == GROUP_TYPE_MAIL) {
			fprintf (fp, "%s %08ld %08ld %s\n", 
				active[i].name, active[i].max,
				active[i].min, active[i].spooldir);
		}
	}
	fclose (fp);

#endif	/* INDEX_DAEMON	*/
}

/*
 *  Load the text description from ~/.tin/mailgroups for each mail group into 
 *  the active[] array.
 */

void read_mailgroups_file ()
{
#ifndef INDEX_DAEMON
	FILE *fp;
	
	if (show_description == FALSE || save_news || catchup) {
		return;
	}

	if ((fp = open_mailgroups_fp ()) != (FILE *) 0) {
		wait_message (txt_reading_mailgroups_file);

		read_groups_descriptions (fp, (FILE *) 0);
		
		fclose (fp);

		if (cmd_line && ! update && ! verbose) {
			wait_message ("\n");
		}
	}
	
#endif	/* INDEX_DAEMON */
}

/*
 *  Load the text description from LIBDIR/newsgroups for each group into the 
 *  active[] array. Save a copy locally if reading via NNTP to save bandwidth.
 */

void read_newsgroups_file ()
{
#ifndef INDEX_DAEMON
	FILE *fp;
	FILE *fp_save = (FILE *) 0;
	
	if (show_description == FALSE || save_news || catchup) {
		return;
	}

	wait_message (txt_reading_newsgroups_file);

	if ((fp = open_newsgroups_fp ()) != (FILE *) 0) {
		if (read_news_via_nntp && ! read_local_newsgroups_file) {
			fp_save = fopen (local_newsgroups_file, "w");
		}
	
		read_groups_descriptions (fp, fp_save);

		fclose (fp);

		if (fp_save) {
			fclose (fp_save);
			read_local_newsgroups_file = TRUE;
		}
	}
	
	if (cmd_line && ! update && ! verbose) {
		wait_message ("\n");
	}
#endif	/* INDEX_DAEMON */
}

/*
 *  Read groups descriptions from opened file & make local backup copy 
 *  of all groups that don't have a 'x' in the active file moderated 
 *  field & if reading groups of type GROUP_TYPE_NEWS.
 */

void read_groups_descriptions (fp, fp_save)
	FILE *fp;
	FILE *fp_save;
{
	char buf[LEN];
	char group[PATH_LEN];
	char *p, *q;
	int i;
	
	while (fgets (buf, sizeof (buf), fp) != NULL) {
		if (buf[0] == '#' || buf[0] == '\n') {
			continue;
		}
		if (p = (char *) strchr (buf, '\n')) {
			*p = '\0';
		}

		for (p = buf, q = group ; *p && *p != ' ' && *p != '\t' ; p++, q++) {
			*q = *p;
		}
		*q = '\0';

		while (*p == '\t' || *p == ' ') {
			p++;
		}	

		i = find_group_index (group);
		 
		if (i >= 0 && active[i].description == (char *) 0) {
			active[i].description = str_dup (p);
			if (active[i].type == GROUP_TYPE_NEWS) {
				if (fp_save && read_news_via_nntp && 
				    ! read_local_newsgroups_file) {
					fprintf (fp_save, "%s\n", buf);
				}
			}
		}
	}
}
