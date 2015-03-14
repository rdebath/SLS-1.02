/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : art.c
 *  Author    : I.Lea & R.Skrenta
 *  Created   : 01-04-91
 *  Updated   : 07-11-92
 *  Notes     :
 *  Copyright : (c) Copyright 1991-92 by Iain Lea & Rich Skrenta
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

char index_file[PATH_LEN];
char *glob_art_group;
static long last_read_article;


/*
 *  Construct the pointers to the basenotes of each thread
 *  arts[] contains every article in the group.  inthread is
 *  set on each article that is after the first article in the
 *  thread.  Articles which have been expired have their thread
 *  set to -2 (ART_EXPIRED).
 */

void find_base (index)
	int index;	/* active[index] */
{
	register int i;
	register int j;

	top_base = 0;

	debug_print_arts ();

	if (active[index].attribute.show_only_unread) {
		for (i = 0; i < top; i++) {
			if (IGNORE_ART(i) || arts[i].inthread != FALSE) {
				continue;
			}	
			if (top_base >= max_art) {
				expand_art ();
			}
			if (arts[i].unread == ART_UNREAD) {
				base[top_base++] = i;
			} else {
				for (j = i ; j >= 0 ; j = arts[j].thread) {
					if (arts[j].unread) {
						base[top_base++] = i;
						break;
					}
				}
			}
		}
	} else {
		for (i = 0; i < top; i++) {
			if (IGNORE_ART(i) || arts[i].inthread != FALSE) {
				continue;
			}	
			if (top_base >= max_art) {
				expand_art ();
			}
			base[top_base++] = i;
		}
	}
}

/* 
 *  Count the number of non-expired articles in arts[]
 */

int num_of_arts ()
{
	int sum = 0;
	register int i;

	for (i = 0; i < top; i++) {
		if (arts[i].thread != ART_EXPIRED) {
			sum++;
		}
	}

	return sum;
}

/*
 *  Do we have an entry for article art?
 */

int valid_artnum (art)
	long art;
{
	register int i;

	for (i = 0; i < top; i++) {
		if (arts[i].artnum == art) {
			return i;
		}
	}

	return -1;
}

/*
 *  Return TRUE if arts[] contains any expired articles
 *  (articles we have an entry for which don't have a 
 *  corresponding article file in the spool directory)
 */

int purge_needed (group_path)
	char *group_path;
{
	register int i;

	for (i = 0; i < top; i++) {
		if (arts[i].thread == ART_EXPIRED) {
			return TRUE;
		}
	}
	
	return FALSE;
}

/*
 *  Main group indexing routine.  Group should be the name of the
 *  newsgroup, i.e. "comp.unix.amiga".  group_path should be the
 *  same but with the .'s turned into /'s: "comp/unix/amiga"
 *
 *  Will read any existing index, create or incrementally update
 *  the index by looking at the articles in the spool directory,
 *  and attempt to write a new index if necessary.
 */

int index_group (group, group_path)
	char *group;
	char *group_path;
{
	int index; 
	int killed = FALSE;
	int expired = FALSE;
	int modified = FALSE;
	register int i;
	
	glob_art_group = group;

	if (! update) {
		sprintf (msg, txt_group, group);
		wait_message (msg);
	}

	if ((index = find_group_index (group)) >= 0) {
		set_alarm_clock_off ();

		set_signals_art ();

		hash_reclaim ();
		free_art_array ();

		/*
		 *  load articles from index file if it exists
		 */
		read_index_file (group);

		/*
		 *  add any articles to arts[] that are new or were killed
		 */
		modified = read_group (group, group_path);

		/*
		 *  check that user did not abort indexing
		 */
		if (modified == -1) {  
			set_alarm_clock_on ();
			return FALSE;
		}

		/*
		 * Stat all articles to see if any have expired
		 */
		if (purge_index_files) {
			if (! cmd_line) {
				sprintf (msg, txt_purge, group);
				wait_message (msg);
			}			
			for (i = 0 ; i < top ; i++) {
				if (! stat_article (arts[i].artnum, group_path)) {
					expired = TRUE;
					if (cmd_line && verbose) {
						fputc ('P', stdout);
						fflush (stdout);
					}
				}
			}
			if (expired && cmd_line && verbose) {
				fputc ('\n', stdout);
				fflush (stdout);
			}
		}

		/*
		 * If reading index file via XINDEX this is useless
		 */
		if (expired || modified || purge_needed (group_path)) {
			write_index_file (group);
		}
	
		read_newsrc_line (group);
		killed = kill_any_articles (index);
		make_threads (FALSE);
		find_base (index);
	
		if ((modified || killed) && ! update) {
			clear_message ();
		}
		set_alarm_clock_on ();
	}
	return TRUE;
}

/*
 *  Index a group.  Assumes any existing index has already been
 *  loaded. Return values are:
 *    TRUE   loaded index and modified it
 *    FALSE  loaded index but not modified
 *    -1     user aborted indexing operation
 */

int read_group (group, group_path)
	char *group;
	char *group_path;
{
	char buf[PATH_LEN];
	char dir[PATH_LEN];
	FILE *fp;
	int count = 0;
	int modified = FALSE;
	int respnum, total = 0;
	long art;
	register int i;

	/*
	 * change to groups spooldir to optimize fopen()'s on local articles
	 */
	get_cwd (dir);
	joinpath (buf, active[my_group[cur_groupnum]].spooldir, group_path);
	chdir (buf);

	buf[0] = '\0';
	 
	/* 
	 * load article numbers into base[] 
	 */
	setup_base (group, group_path);

	/*
	 *  Count num of arts to index so the user has an idea of index time
	 */
	for (i = 0; i < top_base; i++) {
		if (base[i] <= last_read_article || valid_artnum (base[i]) >= 0) {
			continue;
		}
		total++;
	}

	for (i = 0; i < top_base; i++) {	/* for each article # */
		art = base[i];

		/*
		 *  Do we already have this article in our index?  Change 
		 *  thread from (ART_EXPIRED) to (ART_NORMAL) if so and 
		 *  skip the header eating.
		 */
		if ((respnum = valid_artnum (art)) >= 0 || art <= last_read_article) {
			if (respnum >= 0) {
				arts[respnum].thread = ART_NORMAL;
				arts[respnum].unread = ART_UNREAD;
			}	
			continue;
		}

		/* 
		 * we've modified the index so it will need to be re-written 
		 */
		if (! modified) {
			modified = TRUE;
		}

		if ((fp = open_header_fp (art)) == (FILE *) 0) {
			continue;
		}

		/*
		 *  Add article to arts[]
		 */
		if (top >= max_art) {
			expand_art();
		}

		arts[top].artnum = art;
		arts[top].thread = ART_NORMAL;

		set_article (&arts[top]);

		if (! parse_headers (fp, &arts[top])) {
			sprintf (buf, "FAILED parse_header(%ld)", art);
			debug_nntp ("read_group", buf);
			fclose (fp);
			continue;
		}

		fclose (fp);
		last_read_article = arts[top].artnum;	/* used if arts are killed */
		top++;

		if (++count % MODULO_COUNT_NUM == 0 && ! update) {
			if (input_pending ()) {
				if (read (STDIN_FILENO, buf, sizeof (buf)-1)) {
					if (buf[0] == ESC || buf[0] == 'q' || buf[0] == 'Q') {
						if (prompt_yn (LINES, txt_abort_indexing, 'y')) {
							return -1;
						}
					}	
				}
			}
			sprintf (msg, txt_indexing_num, group, count, total);
			wait_message (msg);
		}
		if (update && verbose) {
			fputc ('.', stdout);
			fflush (stdout);
		}
	}

	if (count && update && verbose) {
		fputc ('\n', stdout);
		fflush (stdout);
	}

	/*
	 * change to previous dir before indexing started
	 */
	chdir (dir);

	return modified;
}


/*
 *  Go through the articles in arts[] and use .thread to snake threads
 *  through them.  Use the subject line to construct threads.  The
 *  first article in a thread should have .inthread set to FALSE, the
 *  rest TRUE.  Only do unexprired articles we haven't visited yet
 *  (arts[].thread == -1 ART_NORMAL).
 */

void make_threads (rethread)
	int rethread;
{
	register int i;
	register int j;

	if (! cmd_line) {
		if (active[my_group[cur_groupnum]].attribute.thread_arts) {
			wait_message (txt_threading_arts);
		} else {
			wait_message (txt_unthreading_arts);
		}
	}

if (debug == 2) {
	sprintf (msg, "rethread=[%d]  thread_arts=[%d]  attr_thread_arts=[%d]", 	
		rethread, default_thread_arts,
		active[my_group[cur_groupnum]].attribute.thread_arts);
	error_message (msg, "");
}
	/*
	 *  arts[].thread & arts[].inthread need to be reset if re-threading
	 */
	if (rethread || active[my_group[cur_groupnum]].attribute.thread_arts) {
if (debug == 2) {
	error_message("Resetting .thread & .inthread", "");
}
		for (i=0 ; i < top ; i++) {
			arts[i].thread = ART_NORMAL;
			arts[i].inthread = FALSE;
		}
	}

	switch (active[my_group[cur_groupnum]].attribute.sort_art_type) {
		case SORT_BY_NOTHING:		/* don't sort at all */
			qsort ((char *) arts, top, sizeof (struct article_t), artnum_comp);
			break;
		case SORT_BY_SUBJ_DESCEND:
		case SORT_BY_SUBJ_ASCEND:
			qsort ((char *) arts, top, sizeof (struct article_t), subj_comp);
			break;
		case SORT_BY_FROM_DESCEND:
		case SORT_BY_FROM_ASCEND:
			qsort ((char *) arts, top, sizeof (struct article_t), from_comp);
			break;
		case SORT_BY_DATE_DESCEND:
		case SORT_BY_DATE_ASCEND:
			qsort ((char *) arts, top, sizeof (struct article_t), date_comp);
			break;
		default:
			break;
	}

	/*
	 * FIXME - Once full group attributes are implemented what should the case be here?
	 */
	if (active[my_group[cur_groupnum]].attribute.thread_arts == 0 || default_thread_arts == 0) {
if (debug == 2) {
	error_message("Returning before threading", "");
}
		return;
	}

if (debug == 2) {
	error_message("Threading", "");
}
	for (i = 0; i < top; i++) {
		if (arts[i].thread != ART_NORMAL || IGNORE_ART(i)) {
			continue;
		}	
		for (j = i+1; j < top; j++) {
			if (! IGNORE_ART(j) && 
			   ((arts[i].subject == arts[j].subject) ||
			   ((arts[i].part || arts[i].patch) &&
			   arts[i].archive == arts[j].archive))) {
				arts[i].thread = j;
				arts[j].inthread = TRUE;
				break;
			}
		}
	}
}


int parse_headers (fp, h)
	FILE *fp;
	struct article_t *h;
{
	char buf[HEADER_LEN];
	char buf2[HEADER_LEN];
	char art_from_addr[LEN];
	char art_full_name[LEN];
	char *ptr, *ptrline, *s;
	int flag, n = 0, len = 0;
	int lineno = 0;
	int max_lineno = 25;
	int got_archive = FALSE;
	int got_date = FALSE;
	int got_from = FALSE;
	int got_received = FALSE;
	int got_subject = FALSE;
	extern int errno;

	while ((n = fread (buf, 1, sizeof (buf)-1, fp)) == 0) {
		if (feof (fp)) {
			break;
		}
  
#ifdef EINTR
		if (ferror (fp) && errno != EINTR) {
#else
		if (ferror (fp)) {
#endif
			break;
		}
 
		clearerr (fp);
	}

	if (n == 0) {
		return FALSE;
	}

	buf[n-1] = '\0';
  	
	ptr = buf;

	while (1) {
		for (ptrline = ptr; *ptr && *ptr != '\n'; ptr++) {
			if (((*ptr) & 0xFF) < ' ') {
				*ptr = ' ';
			}
		}
		flag = *ptr;
		*ptr++ = '\0';
		lineno++;

		switch (*ptrline) {
			case 'F':	/* From:  mandatory */
			case 'T':	/* To:    mandatory (mailbox) */
				if (! got_from) {
					if (match_header (ptrline, "From", buf2, HEADER_LEN) ||
					    match_header (ptrline, "To", buf2, HEADER_LEN)) {
						parse_from (buf2, art_from_addr, art_full_name);
						h->from = hash_str (art_from_addr);
						if (art_full_name[0]) {
							h->name = hash_str (art_full_name);
						}
						got_from = TRUE;
					}
				}
				break;
			case 'R':	/* Received:  If found its probably a mail article */
				if (! got_received) {
					if (match_header (ptrline, "Received", buf2, HEADER_LEN)) {
						max_lineno = 50;
						got_received = TRUE;
					}
				}
				break;
			case 'S':	/* Subject:  mandatory */	
				if (! got_subject) {
					if (match_header (ptrline, "Subject", buf2, HEADER_LEN)) {
						s = eat_re (buf2);
						h->subject = hash_str (eat_re (s));
						got_subject = TRUE;
					}
				}
				break;
			case 'D':	/* Date:  mandatory */
				if (! got_date) {
					if (match_header (ptrline, "Date", buf2, HEADER_LEN)) {
						h->date = parsedate (buf2, (struct _TIMEINFO *) 0);
						got_date = TRUE;
					}
				}
				break;
			case 'A':	/* Archive-name:  optional */
				if (match_header (ptrline, "Archive-name", buf2, HEADER_LEN) ||
				    match_header (ptrline, "Archive-Name", buf2, HEADER_LEN)) {
					if ((s = (char *) strchr (buf2, '/')) != (char *) 0) {
						if (strncmp (s+1, "part", 4) == 0 ||
						    strncmp (s+1, "Part", 4) == 0) {
							h->part = str_dup (s+5);
							len = (int) strlen (h->part);
							if (h->part[len-1] == '\n') {
								h->part[len-1] = '\0';
							}
						} else if (strncmp (s+1,"patch",5) == 0 ||
							   strncmp (s+1,"Patch",5) == 0) {
							h->patch = str_dup (s+6);
							len = (int) strlen (h->patch);
							if (h->patch[len-1] == '\n') {
								h->patch[len-1] = '\0';
							}
						}
						if (h->part || h->patch) {
							s = buf2;
							while (*s && *s != '/')
								s++;
							*s = '\0';	
							s = buf2;
							h->archive = hash_str (s);
							got_archive = TRUE;
						}
					}
				}
				break;
		}

		if (! flag || lineno > max_lineno || got_archive) {
			if (got_from && got_date) {
				if (! got_subject) {
					h->subject = hash_str ("<No subject>");
				}
				debug_print_header (h);
				return TRUE;
			} else {
				return FALSE;
			}	
		}
	}
	/* NOTREACHED */
}

/* 
 *  Write out  an index file.  Write the group name first so if
 *  local indexing is done so we can disambiguate between group
 *  name hash collisions by looking at the index file.
 */

void write_index_file (group)
	char *group;
{
	char nam[LEN];
	FILE *fp;
	int *iptr;
	int index;
	int realnum;
	long min_artnum = 0L;
	long max_artnum = 0L;
	register int i;

	set_tin_uid_gid();

        sprintf (nam, "%s.%d", index_file, process_id);
	if ((fp = fopen (nam, "w")) == NULL) {
		perror_message (txt_cannot_open, nam);
		set_real_uid_gid ();
		return;
	}

	/*
	 *  dump group header info.
	 */
	index = my_group[cur_groupnum];
	if (active[index].attribute.sort_art_type != SORT_BY_NOTHING) {
		qsort ((char *) arts, top, sizeof (struct article_t), artnum_comp);
	}
	fprintf (fp, "%s\n", group);
	fprintf (fp, "%d\n", num_of_arts ());
	if (top <= 0) {
		fprintf (fp, "0\n");
	} else {
		min_artnum = arts[0].artnum;
		max_artnum = arts[top-1].artnum;
		if (last_read_article > max_artnum) {
			max_artnum = last_read_article;
			fprintf (fp, "%ld\n", last_read_article);
		} else {
			fprintf (fp, "%ld\n", max_artnum);
		}
		if (active[index].type == GROUP_TYPE_MAIL) {
			i = FALSE;
			if (min_artnum > active[index].min) {
				active[index].min = min_artnum;
				i = TRUE;
			}
			if (max_artnum > active[index].max) {
				active[index].max = max_artnum;
				i = TRUE;
			}
			if (i) {
				write_mail_active_file ();
			}
		}
	}
	
	/*
	 *  dump articles
	 */
	realnum = 0; 
	for (i = 0; i < top; i++) {
		if (arts[i].thread == ART_EXPIRED) { 
			continue;
		}
#ifdef DEBUG			
		debug_print_header (&arts[i]);
#endif
		fprintf(fp, "%ld\n", arts[i].artnum);

		iptr = (int *) arts[i].subject;
		iptr--;

		if (! arts[i].subject) {
			fprintf(fp, " \n");
		} else if (*iptr < 0 || *iptr > top) {
			fprintf(fp, " %s\n", arts[i].subject);
			*iptr = realnum;
		} else if (*iptr == i) {
			fprintf(fp, " %s\n", arts[i].subject);
		} else {
			fprintf(fp, "%%%d\n", *iptr);
		}
	
		iptr = (int *) arts[i].from;
		iptr--;

		if (! arts[i].from) {
			fprintf (fp, " \n");
		} else if (*iptr < 0 || *iptr > top) {
			fprintf (fp, " %s\n", arts[i].from);
			*iptr = realnum;
		} else if (*iptr == i) {
			fprintf(fp, " %s\n", arts[i].from);
		} else {
			fprintf(fp, "%%%d\n", *iptr);
		}

		iptr = (int *) arts[i].name;
		iptr--;

		if (! arts[i].name) {
			fprintf (fp, " \n");
		} else if (*iptr < 0 || *iptr > top) {
			fprintf (fp, " %s\n", arts[i].name);
			*iptr = realnum;
		} else if (*iptr == i) {
			fprintf(fp, " %s\n", arts[i].name);
		} else {
			fprintf(fp, "%%%d\n", *iptr);
		}

		fprintf (fp, "%ld\n", arts[i].date);
			
		iptr = (int *) arts[i].archive;
		iptr--;

		if (! arts[i].archive) {
			fprintf (fp, "\n");
		} else if (*iptr < 0 || *iptr > top) {
			fprintf (fp, " %s\n", arts[i].archive);
			*iptr = realnum;
		} else if (arts[i].part || arts[i].patch) {
			if (*iptr == i) {
				fprintf(fp, " %s\n", arts[i].archive);
			} else {
				fprintf (fp, "%%%d\n", *iptr);
			}
		} else {
			fprintf (fp, "\n");
		}
			
		if (! arts[i].part) {
			fprintf (fp, " \n");
		} else {
			fprintf (fp, "%s\n", arts[i].part);
		}

		if (! arts[i].patch) {
			fprintf (fp, " \n");
		} else {
			fprintf (fp, "%s\n", arts[i].patch);
		}

		realnum++;
	}
	fclose (fp);
	rename_file (nam, index_file);
	chmod (index_file, 0644);
	set_real_uid_gid();
	if (debug == 2) {
		sprintf (msg, "cp %s INDEX", index_file);
		system (msg);
	}
}

/*
 *  Read in an index file. 
 *
 *  index file header 
 *    1.  newsgroup name (ie. alt.sources)
 *    2.  number of articles (ie. 26)
 *    3.  number of last read article (ie. 210)
 *    4.  Is this a complete/killed index file (ie. COMPLETE/KILLED)
 *
 *  index file record
 *    1.  article number    (ie. 183)               [mandatory]
 *    2.  Subject: line     (ie. Which newsreader?) [mandatory]
 *    3.  From: line (addr) (ie. iain@norisc)       [mandatory]
 *    4.  From: line (name) (ie. Iain Lea)          [mandatory]
 *    5.  Date: of posting  (ie. 71459801)          [mandatory]
 *    6.  Archive: name     (ie. compiler)          [optional]
 *    7.  Part number of Archive: name  (ie. 01)    [optional]
 *    8.  Patch number of Archive: name (ie. 01)    [optional]
 */

void read_index_file (group_name)
	char *group_name;
{
	char buf[LEN], *p;
	int error = 0;
	int i, n;
	FILE *fp = NULL;

	top = 0;
	last_read_article = 0L;

	if ((fp = open_index_fp (group_name)) == NULL) {
		return;
	}

	/*
	 *  load header - discard group name, num. of arts in index file after any arts were killed
	 */
	if (fgets(buf, sizeof buf, fp) == NULL ||
		fgets(buf, sizeof buf, fp) == NULL) {
		error = 0;			
		goto corrupt_index;	
	}
	i = atoi (buf);

	/*
	 * num. of last_read_article including any that were killed
	 */
	if (fgets(buf, sizeof buf, fp) == NULL) {
		error = 1;				
		goto corrupt_index;	
	}							
	last_read_article = (long) atol (buf);
	
	/*
	 *  load articles
	 */
	while (top < i) {
		if (top >= max_art) {
			expand_art ();
		}

		arts[top].thread = ART_EXPIRED;
		set_article (&arts[top]);

		/*
		 * 0.  Article no.
		 */
		if (fgets (buf, sizeof buf, fp) == NULL) {
			error = 2;
			goto corrupt_index;
		}
		arts[top].artnum = (long) atol (buf);

		/*
		 * 1.  Subject:
		 */
		if (fgets (buf, sizeof buf, fp) == NULL) {
			error = 3;
			goto corrupt_index;
		}

		if (buf[0] == '%') {
			n = atoi (&buf[1]);
			if (n >= top || n < 0) {
				error = 4;
				goto corrupt_index;
			}
			arts[top].subject = arts[n].subject;
		} else if (buf[0] == ' ') {
			for (p = &buf[1];  *p && *p != '\n'; p++)
				continue;	
			*p = '\0';
			arts[top].subject = hash_str (&buf[1]);
		} else {
			error = 5;
			goto corrupt_index;
		}
			
		/*
		 * 2.  From: (addr part)
		 */
		if (fgets (buf, sizeof buf, fp) == NULL) {
			error = 6;
			goto corrupt_index;
		}

		if (buf[0] == '%') {
			n = atoi (&buf[1]);
			if (n >= top || n < 0) {
				error = 7;
				goto corrupt_index;
			}
			arts[top].from = arts[n].from;
		} else if (buf[0] == ' ') {
			for (p = &buf[1];  *p && *p != '\n'; p++)
				continue;
			*p = '\0';
			arts[top].from = hash_str (&buf[1]);
		} else {
			error = 8;
			goto corrupt_index;
		}

		/*
		 * 3.  From: (name part)
		 */
		if (fgets (buf, sizeof buf, fp) == NULL) {
			error = 9;
			goto corrupt_index;
		}

		if (buf[0] == '%') {
			n = atoi (&buf[1]);
			if (n > top || n < 0) {
				error = 10;
				goto corrupt_index;
			}
			if (n == top) {		/* no full name so .name = .from */
				arts[top].name = arts[top].from;
			} else {
				arts[top].name = arts[n].name;
			}
		} else if (buf[0] == ' ') {
			for (p = &buf[1];  *p && *p != '\n'; p++)
				continue;
			*p = '\0';
			if (buf[1]) {
				arts[top].name = hash_str (&buf[1]);
			}
		} else {
			error = 11;
			goto corrupt_index;
		}

		/*
		 * 4.  Date:
		 */
		if (fgets(buf, sizeof buf, fp) == NULL) {
			error = 12;
			goto corrupt_index;
		}

		buf[strlen (buf)-1] = '\0';
		arts[top].date = atol (buf);

		/*
		 * 5.  Archive-name:
		 */
		if (fgets(buf, sizeof buf, fp) == NULL) {
			error = 13;
			goto corrupt_index;
		}

		if (buf[0] == '\n') {
			arts[top].archive = (char *) 0;
		} else if (buf[0] == '%') {
			n = atoi (&buf[1]);
			if (n > top || n < 0) {
				error = 14;
				goto corrupt_index;
			}
			arts[top].archive = arts[n].archive;
		} else if (buf[0] == ' ') {
			for (p = &buf[1]; *p && *p != '\n' ; p++)
				continue;
			*p = '\0';
			arts[top].archive = hash_str (&buf[1]);
		} else {
			error = 15;
			goto corrupt_index;
		}

		/*
		 * 6.  Part no.
		 */
		if (fgets(buf, sizeof buf, fp) == NULL) {
			error = 16;
			goto corrupt_index;
		}

		if (buf[0] != ' ') { 
			buf[strlen (buf)-1] = '\0';
			arts[top].part = str_dup (buf);
		}

		/*
		 * 7.  Patch no.
		 */
		if (fgets(buf, sizeof buf, fp) == NULL) {
			error = 17;
			goto corrupt_index;
		}

		if (buf[0] != ' ') { 
			buf[strlen (buf)-1] = '\0';
			arts[top].patch = str_dup (buf);
		}

		debug_print_header (&arts[top]);

		top++;
	}
	fclose(fp);
	
	/*
	 * If reading in a mail group index check if min & max numbers are
	 * consistant with what has been read from index file.
	 */
	i = my_group[cur_groupnum]; 
	if (active[i].type == GROUP_TYPE_MAIL) {
		n = FALSE;
		if (top && top > active[i].max) {
			active[i].max = top;
			n = TRUE;
		}
		if (top && arts[0].artnum > active[i].min) {
			active[i].min = arts[0].artnum;
			n = TRUE;
		}
		if (n) {
			write_mail_active_file ();
		}
	}
	 
	return;

corrupt_index:
	if (! update) {
		sprintf (msg, txt_corrupt_index, index_file, error, top); 
		error_message (msg, "");
	}

	if (debug == 2) {
		sprintf (msg, "cp %s INDEX.BAD", index_file);
		system (msg);
	}

	last_read_article = 0L;
	if (fp) {
		fclose(fp);
	}	
	set_tin_uid_gid ();
	unlink (index_file);
	set_real_uid_gid ();
	top = 0;
}


/*
 *  Look in the local $HOME/RCDIR/[ INDEX_NEWSDIR | INDEX_MAILDIR ] etc.
 *  directory for the index file for the given group.  Hashing the group 
 *  name gets a number.  See if that #.1 file exists; if so, read first 
 *  line. Group we want?  If no, try #.2.  Repeat until no such file or
 *  we find an existing file that matches our group.
 *  Return group type of the index file (mail / news / -1 on error).
 */

int find_index_file (group)
	char *group;
{
	char *p, dir[PATH_LEN];
	FILE *fp;
	int i = 1, j, type;
	static char buf[PATH_LEN];
	unsigned long h;

	j = find_group_index (group);
	if (j == -1) {
		return j;
	}
	type = active[j].type;
	h = hash_groupname (group);

	if (read_news_via_nntp && xindex_supported && type == GROUP_TYPE_NEWS) {
		sprintf (index_file, "%sxindex.%d", TMPDIR, process_id);
		return (type);
	}
	
	if (local_index) {
		if (type == GROUP_TYPE_NEWS) {
			my_strncpy (dir, index_newsdir, sizeof (dir));
		} else {
			my_strncpy (dir, index_maildir, sizeof (dir));
		}
	} else {
		joinpath (dir, get_val ("TIN_INDEX", active[j].spooldir), INDEX_NEWSDIR);
	}
	
	i = 1;
	while (TRUE) {
		sprintf (index_file, "%s/%lu.%d", dir, h, i);
		
		if ((fp = fopen (index_file, "r")) == (FILE *) 0) {
			return (type);
		}

		if (fgets (buf, sizeof (buf), fp) == (char *) 0) {
			fclose (fp);
			return (type);
		}
		fclose (fp);

		for (p = buf; *p && *p != '\n'; p++) {
			continue;
		}	
		*p = '\0';

		if (strcmp (buf, group) == 0) {
			return (type);
		}	
		i++;
	}	
}

/*
 *  Run the index file updater only for the groups we've loaded.
 */

void do_update ()
{
	char group_path[PATH_LEN];
	register int i, j;
	long beg_epoch, end_epoch;
#ifdef INDEX_DAEMON
	char buf[PATH_LEN];
	long group_time, index_time;
	struct stat stinfo;
#endif
	
	if (verbose) {
		time (&beg_epoch);
	}

	/*
	 * load last updated times for each group (tind daemon only)
	 */
	read_active_times_file ();
	
	/*
	 * loop through groups and update any required index files
	 */
	for (i = 0; i < group_top; i++) {
		make_group_path (active[my_group[i]].name, group_path);
#ifdef INDEX_DAEMON
		joinpath (buf, active[my_group[i]].spooldir, group_path);
		if (stat (buf, &stinfo) == -1) {
			if (verbose) {
				error_message ("Can't stat group %s\n", buf);
			}
			continue;
		}

		group_time = stinfo.st_mtime;

		index_time = 0L;
		if (find_index_file (active[my_group[i]].name) == -1) {
			continue;
		}

		if (stat (index_file, &stinfo) == -1) {
			if (verbose) {
				printf ("Can't stat %s index %s\n", active[my_group[i]].name, index_file);
			}
		} else {
			index_time = stinfo.st_mtime;
		}

		if (debug == 2) {
			printf ("[%s] [%s]  idxtime=[%ld]  old=[%ld]  new=[%ld]\n", 
				active[my_group[i]].name, index_file, index_time,
				active[my_group[i]].last_updated_time, group_time);
		}
		
		if (index_time == 0L || active[my_group[i]].last_updated_time == 0L || 
		    group_time > active[my_group[i]].last_updated_time || purge_index_files) {
			active[my_group[i]].last_updated_time = group_time;
		} else {
			continue;
		}
#endif		

		if (verbose) {
			printf ("%s %s\n", (catchup ? "Catchup" : "Updating"),
				active[my_group[i]].name);
			fflush (stdout);
		}
		if (! index_group (active[my_group[i]].name, group_path)) {
			continue;
		}
		if (catchup) {
			for (j = 0; j < top; j++) {
				arts[j].unread = ART_READ;
			}
			update_newsrc (active[my_group[i]].name, my_group[i], FALSE);
		}
	}

	/*
	 * save last updated times for each group (tind daemon only)
	 */
	write_active_times_file ();
	
	if (verbose) {
		time (&end_epoch);
		sprintf (msg, txt_catchup_update_info,
			(catchup ? "Caughtup" : "Updated"), 
			group_top, end_epoch - beg_epoch);
		wait_message (msg);
	}
}

int artnum_comp (p1, p2)
	char *p1;
	char *p2;
{
	struct article_t *s1 = (struct article_t *) p1;
	struct article_t *s2 = (struct article_t *) p2;

	/* 
	 * s1->artnum less than s2->artnum 
	 */
	if (s1->artnum < s2->artnum) {
		return -1;
	}

	/* 
	 * s1->artnum greater than s2->artnum 
	 */
	if (s1->artnum > s2->artnum) {
		return 1;
	}
	return 0;
}


int subj_comp (p1, p2)
	char *p1;
	char *p2;
{
	struct article_t *s1 = (struct article_t *) p1;
	struct article_t *s2 = (struct article_t *) p2;

	/* 
	 * return result of strcmp (reversed for descending) 
	 */
	return (active[my_group[cur_groupnum]].attribute.sort_art_type == SORT_BY_SUBJ_ASCEND 
			? my_stricmp (s1->subject, s2->subject) 
			: my_stricmp (s2->subject, s1->subject));
}


int from_comp (p1, p2)
	char *p1;
	char *p2;
{
	struct article_t *s1 = (struct article_t *) p1;
	struct article_t *s2 = (struct article_t *) p2;

	/* 
	 * return result of strcmp (reversed for descending) 
	 */
	return (active[my_group[cur_groupnum]].attribute.sort_art_type == SORT_BY_FROM_ASCEND 
			? my_stricmp (s1->from, s2->from) 
			: my_stricmp (s2->from, s1->from));
}


int date_comp (p1, p2)
	char *p1;
	char *p2;
{
	struct article_t *s1 = (struct article_t *) p1;
	struct article_t *s2 = (struct article_t *) p2;


	if (active[my_group[cur_groupnum]].attribute.sort_art_type == SORT_BY_DATE_ASCEND) {
		/* 
		 * s1->date less than s2->date 
		 */
		if (s1->date < s2->date) {
			return -1;
		}
		/* 
		 * s1->date greater than s2->date 
		 */
		if (s1->date > s2->date) {
			return 1;
		}
	} else {
		/* 
		 * s2->date less than s1->date 
		 */
		if (s2->date < s1->date) {
			return -1;
		}
		/* 
		 * s2->date greater than s1->date 
		 */
		if (s2->date > s1->date) {
			return 1;
		}
	}
	
	return 0;
}


void set_article (art)
	struct article_t *art;
{	
	art->subject = (char *) 0;
	art->from = (char *) 0;
	art->name = (char *) 0;
	art->date = 0L;
	art->archive = (char *) 0;
	art->part = (char *) 0;
	art->patch = (char *) 0;
	art->xref = (char *) 0;
	art->unread = ART_UNREAD;
	art->inthread = FALSE;
	art->killed = FALSE;
	art->tagged = FALSE;
	art->hot = FALSE;
	art->zombie = FALSE;
}

int input_pending ()
{
#ifndef DONT_HAVE_SELECT
	int fd = STDIN_FILENO;
	fd_set fdread;
	struct timeval tvptr;

	FD_ZERO(&fdread);

	tvptr.tv_sec = 0;
	tvptr.tv_usec = 0;

	FD_SET(fd, &fdread);
	if (select (1, &fdread, NULL, NULL, &tvptr)) {
		if (FD_ISSET(fd, &fdread)) {
			return TRUE;
		}
	}
#endif
	return FALSE;
}

