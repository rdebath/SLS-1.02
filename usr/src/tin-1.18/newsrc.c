/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : newsrc.c
 *  Author    : I.Lea & R.Skrenta
 *  Created   : 01-04-91
 *  Updated   : 02-11-92
 *  Notes     :
 *  Copyright : (c) Copyright 1991-92 by Iain Lea & Rich Skrenta
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

/*
 * Automatically subscribe user to newsgroups specified in
 * /usr/lib/news/subscribe (locally) or same file but from
 * NNTP server (LIST AUTOSUBSCRIBE) and create .newsrc
 */

int auto_subscribe_groups ()
{
	char buf[LEN];
	FILE *fp_newsrc;
	FILE *fp_subs;
	int len;
	int ret_code = FALSE;
	
	if ((fp_subs = open_subscription_fp ()) != NULL) {
		if ((fp_newsrc = fopen (newsrc, "w")) != NULL) {
			while (fgets (buf, sizeof (buf), fp_subs) != NULL) {
				if (buf[0] != '#' && buf[0] != '\n') {
					len = strlen (buf);
					if (len > 1) {
						buf[len-1] = '\0';
						fprintf (fp_newsrc, "%s:\n", buf);
					}
				}	
			}	
			fclose (fp_newsrc);
			ret_code = TRUE;
		}	
		fclose (fp_subs);
	}

	return (ret_code);
}

/*
 * make a backup of users .newsrc in case of the bogie man
 */

void backup_newsrc ()
{
	char buf[NEWSRC_LINE];
	FILE *fp_newsrc, *fp_backup;
	
	if ((fp_newsrc = fopen (newsrc, "r")) != NULL) {
		joinpath (buf, homedir, ".oldnewsrc");
		unlink (buf);	/* because rn makes a link of .newsrc -> .oldnewsrc */
		if ((fp_backup = fopen (buf, "w")) != NULL) {
			while (fgets (buf, sizeof (buf), fp_newsrc) != NULL) {
				fputs (buf, fp_backup);
			}
			fclose (fp_backup);
		}
		fclose (fp_newsrc);
	}
}

/*
 *  Read $HOME/.newsrc into my_group[]. my_group[] ints point to
 *  active[] entries.  Sub_only determines  whether to just read
 *  subscribed groups or all of them. 
 */

void read_newsrc (sub_only)
	int sub_only;		/* TRUE=subscribed groups only, FALSE=all groups */
{
	char c, *p, buf[NEWSRC_LINE];
	char old_groups[PATH_LEN];
	FILE *fp = (FILE *) 0;
	FILE *fp_old = (FILE *) 0;
	int i;
	int remove_old_groups = FALSE;

	group_top = 0;

reread_newsrc:

	/* 
	 * make a .newsrc if one does'nt exist & auto subscribe to set groups
	 */
	if ((fp = fopen (newsrc, "r")) == NULL) {
		if (auto_subscribe_groups ()) {
			goto reread_newsrc;
		}	
		for (i = 0; i < num_active; i++) {
			if (group_top >= max_active) {
				expand_active ();
			}
			my_group[group_top] = i;
			active[i].my_group = 0;
			active[i].unread = -1;
			group_top++;
		}
		write_newsrc ();
		return;
	}

	joinpath (old_groups, homedir, ".newsrc.");
	sprintf (&old_groups[strlen(old_groups)], "%d", process_id);

	while (fgets (buf, sizeof buf, fp) != NULL) {
		p = buf;
		while (*p && *p != '\n' && *p != ' ' && *p != ':' && *p != '!')
			p++;
		c = *p;
		*p++ = '\0';

		if (c == '!' && sub_only) {
			continue;		/* unsubscribed */
		}
		
		i = add_group (buf, FALSE);
		
		if (i < 0) {
			if (! remove_old_groups) {
				if ((fp_old = fopen (old_groups, "w")) == NULL) {
					perror_message (txt_cannot_open, old_groups);
					continue;
				}
				remove_old_groups = TRUE;
			}
			fprintf (fp_old, "%s\n", buf);
			continue;
		}

		if (c != '!') {		/* if we're subscribed to it */
			active[my_group[i]].my_group |= SUBSCRIBED;
		}
		
		active[my_group[i]].unread = parse_unread (p, my_group[i]);
	}
	fclose (fp);

	/*
	 *  rewrite newsrc to get rid of any non-existant groups 
	 */
	if (remove_old_groups) {
		fclose (fp_old);
		rewrite_newsrc ();
		unlink (old_groups);
	}
}

/*
 *  Write a new newsrc from my_group[] and active[] mygroup if
 *  rewriting to get rid of groups that don't exist any longer. Used
 *  to a create a new .newsrc if there isn't one already, or when
 *  the newsrc is reset.
 */

void write_newsrc ()
{
	FILE *fp;
	int i;

	if ((fp = fopen (newsrc, "w")) == (FILE *) 0) {
		return;
	}

	wait_message (txt_creating_newsrc);

	for (i=0 ; i < num_active ; i++) {
		fprintf (fp, "%s:\n", active[i].name);
	}

	fclose (fp);
}

/*
 *  Rewrite newsrc to get rid of groups that don't exist any longer.
 */

void rewrite_newsrc ()
{
	char buf[NEWSRC_LINE];
	char old[NEWSRC_LINE];
	char old_groups[PATH_LEN];
	FILE *fp, *fp_old, *fp_new;
	int found_old_group, len;	

	joinpath (old_groups, homedir, ".newsrc.");
	sprintf (&old_groups[strlen(old_groups)], "%d", process_id);

	if ((fp = fopen (newsrc, "r")) == NULL)
		goto removed_old_groups_done;

	if ((fp_old = fopen (old_groups, "r")) == NULL)
		goto removed_old_groups_done;

	if ((fp_new = fopen (newnewsrc, "w")) == NULL)
		goto removed_old_groups_done;

	while (fgets (buf, sizeof buf, fp) != NULL) {			/* read group from newsrc */
		rewind (fp_old);
		found_old_group = FALSE;	
		while (fgets (old, sizeof old, fp_old) != NULL) {	/* read group from oldgroups */
			len = strlen (old)-1;
			if ((buf[len] == ':' || buf[len] == '!') &&
				strncmp (buf, old, len) == 0) {
				old[len] = '\0';
				sprintf (msg, txt_deleting_from_newsrc, old);
				wait_message (msg);
				if (cmd_line) {
					wait_message ("\n");
				}	
				found_old_group = TRUE;	
			}
		}
		if (! found_old_group) {
			fprintf (fp_new, "%s", buf);
		}
	}
	
	fclose (fp);
	fclose (fp_old);
	fclose (fp_new);

	rename_file (newnewsrc, newsrc);

removed_old_groups_done:
	unlink (old_groups);
}

/*
 *  Load the sequencer rang lists and mark arts[] according to the
 *  .newsrc info for a particular group.  i.e.  rec.arts.comics: 1-94,97
 */

void read_newsrc_line (group)
	char *group;
{
	FILE *fp;
	char buf[NEWSRC_LINE];
	char *p;

	if ((fp = fopen (newsrc, "r")) == NULL) {
		return;
	}

	while (fgets (buf, sizeof buf, fp) != NULL) {
		p = buf;
		while (*p && *p != '\n' && *p != ' ' && *p != ':' && *p != '!')
			p++;
		*p++ = '\0';
		if (strcmp (buf, group) != 0)
			continue;
		parse_seq (p);
		break;
	}

	fclose (fp);
}

/*
 *  For our current group, update the sequencer information in .newsrc
 */

void update_newsrc (group, groupnum, mark_unread)
	char *group;
	int groupnum;			/* index into active[] for this group */
	int mark_unread;
{
	char buf[NEWSRC_LINE];
	char c, *p;
	FILE *fp;
	FILE *newfp;

	if ((newfp = fopen (newnewsrc, "w")) == NULL) {
		goto update_done;
	}

	if ((fp = fopen (newsrc, "r")) != NULL) {
		while (fgets (buf, sizeof buf, fp) != NULL) {
			for (p = buf; *p; p++)
				if (*p == '\n') {
					*p = '\0';
					break;
				}

			p = buf;
			while (*p && *p != ' ' && *p != ':' && *p != '!')
					p++;
			c = *p;
			if (c != '\0')
				*p++ = '\0';

			if (c != '!' && c != ' ')
				c = ':';

			if (strcmp (buf, group) == 0) {
				if (mark_unread) {
					fprintf (newfp, "%s%c\n", buf, c);
				} else {
					fprintf (newfp, "%s%c ", buf, c);
					print_seq (newfp, groupnum);
					fprintf (newfp, "\n");
				}
			} else
				fprintf (newfp, "%s%c%s\n", buf, c, p);
		}
		fclose (fp);
	}

	fclose (newfp);
	rename_file (newnewsrc, newsrc);

update_done:
	;
}

/*
 *  Subscribe/unsubscribe to a group in .newsrc.  ch should either be
 *  '!' to unsubscribe or ':' to subscribe.  num is the group's index
 *  in active[].
 */

void subscribe (group, ch, num, out_seq)
	char *group;
	int ch;
	int num;
	int out_seq;				/* output sequencer info? */
{
	char buf[NEWSRC_LINE];
	char c, *p;
	FILE *fp;
	FILE *newfp;
	int gotit = FALSE;

	if (ch == '!') {
		active[num].my_group &= ~SUBSCRIBED;
	} else {
		active[num].my_group |= SUBSCRIBED;
	}
	
	if ((newfp = fopen (newnewsrc, "w")) == NULL)
		goto subscribe_done;

	if ((fp = fopen (newsrc, "r")) != NULL) {
		while (fgets (buf, sizeof buf, fp) != NULL) {
			if (strncmp ("options ", buf, 8) == 0) {
				fprintf (newfp, buf);
			} else {
				for (p = buf; *p; p++) {
					if (*p == '\n') {
						*p = '\0';
						break;
					}
				}	

				p = buf;
				while (*p && *p != ' ' && *p != ':' && *p != '!')
						p++;
				c = *p;
				if (c != '\0')
					*p++ = '\0';

				if (c != '!')
					c = ':';

				if (strcmp (buf, group) == 0) {
					fprintf (newfp, "%s%c%s\n", buf, ch, p);
					gotit = TRUE;
				} else {
					fprintf (newfp, "%s%c%s\n", buf, c, p);
				}
			}
		}
		fclose (fp);
	}

	if (! gotit) {
		if (out_seq) {
			fprintf (newfp, "%s%c ", group, ch);
			print_seq (newfp, num);
			fprintf (newfp, "\n");
		} else
			fprintf (newfp, "%s%c\n", group, ch);
	}

	fclose (newfp);
	rename_file (newnewsrc, newsrc);

subscribe_done:
	;
}


void reset_newsrc ()
{
	char buf[NEWSRC_LINE];
	char c, *p;
	FILE *fp;
	FILE *newfp;
	int i;

	if ((newfp = fopen (newnewsrc, "w")) == NULL)
		goto update_done;

	if ((fp = fopen (newsrc, "r")) != NULL) {
		while (fgets (buf, sizeof (buf), fp) != NULL) {
			for (p = buf; *p && *p != '\n'; p++)
				continue;
			*p = '\0';

			p = buf;
			while (*p && *p != ' ' && *p != ':' && *p != '!')
					p++;
			c = *p;
			if (c != '\0')
				*p++ = '\0';

			if (c != '!')
				c = ':';

			fprintf (newfp, "%s%c\n", buf, c);
		}
		fclose (fp);
	}

	fclose (newfp);
	rename_file (newnewsrc, newsrc);

update_done:
	for (i = 0; i < group_top; i++) {
		active[my_group[i]].unread = -1;
	}	
}


void delete_group (group)
	char *group;
{
	FILE *fp;
	FILE *newfp;
	char buf[8192];
	char *p;
	char c;
	int gotit = FALSE;
	FILE *del;

	if ((newfp = fopen (newnewsrc, "w")) == NULL) {
		goto del_done;
	}

	if ((del = fopen (delgroups, "a+")) == NULL) {
		fclose (newfp);
		goto del_done;
	}

	if ((fp = fopen (newsrc, "r")) != NULL) {
		while (fgets (buf, sizeof (buf), fp) != NULL) {
			for (p = buf; *p && *p != '\n'; p++)
				continue;
			*p = '\0';

			p = buf;
			while (*p && *p != ' ' && *p != ':' && *p != '!')
					p++;
			c = *p;
			if (c != '\0')
				*p++ = '\0';

			if (c != '!')
				c = ':';

			if (strcmp (buf, group) == 0) {
				fprintf (del, "%s%c%s\n", buf, c, p);
				gotit = TRUE;
			} else
				fprintf (newfp, "%s%c%s\n", buf, c, p);
		}
		fclose (fp);
		fclose (newfp);
	}

	if (! gotit)
		fprintf (del, "%s!\n", group);

	fclose (del);
	rename_file (newnewsrc, newsrc);

del_done:
	;
}


int undel_group ()
{
	char buf[2][NEWSRC_LINE];
	char c, *p;
	FILE *del;
	FILE *newfp;
	FILE *fp;
	int i, j;
	int which = 0;
	long h;

	if ((del = fopen (delgroups, "r")) == NULL) {
		return FALSE;
	}

	unlink (delgroups);
	
	if ((newfp = fopen (delgroups, "w")) == NULL) {
		return FALSE;
	}

	buf[0][0] = '\0';
	buf[1][0] = '\0';

	while (fgets (buf[which], sizeof (buf[which]), del) != NULL) {
		which = !which;
		if (*buf[which])
			fputs (buf[which], newfp);
	}

	fclose (del);
	fclose (newfp);
	which = !which;

	if (!*buf[which]) {
		return FALSE;
	}

	for (p = buf[which]; *p && *p != '\n'; p++)
		continue;
	*p = '\0';

	p = buf[which];
	while (*p && *p != ' ' && *p != ':' && *p != '!')
		p++;
	c = *p;
	if (c != '\0')
		*p++ = '\0';

	if (c != '!')
		c = ':';

	h = hash_groupname (buf[which]);

	for (i = group_hash[h]; i >= 0; i = active[i].next) {
		if (strcmp (buf[which], active[i].name) == 0) {
			for (j = 0; j < group_top; j++)
				if (my_group[j] == i) {
					return j;
				}

			active[i].my_group &= ~UNSUBSCRIBED;   /* mark that we got it */
			if (c != '!')
				active[i].my_group |= SUBSCRIBED;

			if (group_top >= max_active)
				expand_active ();
			group_top++;
			for (j = group_top; j > cur_groupnum; j--) {
/* FIXME delete			activeunread[j] = unread[j-1];
*/
				my_group[j] = my_group[j-1];
			}
			my_group[cur_groupnum] = i;
			active[i].unread = parse_unread (p, i);

			if ((fp = fopen (newsrc, "r")) == NULL) {
				return FALSE;
			}
			if ((newfp = fopen(newnewsrc, "w")) == NULL) {
				fclose(fp);
				return FALSE;
			}
			i = 0;
			while (fgets(buf[!which], sizeof (buf[!which]), fp) != NULL) {
				for (p = buf[!which]; *p && *p != '\n'; p++)
					continue;
				*p = '\0';

				p = buf[!which];
				while (*p && *p!=' ' && *p != ':' && *p != '!')
					p++;
				c = *p;
				if (c != '\0')
					*p++ = '\0';

				if (c != '!')
					c = ':';

				while (i < cur_groupnum) {
					if (strcmp(buf[!which],
					  active[my_group[i]].name) == 0) {
						fprintf(newfp, "%s%c%s\n",
							buf[!which], c, p);
						goto foo_cont;
					}
					i++;
				}
				fprintf(newfp, "%s%c%s\n", buf[which], c, p);
				fprintf(newfp, "%s%c%s\n", buf[!which], c, p);
				break;
foo_cont:;
			}

			while (fgets (buf[!which], sizeof (buf[!which]), fp) != NULL)
				fputs (buf[!which], newfp);

			fclose (newfp);
			fclose (fp);
			rename_file (newnewsrc, newsrc);
			return TRUE;
		}
	}
	return FALSE;
}


void mark_group_read (group, groupnum)
	char *group;
	int groupnum;			/* index into active[] for this group */
{
	char buf[NEWSRC_LINE];
	char c, *p;
	FILE *fp;
	FILE *newfp;

	if (active[groupnum].max < 2)
		return;

	if ((newfp = fopen (newnewsrc, "w")) == NULL)
		goto mark_group_read_done;

	if ((fp = fopen (newsrc, "r")) != NULL) {
		while (fgets(buf, sizeof (buf), fp) != NULL) {
			for (p = buf; *p; p++)
				if (*p == '\n') {
					*p = '\0';
					break;
				}

			p = buf;
			while (*p && *p != ' ' && *p != ':' && *p != '!')
					p++;
			c = *p;
			if (c != '\0')
				*p++ = '\0';

			if (c != '!')
				c = ':';

			if (strcmp (buf, group) == 0) {
				fprintf (newfp, "%s%c 1-%ld\n", buf, c, active[groupnum].max);
			} else
				fprintf(newfp, "%s%c%s\n", buf, c, p);
		}
		fclose (fp);
	}

	fclose (newfp);
	rename_file (newnewsrc, newsrc);

mark_group_read_done:
	;
}


void parse_seq(s)
	char *s;
{
	long low, high;
	int i;

	while (*s) {
		while (*s && (*s < '0' || *s > '9'))
			s++;

		if (*s && *s >= '0' && *s <= '9') {
			low = (long) atol (s);
			while (*s && *s >= '0' && *s <= '9')
				s++;
			if (*s == '-') {
				s++;
				high = (long) atol (s);
				while (*s && *s >= '0' && *s <= '9')
					s++;
			}  else
				high = low;

			for (i = 0; i < top; i++)
				if (arts[i].artnum >= low && arts[i].artnum <= high)
					arts[i].unread = ART_READ;
		}
	}
}

/*
 *  Read the first range from the .newsrc sequencer information.
 *  If the top of the first range is higher than what the active
 *  file claims is the bottom, use it as the new bottom instead.
 */

int parse_unread (s, groupnum)
	char *s;
	int groupnum;			/* index for group in active[] */
{
	int n, sum = 0;
	int gotone = FALSE;
	long low, high;
	long last_high;

	high = 0;

	if (*s) {
		while (*s && (*s < '0' || *s > '9')) {
			s++;
		}	
#if 0
		if (*s && *s >= '0' && *s <= '9') {
			low = (long) atol (s);
#endif
		if (!*s || *s >= '0' && *s <= '9') {
			low = *s ? (long) atol (s) : 0L;
			while (*s && *s >= '0' && *s <= '9')
				s++;
			if (*s == '-') {
				s++;
				high = (long) atol (s);
				while (*s && *s >= '0' && *s <= '9')
					s++;
			}  else
				high = low;
			gotone = TRUE;
		}
	}

	/* Note that in the active file min will be one greater than max
	 * when there are no articles in the spool directory. ie., it is
 	 * always true that "max - min + 1 = article count (including
 	 * expired articles)"
 	 */

	if (high < active[groupnum].min - 1)
		high = active[groupnum].min - 1;

	while (*s) {
		last_high = high;

		while (*s && (*s < '0' || *s > '9'))
			s++;

		if (*s && *s >= '0' && *s <= '9') {
			low = (long) atol (s);
			while (*s && *s >= '0' && *s <= '9')
				s++;
			if (*s == '-') {
				s++;
				high = (long) atol (s);
				while (*s && *s >= '0' && *s <= '9')
					s++;
			}  else
				high = low;

			if (low > last_high)	/* otherwise seq out of order */
				sum += (low - last_high) - 1;
		}
	}

	if (gotone) {
		if (active[groupnum].max > high)
			sum += active[groupnum].max - high;
		return sum;
	}

	n = (int) (active[groupnum].max - active[groupnum].min) + 1;
	
	if (n < 0)
		return -1;
	else
		return (n);
}


int get_line_unread (group, groupnum)
	char *group;
	int groupnum;				/* index for group in active[] */
{
	char *p, buf[NEWSRC_LINE];
	FILE *fp;
	int ret = -1;

	if ((fp = fopen(newsrc, "r")) == NULL)
		return -1;

	while (fgets(buf, sizeof (buf), fp) != NULL) {
		p = buf;
		while (*p && *p != '\n' && *p != ' ' && *p != ':' && *p != '!')
			p++;
		*p++ = '\0';
		
		if (strcmp (buf, group) != 0)
			continue;
			
		ret = parse_unread (p, groupnum);
		break;
	}

	fclose (fp);
	return ret;
}


void print_seq (fp, groupnum)
	FILE *fp;
	int groupnum;			/* index into active[] for this group */
{
	long int artnum, last_read, artmax;
	int i, flag = FALSE;
	
	assert(top >= 0);

  	/*
  	 *  sort into the same order as in the spool area for writing
  	 *  read article numbers to ~/.newsrc
  	 */
 	if (top > 0)
 		qsort ((char *) arts, top, sizeof (struct article_t), artnum_comp);
  
 	/*
 	 * Note that killed and expired articles do not appear in arts[].
 	 * So, even if top is 0 there may be sequencer info to output.
	 */
 	if (top > 0 && arts[top-1].artnum > active[groupnum].max)
 		artmax = arts[top-1].artnum;
 	else
 		artmax = active[groupnum].max;
 	for (artnum=1, i=0; artnum <= artmax; ++artnum, ++i) {
 		assert(i<=top);
 		if (i==top || arts[i].unread == ART_READ || artnum != arts[i].artnum) {
  			if (flag)
  				fprintf(fp, ",");
  			else
  				flag = TRUE;
 			fprintf (fp, "%ld", artnum);
 
 			while (i < top && arts[i].unread == ART_READ)
 				++i;
 
 			last_read = (i<top ? arts[i].artnum - 1 : artmax);
 
 			if (last_read != artnum) {
 				fprintf(fp, "-%ld", last_read);
  			}

 			assert(i <= top);
 			if (i == top)
 				break;
 			artnum = arts[i].artnum;
  		}
  	}
  
  	fflush (fp);
 	if (top == 0)
 		return;

	/*
	 *  resort into required sort order
	 */
	switch (active[groupnum].attribute.sort_art_type) {
		case SORT_BY_NOTHING:		/* already sorted above */
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
	}
}

/*
 *  rewrite .newsrc and position group at specifed position
 */

int pos_group_in_newsrc (group, pos)
	char *group;
	int pos;
{
	char buf[NEWSRC_LINE];
	char newsgroup[NEWSRC_LINE];
	char sub[PATH_LEN];
	char unsub[PATH_LEN];
	FILE *fp_in, *fp_out;
	FILE *fp_sub, *fp_unsub;
	int repositioned = FALSE;
	int subscribed_pos = 1;
	int group_len;
	int option_line = FALSE;
	int ret_code = FALSE;

	if ((fp_in = fopen (newsrc, "r")) == NULL) {
		goto rewrite_group_done;
	}
	if ((fp_out = fopen (newnewsrc, "w")) == NULL) {
		goto rewrite_group_done;
	}

	joinpath (buf, TMPDIR, ".subrc");
	sprintf (sub, "%s.%d", buf, process_id);

	joinpath (buf, TMPDIR, ".unsubrc");
	sprintf (unsub, "%s.%d", buf, process_id);

	if ((fp_sub = fopen (sub, "w")) == NULL) {
		goto rewrite_group_done;
	}
	if ((fp_unsub = fopen (unsub, "w")) == NULL) {
		goto rewrite_group_done;
	}

	/*
	 *  split newsrc into subscribed and unsubscribed to files
	 */
	group_len = strlen (group);

	while (fgets (buf, sizeof (buf), fp_in) != NULL) {
		if (strncmp (group, buf, group_len) == 0 && buf[group_len] == ':') {
			my_strncpy (newsgroup, buf, sizeof (newsgroup));
		} else if (strchr (buf, ':') != NULL) {
			fprintf (fp_sub, "%s", buf);
		} else if (strchr (buf, '!') != NULL) {
			fprintf (fp_unsub, "%s", buf);
		} else {	/* options line at beginning of .newsrc */
			fprintf (fp_sub, "%s", buf);
			option_line = TRUE;
		}
	}

	fclose (fp_in);
	fclose (fp_sub);
	fclose (fp_unsub);

	/*
	 *  write subscribed groups & position group to newnewsrc
	 */
	if ((fp_sub = fopen (sub, "r")) == NULL) {
		unlink (sub);
		goto rewrite_group_done;
	}
	while (fgets (buf, sizeof (buf), fp_sub) != NULL) {
		if (option_line) {
			if (strchr (buf, ':') == NULL && strchr (buf, '!') == NULL) {
				fprintf (fp_out, "%s", buf);
				continue;
			} else {
				option_line = FALSE;
			}
		}

		if (pos == subscribed_pos) {
			fprintf (fp_out, "%s\n", newsgroup);
			repositioned = TRUE;
		}
		
		fprintf (fp_out, "%s", buf);

		subscribed_pos++;
	}
	if (! repositioned) {
		fprintf (fp_out, "%s\n", newsgroup);
		repositioned = TRUE;
	}
	
	fclose (fp_sub);
 	unlink (sub);

	/*
	 *  write unsubscribed groups to newnewsrc
	 */
	if ((fp_unsub = fopen (unsub, "r")) == NULL) {
		unlink (unsub);
		goto rewrite_group_done;
	}
	while (fgets (buf, sizeof (buf), fp_unsub) != NULL) {
		fprintf (fp_out, "%s", buf);
	}

	fclose (fp_unsub);
	unlink (unsub);
	fclose (fp_out);

	if (repositioned) {
		cur_groupnum = pos;
		rename_file (newnewsrc, newsrc);
		ret_code = TRUE;
	}

rewrite_group_done:
	return ret_code;
}

/*
 *  mark all orther Xref: articles as read when one article read
 *  Xref: sitename newsgroup:artnum newsgroup:artnum [newsgroup:artnum ...]
 */
 
void mark_all_xref_read (xref_line)
	char *xref_line; 
{
/*
	char group[LEN];
	long artnum;
*/	
	if (xref_line == (char *) 0) {
		return;
	}

	/*
	 *  check sitename macthes nodename of current machine
	 */

	/*
	 *  tokenize each pair and update that newsgroup if it
	 *  is in users my_group[].
	 */
	 
}
