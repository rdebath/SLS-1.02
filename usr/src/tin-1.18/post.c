/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : post.c
 *  Author    : I.Lea
 *  Created   : 01-04-91
 *  Updated   : 21-11-92
 *  Notes     : mail/post/replyto/followup/crosspost & cancel articles
 *  Copyright : (c) Copyright 1991-92 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

#define	PRINT_LF()	Raw (FALSE); fputc ('\n', stdout); fflush (stdout); Raw (TRUE);

extern char note_h_distrib[PATH_LEN];		/* Distribution: */
extern char note_h_followup[LEN];		/* Followup-To: */
extern char note_h_messageid[PATH_LEN];		/* Message-ID:	*/
extern char note_h_newsgroups[LEN];		/* Newsgroups:	*/
extern char note_h_subj[LEN];			/* Subject:	*/
extern char note_h_date[PATH_LEN];		/* Date:	*/
extern FILE *note_fp;				/* the body of the current article */
extern long note_mark[MAX_PAGES];		/* ftells on beginnings of pages */

int unlink_article = TRUE;
static int reread_active_for_posted_arts = FALSE;
struct posted_t *posted;


int user_posted_messages ()
{
	char buf[LEN];
	FILE *fp;
	int i, j, k;
	int no_of_lines = 0;

	if ((fp = fopen (postfile, "r")) == NULL) {
		clear_message ();
		return FALSE;
	} else {
		while (fgets (buf, sizeof (buf), fp) != NULL) {
			no_of_lines++;
		}
		if (! no_of_lines) {
			fclose (fp);
			info_message (txt_no_arts_posted);
			return FALSE;
		}
		rewind (fp);
		posted = (struct posted_t *) my_malloc ((unsigned) (no_of_lines+1) * sizeof (struct posted_t));
		for (i=0 ; fgets (buf, sizeof (buf), fp) != NULL ; i++) {
			for (j=0 ; buf[j] != '|' && buf[j] != '\n' ; j++) {
				posted[i].date[j] = buf[j];		/* posted date */
			}
			if (buf[j] == '\n') {	
				error_message ("Corrupted file %s", postfile);
				sleep (1);
				fclose (fp);
				clear_message ();
				return FALSE;
			}
			posted[i].date[j++] = '\0';
			posted[i].action = buf[j];
			j += 2;
			for (k=j,j=0 ; buf[k] != '|' && buf[k] != ',' ; k++, j++) {
				posted[i].group[j] = buf[k];
			}
			if (buf[k] == ',') {
				while (buf[k] != '|' && buf[k] != '\n') {
					k++;
				}
				posted[i].group[j++] = ',';
				posted[i].group[j++] = '.';
				posted[i].group[j++] = '.';
				posted[i].group[j++] = '.';
			}
			posted[i].group[j++] = '\0';
			k++;
			for (j=k,k=0 ; buf[j] != '\n' ; j++, k++) {
				posted[i].subj[k] = buf[j];
			}
			posted[i].subj[k++] = '\0';
		}
		fclose (fp);

		show_info_page (POST_INFO, (char **) 0, txt_post_history_menu);
		if (posted != (struct posted_t *) 0) {
			free ((char *) posted);
			posted = (struct posted_t *) 0;
		}
		return TRUE;
	}
}


void update_art_posted_file (group, action, subj)
	char *group;
	int action;
	char *subj;
{
	char buf[LEN];
	char tmp_post[LEN];
	FILE *fp, *tmp_fp;
	long epoch;
	struct tm *tm;

	sprintf (tmp_post, "%s.%d", postfile, process_id);

	if ((tmp_fp = fopen (tmp_post, "w")) != NULL) {
		time (&epoch);
		tm = localtime (&epoch);
		fprintf (tmp_fp, "%02d-%02d-%02d|%c|%s|%s\n",
			tm->tm_mday, tm->tm_mon+1, tm->tm_year,
			action, group, subj);
		fclose (tmp_fp);
	}

	if ((tmp_fp = fopen (tmp_post, "a+")) != NULL) {
		if ((fp = fopen (postfile, "r")) != NULL) {
			while (fgets (buf, sizeof buf, fp) != NULL) {
				fprintf (tmp_fp, "%s", buf);
			}	
			fclose (fp);
			rename_file (tmp_post, postfile);
		}
		fclose (tmp_fp);
	}
}

/*
 * Check the article file so that it is not missing the blank line
 * between the header information and the text.
 */

int post_header_ok (article)
	char* article;
{
	FILE *fp;
	char line[LEN];
	int cnt= 0;
	int len, ind;
	char prev_ch;
	int header;

	if ((fp = fopen (article, "r")) == NULL) {
		perror_message (txt_cannot_open, article);
		return FALSE;
	}

	while (fgets (line, sizeof (line), fp) != NULL) {
		cnt++;
		len= strlen (line);
		if (len > 0)
			if (line[len - 1] == '\n') 
				line[--len]= 0;
				
		if ((len == 0) && (cnt >= 2)) {
			fclose(fp);
			return TRUE;
		}
		prev_ch= ' ';
		header= FALSE;
		for (ind= 0; ind < len; ind++) /* Skip white space */
			if ((line[ind] != ' ') && (line[ind] != '\t'))
				break;
		for (; ind < len; ind++) {   
			/* Header as long as the first token ends with ':' */
			if (((ind == len - 1) &&
			     (line[ind] == ':')) ||
			    (((line[ind] == ' ') ||
			      (line[ind] == '\t')) &&
			     (prev_ch == ':'))) {
				header= TRUE;
				break;
			}

			if ((line[ind] == ' ') ||
			    (line[ind] == '\t'))
				break;
			prev_ch= line[ind];
		}
		if (! header) {
			fclose (fp);
			return FALSE;
		}
	}
	fclose (fp);
	return FALSE;
}

/*
 *  Quick post an article (not a followup)
 */

void quick_post_article ()
{
	FILE *fp;
	char ch, *ptr;
	char ch_default = 'p';
	char group[PATH_LEN];
	char subj[PATH_LEN];
	char buf[LEN], tmp[LEN];
	int i, done = FALSE;

	if (! can_post) {
		info_message (txt_cannot_post);
		return;
	}

	/*
	 * Don't allow if not active news feed 
	 */
	if (! spooldir_is_active) {
		info_message (txt_not_active_newsfeed);
		return;
	}
	
	setup_screen ();
	InitScreen();

	/*
	 * Get groupname & subject for posting article.
	 * If multiple newsgroups test all to see if any are moderated.
	 */
	sprintf (buf, txt_post_newsgroups, default_post_newsgroups);
	
	if (! prompt_string (buf, group)) {
		fprintf (stderr, "%s\n", txt_no_quick_newsgroups);
		return;
	}

	if (strlen (group)) {
		my_strncpy (default_post_newsgroups, group,
			sizeof (default_post_newsgroups));
	} else {
		if (default_post_newsgroups[0]) {
			my_strncpy (group, default_post_newsgroups, sizeof (group));
		} else {
			fprintf (stderr, "%s\n", txt_no_quick_newsgroups);
			return;
		}
	}

	/*
	 * Check if any of the newsgroups are moderated.
	 */
	strcpy (tmp, group);
	while (! done) {
		strcpy (buf, tmp);
		ptr = buf;
		ptr = strchr(buf, ',');
		if (*ptr) {
			strcpy (tmp, ptr+1);
			*ptr = '\0';
		} else {
			done = TRUE;	
		}
		i = find_group_index (buf);

		if (debug == 2) {
			sprintf (msg, "Group=[%s] index=[%d]", buf, i);
			wait_message (msg);
		}
		
		if (i == -1) {
			Raw(FALSE);
			fprintf (stderr, "\nGroup %s not found in active file. Exiting...\n", buf);
			return;
		}
		if (active[i].moderated == 'm') {
			sprintf (msg, "Group %s is moderated. Continue? (y/n): ", buf);
			if (! prompt_yn (LINES, msg, 'y')) {
				Raw(FALSE);
				fprintf (stderr, "\nExiting...\n");
				return;
			}
		}
	}

	PRINT_LF();
	sprintf (buf, txt_post_subject, default_post_subject);
	
	if (! prompt_string (buf, subj)) {
		Raw (FALSE);
		fprintf (stderr, "%s\n", txt_no_quick_subject);
		return;
	}

	if (strlen (subj)) {
		my_strncpy (default_post_subject, subj,
			sizeof (default_post_subject));
	} else {
		if (default_post_subject[0]) {
			my_strncpy (subj, default_post_subject, sizeof (subj));
		} else {
			Raw (FALSE);
			fprintf (stderr, "%s\n", txt_no_quick_subject);
			return;
		}
	}
	
	PRINT_LF();
	start_line_offset = 6;

	if ((fp = fopen (article, "w")) == NULL) {
		Raw (FALSE);
		perror_message (txt_cannot_open, article);
		return;
	}
	chmod (article, 0600);

/* FIXME so that group only conatains 1 group when finding an index number */
i = find_group_index (group);

	fprintf (fp, "Subject: %s\n", subj);
	fprintf (fp, "Newsgroups: %s\n", group);
	if (i >= 0 && active[i].attribute.organization != (char *) 0) {
		fprintf (fp, "Organization: %s\n", active[i].attribute.organization);
		start_line_offset++;
	}
	if (*reply_to) {
		fprintf (fp, "Reply-To: %s\n", reply_to);
		start_line_offset++;
	}
	if (i >= 0 && active[i].attribute.followup_to != (char *) 0) {
		fprintf (fp, "Followup-To: %s\n", active[i].attribute.followup_to);
		start_line_offset++;
	}
	if (*my_distribution) {
		fprintf (fp, "Distribution: %s\n", my_distribution);
		start_line_offset++;
	}
	fprintf (fp, "Summary: \n");
	fprintf (fp, "Keywords: \n\n\n");

	add_signature (fp, FALSE);
	fclose (fp);

	ch = 'e';
	while (1) {
		switch (ch) {
		case 'e':
			invoke_editor (article);
			while (! post_header_ok (article)) {
				do {
					sprintf (msg, "%s%c", txt_no_blank_line, 'e');
					wait_message (msg);
					MoveCursor (LINES, (int) strlen (txt_no_blank_line));
					if ((ch = (char) ReadCh ()) == CR)
						ch = 'e';
				} while (! strchr ("eipq\033", ch));
				if (ch == 'e')
					invoke_editor (article);
				else
					break;
			}
			if (ch == 'e') {
				break;
			}
		case 'i':
			invoke_ispell (article);
			break;
		case 'q':
		case ESC:
			if (unlink_article)
				unlink (article);
			clear_message ();
			return;

		case 'p':
			wait_message (txt_posting);
			if (submit_file (article)) {
				Raw (FALSE);
				info_message (txt_art_posted);
				reread_active_for_posted_arts = TRUE;		
				goto post_article_done;
			} else {
				rename_file (article, dead_article);
				Raw (FALSE);
				error_message (txt_art_rejected, dead_article);
				return;
			}
		}

		do {
			sprintf (msg, "%s%c", txt_quit_edit_post, ch_default);
			wait_message (msg);
			MoveCursor (LINES, (int) strlen (txt_quit_edit_post));
			if ((ch = (char) ReadCh ()) == CR)
				ch = ch_default;
		} while (! strchr ("eipq\033", ch));
	}

post_article_done:
	find_mail_header (HEADER_NEWSGROUPS, article, group);
	find_mail_header (HEADER_SUBJECT, article, subj);

	if (unlink_article) {
		unlink (article);
	}
	update_art_posted_file (group, 'w', subj);

	my_strncpy (default_post_newsgroups, group, sizeof (default_post_newsgroups));
	my_strncpy (default_post_subject, subj, sizeof (default_post_subject));

	write_rcfile ();
	
	return;
}


/*
 *  Post an original article (not a followup)
 */

int post_article (group, posted)
	char *group;
	int *posted;
{
	FILE *fp;
	char ch;
	char ch_default = 'p';
	char subj[LEN];
	char buf[LEN];
	int i, redraw_screen = FALSE;

	if (! can_post) {
		info_message (txt_cannot_post);
		return (redraw_screen);
	}

	/*
	 * Don't allow if not active news feed 
	 */
	if (! spooldir_is_active) {
		info_message (txt_not_active_newsfeed);
		return (redraw_screen);
	}
	
	*posted = FALSE;
	start_line_offset = 6;

	if (active[my_group[cur_groupnum]].moderated == 'm') {
		sprintf (msg, "Group %s is moderated. Continue? (y/n): ", group);
		if (! prompt_yn (LINES, msg, 'y')) {
			clear_message ();
			return (redraw_screen);
		}
	}

	sprintf (msg, txt_post_subject, default_post_subject);
	
	if (! prompt_string (msg, subj)) {
		clear_message ();
		return (redraw_screen);
	}

	if (strlen (subj)) {
		my_strncpy (default_post_subject, subj,
			sizeof (default_post_subject));
	} else {
		if (default_post_subject[0]) {
			my_strncpy (subj, default_post_subject, sizeof (subj));
		} else {
			info_message (txt_no_subject);
			return (redraw_screen);
		}
	}
	
	wait_message (txt_post_an_article);

	if ((fp = fopen (article, "w")) == NULL) {
		perror_message (txt_cannot_open, article);
		return (redraw_screen);
	}
	chmod (article, 0600);

	i = find_group_index (group);

	fprintf (fp, "Subject: %s\n", subj);
	fprintf (fp, "Newsgroups: %s\n", group);
	if (i >= 0 && active[i].attribute.organization != (char *) 0) {
		fprintf (fp, "Organization: %s\n", active[i].attribute.organization);
		start_line_offset++;
	}
	if (*reply_to) {
		fprintf (fp, "Reply-To: %s\n", reply_to);
		start_line_offset++;
	}
	if (i >= 0 && active[i].attribute.followup_to != (char *) 0) {
		fprintf (fp, "Followup-To: %s\n", active[i].attribute.followup_to);
		start_line_offset++;
	}
	if (*my_distribution) {
		fprintf (fp, "Distribution: %s\n", my_distribution);
		start_line_offset++;
	}
	fprintf (fp, "Summary: \n");
	fprintf (fp, "Keywords: \n\n\n");

	add_signature (fp, FALSE);
	fclose (fp);

	ch = 'e';
	while (1) {
		switch (ch) {
		case 'e':
			invoke_editor (article);
			while (! post_header_ok (article)) {
				do {
					sprintf (msg, "%s%c", txt_no_blank_line, 'e');
					wait_message (msg);
					MoveCursor (LINES, (int) strlen (txt_no_blank_line));
					if ((ch = (char) ReadCh ()) == CR)
						ch = 'e';
				} while (! strchr ("eipq\033", ch));
				if (ch == 'e')
					invoke_editor (article);
				else
					break;
			}
			redraw_screen = TRUE;
			if (ch == 'e') {
				break;
			}
		case 'i':
			invoke_ispell (article);
			break;
		case 'q':
		case ESC:
			if (unlink_article)
				unlink (article);
			clear_message ();
			return (redraw_screen);

		case 'p':
			wait_message (txt_posting);
			if (submit_file (article)) {
				info_message (txt_art_posted);
				*posted = TRUE;
				reread_active_for_posted_arts = TRUE;		
				goto post_article_done;
			} else {
				rename_file (article, dead_article);
				sprintf (buf, txt_art_rejected, dead_article);
				info_message (buf);
				sleep (3);
				return (redraw_screen);
			}
		}

		do {
			sprintf (msg, "%s%c", txt_quit_edit_post, ch_default);
			wait_message (msg);
			MoveCursor (LINES, (int) strlen (txt_quit_edit_post));
			if ((ch = (char) ReadCh ()) == CR)
				ch = ch_default;
		} while (! strchr ("eipq\033", ch));
	}

post_article_done:
	find_mail_header (HEADER_SUBJECT, article, subj);

	if (unlink_article) {
		unlink (article);
	}
	update_art_posted_file (group, 'w', subj);

	my_strncpy (default_post_newsgroups, group, sizeof (default_post_newsgroups));
	my_strncpy (default_post_subject, subj, sizeof (default_post_subject));

	return (redraw_screen);
}


int post_response (group, respnum, copy_text)
	char *group;
	int respnum;
	int copy_text;
{
	FILE *fp;
	char ch, *ptr;
	char ch_default = 'p';
	char buf[LEN];
	int i;
	int ret_code = POSTED_NONE;
	
	/*
	 * Don't allow if not active news feed 
	 */
	if (! spooldir_is_active) {
		info_message (txt_not_active_newsfeed);
		return (ret_code);
	}

	start_line_offset = 4;

	wait_message (txt_post_a_followup);
	
	if (*note_h_followup && strcmp (note_h_followup, "poster") == 0) {
		clear_message ();
		if (! prompt_yn (LINES, txt_resp_to_poster, 'y')) {
			return (ret_code);
		}
		*note_h_followup = '\0';
		find_reply_to_addr (respnum, buf);
		mail_to_someone (respnum, buf, TRUE, FALSE, &ret_code);
		return (ret_code);
	} else if (*note_h_followup && strcmp (note_h_followup, group) != 0) {
		MoveCursor (LINES/2, 0);
		CleartoEOS ();
		center_line ((LINES/2)+2, TRUE, txt_resp_redirect);
		MoveCursor ((LINES/2)+4, 0);

		fputs ("    ", stdout);
		ptr = note_h_followup;
		while (*ptr) {
			if (*ptr != ',') {
				fputc (*ptr, stdout);
			} else {
				fputs ("\r\n    ", stdout);
			}
			ptr++;
		}
		fflush (stdout);

		if (! prompt_yn (LINES, txt_continue, 'y')) {
			return (ret_code);
		}
	}

	if ((fp = fopen (article, "w")) == NULL) {
		perror_message (txt_cannot_open, article);
		return (ret_code);
	}
	chmod (article, 0600);

	i = find_group_index (group);

	fprintf (fp, "Subject: Re: %s\n", eat_re (note_h_subj));

	if (*note_h_followup && strcmp (note_h_followup, "poster") != 0) {
		fprintf (fp, "Newsgroups: %s\n", note_h_followup);
	} else {
		fprintf (fp, "Newsgroups: %s\n", note_h_newsgroups);
		if (i >= 0 && active[i].attribute.followup_to != (char *) 0) {
			fprintf (fp, "Followup-To: %s\n", 
				active[i].attribute.followup_to);
			start_line_offset++;
		} else {
			ptr = (char *) strchr (note_h_newsgroups, ',');
			if (ptr) {
				fprintf (fp, "Followup-To: %s\n", 
					note_h_newsgroups);
				start_line_offset++;
			}
		}
	}

	fprintf (fp, "References: %s\n", note_h_messageid);

	if (i >= 0 && active[i].attribute.organization != (char *) 0) {
		fprintf (fp, "Organization: %s\n", active[i].attribute.organization);
		start_line_offset++;
	}
	if (*reply_to) {
		fprintf (fp, "Reply-To: %s\n", reply_to);
		start_line_offset++;
	}
	if (*note_h_distrib) {
		fprintf (fp, "Distribution: %s\n", note_h_distrib);
		start_line_offset++;
	}
	fprintf (fp, "\n");

	if (copy_text) {
		if (strfquote (group, respnum, buf, sizeof (buf), news_quote_format)) {
			fprintf (fp, "%s\n", buf);
		}
		fseek (note_fp, note_mark[0], 0);
		copy_fp (note_fp, fp, quote_chars);
	}

	add_signature (fp, FALSE);
	fclose (fp);

	ch = 'e';
	while (1) {
		switch (ch) {
		case 'e':
			invoke_editor (article);
			while (! post_header_ok (article)) {
				do {
					sprintf (msg, "%s%c", txt_no_blank_line, 'e');
					wait_message (msg);
					MoveCursor (LINES, (int) strlen (txt_no_blank_line));
					if ((ch = (char) ReadCh ()) == CR)
						ch = 'e';
				} while (! strchr ("eq\033", ch));
				if (ch == 'e')
					invoke_editor (article);
				else
					break;
			}
			if (ch == 'e') {
				break;
			}
			ret_code = POSTED_REDRAW;
			break;

		case 'i':
			invoke_ispell (article);
			ret_code = POSTED_REDRAW;
			break;
			
		case 'q':
		case ESC:
			if (unlink_article)
				unlink (article);
			clear_message ();
			return (ret_code);

		case 'p':
			wait_message (txt_posting);
			if (submit_file (article)) {
				info_message (txt_art_posted);
				ret_code = POSTED_OK;
				reread_active_for_posted_arts = TRUE;		
				goto post_response_done;
			} else {
				rename_file (article, dead_article);
				sprintf (buf, txt_art_rejected, dead_article);
				info_message (buf);
				sleep (3);
				return (ret_code);
			}
		}

		do {
			sprintf (msg, "%s%c", txt_quit_edit_post, ch_default);
			wait_message (msg);
			MoveCursor(LINES, (int) strlen (txt_quit_edit_post));
			if ((ch = (char) ReadCh()) == CR)
				ch = ch_default;
		} while (! strchr ("eipq\033", ch));
	}

post_response_done:
	if (*note_h_followup && strcmp(note_h_followup, "poster") != 0) {
		find_mail_header (HEADER_SUBJECT, article, buf);
		update_art_posted_file (note_h_followup, 'f', buf);
	} else {
		find_mail_header (HEADER_SUBJECT, article, buf);
		update_art_posted_file (note_h_newsgroups, 'f', buf);
		my_strncpy (default_post_newsgroups, note_h_newsgroups, 
			sizeof (default_post_newsgroups));
	}

	my_strncpy (default_post_subject, buf, sizeof (default_post_subject));

	if (unlink_article) {
		unlink (article);
	}
	
	return (ret_code);
}


int mail_to_someone (respnum, address, mail_to_poster, confirm_to_mail, mailed_ok)
	int respnum;
	char *address;
	int mail_to_poster;
	int confirm_to_mail;
	int *mailed_ok;
{
	char nam[100];
	char ch = 's';
	char ch_default = 's';
	char buf[LEN];
	char mail_to[LEN];
	FILE *fp;
	int redraw_screen = FALSE;

	start_line_offset = 4;
	
	strcpy (mail_to, address);
	clear_message ();
	
	joinpath (nam, homedir, ".letter");
	if ((fp = fopen (nam, "w")) == NULL) {
		perror_message (txt_cannot_open, nam);
		return (redraw_screen);
	}
	chmod (nam, 0600);

	fprintf (fp, "To: %s\n", mail_to);

	if (mail_to_poster) {
		fprintf (fp, "Subject: Re: %s\n", eat_re (note_h_subj));
	} else {
		fprintf (fp, "Subject: (fwd) %s\n", note_h_subj);
	}
	
	if (*note_h_followup) {
		fprintf (fp, "Newsgroups: %s\n\n", note_h_followup);
	} else {
		fprintf (fp, "Newsgroups: %s\n", note_h_newsgroups);
	}
	if (*default_organization) {
		fprintf (fp, "Organization: %s\n", default_organization);
		start_line_offset++;
	}
	if (*reply_to) {
		fprintf (fp, "Reply-To: %s\n", reply_to);
		start_line_offset++;
	}
	fputc ('\n', fp);
	
	if (mail_to_poster) {
		ch = 'e';
		if (strfquote (active[my_group[cur_groupnum]].name, 
		    respnum, buf, sizeof (buf), mail_quote_format)) {
			fprintf (fp, "%s\n", buf);
		}
		fseek (note_fp, note_mark[0], 0);
		copy_fp (note_fp, fp, quote_chars);
	} else {
		fseek (note_fp, 0L, 0);
		copy_fp (note_fp, fp, "");
	}
	
	add_signature (fp, TRUE);
	fclose (fp);
	
	while (1) {
		if (confirm_to_mail) {
			do {
				sprintf (msg, "%s [%.*s]: %c", txt_quit_edit_ispell_send, 
					COLS-36, note_h_subj, ch_default);
				wait_message (msg);
				MoveCursor (LINES, (int) (strlen (msg)-1));
				if ((ch = (char) ReadCh ()) == CR)
					ch = ch_default;
			} while (! strchr ("eiqs\033", ch));
		}
		switch (ch) {
			case 'e':
				invoke_editor (nam);
				redraw_screen = TRUE;
				break;

			case 'i':
				invoke_ispell (nam);
				break;

			case 'q':
			case ESC:
				unlink (nam);
				clear_message ();
				*mailed_ok = FALSE;
				return (redraw_screen);

			case 's':
				/*
				 *  Open letter and get the To: line in case 
				 *  they changed it with the editor
				 */
				find_mail_header (HEADER_TO, nam, mail_to);
				sprintf (msg, txt_mailing_to, mail_to);
				wait_message (msg);
#ifdef AMIGA
				sprintf (buf, "%s <%s -f %s", mailer, nam, userid);
#else				
				sprintf (buf, "%s \"%s\" < %s", mailer, mail_to, nam);
#endif	/* AMIGA */
				if (invoke_cmd (buf)) {
					goto mail_to_someone_done;
				} else {
					error_message (txt_command_failed_s, buf);
					*mailed_ok = FALSE;
					break;
				}
		}
		if (mail_to_poster) {
			do {
				sprintf (msg, "%s [Re: %.*s]: %c", txt_quit_edit_send, 
					COLS-36, eat_re (note_h_subj), ch_default);
				wait_message (msg);
				MoveCursor (LINES, (int) (strlen (msg)-1));
				if ((ch = (char) ReadCh ()) == CR)
					ch = ch_default;
			} while (! strchr ("eqs\033", ch));
		}	
	}

mail_to_someone_done:
	unlink (nam);
	*mailed_ok = TRUE;
	return (redraw_screen);
}


int mail_bug_report ()
{
	char buf[LEN], nam[100];
	char *gateway = (char *) 0;
	char *domain = (char *) 0;
	char ch, ch_default = 's';
	char mail_to[PATH_LEN];
	FILE *fp;
	FILE *fp_uname;
	int is_debug = FALSE;
	int is_longfiles = FALSE;
	int is_nntp = FALSE;
	int is_nntp_only = FALSE;
	int uname_ok = FALSE;

	start_line_offset = 5;
	
	wait_message (txt_mail_bug_report);
	
	joinpath (nam, homedir, ".bugreport");
	if ((fp = fopen (nam, "w")) == NULL) {
		perror_message (txt_cannot_open, nam);
		return FALSE;
	}
	chmod(nam, 0600);

	fprintf (fp, "To: %s%s\n", bug_addr, add_addr);
	fprintf (fp, "Subject: BUG REPORT tin %s PL%d\n", VERSION, PATCHLEVEL);
	if (*default_organization) {
		fprintf (fp, "Organization: %s\n", default_organization);
		start_line_offset++;
	}
	if (*reply_to) {
		fprintf (fp, "Reply-To: %s\n", reply_to);
		start_line_offset++;
	}

#ifndef AMIGA
	if ((fp_uname = (FILE *) popen ("uname -a", "r")) != NULL) {
		while (fgets (buf, sizeof (buf), fp_uname) != NULL) {
			fprintf (fp, "\nBOX1: %s", buf);
			start_line_offset += 2;
			uname_ok = TRUE;
		}
		pclose (fp_uname);
	}
#endif	/* AMIGA */

	if (! uname_ok) {
		fprintf (fp, "\nPlease enter the following information:\n");
		fprintf (fp, "BOX1: Machine+OS:\n");
	}
#ifdef HAVE_LONG_FILENAMES
	is_longfiles = TRUE;
#endif
#ifdef NNTP_ABLE
	is_nntp = TRUE;
#endif
#ifdef NNTP_ONLY
	is_nntp_only = TRUE;
#endif
#ifdef DEBUG
	is_debug = TRUE;
#endif
#ifdef NNTP_INEWS_GATEWAY
	gateway = NNTP_INEWS_GATEWAY;
#endif
#ifdef NNTP_INEWS_DOMAIN
	domain = NNTP_INEWS_DOMAIN;
#endif
	fprintf (fp, "\nCFG1: active=%d  arts=%d  reread=%d  longfilenames=%d  setuid=%d\n",
		DEFAULT_ACTIVE_NUM, 
		DEFAULT_ARTICLE_NUM, 
		reread_active_file_secs,
		is_longfiles, 
		(tin_uid == real_uid ? 0 : 1));
	fprintf (fp, "CFG2: nntp=%d  nntp_only=%d  nntp_xuser=%d  nntp_xindex=%d  nntp_xspooldir=%d\n",
		is_nntp, 
		is_nntp_only,  
		xuser_supported, 
		xindex_supported, 
		xspooldir_supported);
	fprintf (fp, "CFG3: debug=%d gateway=[%s] domain=[%s]\n",
		is_debug,
		(gateway ? gateway : ""),
		(domain ? domain : ""));

	start_line_offset += 3;
	
	fprintf (fp, "\nPlease enter bug report/gripe/comment:\n");

	add_signature (fp, TRUE);
	fclose (fp);
	
	ch = 'e';
	while (1) {
		switch (ch) {
			case 'e':
				invoke_editor (nam);
				break;

			case 'i':
				invoke_ispell (nam);
				break;

			case 'q':
			case ESC:
				unlink (nam);
				clear_message ();
				return TRUE;

			case 's':
				sprintf (msg, txt_mail_bug_report_confirm, bug_addr, add_addr);
				if (prompt_yn (LINES, msg, 'n')) {
					strcpy (mail_to, bug_addr);
					find_mail_header (HEADER_TO, nam, mail_to);
					sprintf (msg, txt_mailing_to, mail_to);
					wait_message (msg);
#ifdef AMIGA
					sprintf (buf, "%s <%s -f %s", mailer, nam, userid);
#else
					sprintf (buf, "%s \"%s\" < %s", mailer, mail_to, nam);
#endif
					if (invoke_cmd (buf)) {
						sprintf (msg, txt_mailed, 1);
						info_message (msg);
						goto mail_bug_report_done;
					} else {
						error_message (txt_command_failed_s, buf);
						break;
					}
				} else {
					goto mail_bug_report_done;
				}
		}
		do {
			sprintf (msg, "%s: %c", txt_quit_edit_ispell_send, ch_default);
			wait_message (msg);
			MoveCursor (LINES, (int) strlen (msg)-1);
			if ((ch = (char) ReadCh ()) == CR)
				ch = ch_default;
		} while (! strchr ("eiqs\033", ch));
	}

mail_bug_report_done:
	unlink (nam);

	return TRUE;
}


int mail_to_author (group, respnum, copy_text)
	char *group;
	int respnum;
	int copy_text;
{
	char buf[LEN];
	char from_addr[LEN];
	char nam[100];
	char mail_to[LEN];
	char ch, ch_default = 's';
	FILE *fp;
	int redraw_screen = FALSE;

	start_line_offset = 4;
	
	wait_message (txt_reply_to_author);

	joinpath (nam, homedir, ".letter");
	if ((fp = fopen (nam, "w")) == NULL) {
		perror_message (txt_cannot_open, nam);
		return (redraw_screen);
	}
	chmod (nam, 0600);

	find_reply_to_addr (respnum, from_addr);

	fprintf (fp, "To: %s\n", from_addr);
	fprintf (fp, "Subject: Re: %s\n", eat_re(note_h_subj) );
	fprintf (fp, "Newsgroups: %s\n", note_h_newsgroups);
	if (*default_organization) {
		fprintf (fp, "Organization: %s\n", default_organization);
		start_line_offset++;
	}
	if (*reply_to) {
		fprintf (fp, "Reply-To: %s\n", reply_to);
		start_line_offset++;
	}
	fputc ('\n', fp);

	if (copy_text) {
		if (strfquote (group, respnum, buf, sizeof (buf), mail_quote_format)) {
			fprintf (fp, "%s\n", buf);
		}
		fseek (note_fp, note_mark[0], 0);
		copy_fp (note_fp, fp, quote_chars);
	}

	add_signature (fp, TRUE);
	fclose (fp);

	ch = 'e';
	while (1) {
		switch (ch) {
		case 'e':
			invoke_editor (nam);
			redraw_screen = TRUE;
			break;

		case 'q':
		case ESC:
			unlink (nam);
			clear_message ();
			return (redraw_screen);

		case 's':
			my_strncpy (mail_to, arts[respnum].from, sizeof (mail_to));
			find_mail_header (HEADER_TO, nam, mail_to);
			sprintf (msg, txt_mailing_to, mail_to);
			wait_message (msg);
			insert_x_headers (nam);
#ifdef AMIGA
			sprintf (buf, "%s <%s -f %s", mailer, nam, userid);
#else
			sprintf (buf, "%s \"%s\" < %s", mailer, mail_to, nam);
#endif
			if (invoke_cmd (buf)) {
				sprintf (msg, txt_mailed, 1);
				info_message (msg);
				goto mail_to_author_done;
			} else {
				error_message (txt_command_failed_s, buf);
				break;
			}
		}

		do {
			sprintf (msg, "%s: %c", txt_quit_edit_send, ch_default);
			wait_message (msg);
			MoveCursor (LINES, (int) strlen (msg)-1);
			if ((ch = (char) ReadCh ()) == CR)
				ch = ch_default;
		} while (! strchr ("eqs\033", ch));
	}

mail_to_author_done:
	find_mail_header (HEADER_SUBJECT, nam, buf);
	unlink (nam);
	update_art_posted_file (group, 'r', buf);

	return (redraw_screen);
}

/*
 *  Read a file grabbing the value of the specified mail header line
 */

void find_mail_header (header, file, value)
	int header;
	char *file;
	char *value;
{
	FILE *fp;
	char buf[LEN];
	char buf2[LEN];
	char new_value[LEN];
	char *p;
	int found = FALSE;

	*new_value = '\0';

	if ((fp = fopen (file, "r")) == NULL) {
		perror_message (txt_cannot_open, file);
		return;
	}

	while (!found && fgets (buf, sizeof (buf), fp) != NULL) {
		for (p = buf; *p && *p != '\n'; p++)
			continue;
		*p = '\0';

		if (*buf == '\0')
			break;

		switch (header) {
			case HEADER_TO:
				if (strncmp (buf, "To: ", 4) == 0 ||
				    strncmp (buf, "Cc: ", 4) == 0) {
					my_strncpy (buf2, &buf[4], sizeof (buf2));
					yank_to_addr (buf2, new_value);
					found = TRUE;
				}
				break;
			case HEADER_NEWSGROUPS:
				if (match_string (buf, "Newsgroups: ", new_value, sizeof (new_value))) {
					found = TRUE;
				}
				break;
			case HEADER_SUBJECT:
				if (strncmp (buf, "Subject: ", 9) == 0) {
					my_strncpy (new_value, &buf[9], sizeof (new_value));
					found = TRUE;
				}
				break;
		}
	}

	fclose (fp);

	if (new_value[0] == ' ') {
		strcpy (value, &new_value[1]);
	} else {
		strcpy (value, new_value);
	}
}


int delete_article (group, respnum)
	char *group;
	int respnum;
{
	char ch, ch_default = 'd';
	char buf[LEN];
	char delete[PATH_LEN];
	char host_name[PATH_LEN];
	char user_name[128];
	char full_name[128];
	char from[PATH_LEN];
	FILE *fp;
	int i;
	int redraw_screen = FALSE;

	/*
	 * Don't allow if not active news feed 
	 */
	if (! spooldir_is_active) {
		info_message (txt_not_active_newsfeed);
		return (redraw_screen);
	}

	/*
	 * Check if news / mail group
	 */
	i = find_group_index (group);
	if (i == -1) {
		clear_message ();
		return (redraw_screen); 
	}
	if (active[i].type == GROUP_TYPE_MAIL) {
		make_group_path (group, buf);
		sprintf (delete, "%s/%s/%ld", active[i].spooldir, buf, respnum);
		sprintf (buf, "Delete article [%s]? (y/n): ", delete);
		if (prompt_yn (LINES, buf, 'y')) {
			unlink (delete);
			return TRUE;
		} else {
			return (redraw_screen); 
		}
	}
		 
	start_line_offset = 4;

	get_host_name (host_name);
	get_user_info (user_name, full_name);
	get_from_name (user_name, host_name, full_name, delete);

	if (arts[respnum].from != arts[respnum].name) {
		sprintf (from, "%s (%s)", arts[respnum].from, arts[respnum].name);
	} else {
		my_strncpy (from, arts[respnum].from, sizeof (from));
	}

	if (debug == 2) {
		sprintf (msg, "From=[%s]  Cancel=[%s]", from, delete);		
		error_message (msg, "");
	}
	
	if (strcmp (from, delete) != 0) {
		info_message (txt_art_cannot_delete);
		return (redraw_screen);
	}
			
	clear_message ();
	
	joinpath (delete, homedir, ".cancel");
	if ((fp = fopen (delete, "w")) == NULL) {
		perror_message (txt_cannot_open, delete);
		return (redraw_screen);
	}
	chmod (delete, 0600);

	fprintf (fp, "Subject: cancel %s\n", note_h_messageid);
	if (*note_h_followup) {
		fprintf (fp, "Newsgroups: %s\n", note_h_followup);
	} else {
		fprintf (fp, "Newsgroups: %s\n", note_h_newsgroups);
	}
	fprintf (fp, "Control: cancel %s\n", note_h_messageid);
	if (*default_organization) { 
		fprintf (fp, "Organization: %s\n", default_organization);
		start_line_offset++;	
	}
	if (*reply_to) {
		fprintf (fp, "Reply-To: %s\n", reply_to);
		start_line_offset++;	
	}
	if (*note_h_distrib) {
		fprintf (fp, "Distribution: %s\n", note_h_distrib);
		start_line_offset++;	
	}
	fputc ('\n', fp);

	fprintf (fp, "Article cancelled from within tin [v%s PL%d]\n",
		VERSION, PATCHLEVEL);
	
	fclose (fp);
	
	while (1) {
		do {
			sprintf (msg, "%s [%.*s]: %c", txt_quit_edit_delete,
				COLS-30, note_h_subj, ch_default);
			wait_message (msg);
			MoveCursor (LINES, (int) strlen (msg)-1);
			if ((ch = (char) ReadCh ()) == CR)
				ch = ch_default;
		} while (! strchr ("deq\033", ch));

		switch (ch) {
		case 'e':
			invoke_editor (delete);
			redraw_screen = TRUE;
			break;

		case 'q':
		case ESC:
			unlink (delete);
			clear_message ();
			return (redraw_screen);

		case 'd':
			wait_message (txt_deleting_art);
			if (submit_file (delete)) {
				info_message (txt_art_deleted);
				goto delete_article_done;
			} else {
				error_message (txt_command_failed_s, delete);
				break;
			}
		}
	}

delete_article_done:
	find_mail_header (HEADER_SUBJECT, delete, buf);
	unlink (delete);
	update_art_posted_file (group, 'd', buf);

	return (redraw_screen);
}

/*
 * Crosspost an already existing article to another group (ie. local group)
 */
 
int crosspost_article (group, respnum)
	char *group;
	int respnum;
{
	char buf[LEN];
	char ch;
	char ch_default = 'p';
	FILE *fp;
	int i, ret_code = POSTED_NONE;
	
	start_line_offset = 4;

	if ((fp = fopen (article, "w")) == NULL) {
		perror_message (txt_cannot_open, article);
		return (ret_code);
	}
	chmod (article, 0600);

	fprintf (fp, "Subject: %s\n", eat_re (note_h_subj));
	fprintf (fp, "Newsgroups: %s\n", group);

	i = find_group_index (group);
	if (i >= 0 && active[i].attribute.organization != (char *) 0) {
		fprintf (fp, "Organization: %s\n", active[i].attribute.organization);
		start_line_offset++;
	}
	if (*reply_to) {
		fprintf (fp, "Reply-To: %s\n", reply_to);
		start_line_offset++;
	}
	if (*note_h_distrib) {
		fprintf (fp, "Distribution: %s\n", note_h_distrib);
		start_line_offset++;
	}

	fprintf (fp, "\n[ Article crossposted from %s ]", note_h_newsgroups);
	get_author (FALSE, respnum, buf);
	fprintf (fp, "\n[ Author was %s ]", buf);
	fprintf (fp, "\n[ Posted on %s ]\n\n", note_h_date);
  
	fseek (note_fp, note_mark[0], 0);
	copy_fp (note_fp, fp, "");

	add_signature (fp, FALSE);
	fclose (fp);

	while (1) {
		do {
			sprintf (msg, txt_quit_edit_xpost, 
				COLS-(strlen (txt_quit_edit_xpost)-1),
				note_h_subj, ch_default);
			wait_message (msg);
			MoveCursor (LINES, (int) strlen (msg)-1);
			if ((ch = (char) ReadCh ()) == CR)
				ch = ch_default;
		} while (! strchr ("epq\033", ch));
		switch (ch) {
		case 'e':
			invoke_editor (article);
			ret_code = POSTED_REDRAW;
			break;

		case 'q':
		case ESC:
			if (unlink_article)
				unlink (article);
			clear_message ();
			return (ret_code);

		case 'p':
			wait_message (txt_crosspost_an_article);
			if (submit_file (article)) {
				info_message (txt_art_posted);
				ret_code = POSTED_OK;
				reread_active_for_posted_arts = TRUE;		
				goto crosspost_done;
			} else {
				rename_file (article, dead_article);
				sprintf (buf, txt_art_rejected, dead_article);
				info_message (buf);
				sleep (3);
				return (ret_code);
			}
		}
	}

crosspost_done:
	find_mail_header (HEADER_SUBJECT, article, buf);
	update_art_posted_file (group, 'x', buf);

	if (unlink_article) {
		unlink (article);
	}
	
	return (ret_code);
}


int submit_file (name)
	char *name;
{
	char buf[LEN];
	char *cp = buf;
	int ret_code = FALSE;

	insert_x_headers (name);

	if (read_news_via_nntp && use_builtin_inews) {
#ifdef DEBUG
		if (debug == 2) {
			wait_message ("Using BUILTIN inews");
			sleep (3);	
		}
#endif /* DEBUG */			
		ret_code = submit_inews (name);
	} else {
#ifdef DEBUG
		if (debug == 2) {
			wait_message ("Using EXTERNAL inews");
			sleep (3);	
		}	
#endif /* DEBUG */

#ifdef AMIGA
	sprintf (cp, "uucp:c/postnews %s", name);
#else			
#	ifdef INEWSDIR
		strcpy (buf, INEWSDIR);
		strcat (buf, "/");
		cp = &buf[strlen(buf)];
#	endif /* INEWSDIR */
		sprintf (cp, "inews -h < %s", name);
#endif	/* AMIGA */
	
		ret_code = invoke_cmd (buf);
	} 

	return (ret_code);
}


void insert_x_headers (infile)
	char *infile;
{
	char line[LEN];
	char outfile[PATH_LEN];
	FILE *fp_in, *fp_out;
	int gotit = FALSE;
	
	if ((fp_in = fopen (infile, "r")) != NULL) {
		sprintf (outfile, "%s.%d", infile, process_id);
		if ((fp_out = fopen (outfile, "w")) != NULL) {
			while (fgets (line, sizeof (line), fp_in) != NULL) {
				if (! gotit && line[0] == '\n') {
					if (active[my_group[cur_groupnum]].type == GROUP_TYPE_MAIL) {
						fprintf (fp_out, "X-Mailer: TIN [version %s PL%d]\n\n",
							VERSION, PATCHLEVEL);
					} else {
						fprintf (fp_out, "X-Newsreader: TIN [version %s PL%d]\n\n",
							VERSION, PATCHLEVEL);
					}
					gotit = TRUE;
				} else {
					fputs (line, fp_out); 
				}	
			}
			fclose (fp_out);
			fclose (fp_in);
			rename_file (outfile, infile);
		}
	}
}


void find_reply_to_addr (respnum, from_addr)
	int respnum;
	char *from_addr;
{
	char buf[LEN];
	int found = FALSE;
	int len = 0;
	long orig_offset;

	orig_offset = ftell (note_fp);
	fseek (note_fp, 0L, 0);
	
	while (fgets (buf, sizeof (buf), note_fp) != NULL && 
		found == FALSE && buf[0] != '\n') {
		if (strncmp (buf, "Reply-To: ", 10) == 0) {
			strcpy (from_addr, &buf[10]);
			len = strlen (from_addr);
			from_addr[len-1] = '\0';
			sprintf (buf, "%s%s", from_addr, add_addr);
			strcpy (from_addr, buf);
			found = TRUE;
		}	
	}

	if (! found) {
		if (arts[respnum].name != arts[respnum].from) { 
			sprintf (buf, "%s%s (%s)",
				 arts[respnum].from, add_addr,
				 arts[respnum].name);
			strcpy (from_addr, buf);
		} else { 
			sprintf (from_addr, "%s%s",
				 arts[respnum].from, add_addr);
		}	
	}
	
	fseek (note_fp, orig_offset, 0);
}

/*
 * If any arts have been posted by the user reread the active
 * file so that they are shown in the unread articles number
 * for each group at the group selection level.
 */
 		
void reread_active_file_after_posting ()
{
	if (reread_active_for_posted_arts) {
		yank_active_file ();		
		reread_active_for_posted_arts = FALSE;		
	}
}
