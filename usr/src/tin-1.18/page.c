/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : page.c
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

char note_h_path[LEN];			/* Path:	*/
char note_h_date[PATH_LEN];		/* Date:	*/
char note_h_subj[LEN];			/* Subject:	*/
char note_h_org[PATH_LEN];		/* Organization: */
char note_h_newsgroups[LEN];		/* Newsgroups:	*/
char note_h_messageid[PATH_LEN];	/* Message-ID:	*/
char note_h_distrib[PATH_LEN];		/* Distribution: */
char note_h_followup[LEN];		/* Followup-To: */

char *glob_page_group;

FILE *note_fp;					/* the body of the current article */

int glob_respnum;
int last_resp;					/* current & previous article for - command */
int note_end;					/* we're done showing this article */
int note_line;
int note_page;					/* what page we're on */
int rotate;						/* 0=normal, 13=rot13 decode */
int this_resp;
int doing_pgdn;

long note_mark[MAX_PAGES];		/* ftells on beginnings of pages */
long note_size;					/* stat size in bytes of article */


int show_page (respnum, threadnum, group, group_path)
	int respnum;
	int *threadnum;		/* to allow movement in thread mode */
	char *group;
	char *group_path;
{
#ifndef INDEX_DAEMON

	int ch, i, n = 0;
	int copy_text;
	int kill_state = NO_KILLING;
	int old_sort_art_type = default_sort_art_type;
	int old_top;
	int posted;
	int ret_code;
	long old_artnum;
	long art;

restart:
	if (read_news_via_nntp && display_reading_prompt) {
		wait_message (txt_reading_article);
	}

	glob_respnum = respnum;
	glob_page_group = group;

	set_signals_page ();
	
	if (respnum != this_resp) {	   /* remember current & previous */
		last_resp = this_resp;	   /* articles for - command */
		this_resp = respnum;
	}

	rotate = 0;			/* normal mode, not rot13 */
	art = arts[respnum].artnum;

	if (arts[respnum].unread != ART_READ) {
		if (arts[respnum].hot && num_of_hot_arts) {
			num_of_hot_arts--;
		}
	}
	arts[respnum].unread = ART_READ;	/* mark article as read */
	
	if ((note_page = art_open (art, group_path)) == ART_UNAVAILABLE) {
/*		arts[respnum].thread = ART_EXPIRED; */ /* 21.11.92 FIXME */
		sprintf (msg, txt_art_unavailable, art);
		if (debug) {
			error_message (msg, "");
		} else {
			wait_message (msg);
		}
		return (-5);	/* special retcode to stop redrawing screen */
	} else {
		show_note_page (respnum, group);
	}

	while (TRUE) {
		ch = ReadCh ();

		if (ch >= '0' && ch <= '9') {
			n = which_thread (respnum);
			if (! num_of_responses (n)) {
				info_message (txt_no_responses);
			} else {
				n = prompt_response (ch, respnum);
				if (n != -1) {
					respnum = n;
					goto restart;
				}
			}
			continue;
		}
		switch (ch) {
			case ESC:
#ifdef AMIGA
			case 0x9b:
#endif
				switch (get_arrow_key ()) {
					case KEYMAP_PAGE_UP:
						goto page_up;

					case KEYMAP_PAGE_DOWN:
						goto page_down;

					case KEYMAP_HOME:
						goto begin_of_article;

					case KEYMAP_END:
						goto end_of_article;
				}
				break;

#ifndef NO_SHELL_ESCAPE
			case '!':
				shell_escape ();
				redraw_page (respnum, group);
				break;
#endif

			case '$':	/* goto end of article */
			case 'G':	/* 'less' compatible */
end_of_article:			
				if (show_last_page ()) {
					show_note_page (respnum, group);
				}
				break;

			case '-':	/* show last viewed article */
				if (last_resp < 0) {
					info_message (txt_no_last_message);
					break;
				}
				art_close ();
				respnum = last_resp;
				goto restart;

			case '|':	/* pipe article/thread/tagged arts to command */
				feed_articles (FEED_PIPE, PAGE_LEVEL, "Pipe", respnum, group_path);
				break;

			case '/':	/* search forwards in article */
				if (search_article (TRUE)) {
					show_note_page (respnum, group);
				}
				break;

			case '<':	/* goto first article in current thread */
				if (arts[respnum].inthread) {
					n = which_thread (respnum);
					if (n >= 0 && base[n] != respnum) {
						assert (n < top_base);
						respnum = base[n];
						art_close ();
						goto restart;
					}
				}
				break;

			case '>':	/* goto last article in current thread */
				for (i = respnum; i >= 0; i = arts[i].thread) {
					n = i;
				}
				if (n != respnum) {
					respnum = n;
					art_close ();
					goto restart;
				}
				break;

			case ' ':		/* page down or next response */
			case ctrl('D'):
			case ctrl('F'):		/* vi style */
page_down:
				if (note_page == ART_UNAVAILABLE) {
					n = next_response (respnum);
					if (n == -1) {
						return (which_thread (respnum));
					}
					respnum = n;
					goto restart;
				} else if (note_end) {
					art_close ();
					n = next_response (respnum);
					if (n == -1) {
						return (which_thread (respnum));
					}
					respnum = n;
					goto restart;
				} else {
					doing_pgdn = TRUE;
					show_note_page (respnum, group);
				}
				break;

			case '\r':
			case '\n':	/* go to start of next thread */
				art_close ();
				n = next_thread (respnum);
				if (n == -1)
					return (which_thread (respnum));
				respnum = n;
				goto restart;

			case '\t': 	/* goto next unread article */
#ifndef TAB_GOTO_NEXT_UNREAD
				if (note_page == ART_UNAVAILABLE) {
					n = next_unread (next_response (respnum));
					if (n == -1) {
						return (which_thread (respnum));
					}
					respnum = n;
					goto restart;
				} else if (note_end) {
					art_close ();
					n = next_unread (next_response (respnum));
					if (n == -1) {
						return (which_thread (respnum));
					}
					respnum = n;
					goto restart;
				} else {
					show_note_page (respnum, group);
				}	
#else
				if (note_page != ART_UNAVAILABLE) {
					art_close();
				}
				n = next_unread (next_response (respnum));
				if (n == -1) {
					return (which_thread (respnum));
				}
				respnum = n;
				goto restart;
#endif
				break;

			case ctrl('H'):	/* show article headers */
				if (note_page == ART_UNAVAILABLE) {
					n = next_response (respnum);
					if (n == -1)
						return (which_thread (respnum));
					respnum = n;
					goto restart;
				} else {
					note_page = 0;
					note_end = FALSE;
					fseek(note_fp, 0L, 0);
					show_note_page(respnum, group);
				}
				break;

			case ctrl('K'):		/* kill article */
				if (kill_art_menu (group, respnum)) {
					i = which_thread (respnum);
					if (kill_any_articles (my_group[cur_groupnum])) {
						make_threads (FALSE);
						find_base (my_group[cur_groupnum]);
						if (i >= top_base)
							i = top_base - 1;
						respnum = base[i];	
					}
				}
/*
				if (which_thread (respnum) < 0) {
					return -1;
				}
*/
				redraw_page (respnum, group);
				break;

			case ctrl('L'):		/* redraw current page of article */
				redraw_page (respnum, group);
				break;

			case ctrl('R'):		/* redraw beginning of article */
			case 'g':			/* 'less' compatible */
begin_of_article:			
				if (note_page == ART_UNAVAILABLE) {
					ClearScreen ();
					printf (txt_art_unavailable, arts[respnum].artnum);
					fflush (stdout);
				} else {
					note_page = 0;
					note_end = FALSE;
					fseek (note_fp, note_mark[0], 0);
					show_note_page (respnum, group);
				}
				break;

			case ctrl('X'):
			case '%':
			case 'd':	/* toggle rot-13 mode */
				rotate = (rotate ? 0 : 13);
				redraw_page (respnum, group);
				info_message (txt_toggled_rot13);
				break;

			case 'a':	/* author search forward */
			case 'A':	/* author search backward */
				i = (ch == 'a');
				n = search_author (my_group[cur_groupnum], respnum, i);
				if (n < 0)
					break;
				respnum = n;
				goto restart;
				/* NOTREACHED */

			case 'b':		/* page up */
			case ctrl('U'):
			case ctrl('B'):		/* vi style */
page_up:
				if (note_page == ART_UNAVAILABLE) {
					art_close ();
					n = prev_response (respnum);
					if (n == -1)
						return (which_response (respnum));
					respnum = n;
					goto restart;

				} else {
					if (note_page <= 1) {
						info_message (txt_begin_of_art);
					} else {
						note_page -= 2;
						note_end = FALSE;
						fseek (note_fp, note_mark[note_page], 0);
						show_note_page (respnum, group);
					}
				}
				break;

			case 'B':	/* bug/gripe/comment mailed to author */
				mail_bug_report ();
				redraw_page (respnum, group);
				break;
				
			case 'c':	/* catchup - mark all articles as read */
			case 'C':	/* and goto next group */
				if (!confirm_action || prompt_yn (LINES, txt_mark_all_read, 'y')) {
					for (n = 0; n < top; n++) {
						arts[n].unread = ART_READ;
					}
					ret_code = (ch == 'C' ? -4 : -1);
					fix_new_highest (cur_groupnum);
					if (cur_groupnum + 1 < group_top) {
						cur_groupnum++;
					} else {
						ret_code = -1;
					}
					art_close ();
					space_mode = TRUE;
					return ret_code;
				}
				break;

			case 'D':	/* delete an article */
				if (delete_article (group, respnum)) {
					redraw_page (respnum, group);
				}
				break;
	
			case 'f':	/* post a followup to this article */
			case 'F':
				if (! can_post) {
					info_message (txt_cannot_post);
					break;
				}
				copy_text = (ch == 'f' ? TRUE : FALSE);
				ret_code = post_response (group, respnum, copy_text);
				redraw_page (respnum, group);
				break;

			case 'h':	/* help */
				show_info_page (HELP_INFO, help_page, txt_art_pager_com);
				redraw_page (respnum, group);
				break;

			case 'H':	/* toggle mini help menu */
				toggle_mini_help (PAGE_LEVEL);
				redraw_page (respnum, group);
				break;

			case 'q':	/* return to index page */
return_to_index:
				art_close ();
				if (kill_state == NO_KILLING &&
					default_sort_art_type != old_sort_art_type) {
					make_threads (TRUE);
					find_base (my_group[cur_groupnum]);
				}
				i = which_thread (respnum);
				*threadnum = which_response (respnum);
				if (kill_state == KILLING) {
					old_top = top;
					old_artnum = arts[respnum].artnum;
					kill_any_articles (my_group[cur_groupnum]);
					make_threads (FALSE);
					find_base (my_group[cur_groupnum]);
					i = find_new_pos (old_top, old_artnum, i);
				}
				return (i);

			case 'I':	/* toggle inverse video */
				toggle_inverse_video ();
				redraw_page (respnum, group);
				break;

			case 'k':
				if (note_page == ART_UNAVAILABLE) {
					n = next_unread (next_response(respnum));
					if (n == -1)
						return (which_thread (respnum));
				} else {
					art_close ();
					n = next_unread (next_response (respnum));
					if (n == -1)
						return (which_thread (respnum));
				}
				respnum = n;
				goto restart;
				/* NOTREACHED */

			case 'K':	/* mark rest of thread as read */
				for (n = respnum; n >= 0; n = arts[n].thread) {
					if (arts[n].unread != ART_READ) {
						if (arts[n].hot && num_of_hot_arts) {
							num_of_hot_arts--;
						}
					}
					arts[n].unread = ART_READ;
				}	
				n = next_unread (next_response (respnum));
				if (n == -1)
					goto return_to_index;
				art_close ();
				respnum = n;
				goto restart;
				/* NOTREACHED */

			case 'm':	/* mail article/thread/tagged articles to somebody */
				feed_articles (FEED_MAIL, PAGE_LEVEL, "Mail", respnum, group_path);
				break;

			case 'M':	/* options menu */
				if (change_rcfile (group, FALSE) == KILLING) {
					kill_state = KILLING;
				}
				set_subj_from_size (COLS); 
				redraw_page (respnum, group);
			    break;

			case 'n':	/* skip to next article */
				art_close ();
				n = next_response (respnum);
				if (n == -1)
					return (which_thread(respnum));
				respnum = n;
				goto restart;
				/* NOTREACHED */
				
			case 'N':	/* next unread article */
				n = next_unread (next_response (respnum));
				if (n == -1)
					info_message (txt_no_next_unread_art);
				else {
					art_close ();
					respnum = n;
					goto restart;
				}
				break;

			case 'o':	/* output art/thread/tagged arts to printer */
				feed_articles (FEED_PRINT, PAGE_LEVEL, "Print", respnum, group_path);
				break;

			case 'p':	/* previous article */
				art_close ();
				n = prev_response (respnum);
				if (n == -1)
					return (which_response (respnum));
				respnum = n;
				goto restart;

			case 'P':	/* previous unread article */
				n = prev_unread (prev_response (respnum));
				if (n == -1)
				    info_message (txt_no_prev_unread_art);
				else {
					art_close ();
					respnum = n;
					goto restart;
				}
				break;

			case 'Q':	/* quit */
				return -2;
	
			case 'r':	/* reply to author through mail */
			case 'R':
				copy_text = (ch == 'r' ? TRUE : FALSE);
				mail_to_author (group, respnum, copy_text);
				redraw_page (respnum, group);
				break;

			case 's':	/* save article/thread/tagged articles */
				feed_articles (FEED_SAVE, PAGE_LEVEL, "Save", respnum, group_path);
				break;

			case 't':	/* tag/untag article for saving */
				if (arts[respnum].tagged) {
					arts[respnum].tagged = 0;
					info_message (txt_untagged_art);
				} else {
					arts[respnum].tagged = ++num_of_tagged_arts;
					info_message (txt_tagged_art);
				}
				break;

			case 'T':	/* return to group selection page */
				art_close ();
				if (kill_state == KILLING) {
					kill_any_articles (my_group[cur_groupnum]);
					make_threads (FALSE);
					find_base (my_group[cur_groupnum]);
				}
				update_newsrc (group, my_group[cur_groupnum], FALSE);
				fix_new_highest (cur_groupnum);
				return -1;

			case 'v':
				info_message (cvers);
				break;

			case 'w':	/* post a basenote */
				if (post_article (group, &posted)) {
					redraw_page (respnum, group);
				}
				break;

			case 'W':	/* display messages posted by user */
				if (user_posted_messages ()) {
					redraw_page (respnum, group);
				}
				break;

			case 'x':	/* crosspost current article */
				feed_articles (FEED_XPOST, PAGE_LEVEL, "Crosspost", respnum, group_path);
				break;

			case 'z':	/* mark article as unread (to return) */
				if (arts[n].unread != ART_UNREAD) {
					if (arts[n].hot) {
						num_of_hot_arts++;
					}
				}
				arts[respnum].unread = ART_WILL_RETURN;
				info_message (txt_art_marked_as_unread);
				break;

			default:
			    info_message(txt_bad_command);
		}
	}

#endif /* INDEX_DAEMON */
}


void redraw_page (respnum, group)
	int respnum;
	char *group;
{
	if (note_page == ART_UNAVAILABLE) {
		ClearScreen ();
		printf (txt_art_unavailable, arts[respnum].artnum);
		fflush (stdout);
	} else if (note_page > 0) {
		note_page--;
		fseek (note_fp, note_mark[note_page], 0);
		show_note_page (respnum, group);
	}
}


void show_note_page (respnum, group)
	int respnum;
	char *group;
{
#ifndef INDEX_DAEMON

	static char buf[LEN];
	char buf2[LEN+50];
	char *p, *q;
	int i, j;
	int ctrl_L;		/* form feed character detected */
	int first  = TRUE;
	int lines;
	long tmp_pos;

	if (beginner_level) {
		lines = LINES - (MINI_HELP_LINES - 1);
	} else {
		lines = LINES;
	}
	
	ClearScreen ();

	note_line = 1;

	if (note_size == 0L) {
		tmp_pos = ftell (note_fp);
		fseek (note_fp, 0L, 2);			/* goto end of article */
		note_size = ftell (note_fp);
		fseek (note_fp, tmp_pos, 0);	/* goto old position */
	}
	
	if (note_page == 0) {
		buf2[0] = '\0';
		doing_pgdn = FALSE;
		show_first_header (respnum, group);
	} else {
		show_cont_header (respnum);
	}
	
	ctrl_L = FALSE;
	while (note_line < lines) {
		if (show_last_line_prev_page) {
			note_mark[note_page+1] = ftell (note_fp);
			if (doing_pgdn && first && buf2[0]) {
				goto print_a_line;
			}
		}
		first = FALSE;
		if (fgets (buf, sizeof (buf), note_fp) == NULL) {
			note_end = TRUE;
			break;
		}

		buf[LEN-1] = '\0';
		if (rotate)
			for (p = buf, q = buf2; *p && *p != '\n' && q < &buf2[LEN]; p++) {
				if (*p == '\b' && q > buf2) {
					q--;
				} else if (*p == 12) {		/* ^L */
					*q++ = '^';
					*q++ = 'L';
					ctrl_L = TRUE;
				} else if (*p == '\t') {
					i = q - buf2;
					j = (i|7) + 1;

					while (i++ < j)
						*q++ = ' ';
				} else if (((*p) & 0xFF) < ' ') {
					*q++ = '^';
					*q++ = ((*p) & 0xFF) + '@';
				} else if (*p >= 'A' && *p <= 'Z')
					*q++ = 'A' + (*p - 'A' + rotate) % 26;
				else if (*p >= 'a' && *p <= 'z')
					*q++ = 'a' + (*p - 'a' + rotate) % 26;
				else
					*q++ = *p;
			}
		else
			for (p = buf, q = buf2; *p && *p != '\n' && q < &buf2[LEN]; p++) {
				if (*p == '\b' && q > buf2) {
					q--;
				} else if (*p == 12) {		/* ^L */
					*q++ = '^';
					*q++ = 'L';
					ctrl_L = TRUE;
				} else if (*p == '\t') {
					i = q - buf2;
					j = (i|7) + 1;

					while (i++ < j)
						*q++ = ' ';
				} else if (((*p) & 0xFF) < ' ') {
					*q++ = '^';
					*q++ = ((*p) & 0xFF) + '@';
				} else
					*q++ = *p;
			}

		*q = '\0';

print_a_line:
		if (first) {
			StartInverse ();
		}	
		strip_line (buf2, strlen (buf2));
		printf("%s\r\n", buf2);
		if (first) {
			EndInverse ();
		}	
		first = FALSE;
		doing_pgdn = FALSE;
		
		note_line += ((int) strlen (buf2) / COLS) + 1;

		if (ctrl_L) {
			break;
		}
	}

	if (! show_last_line_prev_page) {
		note_mark[++note_page] = ftell (note_fp);
	} else {
		note_page++;
	}
	
	if (ftell (note_fp) == note_size) {
		note_end = TRUE;
	}

	if (note_end) {
		MoveCursor (LINES, MORE_POS-(5+BLANK_PAGE_COLS));
		StartInverse ();	
		if (arts[respnum].thread != -1) {
			fputs (txt_next_resp, stdout);
			fflush (stdout);
		} else {
			fputs (txt_last_resp, stdout);
			fflush (stdout);
		}
		EndInverse ();
	} else {
		if (note_size > 0) {
			draw_percent_mark ((int) note_mark[note_page], (int) note_size);
		} else {
			MoveCursor (LINES, MORE_POS-BLANK_PAGE_COLS);
			StartInverse ();	
			fputs (txt_more, stdout);
			fflush (stdout);
			EndInverse ();
		}
	}
	
	show_mini_help (PAGE_LEVEL);
	
	MoveCursor (LINES, 0);

#endif /* INDEX_DAEMON */
}


void show_first_header (respnum, group)
	int respnum;
	char *group;
{
	char buf[LEN];
	char tmp[LEN];
	int whichresp;
	int x_resp;
	int pos, i, n;
	struct tm *tm;

	whichresp = which_response (respnum);
	x_resp = num_of_responses (which_thread (respnum));

	ClearScreen ();

	tm = localtime (&arts[respnum].date);
	if (! my_strftime (buf, sizeof (buf), "%a, %d %b %Y %H:%M:%S", tm)) {
		strcpy (buf, note_h_date);
	}

	pos = (COLS - (int) strlen (group)) / 2;
	for (i = strlen(buf); i < pos; i++) {
		buf[i] = ' ';
	}
	buf[i] = '\0';

	strcat (buf, group);

	for (i = strlen(buf); i < RIGHT_POS ; i++) {
		buf[i] = ' ';
	}
	buf[i] = '\0';

	sprintf (tmp, txt_thread_x_of_n, buf, which_thread (respnum) + 1, top_base);
	fputs (tmp, stdout);

	sprintf (buf, txt_art, arts[respnum].artnum);
	n = strlen (buf);
	fputs (buf, stdout);

	if (note_h_subj[0]) {
		strcpy (buf, note_h_subj);
	} else {
		strcpy (buf, arts[respnum].subject);
	}
	buf[RIGHT_POS - 5 - n] = '\0';

	pos = ((COLS - (int) strlen (buf)) / 2) - 2;

	if (pos > n) {
		MoveCursor (1, pos);
	} else {
		MoveCursor (1, n);
	}

	StartInverse ();
	fputs (buf, stdout);
	EndInverse ();

	MoveCursor (1, RIGHT_POS);
	if (whichresp)
		printf (txt_resp_x_of_n, whichresp, x_resp);
	else {
		if (x_resp == 0)
			fputs (txt_no_resp, stdout);
		else if (x_resp == 1)
			fputs (txt_1_resp, stdout);
		else
			printf (txt_x_resp, x_resp);
	}

	if (*note_h_org) {
		if (arts[respnum].name) {
			sprintf (tmp, txt_s_at_s, arts[respnum].name, note_h_org);
		} else {
			strcpy (tmp, note_h_org);
		}
	} else if (arts[respnum].name) {
		strcpy (tmp, arts[respnum].name);
	} else {
		strcpy (tmp, " ");
	}

	tmp[LEN-1] = '\0';

	sprintf (buf, "%s  ", arts[respnum].from);

	pos = COLS - 1 - (int) strlen(tmp);
	if ((int) strlen (buf) + (int) strlen (tmp) >= COLS - 1) {
		strncat (buf, tmp, COLS - 1 - (int) strlen(buf));
		buf[COLS-1] = '\0';
	} else {
		for (i = strlen(buf); i < pos; i++)
			buf[i] = ' ';
		buf[i] = '\0';
		strcat (buf, tmp);
	}
	strip_line (buf, strlen (buf));
	printf ("%s\r\n\r\n", buf);

	note_line += 4;
}


void show_cont_header (respnum)
	int respnum;
{
	int maxresp;
	int whichresp;
	int whichbase;
	char buf[LEN];

	whichresp = which_response (respnum);
	whichbase = which_thread (respnum);
	maxresp = num_of_responses (whichbase);

	assert (whichbase < top_base);

	if (whichresp) {
		sprintf(buf, txt_thread_resp_page,
			whichbase + 1,
			top_base,
			whichresp,
			maxresp,
			note_page + 1,
			note_h_subj);
	} else {
		sprintf(buf, txt_thread_page,
			whichbase + 1,
			top_base,
			note_page + 1,
			note_h_subj);
	}
	strip_line (buf, strlen (buf));
	if (COLS) {
		buf[COLS-1] = '\0';
	}
	printf("%s\r\n\r\n", buf);

	note_line += 2;
}


int art_open (art, group_path)
	long art;
	char *group_path;
{
	char buf[8192];
	char *ptr;

	note_page = 0;

	art_close ();	/* just in case */

	if ((note_fp = open_art_fp (group_path, art)) == (FILE *) 0) {
		return (ART_UNAVAILABLE);
	}

	note_h_path[0] = '\0';
	note_h_subj[0] = '\0';
	note_h_org[0] = '\0';
	note_h_date[0] = '\0';
	note_h_newsgroups[0] = '\0';
	note_h_messageid[0] = '\0';
	note_h_distrib[0] = '\0';
	note_h_followup[0] = '\0';

	while (fgets(buf, sizeof buf, note_fp) != NULL) {
		buf[8191] = '\0';

		for (ptr = buf ; *ptr && *ptr != '\n' ; ptr++) {
			if (((*ptr) & 0xFF) < ' ')
				*ptr = ' ';
		}
		*ptr = '\0';
		
		if (*buf == '\0')
			break;

  		if (match_header (buf, "Path", note_h_path, LEN))
  			continue;
  		if (match_header (buf, "Subject", note_h_subj, LEN))
  			continue;
  		if (match_header (buf, "Organization", note_h_org, PATH_LEN))
  			continue;
  		if (match_header (buf, "Date", note_h_date, PATH_LEN))
  			continue;
  		if (match_header (buf, "Newsgroups", note_h_newsgroups, LEN))
  			continue;
  		if (match_header (buf, "Message-ID", note_h_messageid, PATH_LEN))
  			continue;
  		if (match_header (buf, "Message-Id", note_h_messageid, PATH_LEN))
  			continue;
  		if (match_header (buf, "Distribution", note_h_distrib, PATH_LEN))
  			continue;
  		if (match_header (buf, "Followup-To", note_h_followup, LEN))
  			continue;
	}

	note_mark[0] = ftell (note_fp);
	note_end = FALSE;

	/*
	 * If Newsgroups is empty its a good bet the article is a mail article
	 */
	if (! note_h_newsgroups[0]) {
		strcpy (note_h_newsgroups, group_path);
		while ((ptr = (char *) strchr (note_h_newsgroups, '/'))) {
			*ptr = '.';
		}
	}
	
	return (0);
}


void art_close ()
{
	if (note_fp && note_page != ART_UNAVAILABLE) {
		fclose (note_fp);
		note_fp = (FILE *) 0;
	}
}


int prompt_response (ch, respnum)
	int ch;
	int respnum;
{
	int num;

	clear_message ();

	if ((num = prompt_num (ch, txt_read_resp)) == -1) {
		clear_message ();
		return -1;
	}

	return choose_response (which_thread (respnum), num);
}


void yank_to_addr (orig, addr)
	char *orig;
	char *addr;
{
	char *p;

	for (p = orig; *p; p++)
		if (((*p) & 0xFF) < ' ')
			*p = ' ';

	while (*addr)
		addr++;

	while (*orig) {
		while (*orig && (*orig == ' ' || *orig == '"' || *orig == ','))
			orig++;
		*addr++ = ' ';
		while (*orig && (*orig != ' ' && *orig != ',' && *orig != '"'))
			*addr++ = *orig++;
		while (*orig && (*orig == ' ' || *orig == '"' || *orig == ','))
			orig++;
		if (*orig == '(') {
			while (*orig && *orig != ')')
				orig++;
			if (*orig == ')')
				orig++;
		}
	}
	*addr = '\0';
}


int show_last_page ()
{
	char buf[LEN];
	char buf2[LEN+50];
	char *p, *q;
	int ctrl_L;		/* form feed character detected */
	int i, j;
	long tmp_pos;
	
	if (note_end) {
		return FALSE;
	}

	if (note_size == 0L) {
		tmp_pos = ftell (note_fp);
		fseek (note_fp, 0L, 2);			/* goto end of article */
		note_size = ftell (note_fp);
		fseek (note_fp, tmp_pos, 0);	/* goto old position */
	}

	while (! note_end) {
		note_line = 1;
		ctrl_L = FALSE;

		if (note_page == 0) {
			note_line += 4;
		} else {
			note_line += 2;
		}
		while (note_line < LINES) {
			if (fgets (buf, sizeof buf, note_fp) == NULL) {
				note_end = TRUE;
				break;
			}
			buf[LEN-1] = '\0';
			for (p = buf, q = buf2;	*p && *p != '\n' && q<&buf2[LEN]; p++) {
				if (*p == '\b' && q > buf2) {
					q--;
				} else if (*p == 12) {		/* ^L */
					*q++ = '^';
					*q++ = 'L';
					ctrl_L = TRUE;
				} else if (*p == '\t') {
					i = q - buf2;
					j = (i|7) + 1;

					while (i++ < j) {
						*q++ = ' ';
					}
				} else if (((*p) & 0xFF) < ' ') {
					*q++ = '^';
					*q++ = ((*p) & 0xFF) + '@';
				} else {
					*q++ = *p;
				}
			}
			*q = '\0';
			note_line += ((int) strlen (buf2) / COLS) + 1;

			if (ctrl_L) {
				break;
			}
		}
		if (note_mark[note_page] == note_size) {
			note_end = TRUE;
			note_page--;
			break;
		} else if (! note_end) {
			note_mark[++note_page] = ftell(note_fp);
		}
	}
	fseek (note_fp, note_mark[note_page], 0);
	return TRUE;
}


int match_header (buf, pat, body, len)
	char *buf;
	char *pat;
	char *body;
	int	len;
{
	int	plen = strlen (pat);

	if(strncmp (buf, pat, plen) == 0 && buf[plen] == ':' && buf[plen + 1] == ' ') {
		plen += 2;
		while (buf[plen] == ' ')
			plen++;
		strncpy (body, &buf[plen], len);
		body[len - 1] = '\0';
		return TRUE;
	}
	return FALSE;
}
