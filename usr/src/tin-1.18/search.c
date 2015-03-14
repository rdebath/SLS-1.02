/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : search.c
 *  Author    : I.Lea & R.Skrenta
 *  Created   : 01-04-91
 *  Updated   : 27-09-92
 *  Notes     :
 *  Copyright : (c) Copyright 1991-92 by Iain Lea & Rich Skrenta
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

extern FILE *note_fp;
extern int first_group_on_screen;
extern int last_group_on_screen;
extern int first_subj_on_screen;
extern int last_subj_on_screen;
extern int index_point;
extern int note_line;
extern int note_page;
extern int note_end;
extern long note_mark[MAX_PAGES];

/*
 * last search patterns
 */

char default_author_search[LEN];
char default_group_search[LEN];
char default_subject_search[LEN];
char default_art_search[LEN];


/*
 *  group.c & page.c
 */
 
int search_author (index, current_art, forward)
	int index;
	int current_art;
	int forward;
{
	char buf[LEN];
	char buf2[LEN];
	int i, patlen;

	clear_message ();

	if (forward) {
		sprintf (buf2, txt_author_search_forwards, default_author_search);
	} else {
		sprintf (buf2, txt_author_search_backwards, default_author_search);
	}
	
	if (! prompt_string (buf2, buf)) {
		return -1;
	}
	
	if (strlen (buf)) {
		strcpy (default_author_search, buf);
	} else {
		if (default_author_search[0]) {
			strcpy (buf, default_author_search);
		} else {
			info_message (txt_no_search_string);	
			return -1;
		}
	}

	wait_message (txt_searching);

	make_lower (default_author_search, buf);

	patlen = strlen (default_author_search);

	i = current_art;

	do {
		if (forward) {
			i = next_response (i);
			if (i < 0)
				i = base[0];
		} else {
			i = prev_response (i);
			if (i < 0)
				i = base[top_base - 1] + 
					num_of_responses (top_base - 1);
		}

		if (active[index].attribute.show_only_unread && 
		    arts[i].unread != ART_UNREAD) {
			continue;
		}
			
		if (arts[i].name == (char *) 0) {
			make_lower (arts[i].from, buf2);
		} else {
			sprintf (msg, "%s (%s)", arts[i].from, arts[i].name);
			make_lower (msg, buf2);
		}

		if (str_str (buf2, buf, patlen) != 0) {
			clear_message ();
			return i;
		}
	} while (i != current_art);

	info_message (txt_no_match);
	return -1;
}

/*
 * select.c
 */
 
void search_group (forward)
	int forward;
{
	char buf[LEN];
	char buf2[LEN];
	int i, patlen;

/*	if (! group_top && show_only_unread_groups) {
*/
	if (! group_top) {
		info_message (txt_no_groups);
		return;
	}

	clear_message ();

	if (forward) {
		sprintf (buf2, txt_search_forwards, default_group_search);
	} else {
		sprintf (buf2, txt_search_backwards, default_group_search);
	}

	if (! prompt_string (buf2, buf)) {
		return;
	}

	if (strlen (buf)) {
		strcpy (default_group_search, buf);
	} else {
		if (default_group_search[0]) {
			strcpy (buf, default_group_search);
		} else {
			info_message (txt_no_search_string);	
			return;
		}
	}

	wait_message (txt_searching);

	make_lower (default_group_search, buf);

	patlen = strlen (default_group_search);

	i = cur_groupnum;

	do {
		if (forward)
			i++;
		else
			i--;

		if (i >= group_top)
			i = 0;
		if (i < 0)
			i = group_top - 1;

		make_lower (active[my_group[i]].name, buf2);

		if (str_str (buf2, buf, patlen) != 0) {
			if (_hp_glitch) {
				erase_group_arrow ();
			}
			if (i >= first_group_on_screen
			&&  i < last_group_on_screen) {
				clear_message ();
				erase_group_arrow ();
				cur_groupnum = i;
				draw_group_arrow ();
			} else {
				cur_groupnum = i;
				group_selection_page ();
			}
			return;
		}
	} while (i != cur_groupnum);

	info_message (txt_no_match);
}

/*
 * group.c
 */

void search_subject (forward, group)
	int forward;
	char *group;
{
	char buf[LEN];
	char buf2[LEN];
	int i, j, patlen;

	if (index_point < 0) {
		info_message (txt_no_arts);
		return;
	}
	
	clear_message ();

	if (forward) {
		sprintf (buf2, txt_search_forwards, default_subject_search);
	} else {
		sprintf (buf2, txt_search_backwards, default_subject_search);
	}

	if (! prompt_string (buf2, buf)) {
		return;
	}

	if (strlen (buf)) {
		strcpy (default_subject_search, buf);
	} else {
		if (default_subject_search[0]) {
			strcpy (buf, default_subject_search);
		} else {
			info_message (txt_no_search_string);	
			return;
		}
	}

	wait_message (txt_searching);

	make_lower (default_subject_search, buf);

	patlen = strlen (default_subject_search);

	i = index_point;

	do {
		if (forward)
			i++;
		else
			i--;

		if (i >= top_base)
			i = 0;
		if (i < 0)
			i = top_base - 1;

		j = (int) base[i];

		make_lower (arts[j].subject, buf2);

		if (str_str (buf2, buf, patlen) != 0) {
			if (_hp_glitch) {
				erase_subject_arrow ();
			}
			if (i >= first_subj_on_screen
			    &&  i < last_subj_on_screen) {
				clear_message ();
				erase_subject_arrow ();
				index_point = i;
				draw_subject_arrow ();
			} else {
				index_point = i;
				show_group_page ();
			}
			return;
		}
	} while (i != index_point);

	info_message (txt_no_match);
}

/*
 *  page.c (search article body)
 */

int search_article (forward)
	int forward;
{
	char buf[LEN];
	char buf2[LEN];
	char string[LEN];
	char pattern[LEN];
	char *p, *q;
	int ctrl_L;
	int i, j, patlen;
	int orig_note_end;
	int orig_note_page;

	clear_message ();

	if (forward) {
		sprintf (buf2, txt_search_forwards, default_art_search);
	} else {
		sprintf (buf2, txt_search_backwards, default_art_search);
	}

	if (! prompt_string (buf2, buf)) {
		return FALSE;
	}

	if (strlen (buf)) {
		strcpy (default_art_search, buf);
	} else {
		if (default_art_search[0]) {
			strcpy (buf, default_art_search);
		} else {
			info_message (txt_no_search_string);	
			return FALSE;
		}
	}

	wait_message (txt_searching);
	
	make_lower (default_art_search, pattern);

	patlen = strlen (default_art_search);

	/*
	 *  save current position in article
	 */
	orig_note_end = note_end;
	orig_note_page = note_page;
	
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
				} else if (*p == '\f') {		/* ^L */
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

			make_lower (buf2, string);

			if (str_str (string, pattern, patlen) != 0) {
				fseek (note_fp, note_mark[note_page], 0);
				return TRUE;
			}

			note_line += ((int) strlen(buf2) / COLS) + 1;

			if (ctrl_L) {
				break;
			}
		}
		if (! note_end) {
			note_mark[++note_page] = ftell (note_fp);
		}
	}

	note_end = orig_note_end;
	note_page = orig_note_page;
	fseek (note_fp, note_mark[note_page], 0);
	info_message (txt_no_match);
	return FALSE;
}


void make_lower (s, t)
	char *s;
	char *t;
{

	while (*s) {
		if (isupper(*s))
			*t = tolower(*s);
		else
			*t = *s;
		s++;
		t++;
	}
	*t = 0;
}
