/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : spooldir.c
 *  Author    : I.Lea & Tom Theel
 *  Created   : 08-05-92
 *  Updated   : 23-11-92
 *  Notes     : Changes spooldir to read news from (ie. news, nntp, cdrom)
 *  Copyright : (c) Copyright 1991-92 by Iain Lea & Tom Theel
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

int cur_spoolnum = 0;
int first_spooldir_on_screen;
int last_spooldir_on_screen;
int spool_top = 0;

/*
 * Change spooldir via menu of available choices
 */

int spooldir_index ()
{
#ifndef INDEX_DAEMON

	int ch;
	int n;
	int scroll_lines;
	
	spool_top = num_spooldir;

	if (! xspooldir_supported) {
		info_message (txt_spooldirs_not_supported);
		return FALSE;
	}

	if (! spool_top) {
		info_message (txt_no_spooldirs);
		return FALSE;
	}

	
	mail_setup ();		/* record mailbox size for "you have mail" */

#ifndef USE_CLEARSCREEN
	ClearScreen();
#endif

	show_spooldir_page ();		/* display spooldir selection page */
	
	while (TRUE) {
		ch = ReadCh ();

		if (ch > '0' && ch <= '9') {
			prompt_spooldir_num (ch);
			continue;
		}
		switch (ch) {
			case ESC:	/* (ESC) common arrow keys */
#ifdef AMIGA
			case 0x9b:
#endif
				switch (get_arrow_key ()) {
					case KEYMAP_UP:
						goto spooldir_up;

					case KEYMAP_DOWN:
						goto spooldir_down;

					case KEYMAP_PAGE_UP:
						goto spooldir_page_up;

					case KEYMAP_PAGE_DOWN:
						goto spooldir_page_down;

					case KEYMAP_HOME:
						if (cur_spoolnum != 0) {
							if (0 < first_spooldir_on_screen) {
#ifndef USE_CLEARSCREEN
								erase_spooldir_arrow ();
#endif					
								cur_spoolnum = 0;
								show_spooldir_page ();
							} else {
								erase_spooldir_arrow ();
								cur_spoolnum = 0;
								draw_spooldir_arrow ();
							}
						}
						break;
					
					case KEYMAP_END:
						goto end_of_list;
				}
				break;

			case '$':	/* show last page of spooldirs */
end_of_list:
				if (cur_spoolnum != spool_top - 1) {
					if (spool_top - 1 > last_spooldir_on_screen) {
#ifndef USE_CLEARSCREEN
						erase_spooldir_arrow ();
#endif					
						cur_spoolnum = spool_top - 1;
						show_spooldir_page ();
					} else {
						erase_spooldir_arrow ();
						cur_spoolnum = spool_top - 1;
						draw_spooldir_arrow ();
					}
				}
			    break;

			case '\r':	/* select spooldir */
			case '\n':
				if (set_spooldir (spooldirs[cur_spoolnum].name)) {
					wait_message (txt_reading_news_active_file);
					free_active_arrays ();
					max_active = DEFAULT_ACTIVE_NUM;
					expand_active ();
					read_mail_active_file ();
					read_news_active_file ();
					read_attributes_file ();
					read_newsgroups_file ();
					if (! read_cmd_line_groups ()) {
						read_newsrc (TRUE);
						toggle_my_groups (show_only_unread_groups, "");
					}
					set_groupname_len (FALSE);
					return TRUE;
				}	
				break;

			case ' ':		/* page down */
			case ctrl('D'):
			case ctrl('F'):		/* vi style */
spooldir_page_down:
				if (cur_spoolnum == spool_top - 1) {
					if (0 < first_spooldir_on_screen) {
#	ifndef USE_CLEARSCREEN
						erase_spooldir_arrow ();
#	endif					
						cur_spoolnum = 0;
						show_spooldir_page ();
					} else {
						erase_spooldir_arrow ();
						cur_spoolnum = 0;
						draw_spooldir_arrow ();
					}
					break;
				}
				erase_spooldir_arrow ();
				scroll_lines = (full_page_scroll ? NOTESLINES : NOTESLINES / 2);
				cur_spoolnum = ((cur_spoolnum + scroll_lines) / scroll_lines) * scroll_lines;
				if (cur_spoolnum >= spool_top) {
					cur_spoolnum = (spool_top / scroll_lines) * scroll_lines;
					if (cur_spoolnum < spool_top - 1) {
						cur_spoolnum = spool_top - 1;
					}
				}

				if (cur_spoolnum <= first_spooldir_on_screen
				||  cur_spoolnum >= last_spooldir_on_screen)
					show_spooldir_page ();
				else
					draw_spooldir_arrow ();
				break;

			case ctrl('L'):		/* redraw */
#ifndef USE_CLEARSCREEN
				ClearScreen ();
#endif
				show_spooldir_page ();
				break;

			case ctrl('N'):		/* line down */
			case 'j':
spooldir_down:
				if (cur_spoolnum + 1 >= spool_top) {
					if (0 < first_spooldir_on_screen) {
#	ifndef USE_CLEARSCREEN
						erase_spooldir_arrow ();
#	endif					
						cur_spoolnum = 0;
						show_spooldir_page ();
					} else {
						erase_spooldir_arrow ();
						cur_spoolnum = 0;
						draw_spooldir_arrow ();
					}
					break;
				}
				if (cur_spoolnum + 1 >= last_spooldir_on_screen) {
#ifndef USE_CLEARSCREEN
					erase_spooldir_arrow ();
#endif					
					cur_spoolnum++;
					show_spooldir_page ();
				} else {
					erase_spooldir_arrow ();
					cur_spoolnum++;
					draw_spooldir_arrow ();
				}
				break;

			case ctrl('P'):		/* line up */
			case 'k':
spooldir_up:
				if (cur_spoolnum == 0) {
					if (_hp_glitch) {
						erase_spooldir_arrow ();
					}
					if (spool_top > last_spooldir_on_screen) {
						cur_spoolnum = spool_top - 1;
						show_spooldir_page ();
					} else {
						erase_spooldir_arrow ();
						cur_spoolnum = spool_top - 1;
						draw_spooldir_arrow ();
					}
					break;
				}
				if (_hp_glitch) {
					erase_spooldir_arrow ();
				}
				if (cur_spoolnum <= first_spooldir_on_screen) {
					cur_spoolnum--;
					show_spooldir_page ();
				} else {
					erase_spooldir_arrow ();
					cur_spoolnum--;
					draw_spooldir_arrow ();
				}
				break;

			case 'b':		/* page up */
			case ctrl('U'):
			case ctrl('B'):		/* vi style */
spooldir_page_up:
				if (cur_spoolnum == 0) {
					if (_hp_glitch) {
						erase_spooldir_arrow ();
					}
					if (spool_top > last_spooldir_on_screen) {
						cur_spoolnum = spool_top - 1;
						show_spooldir_page ();
					} else {
						erase_spooldir_arrow ();
						cur_spoolnum = spool_top - 1;
						draw_spooldir_arrow ();
					}
					break;
				}
				erase_spooldir_arrow ();
				scroll_lines = (full_page_scroll ? NOTESLINES : NOTESLINES / 2);
				if ((n = cur_spoolnum % scroll_lines) > 0) {
					cur_spoolnum = cur_spoolnum - n;
				} else {
					cur_spoolnum = ((cur_spoolnum - scroll_lines) / scroll_lines) * scroll_lines;
				}
				if (cur_spoolnum < 0) {
					cur_spoolnum = 0;
				}
				if (cur_spoolnum < first_spooldir_on_screen
				||  cur_spoolnum >= last_spooldir_on_screen)
					show_spooldir_page ();
				else
					draw_spooldir_arrow ();
				break;

			case 'B':	/* bug/gripe/comment mailed to author */
				mail_bug_report ();
#ifndef USE_CLEARSCREEN
				ClearScreen ();
#endif
				show_spooldir_page ();
				break;
				
			case 'h':	/* help */
				show_info_page (HELP_INFO, help_spooldir, txt_spooldir_com);
				show_spooldir_page ();
				break;

			case 'H':	/* toggle mini help menu */
				toggle_mini_help (SPOOLDIR_LEVEL);
				show_spooldir_page();
				break;

			case 'I':	/* toggle inverse video */
				erase_spooldir_arrow ();
				toggle_inverse_video ();
				show_spooldir_page ();
				break;

			case 'q':	/* quit */
				return TRUE;
				
			case 'Q':	/* quit */
				write_rcfile ();
				tin_done (0);
				break;

			case 'v':	/* show tin version */
				info_message (cvers);
				break;

			default:
			    info_message(txt_bad_command);
		}
	}

#endif	/* INDEX_DAEMON */
}


void show_spooldir_page ()
{
#ifndef INDEX_DAEMON

	char buf[PATH_LEN];
	int i, j;
	int spoolname_len;

	set_signals_spooldir ();

#ifdef USE_CLEARSCREEN
	ClearScreen ();
#else
	MoveCursor (0, 0);		/* top left corner */
	CleartoEOLN ();
#endif

	sprintf (buf, txt_spooldir_selection, num_spooldir);
	show_title (buf);

#ifndef USE_CLEARSCREEN
	MoveCursor (1, 0);
	CleartoEOLN ();
#endif

	MoveCursor (INDEX_TOP, 0);

	if (cur_spoolnum >= spool_top) {
		cur_spoolnum = spool_top - 1;
	}
	if (cur_spoolnum < 0) {
		cur_spoolnum = 0;
	}
 
	if (NOTESLINES <= 0) {
		first_spooldir_on_screen = 0;
	} else {
		first_spooldir_on_screen = (cur_spoolnum / NOTESLINES) * NOTESLINES;
		if (first_spooldir_on_screen < 0) {
			first_spooldir_on_screen = 0;
		}
	}

	last_spooldir_on_screen = first_spooldir_on_screen + NOTESLINES;

	if (last_spooldir_on_screen >= spool_top) {
		last_spooldir_on_screen = spool_top;
		first_spooldir_on_screen = (cur_spoolnum / NOTESLINES) * NOTESLINES;

		if (first_spooldir_on_screen == last_spooldir_on_screen ||
			first_spooldir_on_screen < 0) {
			if (first_spooldir_on_screen < 0) {
				first_spooldir_on_screen = 0;
			} else {
				first_spooldir_on_screen = last_spooldir_on_screen - NOTESLINES;
			}
		}	
	}

	if (spool_top == 0) {
		first_spooldir_on_screen = 0;
		last_spooldir_on_screen = 0;
	}

	spoolname_len = COLS - 11;
	
	for (j=0, i = first_spooldir_on_screen; i < last_spooldir_on_screen; i++,j++) {
		sprintf (buf, "%-16.16s  %s", spooldirs[i].name, spooldirs[i].comment);
		sprintf (screen[j].col, "   %4.d  %-*.*s\r\n",
			i+1, spoolname_len, spoolname_len, buf);
		if (slow_speed_terminal) {
			strip_line (screen[j].col, strlen (screen[j].col));
			strcat (screen[j].col, "\r\n");
		}
		fputs (screen[j].col, stdout);
	}
	
#ifndef USE_CLEARSCREEN
	CleartoEOS ();
#endif

	show_mini_help (SPOOLDIR_LEVEL);

	draw_spooldir_arrow ();

#endif	/* INDEX_DAEMON */
}


int prompt_spooldir_num (ch)
	char ch;
{
	int num;

	clear_message ();

	if ((num = prompt_num (ch, txt_select_spooldir)) == -1) {
		clear_message ();
		return FALSE;
	}
	num--;		/* index from 0 (internal) vs. 1 (user) */

	if (num < 0) {
		num = 0;
	}
	if (num >= spool_top) {
		num = spool_top - 1;
	}

	if (num >= first_spooldir_on_screen
	&&  num < last_spooldir_on_screen) {
		erase_spooldir_arrow ();
		cur_spoolnum = num;
		draw_spooldir_arrow ();
	} else {
#ifndef USE_CLEARSCREEN
		erase_spooldir_arrow ();
#endif		
		cur_spoolnum = num;
		show_spooldir_page ();
	}

	return TRUE;
}


void erase_spooldir_arrow ()
{
	erase_arrow (INDEX_TOP + (cur_spoolnum-first_spooldir_on_screen));
}


void draw_spooldir_arrow()
{
	draw_arrow (INDEX_TOP + (cur_spoolnum-first_spooldir_on_screen));
}
 
/*
 * Load all spooldirs into spooldir[] array
 */
 
int load_spooldirs ()
{
	char comment[PATH_LEN];
	char line[NNTP_STRLEN];
	char name[PATH_LEN];
	char *ptr;
	int i, state;

	for (i = 0 ; i < max_spooldir ; i++) {
		spooldirs[i].state = 0;
		spooldirs[i].name = (char *) 0;
		spooldirs[i].comment = (char *) 0;
	}

	num_spooldir = 0;
	xspooldir_supported = FALSE;
	
	if (! read_news_via_nntp) {
		return (xspooldir_supported);
	}

	put_server ("spooldir list");
	(void) get_server (line, NNTP_STRLEN);
	if (*line != CHAR_OK) {
		xspooldir_supported = FALSE;
		if (atoi (line) != ERR_COMMAND) {
			fprintf (stderr, "%s", line);
			fprintf (stderr, txt_spooldir_server_error_1);
			fprintf (stderr, txt_spooldir_server_error_2);
		}
		return (xspooldir_supported);
	}
	if (debug == 1) {
		wait_message (line);
	}
	
	xspooldir_supported = TRUE;
	
	do {
		get_server (line, NNTP_STRLEN);
		if (line[0] != '.') {
			if (debug == 1) {
				printf ("%s\n", line);
			}	
			state = atoi (line);

			if ((ptr = strchr (line, ' ')) != (char *) 0) {
				strncpy (name, ++ptr, sizeof (name));
				ptr = strchr (name, ' ');
				*ptr = '\0';				
			}

			if ((ptr = strchr (line, '[')) != (char *) 0) {
				strncpy (comment, ++ptr, sizeof (comment));
				ptr = strchr (comment, ']');
				*ptr = '\0';				
			}

			if (num_spooldir >= max_spooldir - 1) {
				expand_spooldirs ();
			}

			spooldirs[num_spooldir].state = state;
			spooldirs[num_spooldir].name = str_dup (name);
			spooldirs[num_spooldir].comment = str_dup (comment);

			if (debug == 1) {
				printf ("NUM=[%d] MAX=[%d] STATE=[%d] ALIAS=[%s] COMMENT=[%s]\n", 
					num_spooldir, max_spooldir,
					spooldirs[num_spooldir].state,
					spooldirs[num_spooldir].name,
					spooldirs[num_spooldir].comment);
			}
			num_spooldir++;
		}
	} while (!((line[0] == '.') && ((line[1] == '\0') || (line[1] == '\r'))));

	return (xspooldir_supported);
}
 
/*
 * Need to select a spooldir directory for reading news from and store all
 * spooldir's in an array for later use when changing spooldir's
 */

void get_spooldir ()
{
#ifdef NNTP_ABLE
	char default_alias[32];
	int i, set_alias = FALSE;
	
	default_alias[0] = '\0';

	if (! load_spooldirs ()) {
		if (! xspooldir_supported) {
			strcpy (spooldir_alias, "news");
			set_tindir ();
		}	
		return;
	}	

	/*
	 * default to current spooldir from last session or 1st in spooldirs[]
	 */
	if (spooldir_alias[0]) {
		my_strncpy (default_alias, spooldir_alias, sizeof (default_alias));
	} else {
		my_strncpy (default_alias, spooldirs[0].name, sizeof (default_alias));
	}

	/*
	 * Try to use default spooldir. If that fails go through spooldir list
	 * looking for first available spooldir.
	 */
	if (! set_spooldir (spooldir_alias)) {
		for (i = 0 ; spooldirs[i].name != (char *) 0 ; i++) {
			if (set_spooldir (spooldirs[i].name)) {
				set_alias = TRUE;
				break;
			}
		}
		if (! set_alias) {
			error_message (txt_cannot_change_spooldir, "");
			exit (1);			
		}
	}

	/*
	 * And now set tin to act as though it is reading via NNTP
	 */
	read_news_via_nntp = TRUE;

#endif /* NNTP_ABLE */
}

/*
 * Change to specified spooldir if everythings OK.
 */
 
int set_spooldir (name)
	char *name;
{
	char line[NNTP_STRLEN];
	int respcode;

	if (cmd_line) {
		sprintf (line, "%s %s...\n", txt_changing_spooldir_to, name);
	} else {
		sprintf (line, "%s %s...", txt_changing_spooldir_to, name);
	}	
	wait_message (line);

	sprintf (line, "spooldir %s", name);
	debug_nntp ("set_spooldir", line);
	put_server (line);

	respcode = get_respcode ();

	switch (respcode) {
		case OK_SPSWITCH:	/* Switching to a different spooldir */
			my_strncpy (spooldir_alias, name, sizeof (spooldir_alias));
			set_tindir ();
			return TRUE;
		case OK_SPNOCHANGE:	/* Still using same spooldir */
			clear_message ();
			break;
		default:
			error_message ("%s", nntp_respcode (respcode));
			clear_message ();
			break;
	}
	return FALSE;
}
