/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : select.c
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


extern char cvers[LEN];
extern int index_point;

char default_goto_group[LEN];
int default_move_group;
int cur_groupnum = 0;
int first_group_on_screen;
int last_group_on_screen;
int space_mode;
int yank_in_active_file = TRUE;


void selection_index (start_groupnum)
	int start_groupnum;
{
#ifndef INDEX_DAEMON

	char buf[LEN];
	char post_group[LEN];
	int ch, i, n;
	int patlen;
	int posted;
	int scroll_lines;
	int subscribe_num;
	
	cur_groupnum = start_groupnum;
	
	mail_setup ();		/* record mailbox size for "you have mail" */

#ifdef READ_CHAR_HACK
	setbuf (stdin, 0);
#endif

#ifndef USE_CLEARSCREEN
	ClearScreen();
#endif

	set_groupname_len (FALSE);	/* find longest subscribedto groupname */
	group_selection_page ();	/* display group selection page */
	set_alarm_signal ();		/* set alarm signal for resync_active_file () */
	
	while (TRUE) {
		reread_active_file_after_posting ();
#ifndef DONT_REREAD_ACTIVE_FILE
		resync_active_file ();	/* reread active file if alarm set */
#endif
		ch = ReadCh ();
#ifndef DONT_REREAD_ACTIVE_FILE
		if (ch != 'q' && ch != 'Q') {
			resync_active_file ();
		}
#endif

		if (ch > '0' && ch <= '9') {
			prompt_group_num (ch);
			continue;
		}
		switch (ch) {
			case ESC:	/* (ESC) common arrow keys */
#ifdef AMIGA
			case 0x9b:
#endif
				switch (get_arrow_key ()) {
					case KEYMAP_UP:
						goto select_up;

					case KEYMAP_DOWN:
						goto select_down;

					case KEYMAP_PAGE_UP:
						goto select_page_up;

					case KEYMAP_PAGE_DOWN:
						goto select_page_down;

					case KEYMAP_HOME:
						if (cur_groupnum != 0) {
							if (0 < first_group_on_screen) {
#ifndef USE_CLEARSCREEN
								erase_group_arrow();
#endif					
								cur_groupnum = 0;
								group_selection_page();
							} else {
								erase_group_arrow();
								cur_groupnum = 0;
								draw_group_arrow();
							}
						}
						break;
					
					case KEYMAP_END:
						goto end_of_list;
				}
				break;

#ifndef NO_SHELL_ESCAPE
			case '!':
				shell_escape ();
				group_selection_page ();
				break;
#endif

			case '$':	/* show last page of groups */
end_of_list:
				if (cur_groupnum != group_top - 1) {
					if (group_top - 1 > last_group_on_screen) {
#ifndef USE_CLEARSCREEN
						erase_group_arrow();
#endif					
						cur_groupnum = group_top - 1;
						group_selection_page();
					} else {
						erase_group_arrow();
						cur_groupnum = group_top - 1;
						draw_group_arrow();
					}
				}
			    break;

			case '/':	/* search forward */
			case '?':	/* search backward */
				i = (ch == '/');
				search_group (i);
				break;

			case '\r':	/* go into group */
			case '\n':
				if (group_top == 0) {
					info_message (txt_no_groups);
					break;
				}
				
				n = my_group[cur_groupnum];
				if (active[n].min <= active[n].max) {
					space_mode = pos_first_unread;
					clear_message();
					index_point = -1;
					do {
						n = my_group[cur_groupnum];
						group_page (active[n].name);
					} while (index_point == -3 || index_point == -4);
#ifndef DONT_REREAD_ACTIVE_FILE					
					if (! reread_active_file)
#endif
						group_selection_page ();
				} else {
					info_message (txt_no_arts);
				}
				break;

			case '\t':	/* enter next group containing unread articles */
			case 'n':
				next_unread_group (TRUE);
				break;

			case ' ':		/* page down */
			case ctrl('D'):
			case ctrl('F'):		/* vi style */
select_page_down:
				if (! group_top) {
					break;
				}
				if (cur_groupnum == group_top - 1) {
					if (0 < first_group_on_screen) {
#	ifndef USE_CLEARSCREEN
						erase_group_arrow();
#	endif					
						cur_groupnum = 0;
						group_selection_page();
					} else {
						erase_group_arrow();
						cur_groupnum = 0;
						draw_group_arrow();
					}
					break;
				}
				erase_group_arrow ();
				scroll_lines = (full_page_scroll ? NOTESLINES : NOTESLINES / 2);
				cur_groupnum = ((cur_groupnum + scroll_lines) / scroll_lines) * scroll_lines;
				if (cur_groupnum >= group_top) {
					cur_groupnum = (group_top / scroll_lines) * scroll_lines;
					if (cur_groupnum < group_top - 1) {
						cur_groupnum = group_top - 1;
					}
				}

				if (cur_groupnum <= first_group_on_screen
				||  cur_groupnum >= last_group_on_screen)
					group_selection_page ();
				else
					draw_group_arrow ();
				break;

			case ctrl('K'):		/* delete group */
				if (group_top <= 0) {
					info_message (txt_no_groups_to_delete);
					break;
				}

				sprintf (buf, active[my_group[cur_groupnum]].name);
				sprintf (msg, txt_del_group_in_newsrc, buf);
				if (prompt_yn (LINES, msg, 'y')) {
					delete_group (active[my_group[cur_groupnum]].name);
					active[my_group[cur_groupnum]].my_group = UNSUBSCRIBED;	

					group_top--;
					for (i = cur_groupnum; i < group_top; i++) {
#if 0
						active[my_group[i]].unread = 
							active[my_group[i+1]].unread;
#endif							
						my_group[i] = my_group[i+1];
					}
					if (cur_groupnum >= group_top)
						cur_groupnum = group_top - 1;	

					set_groupname_len (FALSE);	
					group_selection_page ();
					sprintf (msg, txt_group_deleted, buf);
					info_message (msg);
				}
				break;

			case ctrl('L'):		/* redraw */
#ifndef USE_CLEARSCREEN
				ClearScreen ();
#endif
				group_selection_page ();
				break;

			case ctrl('N'):		/* line down */
			case 'j':
select_down:
				if (! group_top) {
					break;
				}
				if (cur_groupnum + 1 >= group_top) {
					if (0 < first_group_on_screen) {
#	ifndef USE_CLEARSCREEN
						erase_group_arrow();
#	endif					
						cur_groupnum = 0;
						group_selection_page();
					} else {
						erase_group_arrow();
						cur_groupnum = 0;
						draw_group_arrow();
					}
					break;
				}
				if (cur_groupnum + 1 >= last_group_on_screen) {
#ifndef USE_CLEARSCREEN
					erase_group_arrow();
#endif					
					cur_groupnum++;
					group_selection_page();
				} else {
					erase_group_arrow();
					cur_groupnum++;
					draw_group_arrow();
				}
				break;

			case ctrl('P'):		/* line up */
			case 'k':
select_up:
				if (! group_top) {
					break;
				}
				if (cur_groupnum == 0) {
					if (_hp_glitch) {
						erase_group_arrow ();
					}
					if (group_top > last_group_on_screen) {
						cur_groupnum = group_top - 1;
						group_selection_page ();
					} else {
						erase_group_arrow ();
						cur_groupnum = group_top - 1;
						draw_group_arrow ();
					}
					break;
				}
				if (_hp_glitch) {
					erase_group_arrow ();
				}
				if (cur_groupnum <= first_group_on_screen) {
					cur_groupnum--;
					group_selection_page ();
				} else {
					erase_group_arrow ();
					cur_groupnum--;
					draw_group_arrow ();
				}
				break;

			case ctrl('R'):	/* reset .newsrc */
			    if (prompt_yn (LINES, txt_reset_newsrc, 'n')) {
					reset_newsrc ();
					cur_groupnum = 0;
					group_selection_page ();
			    }
			    break;

			case 'b':		/* page up */
			case ctrl('U'):
			case ctrl('B'):		/* vi style */
select_page_up:
				if (! group_top) {
					break;
				}
				if (cur_groupnum == 0) {
					if (_hp_glitch) {
						erase_group_arrow ();
					}
					if (group_top > last_group_on_screen) {
						cur_groupnum = group_top - 1;
						group_selection_page ();
					} else {
						erase_group_arrow ();
						cur_groupnum = group_top - 1;
						draw_group_arrow ();
					}
					break;
				}
				erase_group_arrow ();
				scroll_lines = (full_page_scroll ? NOTESLINES : NOTESLINES / 2);
				if ((n = cur_groupnum % scroll_lines) > 0) {
					cur_groupnum = cur_groupnum - n;
				} else {
					cur_groupnum = ((cur_groupnum - scroll_lines) / scroll_lines) * scroll_lines;
				}
				if (cur_groupnum < 0) {
					cur_groupnum = 0;
				}
				if (cur_groupnum < first_group_on_screen
				||  cur_groupnum >= last_group_on_screen)
					group_selection_page ();
				else
					draw_group_arrow ();
				break;

			case 'B':	/* bug/gripe/comment mailed to author */
				mail_bug_report ();
#ifndef USE_CLEARSCREEN
				ClearScreen ();
#endif
				group_selection_page ();
				break;

			case 'c':	/* catchup - mark all articles as read */
			case 'C':	/* and goto next unread group */
				if (group_top == 0) {
					break;
				}
				catchup_group ((ch == 'C'));
			    break;

			case 'd':	/* toggle newsgroup descriptions */
				show_description = !show_description;
				if (show_description) {
					read_newsgroups_file ();
				}
				set_groupname_len (FALSE);
				group_selection_page ();
				break;

			case 'g':	/* prompt for a new group name */
				if ((n = choose_new_group ()) >= 0) {
					erase_group_arrow ();
					if (active[my_group[n]].my_group != SUBSCRIBED) {
						subscribe (active[my_group[n]].name, ':',
							my_group[n], FALSE);
						cur_groupnum = reposition_group (active[my_group[n]].name,
							(n ? n : cur_groupnum));
					} else {
						cur_groupnum = n;
					}
					set_groupname_len (FALSE);				
					if (cur_groupnum < first_group_on_screen ||
						cur_groupnum >= last_group_on_screen ||
						cur_groupnum != n) {
						group_selection_page();
					} else {
						clear_message ();
						draw_group_arrow();
					}
				}
				break;

			case 'h':	/* help */
				show_info_page (HELP_INFO, help_select, txt_group_select_com);
				group_selection_page ();
				break;

			case 'H':	/* toggle mini help menu */
				toggle_mini_help (SELECT_LEVEL);
				group_selection_page ();
				break;

			case 'I':	/* toggle inverse video */
				erase_group_arrow ();
				toggle_inverse_video ();
				group_selection_page ();
				break;

			case 'l':	/* list available spooldirs */
				if (spooldir_index ()) {
					group_selection_page ();
				}	
				break;

			case 'm':	/* reposition group within group list */
				if (active[my_group[cur_groupnum]].my_group == SUBSCRIBED) {
					n = cur_groupnum;
					cur_groupnum = reposition_group (active[my_group[n]].name, n);
					if (_hp_glitch) {
						erase_group_arrow ();
					}
					if (cur_groupnum < first_group_on_screen ||
						cur_groupnum >= last_group_on_screen ||
						cur_groupnum != n) {
						group_selection_page ();
					} else {
						i = cur_groupnum;
						cur_groupnum = n;
						erase_group_arrow ();
						cur_groupnum = i;
						clear_message ();
						draw_group_arrow ();
					}
				}
				break;

			case 'M':	/* options menu */
				set_alarm_clock_off ();
				change_rcfile ("", TRUE);
				free_attributes_array ();
				read_attributes_file ();
				group_selection_page ();
				set_alarm_clock_on ();
				break;

			case 'N':	/* goto next unread group */
				next_unread_group (FALSE);
				break;

			case 'q':	/* quit */
			case 'Q':
				write_rcfile ();
				tin_done (0);
				break;

			case 'r':
	 			/* 
	 			 * If in show_only_unread_groups mode toggle
	 			 * all subscribed to groups and only  groups
	 			 * that contain unread articles
	 			 *
	 			 * Disabled when started with cmdline groups
	 			 */
	 			if (! read_cmd_line_groups ()) { 
	 				show_only_unread_groups = !show_only_unread_groups;
					if (show_only_unread_groups) {
						wait_message (txt_reading_new_groups);
					} else {
						wait_message (txt_reading_all_groups);
					}
					toggle_my_groups (show_only_unread_groups, "");
					set_groupname_len (FALSE);
					group_selection_page ();
				}
				break;

			case 's':	/* subscribe to current group */
				if (group_top == 0) {
					break;
				}
				if (active[my_group[cur_groupnum]].my_group != SUBSCRIBED) {
					mark_screen (SELECT_LEVEL, cur_groupnum - first_group_on_screen, 2, " ");

					subscribe (active[my_group[cur_groupnum]].name,
						':', my_group[cur_groupnum], FALSE);

					sprintf (buf, txt_subscribed_to, active[my_group[cur_groupnum]].name);
					info_message (buf);
				}
			    break;

			case 'S':	/* subscribe to groups matching pattern */
				if (group_top == 0) {
					break;
				}
				if (prompt_string (txt_subscribe_pattern, buf) && buf[0]) {
					wait_message (txt_subscribing);
					patlen = strlen (buf);
					for (subscribe_num=0, i=0 ; i < group_top ; i++) {
#ifdef NO_REGEX 
						if (str_str (active[my_group[i]].name, buf, patlen)) {
#else		
						if (wildmat (active[my_group[i]].name, buf)) {
#endif		
			   		 		if (active[my_group[i]].my_group != SUBSCRIBED) {
#ifndef SLOW_SCREEN_UPDATE
								sprintf (msg, txt_subscribing_to, active[my_group[i]].name);
								wait_message (msg);
#endif								
								subscribe (active[my_group[i]].name,
									':', my_group[i], FALSE);
							}
							subscribe_num++;
						}
					}
					if (subscribe_num) {
						group_selection_page ();	
						sprintf (buf, txt_subscribed_num_groups, subscribe_num);
						info_message (buf);
					} else {
						info_message (txt_no_match);
					}
				} else {
					clear_message ();
				}
			    break;

			case 'u':	/* unsubscribe to current group */
				if (group_top == 0) {
					break;
				}
				if (active[my_group[cur_groupnum]].my_group == SUBSCRIBED) {
					mark_screen (SELECT_LEVEL, cur_groupnum - first_group_on_screen, 2, "u");

					subscribe(active[my_group[cur_groupnum]].name,
						'!', my_group[cur_groupnum], FALSE);

					sprintf(buf, txt_unsubscribed_to,active[my_group[cur_groupnum]].name);
					info_message(buf);
				}
				goto_next_group_on_screen ();
				break;

			case 'U':	/* unsubscribe to groups matching pattern */
				if (group_top == 0) {
					break;
				}
				if (prompt_string (txt_unsubscribe_pattern, buf) && buf[0]) {	
					wait_message (txt_unsubscribing);
					patlen = strlen (buf);	
					for (subscribe_num=0, i=0 ; i < group_top ; i++) {		
#ifdef NO_REGEX
						if (str_str (active[my_group[i]].name, buf, patlen)) {
#else		
						if (wildmat (active[my_group[i]].name, buf)) {
#endif		
			   		 		if (active[my_group[i]].my_group == SUBSCRIBED) {
#ifndef SLOW_SCREEN_UPDATE
								sprintf (msg, txt_unsubscribing_from, active[my_group[i]].name);
								wait_message (msg);
#endif								
								subscribe (active[my_group[i]].name,
									'!', my_group[i], FALSE);
							}
							subscribe_num++;
						}
					}
					if (subscribe_num) {
						group_selection_page ();	
						sprintf (buf, txt_unsubscribed_num_groups, subscribe_num);
						info_message (buf);
					} else {
						info_message (txt_no_match);
					}
				} else {
					clear_message ();
				}
			    break;

			case 'v':	/* show tin version */
				info_message (cvers);
				break;

			case 'w':	/* post a basenote */
				if (can_post) {
					if (group_top == 0) {
						if (! prompt_string (txt_post_newsgroup, buf)) 
							break;
						if (buf[0] == '\0')
							break;
						strcpy (post_group, buf);
					} else {
						strcpy (post_group, active[my_group[cur_groupnum]].name);
					}
					if (post_article (post_group, &posted)) {
						group_selection_page ();
					}
				}
				break;

			case 'W':	/* display messages posted by user */
				if (user_posted_messages ()) {
					group_selection_page ();
				}
				break;

			case 'y':	/* pull in rest of groups from active */
				if (yank_in_active_file) {
					wait_message (txt_yanking_all_groups);
					set_alarm_clock_off ();
					n = group_top;
					for (i = 0; i < num_active; i++) {
						active[i].my_group = UNSUBSCRIBED;
					}
					read_newsrc (FALSE);
					for (i = 0; i < num_active; i++) {
						if (active[i].my_group & UNSUBSCRIBED) {
							active[i].my_group &= ~UNSUBSCRIBED;
							active[i].unread = -1;
							my_group[group_top] = i;
							group_top++;
						}
					}
					if (n < group_top) {
						sprintf (buf, txt_added_groups, group_top - n,
							group_top - n == 1 ? "" : txt_plural);
						set_groupname_len (yank_in_active_file);
						group_selection_page ();
						info_message (buf);
					} else {
					    info_message (txt_no_groups_to_yank_in);
					}
					yank_in_active_file = FALSE;
				} else {
					wait_message (txt_yanking_sub_groups);
					read_newsrc (TRUE);
					if (_hp_glitch) {
						erase_group_arrow ();
					}					
					cur_groupnum = group_top - 1;
					set_groupname_len (yank_in_active_file);
					group_selection_page ();
					yank_in_active_file = TRUE;
					set_alarm_clock_on ();
				}
				break;

			case 'Y':	/* yank active file to see if any new news */
				yank_active_file ();
				break;

			case 'z':	/* mark group unread */
				if (group_top == 0) {
					break;
				}
				n = cur_groupnum;
				update_newsrc (active[my_group[n]].name, my_group[n], TRUE);
				cur_groupnum = 0;
				group_top = 0;
				read_newsrc (TRUE);
				cur_groupnum = n;
				if (active[my_group[cur_groupnum]].unread) {
					sprintf (msg, "%5d", active[my_group[cur_groupnum]].unread);
				} else {
					strcpy (msg, "     ");
				}
				mark_screen (SELECT_LEVEL, cur_groupnum - first_group_on_screen, 9, msg);
				break;
				
			case 'Z':	/* undelete groups deleted by ctrl-K */
				if (undel_group ()) {
					set_groupname_len (FALSE);
					group_selection_page ();
					info_message (txt_group_undeleted);
				}
				break;

			default:
			    info_message(txt_bad_command);
		}
	}

#endif /* INDEX_DAEMON */
}


void group_selection_page ()
{
#ifndef INDEX_DAEMON

	char buf[LEN];
	char new[10];
	char subs;
	int i, j, n;
	int blank_len;

	set_signals_select ();

#ifdef USE_CLEARSCREEN
	ClearScreen ();
#else
	MoveCursor (0, 0);		/* top left corner */
	CleartoEOLN ();
#endif

	if (read_news_via_nntp || xspooldir_supported) {
		sprintf (buf, "%s (%s  %d%s)", txt_group_selection, 
			(xspooldir_supported ? spooldir_alias : nntp_server), 
			group_top, (show_only_unread_groups ? " R" : ""));
	} else {
		sprintf (buf, "%s (%d%s)", txt_group_selection, group_top,
			(show_only_unread_groups ? " R" : ""));
	}	
	show_title (buf);

#ifndef USE_CLEARSCREEN
	MoveCursor (1, 0);
	CleartoEOLN ();
#endif

	MoveCursor (INDEX_TOP, 0);

	if (cur_groupnum >= group_top) {
		cur_groupnum = group_top - 1;
	}
	if (cur_groupnum < 0) {
		cur_groupnum = 0;
	}
 
	if (NOTESLINES <= 0) {
		first_group_on_screen = 0;
	} else {
		first_group_on_screen = (cur_groupnum / NOTESLINES) * NOTESLINES;
		if (first_group_on_screen < 0) {
			first_group_on_screen = 0;
		}
	}

	last_group_on_screen = first_group_on_screen + NOTESLINES;

	if (last_group_on_screen >= group_top) {
		last_group_on_screen = group_top;
		first_group_on_screen = (cur_groupnum / NOTESLINES) * NOTESLINES;

		if (first_group_on_screen == last_group_on_screen ||
			first_group_on_screen < 0) {
			if (first_group_on_screen < 0) {
				first_group_on_screen = 0;
			} else {
				first_group_on_screen = last_group_on_screen - NOTESLINES;	
			}
		}	
	}

	if (group_top == 0) {
		first_group_on_screen = 0;
		last_group_on_screen = 0;
	}

	if (show_description) {
		blank_len = (COLS - (groupname_len + SELECT_MISC_COLS)) + 2;
	} else {
		blank_len = (COLS - (groupname_len + SELECT_MISC_COLS)) + 4;
	}
	
	for (j=0, i=first_group_on_screen; i < last_group_on_screen; i++, j++) {
		switch (active[my_group[i]].unread) {
			case -2:
				strcpy (new, "    ?");
				break;

			case -1:
				strcpy (new, "    -");
				break;

			case 0:
				strcpy (new, "     ");
				break;

			default:
				sprintf (new, "%5.d", active[my_group[i]].unread);
		}
		
		n = my_group[i];
		if (active[n].my_group & SUBSCRIBED) {	/* subscribed? */
			subs = ' ';
		} else {
			subs = 'u';	/* u next to unsubscribed groups */
		}

		if (show_description) {		
			if (draw_arrow_mark) {
				sprintf (screen[j].col, "  %c %4.d %s  %-*.*s  %-*.*s\r\n",
				       subs, i+1, new, groupname_len, groupname_len, 
				       active[n].name, blank_len, blank_len, 
				       (active[n].description ? active[n].description : " "));
			} else {
				sprintf (screen[j].col, "  %c %4.d %s  %-*.*s  %-*.*s\r\n",
					subs, i+1, new, groupname_len, groupname_len, 
					active[n].name, blank_len, blank_len, 
				       (active[n].description ? active[n].description : " "));
			}
		} else {
			if (draw_arrow_mark) {
				sprintf (screen[j].col, "  %c %4.d %s  %-*.*s\r\n",
					subs, i+1, new, groupname_len, groupname_len, active[n].name);
			} else {
				sprintf (screen[j].col, "  %c %4.d %s  %-*.*s%*s\r\n",
					subs, i+1, new, groupname_len, groupname_len, active[n].name,
					blank_len, " ");
			}
		}
		if (slow_speed_terminal) {
			strip_line (screen[j].col, strlen (screen[j].col));
			strcat (screen[j].col, "\r\n");
		}
 		CleartoEOLN ();
 		fputs (screen[j].col, stdout);
	}

#ifndef USE_CLEARSCREEN
	CleartoEOS ();
#endif

	show_mini_help (SELECT_LEVEL);

	if (group_top <= 0) {
		info_message (txt_no_groups);
		return;
	} else if (last_group_on_screen == group_top) {
		info_message (txt_end_of_groups);
	}
	
	draw_group_arrow ();

#endif /* INDEX_DAEMON */
}


int prompt_group_num (ch)
	int ch;
{
	int num;

	clear_message ();

	if ((num = prompt_num (ch, txt_select_group)) == -1) {
		clear_message ();
		return FALSE;
	}
	num--;		/* index from 0 (internal) vs. 1 (user) */

	if (num < 0) {
		num = 0;
	}
	if (num >= group_top) {
		num = group_top - 1;
	}

	if (num >= first_group_on_screen
	&&  num < last_group_on_screen) {
		erase_group_arrow ();
		cur_groupnum = num;
		draw_group_arrow ();
	} else {
#ifndef USE_CLEARSCREEN
		erase_group_arrow ();
#endif		
		cur_groupnum = num;
		group_selection_page ();
	}

	return TRUE;
}


void erase_group_arrow ()
{
	erase_arrow (INDEX_TOP + (cur_groupnum-first_group_on_screen));
}


void draw_group_arrow()
{
	draw_arrow (INDEX_TOP + (cur_groupnum-first_group_on_screen));
}


void yank_active_file ()
{
	reread_active_file = TRUE;
	resync_active_file ();
}


int choose_new_group ()
{
	char buf[LEN];
	char *p;
	int ret;

	if (! group_top && show_only_unread_groups) {
		return -1;
	}
	
	sprintf (msg, txt_newsgroup, default_goto_group);

	if (! prompt_string (msg, buf)) {
		return -1;
	}
	
	if (strlen (buf)) {
		strcpy (default_goto_group, buf);
	} else {
		if (default_goto_group[0]) {
			strcpy (buf, default_goto_group);
		} else {
			return -1;
		}
	}

	for (p = buf; *p && (*p == ' ' || *p == '\t'); p++)
		continue;
	if (*p == '\0')
		return -1;

	clear_message ();

	if ((ret = add_group (p, TRUE)) < 0) {
		sprintf (msg, txt_not_in_active_file, p);
		info_message (msg);
	}

	return ret;
}

/*
 *  Add a group to the users selection list (my_group[])
 *  Return the index of my_group[] if group is added or was already
 *  there.  Return -1 if named group is not in active[].
 */

int add_group (s, get_unread)
	char *s;
	int get_unread;			/* look in .newsrc for sequencer unread info? */
{
	long h;
	int i, j;

	h = hash_groupname (s);

	for (i = group_hash[h]; i >= 0; i = active[i].next) {
		if (strcmp (s, active[i].name) == 0) {
			for (j = 0; j < group_top; j++) {
				if (my_group[j] == i) {
					return j;
				}
			}

			active[i].my_group &= ~UNSUBSCRIBED;   /* mark that we got it */

			my_group[group_top] = i;

			if (get_unread) {
				active[my_group[group_top]].unread = get_line_unread (s, i);
			} else {
				active[my_group[group_top]].unread = -1;
			}
			group_top++;

			return (group_top - 1);
		}
	}

	return -1;
}


int reposition_group (group, default_num)
	char *group;
	int default_num;
{
	char buf[LEN];
	char pos[LEN];
	int pos_num = 0;

	sprintf (buf, txt_newsgroup_position, group, 
		(default_move_group ? default_move_group : default_num+1));
	
	if (! prompt_string (buf, pos)) {
		return default_num;
	}	

	if (strlen (pos)) {
		if (pos[0] == '$') {
			pos_num = group_top;
		} else {
			pos_num = atoi (pos);
		}	
	} else {
		if (default_move_group) {
			pos_num = default_move_group;
		} else {
			return default_num;
		}
	}
		
	if (pos_num > group_top) {
		pos_num = group_top;
	} else if (pos_num <= 0) {
		pos_num = 1;
	}

	sprintf (buf, txt_moving, group);
	wait_message (buf);
	
	if (pos_group_in_newsrc (group, pos_num)) {
		read_newsrc (TRUE);
		default_move_group = pos_num;
		return (pos_num-1);
	} else {
		default_move_group = default_num + 1;
		return (default_num);
	}
}


void catchup_group (goto_next_unread_group)
	int goto_next_unread_group;
{	
	sprintf (msg, txt_mark_group_read, active[my_group[cur_groupnum]].name);
	if (! confirm_action || prompt_yn (LINES, msg, 'y')) {
		active[my_group[cur_groupnum]].unread = 0;
		mark_group_read (active[my_group[cur_groupnum]].name,
			my_group[cur_groupnum]);

		mark_screen (SELECT_LEVEL, cur_groupnum - first_group_on_screen, 
			9, "     ");

		goto_next_group_on_screen ();
		
		if (goto_next_unread_group) {
			next_unread_group (FALSE);	
		}
	}
}


void next_unread_group (enter_group)
	int enter_group;
{
	int i, all_groups_read = TRUE;

	for (i = cur_groupnum ; i < group_top ; i++) {
		if (active[my_group[i]].unread != 0) {
			all_groups_read = FALSE;
			erase_group_arrow ();
			break;
		}
	}

	if (all_groups_read) {
		for (i = 0 ; i < cur_groupnum ; i++) {
			if (active[my_group[i]].unread != 0) {
				all_groups_read = FALSE;
				break;
			}
		}
	}
	
	if (all_groups_read) {
		info_message (txt_no_groups_to_read);
		return;
	}

	if (i != cur_groupnum) {
		erase_group_arrow ();
	}
	cur_groupnum = i;
	if (cur_groupnum < first_group_on_screen ||
	    cur_groupnum >= last_group_on_screen) {
		group_selection_page ();
	} else {
		draw_group_arrow ();
	}
	space_mode = pos_first_unread;

	if (enter_group) {
		clear_message ();
		index_point = -1;
		do {
			group_page (active[my_group[cur_groupnum]].name);
		} while (index_point == -3 || index_point == -4);
		group_selection_page ();
	}
}

/*
 *  Calculate max length of groupname field for group selection level.
 *  If all_group is TRUE check all groups in active file otherwise
 *  just subscribed to groups.
 */
 
void set_groupname_len (all_groups)
	int all_groups;
{
	int len;
	register int i;

	groupname_len = 0;
	
	if (all_groups) {
		for (i = 0 ; i < num_active ; i++) {
			len = strlen (active[i].name);
			if (len > groupname_len) {
				groupname_len = len;
			}
		}
	} else {
		for (i = 0 ; i < group_top ; i++) {
			len = strlen (active[my_group[i]].name);
			if (len > groupname_len) {
				groupname_len = len;
			}
		}
	}

	if (groupname_len >= (COLS - SELECT_MISC_COLS)) {
		groupname_len = COLS - SELECT_MISC_COLS - 1;
		if (groupname_len < 0) {
			groupname_len = 0;
		}	
	}
	
	/*
	 * If newsgroups descriptions are ON then cut off groupnames
	 * to specified max. length otherwise display full length
	 */
	if (show_description && groupname_len > groupname_max_length) {
		groupname_len = groupname_max_length;
	}
}


void toggle_my_groups (only_unread_groups, group)
	int only_unread_groups;
	char *group;
{
#ifndef INDEX_DAEMON
	char buf[8192];
	char old_curr_group[PATH_LEN];
	char *ptr;
	FILE *fp;
	int active_idx = 0;
	int group_num = cur_groupnum;
	register int i = 0, j = 0;

	if ((fp = fopen (newsrc, "r")) != (FILE *) 0) {
		/*
		 * Save current or next group with unread arts for later use
		 */
		old_curr_group[0] = '\0';

		if (group_top) {
			if (! only_unread_groups || reread_active_file) {
				if (strlen (group)) {
					if ((i = find_group_index (group)) >= 0) {
						active_idx = i;
					}
				} else {
					active_idx = my_group[group_num];
				}
				my_strncpy (old_curr_group, active[active_idx].name, 
					sizeof (old_curr_group));
			} else {
				for (i = group_num ; i < group_top ; i++) {
					if (active[my_group[i]].unread) {
						my_strncpy (old_curr_group, active[my_group[i]].name, 
							sizeof (old_curr_group));
						break;
					}
				}
			}
		}

		group_top = 0;
		while (fgets (buf, sizeof (buf), fp) != (char *) 0) {
			ptr = (char *) strchr (buf, ':');
			if (ptr != (char *) 0) {
				*ptr = '\0';
				if ((i = find_group_index (buf)) >= 0) {
					if (only_unread_groups) {					
						if (active[i].unread) {
							my_group[group_top] = i;
							group_top++;
						}
					} else {
						my_group[group_top] = i;
						group_top++;
					}	
				}
			}
		}		
		fclose (fp);

		/*
		 * Try and reposition on same or next group before toggling 
		 */
		cur_groupnum = 0;
		
		if ((i = find_group_index (old_curr_group)) >= 0) {
			for (j = 0 ; j < group_top ; j++) {
				if (my_group[j] == i) {
					cur_groupnum = j;
					break;
				}
			}
		}
	}

#endif	/* INDEX_DAEMON */
}


void	goto_next_group_on_screen ()
{
	if (_hp_glitch) {
		erase_group_arrow ();
	}

	if (cur_groupnum+1 < last_group_on_screen) {
		erase_group_arrow ();
		cur_groupnum++;
		draw_group_arrow ();
	} else {
		cur_groupnum++;
		group_selection_page ();
	}
}

/*
 * Strip trailing blanks
 */

void strip_line (line, len)
	char *line;
	int len;
{
	char *ptr = line + (len - 1);
	
	while (*ptr == ' ' || *ptr == '\r' || *ptr == '\n') {
		ptr--;
	}	
	*++ptr = '\0';
}

