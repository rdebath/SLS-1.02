/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : group.c
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

#define MARK_OFFSET	8

extern char cvers[LEN];
extern char proc_ch_type;	/* feed.c */
extern int last_resp;		/* page.c */
extern int this_resp;		/* page.c */
extern int note_page;		/* page.c */

char *glob_group;
int index_point;
int first_subj_on_screen;
int last_subj_on_screen;
static int len_from;
static int len_subj;
static char *spaces = "XXXX";

static int bld_sline ();
static int draw_sline ();

#ifndef ART_ADJUST	/* what we do here is bizarre */
#define ART_ADJUST(n)	(active[my_group[cur_groupnum]].attribute.show_only_unread \
				? ((n) > 1 ? (n) : 0) \
				: ((n) > 0 ? (n) - 1 : 0))
#endif

#define INDEX2SNUM(i)	((i) % NOTESLINES)
#define SNUM2LNUM(i)	(INDEX_TOP + (i))
#define INDEX2LNUM(i)	(SNUM2LNUM(INDEX2SNUM(i)))

void decr_tagged (tag)
	int tag;
{
	int i, j;

	for (i = 0; i < top_base; ++i) {
		for (j = (int) base[i] ; j != -1 ; j = arts[j].thread)
			if (arts[j].tagged > tag)
				--arts[j].tagged;
	}
}


void group_page (group)
	char *group;
{
#ifndef INDEX_DAEMON

	char group_path[LEN];
 	char buf[128];
 	char pat[128];
	int ch;
	int dummy = 0;
	int flag, i;
	int n = -1;
	int kill_state;
	int old_top = 0;
	int old_hot_arts = 0;
	int posted;
	int sav_groupnum;
	int scroll_lines;
	long old_artnum = 0L;
	int xflag = 0;
 	struct art_stat_t sbuf;

	/*
	 * Set the group attributes 
	 */
	active[my_group[cur_groupnum]].attribute.read_during_session = TRUE;
	show_author = active[my_group[cur_groupnum]].attribute.show_author;

 	proc_ch_default = get_post_proc_type (active[my_group[cur_groupnum]].attribute.post_proc_type);

	glob_group = group;
	sav_groupnum = cur_groupnum;
	num_of_tagged_arts = 0;
	
	make_group_path (group, group_path);

	last_resp = -1;
	this_resp = -1;
	
	/* 
	 * update index file. quit group level if user aborts indexing
	 */
	if (! index_group (group, group_path)) {
		return;
	}	
	
	if (space_mode) {
		for (i = 0; i < top_base; i++) {
			if (new_responses (i)) {
				break;
			}
		}
		if (i < top_base) {
			index_point = i;
		} else {
			index_point = top_base - 1;
		}
	} else {
		index_point = top_base - 1;
	}
	if (index_point < 0) {
		index_point = 0;
	}
	
	set_subj_from_size (COLS);
	clear_note_area ();
	show_group_page ();

	while (TRUE) {
		ch = ReadCh ();

		if (ch > '0' && ch <= '9') {	/* 0 goes to basenote */
			prompt_subject_num (ch, group);
			continue;
		} 
		switch (ch) {
			case ESC:	/* common arrow keys */ 	
#ifdef AMIGA
			case 0x9b:
#endif
				switch (get_arrow_key ()) {
				case KEYMAP_UP:
					goto group_up;

				case KEYMAP_DOWN:
					goto group_down;

				case KEYMAP_PAGE_UP:
					goto group_page_up;

				case KEYMAP_PAGE_DOWN:
					goto group_page_down;

				case KEYMAP_HOME:
					if (! top_base) {
						break;
					}			
					if (index_point != 0) {
						if (0 < first_subj_on_screen) {
#ifndef USE_CLEARSCREEN
							erase_subject_arrow ();
#endif					
							index_point = 0;
							show_group_page ();
						} else {
							erase_subject_arrow ();
							index_point = 0;
							draw_subject_arrow ();
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
				show_group_page ();
				break;
#endif

			case '$':	/* show last page of articles */
end_of_list:
				if (! top_base) {
					break;
				}			
				if (index_point != top_base - 1) {
					if (top_base - 1 > last_subj_on_screen) {
#ifndef USE_CLEARSCREEN
						erase_subject_arrow ();
#endif					
						index_point = top_base - 1;
						show_group_page ();
					} else {
						erase_subject_arrow ();
						index_point = top_base - 1;
						draw_subject_arrow ();
					}
				}
				break;
				
			case '-':	/* go to last viewed article */
				if (this_resp < 0) {
					info_message (txt_no_last_message);
					break;
				}
				index_point = show_page (this_resp, &dummy, group, group_path);
				if (index_point == -5) {
					index_point = which_thread (this_resp);
					clear_message ();
				} else {
					if (index_point < 0) {
						space_mode = (index_point == -4);
						goto group_done;
					}
					clear_note_area ();
					show_group_page ();
				}
				break;

			case '|':	/* pipe article/thread/tagged arts to command */
				if (index_point >= 0) {
					feed_articles (FEED_PIPE, GROUP_LEVEL, "Pipe",
						(int) base[index_point], group_path);
				}
				break;

			case '/':	/* forward/backward search */
			case '?':
				i = (ch == '/');
				search_subject (i, group);
				break;

			case '\r':
			case '\n':	/* read current basenote */
				if (index_point < 0) {
					info_message(txt_no_arts);
					break;
				}
				i = (int) base[index_point];
				index_point = show_page (i, &dummy, group, group_path);
				if (index_point == -5) {
					index_point = which_thread (i);
					clear_message ();
				} else {
					if (index_point < 0) {
						space_mode = (index_point == -4);
						goto group_done;
					}
					clear_note_area ();
					show_group_page ();
				}
				break;

			case '\t':	/* goto next unread article/group */
group_tab_pressed:			
 				space_mode = TRUE;
				if (index_point < 0) {
					n =  -1;
				} else {
					n = next_unread ((int) base[index_point]);
				}	
				if (index_point < 0 || n < 0) {
					for (i = cur_groupnum+1 ; i < group_top ; i++) {
						if (active[my_group[i]].unread > 0) {
							break;
						}
					}		
					if (i >= group_top) {
						goto group_done;
					}
					cur_groupnum = i;
					index_point = -3;
					goto group_done;
				}
				index_point = show_page (n, &dummy, group, group_path);
				if (index_point == -5) {
					index_point = which_thread (n);
					goto group_tab_pressed;	/* repeat TAB */
				} else {
					if (index_point < 0) {
						goto group_done;
					}	
					clear_note_area ();
					show_group_page ();
				}
				break;
	
			case ' ':		/* page down */
			case ctrl('D'):
			case ctrl('F'):		/* vi style */
group_page_down:
				if (! top_base) {
					break;
				}
				if (index_point == top_base - 1) {
					if (0 < first_subj_on_screen) {
#	ifndef USE_CLEARSCREEN
						erase_subject_arrow ();
#	endif					
						index_point = 0;
						show_group_page ();
					} else {
						erase_subject_arrow ();
						index_point = 0;
						draw_subject_arrow ();
					}
					break;
				}
				erase_subject_arrow ();
				scroll_lines = (full_page_scroll ? NOTESLINES : NOTESLINES / 2);
				index_point = ((index_point + scroll_lines) / scroll_lines) * scroll_lines;
				if (index_point >= top_base) {
					index_point = (top_base / scroll_lines) * scroll_lines;
					if (index_point < top_base - 1) {
						index_point = top_base - 1;
					}
				}
				if (index_point < first_subj_on_screen
				|| index_point >= last_subj_on_screen)
					show_group_page ();
				else
					draw_subject_arrow ();
				break;

			case ctrl('K'):		/* kill article */
 				if (index_point < 0) {
 					info_message (txt_no_arts);
					break;
				}
				old_top = top;
				n = (int) base[index_point];
				old_artnum = arts[n].artnum;
				if (kill_art_menu (group, (int) base[index_point])) {
					kill_any_articles (my_group[cur_groupnum]);
					make_threads (FALSE);
					find_base (my_group[cur_groupnum]);
					index_point = find_new_pos (old_top, old_artnum, index_point);
				}
				show_group_page ();
				break;

			case ctrl('L'):		/* redraw screen */
			case ctrl('R'):
			case ctrl('W'):
#ifndef USE_CLEARSCREEN
				ClearScreen ();
#endif
				show_group_page ();
				break;

			case ctrl('N'):
			case 'j':		/* line down */
group_down:
				if (! top_base) {
					break;
				}
				if (index_point + 1 >= top_base) {
					if (_hp_glitch) {
						erase_subject_arrow ();
					}
					if (0 < first_subj_on_screen) {
						index_point = 0;
						show_group_page ();
					} else {
						erase_subject_arrow ();
						index_point = 0;
						draw_subject_arrow ();
					}
					break;
				}
				if (index_point + 1 >= last_subj_on_screen) {
#ifndef USE_CLEARSCREEN
					erase_subject_arrow();
#endif					
					index_point++;
					show_group_page ();
				} else {
					erase_subject_arrow ();
					index_point++;
					draw_subject_arrow ();
				}
				break;

			case ctrl('P'):
			case 'k':		/* line up */
group_up:
				if (! top_base) {
					break;
				}	
				if (index_point == 0) {
					if (_hp_glitch) {
						erase_subject_arrow ();
					}
					if (top_base > last_subj_on_screen) {
						index_point = top_base - 1;
						show_group_page ();
					} else {
						erase_subject_arrow ();
						index_point = top_base - 1;
						draw_subject_arrow ();
					}
					break;
				}
				if (_hp_glitch) {
					erase_subject_arrow ();
				}
				if (index_point <= first_subj_on_screen) {
					index_point--;
					show_group_page ();
				} else {
					erase_subject_arrow ();
					index_point--;
					draw_subject_arrow ();
				}
				break;

			case 'b':		/* page up */
			case ctrl('U'):
			case ctrl('B'):		/* vi style */
group_page_up:
				if (! top_base) {
					break;
				}
				if (index_point == 0) {
					if (_hp_glitch) {
						erase_subject_arrow ();
					}
					if (top_base > last_subj_on_screen) {
						index_point = top_base - 1;
						show_group_page ();
					} else {
						erase_subject_arrow ();
						index_point = top_base - 1;
						draw_subject_arrow ();
					}
					break;
				}
#ifndef USE_CLEARSCREEN
				clear_message ();
#endif
				erase_subject_arrow ();
				scroll_lines = (full_page_scroll ? NOTESLINES : NOTESLINES / 2);
				if ((n = index_point % scroll_lines) > 0) {
					index_point = index_point - n;
				} else {
					index_point = ((index_point - scroll_lines) / scroll_lines) * scroll_lines;
				}
				if (index_point < 0) {
					index_point = 0;
				}
				if (index_point < first_subj_on_screen
				|| index_point >= last_subj_on_screen)
					show_group_page ();
				else
					draw_subject_arrow ();
				break;

			case 'a':	/* author search forward */
			case 'A':	/* author search backward */
				if (index_point < 0) {
					info_message (txt_no_arts);
					break;
				}

				i = (ch == 'a');

				n = search_author (my_group[cur_groupnum], (int) base[index_point], i);
				if (n < 0)
					break;

				index_point = show_page (n, &dummy, group, group_path);
				if (index_point != -5) {
					if (index_point < 0) {
						space_mode = FALSE;
						goto group_done;
					}
					clear_note_area ();
					show_group_page ();
				}
				break;

			case 'B':	/* bug/gripe/comment mailed to author */
				mail_bug_report ();
#ifndef USE_CLEARSCREEN
				ClearScreen ();
#endif
				show_group_page ();
				break;
				
			case 'c':	/* catchup - mark all articles as read */
			case 'C':	/* catchup - and goto next unread group */
			    if (! active[my_group[cur_groupnum]].unread || 
			        ! confirm_action || prompt_yn (LINES, txt_mark_all_read, 'y')) {
					for (n = 0; n < top; n++) {
						arts[n].unread = ART_READ;
					}
					if (ch == 'c') {
						if (cur_groupnum + 1 < group_top) {
							cur_groupnum++;
						}
						goto group_done;
					} else {
						goto group_tab_pressed;
					}	
			    }
			    break;

			case 'd':	/* toggle display of subject & subj/author */
				toggle_subject_from ();
				show_group_page ();
				break;

			case 'g':	/* choose a new group by name */
				n = choose_new_group ();
				if (n >= 0 && n != cur_groupnum) {
					cur_groupnum = n;
					index_point = -3;
					goto group_done;
				}
				break;

			case 'h':	/* help */
				show_info_page (HELP_INFO, help_group, txt_index_page_com);
				show_group_page ();
				break;

			case 'H':	/* toggle mini help menu */
				toggle_mini_help (GROUP_LEVEL);
				show_group_page();
				break;

			case 'I':	/* toggle inverse video */
				toggle_inverse_video ();
				show_group_page ();
				break;

			case 'K':	/* mark rest of thread as read */
				if (index_point < 0) {
					info_message (txt_no_next_unread_art);
					break;
				}
				old_hot_arts = num_of_hot_arts;
				for (i = (int) base[index_point]; i >= 0; i = arts[i].thread) {
					if (arts[i].unread != ART_READ) {
						arts[i].unread = ART_READ;
						if (arts[i].hot) {
							if (num_of_hot_arts) {
								num_of_hot_arts--;
							}
						}
					}
				}
				if (num_of_hot_arts != old_hot_arts) {
					show_group_title (TRUE);
				}
				bld_sline (index_point);
				draw_sline (index_point, FALSE);

				n = next_unread (next_response ((int) base[index_point]));
				if (n < 0) {
					draw_subject_arrow ();
					info_message (txt_no_next_unread_art);
					break;
				}

				if ((n = which_thread (n)) < 0) {
					error_message ("Internal error: K which_thread < 0", "");
					break;
				}

				if (n < first_subj_on_screen || n >= last_subj_on_screen) {
					index_point = n;
					show_group_page ();
				} else {
					erase_subject_arrow ();
					index_point = n;
					draw_subject_arrow ();
				}
				break;

			case 'l':	/* list articles within current thread */
				if (index_point < 0) {
					info_message (txt_no_arts);
					break;
				}
 				space_mode = TRUE;
				n = show_thread ((int) base[index_point], group, group_path);
				if (n == -2) {
					index_point = n;
					space_mode = FALSE;
					goto group_done;
				} else {
					if (index_point < 0) {
						space_mode = FALSE;
						goto group_done;
					}
					clear_note_area ();
					show_group_page ();
				}
				break;	

			case 'm':	/* mail article to somebody */
				if (index_point >= 0) {
					feed_articles (FEED_MAIL, GROUP_LEVEL, "Mail",
						(int) base[index_point], group_path);
				}
				break;

			case 'M':	/* options menu */
				if (top_base > 0) {
					old_top = top;
					n = (int) base[index_point];
					old_artnum = arts[n].artnum;
				}
				n = default_sort_art_type;
				kill_state = change_rcfile (group, TRUE);
				if (kill_state == NO_KILLING && n != default_sort_art_type) {
					make_threads (TRUE);
					find_base (my_group[cur_groupnum]);
				}
				set_subj_from_size (COLS);
				index_point = find_new_pos (old_top, old_artnum, index_point);
				show_group_page ();
			    break;

			case 'n':	/* goto next group */
				clear_message ();
				if (cur_groupnum + 1 >= group_top)
					info_message (txt_no_more_groups);
				else {
					cur_groupnum++;
					index_point = -3;
					space_mode = pos_first_unread;
					goto group_done;
				}
				break;

			case 'N':	/* goto next unread article */
				if (index_point < 0) {
					info_message(txt_no_next_unread_art);
					break;
				}

				n = next_unread ((int) base[index_point]);
				if (n == -1)
					info_message (txt_no_next_unread_art);
				else {
					index_point = show_page (n, &dummy, group, group_path);
					if (index_point != -5) {
						if (index_point < 0) {
							space_mode = pos_first_unread;
							goto group_done;
						}
						clear_note_area ();
						show_group_page ();
					}
				}
				break;

			case 'o':	/* output art/thread/tagged arts to printer */
				if (index_point >= 0) {
					feed_articles (FEED_PRINT, GROUP_LEVEL, "Print",
						(int) base[index_point], group_path);
				}
				break;

			case 'p':	/* previous group */
				clear_message();
				if (cur_groupnum <= 0)
					info_message(txt_no_prev_group);
				else {
					cur_groupnum--;
					index_point = -3;
					space_mode = pos_first_unread;
					goto group_done;
				}
				break;

			case 'P':	/* go to previous unread article */
				if (index_point < 0) {
				    info_message(txt_no_prev_unread_art);
				    break;
				}
				n = prev_response ((int) base[index_point]);
				n = prev_unread (n);
				if (n == -1)
				    info_message(txt_no_prev_unread_art);
				else {
					index_point = show_page (n, &dummy, group, group_path);
					if (index_point != -5) {
						if (index_point < 0) {
							space_mode = pos_first_unread;
							goto group_done;
						}
						clear_note_area ();
						show_group_page ();
					}
				}
				break;

			case 'q':	/* return to group selection page */
			case 'i':
				goto group_done;

			case 'Q':		/* quit */
				index_point = -2;
				space_mode = FALSE;
				goto group_done;

	 		case 'r':		
	 			/* 
	 			 * If in show_only_unread mode or there  are
	 			 * unread articles we know this thread  will
	 			 * exist after toggle. Otherwise we find the
	 			 * next closest 
	 			 */
 				if (active[my_group[cur_groupnum]].attribute.show_only_unread) {
					wait_message (txt_reading_all_arts);
 				} else {
					wait_message (txt_reading_new_arts);
 				} 
 				i = -1;
 				if (index_point >= 0) {
 					if (active[my_group[cur_groupnum]].attribute.show_only_unread || 
 					    new_responses (index_point)) {
 						i = base[index_point];
 					} else if ((n = prev_unread (base[index_point])) >= 0) {
 						i = n;
 					} else if ((n = next_unread (base[index_point])) >= 0) {
 						i = n;
 					}	
 				}
 				active[my_group[cur_groupnum]].attribute.show_only_unread = 
 					!active[my_group[cur_groupnum]].attribute.show_only_unread;
 				auto_select_articles (my_group[cur_groupnum]);
 				find_base (my_group[cur_groupnum]);
 				if (i >= 0 && (n = which_thread (i)) >= 0)
 					index_point = n;
 				else if (top_base > 0)
 					index_point = top_base - 1;
 				show_group_page ();
 				break;

			case 's':	/* save regex pattern to file/s */
				if (index_point >= 0) {
					feed_articles (FEED_SAVE, GROUP_LEVEL, "Save",
						(int) base[index_point], group_path);
				}
				break;
			
			case 't':	/* tag/untag art for mailing/piping/printing/saving */
				if (index_point >= 0) {
					int tagged = TRUE;
					n = (int) base[index_point];
					if (active[my_group[cur_groupnum]].attribute.thread_arts) {
						int i;
						for (i = n; i != -1 && tagged; i = arts[i].thread) {
							if (! arts[i].tagged)
								tagged = FALSE;
						}
						if (tagged) {
							/* 
							 * Here we repeat the tagged test in both blocks
							 * to leave the choice of tagged/untagged
							 * determination politic in the previous lines. 
							 */
							info_message (txt_untagged_thread);
							for (i = n; i != -1; i = arts[i].thread) {
								if (arts[i].tagged) {
									tagged = TRUE;
									decr_tagged (arts[i].tagged);
									arts[i].tagged = 0;
									--num_of_tagged_arts;
								}
							}
						} else {
							info_message (txt_tagged_thread);
							for (i = n; i != -1; i = arts[i].thread) {
								if (!arts[i].tagged)
									arts[i].tagged = ++num_of_tagged_arts;
							}
						}
					} else {
						if (tagged == arts[n].tagged) {
							decr_tagged (arts[n].tagged);
							--num_of_tagged_arts;
							arts[n].tagged = 0;
							info_message (txt_untagged_art);
						} else {
							arts[n].tagged = ++num_of_tagged_arts;
							info_message (txt_tagged_art);
						}
					}
					bld_sline (index_point);
					draw_sline (index_point, FALSE);
					if (tagged)
						show_group_page ();
					if (index_point + 1 < top_base)
						goto group_down;
					draw_subject_arrow ();
				}
				break;

			case 'u':	/* unthread/thread articles */
 				if (index_point >= 0) {
					active[my_group[cur_groupnum]].attribute.thread_arts = 
						!active[my_group[cur_groupnum]].attribute.thread_arts;
					make_threads (TRUE);
					find_base (my_group[cur_groupnum]);
					show_group_page ();
				}
				break;

			case 'U':	/* untag all articles */
 				if (index_point >= 0) {
					untag_all_articles ();
					update_group_page ();
				}
				break;

			case 'v':
				info_message (cvers);
				break;

			case 'w':	/* post an article */
				if (post_article (group, &posted)) {
					show_group_page ();
				}
				break;

			case 'W':	/* display messages posted by user */
				if (user_posted_messages ()) {
					show_group_page ();
				}
				break;

			case 'x':	/* crosspost current article */
				if (index_point >= 0) {
					feed_articles (FEED_XPOST, GROUP_LEVEL, "Crosspost",
						(int) base[index_point], group_path);
				}
				break;

			case 'z':	/* mark article as unread */
			case 'Z':	/* mark thread as unread */
 				if (index_point < 0) {
 					info_message (txt_no_arts);
					break;
				}
				n = 0;
				for (i = (int) base[index_point] ; i != -1 ; i = arts[i].thread) {
					if (arts[i].unread == ART_READ) {
						if (arts[i].hot && num_of_hot_arts) {
							num_of_hot_arts++;
						}
					}
					arts[i].unread = ART_UNREAD;
					++n;
					if (ch == 'z')
						break;
				}
				assert (n > 0);
				show_group_title (TRUE);
				bld_sline(index_point);
				draw_sline(index_point, FALSE);
				draw_subject_arrow();
				if (ch == 'z')
					info_message (txt_art_marked_as_unread);
				else
					info_message (txt_thread_marked_as_unread);
				break;

			case '*':	/* mark thread as selected */
			case '.':	/* toggle thread */
 				if (index_point < 0) {
 					info_message (txt_no_arts);
					break;
				}
				
				flag = 1;
				if (ch == '.') {
					stat_thread(index_point, &sbuf);
					if (sbuf.hot_unread == sbuf.unread)
						flag = 0;
				}
				n = 0;
				for (i = (int) base[index_point] ; i != -1 ; i = arts[i].thread) {
					arts[i].hot = flag;
					++n;
				}
				assert (n > 0);
				bld_sline(index_point);
				draw_sline(index_point, FALSE);
#if 0
				info_message ( flag
					      ? txt_thread_marked_as_selected
					      : txt_thread_marked_as_deselected);
#endif
				if (index_point + 1 < top_base)
					goto group_down;
				draw_subject_arrow ();
				break;

			case '@':	/* reverse selections */
				for (i=0; i<top; ++i)
					arts[i].hot = (arts[i].hot ? 0 : 1);
				update_group_page ();
				break;

  			case '~':	/* undo selections */
 				for (i=0; i<top; ++i) {
  					arts[i].hot = 0;
 					arts[i].zombie = 0;
 				}
 				xflag = 0;
  				update_group_page ();
  				break;
  
 			case '=':	/* select matching patterns */
 				sprintf (msg, txt_select_pattern, default_select_pattern);
 				if (! prompt_string (msg, buf)) {
 					break;
 				}	
 				if (strcmp (buf, "") == 0) {
 					if (default_select_pattern[0] == '\0') {
 						info_message ("No previous expression");
 						break;
 					}
 					strcpy (pat, default_select_pattern);
 				} else if (strcmp (buf, "*") == 0) { /* all */
 					strcpy (pat, buf);
 					strcpy (default_select_pattern, pat);
 				} else {
 					make_lower (buf, buf);
 					strcpy (default_select_pattern, buf);
 					sprintf (pat, "*%s*", default_select_pattern);
 				}
 
 				flag = 0;
 				for (n=0; n < top_base; ++n) {
 					char sub[LEN];
 					make_lower (arts[base[n]].subject, sub);
 					if (wildmat (sub, pat) == 0)
 						continue;
 					for (i = (int) base[n] ; i != -1 ; i = arts[i].thread) {
 						arts[i].hot = 1;
 					}
 					bld_sline(n);
 					++flag;
 				}
 				if (flag)
 					update_group_page ();
 				break;

			case ';':	/* make all unread hot if 1 is hot */
				for (n=0; n<top_base; ++n) {
					stat_thread(n, &sbuf);
					if (sbuf.hot_unread == 0
					    || sbuf.hot_unread == sbuf.unread)
						continue;
					for (i = (int) base[n] ; i != -1 ; i = arts[i].thread) {
						arts[i].hot = 1;
					}
				}
				/* no screen update needed */
				break;

			case 'X':	/* mark read all unselected arts */
				if (xflag)
					goto X_undo;

				for (i=0; i<top; ++i) {
					if (arts[i].unread != ART_UNREAD)
						continue;
					if (arts[i].hot != 1) {
						arts[i].unread = ART_READ;
						arts[i].zombie = 1;
					}
				}

				if (active[my_group[cur_groupnum]].attribute.show_only_unread) {
					find_base (my_group[cur_groupnum]);
				}

				xflag = 1;
				index_point = 0;
 				show_group_page ();
				break;

			case '+':	/* perform auto-selection on group */
				if (auto_select_articles (my_group[cur_groupnum])) {
					update_group_page ();
				}
				break;

X_undo:
				for (i=0; i<top; ++i) {
					if (arts[i].unread == ART_READ
					    && arts[i].zombie == 1) {
						arts[i].unread = ART_UNREAD;
						arts[i].zombie = 0;
					}
				}

				if (active[my_group[cur_groupnum]].attribute.show_only_unread) {
					find_base (my_group[cur_groupnum]);
				}

				xflag = 0;
				index_point = 0;	/* do we want this ? */
 				show_group_page ();

				break;
				


			default:
			    info_message (txt_bad_command);
		}
	}

group_done:
	fix_new_highest (sav_groupnum);
	update_newsrc (group, my_group[sav_groupnum], FALSE);

	if (index_point == -2) {
		write_rcfile ();
		tin_done (0);
	}	
	clear_note_area ();

#endif /* INDEX_DAEMON */
}


/*
 *  Correct highest[] for the group selection page display since
 *  new articles may have been read or marked unread
 */

void fix_new_highest (groupnum)
	int groupnum;
{
	register int i;
	int sum = 0;

	for (i = 0; i < top; i++) {
		if (arts[i].unread) {
			sum++;
		}
	}
	
	active[my_group[groupnum]].unread = sum;
}


void show_group_page ()
{
#ifndef INDEX_DAEMON

	int i;

	set_signals_group ();
	
#ifdef USE_CLEARSCREEN
	ClearScreen ();
#else
	MoveCursor (0, 0);
	CleartoEOLN ();
#endif

	show_group_title (FALSE);

#ifndef USE_CLEARSCREEN
	MoveCursor (1, 0);
	CleartoEOLN ();
#endif

	MoveCursor (INDEX_TOP, 0);

	if (index_point >= top_base) {
		index_point = top_base - 1;
	}

	if (NOTESLINES <= 0) {
		first_subj_on_screen = 0;
	} else {
		first_subj_on_screen = (index_point / NOTESLINES) * NOTESLINES;
		if (first_subj_on_screen < 0) {
			first_subj_on_screen = 0;
		}
	}

	last_subj_on_screen = first_subj_on_screen + NOTESLINES;

	if (last_subj_on_screen >= top_base) {
		last_subj_on_screen = top_base;
		first_subj_on_screen = (top_base / NOTESLINES) * NOTESLINES;

		if (first_subj_on_screen == last_subj_on_screen ||
			first_subj_on_screen < 0) {
			if (first_subj_on_screen < 0) {
				first_subj_on_screen = 0;
			} else {
				first_subj_on_screen = last_subj_on_screen - NOTESLINES;
			}
		}
	}

	if (top_base == 0) {
		first_subj_on_screen = 0;
		last_subj_on_screen = 0;
	}

	if (draw_arrow_mark) {
		CleartoEOS ();
	}

	for (i = first_subj_on_screen; i < last_subj_on_screen; ++i) {
		bld_sline(i);
		draw_sline(i, TRUE);
	}

#ifndef USE_CLEARSCREEN
	CleartoEOS ();
#endif

	show_mini_help (GROUP_LEVEL);

	if (top_base <= 0) {
		info_message(txt_no_arts);
		return;
	} else if (last_subj_on_screen == top_base) {
		info_message(txt_end_of_arts);
	}

	draw_subject_arrow();

#endif /* INDEX_DAEMON */
}


void update_group_page ()
{
#ifndef INDEX_DAEMON
	register int i;

	for (i = first_subj_on_screen; i < last_subj_on_screen; ++i) {
		bld_sline (i);
		draw_sline (i, FALSE);
	}

	if (top_base <= 0)
		return;

	draw_subject_arrow ();
#endif /* INDEX_DAEMON */
}


void draw_subject_arrow ()
{
	MoveCursor (INDEX2LNUM(index_point), 0);

	if (draw_arrow_mark) {
		fputs ("->", stdout);
		fflush (stdout);
	} else {
		StartInverse();
		draw_sline(index_point, TRUE);
		EndInverse();
	}
	MoveCursor (LINES, 0);
}

void erase_subject_arrow ()
{
	MoveCursor (INDEX2LNUM(index_point), 0);

	if (draw_arrow_mark) {
		fputs ("  ", stdout);
	} else {
		if (_hp_glitch) {
			EndInverse ();
		}
		draw_sline(index_point, TRUE);
	}
	fflush (stdout);
}


void prompt_subject_num (ch, group)
	int ch;
	char *group;
{
	int num;

	if (! top_base) {
		return;
	}

	clear_message ();

	if ((num = prompt_num (ch, txt_read_art)) == -1) {
		clear_message ();
		return;
	}
	num--;		/* index from 0 (internal) vs. 1 (user) */

	if (num < 0) {
		num = 0;
	}
	if (num >= top_base) {
		num = top_base - 1;
	}

	if (num >= first_subj_on_screen
	&&  num < last_subj_on_screen) {
		erase_subject_arrow ();
		index_point = num;
		draw_subject_arrow ();
	} else {
#ifndef USE_CLEARSCREEN
		erase_subject_arrow ();
#endif		
		index_point = num;
		show_group_page ();
	}
}


void clear_note_area ()
{
#ifndef USE_CLEARSCREEN
	MoveCursor (INDEX_TOP, 0);
	CleartoEOS ();
#endif
}

/*
 * Find new index position after a kill or unkill. Because
 * kill can work on author it is impossible to know which,
 * if any, articles will be left afterwards. So we make a
 * "best attempt" to find a new index point.
 */

int find_new_pos (old_top, old_artnum, cur_pos)
	int old_top;
	long old_artnum;
	int cur_pos;
{
	int pos;
	
 	if (top == old_top) {
 		return (cur_pos);
 	}	
  
 	if ((pos = valid_artnum (old_artnum)) >= 0) {
 		if ((pos = which_thread (pos)) >= 0){
 			return pos;
 		}
 	}		
 	
 	if (cur_pos < top_base) {
 		return cur_pos;
 	} else {
 		return (top_base - 1);
 	}	
}


void mark_screen (level, screen_row, screen_col, value)
	int level;
	int screen_row;
	int screen_col;
	char *value;
{
	int i, len;

	len = strlen (value);
	
	if (draw_arrow_mark) {
		MoveCursor(INDEX_TOP + screen_row, screen_col);
		fputs (value, stdout);
		MoveCursor (LINES, 0);
		fflush (stdout);
	} else {
		for (i=0 ; i < len ; i++) {
			screen[screen_row].col[screen_col+i] = value[i];
		}
		if (level == SELECT_LEVEL) {
			draw_group_arrow();
		} else {
			draw_subject_arrow();
		}	
	}
}


void set_subj_from_size (num_cols)
	int num_cols;
{
	int i, size = 0;
	
	i = my_group[cur_groupnum];
	
	if (show_author == SHOW_FROM_BOTH) {
		max_subj = (num_cols / 2) - 2;
	} else {
		max_subj = (num_cols / 2) + 5;
	}
	max_from = (num_cols - max_subj) - 17;

	if (show_author != SHOW_FROM_BOTH) {
		if (max_from > 25) {
			size = max_from - 25;
			max_from = 25;
			max_subj = max_subj + size;
		}
	}

	if (show_author != SHOW_FROM_NONE) {
		len_from = max_from - BLANK_GROUP_COLS;
		len_subj = max_subj;
		spaces = "   ";
	} else {
		len_from = 0;
		len_subj = (max_subj+max_from+3) - BLANK_GROUP_COLS;
		spaces = "";
	}
}


void toggle_subject_from ()
{
	int i;
	int tmp;
	
	i = my_group[cur_groupnum];

	tmp = show_author;
	
	if (active[i].attribute.show_author != SHOW_FROM_NONE) {
		if (show_author != SHOW_FROM_NONE) {
			show_author = SHOW_FROM_NONE;
		} else {
			show_author = active[i].attribute.show_author;
		}
	} else {
		if (show_author + 1 > SHOW_FROM_BOTH) {
			show_author = SHOW_FROM_NONE;
		} else {
			show_author++;
		}
	}

	set_subj_from_size (COLS);
}

/*
 * Build subject line given an index into base[]. 
 *
 * WARNING: the routine is tightly coupled with draw_sline() in the sense
 * that draw_sline() expects bld_sline() to place the article mark
 * (read_art_makr, hot_art_mark, etc) at MARK_OFFSET in the screen[].col.
 * So, if you change the format used in this routine, be sure to check
 * that the value of MARK_OFFSET is still correct. 
 * Yes, this is somewhat kludgy.
 */
 
static int bld_sline (i)
	int i;
{
#ifndef INDEX_DAEMON
	int respnum;
	int n, j;
	char from[LEN];
	char new_resps[8];
	char art_cnt[8];
 	struct art_stat_t sbuf;

	from[0] = '\0';
	respnum = (int) base[i];
	
	stat_thread(i, &sbuf);
	if (active[my_group[cur_groupnum]].attribute.show_only_unread)
		n = sbuf.unread + sbuf.seen;
	else
		n = sbuf.total;
	
	n = ART_ADJUST(n);
	
	if (arts[respnum].tagged) {
		sprintf (new_resps, "%3d", arts[respnum].tagged);
	} else {
		sprintf (new_resps, "  %c", sbuf.art_mark);
	}

	if (n) {
		sprintf (art_cnt, "%-3d", n); 
	} else {
		strcpy (art_cnt, "   ");
	}
	
	if (show_author != SHOW_FROM_NONE) {
		get_author (FALSE, respnum, from);
	}	
	
	j = INDEX2SNUM(i);
	sprintf (screen[j].col, "  %4d%3s %s%-*.*s%s%-*.*s",
		 i+1, new_resps, art_cnt, len_subj, len_subj, 
		 arts[respnum].subject, spaces, len_from, len_from, from);
	
#endif /* INDEX_DAEMON */
	return(0);
}

/*
 * Draw subject line given an index into base[].
 *
 * WARNING: this routine is tightly coupled with bld_sline(); see the warning
 * associated with that routine for details. (C++ would be handy here.)
 *
 * NOTE: the 2nd argument is used to control whether the full line is 
 * redrawn or just the the parts of it that can be changed by a
 * command; i.e., the unread art count and the art mark. This will result
 * in a slightly more efficient update, though at the price of increased 
 * code complexity and readability.
 */

static int draw_sline (i, full)
	int i;
	int full;	/* unused at moment */
{
#ifndef INDEX_DAEMON
	int j, tlen, x;
	int k = MARK_OFFSET;
	char *s;

	j = INDEX2SNUM(i);

	if (full) {	
		s = screen[j].col;
		tlen = strlen (s);
		x = 0;
		if (slow_speed_terminal) {		
			strip_line (s, tlen);
			CleartoEOLN ();
		}
	} else {
		tlen  = 7;
		s = &screen[j].col[6];
		x = 6;
	}

	MoveCursor (INDEX2LNUM(i), x);
	fwrite (s, 1, tlen, stdout);

	/* it is somewhat less efficient to go back and redo that art mark
	 * if hot, but it is quite readable as to what is happening 
	 */
	if (screen[j].col[k] == hot_art_mark) {
		MoveCursor (INDEX2LNUM(i), k);
		ToggleInverse ();
		fputc (screen[j].col[k], stdout);
		ToggleInverse ();
	}

	MoveCursor(INDEX2LNUM(i)+1, 0);
	
#endif /* INDEX_DAEMON */
	return(0);
}


void show_group_title (clear_title)
	int clear_title;
{
#ifndef INDEX_DAEMON

	char buf[PATH_LEN];
	int num;
	register int i, art_cnt = 0;
	
	num = my_group[cur_groupnum];
	
	if (active[num].attribute.show_only_unread) {
		for (i = 0 ; i < top_base ; i++) {
			art_cnt += new_responses (i);
		}	
	} else {
		for (i = 0 ; i < top ; i++) {
			if (! IGNORE_ART(i)) {
				++art_cnt;
			}
		}		
	}

	if (active[num].attribute.thread_arts && default_thread_arts) {
		sprintf (buf, "%s (%dT %dA %dK %dH%s)", 
			active[num].name, top_base, 
			art_cnt, num_of_killed_arts, num_of_hot_arts,
			(active[num].attribute.show_only_unread ? " R" : ""));
	} else {
		sprintf (buf, "%s (%dU %dK %dH%s)", 
			active[num].name,
			art_cnt, num_of_killed_arts, num_of_hot_arts,
			(active[num].attribute.show_only_unread ? " R" : ""));
	}

	if (clear_title) {
		MoveCursor (0, 0);
		CleartoEOLN ();
	}
	
	show_title (buf);

#endif /* INDEX_DAEMON */
}
