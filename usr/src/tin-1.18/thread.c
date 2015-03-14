/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : thread.c
 *  Author    : I.Lea
 *  Created   : 01-04-91
 *  Updated   : 23-11-92
 *  Notes     :
 *  Copyright : (c) Copyright 1991-92 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

extern int index_point;
int threaded_on_subject;
static int top_thread = 0;
static int thread_index_point = 0;
static int thread_basenote = 0;
static int thread_respnum = 0;
static int first_thread_on_screen = 0;
static int last_thread_on_screen = 0;

#define INDEX2TNUM(i)	((i) % NOTESLINES)
#define TNUM2LNUM(i)	(INDEX_TOP + (i))
#define INDEX2LNUM(i)	(TNUM2LNUM(INDEX2TNUM(i)))

#define MARK_OFFSET 8


static int bld_tline (l, i)
	int l;
	int i;
{
#ifndef INDEX_DAEMON
	char mark;
	char new_resps[8];
	char from[LEN];
	char *spaces = "XXX";
	int j;
	int len_from;
	int len_subj = 0;
	int off_subj = 0;
	int off_both = 0;

	if (! draw_arrow_mark) {
		off_subj = 2;
		off_both = 5;
	}	

	if (threaded_on_subject) {
		len_from = max_subj+max_from+off_both;
		spaces = "";
	} else {
		if (show_author != SHOW_FROM_NONE) {
			len_from = max_from;
			len_subj = max_subj+off_subj;
			spaces = "   ";
		} else {
			len_from = 0;
			len_subj = max_from+max_subj+off_subj;
			spaces = "";
		}
	}	

	j = INDEX2TNUM(l);

	if (arts[i].tagged) {
		sprintf (new_resps, "%3d", arts[i].tagged);
	} else {
		if (arts[i].unread == ART_UNREAD) {
			mark = (arts[i].hot ? hot_art_mark : unread_art_mark);
		} else if (arts[i].unread == ART_WILL_RETURN) {
			mark =  return_art_mark;
		} else {
			mark = READ_ART_MARK;
		}
		sprintf (new_resps, "  %c", mark);
	}
	
	from[0] = '\0';
	if (threaded_on_subject || show_author != SHOW_FROM_NONE) {
		get_author (TRUE, i, from);
	}	

	sprintf (screen[j].col, "  %4d%3s  %-*.*s%s%-*.*s",
		 l, new_resps, len_subj, len_subj, arts[i].subject, 
		 spaces, len_from, len_from, from);

#endif
	return(0);
}


static int draw_tline (i, full)
	int i;
	int full;
{
#ifndef INDEX_DAEMON
	int j, tlen, x;
	int k = MARK_OFFSET;
	char *s;

	j = INDEX2TNUM(i);

	if (full) {
		s = screen[j].col;
		tlen = strlen (s);
		x = 0;
		if (slow_speed_terminal) {		
			strip_line (s, tlen);
			CleartoEOLN ();
		}
	} else {
		tlen  = 3;
		s = &screen[j].col[6];
		x = 6;
	}

	MoveCursor(INDEX2LNUM(i), x);
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
#endif
	return(0);
}
/*
 * show current thread. If threaded on Subject: show
 *   <respnum> <name>    <respnum> <name>
 * If threaded on Archive-name: show
 *   <respnum> <subject> <name>
 */
 
int show_thread (respnum, group, group_path)
	int respnum;
	char *group;
	char *group_path;
{
	int ret_code = TRUE;
#ifndef INDEX_DAEMON
	int ch;
	int i, index, n;
	int scroll_lines;
	int flag;

	thread_respnum = respnum;
	thread_basenote = which_thread (thread_respnum);
	top_thread = num_of_responses (thread_basenote) + 1;

	if (top_thread <= 0) {
		info_message (txt_no_resps_in_thread);
		return FALSE;
	}

	if (arts[thread_respnum].archive != (char *) 0) {
		threaded_on_subject = FALSE;
	} else {
		threaded_on_subject = TRUE;
	}

	thread_index_point = top_thread;
	if (space_mode) {
		i = new_responses (thread_basenote);
		if (i) {
			for (n=0, i = base[thread_basenote]; i >= 0 ; i = arts[i].thread, n++) {
				if (arts[i].unread == ART_UNREAD) {
					if (arts[i].thread == ART_EXPIRED) {
						arts[i].unread == ART_READ;
					} else {
						thread_index_point = n;
					}
					break;
				}
			}
		}
	}

	if (thread_index_point < 0) {
		thread_index_point = 0;
	}

	show_thread_page ();

	while (TRUE) {
		ch = ReadCh ();

		if (ch >= '0' && ch <= '9') {	/* 0 goes to basenote */
			if (top_thread == 1) {
				info_message (txt_no_responses);
			} else {
				prompt_thread_num (ch);
			}
			continue;
		}
		switch (ch) {
			case ESC:	/* common arrow keys */
#ifdef AMIGA
			case 0x9b:
#endif
				switch (get_arrow_key ()) {
					case KEYMAP_UP:
						goto thread_up;

					case KEYMAP_DOWN:
						goto thread_down;

					case KEYMAP_PAGE_UP:
						goto thread_page_up;

					case KEYMAP_PAGE_DOWN:
						goto thread_page_down;

					case KEYMAP_HOME:
						if (thread_index_point != 0) {
							if (0 < first_thread_on_screen) {
#ifndef USE_CLEARSCREEN
								erase_thread_arrow ();
#endif
								thread_index_point = 0;
								show_thread_page ();
							} else {
								erase_thread_arrow ();
								thread_index_point = 0;
								draw_thread_arrow ();
							}
						}
						break;
					
					case KEYMAP_END:
						goto end_of_thread;
				}
				break;

			case '$':	/* show last page of threads */
end_of_thread:			
				if (thread_index_point < top_thread - 1) {
					if (top_thread > last_thread_on_screen) {
#ifndef USE_CLEARSCREEN
						erase_thread_arrow ();
#endif					
						thread_index_point = top_thread - 1;
						show_thread_page ();
					} else {
						erase_thread_arrow ();
						thread_index_point = top_thread - 1;
						draw_thread_arrow ();
					}
				}
				break;
				
			case '\r':
			case '\n':	/* read current article within thread */
				n = choose_response (thread_basenote, thread_index_point);
				n = show_page (n, &thread_index_point, group, group_path);
				if (n != -5) {
					if (n == thread_basenote) {
						show_thread_page ();
					} else {
						index_point = n;	
						goto thread_done;
					}
				}
				break;

			case '\t':
thread_tab_pressed:
 				space_mode = TRUE;
				if (thread_index_point == 0) {
					n = thread_respnum;
				} else {
					n = choose_response (thread_basenote, thread_index_point);
				}
				index = thread_index_point;
				for (i = n ; i != -1 ; i = arts[i].thread) {
					if (arts[i].unread == ART_UNREAD) {
						n = show_page (i, &thread_index_point, group, group_path);
						break;
					}
					index++;
				}
				if (n == -5) {
					goto thread_tab_pressed;
				} else {
					if (n == thread_basenote) {
						show_thread_page ();
					} else {
						index_point = n;	
						goto thread_done;
					}
				}
				break;
	
			case ' ':		/* page down */
			case ctrl('D'):
			case ctrl('F'):		/* vi style */
thread_page_down:
				if (thread_index_point + 1 == top_thread) {
					if (0 < first_thread_on_screen) {
#	ifndef USE_CLEARSCREEN
						erase_thread_arrow ();
#	endif					
						thread_index_point = 0;
						show_thread_page ();
					} else {
						erase_thread_arrow ();
						thread_index_point = 0;
						draw_thread_arrow ();
					}
					break;
				}
				erase_thread_arrow ();
				scroll_lines = (full_page_scroll ? NOTESLINES : NOTESLINES / 2);
				thread_index_point = ((thread_index_point + scroll_lines) /
							scroll_lines) * scroll_lines;
				if (thread_index_point >= top_thread) {
					thread_index_point = (top_thread / scroll_lines) * scroll_lines;
					if (thread_index_point < top_thread - 1) {
						thread_index_point = top_thread - 1;
					}
				}
				if (thread_index_point < first_thread_on_screen ||
					thread_index_point >= last_thread_on_screen) {
					show_thread_page ();
				} else {
					draw_thread_arrow ();
				}
				break;

			case ctrl('L'):		/* redraw screen */
			case ctrl('R'):
			case ctrl('W'):
#ifndef USE_CLEARSCREEN
				ClearScreen ();
#endif
				show_thread_page ();
				break;

			case ctrl('N'):
			case 'j':		/* line down */
thread_down:
				if (thread_index_point + 1 >= top_thread) {
					if (_hp_glitch) {
						erase_thread_arrow ();
					}
					if (0 < first_thread_on_screen) {
						thread_index_point = 0;
						show_thread_page ();
					} else {
						erase_thread_arrow ();
						thread_index_point = 0;
						draw_thread_arrow ();
					}
					break;
				}
				if (thread_index_point + 1 >= last_thread_on_screen) {
#ifndef USE_CLEARSCREEN
					erase_thread_arrow ();
#endif					
					thread_index_point++;
					show_thread_page ();
				} else {
					erase_thread_arrow ();
					thread_index_point++;
					draw_thread_arrow ();
				}
				break;

			case ctrl('P'):
			case 'k':		/* line up */
thread_up:
				if (thread_index_point == 0) {
					if (_hp_glitch) {
						erase_thread_arrow ();
					}
					if (top_thread > last_thread_on_screen) {
						thread_index_point = top_thread - 1;
						show_thread_page ();
					} else {
						erase_thread_arrow ();
						thread_index_point = top_thread - 1;
						draw_thread_arrow ();
					}
					break;
				}
				if (_hp_glitch) {
					erase_thread_arrow ();
				}
				if (thread_index_point <= first_thread_on_screen) {
					thread_index_point--;
					show_thread_page ();
				} else {
					erase_thread_arrow ();
					thread_index_point--;
					draw_thread_arrow ();
				}
				break;

			case 'b':		/* page up */
			case ctrl('U'):
			case ctrl('B'):		/* vi style */
thread_page_up:
				if (thread_index_point == 0) {
					if (_hp_glitch) {
						erase_thread_arrow ();
					}
					if (top_thread > last_thread_on_screen) {
						thread_index_point = top_thread - 1;
						show_thread_page ();
					} else {
						erase_thread_arrow ();
						thread_index_point = top_thread - 1;
						draw_thread_arrow ();
					}
					break;
				}
#ifndef USE_CLEARSCREEN
				clear_message ();
#endif
				erase_thread_arrow ();
				scroll_lines = (full_page_scroll ? NOTESLINES : NOTESLINES / 2);
				if ((n = thread_index_point % scroll_lines) > 0) {
					thread_index_point = thread_index_point - n;
				} else {
					thread_index_point = ((thread_index_point - scroll_lines) / scroll_lines) * scroll_lines;
				}
				if (thread_index_point < 0) {
					thread_index_point = 0;
				}
				if (thread_index_point < first_thread_on_screen
				|| thread_index_point >= last_thread_on_screen)
					show_thread_page ();
				else
					draw_thread_arrow ();
				break;

			case 'B':	/* bug/gripe/comment mailed to author */
				mail_bug_report ();
#ifndef USE_CLEARSCREEN
				ClearScreen ();
#endif
				show_thread_page ();
				break;

			case 'c':	/* catchup thread but ask for confirmation */
			case 'K':	/* mark thread as read immediately */
				if (ch == 'c') {
					if (confirm_action && !prompt_yn (LINES, txt_mark_thread_read, 'y')) {
						break;
					}
				}
				for (i = (int) base[thread_basenote] ; i != -1 ; i = arts[i].thread) {
					arts[i].unread = ART_READ;
				}
				goto thread_done;

			case 'd':	/* toggle display of subject & subj/author */
				if (! threaded_on_subject) {
					toggle_subject_from ();
					show_thread_page ();
				}	
				break;
				
			case 'h':	/* help */
				show_info_page (HELP_INFO, help_thread, txt_thread_com);
				show_thread_page ();
				break;

			case 'H':	/* toggle mini help menu */
				toggle_mini_help (THREAD_LEVEL);
				show_thread_page();
				break;

			case 'I':	/* toggle inverse video */
				toggle_inverse_video ();
				show_thread_page ();
				break;

			case 'q':	/* return to previous level */
				goto thread_done;

			case 'Q':	/* quit */
				ret_code = -2;
				goto thread_done;

 			case 'T':	/* tag/untag art for mailing/piping/printing/saving */
				n = choose_response (thread_basenote, thread_index_point);

 				if (n < 0)
 					break;
 
 				if (arts[n].tagged) {
 					arts[n].tagged = 0;
 					info_message (txt_untagged_art);
 				} else {
 					arts[n].tagged = ++num_of_tagged_arts;
					info_message (txt_tagged_art);
 				}
				bld_tline (thread_index_point, n);
				draw_tline (thread_index_point, FALSE);
				if (thread_index_point + 1 < top_thread)
					goto thread_down;
				draw_thread_arrow ();
				break;

			case 'v':	/* version */
				info_message (cvers);
				break;

			case 'z':	/* mark article as unread */
			case 'Z':	/* mark thread as unread */
				n = choose_response (thread_basenote, thread_index_point);
				if (ch == 'z') {
					if (arts[n].unread == ART_READ) {
						if (arts[n].hot) {
							num_of_hot_arts++;
						}
					}
					arts[n].unread = ART_UNREAD;
				} else {
					for (i = (int) base[thread_basenote] ; i != -1; i = arts[i].thread) {
						if (arts[n].unread == ART_READ) {
							if (arts[n].hot) {
								num_of_hot_arts++;
							}
						}
						arts[i].unread = ART_UNREAD;
					}
				}
				bld_tline (thread_index_point, n);
				draw_tline (thread_index_point, FALSE);
				info_message (ch == 'z' 
					      ? txt_art_marked_as_unread
					      : txt_thread_marked_as_unread);
				draw_thread_arrow ();
				break;
				
			case '*':	/* mark article as selected */
			case '.':	/* toggle article as selected */
				n = choose_response (thread_basenote, thread_index_point);

				if (n < 0)
					break;
				if (ch == '.' && arts[n].hot == 1)
					flag = 0;
				else
					flag = 1;
				arts[n].hot = flag;
/*				update_thread_page (); */
				bld_tline (thread_index_point, n);
				draw_tline (thread_index_point, FALSE);
				if (thread_index_point + 1 < top_thread)
					goto thread_down;
				draw_thread_arrow ();
#if 0
				info_message (flag 
					      ? txt_art_marked_as_selected
					      : txt_art_marked_as_deselected)
#endif
				break;

			case '@':	/* reverse selections */
				for (i = (int) base[thread_basenote] ; i != -1 ; i = arts[i].thread) {
					arts[i].hot = (arts[i].hot ? 0 : 1);
				}
				update_thread_page ();
				break;

			case '~':	/* undo selections */
				for (i = (int) base[thread_basenote] ; i != -1 ; i = arts[i].thread) {
					arts[i].hot = 0;
				}
				update_thread_page ();
				break;

			default:
			    info_message (txt_bad_command);
		}
	}

thread_done:
	clear_note_area ();

#endif /* INDEX_DAEMON */

	return (ret_code);
}


void show_thread_page ()
{
#ifndef INDEX_DAEMON

	extern int index_point;
	int i, j;
	static int index = 0;

	set_signals_thread ();
	
	ClearScreen ();

	if (threaded_on_subject) {
		sprintf (msg, "Thread (%.*s)", COLS-23, arts[thread_respnum].subject);
	} else {
		sprintf (msg, "List Thread (%d of %d)", index_point+1, top_base);
	}
	show_title (msg);

	MoveCursor (INDEX_TOP, 0);
	if (thread_index_point > top_thread - 1) {
		thread_index_point = top_thread - 1;
	}

	if (NOTESLINES <= 0) {
		first_thread_on_screen = 0;
	} else {
		first_thread_on_screen = (thread_index_point / NOTESLINES) * NOTESLINES;
		if (first_thread_on_screen < 0) {
			first_thread_on_screen = 0;
		}
	}

	last_thread_on_screen = first_thread_on_screen + NOTESLINES;

	if (last_thread_on_screen >= top_thread) {
		last_thread_on_screen = top_thread;
		first_thread_on_screen = (top_thread / NOTESLINES) * NOTESLINES;

		if (first_thread_on_screen == last_thread_on_screen ||
			first_thread_on_screen < 0) {
			if (first_thread_on_screen < 0) {
				first_thread_on_screen = 0;
			} else {
				first_thread_on_screen = last_thread_on_screen - NOTESLINES;
			}
		}
	}

	if (top_thread == 0) {
		first_thread_on_screen = 0;
		last_thread_on_screen = 0;
	}

	index = choose_response (thread_basenote, first_thread_on_screen);
	assert(first_thread_on_screen != 0 || index == thread_respnum);

	for (j=0, i = first_thread_on_screen; j < NOTESLINES && i < last_thread_on_screen; i++, j++) {
		bld_tline (i, index);
		draw_tline (i, TRUE);
		if ((index = next_response (index)) == -1) {
			break;
		}	
	}

#ifndef USE_CLEARSCREEN
	CleartoEOS ();
#endif

	show_mini_help (THREAD_LEVEL);

	if (last_thread_on_screen == top_thread) {
		info_message (txt_end_of_thread);
	}

	draw_thread_arrow ();

#endif /* INDEX_DAEMON */
}


void update_thread_page()
{
#ifndef INDEX_DAEMON
	register int i, j, index;

	index = choose_response (thread_basenote, first_thread_on_screen);
	assert(first_thread_on_screen != 0 || index == thread_respnum);

	for (j=0, i = first_thread_on_screen; j < NOTESLINES && i < last_thread_on_screen; ++i, ++j) {
		bld_tline (i, index);
		draw_tline (i, FALSE);
		if ((index = next_response (index)) == -1) {
			break;
		}	
	}

	draw_thread_arrow();
#endif /* INDEX_DAEMON */
}


void draw_thread_arrow ()
{
	MoveCursor (INDEX2LNUM(thread_index_point), 0);

	if (draw_arrow_mark) {
		fputs ("->", stdout);
		fflush (stdout);
	} else {
		StartInverse ();
		draw_tline (thread_index_point, TRUE);
		EndInverse ();
	}
	MoveCursor (LINES, 0);
}


void erase_thread_arrow ()
{
	MoveCursor (INDEX2LNUM(thread_index_point), 0);

	if (draw_arrow_mark) {
		fputs ("  ", stdout);
	} else {
		if (_hp_glitch) {
			EndInverse ();
		}
		draw_tline (thread_index_point, TRUE);
	}
	fflush (stdout);
}


int prompt_thread_num (ch)
	int ch;
{
	int num;

	clear_message ();

	if ((num = prompt_num (ch, txt_read_art)) == -1) {
		clear_message ();
		return FALSE;
	}

	if (num >= top_thread)
		num = top_thread - 1;

	if (num >= first_thread_on_screen
	&&  num < last_thread_on_screen) {
		erase_thread_arrow ();
		thread_index_point = num;
		draw_thread_arrow ();
	} else {
#ifndef USE_CLEARSCREEN
		erase_thread_arrow ();
#endif		
		thread_index_point = num;
		show_thread_page ();
	}
	return TRUE;
}

/*
 *  Return the number of unread articles there are within a thread
 */

int new_responses (thread)
	int thread;
{
	int i;
	int sum = 0;

	for (i = (int) base[thread]; i >= 0; i = arts[i].thread) {
		if (arts[i].unread != ART_READ) {
			sum++;
		}
	}
	
	return sum;
}

/*
 *  Which base note (an index into base[]) does a respnum
 *  (an index into arts[]) corresponsd to?
 *
 *  In other words, base[] points to an entry in arts[] which is
 *  the head of a thread, linked with arts[].thread.  For any q: arts[q],
 *  find i such that base[i]->arts[n]->arts[o]->...->arts[q]
 *
 *  Note that which_thread() can return -1 if in show_read_only mode and
 *  the article of interest has been read as well as all other articles in
 *  the thread,  thus resulting in no base[] entry for it.
 */

int which_thread (n)
	int n;
{
	register int i, j;

	for (i = 0; i < top_base; i++) {
		for (j = (int) base[i] ; j >= 0 ; j = arts[j].thread) {
			if (j == n) {
				return i;
			}
		}
	}

	sprintf (msg, "%d", n);
	error_message (txt_cannot_find_base_art, msg);
	return -1;
}

/*
 *  Find how deep in a thread a response is.  Start counting at zero
 */

int which_response (n)
	int n;
{
	int i, j;
	int num = 0;

	i = which_thread (n);
	assert(i >= 0);

	for (j = (int) base[i]; j != -1; j = arts[j].thread)
		if		if (arts[i].killed) {
			++sbuf->killed;
		}
#endif
	}


	if (sbuf->hot_unread)
		sbuf->art_mark = hot_art_mark;
	else if (sbuf->unread)
		sbuf->art_mark = unread_art_mark;
	else if (sbuf->seen)
		sbuf->art_mark = return_art_mark;
	else
		sbuf->art_mark = READ_ART_MARK;

	return(sbuf->total);
}


/*
 *  Find the next response.  Go to the next basenote if there
 *  are no more responses in this thread
 */

int next_response (n)
	int n;
{
	int i;

	if (arts[n].thread >= 0) {
		return arts[n].thread;
	}
	
	i = which_thread (n) + 1;

	if (i >= top_base) {
		return -1;
	}
	
	return (int) base[i];
}

/*
 *  Given a respnum (index into arts[]), find the respnum of the
 *  next basenote
 */

int next_thread (n)
	int n;
{
	int i;

	i = which_thread (n) + 1;
	if (i >= top_base)
		return -1;

	return (int) base[i];
}

/*
 *  Find the previous response.  Go to the last response in the previous
 *  thread if we go past the beginning of this thread.
 */

int prev_response (n)
	int n;
{
	int resp;
	int i;

	resp = which_response (n);

	if (resp > 0)
		return choose_response (which_thread (n), resp-1);

	i = which_thread (n) - 1;

	if (i < 0)
		return -1;

	return choose_response (i, num_of_responses (i));
}

/*
 *  return response number n from thread i
 */

int choose_response (i, n)
	int i;
	int n;
{
	int j;

	j = (int) base[i];

	while (n-- > 0 && arts[j].thread >= 0) {
		j = arts[j].thread;
	}

	return j;
}

/*
 *  Find the next unread response in this group. If no response is found
 *  from current point to the end restart from beginning of articles.
 */

int next_unread (n)
	int n;
{
	int cur_base_art = n;
	
	while (n >= 0) {
		if ((arts[n].unread == ART_UNREAD ||
		     arts[n].unread == ART_WILL_RETURN) &&
		     arts[n].thread != ART_EXPIRED) {
			return n;
		}
		n = next_response (n);
	}

	n = base[0];
	while (n != cur_base_art) {
		if ((arts[n].unread == ART_UNREAD ||
		     arts[n].unread == ART_WILL_RETURN) &&
		     arts[n].thread != ART_EXPIRED) {
			return n;
		}
		n = next_response (n);
	}
	
	return -1;
}

/*
 *  Find the previous unread response in this thread
 */

int prev_unread (n)
	int n;
{
	while (n >= 0) {
		if (arts[n].unread == ART_UNREAD && arts[n].thread != ART_EXPIRED) {
			return n;
		}
		n = prev_response (n);
	}

	return -1;
}
return n;
		}
		n = next_response (n);
	}

	n = base[0];
	while (n != cur_base_art) {
		if ((arts[n].unread == ART_UNREAD ||
		     arts[n].unread == ART_WILL_RETURN) &&
		     arts[n].thread != ART_EXPIRED) {
			return n;
		}
		n = next_response (n);
	}
	
	return -1;
}

/*tin-1.18/wildmat.c                                                                                     644     144      12        11322  5310411634   7101                                                                                                                                                                                                                                                                                                                                                                      /*  $Revision: 1.9 $
**
**  Do shell-style pattern matching for ?, \, [], and * characters.
**  Might not be robust in face of malformed patterns; e.g., "foo[a-"
**  could cause a segmentation violation.  It is 8bit clean.
**
**  Written 