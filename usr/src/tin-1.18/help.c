/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : help.c
 *  Author    : I.Lea
 *  Created   : 01-04-91
 *  Updated   : 06-12-92
 *  Notes     :
 *  Copyright : (c) Copyright 1991-92 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

char *help_select[] = {
	txt_help_g_4,
	txt_help_ctrl_d,
	txt_help_ctrl_f,
	txt_help_ctrl_l,
	txt_help_g_ctrl_k,
	txt_help_g_ctrl_r,
	txt_help_g_cr,
	txt_help_g_tab,
	txt_help_b,
	txt_help_bug_report,
	txt_help_sel_c,
	txt_help_g_d,
	txt_help_g,
	txt_help_j,
	txt_help_h,
	txt_help_I,
	txt_help_g_l,
	txt_help_m,
	txt_help_M,
	txt_help_n,
	txt_help_g_q,
	txt_help_g_r,
	txt_help_s,
	txt_help_S,
	txt_help_v,
	txt_help_w,
	txt_help_W,
	txt_help_g_y,
	txt_help_y,
	txt_help_g_z,
	txt_help_g_search,
#ifndef NO_SHELL_ESCAPE
	txt_help_shell,
#endif
	(char *) 0
};

char *help_spooldir[] = {
	txt_help_4,
	txt_help_ctrl_d,
	txt_help_ctrl_f,
	txt_help_ctrl_l,
	txt_help_cr,
	txt_help_b,
	txt_help_bug_report,
	txt_help_h,
	txt_help_I,
	txt_help_j,
	txt_help_i,
	txt_help_q,
	txt_help_v,
	(char *) 0
};

char *help_group[] = {
	txt_help_i_4,
	txt_help_ctrl_d,
	txt_help_ctrl_f,
	txt_help_ctrl_k,
	txt_help_ctrl_l,
	txt_help_i_cr,
	txt_help_i_tab,
	txt_help_a,
	txt_help_b,
	txt_help_bug_report,
	txt_help_c,
	txt_help_cC,
	txt_help_d,
	txt_help_g,
	txt_help_h,
	txt_help_I,
	txt_help_j,
	txt_help_K,
	txt_help_l,
	txt_help_p_m,
	txt_help_M,
	txt_help_o,
	txt_help_i_n,
	txt_help_i_p,
	txt_help_i,
	txt_help_q,
	txt_help_r,
	txt_help_p_s,
	txt_help_T,
	txt_help_u,
	txt_help_U,
	txt_help_v,
	txt_help_w,
	txt_help_W,
	txt_help_x,
	txt_help_p_z,
	txt_help_i_search,
#ifndef NO_SHELL_ESCAPE
	txt_help_shell,
#endif
	txt_help_dash,
#ifndef NO_PIPING
	txt_help_pipe,
#endif
	txt_help_i_star,
	txt_help_i_dot,
	txt_help_i_coma,
	txt_help_i_tilda,
	txt_help_X,
	txt_help_plus,
	txt_help_equal,
	txt_help_semicolon,
	(char *) 0
};

char *help_thread[] = {
	txt_help_t_0,
	txt_help_t_4,
	txt_help_ctrl_d,
	txt_help_ctrl_f,
	txt_help_ctrl_l,
	txt_help_t_cr,
	txt_help_p_tab,
	txt_help_b,
	txt_help_bug_report,
	txt_help_d,
	txt_help_h,
	txt_help_I,
	txt_help_j,
	txt_help_ck,
	txt_help_i,
	txt_help_q,
	txt_help_T,
	txt_help_v,
	txt_help_p_z,
	(char *) 0
};

char *help_page[] = {
	txt_help_p_0,
	txt_help_p_4,
	txt_help_ctrl_d,
	txt_help_ctrl_f,
	txt_help_ctrl_h,
	txt_help_ctrl_k,
	txt_help_ctrl_l,
	txt_help_p_ctrl_r,
	txt_help_p_cr,
	txt_help_p_tab,
	txt_help_b,
	txt_help_a,
	txt_help_bug_report,
	txt_help_c,
	txt_help_cC,
	txt_help_D,
	txt_help_p_d,
	txt_help_p_f,
	txt_help_p_g,
	txt_help_h,
	txt_help_I,
	txt_help_p_k,
	txt_help_p_m,
	txt_help_M,
	txt_help_p_n,
	txt_help_o,
	txt_help_p_p,
	txt_help_i,
	txt_help_q,
	txt_help_p_r,
	txt_help_p_s,
	txt_help_t,
	txt_help_T,
	txt_help_v,
	txt_help_w,
	txt_help_W,
	txt_help_x,
	txt_help_p_z,
	txt_help_p_search,
#ifndef NO_SHELL_ESCAPE
	txt_help_shell,
#endif
	txt_help_dash,
#ifndef NO_PIPING
	txt_help_pipe,
#endif
	txt_help_thread,
	txt_help_p_star,
	txt_help_p_dot,
	txt_help_p_coma,
	txt_help_p_tilda,
	(char *) 0
};

static char *info_title;
static char **info_help;
static int cur_page;
static int group_len = 0;
static int info_type;
static int max_page;
static int pos_help;

void show_info_page (type, help, title)
	int type; 
	char *help[];
	char *title;
{
	int ch;
	int i, len;
	int help_lines = 0;
	int old_page = 0;

	if (NOTESLINES <= 0) {
		return;
	}

	if (beginner_level) {
		help_lines = NOTESLINES + MINI_HELP_LINES - 1; 
	} else {
		help_lines = NOTESLINES;
	}
	
	set_signals_help ();

	cur_page = 1;
	max_page = 1;
	pos_help = 0;
	
	info_help = help;
	info_type = type;
	info_title = title;
	
	/*
	 *  find how many elements in array
	 */
	if (type == HELP_INFO) {
		for (i=0 ; help[i] ; i++) {
			continue;
		}
	} else {
		for (i=0 ; posted[i].date[0] ; i++) {
			len = strlen (posted[i].group);
			if (len > group_len) {
 				group_len = len;
			}
 		}
	}
	
	max_page = i / help_lines;
	if (i % help_lines) {
		max_page++;
	}

	while (1) {
		if (cur_page != old_page) {
			display_info_page ();
		}

		old_page = cur_page;
		
		ch = ReadCh ();
		switch (ch) {
			case ESC:	/* common arrow keys */
#ifdef AMIGA
			case 0x9b:
#endif
				switch (get_arrow_key ()) {
					case KEYMAP_UP:
					case KEYMAP_PAGE_UP:
						goto help_page_up;
						break;

					case KEYMAP_DOWN:
					case KEYMAP_PAGE_DOWN:
						goto help_page_down;
						break;

					case KEYMAP_HOME:
						goto help_home;
						break;
					
					case KEYMAP_END:
						goto help_end;
						break;
				}
				break;

			case ctrl('D'):			/* page down */
			case ctrl('F'):			/* vi style */
			case ' ':
			case 'j':
help_page_down:
				if (cur_page < max_page) {
					pos_help = cur_page * help_lines;
					cur_page++;
				} else {
					pos_help = 0;
					cur_page = 1;
				}				
				break;
			
			case ctrl('U'):			/* page up */
			case ctrl('B'):			/* vi style */
			case 'b':
			case 'k':
help_page_up:
				if (cur_page > 1) {
					cur_page--;
					pos_help = (cur_page-1) * help_lines;
				} else {
					pos_help = (max_page-1) * help_lines;
					cur_page = max_page;
				}				
				break;

			case ctrl('R'):			/* Home */
			case 'g':
help_home:
				if (cur_page != 1) {
					cur_page = 1;
					pos_help = 0;
				}
				break;

			case '$':				/* End */
			case 'G':
help_end:
				if (cur_page != max_page) {
					cur_page = max_page;
					pos_help = (max_page-1) * help_lines;
				}
				break;

			default:
#ifndef USE_CLEARSCREEN
				ClearScreen ();
#endif	
				return;
		}	
	}
}


void display_info_page ()
{
	char buf[PATH_LEN];
	int i, help_lines;
	
	ClearScreen ();
	sprintf (buf, info_title, cur_page, max_page);
	center_line (0, TRUE, buf);
	MoveCursor (INDEX_TOP, 0);

	if (beginner_level) {
		help_lines = NOTESLINES + MINI_HELP_LINES - 1; 
	} else {
		help_lines = NOTESLINES;
	}

	if (info_type == HELP_INFO) { 
		for (i=pos_help ; i < (pos_help + help_lines) && info_help[i] ; i++) {
			fputs (info_help[i], stdout);
		}
	} else {
		for (i=pos_help ; i < (pos_help + help_lines) && posted[i].date[0] ; i++) {
			sprintf (buf, "%8s  %c  %-*s  %s",
				posted[i].date, posted[i].action,  
				group_len, posted[i].group, posted[i].subj);
				buf[COLS-2] = '\0';
			printf ("%s\r\n", buf);
		}
	}

	center_line (LINES, FALSE, txt_hit_space_for_more);
}


void show_mini_help (level)
	int level;
{
	int line = 19;
	
	if (! beginner_level) {
		return;
	}

	line = NOTESLINES + (MINI_HELP_LINES - 2);

	switch (level) {
		case SELECT_LEVEL:
			center_line (line,   FALSE, txt_mini_select_1);
			center_line (line+1, FALSE, txt_mini_select_2);
			center_line (line+2, FALSE, txt_mini_select_3);
			break;
		case SPOOLDIR_LEVEL:
			center_line (line,   FALSE, txt_mini_spooldir_1);
			break;
		case GROUP_LEVEL:
			center_line (line,   FALSE, txt_mini_group_1);
			center_line (line+1, FALSE, txt_mini_group_2);
			center_line (line+2, FALSE, txt_mini_group_3);
			break;
		case THREAD_LEVEL:
			center_line (line,   FALSE, txt_mini_thread_1);
			center_line (line+1, FALSE, txt_mini_thread_2);
			break;
		case PAGE_LEVEL:
			center_line (line,   FALSE, txt_mini_page_1);
			center_line (line+1, FALSE, txt_mini_page_2);
			center_line (line+2, FALSE, txt_mini_page_3);
			break;
		default:
			error_message ("Unknown display level", "");
			break;
	}
}


void toggle_mini_help (level)
	int level;
{
	beginner_level = !beginner_level;
	set_win_size (&LINES, &COLS);
	show_mini_help (level);
}
