/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : rcfile.c
 *  Author    : I.Lea
 *  Created   : 01-04-91
 *  Updated   : 05-12-92
 *  Notes     :
 *  Copyright : (c) Copyright 1991-92 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

extern int index_point;

static int COL1;
static int COL2;
static int COL3;

/*
 *  read_rcfile - read defaults from ~/.tin/tinrc
 */

int read_rcfile ()
{
	char active_size_info[PATH_LEN];
	char buf[LEN];
	FILE *fp;

	if ((fp = fopen (rcfile, "r")) == NULL) {
		return (FALSE);
	}	

	while (fgets (buf, sizeof (buf), fp) != NULL) {
		if (buf[0] == '#' || buf[0] == '\n') { 
			continue;
		}	
		if (match_boolean (buf, "auto_save=", &default_auto_save)) {
			continue;
		}
		if (match_boolean (buf, "batch_save=", &default_batch_save)) {
			continue;
		}
		if (match_boolean (buf, "start_editor_offset=", &start_editor_offset)) {
			continue;
		}	
		if (match_boolean (buf, "mark_saved_read=", &mark_saved_read)) {
			continue;
		}	
		if (match_boolean (buf, "inverse_okay=", &inverse_okay)) {
			continue;
		}	
		if (match_boolean (buf, "draw_arrow=", &draw_arrow_mark)) {
			continue;
		}	
		if (match_boolean (buf, "print_header=", &print_header)) {
			continue;
		}
		if (match_number (buf, "kill_level=", &kill_level)) {
			continue;
		}
		if (match_boolean (buf, "pos_first_unread=", &pos_first_unread)) {
			continue;
		}	
		if (match_boolean (buf, "full_page_scroll=", &full_page_scroll)) {
			continue;
		}	
		if (match_boolean (buf, "catchup_read_groups=", &catchup_read_groups)) {
			continue;
		}	
		if (match_boolean (buf, "thread_articles=", &default_thread_arts)) {
			continue;
		}	
		if (match_boolean (buf, "unlink_article=", &unlink_article)) {
			continue;
		}	
		if (match_boolean (buf, "show_only_unread_groups=", &show_only_unread_groups)) {
			continue;
		}
		if (match_boolean (buf, "show_only_unread=", &default_show_only_unread)) {
			continue;
		}
		if (match_boolean (buf, "confirm_action=", &confirm_action)) {
			continue;
		}
		if (match_boolean (buf, "show_description=", &show_description)) {
			continue;
		}
		if (match_number (buf, "show_author=", &default_show_author)) {
			continue;
		}	
		if (match_number (buf, "post_process_type=", &default_post_proc_type)) {
			proc_ch_default = get_post_proc_type (default_post_proc_type);
			continue;
		}	
		if (match_number (buf, "sort_article_type=", &default_sort_art_type)) {
			continue;
		}	
		if (match_string (buf, "default_savedir=", default_savedir, sizeof (default_savedir))) {
			if (default_savedir[0] == '.' && strlen (default_savedir) == 1) {
				get_cwd (buf);
				my_strncpy (default_savedir, buf, sizeof (default_savedir));
			}
			continue;
		}	
		if (match_string (buf, "default_maildir=", default_maildir, sizeof (default_maildir))) {
			continue;
		}
		if (match_string (buf, "default_printer=", default_printer, sizeof (default_printer))) {
			continue;
		}
		if (match_string (buf, "default_sigfile=", default_sigfile, sizeof (default_sigfile))) {
			continue;
		}
		if (match_string (buf, "quote_chars=", quote_chars, sizeof (quote_chars))) {
			quote_dash_to_space (quote_chars);
			continue;
		}
		if (match_string (buf, "unread_art_mark=", buf, sizeof (buf))) {
			unread_art_mark = buf[0];
			continue;
		}
		if (match_string (buf, "hot_art_mark=", buf, sizeof (buf))) {
			hot_art_mark = buf[0];
			continue;
		}
		if (match_string (buf, "return_art_mark=", buf, sizeof (buf))) {
			return_art_mark = buf[0];
			continue;
		}
		if (match_number (buf, "reread_active_file_secs=", &reread_active_file_secs)) {
			continue;
		}
		if (match_boolean (buf, "show_last_line_prev_page=", &show_last_line_prev_page)) {
			continue;
		}
		if (match_boolean (buf, "tab_after_X_selection=", &tab_after_X_selection)) {
			continue;
		}
		if (match_boolean (buf, "force_screen_redraw=", &force_screen_redraw)) {
			continue;
		}
		if (match_boolean (buf, "display_reading_prompt=", &display_reading_prompt)) {
			continue;
		}
		if (match_boolean (buf, "save_to_mmdf_mailbox=", &save_to_mmdf_mailbox)) {
			continue;
		}
		if (match_boolean (buf, "use_builtin_inews=", &use_builtin_inews)) {
			continue;
		}
		if (match_string (buf, "default_spooldir_alias=", spooldir_alias, sizeof (spooldir_alias))) {
			continue;
		}
		if (match_string (buf, "news_quote_format=", news_quote_format, sizeof (news_quote_format))) {
			continue;
		}
		if (match_string (buf, "mail_quote_format=", mail_quote_format, sizeof (mail_quote_format))) {
			continue;
		}
#ifdef HAVE_KEYPAD
		if (match_boolean (buf, "use_keypad=", &use_keypad)) {
			continue;
		}
#endif
		if (match_boolean (buf, "slow_speed_terminal=", &slow_speed_terminal)) {
			continue;
		}
		if (match_number (buf, "groupname_max_length=", &groupname_max_length)) {
			continue;
		}
		if (match_boolean (buf, "beginner_level=", &beginner_level)) {
			continue;
		}
		if (match_string (buf, "default_author_search=", default_author_search, sizeof (default_author_search))) {
			continue;
		}
		if (match_string (buf, "default_goto_group=", default_goto_group, sizeof (default_goto_group))) {
			continue;
		}
		if (match_string (buf, "default_group_search=", default_group_search, sizeof (default_group_search))) {
			continue;
		}
		if (match_string (buf, "default_subject_search=", default_subject_search, sizeof (default_subject_search))) {
			continue;
		}
		if (match_string (buf, "default_art_search=", default_art_search, sizeof (default_art_search))) {
			continue;
		}
		if (match_string (buf, "default_crosspost_group=", default_crosspost_group, sizeof (default_crosspost_group))) {
			continue;
		}
		if (match_string (buf, "default_mail_address=", default_mail_address, sizeof (default_mail_address))) {
			continue;
		}
		if (match_number (buf, "default_move_group=", &default_move_group)) {
			continue;
		}
		if (match_string (buf, "default_pipe_command=", default_pipe_command, sizeof (default_pipe_command))) {
			continue;
		}
		if (match_string (buf, "default_post_newsgroups=", default_post_newsgroups, sizeof (default_post_newsgroups))) {
			continue;
		}
		if (match_string (buf, "default_post_subject=", default_post_subject, sizeof (default_post_subject))) {
			continue;
		}
		if (match_string (buf, "default_regex_pattern=", default_regex_pattern, sizeof (default_regex_pattern))) {
			continue;
		}
		if (match_string (buf, "default_save_file=", default_save_file, sizeof (default_save_file))) {
			continue;
		}
		if (match_string (buf, "default_select_pattern=", default_select_pattern, sizeof (default_select_pattern))) {
			continue;
		}
		if (match_string (buf, "default_shell_command=", default_shell_command, sizeof (default_shell_command))) {
			continue;
		}
		if (match_string (buf, "motd_file_info=", motd_file_info, sizeof (motd_file_info))) {
			continue;
		}
		if (match_string (buf, "active_file_info=", active_size_info, sizeof (active_size_info))) {
			load_active_size_info (active_size_info);
			continue;
		}
	}
	fclose (fp);
	return TRUE;		
}


/*
 *  write_rcfile - write defaults to ~/.tin/tinrc
 */

void write_rcfile ()
{
	FILE *fp;
	int i;
	
	if ((fp = fopen (rcfile, "w")) == NULL) {
		return;
	}	
	
	if (! cmd_line) {
		wait_message (txt_saving);
	}
	fprintf (fp, "# if ON articles/threads with Archive-name: in mail header will\n");
	fprintf (fp, "# be automatically saved with the Archive-name & part/patch no.\n");
	fprintf (fp, "auto_save=%s\n\n", (default_auto_save ? "ON" : "OFF"));
	fprintf (fp, "# if ON articles/threads will be saved in batch mode when save -S\n");
	fprintf (fp, "# or mail -M is specified on the command line\n");
	fprintf (fp, "batch_save=%s\n\n", (default_batch_save ? "ON" : "OFF"));
	fprintf (fp, "# if ON editor will be started with cursor offset into the file\n");
	fprintf (fp, "# otherwise the cursor will be positioned at the first line\n");
	fprintf (fp, "start_editor_offset=%s\n\n", (start_editor_offset ? "ON" : "OFF"));
	fprintf (fp, "# if ON mark articles that are saved as read\n");
	fprintf (fp, "mark_saved_read=%s\n\n", (mark_saved_read ? "ON" : "OFF"));
	fprintf (fp, "# if 0 killed articles are simply marked as being read\n");
	fprintf (fp, "# if 1 killed articles are removed and never seen\n");
	fprintf (fp, "kill_level=%d\n\n", kill_level);
	fprintf (fp, "# if ON use inverse video for page headers at different levels\n");
	fprintf (fp, "inverse_okay=%s\n\n", (inverse_okay ? "ON" : "OFF"));
	fprintf (fp, "# if ON use -> otherwise highlighted bar for selection\n");
	fprintf (fp, "draw_arrow=%s\n\n", (draw_arrow_mark ? "ON" : "OFF"));
	fprintf (fp, "# if ON print all of mail header otherwise Subject: & From: lines\n");
	fprintf (fp, "print_header=%s\n\n", (print_header ? "ON" : "OFF"));
	fprintf (fp, "# if ON put cursor at first unread art in group otherwise last art\n");
	fprintf (fp, "pos_first_unread=%s\n\n", (pos_first_unread ? "ON" : "OFF"));
	fprintf (fp, "# if ON scroll full page of groups/articles otherwise half a page\n");
	fprintf (fp, "full_page_scroll=%s\n\n", (full_page_scroll ? "ON" : "OFF"));
	fprintf (fp, "# if ON ask user if read groups should all be marked read\n");
	fprintf (fp, "catchup_read_groups=%s\n\n", (catchup_read_groups ? "ON" : "OFF"));
	fprintf (fp, "# if ON confirm certain commands with y/n before executing\n");
	fprintf (fp, "confirm_action=%s\n\n", (confirm_action ? "ON" : "OFF"));
	fprintf (fp, "# if ON show group description text after newsgroup name at\n");
	fprintf (fp, "# group selection level\n");
	fprintf (fp, "show_description=%s\n\n", (show_description ? "ON" : "OFF"));
	fprintf (fp, "# part of from field to display 0) none 1) address 2) full name 3) both\n");
	fprintf (fp, "show_author=%d\n\n", default_show_author);
	fprintf (fp, "# type of post processing to perform after saving articles.\n");
#ifdef AMIGA
	fprintf (fp, "# 0=(none) 1=(unshar) 2=(uudecode) 3=(uudecode & list lha)\n");
	fprintf (fp, "# 4=(uud & extract lha) 5=(uud & list zip) 6=(uud & extract zip)\n");
#else
	fprintf (fp, "# 0=(none) 1=(unshar) 2=(uudecode) 3=(uudecode & list zoo)\n");
	fprintf (fp, "# 4=(uud & extract zoo) 5=(uud & list zip) 6=(uud & extract zip)\n");
#endif
	fprintf (fp, "post_process_type=%d\n\n", default_post_proc_type);
	fprintf (fp, "# if ON all group will be threaded as default.\n");
	fprintf (fp, "thread_articles=%s\n\n", (default_thread_arts ? "ON" : "OFF"));
	fprintf (fp, "# if ON remove ~/.article after posting.\n");
	fprintf (fp, "unlink_article=%s\n\n", (unlink_article ? "ON" : "OFF"));
	fprintf (fp, "# if ON show only subscribed to groups that contain unread articles.\n");
	fprintf (fp, "show_only_unread_groups=%s\n\n", (show_only_unread_groups ? "ON" : "OFF"));
	fprintf (fp, "# if ON show only new/unread articles otherwise show all.\n");
	fprintf (fp, "show_only_unread=%s\n\n", (default_show_only_unread ? "ON" : "OFF"));
	fprintf (fp, "# sort articles by 0=(nothing) 1=(Subject descend) 2=(Subject ascend)\n");
	fprintf (fp, "# 3=(From descend) 4=(From ascend) 5=(Date descend) 6=(Date ascend).\n");
	fprintf (fp, "sort_article_type=%d\n\n", default_sort_art_type);
	fprintf (fp, "# directory where articles/threads are saved\n");
	fprintf (fp, "default_savedir=%s\n\n", default_savedir);
	fprintf (fp, "# (-m) directory where articles/threads are saved in mailbox format\n");	
	fprintf (fp, "default_maildir=%s\n\n", default_maildir);	
	fprintf (fp, "# print program with parameters used to print articles/threads\n");
	fprintf (fp, "default_printer=%s\n\n", default_printer);
	fprintf (fp, "# Signature path (random sigs)/file to be used when posting/replying to messages\n");
	fprintf (fp, "default_sigfile=%s\n\n", default_sigfile);
	fprintf (fp, "# time interval in seconds between rereading the active file\n");
	fprintf (fp, "reread_active_file_secs=%d\n\n", reread_active_file_secs);
	fprintf (fp, "# characters used in quoting to followups and replys. '_' replaced by ' '\n");
	fprintf (fp, "quote_chars=%s\n\n", quote_space_to_dash (quote_chars));
	fprintf (fp, "# character used to show that an art was unread (default '+')\n");
	fprintf (fp, "unread_art_mark=%c\n\n", unread_art_mark);
	fprintf (fp, "# character used to show that an art was auto-selected (default '*')\n");
	fprintf (fp, "hot_art_mark=%c\n\n", hot_art_mark);
	fprintf (fp, "# character used to show that an art will return (default '-')\n");
	fprintf (fp, "return_art_mark=%c\n\n", return_art_mark);
	fprintf (fp, "# if ON show the last line of the previous page as first line of next page\n");
	fprintf (fp, "show_last_line_prev_page=%s\n\n", (show_last_line_prev_page ? "ON" : "OFF"));
	fprintf (fp, "# if ON a TAB command will be automatically done after the X command\n");
	fprintf (fp, "tab_after_X_selection=%s\n\n", (tab_after_X_selection ? "ON" : "OFF"));
	fprintf (fp, "# if ON a screen redraw will always be done after certain external commands\n");
	fprintf (fp, "force_screen_redraw=%s\n\n", (force_screen_redraw ? "ON" : "OFF"));
	fprintf (fp, "# if ON 'Reading...' will be displayed when reading article from NNTP server\n");
	fprintf (fp, "display_reading_prompt=%s\n\n", (display_reading_prompt ? "ON" : "OFF"));
	fprintf (fp, "# if ON save mail to a MMDF style mailbox (default is normal mbox format)\n");
	fprintf (fp, "save_to_mmdf_mailbox=%s\n\n", (save_to_mmdf_mailbox ? "ON" : "OFF"));
	fprintf (fp, "# if ON use the builtin mini inews otherwise use an external inews program\n");
	fprintf (fp, "use_builtin_inews=%s\n\n", (use_builtin_inews ? "ON" : "OFF"));

	fprintf (fp, "# Format of quote line when mailing/posting/followingup an article\n");
	fprintf (fp, "# %%A Address  %%D Date  %%F Addr+Name  %%G Groupname  %%M MessageId  %%N Name\n");
	fprintf (fp, "news_quote_format=%s\n", news_quote_format);
	fprintf (fp, "mail_quote_format=%s\n\n", mail_quote_format);
#ifdef HAVE_KEYPAD
	fprintf (fp, "# If ON enable scroll keys on terminals that support it\n");
	fprintf (fp, "use_keypad=%s\n\n", (use_keypad ? "ON" : "OFF"));
#endif
	fprintf (fp, "# If ON strip blanks from end of lines to speedup display on slow terminals\n");
	fprintf (fp, "slow_speed_terminal=%s\n\n", (slow_speed_terminal ? "ON" : "OFF"));
	fprintf (fp, "# Maximum length of the names of newsgroups displayed\n");
	fprintf (fp, "groupname_max_length=%d\n\n", groupname_max_length);
	fprintf (fp, "# If ON show a mini menu of useful commands at each level\n");
	fprintf (fp, "beginner_level=%s\n\n", (beginner_level ? "ON" : "OFF"));

	fprintf (fp, "# default action/prompt strings\n");
	fprintf (fp, "default_spooldir_alias=%s\n", spooldir_alias);
	fprintf (fp, "default_author_search=%s\n", default_author_search);
	fprintf (fp, "default_goto_group=%s\n", default_goto_group);
	fprintf (fp, "default_group_search=%s\n", default_group_search);
	fprintf (fp, "default_subject_search=%s\n", default_subject_search);
	fprintf (fp, "default_art_search=%s\n", default_art_search);
	fprintf (fp, "default_crosspost_group=%s\n", default_crosspost_group);
	fprintf (fp, "default_mail_address=%s\n", default_mail_address);
	fprintf (fp, "default_move_group=%d\n", default_move_group);
	fprintf (fp, "default_pipe_command=%s\n", default_pipe_command);
	fprintf (fp, "default_post_newsgroups=%s\n", default_post_newsgroups);
	fprintf (fp, "default_post_subject=%s\n", default_post_subject);
	fprintf (fp, "default_regex_pattern=%s\n", default_regex_pattern);
	fprintf (fp, "default_save_file=%s\n", default_save_file);
	fprintf (fp, "default_select_pattern=%s\n", default_select_pattern);
	fprintf (fp, "default_shell_command=%s\n\n", default_shell_command);

	fprintf (fp, "# news motd file dates from server used for detecting new motd info\n");
	fprintf (fp, "motd_file_info=%s\n\n", motd_file_info);

	fprintf (fp, "# active file sizes/dates from different servers used for detecting new groups\n");
	if (! num_active_size) {
		fprintf (fp, "active_file_info=%s[%s]\n", 
			new_active_file_server, new_active_file_attribute);
	} else {
		for (i = 0 ; i < num_active_size ; i++) {
			fprintf (fp, "active_file_info=%s[%s]\n", 
				active_size[i].server, active_size[i].attribute);
		}
	}

	fclose (fp);
	chmod (rcfile, 0600);
}

/*
 *  options menu so that the user can dynamically change parameters
 */
 
int change_rcfile (group, kill_at_once)
	char *group;
	int kill_at_once;
{
	char *str = (char *) 0;
	int ch, i;
	int kill_changed = FALSE;
	int orig_show_only_unread;
	int orig_thread_arts;
	int index;
	int option;
	int ret_code = NO_KILLING;
	int var_orig;
	
#ifdef SIGTSTP
	sigtype_t (*susp)();
	
	susp = (sigtype_t (*)()) 0;

	if (do_sigtstp) {
		susp = sigdisp (SIGTSTP, SIG_DFL);
	}
#endif

	COL1 = 0;
	COL2 = ((COLS / 3) * 1) + 1;
	COL3 = ((COLS / 3) * 2) + 2;

	show_rcfile_menu ();

	while (1) {

#ifdef SIGTSTP
		if (do_sigtstp) {
			sigdisp (SIGTSTP, rcfile_suspend);
		}
#endif
		MoveCursor (LINES, 0);
		ch = ReadCh ();
		if (ch >= '1' && ch <= '9') {
			option = prompt_num (ch, "Enter option number> ");
		} else {
			if (ch == 'q' || ch == ESC) {
				option = -1;
			} else {
				option = 0;
			}
		}
#ifdef SIGTSTP
		if (do_sigtstp) {
			sigdisp (SIGTSTP, SIG_IGN);
		}
#endif
		switch (option) {
			case 0:
				write_rcfile ();
				/* FALLTHRU */
			case -1:
				if (kill_changed) {
					if (kill_at_once) {
						index = my_group[cur_groupnum];
						killed_articles = read_kill_file ();
						if (killed_articles) {
							if (kill_any_articles (index)) {
								make_threads (FALSE);
								find_base (index);
							}
						} else {
							if (unkill_all_articles ()) {
								make_threads (FALSE);
								find_base (index);
							}
						}
					}
					ret_code = KILLING;
				}

				clear_note_area ();
#ifdef SIGTSTP
				if (do_sigtstp) {
					sigdisp (SIGTSTP, susp);
				}
#endif
				return ret_code;
			
			case 1:		/* auto save */
				prompt_on_off (INDEX_TOP, COL1, &default_auto_save, 
					txt_help_autosave, txt_opt_autosave);
				break;

			case 2:		/* start editor with line offset */
				prompt_on_off (INDEX_TOP, COL2, &start_editor_offset, 
					txt_help_start_editor_offset, txt_opt_start_editor_offset);
				break;
			
			case 3:		/* mark saved articles read */
				prompt_on_off (INDEX_TOP, COL3, &mark_saved_read, 
					txt_help_mark_saved_read, txt_opt_mark_saved_read);
				break;

			case 4:		/* confirm action */
				prompt_on_off (INDEX_TOP+2, COL1, &confirm_action, 
					txt_help_confirm_action, txt_opt_confirm_action);
				break;

			case 5:		/* draw -> / highlighted bar */
				prompt_on_off (INDEX_TOP+2, COL2, &draw_arrow_mark, 
					txt_help_draw_arrow, txt_opt_draw_arrow);
				if (draw_arrow_mark == FALSE && inverse_okay == FALSE) {
					inverse_okay = TRUE;
				}
				break;

			case 6:		/* print header */
				prompt_on_off (INDEX_TOP+2, COL3, &print_header, 
					txt_help_print_header, txt_opt_print_header);
				break;
			
			case 7:		/* position cursor at first / last unread art */
				prompt_on_off (INDEX_TOP+4, COL1, &pos_first_unread, 
					txt_help_pos_first_unread, txt_opt_pos_first_unread);
				break;

			case 8:		/* scroll half/full page of groups/articles */
				prompt_on_off (INDEX_TOP+4, COL2, &full_page_scroll, 
					txt_help_page_scroll, txt_opt_page_scroll);
				break;

			case 9:		/* catchup read groups when quitting */
				prompt_on_off (INDEX_TOP+4, COL3, &catchup_read_groups, 
					txt_help_catchup_groups, txt_opt_catchup_groups);
				break;

			case 10:	/* thread/unthread all groups except those in ~/.tin/unthreaded */
				orig_thread_arts = default_thread_arts;	
				prompt_on_off (INDEX_TOP+6, COL1, &default_thread_arts, 
					txt_help_thread_arts, txt_opt_thread_arts);
				if (default_thread_arts != orig_thread_arts || group != (char *) 0) {
					make_threads (TRUE);
					find_base (my_group[cur_groupnum]);
				}
				clear_message ();
				break;

			case 11:	/* show all arts or just new/unread arts */
				orig_show_only_unread = default_show_only_unread;	
				prompt_on_off (INDEX_TOP+6, COL2, &default_show_only_unread, 
					txt_help_show_only_unread, txt_opt_show_only_unread);
				if (default_show_only_unread != orig_show_only_unread || group != (char *) 0) {
					make_threads (TRUE);
					find_base (my_group[cur_groupnum]);
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
				}
				break;

			case 12:	/* show newsgroup description text next to newsgroups */
				prompt_on_off (INDEX_TOP+6, COL3, &show_description, 
					txt_help_show_description, txt_opt_show_description);
				if (show_description) {	/* force reread of newgroups file */
					reread_active_file = TRUE;
				} else {
					set_groupname_len (FALSE);
				}
				break;

			case 13:		/* show subject & author / subject only */
				var_orig = default_show_author;
				show_menu_help (txt_help_show_author);
				do {
					MoveCursor (INDEX_TOP+8, COL1 + (int) strlen (txt_opt_show_author));
					if ((ch	= ReadCh()) == ' ') {
						if (default_show_author + 1 > SHOW_FROM_BOTH) {
							default_show_author = SHOW_FROM_NONE;
						} else {
							default_show_author++;
						}
						switch (default_show_author) {
							case SHOW_FROM_NONE:
								str = txt_show_from_none;
								break;
							case SHOW_FROM_ADDR:
								str = txt_show_from_addr;
								break;
							case SHOW_FROM_NAME:
								str = txt_show_from_name;
								break;
							case SHOW_FROM_BOTH:
								str = txt_show_from_both;
								break;
						}
						fputs (str, stdout);
						fflush (stdout);
					}
				} while (ch != CR && ch != ESC);

				if (ch == ESC) {	/* restore original value */
					default_show_author = var_orig;
					switch (default_show_author) {
						case SHOW_FROM_NONE:
							str = txt_show_from_none;
							break;
						case SHOW_FROM_ADDR:
							str = txt_show_from_addr;
							break;
						case SHOW_FROM_NAME:
							str = txt_show_from_name;
							break;
						case SHOW_FROM_BOTH:
							str = txt_show_from_both;
							break;
					}
					fputs (str, stdout);
					fflush (stdout);
				}
#if 0
				 else {
					set_subj_from_size (COLS);
				}
#endif				
				break;

			case 14:
				var_orig = default_post_proc_type;
				show_menu_help (txt_help_post_proc_type);
				do {
					MoveCursor (INDEX_TOP+8, COL2 + (int) strlen (txt_opt_process_type));
					if ((ch	= ReadCh()) == ' ') {
						if (default_post_proc_type + 1 > POST_PROC_UUD_EXT_ZIP) {
							default_post_proc_type = POST_PROC_NONE;
						} else {
							default_post_proc_type++;
						}
						proc_ch_default = get_post_proc_type (default_post_proc_type);
						switch (default_post_proc_type) {
							case POST_PROC_NONE:
								str = txt_post_process_none;
								break;
							case POST_PROC_SHAR:
								str = txt_post_process_sh;
								break;
							case POST_PROC_UUDECODE:
								str = txt_post_process_uudecode;
								break;
							case POST_PROC_UUD_LST_ZOO:
								str = txt_post_process_uud_lst_zoo;
								break;
							case POST_PROC_UUD_EXT_ZOO:
								str = txt_post_process_uud_ext_zoo;
								break;
							case POST_PROC_UUD_LST_ZIP:
								str = txt_post_process_uud_lst_zip;
								break;
							case POST_PROC_UUD_EXT_ZIP:
								str = txt_post_process_uud_ext_zip;
								break;
						}
						CleartoEOLN (); 
						fputs (str, stdout);
						fflush (stdout);
					}
				} while (ch != CR && ch != ESC);

				if (ch == ESC) {	/* restore original value */
					default_post_proc_type = var_orig;
					switch (default_post_proc_type) {
						case POST_PROC_NONE:
							str = txt_post_process_none;
							proc_ch_default = 'n';
							break;
						case POST_PROC_SHAR:
							str = txt_post_process_sh;
							proc_ch_default = 's';
							break;
						case POST_PROC_UUDECODE:
							str = txt_post_process_uudecode;
							proc_ch_default = 'u';
							break;
						case POST_PROC_UUD_LST_ZOO:
							str = txt_post_process_uud_lst_zoo;
							proc_ch_default = '1';
							break;
						case POST_PROC_UUD_EXT_ZOO:
							str = txt_post_process_uud_ext_zoo;
							proc_ch_default = '2';
							break;
						case POST_PROC_UUD_LST_ZIP:
							str = txt_post_process_uud_lst_zip;
							proc_ch_default = '3';
							break;
						case POST_PROC_UUD_EXT_ZIP:
							str = txt_post_process_uud_ext_zip;
							proc_ch_default = '4';
							break;
					}
					CleartoEOLN (); 
					fputs (str, stdout);
					fflush (stdout);
				}
				break;

			case 15:
				var_orig = default_sort_art_type;
				show_menu_help (txt_help_sort_type);
				do {
					MoveCursor (INDEX_TOP+10, COL1 + (int) strlen (txt_opt_sort_type));
					if ((ch	= ReadCh()) == ' ') {
						if (default_sort_art_type + 1 > SORT_BY_DATE_ASCEND) {
							default_sort_art_type = SORT_BY_NOTHING;
						} else {
							default_sort_art_type++;
						}
						switch (default_sort_art_type) {
							case SORT_BY_NOTHING:
								str = txt_sort_by_nothing;
								break;
							case SORT_BY_SUBJ_DESCEND:
								str = txt_sort_by_subj_descend;
								break;
							case SORT_BY_SUBJ_ASCEND:
								str = txt_sort_by_subj_ascend;
								break;
							case SORT_BY_FROM_DESCEND:
								str = txt_sort_by_from_descend;
								break;
							case SORT_BY_FROM_ASCEND:
								str = txt_sort_by_from_ascend;
								break;
							case SORT_BY_DATE_DESCEND:
								str = txt_sort_by_date_descend;
								break;
							case SORT_BY_DATE_ASCEND:
								str = txt_sort_by_date_ascend;
								break;
						}
						CleartoEOLN (); 
						fputs (str, stdout);
						fflush (stdout);
					}
				} while (ch != CR && ch != ESC);

				if (ch == ESC) {	/* restore original value */
					default_sort_art_type = var_orig;
					switch (default_sort_art_type) {
						case SORT_BY_NOTHING:
							str = txt_sort_by_nothing;
							break;
						case SORT_BY_SUBJ_DESCEND:
							str = txt_sort_by_subj_descend;
							break;
						case SORT_BY_SUBJ_ASCEND:
							str = txt_sort_by_subj_ascend;
							break;
						case SORT_BY_FROM_DESCEND:
							str = txt_sort_by_from_descend;
							break;
						case SORT_BY_FROM_ASCEND:
							str = txt_sort_by_from_ascend;
							break;
						case SORT_BY_DATE_DESCEND:
							str = txt_sort_by_date_descend;
							break;
						case SORT_BY_DATE_ASCEND:
							str = txt_sort_by_date_ascend;
							break;
					}
					CleartoEOLN (); 
					fputs (str, stdout);
					fflush (stdout);
				}
				break;
#ifndef AMIGA_BBS
			case 16:
				show_menu_help (txt_help_savedir);
				prompt_menu_string (INDEX_TOP+12, COL1 + (int) strlen (txt_opt_savedir), default_savedir);
				expand_rel_abs_pathname (INDEX_TOP+12, COL1 + (int) strlen (txt_opt_savedir), default_savedir);
				break;

			case 17:
				show_menu_help (txt_help_maildir);
				prompt_menu_string (INDEX_TOP+14, COL1 + (int) strlen (txt_opt_maildir), default_maildir);
				expand_rel_abs_pathname (INDEX_TOP+14, COL1 + (int) strlen (txt_opt_maildir), default_maildir);
				break;

			case 18:
				show_menu_help (txt_help_printer);
				prompt_menu_string (INDEX_TOP+16, COL1 + (int) strlen (txt_opt_printer), default_printer);
				expand_rel_abs_pathname (INDEX_TOP+16, COL1 + (int) strlen (txt_opt_printer), default_printer);
				break;
#endif	/* AMIGA_BBS */
		}
		show_menu_help (txt_select_rcfile_option);
	}
}


void show_rcfile_menu ()
{
	char *str = (char *) 0;

	ClearScreen ();

	center_line (0, TRUE, txt_options_menu);
	
	MoveCursor (INDEX_TOP, 0);
	printf ("%s%s\r\n\r\n", txt_opt_autosave, (default_auto_save ? "ON " : "OFF"));
	printf ("%s%s\r\n\r\n", txt_opt_confirm_action, (confirm_action ? "ON " : "OFF"));
	printf ("%s%s\r\n\r\n", txt_opt_pos_first_unread, (pos_first_unread ? "ON " : "OFF"));
	printf ("%s%s", txt_opt_thread_arts, (default_thread_arts ? "ON " : "OFF"));

	MoveCursor(INDEX_TOP, COL2);
	printf ("%s%s", txt_opt_start_editor_offset, (start_editor_offset ? "ON " : "OFF"));
	MoveCursor(INDEX_TOP+2, COL2);
	printf ("%s%s", txt_opt_draw_arrow, (draw_arrow_mark ? "ON " : "OFF"));
	MoveCursor(INDEX_TOP+4, COL2);
	printf ("%s%s", txt_opt_page_scroll, (full_page_scroll ? "ON " : "OFF"));
	MoveCursor(INDEX_TOP+6, COL2);
	printf ("%s%s", txt_opt_show_only_unread, (default_show_only_unread ? "ON " : "OFF"));

	MoveCursor(INDEX_TOP, COL3);
	printf ("%s%s", txt_opt_mark_saved_read, (mark_saved_read ? "ON " : "OFF"));
	MoveCursor(INDEX_TOP+2, COL3);
	printf ("%s%s", txt_opt_print_header, (print_header ? "ON " : "OFF"));
	MoveCursor(INDEX_TOP+4, COL3);
	printf ("%s%s", txt_opt_catchup_groups, (catchup_read_groups ? "ON " : "OFF"));
	MoveCursor(INDEX_TOP+6, COL3);
	printf ("%s%s", txt_opt_show_description, (show_description ? "ON " : "OFF"));

	MoveCursor(INDEX_TOP+8, COL1);
	switch (default_show_author) {
		case SHOW_FROM_NONE:
			str = txt_show_from_none;
			break;
		case SHOW_FROM_ADDR:
			str = txt_show_from_addr;
			break;
		case SHOW_FROM_NAME:
			str = txt_show_from_name;
			break;
		case SHOW_FROM_BOTH:
			str = txt_show_from_both;
			break;
	}
	printf ("%s%s", txt_opt_show_author, str);
	MoveCursor(INDEX_TOP+8, COL2);
	switch (default_post_proc_type) {
		case POST_PROC_NONE:
			str = txt_post_process_none;
			break;
		case POST_PROC_SHAR:
			str = txt_post_process_sh;
			break;
		case POST_PROC_UUDECODE:
			str = txt_post_process_uudecode;
			break;
		case POST_PROC_UUD_LST_ZOO:
			str = txt_post_process_uud_lst_zoo;
			break;
		case POST_PROC_UUD_EXT_ZOO:
			str = txt_post_process_uud_ext_zoo;
			break;
		case POST_PROC_UUD_LST_ZIP:
			str = txt_post_process_uud_lst_zip;
			break;
		case POST_PROC_UUD_EXT_ZIP:
			str = txt_post_process_uud_ext_zip;
			break;
	}

	printf ("%s%s\r\n\r\n", txt_opt_process_type, str);
	
	MoveCursor(INDEX_TOP+10, COL1);
	switch (default_sort_art_type) {
		case SORT_BY_NOTHING:
			str = txt_sort_by_nothing;
			break;
		case SORT_BY_SUBJ_DESCEND:
			str = txt_sort_by_subj_descend;
			break;
		case SORT_BY_SUBJ_ASCEND:
			str = txt_sort_by_subj_ascend;
			break;
		case SORT_BY_FROM_DESCEND:
			str = txt_sort_by_from_descend;
			break;
		case SORT_BY_FROM_ASCEND:
			str = txt_sort_by_from_ascend;
			break;
		case SORT_BY_DATE_DESCEND:
			str = txt_sort_by_date_descend;
			break;
		case SORT_BY_DATE_ASCEND:
			str = txt_sort_by_date_ascend;
			break;
	}
	printf ("%s%s\r\n\r\n", txt_opt_sort_type, str);

#ifndef AMIGA_BBS
	printf ("%s%s\r\n\r\n", txt_opt_savedir, default_savedir);
	printf ("%s%s\r\n\r\n", txt_opt_maildir, default_maildir);
	printf ("%s%s\r\n\r\n", txt_opt_printer, default_printer);
#endif	/* AMIGA_BBS */
	fflush(stdout);

	show_menu_help (txt_select_rcfile_option);
	MoveCursor (LINES, 0);
}

/*
 *  expand ~/News to /usr/username/News and print to screen
 */
 
void expand_rel_abs_pathname (line, col, str)
	int line;
	int col;
	char *str;
{
	char buf[LEN];
	
	if (str[0] == '~') {
		if (strlen (str) == 1) {
			strcpy (str, homedir);
		} else {
			joinpath (buf, homedir, str+2);
			strcpy (str, buf);
		}
	}
	MoveCursor (line, col);
	CleartoEOLN ();
	puts (str);
	fflush (stdout);
}

/*
 *  show_menu_help
 */
 
void show_menu_help (help_message)
	char *help_message;
{
	 MoveCursor (LINES-2, 0);
	 CleartoEOLN ();
	 center_line (LINES-2, FALSE, help_message);
}


int match_boolean (line, pat, dst)
	char *line;
	char *pat;
	int *dst;
{
	int	patlen = strlen (pat);

	if (strncmp (line, pat, patlen) == 0) {
		*dst = (strncmp (&line[patlen], "ON", 2) == 0 ? TRUE : FALSE);
		return TRUE;
	}
	return FALSE;
}


int match_number (line, pat, dst)
	char *line;
	char *pat;
	int *dst;
{
	int	patlen = strlen (pat);

	if (strncmp (line, pat, patlen) == 0) {
		*dst = atoi (&line[patlen]);
		return TRUE;
	}
	return FALSE;
}


int match_string (line, pat, dst, dstlen)
	char *line;
	char *pat;
	char *dst;
	int dstlen;
{
	int	patlen = strlen (pat);

	if (strncmp (line, pat, patlen) == 0) {
		strncpy (dst, &line[patlen], dstlen);
		dst[strlen (dst) - 1] = '\0';
		return TRUE;
	}
	return FALSE;
}

/*
 *  convert underlines to spaces in a string
 */

void quote_dash_to_space (s)
	char *s;
{
	int i;

	for (i=0 ; i < strlen (s) ; i++) {
		if (s[i] == '_') {
			s[i] = ' ';
		}
	}
}

/*
 *  convert spaces to underlines in a string
 */

char *quote_space_to_dash (s)
	char *s;
{
	int i;
	static char ds[PATH_LEN];

	for (i=0 ; i < strlen (s) ; i++) {
		(s[i] == ' ') ? (ds[i] = '_') : (ds[i] = s[i]);
	}	
	ds[i] = '\0';

	return ds;
}
