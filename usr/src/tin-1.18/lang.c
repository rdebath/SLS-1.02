/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : lang.c
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

/*
 * active.c
 */
 
char txt_subscribe_to_new_group[] = "Subscribe to new group %s (y/n/q) [%c]: ";
char txt_delete_bogus_group[] = "Remove bogus group %s (y/n/q) [%c]: ";
char txt_reading_news_active_file[] = "Reading news active file...";
char txt_reading_mail_active_file[] = "Reading mail active file...";
char txt_reading_attributes_file[] = "Reading attributes file...";
char txt_writing_attributes_file[] = "Writing attributes file...";
char txt_reading_newsgroups_file[] = "Reading newsgroups file...";
char txt_reading_mailgroups_file[] = "Reading mailgroups file...";

/*
 *  art.c
 */

char txt_group[] = "Group %s...";
char txt_purge[] = "Purging %s...";
char txt_cannot_open_art[] = "Can't open article %s: ";
char txt_indexing_num[] = "Indexing %s (press 'q' to quit)...%4d/%d";
char txt_corrupt_index[] = "Index file %s corrupted. error %d on article %d";
char txt_checking_for_news[] = "Checking for news...";
char txt_there_is_no_news[] = "There is no news\n";
char txt_killing_arts[] = "Selecting articles...";
char txt_unkilling_arts[] = "Unselecting articles...";
char txt_catchup_update_info[] = "%s %d group(s) in %ld seconds\n";
char txt_abort_indexing[] = "Do you want to abort indexing group? (y/n): ";

/*
 *  feed.c
 */

char txt_art_thread_regex_tag[] = " a)rticle, t)hread, h)ot, p)attern, T)agged articles, q)uit: ";
#ifdef AMIGA
char txt_post_process_type[] = "Process n)one, s)har, u)ud, l)ist lha, e)xt lha, L)ist zip, E)xt zip, q)uit: ";
#else
char txt_post_process_type[] = "Process n)one, s)har, u)ud, l)ist zoo, e)xt zoo, L)ist zip, E)xt zip, q)uit: ";
#endif
#ifdef NO_REGEX
char txt_feed_pattern[] = "Enter pattern [%s]> ";
#else
char txt_feed_pattern[] = "Enter regex pattern [%s]> ";
#endif
char txt_no_command[] = "No command";
char txt_piping[] = "Piping...";
char txt_piping_not_enabled[] = "Piping not enabled. Recompile without -DNO_PIPING.";
char txt_saved[] = "-- %d Article(s) saved --";

/*
 *  group.c
 */

char txt_reading_all_arts[] = "Reading all articles...";
char txt_reading_new_arts[] = "Reading unread articles...";
char txt_cannot_post[] = "*** Posting not allowed ***";
char txt_tagged_art[] = "Tagged article";
char txt_tagged_thread[] = "Tagged thread";
char txt_untagged_art[] = "Untagged article";
char txt_untagged_thread[] = "Untagged thread";
char txt_inverse_on[] = "Inverse video enabled";
char txt_inverse_off[] = "Inverse video disabled";
char txt_subscribed_to[] = "Subscribed to %s";
char txt_unsubscribed_to[] = "Unsubscribed from %s";
char txt_mark_all_read[] = "Mark all articles as read? (y/n): ";
char txt_mark_thread_read[] = "Mark thread as read? (y/n): ";
char txt_no_more_groups[] = "No more groups";
char txt_no_prev_group[] = "No previous group";
char txt_no_arts[] = "*** No Articles ***";
char txt_no_groups[] = "*** No Groups ***";
char txt_not_active_newsfeed[] = "Command only allowed on active news";
char txt_end_of_thread[] = "*** End of Thread ***";
char txt_end_of_arts[] = "*** End of Articles ***";
char txt_end_of_groups[] = "*** End of Groups ***";
char txt_no_next_unread_art[] = "No next unread article";
char txt_no_prev_unread_art[] = "No previous unread article";
char txt_no_last_message[] = "No last message";
char txt_bad_command[] = "Bad command.  Type 'h' for help.";
char txt_you_have_mail[] = "    You have mail\n";
char txt_type_h_for_help[] = "           h=help\n";
char txt_read_art[] = "Read article> ";
char txt_search_forwards[] = "Search forwards [%s]> ";
char txt_search_backwards[] = "Search backwards [%s]> ";
char txt_author_search_forwards[] = "Author search forwards [%s]> ";
char txt_author_search_backwards[] = "Author search backwards [%s]> ";
char txt_no_search_string[] = "No search string";
char txt_no_match[] = "No match";
char txt_no_newsgroups[] = "No newsgroups";
char txt_no_quick_newsgroups[] = "\nNo newsgroups. Exiting...";
char txt_no_quick_subject[] = "\nNo subject. Exiting...";
char txt_no_subject[] = "No subject";
char txt_post_newsgroups[] = "Post to newsgroup(s) [%s]> ";
char txt_post_subject[] = "Post subject [%s]> ";
char txt_cannot_open[] = "Can't open %s";
char txt_posting[] = "Posting article...";
char txt_art_posted[] = "Article posted";
char txt_art_rejected[] = "Article rejected (saved to %s)";
char txt_quit_edit_post[] = "q)uit, e)dit, i)spell, p)ost: ";
char txt_help_4[] = "4$       Goto spooldir 4 ($=goto last spooldir)\r\n";
char txt_help_i_4[] = "4$       Goto article 4 ($=goto last article)\r\n";
char txt_help_ctrl_k[] = "^K       Kill / Auto select (hot) current article\r\n";
char txt_help_ctrl_l[] = "^L       Redraw page\r\n";
char txt_help_ctrl_d[] = "^D^U     Down (^U=up) a page\r\n";
char txt_help_ctrl_f[] = "^F^B     Down (^B=up) a page\r\n";
char txt_help_i_cr[] = "<CR>     Read current article\r\n";
char txt_help_cr[] = "<CR>     Read news from selected spooldir\r\n";
char txt_help_i_tab[] = "<TAB>    Goto next unread article or group\r\n";
char txt_help_d[] = "d        Toggle display of subject or subject & author\r\n";
char txt_help_l[] = "l        List articles within current thread\r\n";
char txt_help_m[] = "m        Move current group within group selection list\r\n";
char txt_help_M[] = "M        Menu of configurable options\r\n";
char txt_help_a[] = "aA       Author forward (A=backward) search\r\n";
char txt_help_sel_c[] = "cC       Mark group read (C=and goto next unread group)\r\n";
char txt_help_c[] = "c        Mark all articles as read and goto group selection menu\r\n";
char txt_help_cC[] = "C        Mark all articles as read and goto next unread group\r\n";
char txt_help_g[] = "g        Choose a new group by name\r\n";
char txt_help_I[] = "I        Toggle inverse video\r\n";
char txt_help_K[] = "K        Mark article/thread as read & goto next unread\r\n";
char txt_help_j[] = "jk       Down (k=up) a line\r\n";
char txt_help_i_n[] = "np       Goto next (p=previous) group\r\n";
char txt_help_i_p[] = "NP       Goto next (P=previous) unread article\r\n";
char txt_help_q[] = "Q        Quit\r\n";
char txt_help_r[] = "r        Toggle display to show all / only unread articles\r\n";
char txt_help_s[] = "su       Subscribe (u=unsubscribe) to current group\r\n";
char txt_help_S[] = "SU       Subscribe (U=unsubscribe) to groups that match pattern\r\n";
char txt_help_T[] = "T        Return to group selection level\r\n";
char txt_help_t[] = "t        Tag current article for crossposting/mailing/piping/printing/saving\r\n";
char txt_help_u[] = "u        Toggle display of unthreaded & threaded articles\r\n";
char txt_help_U[] = "U        Untag all tagged articles\r\n";
char txt_help_v[] = "v        Show version information\r\n";
char txt_help_w[] = "w        Post an article to current group\r\n";
char txt_help_x[] = "x        Crosspost current article to another group\r\n";
char txt_help_i_search[] = "/?       Subject forward (?=backward) search\r\n";
char txt_help_thread[] = "<>       Goto first (>=last) article in current thread\r\n";
#ifndef NO_SHELL_ESCAPE
char txt_help_shell[] = "!        Shell escape\r\n";
#endif
char txt_help_dash[] = "-        Show last message\r\n";
char txt_help_i_star[] = "*        Select thread\r\n";
char txt_help_i_dot[] = ".        Toggle selection of thread\r\n";
char txt_help_i_coma[] = "@        Reverse all selections (all articles)\r\n";
char txt_help_i_tilda[] ="~        Undo all selections (all articles)\r\n";
char txt_help_X[] = "X        Mark all unread articles that have not been selected as read\r\n";
char txt_help_plus[] = "+        Perform auto-selection on group\r\n";
char txt_help_equal[] = "=        Mark threads selected if at least one unread art is selected\r\n";
char txt_help_semicolon[] = ";        Mark threads selected if at least one unread art is selected\r\n";
#ifdef NO_REGEX 
char txt_save_pattern[] = "Enter save pattern [%s]> ";
#else
char txt_save_pattern[] = "Enter regex save pattern [%s]> ";
#endif
char txt_saved_pattern_to[] = "-- Saved pattern to %s - %s --";
char txt_saved_to_mailbox[] = "-- Saved to mailbox %s --";
char txt_threading_arts[] = "Threading articles...";
char txt_unthreading_arts[] = "Unthreading articles...";
char txt_select_pattern[] = "Enter selection pattern [%s]> ";

/* 
 *  help.c:
 */

char txt_group_select_com[] = "Group Selection Commands (page %d of %d)";
char txt_spooldir_com[] = "Spooldir Selection Commands (page %d of %d)";
char txt_index_page_com[] = "Index Page Commands (page %d of %d)";
char txt_thread_com[] = "Thread Commands (page %d of %d)";
char txt_art_pager_com[] = "Article Pager Commands (page %d of %d)";
char txt_hit_space_for_more[] = "PgDn,End,<SPACE>,^D - page down. PgUp,Home,b,^U - page up. <CR>,q - quit";
char txt_post_history_menu[] = "Posted articles history (page %d of %d)";
char txt_mini_select_1[] = "<n>=set current to n, TAB=next unread, /=search pattern, c)atchup,";
char txt_mini_select_2[] = "g)oto, j=line down, k=line up, h)elp, m)ove, q)uit, r=toggle all/unread,";
char txt_mini_select_3[] = "s)ubscribe, S)ub pattern, u)nsubscribe, U)nsub pattern, y)ank in/out";
char txt_mini_spooldir_1[] = "<n>=set current to n, CR=selects spooldir, h)elp, j=line down, k=line up, q)uit";
char txt_mini_group_1[] = "<n>=set current to n, TAB=next unread, /=search pattern, ^K)ill/select,";
char txt_mini_group_2[] = "a)uthor search, c)atchup, j=line down, k=line up, K=mark read, l)ist thread,";
char txt_mini_group_3[] = "|=pipe, m)ail, o=print, q)uit, r=toggle all/unread, s)ave, t)ag, w=post";
char txt_mini_thread_1[] = "<n>=set current to n, TAB=next unread, c)atchup, d)isplay toggle,";
char txt_mini_thread_2[] = "h)elp, j=line down, k=line up, q)uit, t)ag, z=mark unread";
char txt_mini_page_1[] = "<n>=set current to n, TAB=next unread, /=search pattern, ^K)ill/select,";
char txt_mini_page_2[] = "a)uthor search, c)atchup, f)ollowup, j=line down, k=line up, K=mark read,";
char txt_mini_page_3[] = "|=pipe, m)ail, o=print, q)uit, r)eply mail, s)ave, t)ag, w=post";

/* 
 *  init.c:
 */

char txt_env_var_not_found[] = "Environment variable %s not found. Set and retry...";

/* 
 *  kill.c:
 */

char txt_corrupt_kill_file[] = "Corrupt kill file %s";
char txt_kill_menu[] = "Kill / Auto-select Article Menu";
char txt_kill_how[] = "Kill type : ";
char txt_kill_subject[] = "Kill Subject [%-*.*s] (y/n): ";
char txt_kill_from[] =    "Kill From    [%-*.*s] (y/n): ";
char txt_kill_text[] = "Kill text pattern : ";
char txt_kill_text_type[] = "Apply pattern to  : ";
char txt_kill_group[] =     "Kill pattern scope: ";
char txt_help_kill_how[] = "Choose kill or auto select. <SPACE> toggles & <CR> sets.";
char txt_help_kill_subject[] = "Subject: line to add to kill file. <SPACE> toggles & <CR> sets.";
char txt_help_kill_from[] = "From: line to add to kill file. <SPACE> toggles & <CR> sets.";
char txt_help_kill_text[] = "Enter text pattern to kill if Subject: & From: lines are not what you want.";
char txt_help_kill_text_type[] = "Select where text pattern should be applied. <SPACE> toggles & <CR> sets.";
char txt_help_kill_group[] = "Kill/auto-select only current group or all groups. <SPACE> toggles & <CR> sets.";
char txt_quit_edit_save_killfile[] = "q)uit e)dit s)ave kill/hot description: ";
char txt_yes[] = "Yes";
char txt_no[] = "No ";


/* 
 *  main.c:
 */

#ifdef AMIGA
char txt_copyright_notice[] = "%s (c) Copyright 1991-92 Iain Lea & Mark Tomlinson.";
#else
char txt_copyright_notice[] = "%s (c) Copyright 1991-92 Iain Lea.";
#endif
char txt_option_not_enabled[] = "Option not enabled. Recompile with %s.";
char txt_not_in_active_file[] = "Group %s not found in active file";
char txt_screen_init_failed[] = "%s: Screen initialization failed";
char txt_bad_active_file[] = "Active file corrupt - %s";

/*
 *  misc.c
 */

char txt_cannot_open_active_file[] = "Can't open %s. Try %s -r to read news via NNTP.\n";
char txt_active_file_is_empty[] = "%s contains no newsgroups. Exiting.";
char txt_checking_active_file[] = "Checking for new newsgroups...";
char txt_checking[] = "Checking...";
char txt_cannot_find_base_art[] = "Can't find base article %s";
char txt_out_of_memory[] = "%s: out of memory";
char txt_rename_error[] = "Error: rename %s to %s";
char txt_shell_escape[] = "Enter shell command [%s]> ";
char txt_ispell_define_not_compiled[] = "Interactive spellchecker not enabled. Recompile with -DHAVE_ISPELL.";

/*
 *  newsrc.c
 */

char txt_creating_newsrc[] = "Creating .newsrc...\n";
char txt_deleting_from_newsrc[] = "Group %s not in active file. Deleting.";

/*
 *  open.c
 */
 
char txt_connecting[] = "Connecting to %s...";
char txt_reconnecting[] = "Reconnecting to %s...";
char txt_cannot_get_nntp_server_name[] = "Cannot find NNTP server name";
char txt_server_name_in_file_env_var[] = "Put the server name in the file %s,\nor set the environment variable NNTPSERVER";
char txt_failed_to_connect_to_server[] = "Failed to connect to NNTP server %s. Exiting...";
char txt_rejected_by_nntpserver[] = "Rejected by server, nntp error %d";
char txt_connection_to_server_broken[] = "Connection to server broken";
char txt_stuff_nntp_cannot_open[] = "stuff_nntp: can't open %s: ";
char txt_nntp_to_fp_cannot_reopen[] = "nntp_to_fp: can't reopen %s: ";
char txt_nntp_to_fd_cannot_reopen[] = "nntp_to_fd: can't reopen %s: ";
char txt_nntp_authorization_failed[] = "NNTP authorization password not found for %s";

/*
 *  page.c
 */

char txt_reading_article[] = "Reading...";
char txt_quit[] = "Do you really want to quit? (y/n): ";
char txt_art_unavailable[] = "Article %ld unavailable";
char txt_art_marked_as_unread[] = "Article marked as unread";
char txt_thread_marked_as_unread[] = "Thread marked as unread";
char txt_begin_of_art[] = "*** Beginning of article ***";
char txt_next_resp[] = "-- Next response --";
char txt_last_resp[] = "-- Last response --";
char txt_more[] = "--More--";
char txt_more_percent[] = "--More--(%d%%) [%ld/%ld]";
char txt_thread_x_of_n[] = "%sThread %4d of %4d\r\n";
char txt_art[] = "Article %ld  ";
char txt_resp_x_of_n[] = "Respno %3d of %3d\r\n";
char txt_no_resp[] = "No responses\r\n";
char txt_1_resp[] = "1 Response\r\n";
char txt_x_resp[] = "%d Responses\r\n";
char txt_s_at_s[] = "%s at %s";
char txt_thread_resp_page[] = "Thread %d of %d, Resp %d/%d (page %d):  %s";
char txt_thread_page[] = "Thread %d of %d (page %d):  %s";
char txt_read_resp[] = "Read response> ";
char txt_help_p_0[] = "0        Read the base article in current thread\r\n";
char txt_help_p_4[] = "4        Read response 4 in current thread\r\n";
char txt_help_p_cr[] = "<CR>     Goto to next thread\r\n";
char txt_help_p_tab[] = "<TAB>    Goto next unread article\r\n";
char txt_help_b[] = "b<SPACE> Back (<SPACE>=forward) a page\r\n";
char txt_help_bug[] = "B        Mail bug/comment to %s\r\n";
char txt_help_p_f[] = "fF       Post (f=copy text) a followup\r\n";
char txt_help_D[] = "D        Delete current article that must have been posted by you\r\n";
char txt_help_ctrl_h[] = "^H       Show articles header\r\n";
char txt_help_h[] = "hH       Command help (H=toggle mini help menu)\r\n";
char txt_help_i[] = "q        Return to previous level\r\n";
char txt_help_ck[] = "cK       Mark thread as read & return to previous level\r\n";
char txt_help_p_k[] = "kK       Mark article (K=thread) as read & advance to next unread\r\n";
char txt_help_p_m[] = "m        Mail article/thread/hot/pattern/tagged articles to someone\r\n";
char txt_help_p_n[] = "nN       Goto to the next (N=unread) article\r\n";
char txt_help_o[] = "o        Output article/thread/hot/pattern/tagged articles to printer\r\n";
char txt_help_p_p[] = "pP       Goto the previous (P=unread) article\r\n";
char txt_help_p_r[] = "rR       Reply through mail (r=copy text) to author\r\n";
char txt_help_p_s[] = "s        Save article/thread/hot/pattern/tagged articles to file\r\n";
char txt_help_p_z[] = "zZ       Mark article (Z=thread) as unread\r\n";
char txt_help_p_ctrl_r[] = "^R$      Redisplay first ($=last) page of article\r\n";
char txt_help_p_g[] = "gG       Goto first (G=last) page of article\r\n";
char txt_help_p_d[] = "d        Toggle rot-13 decoding for current article\r\n";
char txt_help_pipe[] = "|        Pipe article/thread/hot/pattern/tagged articles into command\r\n";
char txt_help_p_search[] = "/        Article forward search\r\n";
char txt_help_p_star[] = "*        Select article\r\n";
char txt_help_p_dot[] = ".        Toggle article selection\r\n";
char txt_help_p_coma[] = "@        Reverse article selections\r\n";
char txt_help_p_tilda[] = "~        Undo all selections in thread\r\n";
char txt_mail_art_to[] = "Mail article(s) to [%.*s]> ";
char txt_no_mail_address[] = "No mail address";
char txt_no_responses[] = "No responses";
char txt_quit_edit_ispell_send[] = "q)uit, e)dit, i)spell, s)end";
char txt_quit_edit_send[] = "q)uit, e)dit, s)end";
char txt_quit_edit_delete[] = "q)uit, e)dit, d)elete";
char txt_mailing_to[] = "Mailing to %s...";
char txt_mailed[] = "-- %d Article(s) mailed --";
char txt_command_failed_s[] = "Command failed: %s\n";
char txt_mail_quote[] = "In article %M you wrote:";
char txt_resp_to_poster[] = "Responses have been directed to the poster. Post anyway? (y/n): ";
char txt_resp_redirect[] = "Responses have been directed to the following newsgroups";
char txt_continue[] = "Continue? (y/n): ";
char txt_news_quote[] = "%F wrote:";
char txt_save_filename[] = "Save filename [%s]> ";
char txt_art_not_saved[] = "Article not saved";
char txt_no_filename[] = "No filename";
char txt_saving[] = "Saving...";
char txt_art_saved_to[] = "Article saved to %s";
char txt_thread_not_saved[] = "Thread not saved";
char txt_thread_saved_to_many[] = "Thread saved to %s - %s";
char txt_thread_saved_to[] = "Thread saved to %s";
char txt_pipe_to_command[] = "Pipe to command [%.*s]> ";
char txt_printing[] = "Printing...";
char txt_printed[] = "%d Article(s) printed";
char txt_append_to_file[] = "File %s exists. Append? (y/n): ";
char txt_toggled_rot13[] = "Toggled rot13 encoding";

/*
 *  post.c
 */

char txt_no_arts_posted[] = "No articles have been posted";
char txt_post_an_article[] = "Post an article...";
char txt_post_a_followup[] = "Post a followup...";
char txt_mail_bug_report[] = "Mail bug report...";
char txt_crosspost_group[] = "Crosspost article(s) to group(s) [%s]> ";
char txt_no_group[] = "No group";
char txt_crosspost_an_article[] = "Crossposting article...";
char txt_mail_bug_report_confirm[] = "Mail BUG REPORT to %s%s? (y/n): ";
char txt_reply_to_author[] = "Reply to author...";
char txt_no_blank_line[] = "No blank line found after header information. q)uit, e)dit: ";
char txt_deleting_art[] = "Deleting article...";
char txt_art_deleted[] = "Article deleted";
char txt_art_cannot_delete[] = "Article cannot be deleted";
char txt_quit_edit_xpost[] = "q)uit, e)dit, p)ost [%.*s]: %c";

/*
 *  prompt.c
 */

char txt_hit_any_key[] = "-- Press any key to continue --";
char txt_cmdline_hit_any_key[] = "Press any key to continue...";

/*
 *  rcfile.c
 */
 
char txt_opt_autosave[] = "1. Auto save       : ";
char txt_opt_start_editor_offset[] = "2. Editor Offset   : ";
char txt_opt_mark_saved_read[] = "3. Mark saved read : ";
char txt_opt_confirm_action[] =  "4. Confirm command : ";
char txt_opt_draw_arrow[] = "5. Draw arrow      : ";
char txt_opt_print_header[] = "6. Print header    : ";
char txt_opt_pos_first_unread[] = "7. Goto 1st unread : ";
char txt_opt_page_scroll[] = "8. Scroll full page: ";
char txt_opt_catchup_groups[] = "9. Catchup on quit : ";
char txt_opt_thread_arts[] =   "10 Thread articles : ";
char txt_opt_show_only_unread[] = "11 Show only unread: ";
char txt_opt_show_description[] = "12 Show description: ";
char txt_opt_show_author[] = "13 Show author     : ";
char txt_opt_process_type[] = "14 Process type    : ";
char txt_opt_sort_type[] = "15 Sort article by : ";
char txt_opt_savedir[] = "16 Save directory  : ";
char txt_opt_maildir[] = "17 Mail directory  : ";
char txt_opt_printer[] = "18 Printer         : ";
char txt_options_menu[] = "Options Menu";
char txt_show_from_none[] = "None";
char txt_show_from_addr[] = "Addr";
char txt_show_from_name[] = "Name";
char txt_show_from_both[] = "Both";
char txt_post_process_none[] = "None";
char txt_post_process_sh[] = "Shell archive";
char txt_post_process_uudecode[] = "Uudecode";
#ifdef AMIGA
char txt_post_process_uud_lst_zoo[] = "Uudecode & list lharc archive";
char txt_post_process_uud_ext_zoo[] = "Uudecode & extract lharc archive";
#else
char txt_post_process_uud_lst_zoo[] = "Uudecode & list zoo archive";
char txt_post_process_uud_ext_zoo[] = "Uudecode & extract zoo archive";
#endif
char txt_post_process_uud_lst_zip[] = "Uudecode & list zip archive";
char txt_post_process_uud_ext_zip[] = "Uudecode & extract zip archive";
char txt_sort_by_nothing[] = "Nothing";
char txt_sort_by_subj_descend[] = "Subject: field (descending)";
char txt_sort_by_subj_ascend[] = "Subject: field (ascending)";
char txt_sort_by_from_descend[] = "From: field (descending)";
char txt_sort_by_from_ascend[] = "From: field (ascending)";
char txt_sort_by_date_descend[] = "Date: field (descending)";
char txt_sort_by_date_ascend[] = "Date: field (ascending)";
char txt_help_autosave[] = "Auto save article/thread by Archive-name: header. <SPACE> toggles & <CR> sets.";
char txt_help_start_editor_offset[] = "Start editor with line offset. <SPACE> toggles & <CR> sets.";
char txt_help_confirm_action[] = "Ask for command confirmation. <SPACE> toggles & <CR> sets.";
char txt_help_print_header[] = "By printing print all/part of header. <SPACE> toggles & <CR> sets.";
char txt_help_pos_first_unread[] = "Put cursor at first/last unread art in groups. <SPACE> toggles & <CR> sets.";
char txt_help_show_author[] = "Show Subject & From (author) fields in group menu. <SPACE> toggles & <CR> sets.";
char txt_help_draw_arrow[] = "Draw -> or highlighted bar for selection. <SPACE> toggles & <CR> sets.";
char txt_help_mark_saved_read[] = "Mark saved articles/threads as read. <SPACE> toggles & <CR> sets."; 
char txt_help_page_scroll[] = "Scroll half/full page of groups/articles. <SPACE> toggles & <CR> sets."; 
char txt_help_catchup_groups[] = "Ask to mark groups read when quiting. <SPACE> toggles & <CR> sets."; 
char txt_help_thread_arts[] = "Enable/disable threading of articles in all groups. <SPACE> toggles & <CR> sets."; 
char txt_help_show_only_unread[] = "Show all articles or only unread articles. <SPACE> toggles & <CR> sets."; 
char txt_help_show_description[] = "Show short description for each newsgroup. <SPACE> toggles & <CR> sets."; 
char txt_help_post_proc_type[] = "Post process (ie. unshar) saved article/thread. <SPACE> toggles & <CR> sets."; 
char txt_help_sort_type[] = "Sort articles by Subject, From or Date fields. <SPACE> toggles & <CR> sets.";
char txt_help_savedir[] = "The directory where you want articles/threads saved.";
char txt_help_maildir[] = "The directory where articles/threads are to be saved in mailbox format.";
char txt_help_printer[] = "The printer program with options that is to be used to print articles/threads.";
char txt_select_rcfile_option[] = "Select option by entering number before text. Any other key to save.";

/*
 *  save.c
 */

char txt_post_processing[] = "Post processing...";
char txt_post_processing_finished[] = "-- post processing completed --";
char txt_deleting[] = "Deleting...";
char txt_uudecoding[] = "Uudecoding...";
char txt_extracting_shar[] ="\r\nExtracting %s...\r\n";
char txt_delete_processed_files[] = "Delete saved files that have been post processed? (y/n): ";
char txt_post_processing_failed[] = "Post processing failed";
char txt_testing_archive[] = "\r\n\r\nTesting %s archive...\r\n"; 
char txt_listing_archive[] = "\r\n\r\nListing %s archive...\r\n"; 
char txt_extracting_archive[] = "\r\n\r\nExtracting %s archive...\r\n";
char txt_checksum_of_file[] = "\r\n\r\nChecksum of %s...\r\n\r\n"; 

/*
 *  search.c
 */

char txt_searching[] = "Searching...";

/*
 *  select.c
 */

char txt_reading_all_groups[] = "Reading all groups...";
char txt_reading_new_groups[] = "Reading unread groups...";
char txt_moving[] = "Moving %s...";
#ifdef NO_REGEX
char txt_subscribe_pattern[] = "Enter subscribe pattern> ";
char txt_unsubscribe_pattern[] = "Enter unsubscribe pattern> ";
#else
char txt_subscribe_pattern[] = "Enter regex subscribe pattern> ";
char txt_unsubscribe_pattern[] = "Enter regex unsubscribe pattern> ";
#endif
char txt_subscribing[] = "Subscribing...";
char txt_subscribing_to[] = "Subscribing to %s...";
char txt_unsubscribing[] = "Unsubscribing...";
char txt_unsubscribing_from[] = "Unsubscribing from %s...";
char txt_subscribed_num_groups[] = "subscribed to %d groups";
char txt_unsubscribed_num_groups[] = "unsubscribed from %d groups";
char txt_del_group_in_newsrc[] = "Delete %s from .newsrc? (y/n): ";
char txt_group_deleted[] = "Group %s deleted";
char txt_group_undeleted[] = "Group undeleted";
char txt_mark_group_read[] = "Mark group %s as read? (y/n): ";
char txt_no_groups_to_delete[] = "No groups to delete";
char txt_reset_newsrc[] = "Reset newsrc? (y/n): ";
char txt_post_newsgroup[] = "Post newsgroup> ";
char txt_yanking_all_groups[] = "Yanking in all groups...";
char txt_yanking_sub_groups[] = "Yanking in subscribed to groups...";
char txt_no_groups_to_read[] = "No more groups to read";
char txt_added_groups[] = "Added %d group%s";
char txt_plural[] = "s";
char txt_no_groups_to_yank_in[] = "No more groups to yank in";
char txt_group_selection[] = "Group Selection";
char txt_spooldir_selection[] = "Spooldir Selection (%d)";
char txt_select_group[] = "Select group> ";
char txt_select_spooldir[] = "Select spooldir> ";
char txt_help_g_4[] = "4$       Select group 4 ($=select last group)\r\n";
char txt_help_g_ctrl_r[] = "^R       Reset .newsrc\r\n";
char txt_help_g_ctrl_k[] = "^KZ      Delete (Z=undelete) group from .newsrc\r\n";
char txt_help_g_cr[] = "<CR>     Read current group\r\n";
char txt_help_g_c[] = "c        Mark group as all read\r\n";
char txt_help_g_d[] = "d        Toggle display of groupname or groupname and description\r\n";
char txt_help_g_l[] = "l        List & select another spooldir\r\n";
char txt_help_g_tab[] =   "n<TAB>   Goto next group with unread news and enter it\r\n";
char txt_help_n[] = "N        Goto next group with unread news\r\n";
char txt_help_g_q[] = "qQ       Quit\r\n";
char txt_help_g_r[] = "r        Toggle display to show all / only unread subscribed to groups\r\n";
char txt_help_W[] = "W        List articles posted by user\r\n";
char txt_help_g_y[] = "y        Yank in subscribed/unsubscribed from .newsrc\r\n";
char txt_help_g_z[] = "z        Mark current group as unread\r\n";
char txt_help_y[] = "Y        Yank in active file to see any new news\r\n";
char txt_help_g_search[] = "/?       Group forward (?=backward) search\r\n";
char txt_newsgroup[] = "Goto newsgroup [%s]> ";
char txt_newsgroup_position[] = "Position %s in group list (1,2,..,$) [%d]> ";

/*
 *  signal.c
 */

char txt_resizing_window[] = "resizing window";
char txt_suspended_message[] = "\nStopped. Type 'fg' to restart TIN\n";

/*
 *  spooldir.c
 */

char txt_spooldirs_not_supported[] = "Multiple spooldirs are not supported";
char txt_no_spooldirs[] = "No spooldirs"; 
char txt_spooldir_server_error_1[] = "Server does not appear to support the spooldir command\n"; 
char txt_spooldir_server_error_2[] = "Reconfigure the news reader or the server & try again.\n"; 
char txt_cannot_change_spooldir[] = "Cannot change to valid spooldir. Exiting..."; 
char txt_changing_spooldir_to[] = "Changing spooldir to";

/*
 *  thread.c
 */

char txt_no_resps_in_thread[] = "No responses to list in current thread";
char txt_help_t_0[] = "0        Goto the base article in current thread\r\n";
char txt_help_t_4[] = "4$       Goto response 4 ($=goto last response) in current thread\r\n";
char txt_help_t_cr[] = "<CR>     Read current response\r\n";
char txt_help_t_tab[] = "<TAB>    Goto next unread response\r\n";
char txt_help_t_K[] =   "K        Mark thread as read & return\r\n";
