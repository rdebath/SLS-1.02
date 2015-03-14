/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : feed.c
 *  Author    : I.Lea
 *  Created   : 31-08-91
 *  Updated   : 01-11-92
 *  Notes     : provides same interface to mail,pipe,print and save commands
 *  Copyright : (c) Copyright 1991-92 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

extern char *glob_group;		/* Group name */
extern char note_h_date[PATH_LEN];	/* Date:	*/
extern char note_h_newsgroups[LEN];	/* Newsgroups:	*/
extern char note_h_subj[LEN];		/* Subject:	*/
extern FILE *note_fp;			/* the body of the current article */
extern int note_end;			/* end of article ? */
extern int note_page;			/* what page we're on */
extern long note_mark[MAX_PAGES];	/* ftells on beginnings of pages */

char default_mail_address[LEN];
char default_pipe_command[LEN];
char default_save_file[PATH_LEN];
char default_regex_pattern[LEN];
char default_crosspost_group[LEN];
char proc_ch_default;			/* set in change_rcfile () */


void feed_articles (function, level, prompt, respnum, group_path)
	int function;
	int level;
	char *prompt;
	int respnum;
	char *group_path;
{
#ifndef INDEX_DAEMON

	char address[LEN];
	char command[LEN];
	char filename[PATH_LEN], *p;
	char group[LEN];
	char mailbox[LEN];
	char pattern[LEN];
	char path[PATH_LEN];
	char proc_ch = proc_ch_default;
	FILE *fp = (FILE *) 0;
	int ch = 'a', ch_default = 'a';
	int b, i, j;
	int confirm = TRUE;
	int processed_ok = TRUE;
	int proceed = FALSE;
	int is_mailbox = FALSE;
	int orig_note_end = 0;
	int orig_note_page = 0;
	int processed = 0;
	int ret1 = FALSE;
	int ret2 = FALSE;
	int redraw_screen = FALSE;
	
#ifdef NO_PIPING
	if (function == FEED_PIPE) {
		error_message (txt_piping_not_enabled, "");
		clear_message ();
		return;
	}
#endif 

	if (level == PAGE_LEVEL) {
		orig_note_end = note_end;
		orig_note_page = note_page;
	}

	b = which_thread (respnum);

	/*
	 * try and work out what default the user wants
	 */
	if (num_of_tagged_arts) {
		ch_default = 'T';
	} else if (num_of_hot_arts && default_auto_save == FALSE) {
		ch_default = 'h';
	} else if (num_of_responses (b)) {
		ch_default = 't';
	} else {
		ch_default = 'a';
	}

	i = my_group[cur_groupnum];

	if ((default_auto_save == FALSE || arts[respnum].archive == (char *) 0) ||
		(default_auto_save == TRUE && function != FEED_SAVE) ||
		ch_default == 'T') {
		do {
			sprintf (msg, "%s%s%c", prompt, txt_art_thread_regex_tag, ch_default);
			wait_message (msg);
			MoveCursor (LINES, (int) strlen (msg)-1);
			if ((ch = ReadCh ()) == CR)
				ch = ch_default;
		} while (! strchr ("ahpqtT\033", ch));
	} else {
		filename[0] = '\0';
		ch = ch_default;
		if (proc_ch != 'n') {
			/* 
			 *  Set up default for *source* / *binaries* group
			 */
			if (str_str (glob_group, "sources", 7)) {
				proc_ch = 's';		
			} else if (str_str (glob_group, "binaries", 8)) {
				proc_ch = get_post_proc_type (active[i].attribute.post_proc_type);
				if (proc_ch < POST_PROC_UUDECODE) {
					proc_ch = 'u';
				}
			} else {
				proc_ch = 's';
			}
		}	
	}

	if (ch == 'q' || ch == ESC) {	/* exit */
		clear_message ();
		return;
	}
	
	if (ch == 'p') {
		sprintf (msg, txt_feed_pattern, default_regex_pattern);
		if (! prompt_string (msg, pattern)) {
			clear_message ();
			return;
		}	
		if (strlen (pattern)) {
			my_strncpy (default_regex_pattern, pattern, 
				sizeof (default_regex_pattern));
		} else {
			if (default_regex_pattern[0]) {
				my_strncpy (pattern, default_regex_pattern, 
					sizeof (default_regex_pattern));
			} else {
				info_message (txt_no_match);
				return;
			}
		}
	}

	switch (function) {
		case FEED_MAIL:
			sprintf (msg, txt_mail_art_to, 
				COLS-(strlen(txt_mail_art_to)+30), default_mail_address);
			if (! prompt_string (msg, address)) {
				clear_message ();
				return;
			}	
			if (strlen (address)) {
				strcpy (default_mail_address, address);
			} else {
				if (default_mail_address[0]) {
					strcpy (address, default_mail_address);
				} else {
					info_message (txt_no_mail_address);	
					return;
				}
			}
			break;
		case FEED_PIPE:
			sprintf (msg, txt_pipe_to_command, 
				COLS-(strlen(txt_pipe_to_command)+30), default_pipe_command);
			if (! prompt_string (msg, command)) {
				clear_message ();
				return;
			}
			if (strlen (command)) {
				strcpy (default_pipe_command, command);
			} else {
				if (default_pipe_command[0]) {
					strcpy (command, default_pipe_command);
				} else {
					info_message (txt_no_command);	
					return;
				}
			}

			if ((fp = (FILE *) popen (command, "w")) == NULL) {
				perror_message (txt_command_failed_s, command);
				return;
			}
			wait_message (txt_piping);
			Raw (FALSE);
			break;
		case FEED_PRINT:	
			if (cmd_line_printer[0]) {
				sprintf (command, "%s %s",
					cmd_line_printer, REDIRECT_OUTPUT);
			} else {
				sprintf (command, "%s %s",
					active[i].attribute.printer, REDIRECT_OUTPUT);
			}
			break;
		case FEED_SAVE:		/* ask user for filename */
			free_save_array ();
			if ((default_auto_save == FALSE || arts[respnum].archive == (char *) 0)) {
				sprintf (msg, txt_save_filename, default_save_file);
				if (! prompt_string (msg, filename)) {
					clear_message ();
					return;
				}
				if (strlen (filename)) {
					my_strncpy (default_save_file, filename,
						sizeof (default_save_file));
				} else {
					if (default_save_file[0]) {
						my_strncpy (filename, default_save_file,
							sizeof (filename));
					} else {
						info_message (txt_no_filename);	
						return;
					}
				}
				for (p = filename; *p && (*p == ' ' || *p == '\t'); p++) {
					continue;
				}
				if (! *p) {
					info_message (txt_no_filename);
					return;
				}
				if ((filename[0] == '~' || filename[0] == '+') && strlen (filename) == 1) {
					info_message (txt_no_filename);
					return;
				}
				
				is_mailbox = create_path (filename);
				if (is_mailbox) {
					if ((int) strlen (filename) > 1) {
						my_strncpy (mailbox, filename+1, sizeof (mailbox));		
					} else {
						my_strncpy (mailbox, glob_group, sizeof (mailbox));
						/*
						 *  convert 1st letter to uppercase
						 */
						if (mailbox[0] >= 'a' && mailbox[0] <= 'z') {
							mailbox[0] = mailbox[0] - 32;
						}
					}
					my_strncpy (filename, mailbox, sizeof (filename));
				} else {		/* ask for post processing type */
					do {
						sprintf (msg, "%s%c", txt_post_process_type, proc_ch_default);
						wait_message (msg);
						MoveCursor (LINES, (int) strlen (msg)-1);
						if ((proc_ch = ReadCh ()) == CR)
							proc_ch = proc_ch_default;
					} while (! strchr ("eElLnqsu\033", proc_ch));
					if (proc_ch == 'q' || proc_ch == ESC) {	/* exit */
						clear_message ();
						return;
					}
				}
			}
			wait_message (txt_saving);
			break;
		case FEED_XPOST:	/* ask user for newsgroups */
			sprintf (msg, txt_crosspost_group, default_crosspost_group);
	
			if (! prompt_string (msg, group)) {
				clear_message ();
				return;
			}

			if (strlen (group)) {
				my_strncpy (default_crosspost_group, group,
					sizeof (default_crosspost_group));
			} else {
				if (default_crosspost_group[0]) {
					my_strncpy (group, default_crosspost_group, 
						sizeof (group));
				} else {
					info_message (txt_no_group);
					return;
				}
			}
			break;
	}
	
	switch (ch) {
		case 'a':		/* article */
			if (level == GROUP_LEVEL) {
				if (! does_article_exist (function, arts[respnum].artnum, group_path)) {
					break;
				}
			}
			switch (function) {
				case FEED_MAIL:
					redraw_screen = mail_to_someone (respnum, address, FALSE, TRUE, &processed_ok);
					break;
				case FEED_PIPE:
					fseek (note_fp, 0L, 0);
					copy_fp (note_fp, fp, "");
					break;
				case FEED_PRINT:
					processed_ok = print_file (command, respnum, 1);
					break;
				case FEED_SAVE:
					note_page = art_open (arts[respnum].artnum, group_path);
					if (note_page != ART_UNAVAILABLE) {
						add_to_save_list (0, &arts[respnum], is_mailbox, TRUE, filename);
						processed_ok = save_art_to_file (respnum, 0, FALSE, "");
					}
					break;
				case FEED_XPOST:
					redraw_screen = crosspost_article (group, respnum);
					break;
			}
			if (processed_ok) {
				processed++;
			}	
			if (mark_saved_read) {
				if (processed_ok) {
					arts[respnum].unread = ART_READ;
				}
			}
			if (level == GROUP_LEVEL) {
				art_close ();
			}
			break;
			
		case 't': 		/* thread */
			confirm = TRUE;
			for (i = (int) base[b]; i >= 0; i = arts[i].thread) {
				if (level == PAGE_LEVEL) {
					art_close ();
				}
				if (! does_article_exist (function, arts[i].artnum, group_path)) {
					continue;
				}
				switch (function) {
					case FEED_MAIL:
						processed_ok = TRUE;	
						mail_to_someone (respnum, address, FALSE, confirm, &processed_ok);
						confirm = FALSE;
						break;
					case FEED_PIPE:
						fseek (note_fp, 0L, 0);
						copy_fp (note_fp, fp, "");
						break;
					case FEED_PRINT:
						processed_ok = print_file (command, i, processed+1);
						break;
					case FEED_SAVE:
						add_to_save_list (i, &arts[i], is_mailbox, TRUE, filename);
						break;
					case FEED_XPOST:
						redraw_screen = crosspost_article (group, i);
						break;
				}
				if (processed_ok) {
					processed++;
				}
				if (mark_saved_read) {
					if (processed_ok) {
						arts[i].unread = ART_READ;
					}	
				}
				art_close ();
			}
			if (function == FEED_SAVE) {
				sort_save_list ();
				(void) save_thread_to_file (is_mailbox, group_path);
			}
			break;

		case 'T': 		/* tagged articles */
			confirm = TRUE;
			for (i=1 ; i <= num_of_tagged_arts ; i++) {
				for (j=0 ; j < top ; j++) {
					if (arts[j].tagged && arts[j].tagged == i) { 
						if (level == PAGE_LEVEL) {
							art_close ();
						}
						if (! does_article_exist (function, arts[j].artnum, group_path)) {
							continue;
						}	
						switch (function) {
							case FEED_MAIL:
								processed_ok = TRUE;
								mail_to_someone (respnum, address, FALSE, confirm, &processed_ok);
								confirm = FALSE;
								break;
							case FEED_PIPE:
								fseek (note_fp, 0L, 0);
								copy_fp (note_fp, fp, "");
								break;
							case FEED_PRINT:
								processed_ok = print_file (command, j, processed+1);
								break;
							case FEED_SAVE:
								add_to_save_list (j, &arts[j], is_mailbox, TRUE, filename);
								break;
							case FEED_XPOST:
								redraw_screen = crosspost_article (group, j);
								break;
						}
						if (processed_ok) {
							processed++;
						}	
						if (mark_saved_read) {
							if (processed_ok) {
								arts[j].unread = ART_READ;
							}	
						}
						art_close ();
					}
				}
			}
			if (function == FEED_SAVE) {				
				(void) save_regex_arts (is_mailbox, group_path);
			}
			untag_all_articles ();
			break;

		case 'h': 		/* hot (auto-selected) articles */
		case 'p': 		/* regex pattern matched articles */
			confirm = TRUE;
			for (i = 0 ; i < top_base ; i++) {
				for (j = (int) base[i]; j >= 0; j = arts[j].thread) {
					proceed = FALSE;
					if (ch == 'p') {
						if (STR_MATCH(arts[j].subject, pattern)) {
							proceed = TRUE;
						}
					} else if (arts[j].hot) {
						proceed = TRUE;
					}				
					if (proceed) {
						if (level == PAGE_LEVEL) {
							art_close ();
						}
						if (! does_article_exist (function, arts[j].artnum, group_path)) {
							continue;
						}	
						switch (function) {
							case FEED_MAIL:
								processed_ok = TRUE;
								mail_to_someone (respnum, address, FALSE, confirm, &processed_ok);
								/* confirm = FALSE; */
								break;
							case FEED_PIPE:
								fseek (note_fp, 0L, 0);
								copy_fp (note_fp, fp, "");
								break;
							case FEED_PRINT:
								processed_ok = print_file (command, j, processed+1);
								break;
							case FEED_SAVE:
								if (! is_mailbox) {
									sprintf (path, "%s.%02d", filename, processed+1);
								}
sprintf (msg, "subj=[%s] is_mailbox=[%d] path=[%s]", arts[j].subject, is_mailbox, path);
error_message (msg, "");
								add_to_save_list (0, &arts[j], is_mailbox, FALSE, path);
								processed_ok = save_art_to_file (respnum, 0, FALSE, "");
								break;
							case FEED_XPOST:
								redraw_screen = crosspost_article (group, j);
								break;
						}
						if (processed_ok) {
							processed++;
						}	
						if (mark_saved_read) {
							if (processed_ok) {
								arts[j].unread = ART_READ;
								if (ch == 'h') {
									arts[j].hot = FALSE;
									num_of_hot_arts--;
								}	
							}	
						}
	 					art_close ();
					}
				}	
			}
			break;
	}

if (debug == 2) {
	printf ("REDRAW=[%d]  ", redraw_screen);
	fflush (stdout);
}
	redraw_screen = mail_check ();	/* in case of sending to oneself */

if (debug == 2) {
	printf ("REDRAW=[%d]", redraw_screen);
	fflush (stdout);
	sleep (2);
}
	switch (function) {
		case FEED_PIPE:
			pclose (fp);		
			Raw (TRUE);
			continue_prompt ();
			redraw_screen = TRUE;
			break;
		case FEED_SAVE:
#ifndef AMIGA
			if (proc_ch != 'n' && is_mailbox == FALSE) {
				ret2 = post_process_files (proc_ch);
			}
#endif
			free_save_array ();
			break;
	}

	if (level == GROUP_LEVEL) {
		ret1 = (mark_saved_read ? TRUE : FALSE);
	}
	if ((ret1 || ret2) && is_mailbox == FALSE) {
		redraw_screen = TRUE;
	}

	if (level == PAGE_LEVEL) {
		if (ch != 'a') {
			note_page = art_open (arts[respnum].artnum, group_path);
		} else if (force_screen_redraw) {
			redraw_screen = TRUE;
		}
		note_end = orig_note_end;
		note_page = orig_note_page;
		fseek (note_fp, note_mark[note_page], 0);
		if (redraw_screen) {
			if (note_page == 0) {
				show_note_page (respnum, glob_group);
			} else {
				redraw_page (respnum, glob_group);
			}
		} else {
			if (function == FEED_PIPE) {
				clear_message ();
			}
		}
	} else {
		if (redraw_screen) {
			show_group_page ();
		}
	}
	if (function == FEED_MAIL) {	
		sprintf (msg, txt_mailed, processed);
		info_message (msg);
	} else if (function == FEED_PRINT) {	
		sprintf (msg, txt_printed, processed);
		info_message (msg);
	} else if (function == FEED_SAVE) {	
		if (ch == 'a') {
			sprintf (msg, txt_saved, processed);
			info_message (msg);
		}	
	}

#endif /* INDEX_DAEMON */
}


int print_file (command, respnum, count)
	char *command;
	int respnum;
	int count;
{
	FILE *fp;
								
	sprintf (msg, "%s%d", txt_printing, count);
	wait_message (msg);
	
	if ((fp = (FILE *) popen (command, "w")) == NULL) {
		perror_message (txt_command_failed_s, command);
		return FALSE;
	}

	if (print_header) {
		fseek(note_fp, 0L, 0);
	} else {
		fprintf (fp, "Newsgroups: %s\n", note_h_newsgroups);
		if (arts[respnum].from == arts[respnum].name) {
			fprintf (fp, "From: %s\n", arts[respnum].from);
		} else {
			fprintf (fp, "From: %s (%s)\n",
				arts[respnum].from, arts[respnum].name);
		}		
		fprintf (fp, "Subject: %s\n", note_h_subj);
		fprintf (fp, "Date: %s\n\n", note_h_date);
		fseek (note_fp, note_mark[0], 0);
	}
	copy_fp (note_fp, fp, "");

	pclose (fp);
	
	return (TRUE);	/* a hack that will check if file was really checked later */
}						


int get_post_proc_type (proc_type)
	int proc_type;
{
	int type;
		
 	switch (proc_type) {
 		case POST_PROC_SHAR:
 			type = 's';
 			break;
 		case POST_PROC_UUDECODE:
 			type = 'u';
 			break;
 		case POST_PROC_UUD_LST_ZOO:
 			type = 'l';
 			break;
 		case POST_PROC_UUD_EXT_ZOO:
 			type = 'e';
 			break;
 		case POST_PROC_UUD_LST_ZIP:
 			type = 'L';
 			break;
 		case POST_PROC_UUD_EXT_ZIP:
 			type = 'E';
 			break;
 		case POST_PROC_NONE:
 		default:
 			type = 'n';
 			break;
 	}
 	
	return type; 
}

/*
 * Opening an article here & also later in the save
 * routine is a real performance (bandwidth) killer
 * as both times the art will be transfered (Ouch!)
 *
 * So if function is to save an article only stat 
 * it the first time which saves a lot and almost
 * gets us the elusive free lunch!
 */

int does_article_exist (function, artnum, path)
	int function;
	long artnum;
	char *path;
{
	int retcode = FALSE;

	if (function == FEED_SAVE) {
		if (stat_article (artnum, path)) {
			retcode = TRUE;
		}
	} else { 
		note_page = art_open (artnum, path);	
		if (note_page != ART_UNAVAILABLE) {
			retcode = TRUE;
		}	
	}
	
	return retcode;
}
