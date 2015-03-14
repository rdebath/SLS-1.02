/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : kill.c
 *  Author    : I.Lea & J.Robinson
 *  Created   : 01-04-91
 *  Updated   : 27-08-92
 *  Notes     : kill & auto select (hot) articles
 *  Copyright : (c) Copyright 1991-92 by Iain Lea & Jim Robinson
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

#ifdef NO_REGEX 
char *stars = "";
#else		
char *stars = "*";
#endif

#define SET_KILLED(i)		(arts[i].unread = ART_READ, arts[i].killed = 1)
#define SET_HOT(i)		(arts[i].hot = 1)
#define IS_READ(i)		(arts[i].unread == ART_READ)
#define IS_KILLED(i)		(arts[i].killed == 1)

#define KILL_CHAR	'K'
#define HOT_CHAR	'H'

#define K_KILL		0
#define K_HOT		1

int kill_level = 1;
int kill_num = 0;
struct kill_t *killf;

/*
 *  read_kill_file - read ~/.tin/kill file contents into kill array
 */

int read_kill_file ()
{
	char buf[LEN];
	FILE *fp;
	int n;
	char c;
	unsigned int type;

	free_kill_array ();
	
	if ((fp = fopen (killfile, "r")) == NULL) {
		return FALSE;
	}

	kill_num=0;
	while (fgets (buf, sizeof buf, fp) != NULL) {
		if (buf[0] == '#') {
			continue;
		}	
		if (kill_num == max_kill-1) {
			expand_kill ();
		}
		n = sscanf(buf, "%d %c", &type, &c);
		if (n == 0) {
			goto corrupt_killfile;
		}	
		if (n > 1 && c == HOT_CHAR) {	/* hot */
			 killf[kill_num].kill_how = K_HOT;
		} else {
			 killf[kill_num].kill_how = K_KILL;
		}	 
		killf[kill_num].kill_type = type;

		if (fgets (buf, sizeof buf, fp) == NULL)  {
			goto corrupt_killfile;
		}
		
		killf[kill_num].kill_group = (long) atol (buf);

		switch (killf[kill_num].kill_type) {
		case KILL_SUBJ:
			if (fgets (buf, sizeof buf, fp) != NULL) {
				buf[strlen (buf)-1] = '\0';
				killf[kill_num].kill_subj = str_dup (buf);
			}
			break;
		case KILL_FROM:
			if (fgets (buf, sizeof buf, fp) != NULL) {
				buf[strlen (buf)-1] = '\0';
				killf[kill_num].kill_from = str_dup (buf);
			}
			break;
		case KILL_BOTH:
			if (fgets (buf, sizeof buf, fp) != NULL) {
				buf[strlen (buf)-1] = '\0';
				killf[kill_num].kill_subj = str_dup (buf);
			}
			if (fgets (buf, sizeof buf, fp) != NULL) {
				buf[strlen (buf)-1] = '\0';
				killf[kill_num].kill_from = str_dup (buf);
			}
			break;
		default:
			goto corrupt_killfile;
		}
		kill_num++;
	}

	fclose (fp);
	return (kill_num);

corrupt_killfile:
	fclose (fp);
	killf[kill_num].kill_type = 0;
	error_message (txt_corrupt_kill_file, killfile);
	return FALSE;
}

/*
 *  write_kill_file - write kill strings to ~/.tin/kill
 */

void write_kill_file ()
{
	FILE *fp;
	int i;
	
	if (kill_num == 0 || (fp = fopen (killfile, "w")) == NULL) {
		return;
	}

	wait_message (txt_saving);
	fprintf (fp, "# 1st line  1=(Subject: only)  2=(From: only)  3=(Subject: & From:)\n");
	fprintf (fp, "#           %c=(kill) %c=(auto-selection)\n", KILL_CHAR, HOT_CHAR);
	fprintf (fp, "# 2nd line  0=(kill on all newsgroups)  >0=(kill on specific newsgroup)\n");
	for (i=0 ; i < kill_num ; i++) {
		if (killf[i].kill_type == 0 || (killf[i].kill_subj == 0 
		    &&  killf[i].kill_from == 0)) 
			continue;

		if (killf[i].kill_how == K_KILL) {
			fprintf (fp, "#\n# %03d KILL\n", i+1);
			fprintf (fp, "%d\t%c\n", killf[i].kill_type, KILL_CHAR);
		} else {
			fprintf (fp, "#\n# %03d HOT\n", i+1);
			fprintf (fp, "%d\t%c\n", killf[i].kill_type, HOT_CHAR);
		}	
		fprintf (fp, "%ld\n", killf[i].kill_group);

		switch (killf[i].kill_type) {
			case KILL_SUBJ:
				fprintf (fp, "%s\n", killf[i].kill_subj);
				break;
			case KILL_FROM:
				fprintf (fp, "%s\n", killf[i].kill_from);
				break;
			case KILL_BOTH:
				fprintf (fp, "%s\n", killf[i].kill_subj);
				fprintf (fp, "%s\n", killf[i].kill_from);
				break;
		}
	}

	fclose (fp);
	chmod (killfile, 0600);
}

static int get_choice (x, help, prompt, opt1, opt2, opt3, opt4)
	int x;
	char *help, *prompt, *opt1, *opt2, *opt3, *opt4;
{
	int ch, n = 0, i = 0;
	char *argv[4];
	
	if (opt1)
		argv[n++] = opt1;
	if (opt2)
		argv[n++] = opt2;
	if (opt3)
		argv[n++] = opt3;
	if (opt4)
		argv[n++] = opt4;
	assert(n > 0);

	if (help)
		show_menu_help (help);
		
	do {
		MoveCursor(x, (int) strlen (prompt));
		fputs (argv[i], stdout);
		fflush (stdout);
		CleartoEOLN (); 
		if ((ch = ReadCh ()) != ' ')
			continue;
		if (++i == n)
			i = 0;
	} while (ch != CR && ch != ESC);

	if (ch == ESC)
		return (-1);
	return (i);
}

/*
 *  options menu so that the user can dynamically change parameters
 */
 
int kill_art_menu (group_name, index)
	char *group_name;
	int index;
{
	char buf[LEN];
	char text[LEN];
	char kill_from[LEN];
	char kill_subj[LEN];
	char kill_group[LEN];
	char ch_default = 's';
	int ch;
	int counter = 0;
	int killed = TRUE;
	int kill_from_ok = FALSE;
	int kill_subj_ok = FALSE;
	int kill_every_group = FALSE;
	int i;
	int kill_how;

#ifdef SIGTSTP
	sigtype_t (*susp)();
	
	susp = (sigtype_t (*)()) 0;

	if (do_sigtstp) {
		susp = sigdisp (SIGTSTP, SIG_DFL);
		sigdisp (SIGTSTP, SIG_IGN);
	}
#endif
	
	sprintf (kill_group, "%s only", group_name);
	sprintf (kill_subj, txt_kill_subject, COLS-35, COLS-35, arts[index].subject);
	if (arts[index].name != (char *) 0) {
		sprintf (text, "%s (%s)", arts[index].from, arts[index].name);
	} else {
		strcpy (text, arts[index].from);
	}
	sprintf (kill_from, txt_kill_from, COLS-35, COLS-35, text);
	text[0] = '\0';
	
	ClearScreen ();

	center_line (0, TRUE, txt_kill_menu);
	
	MoveCursor (INDEX_TOP, 0);
	printf ("%s\r\n\r\n\r\n", txt_kill_how);
	printf ("%s\r\n\r\n", txt_kill_text);
	printf ("%s\r\n\r\n\r\n", txt_kill_text_type);
	printf ("%s\r\n\r\n", kill_subj);
	printf ("%s\r\n\r\n\r\n", kill_from);
	printf ("%s%s", txt_kill_group, kill_group);
	fflush (stdout);

	i = get_choice (INDEX_TOP, txt_help_kill_how, txt_kill_how, 
		       "Kill       ", "Auto Select", NULL, NULL);
	if (i == -1) {
		return FALSE;
	}	
	kill_how = (i == 0 ? K_KILL : K_HOT);

	show_menu_help (txt_help_kill_text);
	
	if (! prompt_menu_string (INDEX_TOP+3, (int) strlen (txt_kill_text), text)) {
		return FALSE;
	}

	if (text[0]) {
		i = get_choice(INDEX_TOP+5, txt_help_kill_text_type, 
			       txt_kill_text_type, "Subject: line only    ", 
			       "From: line only       ", "Subject: & From: lines", 
			       NULL);
		if (i == -1) {
			return FALSE;
		}	
		counter = ((i == 0 ? KILL_SUBJ : (i == 1 ? KILL_FROM : KILL_BOTH)));
	}

	if (! text[0]) {
		i = get_choice (INDEX_TOP+8, txt_help_kill_subject, 
			        kill_subj, txt_yes, txt_no, NULL, NULL);
		if (i == -1) {
			return FALSE;
		} else {
			kill_subj_ok = (i ? FALSE : TRUE);
		}
		if (kill_subj_ok) {
			i = get_choice (INDEX_TOP+10, txt_help_kill_from, 
			        kill_from, txt_no, txt_yes, NULL, NULL);
		} else {
			i = get_choice (INDEX_TOP+10, txt_help_kill_from, 
			        kill_from, txt_yes, txt_no, NULL, NULL);
		}
		if (i == -1) {
			return FALSE;
		} else {
			if (kill_subj_ok) {
				kill_from_ok = (i ? TRUE : FALSE);
			} else {
				kill_from_ok = (i ? FALSE : TRUE);
			}
		}
	}

	if (text[0] || kill_subj_ok || kill_from_ok) {
		i = get_choice (INDEX_TOP+13, txt_help_kill_group, 
			       txt_kill_group, kill_group, "All groups", 
			       NULL, NULL);
		if (i == -1) {
			return FALSE;
		}	
		kill_every_group = (i == 0 ? FALSE : TRUE);
	}

	while (1) {
		do {
			sprintf (msg, "%s%c", txt_quit_edit_save_killfile, ch_default);
			wait_message (msg);
			MoveCursor (LINES, (int) strlen (txt_quit_edit_save_killfile));
			if ((ch = ReadCh ()) == CR)
				ch = ch_default;
		} while (ch != ESC && ch != 'q' && ch != 'e' && ch != 's');
		switch (ch) {
		case 'e':
			start_line_offset = 2;
			invoke_editor (killfile);
			unkill_all_articles ();
			killed_articles = read_kill_file ();
			killed = TRUE;
			goto kill_done;

		case 'q':
		case ESC:
			killed = FALSE;
			goto kill_done;
			
		case 's':
			if (kill_num > max_kill-1) {
				expand_kill ();
			}

			killf[kill_num].kill_how = kill_how;

			if (text[0]) {
				sprintf (buf, "%s%s%s", stars, text, stars);
				switch (counter) {
				case KILL_SUBJ:
					killf[kill_num].kill_subj = str_dup (buf);
					break;
				case KILL_FROM:
					killf[kill_num].kill_from = str_dup (buf);
					break;
				case KILL_BOTH:
					killf[kill_num].kill_subj = str_dup (buf);
					killf[kill_num].kill_from = killf[kill_num].kill_subj; 
					break;
				}
				killf[kill_num].kill_type = counter;
				if (kill_every_group) {
					killf[kill_num].kill_group = 0L;
				} else {
					killf[kill_num].kill_group = hash_s (group_name);
				}
				kill_num++;
			} else {
				if (kill_subj_ok) {
					killf[kill_num].kill_type = KILL_SUBJ;
					sprintf (buf, "%s%s%s", 
						stars, arts[index].subject, stars);
					killf[kill_num].kill_subj = str_dup (buf);
				}
				if (kill_from_ok) {
					killf[kill_num].kill_type |= KILL_FROM;
					if (arts[index].name != (char *) 0) {
						sprintf (buf, "%s%s (%s)%s", 
							stars, arts[index].from, arts[index].name, stars);
					} else {
						sprintf (buf, "%s%s%s", 
							stars, arts[index].from, stars);
					}
					killf[kill_num].kill_from = str_dup (buf);
				}
				if (killf[kill_num].kill_type) {		
					if (kill_every_group) {
						killf[kill_num].kill_group= 0L;
					} else {
						killf[kill_num].kill_group= hash_s (group_name);
					}
					kill_num++;
				}
			}
			write_kill_file ();
			
		kill_done:
			
#ifdef SIGTSTP
			if (do_sigtstp) {
				sigdisp (SIGTSTP, susp);
			}
#endif
			return (killed);
		}	
	}
	/* NOTREACHED */
}


/*
 * We assume that any articles which are tagged as killed are also
 * tagged as being read BECAUSE they were killed. So, we retag
 * them as being unread.
 */
 
int unkill_all_articles ()
{
	int unkilled = FALSE;
	register int i;

	for (i=0 ; i < top ; i++) {
		if (arts[i].killed) {
			arts[i].killed = FALSE;
			arts[i].unread = ART_UNREAD;
			unkilled = TRUE;
		}
	}
	num_of_killed_arts = 0;

	return (unkilled);
}

/*
 * Kill any articles in group active[index]
 */
 
int kill_any_articles (index)
	int index;	/* active[index].name gets groupname */
{
	char buf[LEN];
	int killed = FALSE;
	int run_ok = FALSE;
	int is_hot;
	long newsgroup_hash;
	register int i, j;

	if (! kill_num) {
		return (killed);
	}

	num_of_killed_arts = 0;
	num_of_hot_arts = 0;

	newsgroup_hash = hash_s (active[index].name);

	for (i=0 ; i < kill_num ; i++) {
		if (killf[i].kill_group == 0L ||
		    killf[i].kill_group == newsgroup_hash) {
			run_ok = TRUE;	
		}
	}
	if (! run_ok) {
		return (killed);
	}
	if (debug && ! update) {
		wait_message (txt_killing_arts);
	}
	for (i=0 ; i < top ; i++) {
		if (IS_READ(i) && kill_level == 0) {
			continue;
		}	
		for (j=0 ; j < kill_num ; j++) {
			if (killf[j].kill_group != 0L &&
			    killf[j].kill_group != newsgroup_hash)
				continue;

			is_hot = (killf[j].kill_how == K_HOT ? TRUE : FALSE);
			switch (killf[j].kill_type) {
			case KILL_SUBJ:
				if (STR_MATCH (arts[i].subject, killf[j].kill_subj)) {
					if (! is_hot) {
						SET_KILLED(i);
						num_of_killed_arts++;
					} else {
						SET_HOT(i);
						if (active[index].attribute.show_only_unread) {
							if (arts[i].unread == ART_UNREAD) {
								num_of_hot_arts++;
							}
						} else {
							num_of_hot_arts++;
						}
					}	
				}
				break;
			case KILL_FROM:
				if (arts[i].name != (char *) 0) {
					sprintf (buf, "%s (%s)", arts[i].from, arts[i].name);
				} else {
					strcpy (buf, arts[i].from);
				}
				if (STR_MATCH (buf, killf[j].kill_from)) {
					if (! is_hot) {
						SET_KILLED(i);
						num_of_killed_arts++;
					} else {
						SET_HOT(i);
						if (active[index].attribute.show_only_unread) {
							if (arts[i].unread == ART_UNREAD) {
								num_of_hot_arts++;
							}
						} else {
							num_of_hot_arts++;
						}
					}	
				}
				break;
			case KILL_BOTH:
				if (STR_MATCH (arts[i].subject, killf[j].kill_subj)) {
					if (! is_hot) {
						SET_KILLED(i);
						num_of_killed_arts++;
					} else {
						SET_HOT(i);
						if (active[index].attribute.show_only_unread) {
							if (arts[i].unread == ART_UNREAD) {
								num_of_hot_arts++;
							}
						} else {
							num_of_hot_arts++;
						}
					}	
					break;
				}
				if (arts[i].name != (char *) 0) {
					sprintf (buf, "%s (%s)", arts[i].from, arts[i].name);
				} else {
					strcpy (buf, arts[i].from);
				}

				if (STR_MATCH (buf, killf[j].kill_from)) {
					if (! is_hot) {
						SET_KILLED(i);
						num_of_killed_arts++;
					} else {
						SET_HOT(i);
						if (active[index].attribute.show_only_unread) {
							if (arts[i].unread == ART_UNREAD) {
								num_of_hot_arts++;
							}
						} else {
							num_of_hot_arts++;
						}
					}	
				}
				break;
			}
			if (IS_KILLED(i) || ! killed)
				killed = TRUE;
		}
	}
	return (killed);
}

/*
 * Auto select articles.
 * WARNING - this routinely is presently a kludge. It calls
 * kill_any_articles() which also kills articles. It also always returns
 * true in order to fake out the display code (cause it doesn't know
 * if any articles were actually selected)
 * The correct way to do this is to modify kill_any_articles() to take
 * another arg to specify whether killing, auto-selecting, or both is to be 
 * done, rename it to something else, and then have a new kill_any_articles()
 * and auto_select_articles() call this new routine with the appropriate 
 * arguments.
 */

int auto_select_articles (index)
	int index;
{
	kill_any_articles (index);
	return (TRUE);
}
