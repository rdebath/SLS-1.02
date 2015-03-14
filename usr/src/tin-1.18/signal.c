/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : signal.c
 *  Author    : I.Lea
 *  Created   : 01-04-91
 *  Updated   : 21-11-92
 *  Notes     : signal handlers for different modes and window resizing
 *  Copyright : (c) Copyright 1991-92 by Iain Lea
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

extern char *glob_art_group;
extern char *glob_group;
extern char *glob_page_group;
extern char index_file[PATH_LEN];
extern int glob_respnum;

static int time_remaining;

#ifdef SIGTSTP
int do_sigtstp = 0;
#endif


#ifdef HAVE_POSIX_JC

/*
 * for POSIX systems we know SIGTYPE is void 
 */
 
void (*sigdisp(sig, func))()
	int sig;
	void (*func)();
{
	struct sigaction sa, osa;

	sa.sa_handler = func;
	sigemptyset (&sa.sa_mask);
	sa.sa_flags = 0;
#ifdef SA_RESTART
	sa.sa_flags |= SA_RESTART;
#endif
	if (sigaction (sig, &sa, &osa) < 0) {
#ifdef DONT_PROTOTYPE_PTR_TO_FUNC
		return ((void (*) ()) (-1));
#else
		return ((void (*) (int)) (-1));
#endif		
	}
	return (osa.sa_handler);
}

#else

sigtype_t (*sigdisp(sig, func))()
	int sig;
	sigtype_t (*func)();
{
	return (signal (sig, func));
}

#endif

void set_signal_handlers ()
{
#ifdef SIGINT
	signal (SIGINT, signal_handler);	/* ctrl-C */
#endif
#ifdef SIGQUIT
	signal (SIGQUIT, signal_handler);	/* ctrl-\ */
#endif
#ifdef SIGHUP
	signal (SIGHUP, signal_handler);	/* hangup */
#endif
#ifdef SIGILL
	signal (SIGILL, signal_handler);	/* illegal instruction */
#endif
#ifdef SIGBUS
	signal (SIGBUS, signal_handler);	/* bus error */
#endif
#ifdef SIGSEGV
	signal (SIGSEGV, signal_handler);	/* segmentation violation */
#endif
#ifdef SIGPIPE
	signal (SIGPIPE, SIG_IGN);
#endif
#ifdef SIGCHLD
	signal (SIGCHLD, signal_handler);	/* death of a child process */
#endif
#ifdef SIGPWR
	signal (SIGPWR, signal_handler);	/* powerfail */
#endif	
#ifdef SIGWINCH
	if (debug == 2) {
		wait_message ("SIGWINCH setting signal...");
		sleep (2);
	}
	signal (SIGWINCH, main_resize);
#endif

#if defined(SIGTSTP) && ! defined(MINIX)
	{
		sigtype_t (*ptr)();
		ptr = signal (SIGTSTP, SIG_DFL);
		signal (SIGTSTP, ptr);
		if (ptr != SIG_IGN) {
			/*
			 *  SIGTSTP is ignored when starting from shells
			 *  without job-control
			 */
			do_sigtstp = 1; 
			signal (SIGTSTP, main_suspend);
		}
	}
#endif
}


void set_alarm_signal ()
{
#ifndef DONT_REREAD_ACTIVE_FILE
	/*
	 * Only reread active file if news is not static (ie. CD-ROM)
	 */
	(void) alarm (0);
	if (strcmp (spooldir_alias, "news") == 0) {
		signal (SIGALRM, signal_handler);
		alarm (reread_active_file_secs);
	}	
	reread_active_file = FALSE;
#endif
}


void set_alarm_clock_on ()
{
#ifndef DONT_REREAD_ACTIVE_FILE
	alarm (time_remaining);
#endif
}


void set_alarm_clock_off ()
{
#ifndef DONT_REREAD_ACTIVE_FILE
	time_remaining = alarm (0);
#endif
}


void signal_handler (sig)
	int sig;
{
	char *sigtext;
#ifdef SIGCHLD			
	int wait_status = 1;
#endif
	
	switch (sig) {
#ifdef SIGINT
		case SIGINT:
			if (update) {
				sigtext = "SIGINT ";
			} else {
				signal (SIGINT, signal_handler);
				return;
			}
			break;
#endif
#ifdef SIGQUIT
		case SIGQUIT:
			sigtext = "SIGQUIT ";
			break;
#endif
#ifdef SIGHUP
		case SIGHUP:
			sigtext = "SIGHUP ";
			break;
#endif
#ifdef SIGCHLD			
		case SIGCHLD:
			wait (&wait_status);
#	ifdef WEXITSTATUS			
			system_status = WEXITSTATUS(wait_status);
#	endif			
			return;
#endif			
#ifdef SIGPWR			
		case SIGPWR:
			sigtext = "SIGPWR ";
			break;
#endif			
#ifdef SIGBUS
		case SIGBUS:
			sigtext = "SIGBUS ";
			break;
#endif
#ifdef SIGSEGV
		case SIGSEGV:
			sigtext = "SIGSEGV ";
			break;
#endif
#if defined(SIGALRM) && !defined(DONT_REREAD_ACTIVE_FILE)
		case SIGALRM:
			set_alarm_signal ();
			reread_active_file = TRUE;
			return;
#endif
		default:
			sigtext = "";
			break;
	}
	Raw (FALSE);
	EndWin ();
	fprintf (stderr, "\n%s: signal handler caught signal %s(%d).\n",
		progname, sigtext, sig);
#if defined(SIGBUS) || defined(SIGSEGV)
	if (
#	ifdef SIGBUS
		sig == SIGBUS
#	endif
#	if defined(SIGBUS) && defined(SIGSEGV)
		||
#	endif		
#	ifdef SIGSEGV
		sig == SIGSEGV
#	endif
		) {
		fprintf (stderr, "%s: send a bug report to %s%s\n",
			progname, BUG_REPORT_ADDRESS, add_addr);
	}
#endif
	fflush (stderr);

	cleanup_tmp_files (); 

	exit (1);
}


int set_win_size (num_lines, num_cols)
	int *num_lines;
	int *num_cols;
{
	int	old_lines;
	int	old_cols;
#ifdef TIOCGSIZE
	struct ttysize win;
#else
#  ifdef TIOCGWINSZ
	struct winsize win;
#  endif
#endif

	old_lines = *num_lines;
	old_cols = *num_cols;

	init_screen_array (FALSE);		/* deallocate screen array */

#ifdef TIOCGSIZE
	if (ioctl (0, TIOCGSIZE, &win) == 0) {
		if (win.ts_lines != 0) {
			*num_lines = win.ts_lines - 1;
		}
		if (win.ts_cols != 0) {
			*num_cols = win.ts_cols;
		}
	}
#else
#  ifdef TIOCGWINSZ
	if (ioctl (0, TIOCGWINSZ, &win) == 0) {
		if (win.ws_row != 0) {
			*num_lines = win.ws_row - 1;
		}
		if (win.ws_col != 0) {
			*num_cols = win.ws_col;
		}
	}
#  endif
#endif

	init_screen_array (TRUE);		/* allocate screen array for resize */

	set_subj_from_size (*num_cols);

	RIGHT_POS = *num_cols - 20;
	MORE_POS  = *num_cols - 15;
	if (beginner_level) {
		NOTESLINES = *num_lines - INDEX_TOP - MINI_HELP_LINES;
	} else {
		NOTESLINES = *num_lines - INDEX_TOP - 1;
	}
	if (NOTESLINES <= 0) {
		NOTESLINES = 1;
	}

	if (*num_lines != old_lines || *num_cols != old_cols) {
		return TRUE;
	} else {	
		return FALSE;
	}	
}



void set_signals_art ()
{
#ifdef SIGTSTP
	if (do_sigtstp) {
		sigdisp (SIGTSTP, art_suspend);
	}
#endif

#ifdef SIGWINCH
	signal (SIGWINCH, art_resize);
#endif
}


void set_signals_group ()
{
#ifdef SIGTSTP
	if (do_sigtstp) {
		sigdisp (SIGTSTP, group_suspend);
	}
#endif

#ifdef SIGWINCH
	signal (SIGWINCH, group_resize);
#endif
}


void set_signals_help ()
{
#ifdef SIGTSTP
	if (do_sigtstp) {
		sigdisp (SIGTSTP, help_suspend);
	}
#endif

#ifdef SIGWINCH
	signal (SIGWINCH, help_resize);
#endif
}


void set_signals_page ()
{
#ifdef SIGTSTP
	if (do_sigtstp) {
		sigdisp (SIGTSTP, page_suspend);
	}
#endif

#ifdef SIGWINCH
	signal (SIGWINCH, page_resize);
#endif
}


void set_signals_select ()
{
#ifdef SIGTSTP
	if (do_sigtstp) {
		sigdisp (SIGTSTP, select_suspend);
	}
#endif

#ifdef SIGWINCH
	signal (SIGWINCH, select_resize);
#endif
}


void set_signals_spooldir ()
{
#ifdef SIGTSTP
	if (do_sigtstp) {
		sigdisp (SIGTSTP, spooldir_suspend);
	}
#endif

#ifdef SIGWINCH
	signal (SIGWINCH, spooldir_resize);
#endif
}


void set_signals_thread ()
{
#ifdef SIGTSTP
	if (do_sigtstp) {
		sigdisp (SIGTSTP, thread_suspend);
	}
#endif

#ifdef SIGWINCH
	signal (SIGWINCH, thread_resize);
#endif
}


#ifdef SIGTSTP

/* ARGSUSED0 */
void art_suspend (sig)
	int sig;
{
	set_keypad_off ();
	Raw (FALSE);
	wait_message (txt_suspended_message);

	kill (0, SIGSTOP);

	sigdisp (SIGTSTP, art_suspend);

	if (! update) {
		Raw (TRUE);
		art_resize (0);		
	}
	set_keypad_on ();
}


/* ARGSUSED0 */
void main_suspend (sig)
	int sig;
{
	set_keypad_off ();
	Raw (FALSE);
	wait_message (txt_suspended_message);

	kill (0, SIGSTOP);

	sigdisp (SIGTSTP, main_suspend);

	if (! update) {
		Raw (TRUE);
		main_resize (0);		
	}
	set_keypad_on ();
}


/* ARGSUSED0 */
void select_suspend (sig)
	int sig;
{
	set_keypad_off ();
	Raw (FALSE);
	wait_message (txt_suspended_message);

	kill (0, SIGSTOP);

	sigdisp (SIGTSTP, select_suspend);

	if (! update) {
		Raw (TRUE);
		select_resize (0);
	}
	set_keypad_on ();
}


/* ARGSUSED0 */
void spooldir_suspend (sig)
	int sig;
{
	set_keypad_off ();
	Raw (FALSE);
	wait_message (txt_suspended_message);

	kill (0, SIGSTOP);

	sigdisp (SIGTSTP, spooldir_suspend);

	if (! update) {
		Raw (TRUE);
		spooldir_resize (0);
	}
	set_keypad_on ();
}


/* ARGSUSED0 */
void group_suspend (sig)
	int sig;
{
	set_keypad_off ();
	Raw (FALSE);
	wait_message (txt_suspended_message);

	kill (0, SIGSTOP);

	sigdisp (SIGTSTP, group_suspend);

	if (! update) {
		Raw (TRUE);
		group_resize (0);		
	}
	set_keypad_on ();
}


/* ARGSUSED0 */
void help_suspend (sig)
	int sig;
{
	set_keypad_off ();
	Raw (FALSE);
	wait_message (txt_suspended_message);

	kill (0, SIGSTOP);

	sigdisp (SIGTSTP, help_suspend);

	if (! update) {
		Raw (TRUE);
		help_resize (0);		
	}
	set_keypad_on ();
}


/* ARGSUSED0 */
void page_suspend (sig)
	int sig;
{
	set_keypad_off ();
	Raw (FALSE);
	wait_message (txt_suspended_message);

	kill (0, SIGSTOP);

	sigdisp (SIGTSTP, page_suspend);

	if (! update) {
		Raw (TRUE);
		page_resize (0);		
	}
	set_keypad_on ();
}


/* ARGSUSED0 */
void thread_suspend (sig)
	int sig;
{
	set_keypad_off ();
	Raw (FALSE);
	wait_message (txt_suspended_message);

	kill (0, SIGSTOP);

	sigdisp (SIGTSTP, thread_suspend);

	if (! update) {
		Raw (TRUE);
		thread_resize (0);		
	}	
	set_keypad_on ();
}


/* ARGSUSED0 */
void rcfile_suspend (sig)
	int sig;
{
	set_keypad_off ();
	Raw (FALSE);
	wait_message (txt_suspended_message);

	kill (0, SIGSTOP);

	sigdisp (SIGTSTP, rcfile_suspend);

	Raw (TRUE);
	set_keypad_on ();
	show_rcfile_menu ();	
}

#endif /* SIGTSTP */	


/* ARGSUSED0 */
void art_resize (sig)
	int sig;
{
	char buf[LEN];

#ifdef SIGWINCH
	(void) set_win_size (&LINES, &COLS);
	signal (SIGWINCH, art_resize);
#endif
	mail_setup ();
	ClearScreen ();
	sprintf (buf, txt_group, glob_art_group);
	wait_message (buf);
}


/* ARGSUSED0 */
void main_resize (sig)
	int sig;
{
#ifdef SIGWINCH
	(void) set_win_size (&LINES, &COLS);
	signal (SIGWINCH, main_resize);
#endif
	mail_setup ();
}


/* ARGSUSED0 */
void select_resize (sig)
	int sig;
{
	int resized = TRUE;
	
#ifdef SIGWINCH
	resized = set_win_size (&LINES, &COLS);
	signal (SIGWINCH, select_resize);
#endif
	
#ifndef USE_CLEARSCREEN
	ClearScreen ();
#endif
	mail_setup ();
	if (resized || sig == 0) {
		group_selection_page ();
	}	
}


/* ARGSUSED0 */
void spooldir_resize (sig)
	int sig;
{
	int resized = TRUE;
	
#ifdef SIGWINCH
	resized = set_win_size (&LINES, &COLS);
	signal (SIGWINCH, spooldir_resize);
#endif
	
#ifndef USE_CLEARSCREEN
	ClearScreen ();
#endif
	mail_setup ();
	if (resized || sig == 0) {
		show_spooldir_page ();
	}
}


/* ARGSUSED0 */
void group_resize (sig)
	int sig;
{
	int resized = TRUE;
	
#ifdef SIGWINCH
	resized = set_win_size (&LINES, &COLS);
	signal (SIGWINCH, group_resize);
#endif
	
#ifndef USE_CLEARSCREEN
	ClearScreen ();
#endif
	mail_setup ();
	if (resized || sig == 0) {
		show_group_page ();
	}	
}


/* ARGSUSED0 */
void help_resize (sig)
	int sig;
{
	int resized = TRUE;
	
#ifdef SIGWINCH
	resized = set_win_size (&LINES, &COLS);
	signal (SIGWINCH, help_resize);
#endif
	
	if (resized || sig == 0) {
		display_info_page ();
	}
}

/* ARGSUSED0 */
void page_resize (sig)
	int sig;
{
	int resized = TRUE;
	
#ifdef SIGWINCH
	resized = set_win_size (&LINES, &COLS);
	signal (SIGWINCH, page_resize);
#endif
	
#ifndef USE_CLEARSCREEN
	ClearScreen ();
#endif
	mail_setup ();
	if (resized || sig == 0) {
		redraw_page (glob_respnum, glob_page_group);
	}	
}


/* ARGSUSED0 */
void thread_resize (sig)
	int sig;
{
	int resized = TRUE;
	
#ifdef SIGWINCH
	resized = set_win_size (&LINES, &COLS);
	signal (SIGWINCH, thread_resize);
#endif
	
#ifndef USE_CLEARSCREEN
	ClearScreen ();
#endif
	mail_setup ();
	if (resized || sig == 0) {
		show_thread_page ();
	}	
}
