/*
 *  Project   : tin - a threaded Netnews reader
 *  Module    : misc.c
 *  Author    : I.Lea & R.Skrenta
 *  Created   : 01-04-91
 *  Updated   : 25-11-92
 *  Notes     :
 *  Copyright : (c) Copyright 1991-92 by Iain Lea & Rich Skrenta
 *              You may  freely  copy or  redistribute  this software,
 *              so  long as there is no profit made from its use, sale
 *              trade or  reproduction.  You may not change this copy-
 *              right notice, and it must be included in any copy made
 */

#include	"tin.h"

static char *mailbox_name = (char *) 0;
static int  mailbox_size;


void asfail (file, line, cond)
	char	*file;
	int	line;
	char	*cond;
{
  	fprintf (stderr, "%s: assertion failure: %s (%d): %s\n",
  		progname, file, line, cond);
  	fflush (stderr);
  	
 	/*
 	 * create a core dump
 	 */
#ifndef AMIGA 	 
#ifdef SIGABRT	
	sigdisp(SIGABRT, SIG_DFL);
 	kill (process_id, SIGABRT);
#else
#	ifdef SIGILL
		sigdisp(SIGILL, SIG_DFL);
 		kill (process_id, SIGILL);
#	else
#		ifdef SIGIOT
			sigdisp(SIGIOT, SIG_DFL);
		 	kill (process_id, SIGIOT);
#		endif
#	endif
#endif
#endif	/* AMIGA */
 
	exit(1);
}


void copy_fp (fp_ip, fp_op, prefix)
	FILE *fp_ip;
	FILE *fp_op;
	char *prefix;
{
	extern int errno;
	char buf[8192];
	int retcode;

	while (fgets (buf, sizeof (buf), fp_ip) != (char *) 0) {
		if (buf[0] != '\n') {
			retcode = fprintf (fp_op, "%s%s", prefix, buf);
		} else {
			retcode = fprintf (fp_op, "%s", buf);
		}
		if (retcode == EOF) {
			sprintf (msg, "Failed copy_fp(). errno=%d", errno);
			perror_message (msg, "");
#ifdef EPIPE
			if (errno == EPIPE) {
				return;
			}
#endif
#ifdef ENOSPC
			if (errno == ENOSPC) {
				return;
			}
#endif
		}
	}
}


char *get_val (env, def)
	char *env;		/* Environment variable we're looking for	*/
	char *def;		/* Default value if no environ value found	*/
{
	char *ptr;

	ptr = (char *) getenv(env);
	
	return (ptr != (char *) 0 ? ptr : def);
}


int invoke_editor (nam)
	char *nam;
{
	char buf[PATH_LEN];
	char *my_editor;
	static char editor[PATH_LEN];
	static int first = TRUE;

	if (first) {
		my_editor = (char *) getenv ("VISUAL");

		strcpy (editor, my_editor != NULL ? my_editor : get_val ("EDITOR", DEFAULT_EDITOR));
		first = FALSE;
	}

	if (start_editor_offset) {
		sprintf (buf, "%s +%d %s", editor, start_line_offset, nam);
	} else {
		sprintf (buf, "%s %s", editor, nam);
	}

	wait_message (buf);

	return invoke_cmd (buf);
}


int invoke_ispell (nam)
	char *nam;
{
#ifdef HAVE_ISPELL
	char buf[PATH_LEN];
	char *my_ispell;
	static char ispell[PATH_LEN];
	static int first = TRUE;

	if (first) {
		my_ispell = (char *) getenv ("ISPELL");

		strcpy (ispell, my_ispell != NULL ? my_ispell : "ispell -x");
		first = FALSE;
	}

	sprintf (buf, "%s %s", ispell, nam);

	wait_message (buf);

	return invoke_cmd (buf);
#else
	error_message (txt_ispell_define_not_compiled, "");
	return FALSE;
#endif
}


void shell_escape ()
{
	char shell[LEN];
	char *p;

#ifdef SIGTSTP
	sigtype_t (*susp)();
	susp = (sigtype_t (*)()) 0;
#endif

	sprintf (msg, txt_shell_escape, default_shell_command);
	
	if (! prompt_string (msg, shell))
		my_strncpy (shell, get_val ("SHELL", DEFAULT_SHELL), sizeof (shell));

	for (p = shell; *p && (*p == ' ' || *p == '\t'); p++)
		continue;

	if (*p) {
		my_strncpy (default_shell_command, p, sizeof (default_shell_command));
	} else {
		if (default_shell_command[0]) {
			my_strncpy (shell, default_shell_command, sizeof (shell));
		} else {
			my_strncpy (shell, get_val ("SHELL", DEFAULT_SHELL), sizeof (shell));
		}
		p = shell;
	}

	ClearScreen ();
	sprintf (msg, "Shell Command (%s)", p);
	center_line (0, TRUE, msg);
	MoveCursor (INDEX_TOP, 0);

	set_alarm_clock_off ();
	
	EndWin ();
	Raw (FALSE);

#ifdef SIGTSTP
	if (do_sigtstp)
		susp = signal (SIGTSTP, SIG_DFL);
#endif

	system (p);

#ifdef SIGTSTP
	if (do_sigtstp)
		signal (SIGTSTP, susp);
#endif

	Raw (TRUE);
	InitWin ();

	set_alarm_clock_on ();

	mail_setup ();

	continue_prompt ();

	if (draw_arrow_mark) {
		ClearScreen ();
	}
}


void tin_done (ret)
	int ret;
{
	char group_path[PATH_LEN];
	int ask = TRUE;
	register int i, j;
	
	/*
	 * check if any groups were read & ask if they should marked read
	 */
	if (catchup_read_groups && ! cmd_line) {
		for (i = 0 ; i < group_top ; i++) {
			if (active[my_group[i]].attribute.read_during_session) {
				if (ask) {
					if (prompt_yn (LINES, "Catchup all groups entered during this session? (y/n): ", 'n')) {
						ask = FALSE;
						default_thread_arts = FALSE;	/* speeds up index loading */
					} else {
						break;
					}
				}
				sprintf (msg, "Catchup %s...", active[my_group[i]].name);
				wait_message (msg);
				make_group_path (active[my_group[i]].name, group_path);
				if (index_group (active[my_group[i]].name, group_path)) {
					for (j = 0; j < top; j++) {
						arts[j].unread = ART_READ;
					}
					update_newsrc (active[my_group[i]].name, my_group[i], FALSE);
				}
			}
		}
	}
	write_mail_active_file ();
	nntp_close ();			/* disconnect from NNTP server */
	free_all_arrays ();		/* deallocate all arrays */
	ClearScreen ();
	EndWin ();
	Raw (FALSE);

	cleanup_tmp_files ();

	exit (ret);
}

#ifdef DONT_HAVE_MKDIR
mkdir (path, mode)
	char *path;
	int mode;
{
	char buf[LEN];
	struct stat sb;

	sprintf(buf, "mkdir %s", path);
	if (stat (path, &sb) == -1) {
		system (buf);
		chmod (path, mode);
	}
}
#endif

/*
 * hash group name for fast lookup later 
 */

long hash_groupname (group)
	char *group;
{
#ifdef NEW_HASH_METHOD	/* still testing */
	unsigned long hash = 0L, g, val;
	/* prime == smallest prime number greater than size of string table */
	int prime = 1423;
	char *p;

	for (p = group; *p; p++) {
		hash = (hash << 4) + *p;
		if (g = hash & 0xf0000000) {
			hash ^= g >> 24;
			hash ^= g;
		}
	}
	val = hash % prime;
/*
printf ("hash=[%s] [%ld]\n", group, val);
*/
	return val;
#else
	unsigned long hash_value = 0L;
	unsigned char *ptr = (unsigned char *) group;

	if (*ptr) {
		hash_value = *ptr++;

		while (*ptr)
			hash_value = ((hash_value << 1) ^ *ptr++) % TABLE_SIZE;
	}
	return (hash_value);
#endif
}


#ifdef AMIGA
/*
 * AmigaOS now has links. Better not to use them as not everybody has new ROMS
 */
 
void rename_file (old_filename, new_filename)
	char *old_filename;
	char *new_filename;
{
	char buf[1024];

	unlink (new_filename);
	if (rename (old_filename, new_filename)==EOF)
	{	sprintf (buf, txt_rename_error, old_filename, new_filename);
		perror_message (buf, "THREE");
	}
	return;
}
#else
void rename_file (old_filename, new_filename)
	char *old_filename;
	char *new_filename;
{	
	char buf[1024];
	FILE *fp_old, *fp_new;
	
	unlink (new_filename);
	
	if (link (old_filename, new_filename) == -1) {
		if (errno == EXDEV) {	/* create & copy file across filesystem */
			if ((fp_old = fopen (old_filename, "r")) == (FILE *) 0) {
				sprintf (buf, txt_cannot_open, old_filename);
				perror_message (buf, "ONE");
				return;
			}
			if ((fp_new = fopen (new_filename, "w")) == (FILE *) 0) {
				sprintf (buf, txt_cannot_open, new_filename);
				perror_message (buf, "ONE");
				return;
			}
			copy_fp (fp_old, fp_new, "");
			fclose (fp_new);	
			fclose (fp_old);	
			errno = 0;
		} else {
			sprintf (buf, txt_rename_error, old_filename, new_filename);
			perror_message (buf, "THREE");
			return;
		}	
	}
	if (unlink (old_filename) == -1) {
		sprintf (buf, txt_rename_error, old_filename, new_filename);
		perror_message (buf, "TWO");
		return;
	}
}
#endif	/* AMIGA */


char *str_dup (str)
	char *str;
{
	char *dup = (char *) 0;

	if (str) {
		dup = my_malloc (strlen (str)+1);
		strcpy (dup, str);
	}
	return dup;
}


int invoke_cmd (nam)
	char *nam;
{
	int ret;
#ifdef SIGTSTP
	sigtype_t (*susp)();
	susp = (sigtype_t (*)()) 0;
#endif

	set_alarm_clock_off ();

	EndWin ();
	Raw (FALSE);

#ifdef SIGTSTP
	if (do_sigtstp)
		susp = signal(SIGTSTP, SIG_DFL);
#endif

#if defined(SIGCHLD) && !defined(RS6000)
	system (nam);
	ret = system_status;
#else
	ret = system (nam);
#endif

#ifdef SIGTSTP
	if (do_sigtstp)
		signal (SIGTSTP, susp);
#endif

	Raw (TRUE);
	InitWin ();

	set_alarm_clock_on ();
	
	return ret == 0;
}


void draw_percent_mark (cur_num, max_num)
	int cur_num;
	int max_num;
{
	char buf[32];
	int percent = 0;

	if (NOTESLINES <= 0) {
		return;
	}

	if (cur_num <= 0 && max_num <= 0) {
		return;
	}
		
	percent = cur_num * 100 / max_num;
	sprintf (buf, "%s(%d%%) [%d/%d]", txt_more, percent, cur_num, max_num);
	MoveCursor (LINES, (COLS - (int) strlen (buf))-(1+BLANK_PAGE_COLS));
	StartInverse ();	
	fputs (buf, stdout);
	fflush (stdout);
	EndInverse ();
}

void set_real_uid_gid ()
{
#ifndef AMIGA
	if (local_index)
		return;

	umask (real_umask);
	
#ifdef HAVE_SETREUID
	if (setreuid (-1, real_uid) == -1) {
		perror_message ("Error setreuid(real) failed", "");
	}
	if (setregid (-1, real_gid) == -1) {
		perror_message ("Error setregid(real) failed", "");
	}
#else	
#  if defined(BSD) && ! defined(sinix)
#    ifdef sun
	if (seteuid (real_uid) == -1) {
		perror_message ("Error setreuid(real) failed", "");
	}
	if (setegid (real_gid) == -1) {
		perror_message ("Error setregid(real) failed", "");
	}
#    else
	if (setreuid (tin_uid, real_uid) == -1) {
		perror_message ("Error setreuid(real) failed", "");
	}
	if (setregid (tin_gid, real_gid) == -1) {
		perror_message ("Error setregid(real) failed", "");
	}
#    endif	/* sun */	
#  else
	if (setuid (real_uid) == -1) {
		perror_message ("Error setuid(real) failed", "");
	}
	if (setgid (real_gid) == -1) {
		perror_message ("Error setgid(real) failed", "");
	}
#  endif
#endif

#endif	/* AMIGA */
}


void set_tin_uid_gid ()
{
#ifndef AMIGA
	if (local_index)
		return;

	umask (0);

#ifdef HAVE_SETREUID
	if (setreuid (-1, tin_uid) == -1) {
		perror_message ("Error setreuid(tin) failed", "");
	}
	if (setregid (-1, tin_gid) == -1) {
		perror_message ("Error setregid(tin) failed", "");
	}
#else	
#  if defined(BSD) && ! defined(sinix)
#    ifdef sun
	if (seteuid (tin_uid) == -1) {
		perror_message ("Error setreuid(real) failed", "");
	}
	if (setegid (tin_gid) == -1) {
		perror_message ("Error setregid(real) failed", "");
	}
#    else
	if (setreuid (real_uid, tin_uid) == -1) {
		perror_message ("Error setreuid(tin) failed", "");
	}
	if (setregid (real_gid, tin_gid) == -1) {
		perror_message ("Error setregid(tin) failed", "");
	}
#    endif	/* sun */	
#  else
	if (setuid (tin_uid) == -1) {
		perror_message ("Error setuid(tin) failed", "");
	}
	if (setgid (tin_gid) == -1) {
		perror_message ("Error setgid(tin) failed", "");
	}
#  endif
#endif

#endif	/* AMIGA */
}


void basename (dirname, program)
	char *dirname;		/* argv[0] */
	char *program;		/* progname is returned */
{
	int i;
	
	strcpy (program, dirname);
	
	for (i=(int) strlen (dirname)-1 ; i ; i--) {
		if (dirname[i] == '/') {
			strcpy (program, dirname+(i+1));
			break;
		}
	}
}


/*
 *  Record size of mailbox so we can detect if new mail has arrived
 */

void mail_setup ()
{
	struct stat buf;

	mailbox_name = get_val ("MAIL", mailbox);

	if (stat (mailbox_name, &buf) >= 0) {
		mailbox_size = buf.st_size;
	} else {
		mailbox_size = 0;
	}
}

/*
 *  Return TRUE if new mail has arrived
 */

int mail_check ()
{
	struct stat buf;

	if (mailbox_name != (char *) 0 &&
		stat (mailbox_name, &buf) >= 0 &&
		mailbox_size < buf.st_size) {
		return TRUE;
	}

	return FALSE;
}

/*
 * Returns the user name and E-mail address of the user
 *
 * Written by ahd 15 July 1989
 * Borrowed from UUPC/extended with some mods by nms
 */

void parse_from (from_line, eaddr, fname)
	char*	from_line;
	char*	eaddr;
	char*	fname;
{
	char	*nonblank = NULL;
	char	name[LEN];		/* User full name */
	char	*nameptr = name;
	char	addr[LEN];		/* User e-mail address */
 	char	*addrptr  = addr;
  	char	state = 'A';		/* State = skip whitespace */
 	char	newstate = 'A';		/* Next state to process */
 	int	bananas = 0;		/* No () being processed now */
 
 	/*
 	 *   Begin loop to copy the input field into the address and the
 	 *   user name.  We will begin by copying both (ignoring whitespace
 	 *   for addresses) because we won't know if the input field is an
 	 *   address or a name until we hit either a special character of
 	 *   some sort.
 	 */
 
 	while ((*from_line != '\0') && (state != ',')) {
 
 		switch (state) {
 
 		case 'A':
 			if (isspace(*from_line)) /* Found first non-blank? */
 				break;           /* No --> keep looking */
 
 			nonblank = from_line;
 			state = 'B';
 			/* ... and fall through */
 
 		case 'B':
 		case ')':
 			newstate = *from_line;
 			switch (*from_line) {
 
 			case '(':
 				bananas++;
 				break;
 
 			case '"':
 				break;
 
 			case '<':
 				addrptr = addr;   /* Start address over */
 				nameptr = name;   /* Start name over again */
 				from_line  = nonblank - 1;
 
 				/* Re-scan in new state */
 
 				newstate = '>';   /* Proc all-non <> as name */
 				break;            /* Begin addr over again */
 
 			case ',':
 				break;            /* Terminates address */
 
 			case '>':
 			case ')':
 				strcpy(eaddr, "error@hell");
 				*fname = '\0';
 				return;
 
 			default:
 				newstate = state; /* stay in this state */
 				if (!isspace(*from_line))
 					*addrptr++ = *from_line;
 			}  /* switch(*from_line) */
 			break;
 
 		case '<':   
 			if (*from_line == '>')
 				newstate = '>';
 			else if (isspace(*from_line))
 				*nameptr++ = *from_line;
 			else
 				*addrptr++ = *from_line;
 			break;
 
 		case '>':   
 			if (*from_line == '<')
 				newstate = '<';
 			else
 				*nameptr++ = *from_line;
 			break;
 
 		case '(':   
 			if (*from_line == '(')
 				++bananas;
 			else if (*from_line == ')')
 				if (--bananas == 0) {
 					newstate = ')';
 					break;
 				}
 			*nameptr++ = *from_line;
 			break;
 
 		case '"':   
 			if (*from_line == '"')
 				newstate = ')';
 			else
 				*nameptr++ = *from_line;
 			break;
 
 		default:    
 
 			/* Logic error, bad state */
 
 			strcpy(eaddr, "error@nowhere");
 			*fname = '\0';
 			return;
 		}  /* switch (state) */
 		state = newstate;
 		from_line++;
 	} /* while */
 
 	*addrptr = '\0';
 	*nameptr = '\0';
 
 	if (state == 'A') {
 		strcpy(eaddr, "nobody@nowhere");
 		*fname = '\0';
 		return;
  	}
  
 	strcpy(eaddr, addr);         /* Return the full address */
 	if (state == 'B')
 		strcpy(fname, "");
 	else {
 		while (--nameptr >= name) {
 			if (isspace(*nameptr) || (*nameptr == '"'))
 				*nameptr = '\0';
 			else
 				break;
  		}
  
 		/* Strip leading blanks from the address */
 
 		nameptr = name;
 		while ( *(nameptr) != '\0') {
 			if (!(isspace(*nameptr) || (*nameptr == '"')))
 				break;
 			else
 				nameptr++;
 		}
 		strcpy(fname, nameptr);
 	}
}

/*
 *  Convert a string to a long, only look at first n characters
 */

long my_atol (s, n)
	char *s;
	int n;
{
	long ret = 0;

	while (*s && n--) {
		if (*s >= '0' && *s <= '9')
			ret = ret * 10 + (*s - '0');
		else
			return -1;
		s++;
	}

	return ret;
}

/*
 *  strcmp that ignores case
 */

#define FOLD_TO_UPPER(a)	(islower ((int) (a)) ? toupper ((int) (a)) : (a))

int my_stricmp (p, q)
	char *p;
	char *q;
{
	for (; FOLD_TO_UPPER (*p) == FOLD_TO_UPPER (*q); ++p, ++q) {
		if (*p == '\0') {
			return (0);
		}
	}		

	return (FOLD_TO_UPPER (*p) - FOLD_TO_UPPER (*q));
}

/*
 *  Return a pointer into s eliminating any leading Re:'s.  Example:
 *
 *	  Re: Reorganization of misc.jobs
 *	  ^   ^
 */

char *eat_re (s)
	char *s;
{

	while (*s == 'r' || *s == 'R') {
		if ((*(s+1) == 'e' || *(s+1) == 'E')) {
			if (*(s+2) == ':')
				s += 3;
			else if (*(s+2) == '^' && isdigit(*(s+3)) && *(s+4) == ':')
				s += 5;			/* hurray nn */
			else
				break;
		} else
			break;
		while (*s == ' ')
			s++;
	}

	return s;
}

/*
 *  Hash the subjects (after eating the Re's off) for a quicker
 *  thread search later.  We store the hashes for subjects in the
 *  index file for speed.
 */

long hash_s (s)
	char *s;
{
	long h = 0;
	unsigned char *t = (unsigned char *) s;

	while (*t)
		h = h * 64 + *t++;

	return h;
}

/*
 *  strncpy that stops at a newline and null terminates
 */

void my_strncpy (p, q, n)
	char *p;
	char *q;
	int n;
{
	while (n--) {
		if (! *q || *q == '\n')
			break;
		*p++ = *q++;
	}
	*p = '\0';
}


int untag_all_articles ()
{
	int untagged = FALSE;
	register int i;

	for (i=0 ; i < top ; i++) {
		if (arts[i].tagged) {
			arts[i].tagged = FALSE;
			untagged = TRUE;
		}
	}
	num_of_tagged_arts = 0;

	return (untagged);
}


/*
 * ANSI C strstr () - Uses Boyer-Moore algorithm.
 */
 
char *str_str (text, pattern, patlen)
	char *text;
	char *pattern;
	int patlen;
{
	register unsigned char *p, *t;
	register int i, p1, j, *delta;
	int deltaspace[256];
	int textlen;

	textlen = strlen (text);

	/* algorithm fails if pattern is empty */
	if ((p1 = patlen) == 0)
		return (text);

	/* code below fails (whenever i is unsigned) if pattern too long */
	if (p1 > textlen)
		return (NULL);

	/* set up deltas */
	delta = deltaspace;
	for (i = 0; i <= 255; i++)
		delta[i] = p1;
	for (p = (unsigned char *) pattern, i = p1; --i > 0;)
		delta[*p++] = i;

	/*
	 * From now on, we want patlen - 1.
	 * In the loop below, p points to the end of the pattern,
	 * t points to the end of the text to be tested against the
	 * pattern, and i counts the amount of text remaining, not
	 * including the part to be tested.
	 */
	p1--;
	p = (unsigned char *) pattern + p1;
	t = (unsigned char *) text + p1;
	i = textlen - patlen;
	for (;;) {
		if (*p == *t && memcmp ((p - p1), (t - p1), p1) == 0)
			return ((char *)t - p1);
		j = delta[*t];
		if (i < j)
			break;
		i -= j;
		t += j;
	}
	return (NULL);
}


void get_author (thread, respnum, str)
	int thread;
	int respnum;
	char *str;
{	
	extern int threaded_on_subject;
	int author;

	if (thread) {
		if (threaded_on_subject) {
			author = SHOW_FROM_BOTH;
		} else {
			author = show_author;
		}
	} else {
		author = show_author;
	} 
	
	switch (author) { 
		case SHOW_FROM_NONE:
			str[0] = '\0';
			break;
		case SHOW_FROM_ADDR:
			strcpy (str, arts[respnum].from);
			break;
		case SHOW_FROM_NAME:
			if (arts[respnum].name) {
				strcpy (str, arts[respnum].name);
			} else {
				strcpy (str, arts[respnum].from);
			}
			break;
		case SHOW_FROM_BOTH:
			if (arts[respnum].name) { 
				sprintf (str, "%s (%s)", arts[respnum].name, arts[respnum].from);
			} else { 
				strcpy (str, arts[respnum].from);
			}
			break;
	}
}


void toggle_inverse_video ()
{
	inverse_okay = !inverse_okay;
	if (inverse_okay) {
#ifndef USE_INVERSE_HACK	
		draw_arrow_mark = FALSE;
#endif		
		info_message (txt_inverse_on);
	} else {
		draw_arrow_mark = TRUE;
		info_message (txt_inverse_off);
	}
}


int get_arrow_key ()
{
	int ch;
	
	ch = ReadCh ();
	if (ch == '[' || ch == 'O')
		ch = ReadCh();
	switch (ch) {
		case 'A':
		case 'D':
		case 'i':
			return KEYMAP_UP;

		case 'B':
		case 'C':
			return KEYMAP_DOWN;

		case 'I':		/* ansi  PgUp */
		case 'V':		/* at386 PgUp */
		case 'S':		/* 97801 PgUp */
		case 'v':		/* emacs style */
			return KEYMAP_PAGE_UP;

		case 'G':		/* ansi  PgDn */
		case 'U':		/* at386 PgDn */
		case 'T':		/* 97801 PgDn */
			return KEYMAP_PAGE_DOWN;

		case 'H':		/* at386  Home */
			return KEYMAP_HOME;
					
		case 'F':		/* ansi   End */
		case 'Y':		/* at386  End */
			return KEYMAP_END;

		case '5':		/* vt200 PgUp */
			ch = ReadCh ();	/* eat the ~  */
			return KEYMAP_PAGE_UP;

		case '6':		/* vt200 PgUp */
			ch = ReadCh ();	/* eat the ~  */
			return KEYMAP_PAGE_DOWN;

		case '1':		/* vt200 PgUp */
			ch = ReadCh ();	/* eat the ~  */
			return KEYMAP_HOME;
					
		case '4':		/* vt200 PgUp */
			ch = ReadCh ();	/* eat the ~  */
			return KEYMAP_END;

		default:
			return KEYMAP_UNKNOWN;
	}
}

/*
 * Check for lock file to stop multiple copies of tind or tin -U running 
 * and if it does not exist create it so this is the only copy running
 */

void create_index_lock_file (lock_file)
	char *lock_file;
{
	char buf[64];
	FILE *fp;
	long epoch;
	struct stat sb;

	if (stat (lock_file, &sb) == 0) {
		if ((fp = fopen (lock_file, "r")) != (FILE *) 0) {
			fgets (buf, sizeof (buf), fp);
			fclose (fp);
#ifdef INDEX_DAEMON
			sprintf (msg, "%s: Already started pid=[%d] on %s", 
				progname, atoi(buf), buf+8);
#else
			sprintf (msg, "\n%s: Already started pid=[%d] on %s", 
				progname, atoi(buf), buf+8);
#endif
			error_message (msg, "");
			exit (1);
		}
	} else 	if ((fp = fopen (lock_file, "w")) != (FILE *) 0) {
		time (&epoch);
		fprintf (fp, "%6d  %s\n", process_id, ctime (&epoch));
		fclose (fp);
		chmod (lock_file, 0600);
	}
}

/* 
 * strfquote - produce formatted quote 
 */

int strfquote (group, respnum, s, maxsize, format)
	char *group;
	int respnum;
	char *s;
	int maxsize;
	char *format;
{
	extern char note_h_date[PATH_LEN];
	extern char note_h_messageid[PATH_LEN];
	char *endp = s + maxsize;
	char *start = s;
	char tbuf[PATH_LEN];
	int i;

	if (s == (char *) 0 || format == (char *) 0 || maxsize == 0) {
		return 0;
	}

	if (strchr (format, '%') == (char *) 0 && strlen (format) + 1 >= maxsize) {
		return 0;
	}

	for (; *format && s < endp - 1; format++) {
		tbuf[0] = '\0';

		if (*format != '\\' && *format != '%') {
			*s++ = *format;
			continue;
		}

		if (*format == '\\') {
			switch (*++format) {
				case '\0':
					*s++ = '\\';
					goto out;
				case 'n':	/* linefeed */
					strcpy (tbuf, "\n");
					break;
				default:
					tbuf[0] = '%';
					tbuf[1] = *format;
					tbuf[2] = '\0';
					break;
			}
			i = strlen(tbuf);
			if (i) {
				if (s + i < endp - 1) {
					strcpy (s, tbuf);
					s += i;
				} else {
					return 0;
				}
			}
		}
		if (*format == '%') {
			switch (*++format) {
				case '\0':
					*s++ = '%';
					goto out;
				case '%':
					*s++ = '%';
					continue;
				case 'A':	/* Articles Email address */
					strcpy (tbuf, arts[respnum].from);
					break;
				case 'D':	/* Articles Date */
					strcpy(tbuf, note_h_date);
					break;
				case 'F':	/* Articles Address+Name */
					if (arts[respnum].name) {
						sprintf (tbuf, "%s (%s)",
							arts[respnum].name,
							arts[respnum].from);
					} else {
						strcpy (tbuf, arts[respnum].from);
					}
					break;
				case 'G':	/* Groupname of Article */
					strcpy (tbuf, group);
					break;
				case 'M':	/* Articles MessageId */
					strcpy (tbuf, note_h_messageid);
					break;
				case 'N':	/* Articles Name of author */
					strcpy (tbuf, arts[respnum].name);
					break;
				default:
					tbuf[0] = '%';
					tbuf[1] = *format;
					tbuf[2] = '\0';
					break;
			}
			i = strlen(tbuf);
			if (i) {
				if (s + i < endp - 1) {
					strcpy (s, tbuf);
					s += i;
				} else {
					return 0;
				}
			}
		}
	}	
out:
	if (s < endp && *format == '\0') {
		*s = '\0';
		return (s - start);
	} else
		return 0;
}

/*
 * strfpath - produce formatted pathname expansion. Handles following forms:
 *   ~/News    -> /usr/iain/News
 *   ~abc/News -> /usr/abc/News
 *   $var/News -> /env/var/News
 *   =file     -> /usr/iain/Mail/file
 *   +file     -> /usr/iain/News/group.name/file
 *   ~/News/%G -> /usr/iain/News/group.name
 */

int strfpath (format, str, maxsize, homedir, maildir, savedir, group)
	char *format;
	char *str;
	int maxsize;
	char *homedir;
	char *maildir;
	char *savedir;
	char *group;
{
	char *endp = str + maxsize;
	char *start = str;
	char *envptr;
	char *startp = format;
	char buf[PATH_LEN];
	char tbuf[PATH_LEN];
	char tmp[PATH_LEN];
	int i;
#ifndef AMIGA	
	struct passwd *pwd;
#endif

	if (str == (char *) 0 || format == (char *) 0 || maxsize == 0) {
		return 0;
	}

	if (strlen (format) + 1 >= maxsize) {
		return 0;
	}

	for (; *format && str < endp - 1; format++) {
		tbuf[0] = '\0';

		/*
		 * If just a normal part of the pathname copy it
		 */
		if (! strchr ("~$=+", *format)) {
			*str++ = *format;
			continue;
		}

		switch (*format) {
			case '~':	/* Users or another users homedir */
				switch (*++format) {
					case '/':	/* users homedir */
						sprintf (tbuf, "%s/", homedir);
						break;
					default:	/* some other users homedir */
#ifndef AMIGA
						i = 0;
						while (*format && *format != '/') {
							tbuf[i++] = *format++;
						}
						tbuf[i] = '\0';
						/*
						 * OK lookup the username in/etc/passwd 
						 */
						pwd = getpwnam (tbuf);
						if (pwd == (struct passwd *) 0) {
							str[0] = '\0';
							return 0;
						} else {
							sprintf (tbuf, "%s/", pwd->pw_dir);
						}
#else
						/* Amiga has no ther users */
						return 0;
#endif
						break;
				}
				i = strlen (tbuf);
				if (i) {
					if (str + i < endp - 1) {
						strcpy (str, tbuf);
						str += i;
					} else {
						str[0] = '\0';
						return 0;
					}
				}
				break;
			case '$':	/* Read the envvar and use its value */
				i = 0;
				format++;
				while (*format && *format != '/') {
					tbuf[i++] = *format++;
				}
				tbuf[i] = '\0';
				if (*format == '/') {
					format--;
				}
				/*
				 * OK lookup the variable in the shells environment
				 */
				envptr = (char *) getenv (tbuf);
				if (envptr == (char *) 0) {
					str[0] = '\0';
					return 0;
				} else {
					strncpy (tbuf, envptr, sizeof (tbuf)-1);
				}
				i = strlen (tbuf);
				if (i) {
					if (str + i < endp - 1) {
						strcpy (str, tbuf);
						str += i;
					} else {
						str[0] = '\0';
						return 0;
					}
				}
				break;
			case '=':	
				/* 
				 * Shorthand for group maildir 
				 * Only convert if 1st char in format
				 */
				if (startp == format && maildir != (char *) 0) {
					sprintf (tbuf, "%s/", maildir);					
					i = strlen (tbuf);
					if (i) {
						if (str + i < endp - 1) {
							strcpy (str, tbuf);
							str += i;
						} else {
							str[0] = '\0';
							return 0;
						}
					}
				} else {
					*str++ = *format;
				}
				break;
			case '+':
				/* 
				 * Shorthand for saving to savedir/groupname/file
				 * Only convert if 1st char in format
				 */
				if (startp == format && savedir != (char *) 0) {
					if (strfpath (savedir, buf, sizeof (buf), homedir,
					    (char *) 0, (char *) 0, (char *) 0)) {

#ifdef HAVE_LONG_FILENAMES 
						my_strncpy (tmp, group, sizeof (tmp));
#else
						my_strncpy (tmp, group, 14);
#endif
						/*
						 *  convert 1st letter to uppercase
						 */
						if (tmp[0] >= 'a' && tmp[0] <= 'z') {
							tmp[0] = tmp[0] - 32;
						}
						sprintf (tbuf, "%s/%s/", buf, tmp);
						i = strlen (tbuf);
						if (i) {
							if (str + i < endp - 1) {
								strcpy (str, tbuf);
								str += i;
							} else {
								str[0] = '\0';
								return 0;
							}
						}
					} else {
						str[0] = '\0';
						return 0;
					}
				} else {
					*str++ = *format;
				}
				break;
			case '%':	/* Different forms of parsing cmds */
				*str++ = *format;
				break;
			default:
				break;
		}
	}	

	if (str < endp && *format == '\0') {
		*str = '\0';
/*
clear_message ();
printf ("!!! format=[%s]  path=[%s]", startp, start);
fflush (stdout);
sleep (2);
*/
		return (str - start);
	} else {
		str[0] = '\0';
		return 0;
	}
}


void get_cwd (buf)
	char *buf;
{
#ifdef DONT_HAVE_GETCWD
	getwd (buf);
#else
	getcwd (buf, PATH_LEN);
#endif
}


void make_group_path (name, path)
	char *name;
	char *path;
{
	char *ptr;
	
	strcpy (path, name);
	
	ptr = path;
	
	while (*ptr) {
		if (*ptr == '.') {
			*ptr = '/';
		}
		ptr++;
	}
}

/*
 * Delete tmp index & local newsgroups file
 */

void cleanup_tmp_files ()
{
	extern char index_file[PATH_LEN];

	if (read_news_via_nntp && xindex_supported) {
		unlink (index_file);
	}
	unlink (local_newsgroups_file);
	unlink (lock_file);
}
