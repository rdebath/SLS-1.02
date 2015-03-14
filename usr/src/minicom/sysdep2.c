/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 *
 * sysdep2.c - system dependant routines
 *
 * getrowcols	- get number of columns and rows from the environment.
 * setcbreak	- set tty mode to raw, cbreak or normal.
 * strtok	- for systems that don't have it
 * dup2		- for ancient systems like SVR2
 */
#include <sys/types.h>
#ifdef _POSIX_SOURCE
#  include <stdlib.h>
#  include <unistd.h>
#else
  char *getenv();
#endif
#ifdef _SYSV
#  include <termio.h>
#endif
#if defined (_BSD43) || defined (_SYSV)
#  include <sys/ioctl.h>
#endif
#ifdef _V7
#  include <sgtty.h>
#endif
#ifdef _SVR2
#include <errno.h>
#endif

void getrowcols(rows, cols)
int *rows;
int *cols;
{
#ifdef TIOCGWINSZ
	struct winsize ws;

	if (ioctl(0, TIOCGWINSZ, &ws) < 0) {
		*rows = 0;
		*cols = 0;
	} else {
		*rows = ws.ws_row;
		*cols = ws.ws_col;
	}	
#else
#  ifdef TIOCGSIZE
	struct ttysize ws;

	if (ioctl(0, TIOCGSIZE, &ws) < 0) {
		*rows = 0;
		*cols = 0;
	} else {
		*rows = ws.ts_lines;
		*cols = ws.ts_cols;
	}
#  else
	char *p, *getenv();

	if (p = getenv("LINES"))
		*rows = atoi(p);
	else
		*rows = 0;
	if (p = getenv("COLUMNS"))
		*cols = atoi(p);
	else
		*cols = 0;
#  endif
#endif	
}

/*
 * Set cbreak mode.
 * Mode 0 = normal.
 * Mode 1 = cbreak, no echo
 * Mode 2 = raw, no echo.
 * Mode 3 = only return erasechar (for wkeys.c)
 *
 * Returns: the current erase character.
 */  
int setcbreak(mode)
int mode;
{
#if defined (_V7) || defined (_BSD43)
  struct sgttyb args;
  static int init = 0;
  static int erasechar;
  static struct sgttyb sgttyb;
  static struct tchars tchars;
#ifdef _BSD43  
  static struct ltchars ltchars;
#endif
  
  if (init == 0) {
	(void) ioctl(0, TIOCGETP, &sgttyb);
	(void) ioctl(0, TIOCGETC, &tchars);
#ifdef _BSD43
	(void) ioctl(0, TIOCGLTC, &ltchars);
#endif
	erasechar = sgttyb.sg_erase;
	init++;
  }

  if (mode == 3) return(erasechar);

  if (mode == 0) {
  	(void) ioctl(0, TIOCSETP, &sgttyb);
	(void) ioctl(0, TIOCSETC, &tchars);
#ifdef _BSD43
	(void) ioctl(0, TIOCSLTC, &ltchars);
#endif
  	return(erasechar);
  }

  (void) ioctl(0, TIOCGETP, &args);
  if (mode == 1) {
	args.sg_flags |= CBREAK;
	args.sg_flags &= ~(ECHO|RAW);
  }
  if (mode == 2) {
  	args.sg_flags |= RAW;
  	args.sg_flags &= ~(ECHO|CBREAK);
  }
  (void) ioctl(0, TIOCSETP, &args);
  return(erasechar);
#endif /* _V7 || _BSD43 */

#ifdef _SYSV
  struct termio termio;
  static int init = 0;
  static int erasechar;
  static struct termio saveterm;
  static int lastmode = -1;

#ifndef XCASE
#  define XCASE _XCASE
#endif

  if (init == 0) {
	(void) ioctl(0, TCGETA, &saveterm);
	erasechar = saveterm.c_cc[VERASE];
	init++;
  }

  if (mode == 3) return(erasechar);

#ifdef _HPUX_SOURCE
  /* In HP/UX, TCSETAW does not work for me. So we use only RAW
   * or NORMAL mode, so we never have to switch from cbreak <--> raw
   */
  if (mode == 1) mode = 2;
#endif

  /* Avoid overhead */
  if (mode == lastmode) return(erasechar);
  lastmode = mode;

  /* Wait for output to drain (sometimes TCSETAW doesn't work right) */
  ioctl(0, TCSBRK, 1);

  /* Always return to default settings first */
  (void) ioctl(0, TCSETAW, &saveterm);

  if (mode == 0) {
  	return(erasechar);
  }

  (void) ioctl(0, TCGETA, &termio);
  if (mode == 1) {
	termio.c_oflag &= ~OPOST;
	termio.c_lflag &= ~(XCASE|ECHONL|NOFLSH);
  	termio.c_lflag &= ~(ICANON | ISIG | ECHO);
  	termio.c_cflag |= CREAD;
  	termio.c_cc[VTIME] = 5;
  	termio.c_cc[VMIN] = 1;
  }
  if (mode == 2) { /* raw */
  	termio.c_iflag &= ~(IGNBRK | IGNCR | INLCR | ICRNL | IUCLC | 
  		IXANY | IXON | IXOFF | INPCK | ISTRIP);
  	termio.c_iflag |= (BRKINT | IGNPAR);
	termio.c_oflag &= ~OPOST;
	termio.c_lflag &= ~(XCASE|ECHONL|NOFLSH);
  	termio.c_lflag &= ~(ICANON | ISIG | ECHO);
  	termio.c_cflag |= CREAD;
  	termio.c_cc[VTIME] = 5;
  	termio.c_cc[VMIN] = 1;
  }	
  (void) ioctl(0, TCSETAW, &termio);
  return(erasechar);
#endif /* _SYSV */  
}

#if 0
/*
 * strtok token from the Minix library - I suppose this one's PD.
 *
 * Get next token from string s (NULL on 2nd, 3rd, etc. calls),
 * where tokens are nonempty strings separated by runs of
 * chars from delim.  Writes NULs into s to end tokens.  delim need not
 * remain constant from call to call.
 */

char *strtok(s, delim)		/* NULL if no token left */
char *s;
register char *delim;
{
  register char *scan;
  char *tok;
  register char *dscan;

  if (s == (char *)0 && scanpoint == (char *)0) return((char *)0);
  if (s != (char *)0)
	scan = s;
  else
	scan = scanpoint;

  /* Scan leading delimiters. */
  for (; *scan != '\0'; scan++) {
	for (dscan = delim; *dscan != '\0'; dscan++)
		if (*scan == *dscan) break;
	if (*dscan == '\0') break;
  }
  if (*scan == '\0') {
	scanpoint = (char *)0;
	return((char *)0);
  }
  tok = scan;

  /* Scan token. */
  for (; *scan != '\0'; scan++) {
	for (dscan = delim; *dscan != '\0';)	/* ++ moved down. */
		if (*scan == *dscan++) {
			scanpoint = scan + 1;
			*scan = '\0';
			return(tok);
		}
  }

  /* Reached end of string. */
  scanpoint = (char *)0;
  return(tok);
}
#endif

#ifdef _SVR2
/* Fake the dup2() system call */
int dup2(from, to)
int from, to;
{
  int files[20];
  int n, f, exstat = -1;
  extern int errno;

  /* Ignore if the same */
  if (from == to) return(to);

  /* Initialize file descriptor table */
  for(f = 0; f < 20; f++) files[f] = 0;

  /* Close "to" file descriptor, if open */
  close(to);

  /* Keep opening files until we reach "to" */
  while((n = open("/dev/null", 0)) < to && n >= 0) {
  	if (n == from) break;
  	files[n] = 1;
  }
  if (n == to) {
  	/* Close "to" again, and perform dup() */
  	close(n);
  	exstat = dup(from);
  } else {
  	/* We failed. Set exit status and errno. */
  	if (n > 0) close(n);
  	exstat = -1;
  	errno = EBADF;
  }
  /* Close all temporarily opened file descriptors */
  for(f = 0; f < 20; f++) if (files[f]) close(f);

  /* We're done. */
  return(exstat);
}
#endif

