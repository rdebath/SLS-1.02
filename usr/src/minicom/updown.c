/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 */
#include <sys/types.h>
#ifdef _POSIX_SOURCE
#  include <stdlib.h>
#  include <unistd.h>
#else
  char *getenv();
#endif
#undef NULL
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <fcntl.h>
#include <ctype.h>
#include "window.h"
#include "minicom.h"
#include "configsym.h"

#ifndef _NSIG
# define _NSIG NSIG
#endif

static int udpid;

/* Forward declaration */
_PROTO(static int setenv, (const char *name, const char *value, int overwrite));

/*
 * Change to a directory.
 */
static int mcd(dir)
char *dir;
{
  char buf[256];
  char err[50];
  static char odir[256];
  static int init = 0;
#ifdef _COHERENT
  char *cwd;
  char *dummy;	
#endif 

  if (!init) {
  	if (*dir == 0) return(0);
  	init = 1;
#ifdef _COHERENT
	cwd = getwd(dummy);
	strcpy(&odir[0],cwd);
#else
  	getcwd(odir, 255);
#endif
  }
  if (*dir == 0) {
  	chdir(odir);
  	return(0);
  }
  
  if(*dir != '/') {
  	sprintf(buf, "%s/%s", homedir, dir);
  	dir = buf;
  }
  if (chdir(dir) < 0) {
  	sprintf(err, "Cannot chdir to %.30s", dir);
  	werror(err);
  	return(-1);
  }
  return(0);
}

/*
 * Catch the CTRL-C signal.
 */
/*ARGSUSED*/
static void udcatch(dummy)
int dummy;
{
  signal(SIGINT, udcatch);
  kill(udpid, SIGKILL);
}

/*
 * Choose from numerous up and download protocols!
 */
void updown(what)
int what;
{
  char *name[13];
  int idx[13];
  int r, f, g = 0;
  char *t = what == 'U' ? "Upload" : "Download";
  char buf[128];
  char *s ="";
  int pipefd[2];
  int n, status;
  char cmdline[128];
  WIN *win;

  if (mcd(what == 'U' ? P_UPDIR : P_DOWNDIR) < 0)
  	return;

  for(f = 0; f < 12; f++) {
  	if (P_PNAME(f)[0] && P_PUD(f) == what) {
  		name[g] = P_PNAME(f);
  		idx[g++] = f;
  	}
  }
  name[g] = CNULL;
  if (g == 0) return;

  r = wselect(30, 7, name, NIL_FUNLIST, t, stdattr, MFG, MBG) - 1;
  if (r < 0) return;

  g = idx[r];
  buf[0] = 0;

  if (P_PNN(g) == 'Y') {
  	s = input("Please enter file names", buf);
  	if (s == CNULL || *s == 0) return;
  }

  sprintf(cmdline, "%s %s", P_PPROG(g), s);

  win = wopen(10, 7, 70, 13, BSINGLE, stdattr, MFG, MBG, 1);
 
  pipe(pipefd);

  switch(udpid = fork()) {
  	case -1:
  		werror("Out of memory: could not fork()");
  		close(pipefd[0]);
  		close(pipefd[1]);
  		wclose(win, 1);
  		(void) mcd("");
  		return;
  	case 0: /* Child */
  		dup2(portfd, 0);
  		dup2(portfd, 1);
  		dup2(pipefd[1], 2);
  		close(pipefd[0]);
  		if (pipefd[1] != 2) close(pipefd[1]);
  		
  		for(n = 1; n < _NSIG; n++) signal(n, SIG_DFL);
  		
		setgid(real_gid);
  		setuid(real_uid);
  		(void) fastexec(cmdline);
  		exit(1);
  	default: /* Parent */
  		break;
  }
  (void) setcbreak(1); /* Cbreak, no echo. */
  signal(SIGINT, udcatch);
  close(pipefd[1]);
  while((n = read(pipefd[0], buf, 80)) > 0) {
  	buf[n] = '\0';
  	wputs(win, buf);
  }
  (void) m_wait(&status);
  signal(SIGINT, SIG_IGN);
  sleep(1);
  wclose(win, 1);
  m_flush(portfd);
  (void) setcbreak(2); /* Raw, no echo. */
  close(pipefd[0]);
  (void) mcd("");
}

/*
 * Translate %b to the current baudrate, and
 *           %l to the current tty port.
 */
static char *translate(s)
char *s;
{
  static char buf[128];
  int i;

  for(i = 0; *s && i < 127; i++, s++) {
  	if (*s != '%') {
  		buf[i] = *s;
  		continue;
  	}
  	switch(*++s) {
  		case 'l':
  			strcpy(buf + i, P_PORT);
  			i += strlen(P_PORT) - 1;
  			break;
  		case 'b':
  			strcpy(buf + i, P_BAUDRATE);
  			i += strlen(P_BAUDRATE) - 1;
  			break;
  		default:
  			buf[i++] = '%';
  			buf[i] = *s;
  			break;
  	}
  }
  buf[i] = 0;
  return(buf);
}


void kermit()
{
  char *term;
  int status;
  int pid, n;
  int pipefd[2];
  char buf[81];
  char *ptr;
  int fd;

  if (real_uid != 0 && P_KERMALLOW[0] != 'Y') {
  	werror("You are not allowed to run Kermit");
  	return;
  }
  if ((term = getenv("TERM")) != CNULL) term = "dumb";

  pipe(pipefd);

  switch(pid = fork()) {
  	case -1:
  		werror("Out of memory: could not fork()");
  		close(pipefd[0]);
  		close(pipefd[1]);
  		return;
  	case 0: /* Child */
  		dup2(pipefd[1], 1);
  		dup2(pipefd[1], 2);
  		close(pipefd[0]);
  		close(pipefd[1]);
  		
  		/* Remove lockfile */
  		if (lockfile[0]) unlink(lockfile);
  		if (P_KERMREAL[0] == 'Y') {
			setgid(real_gid);
			setuid(real_uid);
		}
  		for(n = 0; n < _NSIG; n++) signal(n, SIG_DFL);
  		
  		/* Do not allow shell escapes */
  		setenv("SHELL", "/bin/false", 0);
  		(void) fastexec(translate(P_KERMIT));
  		exit(1);
  	default: /* Parent */
  		break;
  }
  close(pipefd[1]);

  us->autocr = 1;

  /* pipe output from kermit to terminal emulator */
  while((n = read(pipefd[0], buf, 80)) > 0) {
  	ptr = buf;
  	while(n--)
  		out_vt100(*ptr++);
  	wflush();
  }

  wputs(us, "\n[ back in minicom ]\n");
  us->autocr = 0;

  (void) m_wait(&status);
  /* Re-create lockfile */
  if (lockfile[0]) 
  	/* Create lockfile compatible with UUCP-1.2 */
  	if ((fd = open(lockfile, O_WRONLY | O_CREAT | O_EXCL)) < 0) {
  		werror("Cannot re-create lockfile!");
  	} else {
  		sprintf(buf, "%05d minicom %s", getpid(), username);
  		write(fd, buf, strlen(buf));
  		close(fd);
  	}
  m_flush(portfd);
  close(pipefd[0]);
}

/* ============ Here begins the setenv function ============= */
/*
 * Compare two strings up to '='
 */
static int varcmp(s1, s2)
char *s1, *s2;
{
  while(*s1 && *s2) {
  	if (*s1 == '=' && *s2 == '=') return(1);
  	if (*s1++ != *s2++) return(0);
  }
  return(1);
}

/*
 * Generate a name=value string.
 */
static char *makenv(name, value)
char *name, *value;
{
  char *p;
  
  if ((p = (char *)malloc(strlen(name) + strlen(value) + 3)) == CNULL)
	return(p);
  sprintf(p, "%s=%s", name, value);
  return(p);
}

/*
 * Set a environment variable. 
 */
static int setenv(name, value, overwrite)
const char *name, *value;
int overwrite;
{
  static int init = 0;
  extern char **environ;
  char *p, **e, **newe;
  int count = 0;

  if ((p = makenv(name, value)) == CNULL) return(-1);

  for(e = environ; *e; e++) {
  	count++;
  	if(varcmp(name, *e)) {
  		*e = p;
  		return(0);
  	}
  }
  count += 2;
  if ((newe = (char **)malloc(sizeof(char *) * count)) == (char **)0) {
  	free(p);
  	return(-1);
  }
  memcpy((char *)newe, (char *)environ , (int) (count * sizeof(char *)));
  if (init) free((char *)environ);
  init = 1;
  environ = newe;
  for(e = environ; *e; e++)
  	;
  *e++ = p;
  *e = CNULL;
  return(0);
}

/* ============ This is the end of the setenv function ============= */

/*
 * Run an external script.
 * ask = 1 if first ask for confirmation.
 * s = scriptname, l=loginname, p=password.
 */
void runscript(ask, s, l, p)
int ask;
char *s, *l, *p;
{
  int status;
  int n;
  int pipefd[2];
  char buf[81];
  char cmdline[128];
  char *ptr;
  WIN *w;
  int done = 0;
  char *msg = "Same as last";

  if (ask) {
	w = wopen(10, 5, 70, 10, BDOUBLE, stdattr, MFG, MBG, 0);
	wtitle(w, TMID, "Run a script");
	wputs(w, "\n");
	wprintf(w, " A -   Username        : %s\n",
					scr_user[0] ? msg : "");
	wprintf(w, " B -   Password        : %s\n",
					scr_passwd[0] ? msg : "");
	wprintf(w, " C -   Name of script  : %s\n", scr_name);
	wlocate(w, 4, 5);
	wputs(w, "Change which setting?     (Return to run, ESC to stop)");
	wredraw(w, 1);

	while(!done) {
	    wlocate(w, 26, 5);
	    n = getch();
	    if (islower(n)) n = toupper(n);
	    switch(n) {
		case '\r':
		case '\n':
			if (scr_name[0] == '\0') {
				wbell();
				break;
			}
			wclose(w, 1);
			done = 1;
			break;
		case 27: /* ESC */
			wclose(w, 1);
			return;
		case 'A':
			wlocate(w, 25, 1);
			wclreol(w);
			scr_user[0] = 0;
			wgets(w, scr_user, 32);
			break;
		case 'B':
			wlocate(w, 25, 2);
			wclreol(w);
			scr_passwd[0] = 0;
			wgets(w, scr_passwd, 32);
			break;
		case 'C':
			wlocate(w, 25, 3);
			wgets(w, scr_name, 32);
			break;
		default:
			break;
	    }
	}
  } else {
  	strcpy(scr_user, l);
  	strcpy(scr_name, s);
  	strcpy(scr_passwd, p);
  }

  /* Throw away status line if temporary */
  if (tempst) {
  	wclose(st, 1);
  	tempst = 0;
  	st = NIL_WIN;
  }
  scriptname(scr_name);
  
  pipe(pipefd);

  if (mcd(P_SCRIPTDIR) < 0) return;

  sprintf(cmdline, "%s %s", P_SCRIPTPROG, scr_name);

  switch(udpid = fork()) {
  	case -1:
  		werror("Out of memory: could not fork()");
  		close(pipefd[0]);
  		close(pipefd[1]);
  		(void) mcd("");
  		return;
  	case 0: /* Child */
  		dup2(portfd, 0);
  		dup2(portfd, 1);
  		dup2(pipefd[1], 2);
  		close(pipefd[0]);
  		close(pipefd[1]);
  		
  		for(n = 1; n < _NSIG; n++) signal(n, SIG_DFL);
  		
		setgid(real_gid);
  		setuid(real_uid);
  		setenv("LOGIN", scr_user, 0);
  		setenv("PASS", scr_passwd, 0);
  		(void) fastexec(cmdline);
  		exit(1);
  	default: /* Parent */
  		break;
  }
  (void) setcbreak(1); /* Cbreak, no echo */
  signal(SIGINT, udcatch);
  close(pipefd[1]);
  
  /* pipe output from "runscript" program to terminal emulator */
  while((n = read(pipefd[0], buf, 80)) > 0) {
  	ptr = buf;
  	while(n--)
  		out_vt100(*ptr++);
  	wflush();
  }
  
  /* Collect status, and clean up. */
  (void) m_wait(&status);
  signal(SIGINT, SIG_IGN);
  (void) setcbreak(2); /* Raw, no echo */
  close(pipefd[0]);
  scriptname("");
  (void) mcd("");
}
