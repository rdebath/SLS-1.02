/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 *
 * main.c - main loop of emulator.
 */
#include <sys/types.h>
#ifdef _POSIX_SOURCE
#  include <unistd.h>
#  include <stdlib.h>
#else
   char *getenv();
#endif
#undef NULL
#include <time.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <string.h>
#include <stdio.h>
#include <setjmp.h>

#include "window.h"
#include "keyserv.h"
#include "minicom.h"
#include "configsym.h"

static jmp_buf albuf;

#ifndef lint
static char *Version = "@(#)Minicom V1.3.2 1992"; /* SCCS ID */
#endif
static char *version = "V1.3 1992";		 /* For status line */

/*
 * Return the last part of a filename.
 */
static char *basename(s)
char *s;
{
  char *p;
  
  if((p = strrchr(s, '/')) == (char *)NULL)
  	p = s;
  else
  	p++;
  return(p);
}

/*
 * Leave.
 */
void leave(s)
char *s;
{
  wclose(stdwin, 1);
  m_restorestate(portfd);
  if (lockfile[0]) unlink(lockfile);
  close (portfd);
  if (P_CALLIN[0]) (void) fastsystem(P_CALLIN, CNULL, CNULL, CNULL);
  fprintf(stderr, "%s", s);
  exit(1);
}

/*
 * Return text describing command-key.
 */
char *esc_key()
{
  static char buf[16];

  if (!alt_override && P_ESCAPE[0] == '^') {
  	sprintf(buf, "CTRL-%c ", P_ESCAPE[1]);
  	return(buf);
  }
#if defined(_MINIX) || defined(_COHERENT)
  sprintf(buf, "ALT-");
#else
  sprintf(buf, "Meta-");
#endif
  return(buf);
}

static void get_alrm(dummy)
int dummy;
{
  longjmp(albuf, 1);
}

/*
 * Open the terminal.
 */
void open_term(doinit)
int doinit;
{
  struct stat stt;
  char buf[128];
  int fd;

  sprintf(lockfile, P_LOCK, basename(P_PORT));
  if (lockfile[0] && stat(lockfile, &stt) >= 0) {
  	if (stdwin != NIL_WIN) wclose(stdwin, 1);
  	fprintf(stderr, "Device %s is locked.\n", P_PORT);
  	exit(1);
  }

  if (setjmp(albuf) == 0) {
	portfd = -1;
	signal(SIGALRM, get_alrm);
	alarm(2);
	portfd = open(P_PORT, O_RDWR);
  }
  alarm(0);
  if (portfd < 0) {
  	if (stdwin != NIL_WIN) wclose(stdwin, 1);
  	fprintf(stderr, "Cannot open %s. Sorry.\n",
  			P_PORT);
  	exit(1);
  }
  if(doinit && P_CALLOUT[0])
#ifdef _COHERENT
	close(portfd);	
	if(fastsystem(P_CALLOUT, CNULL, CNULL, CNULL) < 0) {
#else
  	if(fastsystem(P_CALLOUT, CNULL, CNULL, CNULL) != 0) {
#endif
  		if (stdwin != NIL_WIN) wclose(stdwin, 1);
  		fprintf(stderr, "Could not setup for dial out.\n");
  		if (lockfile[0]) unlink(lockfile);
		close(portfd);
  		exit(1);
  	}
#ifdef _COHERENT
	portfd = open(P_PORT, O_RDWR);
#endif
  if (lockfile[0]) {
  	/* Create lockfile compatible with UUCP-1.2 */
#ifdef _COHERENT
	if ((fd = creat(lockfile, 0666)) < 0) {
#else
  	if ((fd = open(lockfile, O_WRONLY | O_CREAT | O_EXCL, 0666)) < 0) {
#endif
  		if (stdwin != NIL_WIN) wclose(stdwin, 1);
  		fprintf(stderr, "Cannot create lockfile. Sorry.\n");
  		exit(1);
  	}
  	sprintf(buf, "%05d minicom %s", getpid(), username);
  	write(fd, buf, strlen(buf));
  	close(fd);
  }

  m_nohang(portfd);
  if (doinit) m_flush(portfd);
}


/*
 * Initialize screen and status line.
 */
void init_emul(type)
int type;
{
  int x = -1, y = -1;
  char attr;

  if (st != NIL_WIN) {
  	wclose(st, 1);
  	tempst = 0;
  	st = NIL_WIN;
  }

  if (us != NIL_WIN) {
	x = us->curx; y = us->cury;
	attr = us->attr;
  	wclose(us, 0);
  }	
  /* Open a new window. */
  us = wopen(0, 0, 79, 23 + (type == MINIX && LINES > 24),
  			BNONE, A_NORMAL, SFG, SBG, 1);

  if (x >= 0) {
  	wlocate(us, x, y);
  	wsetattr(us, attr);
  }

  if (type == VT100)
  	wresetam(us);
  else
  	wsetam(us);
  linewrap = -1;	

  us->autocr = 0;

  /* See if we have space for a status line */
  if (LINES > (24 + (type == MINIX)) && P_STATLINE[0] == 'e')
  	st = wopen(0, 24 + (type == MINIX), 79, 24 + (type == MINIX),
  			BNONE, A_REVERSE, WHITE, BLACK, 1);

  if (type == MINIX && terminal != MINIX && LINES > 24) {
  	x = us->curx;
  	y = us->cury;
  	wlocate(us, 0, 24);
  	wclreol(us);
  	wlocate(us, x, y);
  }

  terminal = type;
  lines = 24 + ((type == MINIX) && LINES > 24);
  cols = 80;
  
  /* If "keyserv" process is stopped, it will still react to
   * some signals.
   */
  switch(type) {
  	case VT100:
  		keyserv(KVT100, 0);
  		break;
  	case MINIX:
  		keyserv(KMINIX, 0);
  		break;
  	case ANSI:
  		keyserv(KANSI, 0);
  		break;
  }
  if (st != NIL_WIN) show_status();
  init_vt(); /* in v100.c */
}

/*
 * Locate the cursor at the correct position in
 * the user screen.
 */
static void ret_csr()
{
  wlocate(us, us->curx, us->cury);
  wflush();
}

/*
 * Show baudrate, parity etc.
 */
void mode_status()
{
  wlocate(st, 20, 0);
  wprintf(st, " %-5s %s%s1", P_BAUDRATE, P_BITS, P_PARITY);
  ret_csr();
}

/*
 * Show offline or online time.
 * If real dcd is not supported, Online and Offline will be
 * shown in capitals.
 */
void time_status()
{
  wlocate(st, 66, 0);
  if (online < 0)
  	wputs(st, P_HASDCD[0] == 'Y' ? " Offline     " : " OFFLINE     ");
  else
  	wprintf(st," %s %02ld:%02ld", P_HASDCD[0] == 'Y' ? "Online" : "ONLINE",
  		online / 3600, (online / 60) % 60);
  		
  ret_csr();
}

/*
 * Update the online time.
 */
static void updtime()
{
  static int old_online = 0;

  if (old_online == online) return;
  old_online = online;
  if (st != NIL_WIN) {
  	time_status();
  	ret_csr();
  }
  wflush();
}

/*
 * Show the status line 
 */
void show_status()
{
  wlocate(st, 0, 0);
  wprintf(st, " %7.7sZ for help |           | FDX | Minicom %-9.9s |       | ",
  	esc_key(), version);
  mode_status();
  time_status();
  wlocate(st, 59, 0);
  switch(terminal) {
  	case VT100:
  		wputs(st, "VT100");
  		break;
  	case MINIX:
  		wputs(st, "MINIX");
  		break;
  	case ANSI:
  		wputs(st, "ANSI");
  		break;
  }
  ret_csr();
}

/*
 * Show the name of the script running now.
 */
void scriptname(s)
char *s;
{
  if (st == NIL_WIN) return;
  wlocate(st, 39, 0);
  if (*s == 0)
  	wprintf(st, "Minicom %-9.9s", version);
  else
  	wprintf(st, "script %-10.10s", s);
  ret_csr();
}

/*
 * Show status line temporarily
 */
static void showtemp()
{
  int bottom;

  if (st != NIL_WIN) return;

  bottom = 24;
  if (bottom > LINES - 1) bottom = LINES - 1;
  st = wopen(0, bottom, 79, bottom, BNONE, A_REVERSE, WHITE, BLACK, 1);
  show_status();
  tempst = 1;
}

/*
 * The main terminal loop:
 *	- If there are characters recieved send them
 *	  to the screen via the appropriate translate function.
 */
int do_terminal()
{
  char buf[128];
  char *ptr;
  int cnt;
  static time_t t1, start;
  unsigned char c;
  int dcd_support = P_HASDCD[0] == 'Y';

  dirflush = 0;

  /* Show off or online time */
  updtime();

  if ((c = setjmp(ksigbuf)) != 0) {
  	/* We got a CTRL-A */
  	keyserv(KSTOP, 0);
  	/* Restore keyboard modes */
  	(void) setcbreak(1); /* Cbreak, no echo */

  	/* Show status line temporarily */
  	showtemp();
  	if (c < ' ') /* CTRL A */
		(void) read(0, &c, 1);

  	if (c > 128) c -= 128;
  	if (c > ' ') {
		dirflush = 1;
		m_flush(0);
		return(c);
	}
	/* CTRLA - CTRLA means send one CTRLA */
	write(portfd, &c, 1);
  }
  
  /* If the status line was shown temporarily, delete it again. */
  if (tempst) {
  	tempst = 0;
  	wclose(st, 1);
  	st = NIL_WIN;
  }

  /* Set the terminal modes */
  (void) setcbreak(2); /* Raw, no echo */

  keyserv(KSTART, 0);
  /* Main loop */
  while(1) {
  	/* See if we're online. */
  	if ((!dcd_support && bogus_dcd) || (dcd_support && m_getdcd(portfd))) {
  		if (online < 0) {
  			time(&start);
  			t1 = start;
  			online = 0;
  			updtime();
  		}
  	} else {
  		online = -1;
  		updtime();
  	}

  	/* Update online time */
  	if (online >= 0) {
  		time(&t1);
  		if (t1 > (online + start + 59)) {
  			online = t1 - start;
  			updtime();
  		}
  	}

	/* Read from the modem and send it to the screen */
  	if ((cnt = read(portfd, buf, 128)) > 0) {
  		ptr = buf;
  		while(cnt--)
  			out_vt100(*ptr++);
  		wflush();
  	}
  }
}
