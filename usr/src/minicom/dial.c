/*
 * This file is part of the Minicom Communications Program,
 * written by Miquel van Smoorenburg 1991/1992.
 */
#include <sys/types.h>
#ifdef _POSIX_SOURCE
#  include <unistd.h>
#  include <stdlib.h>
#  ifdef _MINIX
#    undef NULL
#  endif
#endif
#include <signal.h>
#if defined(_BSD43) || defined (_SYSV)
#  define NOSTREAMS
#  include <sys/file.h>
#endif
#include <string.h>
#ifdef _MINIX
#  undef NULL
#endif
#include <stdio.h>
#include <setjmp.h>
#include <sys/stat.h>
#include "keyserv.h"
#include "window.h"
#include "minicom.h"
#include "configsym.h"

struct dialent {
  char name[32];
  char number[16];
  char script[16];
  char username[32];
  char password[32];
  char term;
  char baud[8];
  char parity[2];
  char dum1, dum2; /* Alignment */
  char bits[2];
  struct dialent *next;
};
  
#define dialentno(di, no) ((struct dialent *)((char *)(di) + ((no) * sizeof(struct dialent))))  

static struct dialent *dialents;
static int nrents = 1;
static int newtype;
/* Access to ".daildir" denied? */
static int dendd = 0;
static jmp_buf tmbuf;

/*
 * Functions to talk to the modem.
 */
 
/*
 * Send a string to the modem.
 */
void mputs(s)
char *s;
{
  char c;

  while(*s) {
  	if (*s == '^' && (*(s + 1))) {
  		s++;
  		if (*s == '^')
  			c = *s;
  		else
  			c = (*s) & 31;
  	} else
  		c = *s;
  	if (c == '~')
  		sleep(1);
  	else	
  		write(portfd, &c, 1);
  	s++;
  }
}
  
/*
 * Initialize the modem.
 */ 
void modeminit()
{
  WIN *w;

  if (P_MINIT[0] == '\0') return;

  w = tell("Initializing Modem");
  m_dtrtoggle(portfd);
  mputs(P_MINIT);
  wclose(w, 1);
}

/*
 * Reset the modem.
 */
void modemreset()
{
  WIN *w;

  if (P_MRESET[0] == '\0') return;

  w = tell("Resetting Modem");
  mputs(P_MRESET);
  sleep(1);
  wclose(w, 1);
}

/*
 * Hang the line up.
 */
void hangup()
{
  WIN *w;

  w = tell("Hanging up");

  if (P_MDROPDTR[0] == 'Y') {
  	m_dtrtoggle(portfd);
  	/* Sometimes the minix tty driver does not see
  	 * that DCD has dropped.
  	 * (This is a kludge!)
  	 */
  	m_setdcd(portfd, 0);
  } else {
  	mputs(P_MHANGUP);
  	sleep(1);
  }
  /* If we don't have DCD support fake DCD dropped */
  bogus_dcd = 0;
  wclose(w, 1);
}

/*
 * This seemed to fit best in this file
 * Send a break signal.
 */
void sendbreak()
{
  WIN *w;
  
  w = tell("Sending BREAK");
  wcursor(w, CNONE);

  m_break(portfd);
  wclose(w, 1);
}
  
WIN *dialwin;
int dialtime;

/*
 * Get a string from the modem.
 */
static char *mgetstr()
{
  static char buf[80];
  int r;
  int f = 0;

  /* Read until a non CR or LF char */
  do {
  	
  	r = read(portfd, &buf[f], 1);
   } while(r <= 0 || buf[f] == '\r' || buf[f] == '\n');

  /* Read a whole word */
  f++;
  do {
  	/* Read data from serial port */
	r = read(portfd, &buf[f], 1);
	if (r <= 0) continue;
	f++;
  } while(buf[f - 1] != '\r' && buf[f - 1] != '\n');

  buf[f - 1] = '\0';
  
  return(buf);
}

/*
 * Catch the alarm signal (dial timeout)
 */
/*ARGSUSED*/
static void catchalrm(dummy)
int dummy;
{
  if (dialtime > 1) {
	signal(SIGALRM, catchalrm);
	alarm(1);
  } else {
	signal(SIGALRM, SIG_DFL);
	alarm(0);
  }
  dialtime--;
  wlocate(dialwin, 11, 3);
  wprintf(dialwin, "%-3d", dialtime);
  if (dialtime == 0) longjmp(tmbuf, 1);
}

/*
 * The dial has failed. Tell user.
 * Count down until retrytime and return.
 */
static void dialfailed(s, rtime)
char *s;
int rtime;
{
  int f;

  alarm(0);
  wlocate(dialwin, 1, 5);
  wprintf(dialwin, "    No connection: %s.\n", s);
  if (rtime < 0) {
  	wprintf(dialwin, "   Press any key to continue..    ");
  	return;
  }
  wprintf(dialwin, "     Retry in %2d seconds         ", rtime);
  
  for(f = rtime - 1; f >= 0; f--) {
  	sleep(1);
  	wlocate(dialwin, 14, 6);
  	wprintf(dialwin, "%2d ", f);
  }
  sleep(1); /* Allow modem time to hangup if redial time == 0 */
  wlocate(dialwin, 1, 5);
  wprintf(dialwin, "                              \n");
  wprintf(dialwin, "                        ");
}

/*
 * Dial a number, and display the name.
 */
int dial(name, num, keypress)
char *name;
char *num;
int keypress;
{
  char *s, *t;
  int f, nb, retst = -1;
  int ret, retries = 0;
  int maxretries = 1, rdelay = 45;
  char *reason = "Max retries";

  dialwin = wopen(18, 9, 62, 15, BSINGLE, stdattr, MFG, MBG, 0);
  wtitle(dialwin, TMID, "Autodial");
  wcursor(dialwin, CNONE);

  wputs(dialwin, "\n");
  wprintf(dialwin, " Dialing : %s\n", name);
  wprintf(dialwin, "      At : %s\n", num);
  wredraw(dialwin, 1);

  keyserv(KSIGIO, 0);

  maxretries = atoi(P_MRETRIES);
  if (maxretries <= 0) maxretries = 1;
  rdelay = atoi(P_MRDELAY);
  if (rdelay < 0) rdelay = 0;

  /* Main retry loop of dial() */
MainLoop:
  while(++retries <= maxretries) {

	/* Calculate dial time */
	dialtime = atoi(P_MDIALTIME);
	if (dialtime == 0) dialtime = 45;

  	/* Show used time */
	wlocate(dialwin, 0, 3);
	wprintf(dialwin, "    Time : %-3d", dialtime);
	if (maxretries > 1) wprintf(dialwin, "     Attempt #%d", retries);
	wputs(dialwin, "\n\n\n Press any key to cancel dialing");
	
	
	/*
	 * Set the jump buffer for timeout
	 */
	if (setjmp(tmbuf) != 0) {
		mputs(P_MDIALCAN);
		if (maxretries > 1) {
			dialfailed("Timed out", 5);
			retst = -1;
			continue;
		}
		reason = "Timed out";
		break;	
	}

	/*
	 * Here we come after a keypress.
	 */
	if ((ret = setjmp(ksigbuf)) != 0) {
		mputs(P_MDIALCAN);
		alarm(0);
		keyserv(KSTOP, 0);
		if (dialwin != NIL_WIN) wclose(dialwin, 1);
		dialwin = NIL_WIN;
		return(retst);
	}
	
	/* Start the dial */
	m_flush(portfd);
	mputs(P_MDIALPRE);
	mputs(num);
	mputs(P_MDIALSUF);
	
	/* setup the timer */
	signal(SIGALRM, catchalrm);
	alarm(1);
	
	/* Wait 'till the modem says something */
	while(1) {
		s = mgetstr();
	
		if (!strncmp(s, P_MCONNECT, strlen(P_MCONNECT))) {
			alarm(0);
			retst = 0;
			/* Try to do auto-bauding */
			if (sscanf(s + strlen(P_MCONNECT), "%d", &nb) == 1)
				retst = nb;

			/* Try to figure out if this system supports DCD */
			f = m_getdcd(portfd);
			bogus_dcd = 1;

			wlocate(dialwin, 1, 6);
			if (keypress) {
				wputs(dialwin,
				     "Connected. Press any key to continue");
				pause(); /* Keyserv still runs */
			}
			keyserv(KSTOP, 0);
			if (dialwin != NIL_WIN) wclose(dialwin, 1);
			dialwin = NIL_WIN;
			return(retst);
		}
		for(f = 0; f < 3; f++) {
			if (f == 0) t = P_MNOCON1;
			if (f == 1) t = P_MNOCON2;
			if (f == 2) t = P_MNOCON3;
			if (f == 3) t = P_MNOCON4;
			if ((*t) && (!strncmp(s, t, strlen(t)))) {
				alarm(0);
				if (retries < maxretries)
					dialfailed(t, rdelay);
				if (maxretries == 1) reason = t;
				goto MainLoop;
			}
		}
	}
  } /* End of main while cq MainLoop */	
  alarm(0);
  dialfailed(reason, -1);
  pause(); /* Keyserv still runs */
  keyserv(KSTOP, 0);
  if (dialwin != NIL_WIN) wclose(dialwin, 1);
  dialwin = NIL_WIN;
  return(retst);
}

/*
 * Create an empty entry.
 */
static struct dialent *mkstdent()
{
  struct dialent *d;
  
  d = (struct dialent *)malloc(sizeof (struct dialent));
  
  if (d == (struct dialent *)0) return(d);

  d->name[0] = 0;
  d->number[0] = 0;
  d->script[0] = 0;
  d->username[0] = 0;
  d->password[0] = 0;
  d->term = VT100;
  strcpy(d->baud, DEF_BAUD);
  strcpy(d->bits, "8");
  strcpy(d->parity, "N");
  d->next = (struct dialent *)0;

  return(d);
}

/*
 * Read in the dialing directory from $HOME/.dialdir
 */
int readdialdir()
{
  long size;
  FILE *fp;
  char dfile[256];
  static int didread = 0;
  int f;
  struct dialent *d, *prev = (struct dialent *)0;
  struct stat stt;

  if (didread) return(0);
  didread = 1;
  nrents = 1;

  /* Construct path */
  sprintf(dfile, "%s/.dialdir", homedir);

  /* Check if it's readable IF it exists */
  if (stat(dfile, &stt) >= 0 && access(dfile, R_OK) < 0) {
	werror("Cannot open ~/.dialdir: permission denied");
	dialents = mkstdent();
	dendd = 1;
	return(0);
  }
  if ((fp = fopen(dfile, "r")) == (FILE *)NULL) {
  	dialents = mkstdent();
  	return(0);
  }
  /* Get size of the file */
  fseek(fp, 0L, SEEK_END);
  size = ftell(fp);
  if (size == 0) {
  	dialents = mkstdent();
  	fclose(fp);
  	return(0);
  }
  fseek(fp, 0L, SEEK_SET);
  if (size % sizeof(struct dialent) != 0) {
  	werror("Phonelist garbled");
  	fclose(fp);
  	unlink(dfile);
  	dialents = mkstdent();
  	return(-1);
  }

  /* Read in the dialing entries */
  nrents = size / sizeof(struct dialent);
  if (nrents == 0) {
	dialents = mkstdent();
	nrents = 1;
	fclose(fp);
	return(0);
  }
  for(f = 1; f <= nrents; f++) {
  	if ((d = (struct dialent *)malloc(sizeof (struct dialent))) ==
  		(struct dialent *)0) {
  			if(f == 1)
				dialents = mkstdent();
			else
				prev->next = (struct dialent *)0;

  			werror("Out of memory while reading dialing directory");
			fclose(fp);
  			return(-1);
  	}
  	fread((char *)d, sizeof(struct dialent), (size_t)1, fp);
  	if (prev != (struct dialent *)0)
  		prev->next = d;
  	else
  		dialents = d;
  	prev = d;
  }
  d->next = (struct dialent *)0;
  fclose(fp);
  return(0);
}

/*
 * Write the new $HOME/.dialdir
 */
static void writedialdir()
{
  struct dialent *d;
  char dfile[256];
  FILE *fp;
  int ret;

  /* Make no sense if access denied */
  if (dendd) return;

  sprintf(dfile, "%s/.dialdir", homedir);

  if ((ret = waccess(dfile)) < 0 || (fp = fopen(dfile, "w")) == (FILE *)0) {
  	werror("Can't write to ~/.dialdir");
	dendd = 1;
  	return;
  }
  if (ret == A_OK_NOTEXIST) {
#ifndef DEBUG
  	/* FIXME: should use fchown and fchmod */
#ifndef HAS_FCHOWN
	chown(dfile, real_uid, real_gid);
	chmod(dfile, 0600);
#else
	fchown(fileno(fp), real_uid, real_gid);
	fchmod(fileno(fp), 0600);
#endif
#endif
  }
  d = dialents;
  while(d) {
  	if (fwrite(d, sizeof(struct dialent), (size_t)1, fp) != 1) {
  		werror("Error writing ~/.dialdir!");
  		fclose(fp);
  		return;
  	}
  	d = d->next;
  }
  fclose(fp);
}


/*
 * Get entry "no" in list.
 */
static struct dialent *getno(no)
int no;
{
  struct dialent *d;
  
  d = dialents;
  
  if (no >= nrents) return((struct dialent *)NULL);

  while(no--) d = d->next;
  return(d);
}

static char *te[] = { "VT100", "MINIX", "ANSI " };

/*
 * Edit an entry.
 */
static void dedit(d)
struct dialent *d;
{
  WIN *w;
  int c;
  
  w = wopen(5, 5, 75, 13, BDOUBLE, stdattr, MFG, MBG, 0);
  wprintf(w, " A -   Name               : %s\n", d->name);
  wprintf(w, " B -   Number             : %s\n", d->number);
  wprintf(w, " C -   Script             : %s\n", d->script);
  wprintf(w, " D -   Username           : %s\n", d->username);
  wprintf(w, " E -   Password           : %s\n", d->password);
  wprintf(w, " F -   Terminal Emulation : %s\n", te[d->term - 1]);
  wprintf(w, " G -   Line Settings      : %s %s%s1",
  	d->baud, d->bits, d->parity);
  wlocate(w, 4, 9);
  wputs(w, "Change which setting? ");
  wredraw(w, 1);

  while(1) {
      wlocate(w, 26, 9);
      c = getch();
      if (c >= 'a') c -= 32;
      switch(c) {
        case '\033':
        case '\r':
  	case '\n':
  		wclose(w, 1);
  		return;
  	case 'A':
  		wlocate(w, 28, 0);
  		(void) wgets(w, d->name, 31);
  		break;
  	case 'B':
  		wlocate(w, 28, 1);
  		(void) wgets(w, d->number, 15);
  		break;
  	case 'C':
  		wlocate(w, 28, 2);
  		(void) wgets(w, d->script, 15);	
  		break;
  	case 'D':
  		wlocate(w, 28, 3);
  		(void) wgets(w, d->username, 31);
  		break;
  	case 'E':
  		wlocate(w, 28, 4);
  		(void) wgets(w, d->password, 31);
  		break;	
  	case 'F':
  		d->term = (d->term % 3) + 1;
  		wlocate(w, 28, 5);
  		wputs(w, te[d->term - 1]);	
  		break;
  	case 'G':
  		get_bbp(d->baud, d->bits, d->parity);
  		wlocate(w, 28, 6);
  		wprintf(w, "%s %s%s1  ", d->baud, d->bits, d->parity);
  		break;
  	default:
  		break;
      }
  }
}

static WIN *dsub;
static char *what =  "  Dial    Add     Edit   Remove ";
static int dprev;

/*
 * Highlight a choice in the horizontal menu.
 */
static void dhili(k)
int k;
{
  if (k == dprev) return;

  if (dprev >= 0) {
  	wlocate(dsub, 22 + 8*dprev, 0);
	if (!useattr) {
		wputs(dsub, " ");
	} else {
  		wsetattr(dsub, A_REVERSE | stdattr);
  		wprintf(dsub, "%8.8s", what + 8*dprev);
	}
  }
  dprev = k;
  wlocate(dsub, 22 + 8*k, 0);
  if (!useattr) {
	wputs(dsub, ">");
  } else {
	wsetattr(dsub, stdattr);
	wprintf(dsub, "%8.8s", what + 8*k);
  }
}

  
static char *fmt = "\r %2d  %-16.16s %-16.16s%5s %s%s1    %-6.6s %-15.15s\n";

/*
 * Print the dialing directory. Only draw from "cur" to bottom.
 */
static void prdir(dialw, top, cur)
WIN *dialw;
int top, cur;
{
  int f, start;
  struct dialent *d;

  start = cur - top;
  dirflush = 0;
  wlocate(dialw, 0, start + 1);
  for(f = start; f < 16; f++) {
  	d = getno(f + top);
  	if (d == (struct dialent *)0) break;
  	wprintf(dialw, fmt, f+1+top, d->name, d->number,
  		d->baud, d->bits, d->parity, te[d->term - 1], d->script);
  }
  dirflush = 1;
  wflush();
}

/*
 * Ask for a number and if <enter> is given go to the
 * dialing directory
 */
void dialdir()
{
  WIN *w;
  struct dialent *d, *d1, *d2;
  static int cur = 0;
  static int ocur = 0;
  int subm = 0;
  int quit = 0;
  static int top = 0;
  int c, nb;
  char buf[32];
  int pgud = 0;
  int first = 1;

  dprev = -1;

  w = wopen(0, LINES - 1, 79, LINES - 1, BNONE, A_REVERSE, WHITE, BLACK, 1);

  wputs(w, " Number to dial: (enter for dialing directory) ");
  buf[0] = 0;
  if (wgets(w, buf, 19) < 0) {
  	wclose(w, 1);
  	return;
  }
  wclose(w, 1);
  if (buf[0] != 0) {
  	(void) dial("Manually entered number", buf, 1);
  	return;
  }
  
  /* Allright, draw the dialing directory! */
  
  dirflush = 0;
  dsub = wopen(2, 22, 77, 22, BNONE, A_REVERSE | stdattr, MFG, MBG, 0);
  w = wopen(3, 2, 76, 19, BSINGLE, stdattr, MFG, MBG, 0);
  wcursor(w, CNONE);
  wtitle(w, TMID, "Dialing Directory");
  wputs(w, "     Name               Number       Line Format Terminal Script\n");
  wlocate(dsub, 22, 0);
  wputs(dsub, what);

  wsetregion(w, 1, w->ys - 1);
  w->doscroll = 0;

  prdir(w, top, top);  
  wlocate(w, 14, 17);
  wputs(w, "( Press Escape to exit.. )");
  dhili(subm);
  dirflush = 1;
  wredraw(dsub, 1);

again:  
  wcurbar(w, cur + 1 - top, A_REVERSE | stdattr);
  if (first) {
  	wredraw(w, 1);
  	first = 0;
  }
  while(!quit) {
  	d = getno(cur);
  	switch(c = getch()) {
  		case K_UP:
  		case 'k':
  			cur -= (cur > 0);
  			break;
  		case K_DN:
  		case 'j':
  			cur += (cur < nrents - 1);
  			break;
  		case K_LT:
  		case 'h':
			subm--;
			if (subm < 0) subm = 3;
  			break;
  		case K_RT:
  		case 'l':
  			subm = (subm + 1) % 4;
  			break;
  		case K_PGUP:
  		case '\002': /* Control-B */
  			pgud = 1;
  			quit = 1;
  			break;	
  		case K_PGDN:
  		case '\006': /* Control-F */
  			pgud = 2;
  			quit = 1;
  			break;	
  		case '\033':
  		case '\r':
  		case '\n':
  			quit = 1;
  			break;
  		default:
  			break;
  	}
  	/* Decide if we have to delete the cursor bar */
  	if (cur != ocur || quit) wcurbar(w, ocur + 1 - top, A_NORMAL | stdattr);
  		
  	if (cur < top) {
  		top--;
  		prdir(w, top, top);	
	}
	if (cur - top > 15) {
		top++;
  		prdir(w, top, top);	
	}
  	if (cur != ocur) wcurbar(w, cur + 1 - top, A_REVERSE | stdattr);
  	ocur = cur;
  	dhili(subm);
  }
  quit = 0;
  /* ESC means quit */
  if (c == '\033') {
  	writedialdir();
	wclose(w, 1);
	wclose(dsub, 1);
	return;
  }
  /* Page up or down ? */
  if (pgud == 1) { /* Page up */
  	ocur = top;
  	top -= 16;
  	if (top < 0) top = 0;
  	cur = top;
  	pgud = 0;
  	if (ocur != top) prdir(w, top, cur);
  	ocur = cur;
	goto again;
  }
  if (pgud == 2) { /* Page down */
  	ocur = top;
  	if (top < nrents - 16) {
		top += 16;
		if (top > nrents - 16) top = nrents - 16;
		cur = top;
	} else
		cur = nrents - 1;	
	pgud = 0;
	if (ocur != top) prdir(w, top, cur);
	ocur = cur;
	goto again;
  }
  
  /* Dial an entry */
  if (subm == 0) {
  	wclose(w, 1);
  	wclose(dsub, 1);
  	writedialdir();
  	strcpy(P_BAUDRATE, d->baud);
  	strcpy(P_PARITY, d->parity);
  	strcpy(P_BITS, d->bits);
	m_setparms(portfd, P_BAUDRATE, P_PARITY, P_BITS);
  	mode_status();
  	newtype = d->term;
  	if ((nb = dial(d->name, d->number, d->script[0] ? 0 : 1)) < 0) return;

  	/* Did we detect a baudrate , and can we set it? */
  	if (P_MAUTOBAUD[0] == 'Y' && nb) {
  		sprintf(P_BAUDRATE, "%d", nb);
		m_setparms(portfd, P_BAUDRATE, P_PARITY,
							P_BITS);
  	} else
  		nb = 0;
  	if (newtype != terminal)
  		init_emul(newtype);
  	else if (nb)
  		mode_status();
  	if (d->script[0]) runscript(0, d->script, d->username, d->password);
  	return;
  }
  /* Add / insert an entry */
  if (subm == 1) {
  	d1 = mkstdent();
  	if (d1 == (struct dialent *)0) {
  		wbell();
  		goto again;
  	}
  	cur++;
  	ocur = cur;
  	d2 = d->next;
  	d->next = d1;
  	d1->next = d2;
  	
  	nrents++;
  	if (cur - top > 15) {
  		top++;
  		prdir(w, top, top);
  	} else {
  		prdir(w, top, cur);
	}
  }

  /* Edit an entry */
  if (subm == 2) {
  	dedit(d);
  	wlocate(w, 0, cur + 1 - top);
  	wprintf(w, fmt, cur+1, d->name, d->number,
  		d->baud, d->bits, d->parity, te[d->term - 1], d->script);
  }
  
  /* Delete an entry from the list */
  if (subm == 3) {
  	if (nrents == 1) {
  		free((char *)d);
  		d = dialents = mkstdent();
  		prdir(w, top, top);
  		goto again;
  	}
  	if (cur == 0)
  		dialents = d->next;
  	else
  		getno(cur - 1)->next = d->next;
  	free((char *)d);
  	nrents--;
  	if (cur - top == 0 && top == nrents) {
  		top--;
  		cur--;
  		prdir(w, top, top);
  	} else {
  		if (cur == nrents) cur--;
		prdir(w, top, cur);
  	}
	if (nrents - top <= 15) {
		wlocate(w, 0, nrents - top + 1);
		wclreol(w);
	}
  	ocur = cur;
  }
  goto again;
}
