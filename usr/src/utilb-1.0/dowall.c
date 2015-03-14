/*
 * dowall.c	- Write to all users on the system.
 *
 * Author:	  Miquel van Smoorenburg, miquels@drinkel.nl.mugnet.org
 * 
 * Version:	  1.01  18-11-1992
 */
#include <sys/types.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <stdio.h>
#include <utmp.h>
#include <pwd.h>

/* To keep this thing less system dependant, check some things */
#ifndef UTMP
#  ifdef UTMP_FILE
#    define UTMP UTMP_FILE		/* The real name */
#    define WTMP WTMP_FILE
#  else
#    define UTMP "/etc/utmp"
#    define WTMP "/usr/adm/wtmp"
#  endif
#endif
#ifndef NO_PROCESS
#  define NO_PROCESS 0
#endif
#ifndef _NSIG
#  define _NSIG NSIG
#endif

/*
 * Wall function.
 */
void wall(text)
char *text;
{
  FILE *fp, *tp;
  char line[81];
  char term[32];
  static char *user, ttynm[16], *date;
  static int init = 0;
  struct passwd *pwd;
  char *tty, *p;
  time_t t;
  struct utmp utmp;
  
  if ((fp = fopen(UTMP, "r")) == (FILE *)0) return;
  
  if (init == 0) {
  	if ((pwd = getpwuid(getuid())) == (struct passwd *)0) {
  		fprintf(stderr, "You don't exist. Go away.\n");
  		exit(1);
  	}
  	user = pwd->pw_name;
  	if ((p = ttyname(0)) != (char *)0) {
  		if (tty = strrchr(p, '/'))
  			tty++;
  		else
  			tty = p;
  		sprintf(ttynm, "(%s) ", tty);	
  	} else
  		ttynm[0] = 0;
  	init++;
  }
  
  /* Get the time */
  time(&t);
  date = ctime(&t);
  for(p = date; *p && *p != '\n'; p++)
  	;
  *p = 0;

  sprintf(line, "\007\r\nBroadcast message from %s %s%s...\r\n\r\n", user,
  	ttynm, date);

  while(fread(&utmp, sizeof(struct utmp), 1, fp) == 1) {
  	if(utmp.ut_type != USER_PROCESS) continue;
  	sprintf(term, "/dev/%s", utmp.ut_line);
  	if (tp = fopen(term, "w")) {
  		fputs(line, tp);
  		fputs(text, tp);
  		fclose(tp);
  	}
  }
  fclose(fp);
}

