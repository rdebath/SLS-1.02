/* w - show users and their processes
 *
 * Copyright (c) 1993 Larry Greenfield
 * Distributable under the terms of the copyleft, etc.
 *
 * greenfie@gauss.rutgers.edu: Initial version
 *
 * modifications by Michael K. Johnson, johnsonm@stolaf.edu
 *
 * Visible change log (for those great doc writers):
 *  - Feb 11, 1993 (LG):  added "u" switch to ignore users while figuring
 *                        process time and current processes.
 *  - Mar 07, 1993 (LG):  changed the output to go to the next date form
 *                        sooner
 *  - Mar 07, 1993 (LG):  added help on bad command
 *  - Mar 07, 1993 (LG):  added the option of supplying a user name
 *  - Mar 15, 1993 (mkj): fixed formatting with diffs from Quowong P Liu
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <time.h>
#include <utmp.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#include "ps.h"
#include "whattime.h"

struct ps_proc_head *ph; /* a global variable */
int ignoreuser=0; /* another global, for use with -u */

static char *weekday[] = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
static char *month[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                           "Aug", "Sep", "Oct", "Nov", "Dec" };

void showinfo(struct utmp user, int formtype, int maxcmd);
char *stripblank(char *instr);
int get_maxcmd();
char *logintime(struct utmp user);
char *idletime(struct utmp user);
void getproc(struct utmp user, char *name, int *jcpu, int *pcpu, 
	     int maxcmd);

int main(int argc, char **argv)
{
  int header=1, longform=1; /* parameters */
  int args=0; /* were there any args? */
  char ch, *watchuser=NULL;
  FILE *ut; /* /etc/utmp */
  struct utmp utmpstruct; /* utmp record */
  int maxcmd;

/* get options */
  while ((ch = getopt(argc, argv, "hlus")) != EOF)
    switch (ch) {
    case 'h':
      header = 0; args++;
      break;
    case 'l':
      longform=1; args++;
      break;
    case 's':
      longform=0; args++;
      break;
    case 'u':
      ignoreuser=1; args++;
      break;
    default:
      printf("usage: w -hlsu [user]\n");
      exit(1);
      break;
    }

/* was there any user to look for? */
  if ((argv[args ? 2 : 1]) != NULL)
    watchuser = (argv[args ? 2 : 1]);

/* get maximum command length */
  maxcmd = get_maxcmd();

/* get running processes -- want all processes, we want the username as well
   as id, don't need processes without controlling
   terminals, don't need memory stats, uid doesn't matter, and ctty is NULL
   so that it doesn't get considered. */
  ph = take_snapshot(1, 1, 0, 0, 0, 0, 0);

/* print uptime */
  if (header) {
    print_uptime();
    if (longform)
      printf("User     tty       login@  idle   JCPU   PCPU  what\n");
    else
      printf("User     tty       idle    what\n");
  }
/* open utmp */

  ut = fopen(UTMP_FILE, "r");
  if (watchuser == NULL) {
    while (fread(&utmpstruct, sizeof(utmpstruct), 1, ut))
      if ((utmpstruct.ut_type == USER_PROCESS) &&
	  (utmpstruct.ut_name[0] != '\000'))
	showinfo(utmpstruct, longform, maxcmd);
  } else {
    while (fread(&utmpstruct, sizeof(utmpstruct), 1, ut))
      if (!strcmp(utmpstruct.ut_name, watchuser))
	showinfo(utmpstruct, longform, maxcmd);
  }
  fclose(ut);
  return 0;
}

void showinfo(struct utmp user, int formtype, int maxcmd)

{
  char name[254], uname[UT_NAMESIZE + 1], *ltime, *itime;
  int jcpu, pcpu;

  getproc(user, name, &jcpu, &pcpu, maxcmd);
  ltime = logintime(user); 
  itime = idletime(user);
  strncpy(uname, user.ut_user, 8); uname[8] = 0;
  if (formtype) {
    printf ("%-9.9s%-9.9s%-7.7s%-7.7s", user.ut_user, user.ut_line,
	    ltime, itime);
    if (jcpu/60) printf ("%3d:%02d", jcpu/60, jcpu%60);
    else if (jcpu) printf ("    %2d", jcpu);
    else printf ("      ");
    if (pcpu/60) printf (" %3d:%02d  %s\n", pcpu/60, pcpu%60, name);
    else if (pcpu%60) printf ("     %2d  %s\n", pcpu, name);
    else printf ("         %s\n", name);
  }
  else
    printf("%-9.9s%-8.8s%-10.10s%s\n",
	   user.ut_user, user.ut_line, itime, name);
}

char *logintime(struct utmp user)

/* get the login time in order */

{
  time_t curtime;
  struct tm *logintime, *curtm;
  int hour, am, curday, logday;
  static char give[20];

  curtime = time(NULL);
  curtm = localtime(&curtime);
  curday = curtm->tm_yday;
  logintime = localtime(&user.ut_time);
  hour = logintime->tm_hour;
  logday = logintime->tm_yday;
  am = (hour < 12);
  if (!am)
    hour -= 12;
  if (hour == 0)
    hour = 12;
/* This is a newer behavior: it waits 12 hours and the next day, and then
   goes to the 2nd time format. This should reduce confusion.
   It then waits only 6 days (not till the last moment) to go the last
   time format.
*/
  if ((curtime > (user.ut_time + (60 * 60 * 12))) && (logday != curday))
    if (curtime > (user.ut_time + (60 * 60 * 24 * 6)))
      sprintf(give, "%2d%3s%2d", logintime->tm_mday, month[logintime->tm_mon],
	     (logintime->tm_year % 100));
    else
      sprintf(give, "%*s%2d%s", 3, weekday[logintime->tm_wday],
	     hour, am ? "am" : "pm");
  else
    sprintf(give, "%2d:%02d%s", hour, logintime->tm_min, am ? "am" : "pm");
  return give;
}

char *idletime(struct utmp user)

/* find idle time */

{
  struct stat terminfo;
  long idle;
  char ttytmp[40];
  static char give[20];
  time_t curtime;

  curtime = time(NULL);
  sprintf(ttytmp, "/dev/%s", stripblank(user.ut_line));
  stat(ttytmp, &terminfo);
  idle = (curtime - terminfo.st_atime);
  if (idle >= (60 * 60)) /* more than an hour */
    if (idle >= (60 * 60 * 48)) /* more than two days */
      sprintf(give, "%2ddays", idle / (60 * 60 * 24));
    else
      sprintf(give, " %2d:%s%d", idle / (60 * 60), 
	      (idle / 60) % 60 < 10 ? "0" : "", (idle / 60) % 60);
  else
    if (idle / 60)
      sprintf(give, "%6d", idle / 60);
    else
      give[0]=0;
/*      sprintf(give, "");*/
  return give;
}

void getproc(struct utmp user, char *name, int *jcpu, int *pcpu, 
	     int maxcmd)

/* Find current commands */

{
  struct ps_proc *finder, *best;

  (*jcpu) = 0; (*pcpu) = 0;
  best = NULL;
  if (ignoreuser) {
    for (finder = ph->head; finder != NULL; finder = finder->next) {
      if (!strcmp(finder->ttyc, user.ut_id)) {
	(*jcpu) += finder->utime + finder->stime;
	if ((finder->pid == finder->tpgid) &&
	    ((best == NULL) || (finder->pid > best->pid)))
	  best = finder;
      }
    }
  } else { /* pay attention to users */
    for (finder = ph->head; finder != NULL; finder = finder->next) {
      if (!strcmp(finder->user, user.ut_user) &&
	  !strcmp(finder->ttyc, user.ut_id)) {
	(*jcpu) += finder->utime + finder->stime;
	if ((finder->pid == finder->tpgid) &&
	    ((best == NULL) ||(finder->pid > best->pid)))
	  best = finder;
      }
    }
  }
  (*jcpu) /= 100;
  if (best != NULL) {
    (*pcpu) = (best->utime + best->stime) / 100;
    
/* print the command line, but no more than will go off the edge of the
   screen.  This is standard obfuscated c... ;-)  best->cmd is used if
   the process is swapped out. */
    sprintf(name, "%s", strlen(best->cmdline) ?
	   best->cmdline[maxcmd]=0,best->cmdline : best->cmd );
  } else {
    sprintf(name, "-");
  }
}


char *stripblank(char *instr)

/* truncates the string at the site of a ' ' */

{
  int c=0;

  while (instr[c] != ' ' && instr[c] != '\000')
    c++;
  instr[c] = '\000';
  return instr;
}


int get_maxcmd()
{
    struct winsize win;
    int cols = 80;

    if (ioctl(1, TIOCGWINSZ, &win) != -1 && win.ws_col > 0)
	cols = win.ws_col;

    return cols - 48;
}
