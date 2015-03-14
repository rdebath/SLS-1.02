/* This is a trivial uptime program.  I hereby release this program
 * into the public domain.  I disclaim any responsibility for this
 * program --- use it at your own risk.  (as if there were any.. ;-)
 * -michaelkjohnson (johnsonm@stolaf.edu)
 *
 * Modified by Larry Greenfield to give a more traditional output,
 * count users, etc.  (greenfie@gauss.rutgers.edu)
 *
 * Modified by mkj again to fix a few tiny buglies.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <time.h>
#include <utmp.h>
#include <sys/ioctl.h>
#include "whattime.h"

void print_uptime(void) {
  FILE *ut;
  struct utmp utmpstruct;
  int upseconds, upminutes, uphours, updays;
  int fd, nrerr;
  struct tm *realtime;
  time_t realseconds;
  int numuser;
  char uptime[50], load1[10], load2[10], load3[10];

/* first get the current time */

  time(&realseconds);
  realtime = localtime(&realseconds);
  printf (" %2d:%02d%s  ", realtime->tm_hour%12 ? realtime->tm_hour%12 : 12,
	  realtime->tm_min, realtime->tm_hour > 11 ? "pm" : "am");

/* open the proc uptime file */

  fd = open("/proc/uptime", O_RDONLY, 0);
  if (!fd) {
    perror("/proc/uptime");
    exit(1);
  }
  nrerr = read(fd,uptime,49);
  if (nrerr == -1) {
    perror("read /proc/uptime");
    exit(1);
  }
  close(fd);

/* read and calculate the amount of uptime */

  sscanf(uptime, "%d", &upseconds);
  updays = upseconds / (60*60*24);
  printf("up ");
  if (updays)
    printf("%d %s, ", updays, (updays != 1) ? "days" : "day");
  upminutes = upseconds / 60;
  uphours = upminutes / 60;
  uphours = uphours % 24;
  upminutes = upminutes % 60;
  if(uphours)
    printf("%2d:%02d, ", uphours, upminutes);
  else
    printf("%d min, ", upminutes);

/* count the number of users */

  ut = fopen(UTMP_FILE, "r");
  numuser = 0;
  while (fread(&utmpstruct, sizeof(utmpstruct), 1, ut))
    if ((utmpstruct.ut_type == USER_PROCESS) &&
        (utmpstruct.ut_name[0] != '\000'))
      numuser++;
  printf("%2d %s, ", numuser, numuser == 1 ? "user" : "users");

/* figure out the load average by reading the proc fs */

  fd = open("/proc/loadavg", O_RDONLY, 0);
  if (!fd) {
    perror("/proc/loadavg");
    exit(1);
  }
  nrerr = read(fd,uptime,49);
  if (nrerr == -1) {
    perror("read /proc/loadavg");
    exit(1);
  }
  close(fd);

  sscanf(uptime, "%s %s %s", load1, load2, load3);
  printf(" load average: %s, %s, %s\n", load1, load2, load3);
}
