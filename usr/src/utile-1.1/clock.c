#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <getopt.h>
#include <time.h>
#include <string.h>

/* V1.0
 * CMOS clock manipulation - Charles Hedrick, hedrick@cs.rutgers.edu, Apr 1992
 * 
 * clock [-u] -r  - read cmos clock
 * clock [-u] -w  - write cmos clock from system time
 * clock [-u] -s  - set system time from cmos clock
 * clock [-u] -a  - set system time from cmos clock, adjust the time to
 *                  correct for systematic error, and put it back to the cmos.
 *  -u indicates cmos clock is kept in universal time
 *
 * The program is designed to run setuid, since we need to be able to
 * write the CMOS port.
 *
 * I don't know what the CMOS clock will do in 2000, so this program
 * probably won't work past the century boundary.
 *
 *********************
 * V1.1
 * Modified for clock adjustments - Rob Hooft, hooft@chem.ruu.nl, Nov 1992
 * Also moved error messages to stderr. The program now uses getopt.
 * Changed some exit codes. Made 'gcc 2.3 -Wall' happy.
 *
 * I think a small explanation of the adjustment routine should be given
 * here. The problem with my machine is that its CMOS clock is 10 seconds 
 * per day slow. With this version of clock.c, and my '/etc/rc.local' 
 * reading '/etc/clock -au' instead of '/etc/clock -u -s', this error 
 * is automatically corrected at every boot. 
 *
 * To do this job, the program reads and writes the file '/etc/adjtime' 
 * to determine the correction, and to save its data. In this file are 
 * three numbers: 
 *
 * 1) the correction in seconds per day (So if your clock runs 5 
 *    seconds per day fast, the first number should read -5.0)
 * 2) the number of seconds since 1/1/1970 the last time the program was
 *    used.
 * 3) the remaining part of a second which was leftover after the last 
 *    adjustment
 *
 * Installation and use of this program:
 *
 * a) create a file '/etc/adjtime' containing as the first and only line:
 *    '0.0 0 0.0'
 * b) run 'clock -au' or 'clock -a', depending on whether your cmos is in
 *    universal or local time. This updates the second number.
 * c) set your system time using the 'date' command.
 * d) update your cmos time using 'clock -wu' or 'clock -w'
 * e) replace the first number in /etc/adjtime by your correction.
 * f) put the command 'clock -au' or 'clock -a' in your '/etc/rc.local' or 
 *    let 'cron' start it regularly.
 *
 * If the adjustment doesn't work for you, try contacting me by E-mail.
 *
 * Something else: I did have a few occasions during testing where 
 * every number read from the CMOS was 165 (BCD=0xff). In that case 
 * mktime fails, and all kinds of weird things happen. I put in some 
 * trapping code, although I know that this shouldn't (can't?) happen.
 */

/* Here the information for time adjustments is kept. */
#define ADJPATH "/etc/adjtime"

/* used for debugging the code. */
/*#define DEBUG*/
/*#define KEEP_OFF*/

/* Stupid constants */
#define SECONDSPERDAY 86400

/* Globals */
int readit;
int adjustit;
int writeit;
int setit;
int universal;

volatile void usage() 
{
  fprintf(stderr,"clock [-u] -r|w|s|a\n");
  fprintf(stderr,"  r - read and print CMOS clock\n");
  fprintf(stderr,"  w - write CMOS clock from system time\n");
  fprintf(stderr,"  s - set system time from CMOS clock\n");
  fprintf(stderr,"  a - get system time and adjust CMOS clock\n");
  fprintf(stderr,"  u - CMOS clock is in universal time\n");
  exit(1);
}


int cmos_read(int fd, int addr)
{
  unsigned char buf[2];

  lseek(fd, 0x70, 0);
  buf[0] = 0x80 | addr;
  write(fd, buf, 1);
  lseek(fd, 0x71, 0);
  read(fd, buf, 1);
  return (*buf & 15) + (*buf>>4)*10;
}

void cmos_write(int fd, int addr, int value)
{
  unsigned char buf[2];

  value = ((value/10) << 4) + value % 10;
  lseek(fd, 0x70, 0);
  buf[0] = 0x80 | addr;
  write(fd, buf, 1);
  lseek(fd, 0x71, 0);
  buf[0] = value;
  write(fd, buf, 1);
}

int main (int argc, char **argv, char **envp) 
{
  int fd;
  struct tm tm;
  struct tm *tmp;
  time_t systime;
  time_t last_time;
  char *zone;
  char zonebuf[256];
  char arg;
  FILE *adj;
  float factor;
  float not_adjusted;
  int adjustment;

  while ((arg=getopt(argc,argv,"rwsua"))!=-1) {
    switch (arg) {
    case 'r': 
      if (readit || writeit || setit || adjustit)  /* only allow one of these */
	usage();
      readit = 1;
      break;
    case 'w':
      if (readit || writeit || setit || adjustit)
	usage();
      writeit = 1;
      break;
    case 's':
      if (readit || writeit || setit || adjustit)
	usage();
      setit = 1;
      break;
    case 'u':
      universal = 1;
      break;
    case 'a':
      if (readit || writeit || setit || adjustit)
	usage();
      adjustit = 1;
      break;
    default:
      usage();
    }
  }

  if (! (readit | writeit | setit | adjustit))  /* default to read */
    readit = 1;

  if ((fd = open("/dev/port", 2)) < 0) { perror("/dev/port"); exit(2); }

  if (adjustit) { /* Read adjustment parameters first */
      if ((adj = fopen(ADJPATH,"r")) == NULL) { 
	perror(ADJPATH); 
	exit(2);
      }
      if (fscanf(adj,"%f %d %f",&factor,&last_time,&not_adjusted)<0) { 
	perror(ADJPATH);
	exit(2); 
      }
      fclose(adj);
#ifdef DEBUG
	printf("Last adjustment done at %d seconds after 1/1/1970\n",last_time);
#endif
  }
  if (readit || setit || adjustit) {
    /* 
     * If we're unlucky, the seconds could overflow and everything
     * else change, while we're in the process of reading.  This
     * loop keeps trying until we get something that's consistent
     *
     * Changes to make sure that we're (exactly) at a change of second. 
     * This makes adjustments more reliable.
     * The 'usleep(5000)' call makes us 0.0025 seconds late on average.
     */
    tm.tm_sec = cmos_read(fd, 0);
    while (tm.tm_sec == cmos_read(fd,0)) usleep(5000); 
    do {
      int retry = 0;
#ifdef DEBUG
      if (retry>0) printf("Retrying...\n");
#endif
      if (retry>0) sleep(1); /* wait a little before retry....*/
      do {
        tm.tm_sec  = cmos_read(fd, 0);
        tm.tm_min  = cmos_read(fd, 2);
        tm.tm_hour = cmos_read(fd, 4);
        tm.tm_wday = cmos_read(fd, 6);
        tm.tm_mday = cmos_read(fd, 7);
        tm.tm_mon  = cmos_read(fd, 8);
        tm.tm_year = cmos_read(fd, 9);
      } while (tm.tm_min != cmos_read(fd, 2));
      if (retry++ > 5) {
	fprintf(stderr,"%s: Could not get CMOS time. Giving up.\n",argv[0]);
	exit(3);
      }
    } while (tm.tm_sec == 165); /* Read my note regarding '165' above */
    tm.tm_mon--;       /* DOS uses 0 base */
    tm.tm_wday -= 3;   /* DOS uses 3 - 9 for week days */
    tm.tm_isdst = -1;  /* don't know whether it's daylight */
#ifdef DEBUG
    printf("Cmos time : %d:%d:%d\n",tm.tm_hour,tm.tm_min,tm.tm_sec);
#endif
  }
  if (readit) {
    /*
     * Mktime assumes we're giving it local time.  If the CMOS clock
     * is in GMT, we have to set up TZ to mktime knows it.  Tzset gets
     * called implicitly by the time code, but only the first time.  When
     * changing the environment variable, better call tzset explicitly.
     */
    if (universal) {
      zone = (char *)getenv("TZ");  /* save original time zone */
      (void)putenv("TZ=");
      tzset();
      systime = mktime(&tm);
      /* now put back the original zone */
      if (zone) {
	if ((strlen(zone) + 4) > sizeof(zonebuf)) {
	  fprintf(stderr,"Size of TZ variable is too long\n");
	  exit(2);
	}
	strcpy(zonebuf, "TZ=");
	strcat(zonebuf, zone);
	putenv(zonebuf);
      } else { /* wasn't one, so clear it */
	putenv("TZ");
      }
      tzset();
      printf("%s", ctime(&systime));
    } else {
      printf("%s", asctime(&tm));
    }
  }
  if (setit || adjustit) {
    if (getuid() != 0) {  /* program is designed to run setuid */
      fprintf(stderr,"Sorry, must be root to set or adjust time\n");
      exit(2);
    }
    if (universal)
      (void)putenv("TZ=");    
    systime = mktime(&tm);
#ifdef DEBUG
    printf("Number of seconds since 1/1/1970 is %d\n",systime);
#endif
    if (adjustit) { /* the actual adjustment */
	float exact_adjustment;

	exact_adjustment = ((float)(systime - last_time)) 
			* factor / SECONDSPERDAY
			+ not_adjusted;
	if (exact_adjustment > 0)
		adjustment = (int) (exact_adjustment + 0.5);
	else
		adjustment = (int) (exact_adjustment - 0.5);
	not_adjusted = exact_adjustment - (float) adjustment;
	systime+= adjustment;
#ifdef DEBUG
	printf("Time since last adjustment is %d seconds\n",
		(int) (systime - last_time));
	printf("Adjusting time by %d seconds\n",
		adjustment);
	printf("remaining adjustment is %.3f seconds\n",
		not_adjusted);
#endif
    }
#ifdef KEEP_OFF
    if (0 != 0) {
#else
    if (stime(&systime) != 0) {
#endif
      fprintf(stderr,"Unable to set time -- probably you are not root\n");
      exit(1);
    }
  } 
  if (writeit || (adjustit && adjustment != 0)) {
    systime = time(NULL);
    if (universal)
      tmp = gmtime(&systime);
    else
      tmp = localtime(&systime);
    /*
     * set the seconds to 0 first, to prevent any wraparounds while
     * we're setting everything else.  Then set the real seconds last.
     */
#ifndef KEEP_OFF
    cmos_write(fd, 0, 0);
    cmos_write(fd, 2, tmp->tm_min);
    cmos_write(fd, 4, tmp->tm_hour);
    cmos_write(fd, 6, tmp->tm_wday+3);
    cmos_write(fd, 7, tmp->tm_mday);
    cmos_write(fd, 8, tmp->tm_mon+1);
    cmos_write(fd, 9, tmp->tm_year);
    cmos_write(fd, 0, tmp->tm_sec);
#endif
#ifdef DEBUG
    printf("Set to : %d:%d:%d\n",tmp->tm_hour,tmp->tm_min,tmp->tm_sec);
#endif
  }
#ifdef DEBUG
  else
    printf("CMOS clock unchanged.\n");
#endif
  /* Save data for next 'adjustit' call */
  if (adjustit) {
      if ((adj = fopen(ADJPATH,"w")) == NULL) {
	  perror(ADJPATH);
	  exit(2);
      }
      fprintf(adj,"%f %d %f\n",factor,systime,not_adjusted);
      fclose(adj);
  }
  exit(0);
}
