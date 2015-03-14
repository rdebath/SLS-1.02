#include <stdio.h>
#include <errno.h>
#include <malloc.h>
#include <string.h>
#include <syslog.h>
#include <time.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/param.h>
#include <pwd.h>
#include <shadow.h>
#include <grp.h>


#include "pathnames.h"
#include "extensions.h"

extern	char	remotehost[], remoteaddr[];
extern	int		fnmatch(), nameserved, anonymous, guest;
char	shutdown[MAXPATHLEN];

/*************************************************************************/
/* FUNCTION  : parse_time                                                */
/* PURPOSE   : Check a single valid-time-string against the current time */
/*             and return whether or not a match occurs.                 */
/* ARGUMENTS : a pointer to the time-string                              */
/*************************************************************************/

int
parsetime(whattime)
char    *whattime;

{
static char *days[] = { "Su", "Mo", "Tu", "We", "Th", "Fr", "Sa", "Wk" };
time_t	clock;
struct  tm *curtime;
int     wday, start, stop, ltime, validday, loop, match;

   (void) time(&clock);
   curtime = localtime(&clock);
   wday = curtime->tm_wday;
   validday = 0;
   match = 1;

   while (match && isalpha(*whattime) && isupper(*whattime)) {
      match = 0;
      for (loop = 0; loop < 8; loop++) {
         if (strncmp(days[loop], whattime, 2) == NULL) {
            whattime += 2;
            match = 1;
            if ((wday == loop) | ((loop == 7) && wday && (wday < 6))) {
               validday = 1;
            }
         }
      }
   }

   if (strncmp(whattime, "Any", 3) == NULL) {
      validday = 1;
      whattime += 3;
   }

   if (!validday) return(0);

   if (sscanf(whattime, "%d-%d", &start, &stop) == 2) {
      ltime = curtime->tm_min + 100 * curtime->tm_hour;
      if ((start < stop) && ((ltime > start) && ltime < stop)) return(1);
      if ((start > stop) && ((ltime > start) || ltime < stop)) return(1);
   } else return(1);

   return(0);
}

/*************************************************************************/
/* FUNCTION  : validtime                                                 */
/* PURPOSE   : Break apart a set of valid time-strings and pass them to  */
/*             parse_time, returning whether or not ANY matches occurred */
/* ARGUMENTS : a pointer to the time-string                              */
/*************************************************************************/

int
validtime(ptr)
char    *ptr;

{
char    *nextptr;
int	good;

   while (1) {
      nextptr = strchr(ptr, '|');
      if (strchr(ptr, '|') == NULL) return(parsetime(ptr));
      *nextptr = '\0';
      good = parsetime(ptr);
      *nextptr++ = '|';  /* gotta restore the | or things get skipped! */
      if (good) return(1);
      ptr = nextptr;
   }
}

/*************************************************************************/
/* FUNCTION  : hostmatch                                                 */
/* PURPOSE   : Match remote hostname or address against a glob string    */
/* ARGUMENTS : The string to match                                       */
/* RETURNS   : 0 if no match, 1 if a match occurs                        */
/*************************************************************************/

hostmatch(addr)
char	*addr;

{

   if (addr == NULL) return(0);

   if (isdigit(*addr))
      return(fnmatch(addr, remoteaddr, NULL));
   else
      return(fnmatch(addr, remotehost, NULL));
}

/*************************************************************************/
/* FUNCTION  : acl_guestgroup                                            */
/* PURPOSE   : If the real user is a member of any of the listed groups, */
/*             return 1.  Otherwise return 0.                            */
/* ARGUMENTS : pw, a pointer to the passwd struct for the user           */
/*************************************************************************/

int
acl_guestgroup(pw)
struct	passwd	*pw;

{
struct	aclmember	*entry = NULL;
struct	group		*grp;
gid_t	gid;
int		which;
char	**member;

   entry = (struct aclmember *) NULL;
   /* guestgroup <group> [<group> ...] */

   while (getaclentry("guestgroup", &entry)) {
      for (which = 0; which < MAXARGS && entry->arg[which]; which++) {
         if (!(grp = getgrnam(entry->arg[which]))) continue;
         if (pw->pw_gid == grp->gr_gid) return(1);
         for (member = grp->gr_mem; *member; member++) {
            if (!strcmp(*member, pw->pw_name)) return(1);
         }
      }
   }
   return(0);
}

/*************************************************************************/
/* FUNCTION  : acl_autogroup                                             */
/* PURPOSE   : If the guest user is a member of any of the classes in    */
/*             the autogroup comment, cause a setegid() to the specified */
/*             group.                                                    */
/* ARGUMENTS : pw, a pointer to the passwd struct for the user           */
/*************************************************************************/

void
acl_autogroup(pw)
struct	passwd	*pw;

{
char	class[1024];

struct	aclmember	*entry = NULL;
struct	group		*grp;
gid_t	gid;
int		which;

   (void) acl_getclass(class);

   entry = (struct aclmember *) NULL;
   /* autogroup <group> <class> [<class> ...] */

   while (getaclentry("autogroup", &entry)) {
      if (!ARG0 || !ARG1) return;

      grp = getgrnam(ARG0);
      if (grp) {
         gid = grp->gr_gid;
      } else {
         syslog(LOG_ERR, "autogroup: set group %s not found");
         continue;
      }

      for (which = 1; which < MAXARGS && entry->arg[which]; which++) {
         if (!strcmp(entry->arg[which], class)) {
            pw->pw_gid = gid;
            return;
         }
      }
   }
}

/*************************************************************************/
/* FUNCTION  : acl_setfunctions                                          */
/* PURPOSE   : Scan the ACL buffer and determine what logging to perform */
/*             for this user, and whether or not user is allowed to use  */
/*             the automatic TAR and COMPRESS functions.                 */
/* ARGUMENTS : pointer to buffer to class name, pointer to ACL buffer    */
/*************************************************************************/

void
acl_setfunctions()

{
char	class[1024];

extern	int	log_incoming_xfers,
			log_outbound_xfers,
			mangleopts,
			cmdlogging,
			lgi_failure_threshold;

struct	aclmember	*entry = NULL;

int		l_compress = 0,
		l_tar = 0,
		inbound = 0,
		outbound = 0,
		which,
		set;

   log_incoming_xfers = 0;
   log_outbound_xfers = 0;
   cmdlogging = 0;

   (void) acl_getclass(class);

   entry = (struct aclmember *) NULL;
   if (getaclentry("loginfails", &entry) && ARG0 != NULL) {
      lgi_failure_threshold = atoi(ARG0);
   }

#ifndef	NO_PRIVATE
   entry = (struct aclmember *) NULL;
   if (getaclentry("private", &entry) && ARG0 != NULL)
      priv_setup(ARG0);
#endif	/* !NO_PRIVATE */

   entry = (struct aclmember *) NULL;
   set = 0;
   while (!set && getaclentry("compress", &entry)) {
      if (!strcasecmp(ARG0, "yes")) l_compress = 1;
      for (which = 1; (which < MAXARGS) && entry->arg[which]; which++) {
         if (fnmatch(entry->arg[which], class, NULL)) {
            mangleopts |= l_compress * (O_COMPRESS | O_UNCOMPRESS);
            set = 1;
         }
      }
   }

   entry = (struct aclmember *) NULL;
   set = 0;
   while (!set && getaclentry("tar", &entry)) {
      if (!strcasecmp(ARG0, "yes")) l_tar = 1;
      for (which = 1; (which < MAXARGS) && entry->arg[which]; which++) {
         if (fnmatch(entry->arg[which], class, NULL)) {
            mangleopts |= l_tar * O_TAR;
            set = 1;
         }
      }
   }

/* plan on expanding command syntax to include classes for each of these */

   entry = (struct aclmember *) NULL;
   while (getaclentry("log", &entry)) {
      if (!strcasecmp(ARG0, "commands")) {
         if (anonymous && strcasestr(ARG1, "anonymous")) cmdlogging = 1;
         if (guest && strcasestr(ARG1, "guest")) cmdlogging = 1;
         if (!guest && !anonymous && strcasestr(ARG1, "real")) cmdlogging = 1;
      }

      if (!strcasecmp(ARG0, "transfers")) {
         set = 0;
         if (strcasestr(ARG1, "anonymous") && anonymous) set = 1;
         if (strcasestr(ARG1, "guest") && guest) set = 1;
         if (strcasestr(ARG1, "real") && !guest && !anonymous) set = 1;
         if (strcasestr(ARG2, "inbound")) inbound = 1;
         if (strcasestr(ARG2, "outbound")) outbound = 1;
         if (set) log_incoming_xfers = inbound;
         if (set) log_outbound_xfers = outbound;
      }
   }
}

/*************************************************************************/
/* FUNCTION  : acl_getclass                                              */
/* PURPOSE   : Scan the ACL buffer and determine what class user is in   */
/* ARGUMENTS : pointer to buffer to class name, pointer to ACL buffer    */
/*************************************************************************/

int
acl_getclass(classbuf)
char	*classbuf;

{
int		which,
		real = 0,
		anon = 0,
		gues = 0;
struct	aclmember	*entry = NULL;

   while (getaclentry("class", &entry)) {
      if (ARG0) strcpy(classbuf, ARG0);

      for (which = 2; which < MAXARGS && entry->arg[which]; which++) {
         if (anonymous && strcasestr(ARG1, "anonymous") &&
             hostmatch(entry->arg[which])) return(1);

         if (guest && strcasestr(ARG1, "guest") && hostmatch(entry->arg[which]))
            return(1);

         if (!guest && !anonymous && strcasestr(ARG1, "real") &&
             hostmatch(entry->arg[which]))
            return(1);
      }
   }

   *classbuf = (char) NULL;
   return(0);

}

/*************************************************************************/
/* FUNCTION  : acl_getlimit                                              */
/* PURPOSE   : Scan the ACL buffer and determine what limit applies to   */
/*             the user                                                  */
/* ARGUMENTS : pointer class name, pointer to ACL buffer                 */
/*************************************************************************/

int
acl_getlimit(class, msgpathbuf)
char	*class, *msgpathbuf;

{
int		limit;
struct	aclmember	*entry = NULL;

   if (msgpathbuf) *msgpathbuf = NULL;

   /* limit <class> <n> <times> [<message_file>] */
   while (getaclentry("limit", &entry)) {
      if (!ARG0 || !ARG1 || !ARG2) continue;
      if (!strcmp(class, ARG0)) {
         limit = atoi(ARG1);
         if (validtime(ARG2)) {
            if (ARG3 && msgpathbuf) strcpy(msgpathbuf, ARG3);
            return(limit);
         }
      }
   }
   return(-1);
}

/*************************************************************************/
/* FUNCTION  : acl_deny                                                  */
/* PURPOSE   : Scan the ACL buffer and determine a deny command applies  */
/* ARGUMENTS : pointer class name, pointer to ACL buffer                 */
/*************************************************************************/

int
acl_deny(msgpathbuf)
char	*msgpathbuf;

{
struct	aclmember	*entry = NULL;

   if (msgpathbuf) *msgpathbuf = (char) NULL;

   /* deny <addrglob> [<message_file>] */
   while (getaclentry("deny", &entry)) {
      if (!ARG0) continue;
      if (!nameserved && !strcmp(ARG0, "!nameserved")) return(1);
      if (hostmatch(ARG0)) {
         if (ARG1) strcpy(msgpathbuf, entry->arg[1]);
         return(1);
      }
   }
   return(0);
}

/*************************************************************************/
/* FUNCTION  : acl_countusers                                            */
/* PURPOSE   : Check the anonymous FTP access lists to see if this       */
/*             access is permitted.                                      */
/* ARGUMENTS : none                                                      */
/*************************************************************************/

int
acl_countusers(class)
char	*class;

{
int		pidfd,
		count,
		which;
char	pidfile[MAXPATHLEN];
pid_t	buf[MAXUSERS];

   sprintf(pidfile, _PATH_PIDNAMES, class);
   pidfd = open(pidfile, O_RDWR | O_CREAT, 0644);
   if (pidfd < 0) {
      syslog(LOG_ERR, "open of pid file failed: %s", strerror(errno));
      return -1;
   }

   while (flock(pidfd, LOCK_EX)) {
      syslog(LOG_ERR, "sleeping: flock of pid file failed: %s",
         strerror(errno));
      sleep(1);
   }
   lseek(pidfd, 0, L_SET);

   count = 0;

   if (read(pidfd, buf, sizeof(buf)) == sizeof(buf)) {
      for (which = 0; which < MAXUSERS; which++)
         if (buf[which] && !kill(buf[which], 0)) count++;
   }

   flock(pidfd, LOCK_UN);
   close(pidfd);

   return(count);
}

/*************************************************************************/
/* FUNCTION  : acl_join                                                  */
/* PURPOSE   : Add the current process to the list of processes in the   */
/*             specified class.                                          */
/* ARGUMENTS : The name of the class to join                             */
/*************************************************************************/

void
acl_join(class)
char	*class;

{
int		pidfd,
		which,
		avail;
char	pidfile[MAXPATHLEN];
pid_t	buf[MAXUSERS];

   sprintf(pidfile, _PATH_PIDNAMES, class);
   pidfd = open(pidfile, O_RDWR | O_CREAT, 0644);
   if (pidfd < 0) {
      syslog(LOG_ERR, "open of pid file failed: %s", strerror(errno));
      return;
   }

   while (flock(pidfd, LOCK_EX)) {
      syslog(LOG_ERR, "sleeping: flock of pid file failed: %s",
         strerror(errno));
      sleep(1);
   }

   lseek(pidfd, 0, L_SET);
   if (read(pidfd, buf, sizeof(buf)) < sizeof(buf))
      for (which = 0; which < MAXUSERS; buf[which++] = 0)
         continue;

   avail = 0;
   for (which = 0; which < MAXUSERS; which++) {
      if ((buf[which] == 0) || (kill(buf[which], 0) == -1)) {
         avail = which;
         buf[which] = 0;
      }
   }

   buf[avail] = getpid();

   lseek(pidfd, 0, L_SET);
   write(pidfd, buf, sizeof(buf));
   flock(pidfd, LOCK_UN);
   close(pidfd);
   
}

/*************************************************************************/
/* FUNCTION  : pr_mesg                                                   */
/* PURPOSE   : Display a message to the user                             */
/* ARGUMENTS : message code, name of file to display                     */
/*************************************************************************/

int
pr_mesg(msgcode, msgfile)
int	msgcode;
char	*msgfile;

{
FILE	*infile;
char	inbuf[1024],
		outbuf[1024],
		*cr;

   if (msgfile && strlen(msgfile) > 0) {
      infile = fopen(msgfile, "r");
      if (infile) {
         while (fgets(inbuf, 255, infile) != NULL) {
            if ((cr = strchr(inbuf, '\n')) != NULL) *cr = '\0';
            msg_massage(inbuf, outbuf);
            lreply(msgcode, "%s", outbuf);
         }
         fclose(infile);
      }
   }
}

/*************************************************************************/
/* FUNCTION  : access_init                                               */
/* PURPOSE   : Read and parse the access lists to set things up          */
/* ARGUMENTS : none                                                      */
/*************************************************************************/

void
access_init()

{
struct	aclmember	*entry = NULL;
   if (!readacl(_PATH_FTPACCESS)) return;
   (void) parseacl();

   shutdown[0] = '\0';
   entry = (struct aclmember *) NULL;

   if (getaclentry("shutdown", &entry) && ARG0 != NULL)
/*      (void) strncpy(shutdown, ARG0, sizeof(ARG0));*/
      (void) strcpy(shutdown, ARG0);
}

/*************************************************************************/
/* FUNCTION  : access_ok                                                 */
/* PURPOSE   : Check the anonymous FTP access lists to see if this       */
/*             access is permitted.                                      */
/* ARGUMENTS : none                                                      */
/*************************************************************************/

int
access_ok(msgcode)
int	msgcode;

{
char	class[1024],
		msgfile[MAXPATHLEN];
int		limit;
extern int global_userno;

   if (acl_deny(msgfile)) {
      pr_mesg(msgcode, msgfile);
      syslog(LOG_INFO, "ACCESS DENIED (deny command) TO %s [%s]",
         remotehost, remoteaddr);
      return(0);
   }

   /* if user is not in any class, deny access */
   if (!acl_getclass(class)) {
      syslog(LOG_INFO, "ACCESS DENIED (not in any class) TO %s [%s]",
         remotehost, remoteaddr);
      return(0);
   }

   /* if no limits defined, no limits apply -- access OK */
   limit = acl_getlimit(class, msgfile);

   if ((limit == -1) || ((global_userno = acl_countusers(class)) < limit)) {
      acl_join(class);
      return(1);
   } else {
      syslog(LOG_INFO, "ACCESS DENIED (user limit %d; class %s) TO %s [%s]",
         limit, class, remotehost, remoteaddr);
      pr_mesg(msgcode, msgfile);
      return(0);
   }
}
