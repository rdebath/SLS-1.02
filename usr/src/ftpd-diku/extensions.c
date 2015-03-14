#include <stdio.h>
#include <errno.h>
#include <malloc.h>
#include <string.h>
#include <syslog.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/param.h>
#include <pwd.h>
#ifdef SHADOW_PWD
#	include <shadow.h>
#endif

/* #include <arpa/ftp.h> */
#include "support/ftp.h"

#include "pathnames.h"
#include "extensions.h"
#include "support/ftw.h"

extern	int	fnmatch(),
		type,
		transflag,
		autospout_free,
		data;
extern	char	**glob(),
		*globerr,
		remotehost[],
		hostname[],
		*autospout,
		shutdown[];

char	shuttime[30],
		denytime[30],
		disctime[30];

extern	FILE	*dataconn();
FILE	*dout;

time_t	newer_time;

int		show_fullinfo;

/*************************************************************************/
/* FUNCTION  : msg_massage                                               */
/* PURPOSE   : Scan a message line for magic cookies, replacing them as  */
/*             needed.                                                   */
/* ARGUMENTS : pointer input and output buffers                          */
/*************************************************************************/

int
msg_massage(inbuf, outbuf)
char	*inbuf, *outbuf;

{
char	*inptr = inbuf;
char	*outptr = outbuf;
char	buffer[MAXPATHLEN];
time_t	curtime;
int		limit;
extern	struct	passwd	*pw;
extern int global_userno;

   (void) time(&curtime);
   (void) acl_getclass(buffer);

   limit = acl_getlimit(buffer, NULL);

   while (*inptr) {
      if (*inptr != '%')
         *outptr++ = *inptr;
      else {
         switch (*++inptr) {
/* broken*/
            case 'N':
/*		{FILE *f=fopen("/tmp/ftplog","a");if(!f)syslog(LOG_ERR,"no log %m");fclose(f);}
               sprintf(outptr, "%d", acl_countusers(buffer));*/
               sprintf(outptr, "%d", ++global_userno);
               break;
            case 'M':
               sprintf(outptr, "%d", limit);
               break;

            case 'T':
               strncpy(outptr, ctime(&curtime), 24);
               *(outptr + 24) = NULL;
               break;

            case 'F':
		sprintf(outptr, "%ld",myfree());
		break;
            case 'C':
               (void) getwd(outptr);
               break;

            case 'R':
               strcpy(outptr, remotehost);
               break;

            case 'L':
               strcpy(outptr, hostname);
               break;

            case 'U':
               strcpy(outptr, pw->pw_name);
               break;

            case 's':
               strncpy(outptr, shuttime, 24);
               *(outptr + 24) = NULL;
               break;

            case 'd':
               strncpy(outptr, disctime, 24);
               *(outptr + 24) = NULL;
               break;

            case 'r':
               strncpy(outptr, denytime, 24);
               *(outptr + 24) = NULL;
               break;

            case '%':
               *outptr++ = '%';
               *outptr = '\0';
               break;

            default:
               *outptr++ = '%';
               *outptr++ = '?';
               *outptr = '\0';
               break;
         }
         while (*outptr) outptr++;
      }
      inptr++;
   }
   *outptr = NULL;
}

/*************************************************************************/
/* FUNCTION  : cwd_beenhere                                              */
/* PURPOSE   : Return 1 if the user has already visited this directory   */
/*             via CWD.                                                  */
/* ARGUMENTS : a power-of-two directory function code (README, MESSAGE)  */
/*************************************************************************/

int
cwd_beenhere(dircode)
int	dircode;
{
struct	dirlist {
		struct	dirlist	*next;
		int				dircode;
		char			dirname[1];
};

static	struct	dirlist	*head = NULL;
struct	dirlist	*curptr;
char	cwd[MAXPATHLEN];

   (void) getwd(cwd);
   for (curptr = head; curptr != NULL; curptr = curptr->next)
      if (strcmp(curptr->dirname, cwd) == NULL) {
         if (!(curptr->dircode & dircode)) {
            curptr->dircode |= dircode;
            return(0);
         }
         return(1);
      }
   
   curptr = (struct dirlist *) malloc(strlen(cwd)+1+sizeof(struct dirlist));

   if (curptr != NULL) {
      curptr->next = head;
      head = curptr;
      curptr->dircode = dircode;
      strcpy(curptr->dirname, cwd);
   }

   return(0);
}

/*************************************************************************/
/* FUNCTION  : show_banner                                               */
/* PURPOSE   : Display a banner on the user's terminal before login      */
/* ARGUMENTS : reply code to use                                         */
/*************************************************************************/

void
show_banner(msgcode)
int	msgcode;

{
char	*crptr,
		**filelist,
		linebuf[1024],
		outbuf[1024],
		cwd[MAXPATHLEN];
int		show,
		clock;
struct	stat	buf;
struct	aclmember	*entry = NULL;
FILE	*infile;

   /* banner <path> */
   while (getaclentry("banner", &entry)) {
      if (ARG0 && strlen(ARG0) > 0) {
         infile = fopen(ARG0, "r");
         if (infile) {
            while (fgets(linebuf, 255, infile) != NULL) {
               if ((crptr = strchr(linebuf, '\n')) != NULL) *crptr = '\0';
               msg_massage(linebuf, outbuf);
               lreply(msgcode, "%s", outbuf);
            }
            fclose(infile);
            lreply(msgcode, "");
         }
      }
   }
}

/*************************************************************************/
/* FUNCTION  : show_message                                              */
/* PURPOSE   : Display a message on the user's terminal if the current   */
/*             conditions are right                                      */
/* ARGUMENTS : reply code to use, LOGIN|CMD                              */
/*************************************************************************/

void
show_message(msgcode, mode)
int	msgcode, mode;

{
char	*crptr,
		linebuf[1024],
		outbuf[1024],
		cwd[MAXPATHLEN];
int		show;
struct	aclmember	*entry = NULL;
FILE	*infile;

   if (cwd_beenhere(1) != 0) return;

   (void) getwd(cwd);

   /* message <path> [<when>] */
   while (getaclentry("message", &entry)) {
      if (!ARG0) continue;
      show = 0;

      if (mode == LOGIN && (!ARG1 || !strcasecmp(ARG1, "login")))
         show++;
      if (mode == CWD && ARG1 && !strncasecmp(ARG1,"cwd=",4) &&
          !strcmp((ARG1)+4, cwd) || *(ARG1+4) == '*' ||
          fnmatch((ARG1)+4, cwd, FNM_PATHNAME)) show++;

      if (show && strlen(ARG0) > 0) {
         infile = fopen(ARG0, "r");
         if (infile) {
            while (fgets(linebuf, 255, infile) != NULL) {
               if ((crptr = strchr(linebuf, '\n')) != NULL) *crptr = '\0';
               msg_massage(linebuf, outbuf);
               lreply(msgcode, "%s", outbuf);
            }
            fclose(infile);
            lreply(msgcode, "");
         }
      }
   }
}

/*************************************************************************/
/* FUNCTION  : show_readme                                               */
/* PURPOSE   : Display a message about a README file to the user if the  */
/*             current conditions are right                              */
/* ARGUMENTS : pointer to ACL buffer, reply code, LOGIN|CWD              */
/*************************************************************************/

void
show_readme(code, mode)
int	code, mode;

{
char	**filelist,
		cwd[MAXPATHLEN];
int		show,
		clock,
		days;

struct	stat		buf;
struct	tm			*tp;
struct	aclmember	*entry = NULL;

   if (cwd_beenhere(2) != 0) return;

   (void) getwd(cwd);

   /* readme  <path> {<when>} */
   while (getaclentry("readme", &entry)) {
      if (!ARG0) continue;
      show = 0;

      if (mode == LOGIN && (!ARG1 || !strcasecmp(ARG1, "login")))
         show++;
      if (mode == CWD && ARG1 && !strncasecmp(ARG1, "cwd=", 4)
          && (!strcmp((ARG1)+4, cwd) || *(ARG1+4) == '*' ||
          fnmatch((ARG1)+4, cwd, FNM_PATHNAME))) show++;

      if (show) {
         globerr = NULL;
         filelist = glob(ARG0);
         if (!globerr) {
            while (filelist && *filelist) {
               errno = 0;
               if (!stat(*filelist, &buf)) {
                  lreply(code, "Please read the file %s", *filelist);
                  (void) time(&clock);
                  tp = localtime(&clock);
                  days = 365 * tp->tm_year + tp->tm_yday;
                  tp = localtime(&buf.st_mtime);
                  days -= 365 * tp->tm_year + tp->tm_yday;
                  lreply(code, "  it was last modified on %.24s - %d days ago",
                     ctime(&buf.st_mtime), days);
               }
               filelist++;
            }
         }
      }
   }
}

/*************************************************************************/
/* FUNCTION  : deny_badxfertype                                          */
/* PURPOSE   : If user is in ASCII transfer mode and tries to retrieve a */
/*             binary file, abort transfer and display appropriate error */
/* ARGUMENTS : message code to use for denial, path of file to check for */
/*             binary contents or NULL to assume binary file             */
/*************************************************************************/

int
deny_badasciixfer(msgcode, filepath)
int		msgcode;
char	*filepath;

{

   if (type == TYPE_A && !*filepath) {
      reply(msgcode, "This is a BINARY file, using ASCII mode to transfer will corrupt it.");
      return(1);
   }
   /* The hooks are here to prevent transfers of actual binary files, not
    * just TAR or COMPRESS mode files...
    */
   return(0);
}

/*************************************************************************/
/* FUNCTION  : is_shutdown                                               */
/* PURPOSE   :                                                           */
/* ARGUMENTS :                                                           */
/*************************************************************************/

int
is_shutdown(quiet)
int	quiet;

{
static	struct	tm		tmbuf;
static	struct	stat	s_last;
static	time_t	last = 0,
				shut = 0,
				deny = 0,
				disc = 0;

static	char	text[2048];

struct	stat			s_cur;

FILE	*fp;

int		deny_off, disc_off;

time_t	curtime = time(NULL);

char	buf[1024], linebuf[1024];

/*
fprintf (stderr, "is_shutdown(): BEG shutdown=[%s]\n", shutdown);
fflush(stderr);
*/
   return 0;	/* IL 09-04-93 */


   if (shutdown[0] == '\0' || stat(shutdown, &s_cur)) return(0);

   if (s_last.st_mtime != s_cur.st_mtime) {
      s_last = s_cur;

      fp = fopen(shutdown, "r");
      if (fp == NULL) return(0);
      fgets(buf, sizeof(buf), fp);
      if (sscanf(buf, "%d %d %d %d %d %d %d", &tmbuf.tm_year, &tmbuf.tm_mon,
         &tmbuf.tm_mday, &tmbuf.tm_hour, &tmbuf.tm_min, &deny, &disc) != 7) {
         return(0);
      }

      deny_off = 3600 * (deny / 100) + 60 * (deny % 100);
      disc_off = 3600 * (disc / 100) + 60 * (disc % 100);

      tmbuf.tm_year -= 1900;
      tmbuf.tm_isdst = -1;
      shut = mktime(&tmbuf);
      strcpy(shuttime, ctime(&shut));

      disc = shut - disc_off;
      strcpy(disctime, ctime(&disc));

      deny = shut - deny_off;
      strcpy(denytime, ctime(&deny));

      text[0] = '\0';

      while (fgets(buf, sizeof(buf), fp) != NULL) {
         msg_massage(buf, linebuf);
         if ((strlen(text) + strlen(linebuf)) < sizeof(text))
            strcat(text, linebuf);
      }

      (void) fclose(fp);
   }

   /* if last == 0, then is_shutdown() only called with quiet == 1 so far */
   if (last == 0 && !quiet) {
      autospout = text;		/* warn them for the first time */
      autospout_free = 0;
      last = curtime;
   }
   /* if past disconnect time, tell caller to drop 'em */
   if (curtime > disc) {
/*
fprintf (stderr, "is_shutdown(): curtime=[%ld] > disc=[%ld] = RET 1\n", 
	curtime, disc);
fflush(stderr);
*/
       return(1);
   }
   
   /* if less than 60 seconds to disconnection, warn 'em continuously */
   if (curtime > (disc - 60) && !quiet) {
      autospout = text;
      autospout_free = 0;
      last = curtime;
   }

   /* if less than 15 minutes to disconnection, warn 'em every 5 mins */
   if (curtime > (disc - 60*15)) {
      if ((curtime - last) > (60*5) && !quiet) {
         autospout = text;
         autospout_free = 0;
         last = curtime;
      }
   }

   /* if less than 24 hours to disconnection, warn 'em every 30 mins */
   if (curtime < (disc - 24*60*60) && !quiet) {
      if ((curtime - last) > (60*30)) {
         autospout = text;
         autospout_free = 0;
         last = curtime;
      }
   }

   /* if more than 24 hours to disconnection, warn 'em every 60 mins */
   if (curtime > (disc - 24*60*60) && !quiet) {
      if ((curtime - last) >= (24*60*60)) {
         autospout = text;
         autospout_free = 0;
         last = curtime;
      }
   }
/*
fprintf (stderr, "isshutdown(): END\n");
fflush(stderr);
*/
   return(0);
}

newer(date, path, showlots)
char	*date, *path;
int		showlots;

{
struct	tm tm;

   if (sscanf(date, "%04d%02d%02d%02d%02d%02d",
       &tm.tm_year, &tm.tm_mon, &tm.tm_mday,
       &tm.tm_hour, &tm.tm_min, &tm.tm_sec) == 6) {

      tm.tm_year -= 1900;
      tm.tm_mon--;
      tm.tm_isdst = -1;
      newer_time = mktime(&tm);
      dout = dataconn("file list", (off_t)-1, "w", 0);
      transflag++;
      if (dout != NULL) {
         int  check_newer();

         show_fullinfo = showlots;
         treewalk(path, check_newer, -1, NULL);

         if (ferror(dout) != 0)
            perror_reply(550, "Data connection");
         else
            reply(226, "Transfer complete.");

         (void) fclose(dout);
         data = -1;
      }
   } else
      reply(501, "Bad DATE format");
   transflag = 0;
}

check_newer(path, st, flag)
char *path;
struct stat *st;
int flag;
{

   if (st->st_mtime > newer_time) {
      if (show_fullinfo != 0) {
         if (flag == FTW_F || flag == FTW_D) {
            fprintf(dout, "%s %d %d %s", flag == FTW_F ? "F" : "D",
               st->st_size, st->st_mtime, path);
         }
      } else if (flag == FTW_F) fprintf(dout, "%s", path);
   }
   return 0;
}
