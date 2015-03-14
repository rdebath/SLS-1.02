/*
 * logging	This module handles the logging of requests.
 *
 * Version:	@(#)logging.c	1.5	93/04/10
 *
 * TODO:	Merge the two "XXX_log() calls.
 *
 * Authors:	Donald J. Becker, <becker@super.org>
 *		Rick Sladkey, <jrs@world.std.com>
 *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *
 *		This software maybe be used for any purpose provided
 *		the above copyright notice is retained.  It is supplied
 *		as is, with no warranty expressed or implied.
 */

#include "nfsd.h"
#ifdef SYSLOG
#include <syslog.h>
#else
#define LOG_FILE	"/etc/%s.log"
#endif


static char printbuf[1024];		/* local print buffer		*/
static int logging = 0;			/* enable/disable DEBUG logs	*/
#ifndef SYSLOG
static char log_name[32];		/* name of this program		*/
static FILE *log_fp = (FILE *)NULL;	/* fp for the log file		*/
#endif


void log_open(progname)
char *progname;
{
#ifdef SYSLOG
	openlog(progname, LOG_PID, LOG_DAEMON);
#else
	char path[1024];

	sprintf(path, LOG_FILE, progname);
	log_fp = fopen(path, "a");
	if (log_fp == (FILE *) NULL)
		return;
	(void) setbuf(log_fp, (char *) NULL);
	sprintf(log_name, "%s[%d]", progname, getpid());
#endif
}


void toggle_logging(sig)
int sig;
{
	(void) sig;
	logging = 1 - logging;
}


/* Write something to the system logfile. */
void dprintf(int level, const char *fmt,...)
{
	char buff[1024];
	va_list args;
#ifndef SYSLOG
	time_t now;
	struct tm *tm;
#endif

	va_start(args, fmt);
	vsprintf(buff, fmt, args);
	va_end(va);

	if ((level == 0) || (level == 1 && logging == 1)) {
#ifdef SYSLOG
		(void) syslog(LOG_ERR, buff);
#else
		if (log_fp == (FILE *) NULL)
			return;
		(void) time(&now);
		tm = localtime(&now);
		fprintf(log_fp, "%s %02d/%02d/%02d %02d:%02d %s",
		      log_name, tm->tm_mon + 1, tm->tm_mday, tm->tm_year,
			tm->tm_hour, tm->tm_min, buff);
#endif
	}
}


/* Log an incoming call. */
void log_call(rqstp, xname, arg)
struct svc_req *rqstp;
char *xname;
char *arg;
{
	char buff[1024];
	register char *sp;
	int i;

	if (logging == 0)
		return;

	sp = buff;
	sprintf(sp, "%s [%d ", xname, rqstp->rq_cred.oa_flavor);
	sp += strlen(sp);
	if (rqstp->rq_cred.oa_flavor == AUTH_UNIX) {
		struct authunix_parms *unix_cred;
		struct tm *tm;

		unix_cred = (struct authunix_parms *) rqstp->rq_clntcred;
		tm = localtime(&unix_cred->aup_time);
		sprintf(sp, "%d/%d/%d %02d:%02d:%02d %s %d.%d",
			tm->tm_year, tm->tm_mon + 1, tm->tm_mday,
			tm->tm_hour, tm->tm_min, tm->tm_sec,
			unix_cred->aup_machname,
			unix_cred->aup_uid,
			unix_cred->aup_gid);
		sp += strlen(sp);
		if ((int) unix_cred->aup_len > 0) {
			sprintf(sp, "+%d", unix_cred->aup_gids[0]);
			sp += strlen(sp);
			for (i = 1; i < unix_cred->aup_len; i++) {
				sprintf(sp, ",%d", unix_cred->aup_gids[i]);
				sp += strlen(sp);
			}
		}
	}
	dprintf(1, "%s]\n\t%s\n", buff, arg);
}


char *pr_void()
{
	return ("");
}


char *pr_nfs_fh(nfs_fh * argp)
{
	return (fh_pr(argp));
}


char *pr_sattrargs(sattrargs * argp)
{
	sprintf(printbuf, "fh:%s m:%0o u/g:%d/%d size:%d atime:%#x mtime:%#x",
		fh_pr(&argp->file), argp->attributes.mode,
		argp->attributes.uid, argp->attributes.gid,
		argp->attributes.size,
	 argp->attributes.atime.seconds, argp->attributes.mtime.seconds);
	return (printbuf);
}


char *pr_diropargs(diropargs * argp)
{
	sprintf(printbuf, "fh:%s n:%s", fh_pr(&(argp->dir)), argp->name);
	return (printbuf);
}


char *pr_readargs(readargs * argp)
{
	sprintf(printbuf, "%s: %d bytes at %d",
		fh_pr(&argp->file), argp->count, argp->offset);
	return (printbuf);
}

char *pr_writeargs(writeargs * argp)
{
	sprintf(printbuf, "%s: %d bytes at %d",
		fh_pr(&argp->file), argp->data.data_len, argp->offset);
	return (printbuf);
}


char *pr_createargs(createargs * argp)
{
	sprintf(printbuf, "fh:%s n:%s m:%0o u/g:%d/%d size:%d atime:%#x mtime:%#x",
		fh_pr(&argp->where.dir), argp->where.name,
		argp->attributes.mode, argp->attributes.uid,
		argp->attributes.gid, argp->attributes.size,
	 argp->attributes.atime.seconds, argp->attributes.mtime.seconds);
	return (printbuf);
}


char *pr_renameargs(renameargs * argp)
{
	return ("");
}


char *pr_linkargs(linkargs * argp)
{
	return ("");
}


char *pr_symlinkargs(symlinkargs * argp)
{
	return ("");
}


char *pr_readdirargs(readdirargs * argp)
{
	return (fh_pr(&argp->dir));
}
