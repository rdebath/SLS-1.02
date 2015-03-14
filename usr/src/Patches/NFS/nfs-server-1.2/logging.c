/*****************************************************************************\
*									      *
*	File:     logging.c						      *
*	Author:   Don Becker						      *
*	Created:  Mon Jun 12 14:45:58 1989				      *
*	Contents: "print_" functions for NFS transaction logging	      *
*									      *
******************************************************************************/

#include <stdio.h>
#include "nfsd.h"
#include "libc.h"
#include "fh.h"

FILE *debuglog = NULL;
extern char *log_file;

#define LOG_FILE (log_file ? log_file : "/tmp/hnfsd.log")

static char printbuf[512];	/* I should make up a better number than 512 */

/* Functions defined in this file. */
char *print_void(), *print_nfs_fh(), *print_diropargs(),
    *print_readargs(), *print_writeargs(), *print_createargs(),
    *print_renameargs(), *print_linkargs(), *print_symlinkargs(),
    *print_readdirargs(), *print_sattrargs();


static int closemark = 0;
void toggle_logging(void)
{
	if (debuglog == NULL) {
		long tloc;
		if ((debuglog = fopen(LOG_FILE, "w")) == NULL)
			return;
		setlinebuf(debuglog);
		time(&tloc);
		fprintf(debuglog, "\n\nstarting NFS log at %s\n", ctime(&tloc));
	} else {
		closemark = 1;
	}
}
	
void logcall(struct svc_req *rqstp, char *name, char *arg)
{
	int	i;

	if (debuglog == NULL)
		return;
	if (closemark) {
		fclose(debuglog);
		closemark = 0;
		debuglog = NULL;
		return;
	}

	fprintf(debuglog, "%s [%d ", name, rqstp->rq_cred.oa_flavor);
	if (rqstp->rq_cred.oa_flavor == AUTH_UNIX)
	{
		struct authunix_parms *unix_cred;
		struct tm *tm;
		unix_cred = (struct authunix_parms *) rqstp->rq_clntcred;
		tm = localtime(&unix_cred->aup_time);
		fprintf(debuglog, "%d/%d/%d %02d:%02d:%02d %s %d.%d",
			tm->tm_year, tm->tm_mon+1, tm->tm_mday,
			tm->tm_hour, tm->tm_min, tm->tm_sec,
			unix_cred->aup_machname,
			unix_cred->aup_uid,
			unix_cred->aup_gid);
		if (unix_cred->aup_len > 0)
		{
			fprintf(debuglog, "+%d", unix_cred->aup_gids[0]);
			for (i = 1; i < unix_cred->aup_len; i++)
				fprintf(debuglog, ",%d",unix_cred->aup_gids[i]);
		}
	}
	fprintf(debuglog, "]\n\t%s\n", arg);
}


char *print_void()
{
    return "";
}

char *print_nfs_fh(nfs_fh *argp)
{
    return fh_pr(argp);
}

char *print_sattrargs(sattrargs *argp)
{
    sprintf(printbuf, "fh:%s m:%0o u/g:%d/%d size:%d atime:%#x mtime:%#x",
	    fh_pr(&argp->file), argp->attributes.mode,
	    argp->attributes.uid, argp->attributes.gid,
	    argp->attributes.size,
	    argp->attributes.atime.seconds, argp->attributes.mtime.seconds);
    return printbuf;
}

char *print_diropargs(diropargs *argp)
{
    sprintf(printbuf, "fh:%s n:%s", fh_pr(&(argp->dir)), argp->name);
    return printbuf;
}

char *print_readargs(readargs *argp)
{
    sprintf(printbuf, "%s: %d bytes at %d",
	    fh_pr(&argp->file), argp->count, argp->offset);
    return printbuf;
}

char *print_writeargs(writeargs *argp)
{
    sprintf(printbuf, "%s: %d bytes at %d",
	    fh_pr(&argp->file), argp->data.data_len, argp->offset);
    return printbuf;
}

char *print_createargs(createargs *argp)
{
    sprintf(printbuf, "fh:%s n:%s m:%0o u/g:%d/%d size:%d atime:%#x mtime:%#x",
	    fh_pr(&argp->where.dir), argp->where.name,
	    argp->attributes.mode, argp->attributes.uid,
	    argp->attributes.gid, argp->attributes.size,
	    argp->attributes.atime.seconds, argp->attributes.mtime.seconds);
    return printbuf;
}

char *print_renameargs(renameargs *argp)
{
    return "";
}

char *print_linkargs(linkargs *argp)
{
    return "";
}

char *print_symlinkargs(symlinkargs *argp)
{
    return "";
}

char *print_readdirargs(readdirargs *argp)
{
    return fh_pr(&argp->dir);
}
