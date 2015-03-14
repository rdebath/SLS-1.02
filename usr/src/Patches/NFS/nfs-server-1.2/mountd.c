/* UNFSD - copyright Mark A Shand, May 1988.
 * This software maybe be used for any purpose provided
 * the above copyright notice is retained.  It is supplied
 * as is, with no warranty expressed or implied.
 */

/* authentication and export list support - rick sladkey */

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <rpc/rpc.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <syslog.h>
#include <getopt.h>
#include "mount.h"
#include "nfs_prot.h"
#include "fh.h"
#include "nfsd.h"
#include "auth.h"

#ifdef DEBUG
#include <ctype.h>
#include <stdio.h>
static FILE *debuglog = NULL;
#endif

static char *pname = "mountd";
static char argbuf[512];
static int log_flag = 0;
static char *log_file = "/tmp/mountd.log";
static char *auth_file = NULL;

void logcall(xname, arg, rqstp)
char	*xname;
char	*arg;
struct svc_req	*rqstp;
{
	int	i;

#ifdef DEBUG
	if (!log_flag)
		return;
	if (debuglog == NULL)
	{
		unsigned long tloc;
		if ((debuglog = fopen(log_file, "a+")) == NULL) {
			syslog(LOG_DAEMON|LOG_ERR, "Cannot open log file: %s",
				log_file);
			log_flag = 0;
			return;
		}
		time(&tloc);
		fprintf(debuglog, "\n\nstarting %s at %s\n", pname,
			ctime(&tloc));
	}
	fprintf(debuglog, "%s [ %d %d ",
		xname,
		rqstp->rq_cred.oa_flavor,
		rqstp->rq_cred.oa_length);
	if (rqstp->rq_cred.oa_flavor == AUTH_UNIX)
	{
		struct authunix_parms *unix_cred;
		unix_cred = (struct authunix_parms *) rqstp->rq_clntcred;
		fprintf(debuglog, "%.24s %s %d.%d ",
			ctime(&unix_cred->aup_time),
			unix_cred->aup_machname,
			unix_cred->aup_uid,
			unix_cred->aup_gid);
		if (unix_cred->aup_len > 0)
		{
			for (i = 0; i < unix_cred->aup_len; i++)
				fprintf(debuglog, "%c%d", (i==0?'(':','),
					unix_cred->aup_gids[i]);
			fprintf(debuglog, ") ");
		}
	}
	fprintf(debuglog, "]\n\t%s\n", arg);
	fflush(debuglog);
#endif /* DEBUG */
}

#ifdef OLD_RPCGEN
int _rpcpmstart = 0;
#endif

void
mountd_init(int argc, char **argv)
{
	int c;

#ifdef OLD_RPCGEN
#ifndef	RPC_SVC_FG
	int fd;

	if (fork())
		exit(0);
	close(0);
	close(1);
	close(2);
	if ((fd = open("/dev/tty", 2)) >= 0)
	{
		ioctl(fd, TIOCNOTTY, (char *)0);
		(void) close(fd);
	}
#endif /* RPC_SVC_FG */
#endif /* OLD_RPGEN */

	while ((c = getopt(argc, argv, "df:l:pn")) != EOF) {
		switch (c) {
		case 'd':
			log_flag++;
			break;
		case 'f':
			auth_file = optarg;
			break;
		case 'l':
			log_file = optarg;
			break;
		case 'p':
			promiscuous = 1;
			break;
		case 'n':
			allow_non_root = 1;
			break;
		case '?':
			syslog(LOG_DAEMON|LOG_ERR, "Unknown option '-%c'", c);
		}
		if (argv[optind] != NULL)
			syslog(LOG_DAEMON|LOG_ERR, "Bad argument -- %s",
				argv[optind]);
	}
	fh_init();
	init_auth(auth_file);
}

#ifndef OLD_RPCGEN

#define mountproc_null_1	mountproc_null_1_svc
#define mountproc_mnt_1		mountproc_mnt_1_svc
#define mountproc_dump_1	mountproc_dump_1_svc
#define mountproc_umnt_1	mountproc_umnt_1_svc
#define mountproc_umntall_1	mountproc_umntall_1_svc
#define mountproc_export_1	mountproc_export_1_svc
#define mountproc_exportall_1	mountproc_exportall_1_svc

#endif

void *
mountproc_null_1(argp, rqstp)
	void *argp;
	struct svc_req *rqstp;
{
	static char res;

	bzero(&res, sizeof(res));
	logcall("mountproc_null_1", "", rqstp);
	return ((void *)&res);
}


fhstatus *
mountproc_mnt_1(argp, rqstp)
	dirpath *argp;
	struct svc_req *rqstp;
{
	static fhstatus res;
	struct stat stbuf;
	clnt_param *cp;

	bzero(&res, sizeof(res));
	sprintf(argbuf, "%s", *argp);
	logcall("mountproc_mnt_1", argbuf, rqstp);
	if (((cp = auth_clnt(rqstp, argbuf)) == NULL))
		res.fhs_status = NFSERR_ACCES;
	else if (stat(*argp, &stbuf) < 0)
		res.fhs_status = nfs_errno();
	else if ((stbuf.st_mode & S_IFMT) != S_IFDIR)
		res.fhs_status = NFSERR_NOTDIR;
	else {
		res.fhs_status = fh_create((nfs_fh *)
			&(res.fhstatus_u.fhs_fhandle),*argp);
	}
	return (&res);
}


mountlist *
mountproc_dump_1(argp, rqstp)
	void *argp;
	struct svc_req *rqstp;
{
	static mountlist res;

	bzero(&res, sizeof(res));
	logcall("mountproc_dump_1", "", rqstp);
	return (&res);
}


void *
mountproc_umnt_1(argp, rqstp)
	dirpath *argp;
	struct svc_req *rqstp;
{
	static char res;

	bzero(&res, sizeof(res));
	sprintf(argbuf, "%s", *argp);
	logcall("mountproc_umnt_1", argbuf, rqstp);
	return ((void *)&res);
}


void *
mountproc_umntall_1(argp, rqstp)
	void *argp;
	struct svc_req *rqstp;
{
	static char res;

	bzero(&res, sizeof(res));
	logcall("mountproc_umntall_1", "", rqstp);
	return ((void *)&res);
}


exports *
mountproc_export_1(argp, rqstp)
	void *argp;
	struct svc_req *rqstp;
{
	static exports res;

	bzero(&res, sizeof(res));
	logcall("mountproc_export_1", "", rqstp);
	res = export_list;
	return (&res);
}


exports *
mountproc_exportall_1(argp, rqstp)
	void *argp;
	struct svc_req *rqstp;
{
	logcall("mountproc_exportall_1", "", rqstp);
	return (mountproc_export_1(argp, rqstp));
}
