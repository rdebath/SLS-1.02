/*
 * mountd	This program handles RPC "NFS" mount requests.
 *
 * Usage:	[rpc.]mountd [-dpnv] [-f authfile]
 *
 * Version:	@(#)mountd.c	1.5	93/04/10
 *
 * Authors:	Mark A. Shand, May 1988
 *		Donald J. Becker, <becker@super.org>
 *		Rick Sladkey, <jrs@world.std.com>
 *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *
 *		Copyright 1988 Mark A. Shand
 *		This software maybe be used for any purpose provided
 *		the above copyright notice is retained.  It is supplied
 *		as is, with no warranty expressed or implied.
 */
#include "nfsd.h"


#ifndef OLD_RPCGEN
#define mountproc_null_1		mountproc_null_1_svc
#define mountproc_mnt_1			mountproc_mnt_1_svc
#define mountproc_dump_1		mountproc_dump_1_svc
#define mountproc_umnt_1		mountproc_umnt_1_svc
#define mountproc_umntall_1		mountproc_umntall_1_svc
#define mountproc_export_1		mountproc_export_1_svc
#define mountproc_exportall_1		mountproc_exportall_1_svc
#endif


static char *Version = "@(#) mountd 1.5	93/04/10";


char argbuf[MNTPATHLEN + 1];
char *auth_file = NULL;
#ifdef OLD_RPCGEN
int _rpcpmstart = 0;
#endif


static _PRO(void usage, (void));


/* In mount_svc.c: */
extern _PRO(void mount_run, (void));


/* The NULL request handler. */
void *mountproc_null_1(argp, rqstp)
void *argp;
struct svc_req *rqstp;
{
	static char res;

	memset(&res, '\0', sizeof(res));
	log_call(rqstp, "mountproc_null_1", "");
	return ((void *) &res);
}


fhstatus *mountproc_mnt_1(argp, rqstp)
dirpath *argp;
struct svc_req *rqstp;
{
	static fhstatus res;
	struct stat stbuf;
	clnt_param *cp;
	char nargbuf[MNTPATHLEN + 1];
	int i;

	memset(&res, '\0', sizeof(res));
	sprintf(argbuf, "%s", *argp);
	log_call(rqstp, "mountproc_mnt_1", argbuf);

	/* The mount request must be an absolute pathname. */
	if (argbuf[0] != '/') {
		res.fhs_status = NFSERR_ACCES;
		return (&res);
	}

	/* It is important to resolve symlinks before checking permissions. */
	for (i = 0; i < 16; i++) {
		if (lstat(argbuf, &stbuf) < 0) {
			res.fhs_status = nfs_errno();
			return (&res);
		}
		if (!(stbuf.st_mode & S_IFLNK))
			break;
		if (readlink(argbuf, nargbuf, MNTPATHLEN) < 0) {
			res.fhs_status = nfs_errno();
			return (&res);
		}
		nargbuf[MNTPATHLEN] = 0;
		if (nargbuf[0] == '/')
			strcpy(argbuf, nargbuf);
		else {
			*(strrchr(argbuf, '/') + 1) = '\0';
			if (strlen(argbuf) + strlen(nargbuf) > MNTPATHLEN) {
				res.fhs_status = NFSERR_NAMETOOLONG;
				return (&res);
			}
			strcat(argbuf, nargbuf);
		}
	}

	if (i == 16)
		res.fhs_status = NFSERR_ACCES;
	else if (((cp = auth_clnt(rqstp, argbuf)) == NULL))
		res.fhs_status = NFSERR_ACCES;
	else if ((stbuf.st_mode & S_IFMT) != S_IFDIR)
		res.fhs_status = NFSERR_NOTDIR;
	else
		res.fhs_status = fh_create((nfs_fh *)
			&res.fhstatus_u.fhs_fhandle, argbuf);
	return (&res);
}


mountlist *mountproc_dump_1(argp, rqstp)
void *argp;
struct svc_req *rqstp;
{
	static mountlist res;

	memset(&res, '\0', sizeof(res));
	log_call(rqstp, "mountproc_dump_1", "");
	return (&res);
}


void *mountproc_umnt_1(argp, rqstp)
dirpath *argp;
struct svc_req *rqstp;
{
	static char res;

	memset(&res, '\0', sizeof(res));
	sprintf(argbuf, "%s", *argp);
	log_call(rqstp, "mountproc_umnt_1", argbuf);
	return ((void *) &res);
}


void *mountproc_umntall_1(argp, rqstp)
void *argp;
struct svc_req *rqstp;
{
	static char res;

	memset(&res, '\0', sizeof(res));
	log_call(rqstp, "mountproc_umntall_1", "");
	return ((void *) &res);
}


exports *mountproc_export_1(argp, rqstp)
void *argp;
struct svc_req *rqstp;
{
	static exports res;

	memset(&res, '\0', sizeof(res));
	log_call(rqstp, "mountproc_export_1", "");
	res = export_list;
	return (&res);
}


exports *mountproc_exportall_1(argp, rqstp)
void *argp;
struct svc_req *rqstp;
{
	log_call(rqstp, "mountproc_exportall_1", "");
	return (mountproc_export_1(argp, rqstp));
}


static void usage()
{
	dprintf(0, "Usage: mountd [-dpnv] [-f authfile]\n");
	(void) closelog();
	exit(-1);
}


void mountd_init(argc, argv)
int argc;
char **argv;
{
	int c;
	extern int getopt(), optind, opterr;
	extern char *optarg;

	/* Parse the command line options and arguments. */
	opterr = 0;
	while ((c = getopt(argc, argv, "df:pv")) != EOF)
		switch (c) {
		case 'd':
		case 'v':
			toggle_logging(0);
			break;
		case 'f':
			auth_file = optarg;
			break;
		case 'n':
			allow_non_root = 1;
			break;
		case 'p':
			promiscuous = 1;
			break;
		case '?':
		default:
			usage();
		}

	/* No more arguments allowed. */
	if (optind != argc)
		usage();

#ifdef OLD_RPCGEN
#ifndef RPC_SVC_FG
	/* We first fork off a child. */
	if ((c = fork()) > 0)
		exit(0);
	if (c < 0) {
		fprintf(stderr, "mountd: cannot fork: %s\n", strerror(errno));
		exit(-1);
	}
	/* Now we remove ourselves from the foreground. */
	(void) close(0);
	(void) close(1);
	(void) close(2);
	if ((c = open("/dev/tty", O_RDWR)) >= 0) {
		(void) ioctl(c, TIOCNOTTY, (char *) NULL);
		(void) close(c);
	}
#endif /* not RPC_SVC_FG */
#endif /* OLD_RPCGEN */

	/* Initialize logging. */
	log_open("mountd");

	/* Initialize the FH module. */
	fh_init();

	/* Initialize the AUTH module. */
	auth_init(auth_file);

	/* Enable the LOG toggle with a signal. */
	(void) signal(SIGUSR1, toggle_logging);
}
