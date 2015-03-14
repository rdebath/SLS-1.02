/*
 * nfsd		This program handles RPC "NFS" data requests.
 *
 * Usage:	[rpc.]nfsd [-dpnv] [-f authfile]
 *
 * Version:	@(#)nfsd.c	1.5	93/04/10
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
#include <sys/vfs.h>
#include <sys/file.h>
#include <sys/dir.h>		/* Used only in readdir() */


#define _RPCSVC_CLOSEDOWN 120


static char *Version = "@(#) nfsd 1.5	93/04/10";


static char iobuf[NFS_MAXDATA];
static char pathbuf[NFS_MAXPATHLEN + 1];
static char pathbuf_1[NFS_MAXPATHLEN + 1];
int _rpcpmstart = 0;
int _rpcsvcdirty = 0;
int _rpcfdtype = 0;


extern clnt_param *cp;		/* only for the option list	*/


extern _PRO(void nfs_dispatch, (struct svc_req * rqstp, SVCXPRT * transp));
static _PRO(nfsstat build_path, (char *buf, diropargs * da));
static _PRO(int makesock, (int port, int proto, int socksz));
static _PRO(void closedown, (int sig));
static _PRO(void usage, (void));


extern void xdr_free();		/* fill this in later		*/
extern void pmap_unset();	/* why here???			*/


static inline nfsstat build_path(buf, da)
char *buf;
diropargs *da;
{
	nfsstat status;
	char *path, *name;

	if ((path = fh_path(&(da->dir), &status)) == 0)
		return (NFSERR_STALE);

	while (*path)		/* strcpy(buf, path); */
		*buf++ = *path++;
	*buf++ = '/';		/* strcat(buf, "/");  */
	name = da->name;
	while (*name)		/* strcat(pathbuf, argp->where.name); */
		*buf++ = *name++;
	*buf = '\0';
	return (NFS_OK);
}


/*
 * The "wrappers" of the following functions came from `rpcgen -l nfs_prot.x`.
 * This normally generates the client routines, but it provides nice
 * prototypes for the server routines also.
 */
#define CLIENT struct svc_req


int nfsd_nfsproc_null_2(argp)
void *argp;
{
	return (0);
}


int nfsd_nfsproc_getattr_2(argp)
nfs_fh *argp;
{
	return (getattr(argp, &result.attrstat.attrstat_u.attributes, NULL));
}


int nfsd_nfsproc_setattr_2(argp)
sattrargs *argp;
{
	nfsstat status;
	char *path;
	struct stat buf;

	if ((path = fh_path(&(argp->file), &status)) == NULL) {
		/* That means we can't get a path to the file.  Give up. */
		return (NFSERR_STALE);
	}
	/* stat the file first and only change fields that are different */
	if (stat(path, &buf) < 0)
		return (nfs_errno());
	errno = 0;
	if (((argp->attributes.uid != -1
	      && argp->attributes.uid != buf.st_uid) ||
	     (argp->attributes.gid != -1
	      && argp->attributes.gid != buf.st_gid)) &&
	    chown(path, argp->attributes.uid, argp->attributes.gid) != 0)
		goto failure;
	if (argp->attributes.mode != -1 &&
	    (argp->attributes.mode & 07777) != (buf.st_mode & 07777) &&
	    chmod(path, argp->attributes.mode) != 0)
		goto failure;
	if (argp->attributes.size != -1 &&
	    argp->attributes.size != buf.st_size &&
	    truncate(path, argp->attributes.size) != 0)
		goto failure;
	if ((argp->attributes.atime.seconds != (unsigned) -1 &&
	     argp->attributes.atime.seconds != buf.st_atime) ||
	    (argp->attributes.mtime.seconds != (unsigned) -1 &&
	     argp->attributes.mtime.seconds != buf.st_mtime)) {
		struct timeval tvp[2];
		tvp[0].tv_sec = argp->attributes.atime.seconds;
		tvp[0].tv_usec = argp->attributes.atime.useconds;
		tvp[1].tv_sec = argp->attributes.mtime.seconds;
		tvp[1].tv_usec = argp->attributes.mtime.useconds;
		if (utimes(path, tvp) != 0) {
		      failure:
			return (nfs_errno());
		}
	}
	return (getattr(&(argp->file),
			&(result.attrstat.attrstat_u.attributes), NULL));
}


int nfsd_nfsproc_root_2(argp)
void *argp;
{
	return (0);
}


int nfsd_nfsproc_lookup_2(argp)
diropargs *argp;
{
	int status;
	struct stat sbuf;
	struct stat *sbp = &sbuf;
	diropokres *dp = &result.diropres.diropres_u.diropres;

	status = fh_compose(argp, &(dp->file), &sbp, -1, -1);
	if (status == NFS_OK) {
		status = getattr(&(dp->file), &(dp->attributes), sbp);
		if (status == NFS_OK)
			dprintf(1, "\tnew_fh = %s\n", fh_pr(&(dp->file)));
	}
	return (status);
}


int nfsd_nfsproc_readlink_2(argp)
nfs_fh *argp;
{
	nfsstat status;
	char *path;
	int cc;

	if ((path = fh_path(argp, &status)) == 0)
		return (NFSERR_STALE);

	errno = 0;
	if ((cc = readlink(path, pathbuf, NFS_MAXPATHLEN)) < 0) {
		dprintf(1, " >>> %s\n", strerror(errno));
		return (nfs_errno());
	}
	status = NFS_OK;
	pathbuf[cc] = '\0';	/* readlink() doesn't null terminate!! */
	result.readlinkres.readlinkres_u.data = pathbuf;

	if (cp->o.link_relative && pathbuf[0] == '/') {
		/*
		 * We've got an absolute (locally) pathname, and we should
		 * translate to a relative pathname for the client.  We do
		 * this by prepending the correct number of "../"es to the
		 * path. This cannot work if the client does not mount the
		 * specified subtree of the filesystem.
		 */
		int slash_cnt = 0;
		char *p, *q;

		/* Count how many directories down we are. */
		for (p = path + 1; *p != '\0'; p++)
			if (*p == '/')
				slash_cnt++;

		/*
		 * Ok, now we are finished with the orginal file `path'
		 * and will only deal with the link target.
		 */
		p = &pathbuf[cc];	/* Point to the end and calculate */
		if (slash_cnt == 0)
			q = p + 1;	/* the extra space take by a	*/
		else		/* prepended '.'  		*/
			q = p + 3 * slash_cnt - 1;	/* or '../.../..' */

		if (q >= pathbuf + NFS_MAXPATHLEN) {
			dprintf(1, " [[NAME TOO LONG!!]]\n");
			return (NFSERR_NAMETOOLONG);
		} else {
			/* Add some space at the beginning of the string. */
			while (p >= pathbuf)
				*q-- = *p--;

			if (slash_cnt == 0)
				pathbuf[0] = '.';
			else {
				/*
				 * This overwrites the leading '/' on the
				 * last iteration.
				 */
				for (p = pathbuf; slash_cnt > 0; slash_cnt--) {
					*p++ = '.';
					*p++ = '.';
					*p++ = '/';
				}
			}
		}
	}
	dprintf(1, " %s\n", result.readlinkres.readlinkres_u.data);
	return (NFS_OK);
}


int nfsd_nfsproc_read_2(argp)
readargs *argp;
{
	nfsstat status;
	int fd;

	if ((fd = fh_fd(&(argp->file), &status, O_RDONLY)) < 0) {
		return ((int) status);
	}
	errno = 0;
	(void) lseek(fd, (long) argp->offset, L_SET);
	result.readres.readres_u.reply.data.data_val = iobuf;
	if (!errno)
		result.readres.readres_u.reply.data.data_len =
		    read(fd, iobuf, argp->count);
	fd_inactive(fd);
	if (errno)
		return (nfs_errno());
	return (getattr(&(argp->file),
		    &(result.readres.readres_u.reply.attributes), NULL));
}


int nfsd_nfsproc_writecache_2(argp)
void *argp;
{
	return (0);
}


int nfsd_nfsproc_write_2(argp)
writeargs *argp;
{
	nfsstat status;
	int fd;

	if ((fd = fh_fd(&(argp->file), &status, O_WRONLY)) < 0) {
		return ((int) status);
	}
	errno = 0;
	(void) lseek(fd, (long) argp->offset, L_SET);
	if (errno == 0) {	/* We should never fail. */
		if (write(fd, argp->data.data_val, argp->data.data_len) !=
		    argp->data.data_len) {
			dprintf(1, " Write failure, errno is %d.\n", errno);
		}
	}
	fd_inactive(fd);
	if (errno)
		return (nfs_errno());
	return (getattr(&(argp->file),
			&(result.attrstat.attrstat_u.attributes), NULL));
}


#define CREATE_OMODE O_RDWR


int nfsd_nfsproc_create_2(argp)
createargs *argp;
{
	nfsstat status;
	int tmpfd, flags;
	struct stat sbuf;
	struct stat *sbp = &sbuf;

	status = build_path(pathbuf, &argp->where);
	if (status != NFS_OK)
		return ((int) status);
	dprintf(1, "\tfullpath='%s'\n", pathbuf);

	/*
         * Checking the credentials and setting the correct effective
         * IDs is now done in the dispatch routine.
         */
	errno = 0;
	flags = (argp->attributes.size == 0 ?
		 CREATE_OMODE | O_CREAT | O_TRUNC :
		 CREATE_OMODE | O_CREAT);
	tmpfd = path_open(pathbuf, flags, argp->attributes.mode);
	if (tmpfd < 0) {
		dprintf(1, "\tcreat() failed -- errno returned=%d.\n", errno);
		return (nfs_errno());
	}
	(void) fstat(tmpfd, &sbuf);

	/*
         * Set up the correct ownership.  Almost all create requests
         * from the current Sun implementation fail to include
         * explicit uid/gid pairs for creates, so we have to use info from
         * the request authication (now done during dispatch).
         */
	{
		int target_uid = argp->attributes.uid;
		int target_gid = argp->attributes.gid;

		if ((target_uid != -1 && sbuf.st_uid != target_uid) ||
		    (target_gid != -1 && sbuf.st_gid != target_gid))
			if (fchown(tmpfd, target_uid, target_gid) != 0) {
				dprintf(1, "\tfchown() failed: errno:%d  target uid:%d.\n",
					errno, target_uid);
				goto failure;
			} else {
				if (target_uid >= 0)
					sbuf.st_uid = target_uid;
				if (target_gid >= 0)
					sbuf.st_gid = target_gid;
			}
	}

	if (argp->attributes.size != -1 &&
	    argp->attributes.size != sbuf.st_size)
		if (ftruncate(tmpfd, argp->attributes.size) != 0)
			goto failure;
	if ((argp->attributes.atime.seconds != -1 &&
	     argp->attributes.atime.seconds != sbuf.st_atime) ||
	    (argp->attributes.mtime.seconds != -1 &&
	     argp->attributes.mtime.seconds != sbuf.st_mtime)) {
		struct timeval tvp[2];
		tvp[0].tv_sec = argp->attributes.atime.seconds;
		tvp[0].tv_usec = argp->attributes.atime.useconds;
		tvp[1].tv_sec = argp->attributes.mtime.seconds;
		tvp[1].tv_usec = argp->attributes.mtime.useconds;
		if (utimes(pathbuf, tvp) != 0) {
		      failure:
			(void) close(tmpfd);
			return (nfs_errno());
		} else {
			sbuf.st_atime = argp->attributes.atime.seconds;
			sbuf.st_mtime = argp->attributes.mtime.seconds;
		}
	}
	status = fh_compose(&(argp->where),
		       &(result.diropres.diropres_u.diropres.file), &sbp,
			    tmpfd, CREATE_OMODE);
	if (status == NFS_OK) {
		status = getattr(&(result.diropres.diropres_u.diropres.file),
		       &(result.diropres.diropres_u.diropres.attributes),
				 sbp);
		if (status == NFS_OK)
			dprintf(1, "\tnew_fh = %s\n",
				fh_pr(&(result.diropres.diropres_u.diropres.file)));
	} else
		(void) close(tmpfd);
	return (status);
}

#undef CREATE_OMODE


int nfsd_nfsproc_remove_2(argp)
diropargs *argp;
{
	nfsstat status;

	status = build_path(pathbuf, argp);
	if (status != NFS_OK)
		return ((int) status);

	dprintf(1, "\tfullpath='%s'\n", pathbuf);

	/* Remove the file handle from our cache. */
	fh_remove(pathbuf);

	if (unlink(pathbuf) != 0)
		return (nfs_errno());
	else
		return (NFS_OK);
}


int nfsd_nfsproc_rename_2(argp)
renameargs *argp;
{
	nfsstat status;

	status = build_path(pathbuf, &argp->from);
	if (status != NFS_OK)
		return ((int) status);
	status = build_path(pathbuf_1, &argp->to);
	if (status != NFS_OK)
		return ((int) status);

	dprintf(1, "\tpathfrom='%s' pathto='%s'\n", pathbuf, pathbuf_1);

	/* Remove any file handle from our cache. */
	fh_remove(pathbuf);
	fh_remove(pathbuf_1);

	if (rename(pathbuf, pathbuf_1) != 0)
		return (nfs_errno());

	return (NFS_OK);
}


int nfsd_nfsproc_link_2(argp)
linkargs *argp;
{
	nfsstat status;
	char *path;

	if ((path = fh_path(&(argp->from), &status)) == 0)
		return (NFSERR_STALE);

	status = build_path(pathbuf_1, &argp->to);
	if (status != NFS_OK)
		return ((int) status);

	dprintf(1, "\tpathfrom='%s' pathto='%s'\n", path, pathbuf_1);

	if (link(path, pathbuf_1) != 0)
		return (nfs_errno());
	return (NFS_OK);
}


int nfsd_nfsproc_symlink_2(argp)
symlinkargs *argp;
{
	nfsstat status;

	status = build_path(pathbuf, &argp->from);
	if (status != NFS_OK)
		return ((int) status);

	dprintf(1, "\tstring='%s' filename='%s'\n", argp->to, pathbuf);

	/*
         * Ignore the attributes, as the NFS version 2 documentation says
         * "On UNIX servers the attributes are never used...",
         */
	if (symlink(argp->to, pathbuf) != 0)
		return (nfs_errno());
	return (NFS_OK);
}


int nfsd_nfsproc_mkdir_2(argp)
createargs *argp;
{
	nfsstat status;
	struct stat sbuf;
	struct stat *sbp = &sbuf;

	status = build_path(pathbuf, &argp->where);
	if (status != NFS_OK)
		return ((int) status);

	dprintf(1, "\tfullpath='%s'\n", pathbuf);

	if (mkdir(pathbuf, argp->attributes.mode) != 0)
		return (nfs_errno());
	status = fh_compose(&(argp->where),
		       &(result.diropres.diropres_u.diropres.file), &sbp,
			    -1, -1);
	if (status != NFS_OK)
		return ((int) status);

	/*
         * Set up the correct ownership.  As with create, almost all mkdir
         * requests from the current Sun implementation fail to include
         * explicit uid/gid pairs for creates, so we have to use info
         * from the request authication (now done during dispatch).
         */
	{
		int target_uid = argp->attributes.uid;
		int target_gid = argp->attributes.gid;

		if ((target_uid != -1 && sbuf.st_uid != target_uid) ||
		    (target_gid != -1 && sbuf.st_gid != target_gid))
			if (chown(pathbuf, target_uid, target_gid) != 0)
				return (nfs_errno());
	}

	/* Note that the spb buffer is now invalid! */
	status = getattr(&(result.diropres.diropres_u.diropres.file),
		&(result.diropres.diropres_u.diropres.attributes), NULL);
	if (status == NFS_OK)
		dprintf(1, "\tnew_fh = %s\n",
		     fh_pr(&(result.diropres.diropres_u.diropres.file)));
	return ((int) status);
}


int nfsd_nfsproc_rmdir_2(argp)
diropargs *argp;
{
	nfsstat status;

	status = build_path(pathbuf, argp);
	if (status != NFS_OK)
		return ((int) status);

	dprintf(1, "\tfullpath='%s'\n", pathbuf);

	/* Remove that file handle from our cache. */
	fh_remove(pathbuf);

	if (rmdir(pathbuf) != 0)
		return (nfs_errno());

	return (NFS_OK);
}


/* More Mark Shand code. */
static int dpsize(dp)
struct direct *dp;
{
#define DP_SLOP	16
#define MAX_E_SIZE sizeof(entry) + MAXNAMLEN + DP_SLOP
	return (sizeof(entry) + strlen(dp->d_name) + DP_SLOP);
}


int nfsd_nfsproc_readdir_2(argp)
readdirargs *argp;
{
	nfsstat status;
	static readdirres oldres;
	entry **e;
	char *path;
	long dloc;
	DIR *dirp;
	struct direct *dp;
	struct stat sbuf;
	int res_size;

	/* Free the previous result, since it has 'malloc'ed strings.  */
	xdr_free(xdr_readdirres, (caddr_t) & oldres);

	if ((path = fh_path(&(argp->dir), &status)) == NULL)
		return (NFSERR_STALE);

	/* This code is verbatim from Mark Shand's version */
	errno = 0;
	(void) stat(path, &sbuf);
	if ((dirp = opendir(path)) == NULL)
		return ((errno ? nfs_errno() : NFSERR_NAMETOOLONG));

	res_size = 0;
	bcopy(argp->cookie, &dloc, sizeof(dloc));
	if (dloc != 0)
		seekdir(dirp, ntohl(dloc));
	e = &(result.readdirres.readdirres_u.reply.entries);
	while (((res_size + MAX_E_SIZE) < argp->count
		|| e == &(result.readdirres.readdirres_u.reply.entries))
	       && (dp = readdir(dirp)) != NULL) {
		if ((*e = (entry *) malloc(sizeof(entry))) == NULL)
			mallocfailed();
		(*e)->fileid = pseudo_inode(dp->d_ino, sbuf.st_dev);
		if (((*e)->name = malloc(strlen(dp->d_name) + 1)) == NULL)
			mallocfailed();
		strcpy((*e)->name, dp->d_name);
		dloc = htonl(telldir(dirp));
		bcopy(&dloc, ((*e)->cookie), sizeof(nfscookie));
		e = &((*e)->nextentry);
		res_size += dpsize(dp);
	}
	*e = NULL;
	result.readdirres.readdirres_u.reply.eof = (dp == NULL);
	(void) closedir(dirp);
	oldres = result.readdirres;
	return (result.readdirres.status);
}

/*
 * Only reports free space correctly for the filesystem that the
 * mount point is on.  Actually it will work fine for any file
 * handle (e.g. sub mounts) but the NFS spec calls for root_fh
 * to be used by the client when calling this.
 */
int nfsd_nfsproc_statfs_2(argp)
nfs_fh *argp;
{
	nfsstat status;
	char *path;
	struct statfs fsd;

	if ((path = fh_path(argp, &status)) == NULL)
		return (NFSERR_STALE);

	if (statfs(path, &fsd) < 0)
		return (nfs_errno());
	result.statfsres.status = NFS_OK;
	result.statfsres.statfsres_u.reply.tsize = fsd.f_bsize;	/* XXX */
	result.statfsres.statfsres_u.reply.bsize = fsd.f_bsize;
	result.statfsres.statfsres_u.reply.blocks = fsd.f_blocks;
	result.statfsres.statfsres_u.reply.bfree = fsd.f_bfree;
	result.statfsres.statfsres_u.reply.bavail = fsd.f_bavail;

	return (NFS_OK);
}


static int makesock(port, proto, socksz)
int port;
int proto;
int socksz;
{
	struct sockaddr_in sin;
	int s;
	int sock_type;

	sock_type = (proto == IPPROTO_UDP) ? SOCK_DGRAM : SOCK_STREAM;
	s = socket(AF_INET, sock_type, proto);
	if (s < 0) {
		dprintf(0, "Could not make a socket: %s\n", strerror(errno));
		return (-1);
	}
	bzero((char *) &sin, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;
	sin.sin_port = htons(port);

	{
		int val = 1;

		if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &val, sizeof(val)) < 0)
			dprintf(0, "setsockopt failed: %s\n", strerror(errno));
	}

#ifdef SO_SNDBUF
	{
		int sblen, rblen;

		/* 1024 for rpc & transport overheads */
		sblen = rblen = socksz + 1024;
		if (setsockopt(s, SOL_SOCKET, SO_SNDBUF, &sblen, sizeof sblen) < 0 ||
		    setsockopt(s, SOL_SOCKET, SO_RCVBUF, &rblen, sizeof rblen) < 0)
			dprintf(0, "setsockopt failed: %s\n", strerror(errno));
	}
#endif				/* SO_SNDBUF */

	if (bind(s, (struct sockaddr *) &sin, sizeof(sin)) == -1) {
		dprintf(0, "Could not bind name to socket: %s\n", strerror(errno));
		return (-1);
	}
	return (s);
}


/* This is taken from an rpcgen generated service file. */
static void closedown(sig)
int sig;
{
	(void) signal(sig, closedown);
	if (_rpcsvcdirty == 0) {
		extern fd_set svc_fdset;
		static int size;
		int i, openfd;

		if (_rpcfdtype == SOCK_DGRAM)
			exit(0);
		if (size == 0) {
			size = getdtablesize();
		}
		for (i = 0, openfd = 0; i < size && openfd < 2; i++)
			if (FD_ISSET(i, &svc_fdset))
				openfd++;
		if (openfd <= 1)
			exit(0);
	}
	(void) alarm(_RPCSVC_CLOSEDOWN);
}


static void usage()
{
	dprintf(0, "Usage: nfsd [-dvp] [=f auth_file]\n");
	(void) closelog();
	exit(-1);
}


void main(argc, argv)
int argc;
char **argv;
{
	int c, fd;
	struct sockaddr_in saddr;
	int addr_size;
	SVCXPRT *transp;
	int nfs_socket;
	char *auth_file = NULL;
	extern int getopt(), optind, opterr;
	extern char *optarg;

	/*
         * This code uses the RPC library functions in exactly the
         * same way a regular RPC application would.
         */
	nfs_socket = 0;
	_rpcfdtype = 0;
	if (getsockname(0, (struct sockaddr *) &saddr, &addr_size) == 0) {
		int ssize = sizeof(int);
		if (saddr.sin_family != AF_INET)
			exit(1);
		if (getsockopt(0, SOL_SOCKET, SO_TYPE,
			       (char *) &_rpcfdtype, &ssize) < 0)
			exit(1);
		_rpcpmstart = 1;
	} else
		pmap_unset(NFS_PROGRAM, NFS_VERSION);

	if (_rpcfdtype == 0 || _rpcfdtype == SOCK_DGRAM) {
		if (_rpcfdtype == 0 &&
		    (nfs_socket = makesock(NFS_PORT, IPPROTO_UDP, NFS_MAXDATA)) < 0) {
			fprintf(stderr, "nfsd: could not make a UDP socket.\n");
			exit(1);
		}
		transp = svcudp_create(nfs_socket);
		if (transp == NULL) {
			fprintf(stderr, "nfsd: cannot create UDP service.\n");
			exit(1);
		}
		if (!svc_register(transp, NFS_PROGRAM, NFS_VERSION, nfs_dispatch,
				  IPPROTO_UDP)) {
			fprintf(stderr,
				"unable to register(NFS_PROGRAM, NFS_VERSION, UDP).\n");
			exit(1);
		}
	}
	if (_rpcfdtype == 0 || _rpcfdtype == SOCK_STREAM) {
		if (_rpcfdtype == 0 &&
		    (nfs_socket = makesock(NFS_PORT, IPPROTO_TCP, NFS_MAXDATA)) < 0) {
			fprintf(stderr, "nfsd: could not make a TCP socket.\n");
			exit(1);
		}
		transp = svctcp_create(nfs_socket, 0, 0);
		if (transp == NULL) {
			fprintf(stderr, "nfsd: cannot create TCP service.\n");
			exit(1);
		}
		if (!svc_register(transp, NFS_PROGRAM, NFS_VERSION, nfs_dispatch,
				  IPPROTO_TCP)) {
			fprintf(stderr,
				"unable to register(NFS_PROGRAM, NFS_VERSION, TCP).\n");
			exit(1);
		}
	}
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

	/* We first fork off a child. */
	if ((c = fork()) > 0)
		exit(0);
	if (c < 0) {
		fprintf(stderr, "nfsd: cannot fork: %s\n", strerror(errno));
		exit(-1);
	}
	/* Now we remove ourselves from the foreground. */
	(void) close(0);
	(void) close(1);
	(void) close(2);
	if ((fd = open("/dev/tty", O_RDWR)) >= 0) {
		(void) ioctl(fd, TIOCNOTTY, (char *) NULL);
		(void) close(fd);
	}
	/* Initialize logging. */
	log_open("nfsd");

	/*
         * We have to deal with the grunge of real/effective/saved uid.
         * First we make both both the real and effective uid to root.
         * We shouldn't really need to change the real uid, but it oc-
         * casionally has problems with losing root permissions if we
         * don't.  ( ??? yeah -- I should track it down)
         */
	if (setreuid(0, 0) < 0) {
		dprintf(0, "Unable to setreuid(0, 0): %s\n", strerror(errno));
		exit(1);
	}
	/* Initialize the FH module. */
	fh_init();

	/* Initialize the AUTH module. */
	auth_init(auth_file);

	/* Enable the LOG toggle with a signal. */
	(void) signal(SIGUSR1, toggle_logging);

	if (_rpcpmstart) {
		(void) signal(SIGALRM, closedown);
		(void) alarm(_RPCSVC_CLOSEDOWN);
	}
	/* Run the NFS server. */
	svc_run();

	dprintf(0, "Oh no Mr. Bill... nfs_server() returned!\n");
	exit(1);
}
