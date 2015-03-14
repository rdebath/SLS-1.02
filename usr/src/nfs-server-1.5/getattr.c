/*
 * getattr	This module handles the NFS attributes.
 *
 * Version:	@(#)getattr.c	1.5	93/04/10
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


/* Use Mark Shand's ugid_map.c if you have differing uids on your machines. */
#ifdef UGID_MAP
extern clnt_param *client_param;
extern struct svc_req *rqstp;
#define ruid(uid) ruid(uid, client_param, rqstp)
#define rgid(gid) rgid(gid, client_param, rqstp)
#else
#define ruid(uid) (uid)
#define rgid(gid) (gid)
#endif


/*
 * The NFS version 2 specification fails to mention all of
 * these file types, but they exist in the nfs_prot.x file.
 */
#define ftype_map(st_mode) (_ftype_map[((st_mode) & S_IFMT) >> 12])

ftype _ftype_map[16] =
{
#ifdef S_IFIFO
	NFNON, NFFIFO, NFCHR, NFBAD,
#else
	NFNON, NFBAD, NFCHR, NFBAD,
#endif
	NFDIR, NFBAD, NFBLK, NFBAD,
	NFREG, NFBAD, NFLNK, NFBAD,
	NFSOCK, NFBAD, NFBAD, NFBAD,
};


nfsstat getattr(fh, attr, stat_optimize)
nfs_fh *fh;
fattr *attr;
struct stat *stat_optimize;
{
#if DEBUG
	char buff[1024];
	char *sp;
#endif
	nfsstat status;
	char *path;
	struct stat *s;
	struct stat sbuf;

	if ((path = fh_path(fh, &status)) == NULL) {
		dprintf(1, "getattr: failed! No such file.\n");
		return (NFSERR_STALE);
	}
	if (stat_optimize != NULL)
		s = stat_optimize;
	else if (lstat(path, (s = &sbuf)) != 0) {
		dprintf(1, "getattr(%s): failed!  errno=%d\n", path, errno);
		return (nfs_errno());
	}
	attr->type = ftype_map(s->st_mode);
	attr->mode = s->st_mode;
	attr->nlink = s->st_nlink;
	attr->uid = ruid(s->st_uid);
	attr->gid = rgid(s->st_gid);

	/* To account for relative stuff - jrs. */
	if (S_ISLNK(s->st_mode))
		attr->size = NFS_MAXPATHLEN;
	else
		attr->size = s->st_size;
	attr->blocksize = s->st_blksize;
	attr->rdev = s->st_rdev;
	attr->blocks = s->st_blocks;
	attr->fsid = 1;
	attr->fileid = fh_psi(fh);
	attr->atime.seconds = s->st_atime;
	attr->atime.useconds = 0;
	attr->mtime.seconds = s->st_mtime;
	attr->mtime.useconds = 0;
	attr->ctime.seconds = s->st_ctime;
	attr->ctime.useconds = 0;

#ifdef DEBUG
	sp = buff;
	sprintf(sp, " t=%d, m=%o, lk=%d, u/g=%d/%d, sz=%d, bsz=%d",
		attr->type, attr->mode, attr->nlink,
		attr->uid, attr->gid, attr->size,
		attr->blocksize);
	sp += strlen(sp);
	if (attr->type == NFCHR || attr->type == NFBLK) {
		sprintf(sp, " rdev=%d/%d", (attr->rdev >> 8) & 0xff, attr->rdev & 0xff);
		sp += strlen(sp);
		sprintf(sp, "\n  blks=%d, fsid=%d, psi=%d, at=%d, mt=%d, ct=%d\n",
			attr->blocks, attr->fsid, attr->fileid,
			attr->atime.seconds,
			attr->mtime.seconds,
			attr->ctime.seconds);
		sp += strlen(sp);
	} else {
		sprintf(sp, " >>> %s\n", sys_errlist[(int) status]);
	}
	dprintf(1, buff);
#endif

	return (NFS_OK);
}
