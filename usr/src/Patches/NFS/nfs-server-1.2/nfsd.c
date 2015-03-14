/*****************************************************************************\
*									      *
*	File:     hnfsd.c						      *
*	Author:   Don Becker (becker@trantor.harris-atd.com)		      *
*	Header created:  Mon Jun  5 20:04:09 1989			      *
*	Contents: The data server for the Harris NFS deamon.		      *
*									      *
******************************************************************************/
#ifdef linux
char *version =
    "Linux nfsd version 1.1 of "__DATE__"\n";
#else
char *version =
    "Copyright 1989 Harris Corporation  hnfsd version 0.11 of "__DATE__"\n";
#endif

#include <sys/vfs.h>
#include <sys/types.h>
#include <strings.h>
#include <sys/file.h>
#include <sys/dir.h>		/* Used only in readdir() */

#include "nfsd.h"
#include "libc.h"
#include "fh.h"

#ifdef DEBUG
#include <stdio.h>
FILE *debuglog;
#endif DEBUG

extern clnt_param *cp;		/* Only for the option list */
extern nfsstat
 getattr(nfs_fh *fh, fattr *attr, struct stat *stat_optimize);

extern void xdr_free();		/* Fill this in later */

static char iobuf[NFS_MAXDATA];
static char pathbuf[NFS_MAXPATHLEN+1];
static char pathbuf_1[NFS_MAXPATHLEN+1];

static inline nfsstat
    build_path(char *buf, diropargs *da)
{
    nfsstat status;
    char *path, *name;

    if ((path = fh_path(&(da->dir), &status)) == 0)
	return NFSERR_STALE;

    while(*path)		/* strcpy(buf, path); */
	*buf++ = *path++;
    *buf++ = '/';		/* strcat(buf, "/");  */
    name = da->name;
    while(*name)		/* strcat(pathbuf, argp->where.name); */
	*buf++ = *name++;
    *buf = '\0';
    return NFS_OK;
}    

/* The "wrappers" of the following functions came from `rpcgen -l nfs_prot.x`.
   This normally generates the client routines, but it provides nice
   prototypes for the server routines also.
   */
#define CLIENT struct svc_req

int
nfsd_nfsproc_null_2(argp)
     void *argp;
{
	return 0;
}


int
nfsd_nfsproc_getattr_2(argp)
     nfs_fh *argp;
{
    return getattr(argp, &result.attrstat.attrstat_u.attributes, NULL);
}


int
nfsd_nfsproc_setattr_2(argp)
     sattrargs *argp;
{
    nfsstat status;
    char *path;
    struct stat buf;

    if ((path = fh_path(&(argp->file), &status)) == NULL) {
	/* Hmmm, that means we can't get a path to the file.  Give up. */
	return NFSERR_STALE;
    }
    /* stat the file first and only change fields that are different */
    if (stat(path, &buf) < 0)
	return nfs_errno();
    errno = 0;
    if (((argp->attributes.uid != -1 && argp->attributes.uid != buf.st_uid) ||
	(argp->attributes.gid != -1 && argp->attributes.gid != buf.st_gid)) &&
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
	    return nfs_errno();
	}
    }
    return getattr(&(argp->file), &(result.attrstat.attrstat_u.attributes),
		   NULL);
}


int
nfsd_nfsproc_root_2(argp)
     void *argp;
{
    return 0;
}


int
nfsd_nfsproc_lookup_2(argp)
     diropargs *argp;
{
    int status;
    struct stat	sbuf;
    struct stat	*sbp = &sbuf;
    diropokres *dp = &result.diropres.diropres_u.diropres;

    status = fh_compose(argp, &(dp->file), &sbp);
    if (status == NFS_OK) {
	status = getattr(&(dp->file), &(dp->attributes), sbp);
#ifdef DEBUG
	if (debuglog && status == NFS_OK)
	    fprintf(debuglog, "\tnew_fh = %s\n",
		    fh_pr(&(dp->file)));
#endif
    }
    return status;
}


int
nfsd_nfsproc_readlink_2(argp)
     nfs_fh *argp;
{
    nfsstat status;
    char *path;
    int	cc;

    if ((path = fh_path(argp, &status)) == 0)
	return NFSERR_STALE;
	
    errno = 0;
    if ((cc = readlink(path, pathbuf, NFS_MAXPATHLEN)) < 0) {
#ifdef DEBUG
	if (debuglog) fprintf(debuglog, " >>> %s\n", sys_errlist[errno]);
#endif
	return nfs_errno();
    }

    status = NFS_OK;
    pathbuf[cc] = '\0';		/* readlink() doesn't null terminate!! */
    result.readlinkres.readlinkres_u.data = pathbuf;

    if (cp->o.link_relative && pathbuf[0] == '/') {
	/* We've got an absolute (locally) pathname, and we should 4ranslate
	   to a relative pathname for the client.  We do this by prepending
	   the correct number of "../"es to the path.
	   This cannot work if the client does not mount the specified
	   subtree of the filesystem.  */
	
	int	slash_cnt = 0;
	char	*p, *q;
	
	/* Count how many directories down we are. */
	for (p = path+1; *p != '\0'; p++)
	    if (*p == '/')  slash_cnt++;

	/* Ok, now we are finished with the orginal file `path' and
	   will only deal with the link target. */
	p = &pathbuf[cc];		/* Point to the end and calculate */
	if (slash_cnt == 0)		/* the extra space take by a	*/
	    q = p + 1 ;			/* prepended '.'  		*/
	else
	    q = p + 3 * slash_cnt - 1;	/* or '../.../..' */

	if (q >= pathbuf + NFS_MAXPATHLEN) {
#ifdef DEBUG
	    if (debuglog) fprintf(debuglog, " [[NAME TOO LONG!!]]\n");
#endif
	    return NFSERR_NAMETOOLONG;
	} else {
	    /* Add some space at the beginning of the string. */
	    while (p >= pathbuf)
		*q-- = *p--;

	    if (slash_cnt == 0)
		pathbuf[0] = '.';
	    else {
		/* This overwrites the leading '/' on the last iteration. */
		for (p = pathbuf; slash_cnt > 0; slash_cnt-- ) {
		    *p++ = '.';
		    *p++ = '.';	
		    *p++ = '/';
		}
	    }
	}
    }
#ifdef DEBUG
    if (debuglog)
	fprintf(debuglog, " %s\n", result.readlinkres.readlinkres_u.data);
#endif
    return NFS_OK;
}


int
nfsd_nfsproc_read_2(argp)
     readargs *argp;
{
    nfsstat status;
    int fd;
    
    if ((fd = fh_fd(&(argp->file), &status, O_RDONLY)) < 0) {
	return (nfsstat) status;
    }
    errno = 0;
    lseek(fd, (long) argp->offset, L_SET);
    result.readres.readres_u.reply.data.data_val = iobuf;
    if (!errno)
	result.readres.readres_u.reply.data.data_len =
	    read(fd, iobuf, argp->count);
    fd_inactive(fd);
    if (errno)
	return nfs_errno();
    
    return getattr(&(argp->file),
		   &(result.readres.readres_u.reply.attributes), NULL);
}


int
nfsd_nfsproc_writecache_2(argp)
     void *argp;
{
    return 0;
}


int
nfsd_nfsproc_write_2(argp)
     writeargs *argp;
{
    nfsstat status;
    int fd;

    if ((fd = fh_fd(&(argp->file), &status, O_WRONLY)) < 0) {
	return (nfsstat) status;
    }
    errno = 0;
    lseek(fd, (long) argp->offset, L_SET);
    if (errno == 0) {		/* We should never fail. */
	if (write(fd, argp->data.data_val, argp->data.data_len) !=
	    argp->data.data_len) {
#ifdef DEBUG
	    if (debuglog)
		fprintf(debuglog, " Write failure, errno is %d.\n", errno);
#endif
	}
    }
    fd_inactive(fd);
    if (errno)
	return nfs_errno();

    return getattr(&(argp->file), &(result.attrstat.attrstat_u.attributes),
		   NULL);
}


int
nfsd_nfsproc_create_2(argp)
     createargs *argp;
{
    nfsstat status;
    int tmpfd, flags;
    struct stat	sbuf;
    struct stat	*sbp = &sbuf;

    status = build_path(pathbuf, &argp->where);
    if (status != NFS_OK)
	return status;

#ifdef DEBUG
    if (debuglog)
	fprintf(debuglog, "\tfullpath='%s'\n", pathbuf);
#endif

    /* Checking the credentials and setting the correct effective IDs is
       now done in the dispatch routine. */

    errno = 0;
    flags =  (argp->attributes.size == 0  ?
	      O_RDWR | O_CREAT | O_TRUNC  :
	      O_RDWR | O_CREAT);
    tmpfd = open(pathbuf, flags, argp->attributes.mode);
    if (tmpfd < 0) {
#ifdef DEBUG
	if (debuglog)
	    fprintf(debuglog,
		    "\tcreat() failed -- errno returned=%d.\n", errno);
#endif
	return nfs_errno();
    }

    fstat(tmpfd, &sbuf);
    /* Set up the correct ownership.  Almost all create requests
     * from the current Sun implementation fail to include
     * explicit uid/gid pairs for creates, so we have to use info from
     * the request authication (now done during dispatch). */
    {
	int target_uid = argp->attributes.uid;
	int target_gid = argp->attributes.gid;
	if ((target_uid != -1  &&  sbuf.st_uid != target_uid)  ||
	    (target_gid != -1  &&  sbuf.st_gid != target_gid))
	    if (fchown(tmpfd, target_uid, target_gid) != 0) {
#ifdef DEBUG
		if (debuglog)
		    fprintf(debuglog,
			    "\tfchown() failed: errno:%d  target uid:%d.\n",
			    errno, target_uid);
#endif
		goto failure;
	    } else {
		if (target_uid >= 0) sbuf.st_uid = target_uid;
		if (target_gid >= 0) sbuf.st_gid = target_gid;
	    }
    }

    
    if (argp->attributes.size != -1  &&
	argp->attributes.size != sbuf.st_size)
	if (ftruncate(tmpfd, argp->attributes.size) != 0)
	    goto failure;
    if (  (argp->attributes.atime.seconds != -1 &&
	   argp->attributes.atime.seconds != sbuf.st_atime)
	||(argp->attributes.mtime.seconds != -1 &&
	   argp->attributes.mtime.seconds != sbuf.st_mtime)) {
	struct timeval tvp[2];
	tvp[0].tv_sec = argp->attributes.atime.seconds;
	tvp[0].tv_usec = argp->attributes.atime.useconds;
	tvp[1].tv_sec = argp->attributes.mtime.seconds;
	tvp[1].tv_usec = argp->attributes.mtime.useconds;
	if (utimes(pathbuf, tvp) != 0) {
	failure:
	    close(tmpfd);
	    return nfs_errno();
	}
	else {
	    sbuf.st_atime = argp->attributes.atime.seconds;
	    sbuf.st_mtime = argp->attributes.mtime.seconds;
	}
    }

    status = fh_compose(&(argp->where),
			&(result.diropres.diropres_u.diropres.file), &sbp);
    if (status == NFS_OK) {
	status = getattr(&(result.diropres.diropres_u.diropres.file),
			 &(result.diropres.diropres_u.diropres.attributes),
			 sbp);
#ifdef DEBUG
	if (debuglog && status == NFS_OK)
	    fprintf(debuglog, "\tnew_fh = %s\n",
		    fh_pr(&(result.diropres.diropres_u.diropres.file)));
#endif
    }

    close(tmpfd);
    return status;
}


int
nfsd_nfsproc_remove_2(argp)
     diropargs *argp;
{
    nfsstat status;

    status = build_path(pathbuf, argp);
    if (status != NFS_OK)
	return status;

#ifdef DEBUG
    if (debuglog)
	fprintf(debuglog, "\tfullpath='%s'\n", pathbuf);
#endif

    /* Remove the file handle from our cache. */
    fh_remove(pathbuf);

    if (unlink(pathbuf) != 0)
	return nfs_errno();
    else
	return NFS_OK;
}


int
nfsd_nfsproc_rename_2(argp)
     renameargs *argp;
{
    nfsstat status;

    status = build_path(pathbuf, &argp->from);
    if (status != NFS_OK)
	return status;
    status = build_path(pathbuf_1, &argp->to);
    if (status != NFS_OK)
	return status;
    
#ifdef DEBUG
    if (debuglog)
	fprintf(debuglog, "\tpathfrom='%s' pathto='%s'\n",
		pathbuf, pathbuf_1);
#endif

    /* Remove any file handle from our cache. */
    fh_remove(pathbuf);

    if (rename(pathbuf, pathbuf_1) != 0)
	return nfs_errno();

    return NFS_OK;
}


int
nfsd_nfsproc_link_2(argp)
     linkargs *argp;
{
    nfsstat status;
    char *path;
    
    if ((path = fh_path(&(argp->from), &status)) == 0)
	return NFSERR_STALE;

    status = build_path(pathbuf_1, &argp->to);
    if (status != NFS_OK)
	return status;
    
#ifdef DEBUG
    if (debuglog)
	fprintf(debuglog, "\tpathfrom='%s' pathto='%s'\n",
		path, pathbuf_1);
#endif

    if (link(path, pathbuf_1) != 0)
	return nfs_errno();
    else
	return NFS_OK;
}


int
nfsd_nfsproc_symlink_2(argp)
     symlinkargs *argp;
{
    nfsstat status;
    
    status = build_path(pathbuf, &argp->from);
    if (status != NFS_OK)
	return status;

#ifdef DEBUG
    if (debuglog)
	fprintf(debuglog, "\tstring='%s' filename='%s'\n",
		argp->to, pathbuf);
#endif

    /* Ignore the attributes, as the NFS version 2 documentation says
     * "On UNIX servers the attributes are never used..." */

    if (symlink(argp->to, pathbuf) != 0)
	return nfs_errno();
    else
	return NFS_OK;
}


int
nfsd_nfsproc_mkdir_2(argp)
     createargs *argp;
{
    nfsstat status;
    struct stat	sbuf;
    struct stat	*sbp = &sbuf;
    
    status = build_path(pathbuf, &argp->where);
    if (status != NFS_OK)
	return status;

#ifdef DEBUG
    if (debuglog)
	fprintf(debuglog, "\tfullpath='%s'\n", pathbuf);
#endif

    if (mkdir(pathbuf, argp->attributes.mode) != 0) {
	return nfs_errno();
    }
    status = fh_compose(&(argp->where),
			&(result.diropres.diropres_u.diropres.file), &sbp);
    if (status != NFS_OK)
	return (enum nfsstat)status;

    /* Set up the correct ownership.  As with create, almost all mkdir
     * requests from the current Sun implementation fail to include
     * explicit uid/gid pairs for creates, so we have to use info
     * from the request authication (now done during dispatch). */

    {
	int target_uid = argp->attributes.uid;
	int target_gid = argp->attributes.gid;
	if ((target_uid != -1  &&  sbuf.st_uid != target_uid)  ||
	    (target_gid != -1  &&  sbuf.st_gid != target_gid))
	    if (chown(pathbuf, target_uid, target_gid) != 0)
		return nfs_errno();
    }
    /* Note that the spb buffer is now invalid! */
    status = getattr(&(result.diropres.diropres_u.diropres.file),
		     &(result.diropres.diropres_u.diropres.attributes),
		     NULL);
#ifdef DEBUG
    if (debuglog && status == NFS_OK)
	fprintf(debuglog, "\tnew_fh = %s\n",
		fh_pr(&(result.diropres.diropres_u.diropres.file)));
#endif
	return status;
}


int
nfsd_nfsproc_rmdir_2(argp)
     diropargs *argp;
{
    nfsstat status;
    
    status = build_path(pathbuf, argp);
    if (status != NFS_OK)
	return status;

#ifdef DEBUG
    if (debuglog)
	fprintf(debuglog, "\tfullpath='%s'\n", pathbuf);
#endif

    /* Remove that file handle from our cache. */
    fh_remove(pathbuf);

    if (rmdir(pathbuf) != 0)
	return nfs_errno();

    return NFS_OK;
}

/* More Mark Shand code. */
static int
dpsize(dp)
struct direct *dp;
{
#define DP_SLOP	16
#define MAX_E_SIZE sizeof(entry) + MAXNAMLEN + DP_SLOP
	return sizeof(entry) + strlen(dp->d_name) + DP_SLOP;
}


int
nfsd_nfsproc_readdir_2(argp)
     readdirargs *argp;
{
    nfsstat status;
    static readdirres oldres;
    entry **e;
    char *path;
    long	dloc;
    DIR	*dirp;
    struct direct *dp;
    struct stat	sbuf;
    int	res_size;
    
    /* Free the previous result, since it has 'malloc'ed strings.  */
    xdr_free(xdr_readdirres, (caddr_t) &oldres);

    if ((path = fh_path(&(argp->dir), &status)) == NULL)
	return NFSERR_STALE;

    /* This code is verbatim from Mark Shand's version */
    errno = 0;
    stat(path, &sbuf);
    if ((dirp = opendir(path)) == NULL)
	return (errno ? nfs_errno() : NFSERR_NAMETOOLONG);

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
	if (((*e)->name = malloc(strlen(dp->d_name)+1)) == NULL)
	    mallocfailed();
	strcpy((*e)->name, dp->d_name);
	dloc = htonl(telldir(dirp));
	bcopy(&dloc, ((*e)->cookie), sizeof(nfscookie));
	e = &((*e)->nextentry);
	res_size += dpsize(dp);
    }
    *e = NULL;
    result.readdirres.readdirres_u.reply.eof = (dp == NULL);
    closedir(dirp);
    oldres = result.readdirres;
    return result.readdirres.status;
}

/* Here we have to cheat, since the file systems may not have
 * identical blocksizes.  Besides, how do we represent that one file
 * system is full, but the rest have lots of space?  This should be
 * rethought in future versions of NFS.
 *
 * To find the various parameters about a file system we have to read
 * the superblock, block 0 of the raw device.  For now we just cheat and
 * use the root file system. */


int
nfsd_nfsproc_statfs_2(argp)
     nfs_fh *argp;
{
    nfsstat status;
    char *path;
    struct statfs fsd;

    if ((path = fh_path(argp, &status)) == NULL)
	return NFSERR_STALE;

    if (statfs(path, &fsd) < 0)
	return nfs_errno();
    result.statfsres.status = NFS_OK;
    result.statfsres.statfsres_u.reply.tsize = fsd.f_bsize; /* XXX */
    result.statfsres.statfsres_u.reply.bsize = fsd.f_bsize;
    result.statfsres.statfsres_u.reply.blocks = fsd.f_blocks;
    result.statfsres.statfsres_u.reply.bfree = fsd.f_bfree;
    result.statfsres.statfsres_u.reply.bavail = fsd.f_bavail;

    return NFS_OK;
}

