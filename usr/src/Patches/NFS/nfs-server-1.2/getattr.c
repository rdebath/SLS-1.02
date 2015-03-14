#include "nfsd.h"
#include "libc.h"
#include "fh.h"

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


#ifdef DEBUG
#include <stdio.h>
extern FILE *debuglog;
#define debug_1(str1)		if (debuglog) fprintf(debuglog, str1)
#define debug_2(str1, str2)  	if (debuglog) fprintf(debuglog, str1, str2)
#define debug_3(s1, s2, s3)	if (debuglog) fprintf(debuglog, s1, s2, s3)
#else
#define debug_1(str1) 		do{} while(0)
#define debug_2(str1, str2) 	do{} while(0)
#define debug_3(s1, s2, s3) 	do{} while(0)
#endif

/* The NFS version 2 specification fails to mention all of these file types,
   but they exist in the nfs_prot.x file. */
#define ftype_map(st_mode) (_ftype_map[((st_mode) & S_IFMT) >> 12])
ftype _ftype_map[16]= {
#ifdef S_IFIFO
    NFNON, NFFIFO, NFCHR, NFBAD,
#else
    NFNON, NFBAD, NFCHR, NFBAD,
#endif
    NFDIR, NFBAD, NFBLK, NFBAD,
    NFREG, NFBAD, NFLNK, NFBAD,
    NFSOCK, NFBAD, NFBAD, NFBAD,
};

nfsstat
getattr(fh, attr, stat_optimize)
     nfs_fh		*fh;
     fattr		*attr;
     struct stat	*stat_optimize;
{
    nfsstat	status;
    char 	*path;
    struct stat *s;
    struct stat sbuf;
    
    debug_1("   getattr");

    if ((path = fh_path(fh, &status)) == NULL) {
	debug_1(" failed!  No such file.\n");
	return NFSERR_STALE;
    }

    debug_2(" on path '%s'\n\t", path);

    if (stat_optimize != NULL)
	s = stat_optimize;
    else if (lstat(path, (s = &sbuf)) != 0) {
	debug_3(" failed!  path='%s' errno=%d\n", path, errno);
	return nfs_errno();
    }
    
    attr->type = ftype_map(s->st_mode);
    attr->mode = s->st_mode;
    attr->nlink = s->st_nlink;
    attr->uid = ruid(s->st_uid);
    attr->gid = rgid(s->st_gid);
    if (S_ISLNK(s->st_mode))
	attr->size = NFS_MAXPATHLEN; /* to account for relative stuff - jrs */
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
    if (debuglog) {
	if (status == NFS_OK) {
	    fprintf(debuglog, " t=%d, m=%o, lk=%d, u/g=%d/%d, sz=%d, bsz=%d",
		    attr->type, attr->mode, attr->nlink,
		    attr->uid, attr->gid, attr->size,
		    attr->blocksize);
	    if (attr->type == NFCHR || attr->type == NFBLK)
		fprintf(debuglog, " rdev=%d/%d",
			(attr->rdev>>8)&0xff, attr->rdev&0xff);
	    fprintf(debuglog, "\n  blks=%d, fsid=%d, psi=%d, at=%d, mt=%d, ct=%d\n",
		    attr->blocks, attr->fsid, attr->fileid,
		    attr->atime.seconds,
		    attr->mtime.seconds,
		    attr->ctime.seconds);
	}
	else
	    fprintf(debuglog, " >>> %s\n", sys_errlist[(int) status]);
    }
#endif

    return NFS_OK;
}

