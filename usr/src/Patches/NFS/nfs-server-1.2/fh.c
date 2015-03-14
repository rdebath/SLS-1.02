/* UNFSD - copyright Mark A Shand, May 1988.
 *         copyright Donald J. Becker, Harris Corp.  Jan 1989
 * This software maybe be used for any purpose provided
 * the above copyright notice is retained.  It is supplied
 * as is, with no warranty expressed or implied.
 *
 *  Originally written by Mark Shand.
 *  Rewritten to allow write access, fix bugs, and improve
 *  performance by Don Becker (becker@trantor.harris-ad.com).
 */

/*
 *	FILE HANDLE PACKAGE FOR USER-LEVEL NFS SERVER
 *
 *	Interfaces:
 *	    pseudo_inode
 *		mostly used internally, but also called from unfsd.c
 *		when reporting directory contents.
 *	    fh_init
 *		Initializes the queues and 'flush' timer
 *	    fh_pr
 *		debugging primitive; converts file handle into a printable
 *		text string
 *	    fh_create
 *		establishes initial file handle; called from mount daemon
 *	    fh_path
 *		returns unix path corresponding to fh
 *	    fh_fd
 *		returns open file descriptor for given file handle;
 *		provides caching of open files
 *	    fd_idle
 *		provides mututal exclusion of normal file descriptor cache
 *		use, and alarm-driven cache flushing
 *	    fh_compose
 *		construct new file handle from existing file handle and
 *		directory entry
 *	    fh_psi
 *		returns pseudo_inode corresponding to file handle
 *	    fh_remove (new, by Don Becker)
 *		delete the file handle associated with PATH from the cache
 */

#include <sys/param.h>
#include <sys/types.h>
#include <sys/file.h>
/*#include <sys/stat.h> Now in libc.h*/
#include <rpc/rpc.h>
/*#include <sys/time.h> Now in hnfsd.h*/
#include <sys/dir.h>
#include <signal.h>
#include <strings.h>
#include <errno.h>
#include <syslog.h>

/* mask SUNOS/BSD4.3 syslog incompatibilities */
#ifndef LOG_DAEMON
#define	LOG_DAEMON	0
#endif /* LOG_DAEMON */

#ifdef DEBUG
#include <stdio.h>
FILE *debuglog;
#endif DEBUG

#include "nfs_prot.h"
#include "libc.h"
#include "fh.h"

/* External declarations. */
void mallocfailed();

#define	FHC_XONLY_PATH	01
#define	FHC_BUSY	02	/* NOT USED */

#define	CACHE_SIZE_LIMIT	500
#define	LOWAT_CACHE_SIZE	3*CACHE_SIZE_LIMIT
#define	HIWAT_CACHE_SIZE	4*CACHE_SIZE_LIMIT
#define HASH_TAB_SIZE		(5*CACHE_SIZE_LIMIT | 03)

/* Forward declared local functions */
static void fh_flush_fd(void);
static int path_psi(char *path, nfsstat *status, struct stat *sbp);
static char *fh_buildpath(svc_fh *h);


/*
 * Paths constructed in this system always consist of real directories
 * (excepting the last element) i.e. they do not contain symbolic links.
 * This is guaranteed by the way NFS constructs the paths.
 * As a consequence we may assume that
 *	/x/y/z/.. == /x/y
 * and	/x/y/z/. == /x/y/z
 * provided that z != . && z != ..
 * These relations are exploited in fh_compose.
 *
 * Further assumptions:
 *	All cached pathnames consist of a leading /
 *	followed by zero or more / separated names
 *	s.t.
 *		name != .
 *		name != ..
 *		index(name, '/') == 0
 */

typedef struct fhcache
{
	struct fhcache	*next;
	struct fhcache	*prev;
	struct fhcache	*hash_next;
	svc_fh	h;
	int	fd;
	int	omode;
	char	*path;
	time_t	last_used;
	int	flags;
}
	fhcache;

static fhcache fh_head, fh_tail, *last_flushable;
static fhcache *fh_hashed[HASH_TAB_SIZE];
static int fh_list_size;
static time_t	curtime;
extern int errno;
int fh_initialized = 0;

struct {
	enum nfsstat error;
	int errno;
} nfs_errtbl[] = {
	{ NFS_OK,		0		},
	{ NFSERR_PERM,		EPERM		},
	{ NFSERR_NOENT,		ENOENT		},
	{ NFSERR_IO,		EIO		},
	{ NFSERR_NXIO,		ENXIO		},
	{ NFSERR_ACCES,		EACCES		},
	{ NFSERR_EXIST,		EEXIST		},
	{ NFSERR_NODEV,		ENODEV		},
	{ NFSERR_NOTDIR,	ENOTDIR		},
	{ NFSERR_ISDIR,		EISDIR		},
	{ NFSERR_FBIG,		EFBIG		},
	{ NFSERR_NOSPC,		ENOSPC		},
	{ NFSERR_ROFS,		EROFS		},
	{ NFSERR_NAMETOOLONG,	ENAMETOOLONG	},
	{ NFSERR_NOTEMPTY,	ENOTEMPTY	},
#ifdef EDQUOT
	{ NFSERR_DQUOT,		EDQUOT		},
#endif
	{ NFSERR_STALE,		ESTALE		},
	{ NFSERR_WFLUSH,	EIO		},
	{ -1,			EIO		}
};

enum nfsstat nfs_errno()
{
	int i;

	for (i = 0; nfs_errtbl[i].error != -1; i++) {
		if (nfs_errtbl[i].errno == errno)
			return nfs_errtbl[i].error;
	}
	syslog(LOG_DAEMON|LOG_WARNING,
		"non-standard errno: %d (%s)", errno, strerror(errno));
	return NFSERR_IO;
}

static void
fh_move_to_front(fhc)
fhcache	*fhc;
{
	/* Remove from current posn */
	fhc->prev->next = fhc->next;
	fhc->next->prev = fhc->prev;
	/* Insert at head */
	fhc->prev = &fh_head;
	fhc->next = fh_head.next;
	fhc->prev->next = fhc;
	fhc->next->prev = fhc;
}

static void
fh_inserthead(fhc)
fhcache	*fhc;
{
	fhcache	**hash_slot;

	/* Insert at head */
	fhc->prev = &fh_head;
	fhc->next = fh_head.next;
	fhc->prev->next = fhc;
	fhc->next->prev = fhc;
	fh_list_size++;

	/* Insert into hash tab */
	hash_slot = &(fh_hashed[fhc->h.psi % HASH_TAB_SIZE]);
	fhc->hash_next = *hash_slot;
	*hash_slot = fhc;
}

static fhcache *
fh_lookup(psi)
u_long	psi;
{
	fhcache	*fhc;
	fhc = fh_hashed[psi % HASH_TAB_SIZE];
	while (fhc != NULL && fhc->h.psi != psi)
		fhc = fhc->hash_next;
	return fhc;
}

static void
fh_delete(fhc)
fhcache *fhc;
{
	fhcache	**hash_slot;

	/* Remove from current posn */
	fhc->prev->next = fhc->next;
	fhc->next->prev = fhc->prev;
	fh_list_size--;

	/* Remove from hash tab */
	hash_slot = &(fh_hashed[fhc->h.psi % HASH_TAB_SIZE]);
	while (*hash_slot != NULL && *hash_slot != fhc)
		hash_slot = &((*hash_slot)->hash_next);
	if (*hash_slot == NULL)
		syslog(LOG_DAEMON|LOG_WARNING,
			"internal inconsistency -- fhc(%x) not in hash table", fhc);
	else
		*hash_slot = fhc->hash_next;

	/* Free storage */
	if (fhc->path != NULL)
	{
#ifdef DEBUG
		if (debuglog != NULL)
			fprintf(debuglog, "flushing: %s\n",
				fhc->path);
#endif DEBUG
		free(fhc->path);
	}
	if (fhc->fd >= 0)
	    close(fhc->fd);
	free(fhc);
}

/*
 * INODES and DEVICES.  NFS assumes that each file within an NFS mounted
 * file-system has a unique inode number.  Thus to mount an entire file
 * hierarchy, as this server sets out to do, requires mapping from inode/devs
 * to pseudo-inode.  Furthermore mount points must be detected and so that
 *	pseudo-inode("name") == pseudo-inode(direntry("name/../name"))
 * One option is to force the directory entry inode to correspond to the
 * result of the stat call, but this involves stat'ing every directory entry
 * during a readdir.  Instead we force the stat call to corresopnd to the
 * directory entry inode (see inner_getattr).  Of course this technique
 * requires that the parent directory is readable.  If it is not the normal
 * stat call result is used.  There is no chance of conflict because the
 * directory can never be read.
 *
 * In theory unique pseudo-inodes cannot be guaranteed, since inode/dev
 * contains 48 bits of information which must be crammed into an inode
 * number constrained to 32 bits.  Fortunately inodes numbers tend to be
 * small (often < 64k, almost always < 512k)
 */

int
pseudo_inode(inode,dev)
u_long	inode;
u_short	dev;
{
	register dmajor,dminor;

	/* Assuming major and minor numbers are small integers,
	 * gravitate bits of dmajor & dminor device number to
	 * high-order bits of word, to avoid clash with real inode num.
	 */
	/* reverse (byte-wise) */
	dmajor = ((dev & 0xf0f) << 4) | ((dev & 0xf0f0) >> 4);
	dmajor = ((dmajor & 0x3333) << 2) | ((dmajor & 0xcccc) >> 2);
	dmajor = ((dmajor & 0x5555) << 1) | ((dmajor & 0xaaaa) >> 1);
	/* spread low-16 -> 32 with 0's in even posn */
	dmajor = ((dmajor & 0xff00) << 8) | (dmajor & 0xff);
	dmajor = ((dmajor & 0xf000f0) << 4) | (dmajor & 0xf000f);
	dmajor = ((dmajor & 0xc0c0c0c) << 2) | (dmajor & 0x3030303);
	dmajor = ((dmajor & 0x22222222) << 1) | (dmajor & 0x11111111);
	dminor = (dmajor & 0x5555) << 15;
	dmajor = dmajor & 0x55550000;

	return (dmajor | dminor) ^ inode;
}

#define hash_psi(psi) (((psi)^((psi)>>8)^((psi)>>16)^((psi)>>24)) & 0xff)


/* flush_cache() is invoked periodically from SIGALRM, and on
 * demand from fh_find.  A simple form of mutual exclusion
 * protects this routine from multiple concurrent executions.
 * Since the preemption that occurs when a signal is received
 * is one-sided, we do need an atomic test and set.  If the 
 * signal arrives between the test and the set, the first
 * invocation safely stalls until the signal-caused invocation
 * completes.
 */
typedef enum { inactive, active } mutex;

static mutex ex_state = inactive;

/* The following affect execute-only directories */
#define FLUSH_INTERVAL	(60*60*12)	/* Twice a day */
#define BUSY_RETRY_INTERVAL	(60*10)	/* Ten minutes */
#define DISCARD_INTERVAL (FLUSH_INTERVAL*2) /* Two days */

static void
flush_cache(sig, code, scp)
	 int sig, code;
	 struct sigcontext *scp;
{
	fhcache	*h;

#ifdef DEBUG
	if (debuglog != NULL)
	{
		long	thing;
		time(&thing);
		fprintf(debuglog, "flushing cache at %s: state = %s\n",
			ctime(&thing),
			(ex_state == inactive) ? "inactive" : "active");
	}
#endif DEBUG
	if (ex_state == inactive)
	{
		int	cache_size = 0;

		ex_state = active;
		time(&curtime);
		/* Single execution thread */
		/* discard old open files */
		fh_flush_fd();

		/* works in empty case because: fh_tail.next = &fh_tail */
		h = fh_head.next;
		while (h != &fh_tail)
		{
			if (cache_size > LOWAT_CACHE_SIZE
			    || (cache_size > CACHE_SIZE_LIMIT
					&& (h->flags & FHC_XONLY_PATH) == 0)
			    || curtime > h->last_used + DISCARD_INTERVAL
				|| sig == SIGHUP)
			{
				h = h->next;
				fh_delete(h->prev);
			}
			else
			{
				cache_size++;
				h = h->next;
			}
		}
		if (fh_list_size != cache_size)
			syslog(LOG_DAEMON|LOG_WARNING,
				"internal inconsistency (fh_list_size=%d) != (cache_size=%d)",
				fh_list_size, cache_size);
		fh_list_size = cache_size;
		ex_state = inactive;
		if (_rpcpmstart) {
			signal(SIGALRM, flush_cache);
			alarm(FLUSH_INTERVAL);
		}
	}
	else
	{
		if (!_rpcpmstart) {
			signal(SIGALRM, flush_cache);
			alarm(BUSY_RETRY_INTERVAL);
		}
	}
}

void
fh_init()
{
	if (fh_initialized)
		return;
	fh_initialized = 1;
	fh_head.next = fh_tail.next = &fh_tail;
	fh_head.prev = fh_tail.prev = &fh_head;
	last_flushable = &fh_tail;
	fh_tail.flags = FHC_XONLY_PATH;
	signal(SIGHUP, flush_cache);
	if (!_rpcpmstart) {
		signal(SIGALRM, flush_cache);
		alarm(FLUSH_INTERVAL);
	}
}


static fhcache *
fh_find(h, create)
svc_fh	*h;
int	create;
{
	fhcache	*fhc;

	ex_state = active;
	time(&curtime);
	while ((fhc = fh_lookup(h->psi)) != NULL)
	{
		/* but what if hash_paths are not the same? */
		/* Something is stale */
		if (bcmp(h->hash_path, fhc->h.hash_path, HP_LEN) != 0)
		{
			if (!create) {
				ex_state = inactive;
				return NULL;
			}
			fh_delete(fhc);
			break;
		}
		if (fhc != fh_head.next)
			fh_move_to_front(fhc);
		fhc->last_used = curtime;
		ex_state = inactive;
		return fhc;
	}
	if (fh_list_size > CACHE_SIZE_LIMIT)
	{
		/* don't flush current head */
		while (last_flushable != fh_head.next)
		{
			if ((last_flushable->flags & FHC_XONLY_PATH) == 0)
			{
				fhc = last_flushable;
				last_flushable = last_flushable->prev;
				fh_delete(fhc);
				break;
			}
			last_flushable = last_flushable->prev;
		}
		last_flushable = last_flushable->next;
	}
	if (create)
	{
		if ((fhc = (fhcache *) malloc(sizeof *fhc)) == NULL)
			mallocfailed();
		fhc->path = NULL;
		fhc->last_used = curtime;
		fhc->h = *h;
		fh_inserthead(fhc);
	}
	else
	{
		/* attempt to contruct from hash_path */
		char	*path;

		if ((path = fh_buildpath(h)) == NULL) {
			ex_state = inactive;
			return NULL;
		}
		if ((fhc = (fhcache *) malloc(sizeof *fhc)) == NULL)
			mallocfailed();
		fhc->path = path;
		fhc->fd = -1;
		fhc->flags = 0;
		fhc->last_used = curtime;
		fhc->h = *h;
		fh_inserthead(fhc);
	}
	ex_state = inactive;
	if (fh_list_size > HIWAT_CACHE_SIZE)
		flush_cache(0, 0, (struct sigcontext *)0);
	return fhc;
}

static char *
fh_buildpath(h)
svc_fh	*h;
{
	int	i;
	int	psi;
	char	*path;
	struct	stat	sbuf;
	char	pathbuf[MAXPATHLEN+MAXNAMLEN+1];
	long	cookie_stack[HP_LEN+1];
	char	*slash_stack[HP_LEN];

	if (stat("/", &sbuf) < 0)
		return NULL;
	psi = pseudo_inode(sbuf.st_ino, sbuf.st_dev);
	if (h->hash_path[0] == 0)
	{
		if (psi != h->psi)
			return NULL;
		if ((path = malloc(sizeof("/"))) == NULL)
			mallocfailed();
		strcpy(path, "/");
		return path;
	}
	/* else */
	if (hash_psi(psi) != h->hash_path[1])
		return NULL;
	strcpy(pathbuf, "/");
	cookie_stack[2] = 0;
	for (i = 2; i <= h->hash_path[0]+1; i++)
	{
		DIR		*dir;
		struct direct	*dp;

	    backtrack:
		if (stat(pathbuf, &sbuf) >= 0
		    && (dir = opendir(pathbuf)) != NULL)
		{
			if (cookie_stack[i] != 0)
				seekdir(dir, cookie_stack[i]);
			while ((dp = readdir(dir)))
			{
				if (strcmp(dp->d_name, ".") != 0
				 && strcmp(dp->d_name, "..") != 0)
				{
					psi = pseudo_inode(dp->d_ino, sbuf.st_dev);
					if (i == h->hash_path[0]+1)
					{
						if (psi == h->psi)
						{
							/*GOT IT*/
							strcat(pathbuf, dp->d_name);
							if ((path = malloc(strlen(pathbuf)+1)) == NULL)
								mallocfailed();
							strcpy(path, pathbuf);
							closedir(dir);
							return path;
						}
					}
					else
					{
						if (hash_psi(psi) == h->hash_path[i])
						{
							/*PERHAPS WE'VE GOT IT */
							cookie_stack[i] = telldir(dir);
							cookie_stack[i+1] = 0;
							slash_stack[i] = pathbuf + strlen(pathbuf);
							strcpy(slash_stack[i], dp->d_name);
							strcat(pathbuf, "/");

							closedir(dir);
							goto deeper;
						}
					}
				}
			}
			/* dp == NULL */
			closedir(dir);
		}
		else if (i <= h->hash_path[0]
			&& access(pathbuf, R_OK) != 0
			&& access(pathbuf, X_OK) == 0)
		{
			/* Execute-only directory?  Maybe its in the cache. */
			/* Note: cache is frozen for duration of fh_buildpath */
			svc_fh	xh;
			fhcache	*fhc;

			xh = *h;
			xh.hash_path[0] = i-1;
			if (cookie_stack[i] == 0)
				fhc = fh_head.next;
			else
				fhc = ((fhcache *)(cookie_stack[i]))->next;
			while (fhc != &fh_tail)
				if (bcmp(xh.hash_path, fhc->h.hash_path, i) == 0
				    && xh.hash_path[i] == hash_psi(fhc->h.psi))
					break;
				else
					fhc = fhc->next;
			if (fhc != NULL)
			{
				strcpy(pathbuf, fhc->path);
				cookie_stack[i] = (long) fhc;
				cookie_stack[i+1] = 0;
				slash_stack[i] = rindex(pathbuf,'/')+1;
				strcat(pathbuf, "/");
				goto deeper;
			}
		}
		/* shallower */
		i--;
		if (i < 2)
			return NULL; /* SEARCH EXHAUSTED */
		/* Prune path */
		*(slash_stack[i]) = '\0';
		goto backtrack;
	    deeper: ;
	}
	return NULL; /* Actually not reached */
}

char *
fh_pr(fh)
nfs_fh	*fh;
{
	char	*p;
	nfsstat	status;

	p = fh_path(fh, &status);
	if (status != NFS_OK)
		return "///STALE///";
	else
		return p;
}

/* This routine is only uesd by the mount daemon.  It creates the initial file
 * handle.
 */
int
fh_create(fh, path)
nfs_fh	*fh;
char	*path;
{
	svc_fh	*key = (svc_fh *) fh;
	fhcache	*h;
	int psi;
	nfsstat	status;
	char	*s;

	bzero((char *) fh, sizeof fh);
	key->hash_path[0] = 0;
	status = NFS_OK;
	if ((psi = path_psi("/", &status, NULL)) == 0)
		return (int) status;
	s = path;
	while ((s = index(s+1, '/')) != NULL)
	{
		if (++(key->hash_path[0]) >= HP_LEN)
			return (int) NFSERR_NAMETOOLONG;
		key->hash_path[key->hash_path[0]] = hash_psi(psi);
		*s = '\0';
		if ((psi = path_psi(path, &status, NULL)) == 0)
			return (int) status;
		*s = '/';
	}
	if (*(rindex(path, '/')+1) != '\0')
	{
		if (++(key->hash_path[0]) >= HP_LEN)
			return (int) NFSERR_NAMETOOLONG;
		key->hash_path[key->hash_path[0]] = hash_psi(psi);
		if ((psi = path_psi(path, &status, NULL)) == 0)
			return (int) status;
	}
	key->psi = psi;
	h = fh_find(key, 1);
	/* assert(h != NULL); */
	if (h->path == NULL)
	{
		h->fd = -1;
		if ((h->path = malloc(strlen(path)+1)) == NULL)
			mallocfailed();
		strcpy(h->path, path);
		h->flags = 0;
	}
	return (int) status;
}

char *
fh_path(fh, status)
nfs_fh	*fh;
nfsstat	*status;
{
	fhcache	*h;

	if ((h = fh_find((svc_fh *) fh, 0)) == NULL)
	{
		*status = NFSERR_STALE;
		return NULL;
	}
	*status = NFS_OK;
	return h->path;
}

static mutex io_state = inactive;
static int	fd = -1;

static void fh_flush_fd()
{
    if (io_state == inactive) {
	/* special case -- request to flush fd cache */
	if (fd >= 0)
	    close(fd);
	fd = -1;
    }
}

int
fh_fd(fh, status, omode)
nfs_fh	*fh;
nfsstat	*status;
int	omode;
{
	/* Currently we cache 1 open file descriptor.
	 * in the future we could allocate an array of size
	 * getdtablesize() which would contain psi's to provide
	 * an mapping from descriptor to psi's.
	 * Then we could maintain many concurrently open files.
	 */
	fhcache		*h;
	static int	psi = 0;

	if ((h = fh_find((svc_fh *) fh, 0)) == NULL)
	{
		*status = NFSERR_STALE;
		return -1;
	}
	io_state = active;
	if (fd >= 0)
	{
		if (psi == h->h.psi && h->omode == omode)
		{
			if (h->fd != -1)
				return fd;
			/* oops! something is hosed */
		}
		close(fd);
	}
	errno = 0;
	if (!h->path) /* something is really hosed */
		return -1;
	h->fd = fd = open(h->path, omode, 0);
	h->omode = omode;
	psi = h->h.psi;
	*status = nfs_errno();
	return fd;
}

void
fd_inactive(fd)
int	fd;
{
	io_state = inactive;
}

nfsstat
fh_compose(dopa, new_fh, sbpp)
diropargs	*dopa;
nfs_fh	*new_fh;
struct stat **sbpp;
{
	svc_fh	*key;
	fhcache	*dirh, *h;
	char	*sindx;
	int	is_dd;
	nfsstat	ret;
	char	pathbuf[MAXPATHLEN+MAXNAMLEN+1];

	if ((dirh = fh_find((svc_fh *) &(dopa->dir), 0)) == NULL)
		return NFSERR_STALE;

	/* Construct path */

	if (strcmp(dopa->name, ".") == 0)
	{
		*new_fh = dopa->dir;
		*sbpp = NULL;
		return NFS_OK;
	}
	if (strcmp(dopa->name, "..") == 0)
	{
		is_dd = 1;
		sindx = rindex(dirh->path, '/');
		if (sindx == dirh->path)
			strcpy(pathbuf, "/");
		else
		{
			int	len = sindx - dirh->path;
			strncpy(pathbuf, dirh->path, len);
			pathbuf[len] = '\0';
		}
	}
	else
	{
		int	len = strlen(dirh->path);

		is_dd = 0;
		if (dirh->path[len-1] == '/')
			len--;
		strncpy(pathbuf, dirh->path, len);
		pathbuf[len] = '/';
		strcpy(pathbuf + (len+1), dopa->name);
	}

	*new_fh = dopa->dir;

	key = (svc_fh *) new_fh;
	if ((key->psi = path_psi(pathbuf, &ret, *sbpp)) == 0)
		return ret;

	if (is_dd)
		key->hash_path[key->hash_path[0]--] = 0;
	else
	{
		if (++(key->hash_path[0]) >= HP_LEN)
			return NFSERR_NAMETOOLONG;
		key->hash_path[key->hash_path[0]] = hash_psi(dirh->h.psi);
	}
	h = fh_find(key, 1);
	/* New code added by Don Becker */
	if ((h->path != NULL)  &&  (strcmp(h->path, pathbuf) != 0)) {
	    /* We must have cached an old file under the same inode number. */
	    fh_delete(h);
	    h = fh_find(key, 1);
	    if (h->path)
		syslog(LOG_DAEMON|LOG_WARNING,
		       "Internal inconsistency: double entry (path '%s', now '%s').",
		       h->path, pathbuf);
	}
	/* End of new code */
	/* assert(h != NULL); */
	if (h->path == 0) {
		h->fd = -1;
		if ((h->path = malloc(strlen(pathbuf)+1)) == NULL)
			mallocfailed();
		strcpy(h->path, pathbuf);
		h->flags = 0;
		if (!is_dd
		    && access(dirh->path, R_OK) != 0
		    && access(dirh->path, X_OK) == 0)
			h->flags |= FHC_XONLY_PATH;
	}
	return NFS_OK;
}

int
fh_psi(fh)
nfs_fh	*fh;
{
	svc_fh	*h = (svc_fh *) fh;
	return h->psi;
}

static int
path_psi(path, status, sbp)
	char	*path;
	nfsstat	*status;
	struct stat *sbp;
{
	struct stat sbuf;

	if (sbp == NULL)
		sbp = &sbuf;
	if (lstat(path, sbp) < 0)
	{
		*status = nfs_errno();
		return 0;
	}
		
	if ((sbp->st_mode & S_IFMT) == S_IFDIR && strcmp(path, "/") != 0)
	{
		/* Special case for directories--test for mount point */
		struct	stat	ddbuf;
		char	*sindx;
		char	*name;
		char	squirrel;

		/* find start of last component of path */
		if ((sindx = rindex(path, '/')) == path)
		{
			sindx++;
			name = sindx;
		}
		else
			name = sindx + 1;
		/* remove last element of path */
		squirrel = *sindx;
		*sindx = '\0';
		if (lstat(path, &ddbuf) < 0)
		{
			*sindx = squirrel;
			*status = nfs_errno();
			return 0;
		}
		/* sindx now points to directory entry name */
		if (ddbuf.st_dev != sbp->st_dev)
		{
			/* directory is a mount point */
			DIR	*dirp;
			struct direct *dp;

			errno = 0;
			if ((dirp = opendir(path)) == NULL)
			{
				*sindx = squirrel; /* restore path */
				if (errno == EACCES)
					goto unreadable;
				if (errno != 0)
					*status = nfs_errno();
				else
					*status = NFSERR_NOENT;
			}
			else
			{
				*sindx = squirrel; /* restore path */
				*status = NFS_OK;
				do
				{
					if ((dp = readdir(dirp)) == NULL)
					{
						*status = NFSERR_NOENT;
						closedir(dirp);
						return 0;
					}
				} while(strcmp(name, dp->d_name) != 0);
				sbp->st_dev = ddbuf.st_dev;
				sbp->st_ino = dp->d_ino;
				closedir(dirp);
			}
		}
		else
			*sindx = squirrel; /* restore path */ 
	    unreadable:
		;
	}
	return pseudo_inode(sbp->st_ino, sbp->st_dev);
}

void mallocfailed(void)
{
    syslog(LOG_DAEMON|LOG_WARNING, "malloc failed -- exiting");
    exit(1);
}

/*****************************************************************************\
*									      *
*	File:     new_fh.c						      *
*	Author:   Don Becker						      *
*	Created:  Fri Jun 16 16:09:08 1989				      *
*	Contents: New files for fh.c.					      *
*									      *
******************************************************************************/


void
fh_remove(path)
     char *path;
{
    int psi;
    nfsstat status;
    fhcache *fhc;

    psi = path_psi(path, &status, NULL);
    if (psi == 0)
	return;
    ex_state = active;
    fhc = fh_lookup(psi);
    if (fhc != NULL)
	fh_delete(fhc);

    ex_state = inactive;
    return;
}
