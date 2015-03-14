/*
 * fh		This module handles the file-handle cache.
 *		FILE HANDLE PACKAGE FOR USER-LEVEL NFS SERVER
 *
 *		Interfaces:
 *		    pseudo_inode
 *			mostly used internally, but also called from unfsd.c
 *			when reporting directory contents.
 *		    fh_init
 *			Initializes the queues and 'flush' timer
 *		    fh_pr
 *			debugging primitive; converts file handle into a
 *			printable text string
 *		    fh_create
 *			establishes initial file handle; called from mount
 *			daemon
 *		    fh_path
 *			returns unix path corresponding to fh
 *		    fh_fd
 *			returns open file descriptor for given file handle;
 *			provides caching of open files
 *		    fd_idle
 *			provides mututal exclusion of normal file descriptor
 *			cache use, and alarm-driven cache flushing
 *		    fh_compose
 *			construct new file handle from existing file handle
 *			and directory entry
 *		    fh_psi
 *			returns pseudo_inode corresponding to file handle
 *		    fh_remove (new, by Don Becker)
 *			delete the file handle associated with PATH from the
 *			cache
 *
 * Version:	@(#)fh.c	1.5	4/10/93
 *
 * Authors:	Mark A. Shand, May 1988
 *		Donald J. Becker <becker@super.org>
 *		Rick Sladkey <jrs@world.std.com>
 *		Patrick	Sweeney <pjs@raster.Kodak.COM>
 *		Orest Zborowski <obz@raster.Kodak.COM>
 *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *
 *		Copyright 1988 Mark A. Shand
 *		This software maybe be used for any purpose provided
 *		the above copyright notice is retained.  It is supplied
 *		as is, with no warranty expressed or implied.
 */

#include "nfsd.h"
#include <sys/dir.h>


#define hash_psi(psi) (((psi)^((psi)>>8)^((psi)>>16)^((psi)>>24)) & 0xff)


static mutex ex_state = inactive;
static mutex io_state = inactive;
static fhcache fh_head, fh_tail, *last_flushable;
static fhcache *fh_hashed[HASH_TAB_SIZE];
static int fh_list_size;
static time_t curtime;
static fhcache *fd_cache[FOPEN_MAX] = { NULL };
static int fd_cache_size = 0;
struct {
	enum nfsstat error;
	int errno;
} nfs_errtbl[]= {
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
int fh_initialized = 0;


/* Forward declared local functions */
static _PRO(int path_psi, (char *, nfsstat *, struct stat *));
static _PRO(void flush_cache, (int));


void mallocfailed(void)
{
	dprintf(0, "malloc failed -- exiting\n");
	exit(1);
}


static void fh_move_to_front(fhc)
fhcache *fhc;
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


static void fh_inserthead(fhc)
fhcache *fhc;
{
	register fhcache **hash_slot;

	/* Insert at head. */
	fhc->prev = &fh_head;
	fhc->next = fh_head.next;
	fhc->prev->next = fhc;
	fhc->next->prev = fhc;
	fh_list_size++;

	/* Insert into hash tab. */
	hash_slot = &(fh_hashed[fhc->h.psi % HASH_TAB_SIZE]);
	fhc->hash_next = *hash_slot;
	*hash_slot = fhc;
}


static fhcache *fh_lookup(psi)
u_long psi;
{
	register fhcache *fhc;

	fhc = fh_hashed[psi % HASH_TAB_SIZE];
	while (fhc != NULL && fhc->h.psi != psi)
		fhc = fhc->hash_next;
	return (fhc);
}


static void fh_close(fhcache * fhc)
{
	if (fhc->fd >= 0) {
		dprintf(1, "fh_close: closing handle %x ('%s', fd=%d)\n",
		      fhc, fhc->path ? fhc->path : "<unnamed>", fhc->fd);
#if 0
		DBASSERT(fd_cache[fhc->fd] == fhc);
#endif
		fd_cache[fhc->fd] = NULL;
		--fd_cache_size;
		close(fhc->fd);
		fhc->fd = -1;
	}
}


static void fh_delete(fhc)
fhcache *fhc;
{
	register fhcache **hash_slot;

	dprintf(1, "fh_delete: deleting handle %x ('%s', fd=%d)\n", fhc,
		fhc->path ? fhc->path : "<unnamed>", fhc->fd);

	/* Remove from current posn */
	fhc->prev->next = fhc->next;
	fhc->next->prev = fhc->prev;
	fh_list_size--;

	/* Remove from hash tab */
	hash_slot = &(fh_hashed[fhc->h.psi % HASH_TAB_SIZE]);
	while (*hash_slot != NULL && *hash_slot != fhc)
		hash_slot = &((*hash_slot)->hash_next);
	if (*hash_slot == NULL)
		dprintf(0, "internal inconsistency -- fhc(%x) not in hash table\n",
			fhc);
	else
		*hash_slot = fhc->hash_next;

	fh_close(fhc);

	/* Free storage. */
	if (fhc->path != NULL)
		free(fhc->path);
	free(fhc);
}


/* Lookup an NFS error code and return UNIX equivalent. */
enum nfsstat nfs_errno()
{
	int i;

	for (i = 0; nfs_errtbl[i].error != -1; i++) {
		if (nfs_errtbl[i].errno == errno)
			return (nfs_errtbl[i].error);
	}
	dprintf(0, "non-standard errno: %d (%s)\n", errno, strerror(errno));
	return (NFSERR_IO);
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
int pseudo_inode(inode, dev)
u_long inode;
u_short dev;
{
	register dmajor, dminor;

	/*
         * Assuming major and minor numbers are small integers,
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

	dprintf(1, "pseudo_inode: dev=%d, inode=%d, psi=%d\n", dev, inode,
		(dmajor | dminor) ^ inode);
	return ((dmajor | dminor) ^ inode);
}


static char *fh_buildpath(h)
svc_fh *h;
{
	int i;
	int psi;
	char *path;
	struct stat sbuf;
	char pathbuf[MAXPATHLEN + MAXNAMLEN + 1];
	long cookie_stack[HP_LEN + 1];
	char *slash_stack[HP_LEN];

	if (stat("/", &sbuf) < 0)
		return (NULL);
	psi = pseudo_inode(sbuf.st_ino, sbuf.st_dev);
	if (h->hash_path[0] == 0) {
		if (psi != h->psi)
			return (NULL);
		if ((path = malloc(sizeof("/"))) == NULL)
			mallocfailed();
		strcpy(path, "/");
		return (path);
	}
	/* else */
	if (hash_psi(psi) != h->hash_path[1])
		return (NULL);
	strcpy(pathbuf, "/");
	cookie_stack[2] = 0;
	for (i = 2; i <= h->hash_path[0] + 1; i++) {
		DIR *dir;
		struct direct *dp;

	backtrack:
		if (stat(pathbuf, &sbuf) >= 0
		    && (dir = opendir(pathbuf)) != NULL) {
			if (cookie_stack[i] != 0)
				seekdir(dir, cookie_stack[i]);
			while ((dp = readdir(dir))) {
				if (strcmp(dp->d_name, ".") != 0
				    && strcmp(dp->d_name, "..") != 0) {
					psi = pseudo_inode(dp->d_ino, sbuf.st_dev);
					if (i == h->hash_path[0] + 1) {
						if (psi == h->psi) {
							/*GOT IT*/
							strcat(pathbuf, dp->d_name);
							if ((path = malloc(strlen(pathbuf) + 1)) == NULL)
								mallocfailed();
							strcpy(path, pathbuf);
							closedir(dir);
							return (path);
						}
					} else {
						if (hash_psi(psi) == h->hash_path[i]) {
							/*PERHAPS WE'VE GOT IT */
							cookie_stack[i] = telldir(dir);
							cookie_stack[i + 1] = 0;
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
		} else if (i <= h->hash_path[0] && access(pathbuf, R_OK) != 0
			   && access(pathbuf, X_OK) == 0) {

			/*
			 * Execute-only directory?  Maybe its in the cache.
			 * Note: cache is frozen for duration of fh_buildpath.
			 */
			svc_fh xh;
			fhcache *fhc;

			xh = *h;
			xh.hash_path[0] = i - 1;
			if (cookie_stack[i] == 0)
				fhc = fh_head.next;
			else
				fhc = ((fhcache *) (cookie_stack[i]))->next;
			while (fhc != &fh_tail) {
				if (bcmp(xh.hash_path, fhc->h.hash_path, i) == 0
				    && xh.hash_path[i] == hash_psi(fhc->h.psi))
					break;
				else
					fhc = fhc->next;
			}
			if (fhc != NULL) {
				strcpy(pathbuf, fhc->path);
				cookie_stack[i] = (long) fhc;
				cookie_stack[i + 1] = 0;
				slash_stack[i] = rindex(pathbuf, '/') + 1;
				strcat(pathbuf, "/");
				goto deeper;
			}
		}
		/* shallower */
		i--;
		if (i < 2)
			return (NULL);	/* SEARCH EXHAUSTED */

		/* Prune path */
		*(slash_stack[i]) = '\0';
		goto backtrack;
	deeper:
		;
	}
	return (NULL);		/* actually not reached */
}


static int path_psi(path, status, sbp)
char *path;
nfsstat *status;
struct stat *sbp;
{
	struct stat sbuf;

	if (sbp == NULL)
		sbp = &sbuf;
	if (lstat(path, sbp) < 0) {
		*status = nfs_errno();
		return (0);
	}
	if ((sbp->st_mode & S_IFMT) == S_IFDIR && strcmp(path, "/") != 0) {
		/* Special case for directories--test for mount point. */
		struct stat ddbuf;
		char *sindx;
		char *name;
		char squirrel;

		/* Find start of last component of path. */
		if ((sindx = rindex(path, '/')) == path) {
			sindx++;
			name = sindx;
		} else
			name = sindx + 1;

		/* Remove last element of path. */
		squirrel = *sindx;
		*sindx = '\0';
		if (lstat(path, &ddbuf) < 0) {
			*sindx = squirrel;
			*status = nfs_errno();
			return (0);
		}
		/* Sindx now points to directory entry name. */
		if (ddbuf.st_dev != sbp->st_dev) {
			/* Directory is a mount point. */
			DIR *dirp;
			struct direct *dp;

			errno = 0;
			if ((dirp = opendir(path)) == NULL) {
				*sindx = squirrel;	/* restore path */
				if (errno == EACCES)
					goto unreadable;
				if (errno != 0)
					*status = nfs_errno();
				else
					*status = NFSERR_NOENT;
			} else {
				*sindx = squirrel;	/* restore path */
				*status = NFS_OK;
				do {
					if ((dp = readdir(dirp)) == NULL) {
						*status = NFSERR_NOENT;
						closedir(dirp);
						return (0);
					}
				} while (strcmp(name, dp->d_name) != 0);
				sbp->st_dev = ddbuf.st_dev;
				sbp->st_ino = dp->d_ino;
				closedir(dirp);
			}
		} else
			*sindx = squirrel;	/* restore path */
	unreadable:
		;
	}
	return (pseudo_inode(sbp->st_ino, sbp->st_dev));
}


static fhcache *fh_find(h, create)
svc_fh *h;
int create;
{
	char buff[1024], *sp;
	register fhcache *fhc;

	sprintf(buff, "fh_find: psi=%d... ", h->psi);
	sp = buff + strlen(buff);
	ex_state = active;
	(void) time(&curtime);
	while ((fhc = fh_lookup(h->psi)) != NULL) {
		sprintf(sp, "found '%s', fd=%d\n", fhc->path ? fhc->path : "<unnamed>",
			fhc->fd);
		dprintf(1, buff);

		/* But what if hash_paths are not the same? Something is stale. */
		if (bcmp(h->hash_path, fhc->h.hash_path, HP_LEN) != 0) {
			dprintf(1, "fh_find: path mismatch - stale!\n");
			if (!create) {
				ex_state = inactive;
				return (NULL);
			}
			fh_delete(fhc);
			dprintf(1, "fh_find: deleted old handle... sortof\n");
			break;
		}
		if (fhc != fh_head.next)
			fh_move_to_front(fhc);
		fhc->last_used = curtime;
		ex_state = inactive;
		return (fhc);
	}

	dprintf(1, "not found.\n");
	if (fh_list_size > CACHE_SIZE_LIMIT) {
		/* Don't flush current head. */
		while (last_flushable != fh_head.next) {
			if ((last_flushable->flags & FHC_XONLY_PATH) == 0) {
				fhc = last_flushable;
				last_flushable = last_flushable->prev;
				fh_delete(fhc);
				break;
			}
			last_flushable = last_flushable->prev;
		}
		last_flushable = last_flushable->next;
	}
	if ((fhc = (fhcache *) malloc(sizeof *fhc)) == NULL)
		mallocfailed();
	if (create)
		fhc->path = NULL;
	else {
		/* attempt to contruct from hash_path */
		char *path;

		if ((path = fh_buildpath(h)) == NULL) {
			free(fhc);
			ex_state = inactive;
			return NULL;
		}
		fhc->path = path;
	}
	fhc->flags = 0;
	fhc->fd = -1;
	fhc->last_used = curtime;
	fhc->h = *h;
	fh_inserthead(fhc);
	dprintf(1, "fh_find: created new handle %x ('%s')\n", fhc,
		fhc->path ? fhc->path : "<unnamed>");
	ex_state = inactive;
	if (fh_list_size > HIWAT_CACHE_SIZE)
		flush_cache(0);
	return (fhc);
}


static int fh_flush_fds(void)
{
	int i;
	fhcache *discard;

	if (io_state == active) {
		dprintf(1, "fh_flush_fds: not flushing... io active\n");
		return (-1);
	}
	while (fd_cache_size > FD_CACHE_LIMIT) {
		discard = NULL;
		for (i = 0; i < FOPEN_MAX; ++i) {
			if (fd_cache[i] && (discard == NULL ||
			    fd_cache[i]->last_used < discard->last_used))
				discard = fd_cache[i];
		}
		if (discard == NULL) {
			dprintf(1, "fh_flush_fds: can't find oldest??\n");
			return (-1);
		}
		dprintf(1, "fh_flush_fds: discarding old handle %x\n", discard);
		fh_close(discard);
	}
	return (0);
}


/*
 * flush_cache() is invoked periodically from SIGALRM, and on
 * demand from fh_find.  A simple form of mutual exclusion
 * protects this routine from multiple concurrent executions.
 * Since the preemption that occurs when a signal is received
 * is one-sided, we do need an atomic test and set.  If the
 * signal arrives between the test and the set, the first
 * invocation safely stalls until the signal-caused invocation
 * completes.
 */
static void flush_cache(sig)
int sig;
{
	register fhcache *h;

#ifdef DEBUG
	time_t now;
	(void) time(&now);
	dprintf(1, "flushing cache at %s: state = %s\n", ctime(&now),
		(ex_state == inactive) ? "inactive" : "active");
#endif

	if (ex_state == inactive) {
		int cache_size = 0;

		ex_state = active;
		(void) time(&curtime);
		/* Single execution thread */

		/* works in empty case because: fh_tail.next = &fh_tail */
		h = fh_head.next;
		while (h != &fh_tail) {
			if (cache_size > LOWAT_CACHE_SIZE
			    || (cache_size > CACHE_SIZE_LIMIT
				&& (h->flags & FHC_XONLY_PATH) == 0)
			    || curtime > h->last_used + DISCARD_INTERVAL
			    || sig == SIGHUP) {
				h = h->next;
				fh_delete(h->prev);
			} else {
				cache_size++;
				h = h->next;
			}
		}
		if (fh_list_size != cache_size)
			dprintf(0, "internal inconsistency (fh_list_size=%d) != (cache_size=%d)\n",
				fh_list_size, cache_size);
		fh_list_size = cache_size;
		ex_state = inactive;
		if (_rpcpmstart) {
			(void) signal(SIGALRM, flush_cache);
			(void) alarm(FLUSH_INTERVAL);
		}
	} else {
		if (!_rpcpmstart) {
			(void) signal(SIGALRM, flush_cache);
			(void) alarm(BUSY_RETRY_INTERVAL);
		}
	}
}


void fh_init()
{
	if (fh_initialized)
		return;
	fh_initialized = 1;

	fh_head.next = fh_tail.next = &fh_tail;
	fh_head.prev = fh_tail.prev = &fh_head;
	last_flushable = &fh_tail;
	fh_tail.flags = FHC_XONLY_PATH;
	(void) signal(SIGHUP, flush_cache);
	if (!_rpcpmstart) {
		(void) signal(SIGALRM, flush_cache);
		(void) alarm(FLUSH_INTERVAL);
	}
}


char *fh_pr(fh)
nfs_fh *fh;
{
	char *p;
	nfsstat status;

	p = fh_path(fh, &status);
	if (status != NFS_OK)
		return ("///STALE///");
	else
		return (p);
}


/*
 * This routine is only used by the mount daemon.
 * It creates the initial file handle.
 */
int fh_create(fh, path)
nfs_fh *fh;
char *path;
{
	svc_fh *key = (svc_fh *) fh;
	fhcache *h;
	int psi;
	nfsstat status;
	char *s;

	bzero((char *) fh, sizeof fh);
	key->hash_path[0] = 0;
	status = NFS_OK;
	if ((psi = path_psi("/", &status, NULL)) == 0)
		return ((int) status);
	s = path;
	while ((s = index(s + 1, '/')) != NULL) {
		if (++(key->hash_path[0]) >= HP_LEN)
			return ((int) NFSERR_NAMETOOLONG);
		key->hash_path[key->hash_path[0]] = hash_psi(psi);
		*s = '\0';
		if ((psi = path_psi(path, &status, NULL)) == 0)
			return ((int) status);
		*s = '/';
	}
	if (*(rindex(path, '/') + 1) != '\0') {
		if (++(key->hash_path[0]) >= HP_LEN)
			return ((int) NFSERR_NAMETOOLONG);
		key->hash_path[key->hash_path[0]] = hash_psi(psi);
		if ((psi = path_psi(path, &status, NULL)) == 0)
			return ((int) status);
	}
	key->psi = psi;
	h = fh_find(key, 1);

	/* assert(h != NULL); */
	if (h->path == NULL) {
		h->fd = -1;
		if ((h->path = malloc(strlen(path) + 1)) == NULL)
			mallocfailed();
		strcpy(h->path, path);
		h->flags = 0;
	}
	return ((int) status);
}


char *fh_path(fh, status)
nfs_fh *fh;
nfsstat *status;
{
	fhcache *h;

	if ((h = fh_find((svc_fh *) fh, 0)) == NULL) {
		*status = NFSERR_STALE;
		return (NULL);
	}
	*status = NFS_OK;
	return (h->path);
}


int path_open(path, omode, perm)
char *path;
int omode;
int perm;
{
	int fd;
	int oerrno, euid;
	struct stat buf;

	fh_flush_fds();
	fd = open(path, omode, perm);

	/* Do some serious cheating to permit a stateless server. */
	oerrno = errno;
	if (fd < 0 && oerrno == EACCES && stat(path, &buf) != -1
	    && buf.st_uid == (euid = geteuid())) {
		setreuid(-1, 0);
		fd = open(path, omode, perm);
		oerrno = errno;
		(void) setreuid(-1, euid);
	}
	errno = oerrno;
	return (fd);
}


int fh_fd(fh, status, omode)
nfs_fh *fh;
nfsstat *status;
int omode;
{
	fhcache *h;

	if ((h = fh_find((svc_fh *) fh, 0)) == NULL) {
		*status = NFSERR_STALE;
		return (-1);
	}
	if (h->fd >= 0) {
		if (h->omode == omode ||
		    ((omode == O_RDONLY || omode == O_WRONLY) && h->omode == O_RDWR)) {
			dprintf(1, "fh_fd: reusing fd=%d\n", h->fd);
#if 0
			DBASSERT(fd_cache[h->fd] == h);
#endif
			return (h->fd);
		}
		dprintf(1, "fh_fd: mismatch in omode (%d wanted, %d cached)\n",
			omode, h->omode);
		fh_close(h);
	}
	errno = 0;
	if (!h->path)
		return (-1);	/* something is really hosed */
	if ((h->fd = path_open(h->path, omode, 0)) >= 0) {
		io_state = active;
		h->omode = omode;
#if 0
		DBASSERT(fd_cache[h->fd] == NULL);
#endif
		fd_cache[h->fd] = h;
		++fd_cache_size;
		dprintf(1, "fh_fd: new open as fd=%d\n", h->fd);
	} else {
		dprintf(1, "fh_fd: open failed.\n");
		*status = nfs_errno();
	}
	return (h->fd);
}


void fd_inactive(fd)
int fd;
{
	io_state = inactive;
}


nfsstat fh_compose(dopa, new_fh, sbpp, fd, omode)
diropargs *dopa;
nfs_fh *new_fh;
struct stat **sbpp;
int fd;
int omode;
{
	svc_fh *key;
	fhcache *dirh, *h;
	char *sindx;
	int is_dd;
	nfsstat ret;
	char pathbuf[MAXPATHLEN + MAXNAMLEN + 1];

	if ((dirh = fh_find((svc_fh *) & (dopa->dir), 0)) == NULL)
		return (NFSERR_STALE);
	/* Construct path */
	if (strcmp(dopa->name, ".") == 0) {
		*new_fh = dopa->dir;
		*sbpp = NULL;
		return (NFS_OK);
	}
	if (strcmp(dopa->name, "..") == 0) {
		is_dd = 1;
		sindx = rindex(dirh->path, '/');
		if (sindx == dirh->path)
			strcpy(pathbuf, "/");
		else {
			int len = sindx - dirh->path;
			strncpy(pathbuf, dirh->path, len);
			pathbuf[len] = '\0';
		}
	} else {
		int len = strlen(dirh->path);

		is_dd = 0;
		if (dirh->path[len - 1] == '/')
			len--;
		strncpy(pathbuf, dirh->path, len);
		pathbuf[len] = '/';
		strcpy(pathbuf + (len + 1), dopa->name);
	}

	*new_fh = dopa->dir;
	key = (svc_fh *) new_fh;
	if ((key->psi = path_psi(pathbuf, &ret, *sbpp)) == 0)
		return (ret);

	if (is_dd)
		key->hash_path[key->hash_path[0]--] = 0;
	else {
		if (++(key->hash_path[0]) >= HP_LEN)
			return (NFSERR_NAMETOOLONG);
		key->hash_path[key->hash_path[0]] = hash_psi(dirh->h.psi);
	}
	h = fh_find(key, 1);

	/* New code added by Don Becker */
	if ((h->path != NULL) && (strcmp(h->path, pathbuf) != 0)) {
		/* We must have cached an old file under the same inode number. */
		fh_delete(h);
		h = fh_find(key, 1);
		if (h->path)
			dprintf(0, "Internal inconsistency: double entry (path '%s', now '%s').\n",
				h->path, pathbuf);
	}
	dprintf(1, "fh_compose: using  handle %x ('%s', fd=%d)\n",
		h, h->path ? h->path : "<unnamed>", h->fd);
	/* End of new code */

	/* assert(h != NULL); */
	if (h->path == 0) {
		if ((h->path = malloc(strlen(pathbuf) + 1)) == NULL)
			mallocfailed();
		strcpy(h->path, pathbuf);
		h->flags = 0;
		if (!is_dd && access(dirh->path, R_OK) != 0 &&
		    access(dirh->path, X_OK) == 0)
			h->flags |= FHC_XONLY_PATH;
		dprintf(1, "fh_compose: +using  handle %x ('%s', fd=%d)\n",
			h, h->path ? h->path : "<unnamed>", h->fd);
	}
	if (fd >= 0) {
		dprintf(1, "fh_compose: handle %x using passed fd %d\n", h, fd);
#if 0
		DBASSERT(fd_cache[fd] == NULL);
#endif
		if (h->fd >= 0)
			fh_close(h);
		h->fd = fd;
		fd_cache[fd] = h;
		++fd_cache_size;
		dprintf(1, "fh_compose: +using  handle %x ('%s', fd=%d)\n",
			h, h->path ? h->path : "<unnamed>", h->fd);
	}
	if (omode >= 0)
		h->omode = omode;
	return (NFS_OK);
}


int fh_psi(fh)
nfs_fh *fh;
{
	svc_fh *h = (svc_fh *) fh;
	return (h->psi);
}


void fh_remove(path)
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
