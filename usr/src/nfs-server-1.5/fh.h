/*
 * fh		This module handles the file-handle cache.
 *
 * Version:	@(#)fh.h	1.5	93/04/10
 *
 * Authors:	Mark A. Shand, May 1988
 *		Don Becker, <becker@super.org>
 *		Rick Sladkey, <jrs@world.std.com>
 *		Fred N. van Kempen, <waltje@uWalt.NL.Mugnet.ORG>
 *		Copyright 1988 Mark A. Shand
 *		This software maybe be used for any purpose provided
 *		the above copyright notice is retained.  It is supplied
 *		as is, with no warranty expressed or implied.
 */

/* Compatibility between mount and nfs_prot. */
#ifndef NFS_FHSIZE
#   define NFS_FHSIZE		FHSIZE
#endif

#define	HP_LEN			(NFS_FHSIZE - sizeof(u_long))

#define	FHC_XONLY_PATH		01
#define	FHC_BUSY		02	/* NOT USED */

#define	CACHE_SIZE_LIMIT	500
#define	LOWAT_CACHE_SIZE	3*CACHE_SIZE_LIMIT
#define	HIWAT_CACHE_SIZE	4*CACHE_SIZE_LIMIT
#define HASH_TAB_SIZE		(5*CACHE_SIZE_LIMIT | 03)

#define FD_CACHE_LIMIT		8

/* The following affect execute-only directories */
#define FLUSH_INTERVAL		(60*60*12)		/* Twice a day	*/
#define BUSY_RETRY_INTERVAL	(60*10)			/* Ten minutes	*/
#define DISCARD_INTERVAL	(FLUSH_INTERVAL*2)	/* Two days	*/


/*
 * Hashed search path to this file.
 * path is: hash_path[1] ... hash_path[hash_path[0]]
 *
 * hash_path[hash_path[0]+1] ... hash_path[HP_LEN-1] == 0
 */
typedef struct {
  u_long	psi;
  u_char	hash_path[HP_LEN];
} svc_fh;

typedef enum { inactive, active } mutex;

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
typedef struct fhcache {
  struct fhcache	*next;
  struct fhcache	*prev;
  struct fhcache	*hash_next;
  svc_fh		h;
  int			fd;
  int			omode;
  char			*path;
  time_t		last_used;
  int			flags;
} fhcache;


/* Global FH variables. */
extern int _rpcpmstart;
extern int fh_initialized;


/* Global function prototypes. */
extern _PRO( enum nfsstat nfs_errno, (void)				);
extern _PRO( int pseudo_inode, (u_long inode, u_short dev)		);
extern _PRO( void fh_init, (void)					);
extern _PRO( char *fh_pr, (nfs_fh *fh)					);
extern _PRO( int fh_create, (nfs_fh *fh, char *path)			);
extern _PRO( char *fh_path, (nfs_fh *fh, nfsstat *status)		);
extern _PRO( int path_open, (char *path, int omode, int perm)		);
extern _PRO( int fh_fd, (nfs_fh *fh, nfsstat *status, int omode)	);
extern _PRO( void fd_inactive, (int fd)					);
extern _PRO( nfsstat fh_compose, (diropargs *dopa, nfs_fh *new_fh,	\
				  struct stat **sbpp, int fd, int omode));
extern _PRO( int fh_psi, (nfs_fh *fh)					);
extern _PRO( void fh_remove, (char *path)				);

/* End of FH.H */
