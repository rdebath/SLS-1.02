/* UNFSD - copyright Mark A Shand, May 1988.
 * This software maybe be used for any purpose provided
 * the above copyright notice is retained.  It is supplied
 * as is, with no warranty expressed or implied.
 */

/* compatibility between mount and nfs_prot */
#ifndef NFS_FHSIZE
#define NFS_FHSIZE	FHSIZE
#endif

#define	HP_LEN	(NFS_FHSIZE - sizeof(u_long))

typedef struct
{
	u_long	psi;
	u_char	hash_path[HP_LEN];
	/* Hashed search path to this file.
	 * path is: hash_path[1] ... hash_path[hash_path[0]]
	 *
	 * hash_path[hash_path[0]+1] ... hash_path[HP_LEN-1] == 0
	 */
}
	svc_fh;

extern int	pseudo_inode(u_long inode, int dev);
extern void	fh_init(void);
extern char	*fh_pr(nfs_fh *fh);
extern int	fh_create(nfs_fh *fh, char *path);
extern char	*fh_path(nfs_fh *fh, nfsstat *status);
extern int	fh_fd(nfs_fh *fh, nfsstat *status, int omode);
extern void	fd_inactive(int fd);
extern nfsstat	fh_compose(diropargs *dopa, nfs_fh *new_fh,
			   struct stat **sbpp);
extern int	fh_psi(nfs_fh *fh);
extern void	fh_remove(char *path);
extern nfsstat	nfs_errno();
extern int	_rpcpmstart;
extern int	fh_initialized;
