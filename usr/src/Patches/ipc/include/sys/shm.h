#ifndef _SYS_SHM_H_
#define _SYS_SHM_H_
#include <sys/ipc.h>

struct shmid_ds {
	struct	ipc_perm shm_perm;	/* operation perms */
	int	shm_segsz;		/* size of segment (bytes) */
	time_t	shm_atime;		/* last attach time */
	time_t	shm_dtime;		/* last detach time */
	time_t	shm_ctime;		/* last change time */
	ulong	*shm_handle;		/* array of ptrs to frames */
	ushort	shm_cpid;		/* pid, creator */
	ushort	shm_lpid;		/* pid, last operation */
	short	shm_nattch;		/* no. of current attaches */
};

/* mode for attach */
#define	SHM_RDONLY	010000	/* read-only access */
#define	SHM_RND		020000	/* round attach address to SHMLBA boundary */

/* shm_mode upper byte flags */
#define	SHM_ALLOC	01000	/* segment has memory allocated */
#define	SHM_DEST	02000	/* segment will be destroyed on last detach */


#ifdef __cplusplus
extern "C" {
#endif

int shmctl (int shmid, int cmd, struct shmid_ds *buf);
int shmget(key_t key, int size, int flag);
char *shmat (int shmid, char *shmaddr, int shmflg);
int shmdt (char *shmaddr);

#ifdef __cplusplus
}
#endif

#endif /* _SYS_SHM_H_ */






