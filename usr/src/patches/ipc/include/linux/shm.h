#ifndef _LINUX_SHM_H_
#define _LINUX_SHM_H_
#include <linux/ipc.h>

struct shmid_ds {
	struct	ipc_perm shm_perm;	/* operation perms */
	int	shm_segsz;		/* size of segment (bytes) */
	time_t	shm_atime;		/* last attach time */
	time_t	shm_dtime;		/* last detach time */
	time_t	shm_ctime;		/* last change time */
	unsigned short	shm_cpid;	/* pid of creator */
	unsigned short	shm_lpid;	/* pid of last operator */
	short	shm_nattch;		/* no. of current attaches */
	/* the following are private */
	unsigned short   shm_npages;  /* size of segment (pages) */
	unsigned long   *shm_pages;   /* array of ptrs to frames -> SHMMAX */ 
	struct shm_desc *attaches;    /* descriptors for attaches */
};

/* mode for attach */
#define	SHM_RDONLY	010000	/* read-only access */
#define	SHM_RND		020000	/* round attach address to SHMLBA boundary */
#define	SHM_REMAP	040000	/* take-over region on attach */

/* super user shmctl commands */
#define SHM_LOCK 	11
#define SHM_UNLOCK 	12

struct	shminfo {
    int shmmax;	
    int shmmin;	
    int shmmni;	
    int shmseg;	
    int shmall;	
};

#define SHMMAX 0x400000	 /* <= 4M */          /* max shared seg size (bytes) */
#define SHMMIN 1	 /* really PAGE_SIZE */  /* min shared seg size (bytes)*/
#define SHMMNI 128       /* <= 4096 */        /* max num of segs system wide */
#define SHMALL 0x10000 /* <= SHMMAX*SHMMNI/PAGE_SIZE */  /* max shm system wide (pages) */
#define	SHMLBA 0x1000    /* = PAGE_SIZE */   /*  attach addr multiple */
#define SHMSEG SHMMNI    /* <= SHMMNI */    /* max shared segs per process */


#ifdef KERNEL

/* shm_mode upper byte flags */
#define	SHM_DEST	01000	/* segment will be destroyed on last detach */
#define SHM_LOCKED      02000   /* segment will not be swapped */

/* ipcs ctl commands */
#define SHM_STAT 	13
#define SHM_INFO 	14
struct shm_info {
	int   used_ids;
	ulong shm_tot; /* total allocated shm */
	ulong shm_rss; /* total resident shm */
	ulong shm_swp; /* total swapped shm */
	ulong swap_attempts;
	ulong swap_successes;
};


/*
 * Per process internal structure for managing segments.
 * A shmat will add to and shmdt will remove from the list.
 */
struct	shm_desc {
	struct task_struct *task;     /* attacher */
	unsigned long shm_sgn;        /* signature for this attach */
	unsigned long start;   /* virt addr of attach, multiple of SHMLBA */
	unsigned long end;            /* multiple of SHMLBA */
	struct shm_desc *task_next;   /* next attach for task */
	struct shm_desc *seg_next;    /* next attach for segment */
};

/* not present page table entry format bit 0 is 0, high byte defined in mm.h */
#define SHM_IDX_SHIFT 20
#define SHM_IDX_MASK  0x3FF
#define SHM_ID_SHIFT  8
#define SHM_ID_MASK   0xFFF
#define SHM_READ_ONLY 0x80000000

#endif /* KERNEL */

#endif /* _LINUX_SHM_H_ */


