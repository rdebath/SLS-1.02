#ifndef _LINUX_IPC_H
#define _LINUX_IPC_H

/* info must be allocated as one of seminfo, msginfo or shminfo */
int ipc_init (int what, void *info);
int ipc_rm (int what);
/* defines for what in ipc_init and ipc_rm calls */
#define SEM_INIT 0
#define MSG_INIT 1
#define SHM_INIT 2
#define SEM_RM SEM_INIT
#define MSG_RM MSG_INIT
#define SHM_RM SHM_INIT

/* extra superuser ctl commands */
#define SEM_STAT 10
#define MSG_STAT 3
#define SHM_STAT 3

/* ipc implementation defines */
#define _CHECKID 1 /* used by lock ... anything not equal IPC_NOWAIT */
#define _MAX_MSG_TYPE 12000 /* should simply be MAX_LONG */
/* bits correspond to mode flags in other includes */
#define _BIPC_ALLOC 15  /* bit set when queue is allocated */
#define _BIPC_LOCKED 14 /* bit used for mutex lock on queue */
#define _BMSG_RWAIT 9
#define _BMSG_WWAIT 10
#define _BSHM_ALLOC 9
#define _BSHM_DEST 10

/* per process undo requests */
/* this gets linked into the user/task struct */
struct sem_undo {
    struct sem_undo *proc_next;
    struct sem_undo *id_next;
    struct sem_undo *id_prev;
    int semid;
    short   semadj; /* semval adjusted by exit */
    ushort   sem_num; /* semaphore index in array semid */
};      

/*
** Per process internal structure for managing segments.
** Copy during fork...destroy on exit ... update nattach.
** A shmat will add to and shmdt will remove from the list.
*/
struct	shm_desc {
    struct shm_desc *next;
    void *mapaddr;             /* virtual address of attach */
    int size;                  /* size in bytes */
    int shmid;
};

struct sysv_ipc {
  struct shm_desc *shm;
  struct sem_undo *sem;
};

/* 
Configuration Parameters. The values here reflect Implementation
limits ... do not change them without good reason. Instead change
ipcs.c to reflect system specific preferences.
*/
#define SEMMNI  128             /* max # of semaphore identifiers */
#define SEMMSL  256             /* max num of semaphores per id */
#define SEMMNS  (SEMMNI*SEMMSL) /* max # of semaphores in system */
#define SEMOPM  204             /* max num of operations per semop call */
#define SEMVMX  32767           /* semaphore maximum value */
   /* the rest are unused */
#define SEMUME  10              /* max num of undo entries per process */
#define SEMMNU  30              /* num of undo structures system wide */
#define SEMAEM  16384           /* adjust on exit max value */
#define SEMMAP  30              /* # of entries in semaphore map */
#define SEMUSZ  (sizeof(struct sem_undo)) /* size in bytes of undo structure */

#define MSGMNI  128    /* max number of message queue identifiers */
#define MSGMAX  4080   /* max size of message */
#define MSGMNB  8192   /* default max size of a message queue */
    /* the rest are unused */
#define MSGPOOL 16      /* size in kilobytes of message pool */
#define MSGTQL  50     /* number of system message headers */
#define MSGMAP  100    /* number of entries in message map */
#define MSGSSZ  8      /* message segment size */
#define MSGSEG ((MSGPOOL*1024)/MSGSSZ) /* max no. of segments */

#define SHMMAX (1 << 20)	  /* max shared memory segment size (bytes) */
#define SHMMIN (1 << 12)	  /* min shared memory segment size (bytes) */
#define SHMMNI 92                 /* max num of shared segments system wide */
#define SHMALL (1 << 10)          /* max shared mem system wide (in pages) */
#define	SHMLBA	(1 << 12)         /* segment low boundary address multiple */
    /* the rest are unused */
#define SHMSEG SHMMNI             /* max num of shared seg per process */


struct  seminfo {
    int semmap; 
    int semmni; 
    int semmns; 
    int semmnu; 
    int semmsl; 
    int semopm; 
    int semume; 
    int semusz; 
    int semvmx; 
    int semaem; 
};

struct msginfo {
    int msgpool;
    int msgmap; 
    int msgmax; 
    int msgmnb; 
    int msgmni; 
    int msgssz; 
    int msgtql; 
    ushort  msgseg; 
};

struct	shminfo {
    int shmmax;	
    int shmmin;	
    int shmmni;	
    int shmseg;	
    int shmall;	
};


/* 
These are used to wrap system calls and 
are not meant to be user visible.
*/
#ifdef __LIBRARY__
struct ipc_val {
    long third;
    int fourth;
};
/*  
space for 5 calls for ipc, sem, msg, shm
they must be in that order.
*/
#define IPC_INIT 0
#define IPC_RM 1
#define SEMOP 5
#define SEMGET 6
#define SEMCTL 7
#define MSGSND 10
#define MSGRCV 11
#define MSGGET 12
#define MSGCTL 13
#define SHMAT 15
#define SHMDT 16
#define SHMGET 17
#define SHMCTL 18

#endif /* __LIBRARY__ */

#endif /* _LINUX_IPC_H */


