#ifndef _SYS_SEM_H
#define _SYS_SEM_H
#include <sys/ipc.h>

/* semop flags */
#define SEM_UNDO        010000  /* set up adjust to undo on exit */

/* semctl Command Definitions. */
#define GETPID  3       /* get sempid */
#define GETVAL  4       /* get semval */
#define GETALL  5       /* get all semval's */
#define GETNCNT 6       /* get semncnt */
#define GETZCNT 7       /* get semzcnt */
#define SETVAL  8       /* set semval */
#define SETALL  9       /* set all semval's */

/* One semid data structure for each set of semaphores in the system. */
struct semid_ds {
  struct ipc_perm sem_perm;       /* permissions .. see ipc.h */
  struct sem      *sem_base;      /* ptr to first semaphore in array */
  time_t          sem_otime;      /* last semop time */
  time_t          sem_ctime;      /* last change time */
  ushort          sem_nsems;      /* no. of semaphores in array */
};

/* One semaphore structure for each semaphore in the system. */
struct sem {
  short   sempid;         /* pid of last operation */
  ushort  semval;         /* current value */
  ushort  semncnt;        /* num procs awaiting increase in semval */
  ushort  semzcnt;        /* num procs awaiting semval = 0 */
};

/* semop system calls takes an array of these.*/
struct sembuf {
  ushort  sem_num;        /* semaphore index in array */
  short   sem_op;         /* semaphore operation */
  short   sem_flg;        /* operation flags */
};

/* arg for semctl system calls. */
union semun {
  int val;               /* value for SETVAL */
  struct semid_ds *buf;  /* buffer for IPC_STAT & IPC_SET : allocate it.*/
  ushort *array;         /* array for GETALL & SETALL : allocate it */
};

/*
setbit((char *) addr, uint number), clrbit(addr,number), bit(addr,number)
all return the value of the bit prior to alteration.
These are from the linux kernel code ... it seems
they are more useful than the sysv ipc semaphores!
*/

#define bitop(name,op) \
static inline int name(char * addr,unsigned int nr) \
{ \
int __res; \
__asm__ __volatile__("bt" op " %1,%2; adcl $0,%0" \
:"=g" (__res) \
:"r" (nr),"m" (*(addr)),"0" (0)); \
return __res; \
}

bitop(bit,"")
bitop(setbit,"s")
bitop(clrbit,"r")


#ifdef __cplusplus
extern "C" {
#endif

int semget (key_t key, int nsems, int semflg);
int semop (int semid, struct sembuf *sops, unsigned nsops);
int semctl (int semid, int semnum, int cmd, union semun arg);

#ifdef __cplusplus
}
#endif

#endif /* _SYS_SEM_H */



