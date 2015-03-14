#ifndef _SYS_IPC_H
#define _SYS_IPC_H
#include <sys/types.h>

typedef int key_t; /* should go in sys/types.h type for IPC key */

struct ipc_perm
{
  ushort uid;   /* owner euid and egid */
  ushort gid;
  ushort cuid;  /* creator euid and egid */
  ushort cgid;
  ushort mode;  /* access modes see mode flags below */
  ushort seq;   /* sequence number */
  key_t key;
};

#define IPC_PRIVATE (key_t) 0  

/* resource get request flags */
#define IPC_CREAT  00001000   /* create if key is nonexistent */
#define IPC_EXCL   00002000   /* fail if key exists */
#define IPC_NOWAIT 00004000   /* return error on wait */

/* lower byte of mode flags is User RWX, Group RWX, Other RWX. */
/* upper mode flags */
#define IPC_ALLOC  00100000   /* currently allocated */
#define IPC_LOCKED 00040000   /* resource for id is locked */

/* 
** Control commands used with semctl, msgctl and shmctl 
** see also specific commands in sem.h, msg.h and shm.h
** and super user commands in <linux/ipc.h> 
*/
#define IPC_SET  1 /* set options */
#define IPC_STAT 2  /* get options */
#define IPC_RMID 0 /* remove facility */

#endif /* _SYS_IPC_H */


