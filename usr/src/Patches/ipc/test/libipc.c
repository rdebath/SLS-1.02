/*
 *  linux/lib/ipc.c
 *
 *  (C) 1992  Krishna Balasubramanian
 */

#define __LIBRARY__
#include <linux/unistd.h>
#include <sys/shm.h>
#include <sys/msg.h>
#include <sys/sem.h>
#include <linux/ipc.h>
#include <sys/stat.h>
#define NONE 0

/* wrapper for ipc syscalls */
_syscall5(int,ipc,int,call,int,first,int,second,void *,ptr,void *,sptr)

int ipc_init (int what, void *info)
{
    return ipc (IPC_INIT, what, NONE, (void *) info, NULL);
}
int ipc_rm (int what)
{
    return ipc (IPC_RM, what, NONE, NULL, NULL);
}

/* SEM SYSCALLS */
int semget (key_t key, int nsems, int semflg)
{
    int third = semflg, rval;
    rval = ipc (SEMGET, (int) key, nsems, NULL, (void *) &third);
    return rval;
}

int semop (int semid, struct sembuf *sops, unsigned nsops)
{
    return ipc (SEMOP, semid, (int) nsops, (void *) sops, NULL);
}

int semctl (int semid, int semnum, int cmd, union semun arg)
{
    int third = cmd, rval;
    rval = ipc (SEMCTL, semid, semnum, (void *) &arg, (void *) &third); 
    return rval;
}


/* MSG SYSCALLS */
int msgctl (int msqid, int cmd, struct msqid_ds *buf)
{
    return ipc (MSGCTL, msqid, cmd, (void *) buf, NULL);
}

int msgget (key_t key, int msgflg)
{
    return ipc (MSGGET, (int) key, (int) msgflg, NULL, NULL);
}

int msgsnd (int msqid, struct msgbuf *msgp, int msgsz, int msgflg)
{
    int third = msgflg, rval;
    rval = ipc (MSGSND, msqid, msgsz, (void *) msgp, (void *) &third);
    return rval;
}

int msgrcv (int msqid, struct msgbuf *msgp, int msgsz, long msgtyp, int msgflg)
{
    int rval;
    struct ipc_val tmp; 
    tmp.third = msgtyp;
    tmp.fourth = msgflg; 
    rval = ipc (MSGRCV, msqid, msgsz, (void *) msgp, (void *) &tmp);
    return rval;
}

/* SHM SYSCALLS */
int shmget (key_t key, int size, int flag)
{
    int third = flag, rval; 
    rval = ipc (SHMGET, (int) key, size, NULL, (void *) &third);
    return rval;
}

int shmctl (int shmid, int cmd, struct shmid_ds *buf)
{
    return ipc (SHMCTL, shmid, cmd, (void *) buf, NULL);
}

int shmdt (char *shmaddr)
{
    return ipc (SHMDT, NONE, NONE, (void *) shmaddr, NULL);
}

char *shmat (int shmid, char *shmaddr, int shmflg)
{
    return (char *) ipc (SHMAT, shmid, shmflg, (void *) shmaddr, NULL);
}

key_t ftok (char *path, char id)
{
    struct stat buf;
    key_t key;

    if (stat (path, &buf)) {
	return (key_t) -1;
    }
    key = (buf.st_uid & 0xFF) | (id << 8);
    key << 16;
    key |= (buf.st_ino & 0xFFFF);
    return key;
}
    

