/*
 *  linux/lib/ipc.c
 *
 *  (C) 1992  Krishna Balasubramanian
 */

#define KERNEL
#include <linux/unistd.h>
#include <sys/shm.h>
#include <sys/msg.h>
#include <sys/sem.h>
#include <sys/stat.h>
#define NONE 0

/* wrapper for ipc syscalls */
_syscall5(int,ipc,int,call,int,first,int,second,int,third,void *,ptr)

/* SEM SYSCALLS */
int semget (key_t key, int nsems, int semflg)
{
    return ipc (SEMGET, key, nsems, semflg, NULL);
}

int semop (int semid, struct sembuf *sops, unsigned nsops)
{
    return ipc (SEMOP, semid, (int) nsops, NONE, sops);
}

int semctl (int semid, int semnum, int cmd, union semun arg)
{
    return ipc (SEMCTL, semid, semnum, cmd, &arg); 
}


/* MSG SYSCALLS */
int msgctl (int msqid, int cmd, struct msqid_ds *buf)
{
    return ipc (MSGCTL, msqid, cmd, NONE, buf);
}

int msgget (key_t key, int msgflg)
{
    return ipc (MSGGET, key, msgflg, NONE, NULL);
}

int msgsnd (int msqid, struct msgbuf *msgp, int msgsz, int msgflg)
{
    return ipc (MSGSND, msqid, msgsz, msgflg,  msgp);
}

int msgrcv (int msqid, struct msgbuf *msgp, int msgsz, long msgtyp, int msgflg)
{
    struct ipc_kludge tmp; 
    tmp.msgp = msgp;
    tmp.msgtyp = msgtyp; 
    return ipc (MSGRCV, msqid, msgsz, msgflg, &tmp);
}

/* SHM SYSCALLS */
int shmget (key_t key, int size, int shmflg)
{
    return ipc (SHMGET, key, size, shmflg, NULL);
}

int shmctl (int shmid, int cmd, struct shmid_ds *buf)
{
    return ipc (SHMCTL, shmid, cmd, NONE, buf);
}

int shmdt (char *shmaddr)
{
    return ipc (SHMDT, NONE, NONE, NONE, shmaddr);
}

char *shmat (int shmid, char *shmaddr, int shmflg)
{
    int rval; ulong raddr; 
    rval = ipc (SHMAT, shmid, shmflg, (int) &raddr, shmaddr);
    return rval < 0 ? (char *) rval : (char *) raddr;
}

key_t ftok (char *path, char id)
{
    struct stat buf;
    key_t key;

    if (stat (path, &buf)) {
	return (key_t) -1;
    }
    key = (buf.st_ino & 0xFFFF) | ((buf.st_uid & 0xFF) << 16) | (id << 24);
    return key;
}
    

