/*
 * linux/ipc/util.c
 * Copyright (C) 1992 Krishna Balasubramanian
 */

#include <linux/errno.h>
#include <asm/segment.h>
#include <linux/sched.h>
#include <linux/sem.h>
#include <linux/msg.h>
#include <linux/shm.h>

void ipc_init (void);
int sys_ipc (uint call, int first, int second, int third, void *ptr); 
int ipcperms (struct ipc_perm *ipcp, short flag);

extern void sem_init (void), msg_init (void), shm_init (void);
extern int sys_semget (key_t key, int nsems, int semflg);
extern int sys_semop (int semid, struct sembuf *sops, unsigned nsops);
extern int sys_semctl (int semid, int semnum, int cmd, void *arg);
extern int sys_msgget (key_t key, int msgflg);
extern int sys_msgsnd (int msqid, struct msgbuf *msgp, int msgsz, int msgflg);
extern int sys_msgrcv (int msqid, struct msgbuf *msgp, int msgsz, long msgtyp,
		       int msgflg);
extern int sys_msgctl (int msqid, int cmd, struct msqid_ds *buf);
extern int sys_shmctl (int shmid, int cmd, struct shmid_ds *buf);
extern int sys_shmget (key_t key, int size, int flag);
extern int sys_shmat (int shmid, char *shmaddr, int shmflg, ulong *addr);
extern int sys_shmdt (char *shmaddr);

void ipc_init (void)
{
	sem_init();
	msg_init();
	shm_init();
	return;
}

/* 
 * Check user, group, other permissions for access
 * to ipc resources. return 0 if allowed
 */
int ipcperms (struct ipc_perm *ipcp, short flag)
{
	int i, perm = 0007, euid = current->euid, egid;
	
	if (suser())
		return 0;
	if (euid == ipcp->cuid || euid == ipcp->uid) 
		perm = 0700;
	else {
		for (i = 0; (egid = current->groups[i]) != NOGROUP; i++)
			if ((egid == ipcp->cgid) || (egid == ipcp->gid)) { 
				perm = 0070; 
				break;
			}
	}
	if (!(flag & perm) || flag & perm & ~ipcp->mode)
		return -1;
	return 0;
}


int sys_ipc (uint call, int first, int second, int third, void *ptr) 
{
	
	if (call <= SEMCTL)
		switch (call) {
		case SEMOP:
			return sys_semop (first, (struct sembuf *)ptr, second);
		case SEMGET:
			return sys_semget (first, second, third);
		case SEMCTL:
			return sys_semctl (first, second, third, ptr);
		default:
			return -EINVAL;
		}
	if (call <= MSGCTL) 
		switch (call) {
		case MSGSND:
			return sys_msgsnd (first, (struct msgbuf *) ptr, 
					   second, third);
		case MSGRCV: {
			struct ipc_kludge tmp; 
			if (!ptr)
				return -EINVAL;
			memcpy_fromfs (&tmp,(struct ipc_kludge *) ptr, 
				       sizeof (tmp));
			return sys_msgrcv (first, tmp.msgp, second, tmp.msgtyp,
					 	third);
			}
		case MSGGET:
			return sys_msgget ((key_t) first, second);
		case MSGCTL:
			return sys_msgctl (first, second, 
						(struct msqid_ds *) ptr);
		default:
			return -EINVAL;
		}
	if (call <= SHMCTL) 
		switch (call) {
		case SHMAT: /* returning shmaddr > 2G will screw up */
			return sys_shmat (first, (char *) ptr, second, 
							(ulong *) third);
		case SHMDT: 
			return sys_shmdt ((char *)ptr);
		case SHMGET:
			return sys_shmget (first, second, third);
		case SHMCTL:
			return sys_shmctl (first, second, 
						(struct shmid_ds *) ptr);
		default:
			return -EINVAL;
		}
	return -EINVAL;
}

