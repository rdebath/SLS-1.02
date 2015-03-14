/*
 * linux/ipc/sem.c
 * Copyright (C) 1992 Krishna Balasubramanian 
 */

#include <linux/errno.h>
#include <asm/segment.h>
#include <linux/string.h>
#include <linux/sched.h>
#include <sys/sem.h>
#include <linux/ipc.h>

extern int ipcperms (struct ipc_perm *ipcp, short semflg);
static int newary (key_t, int, int);
static int findkey (key_t key);
static void freeary (int id);

static struct semid_ds *semary[SEMMNI];
static int used_sems, used_semids;                    
static struct wait_queue *sem_lock;
static int sem_seq;
static int max_semid;

void sem_init (void)
{
	int i=0;
	
	sem_lock = NULL;
	used_sems = used_semids = max_semid = sem_seq = 0;
	for (i=0; i < SEMMNI; i++)
		semary[i] = IPC_UNUSED;
	return;
}

static int findkey (key_t key)
{
	int id;
	struct semid_ds *sma;
	
	for (id=0; id <= max_semid; id++) {
		while ((sma = semary[id]) == IPC_NOID) 
			interruptible_sleep_on (&sem_lock);
		if (sma == IPC_UNUSED)
			continue;
		if (key == sma->sem_perm.key)
			return id;
	}
	return -1;
}

static int newary (key_t key, int nsems, int semflg)
{
	int id;
	struct semid_ds *sma;
	struct ipc_perm *ipcp;
	int size;

	if (!nsems)
		return -EINVAL;
	if (used_sems + nsems > SEMMNS)
		return -ENOSPC;
	for (id=0; id < SEMMNI; id++) 
		if (semary[id] == IPC_UNUSED) {
			semary[id] = IPC_NOID;
			goto found;
		}
	return -ENOSPC;

found:
	size = sizeof (*sma) + nsems * sizeof (struct sem);
	used_sems += nsems;
	sma = (struct semid_ds *) kmalloc (size, GFP_KERNEL);
	if (!sma) {
		semary[id] = IPC_UNUSED;
		used_sems -= nsems;
		if (sem_lock)
			wake_up (&sem_lock);
		return -ENOMEM;
	}
	memset (sma, 0, size);
	sma->sem_base = (struct sem *) &sma[1];
	ipcp = &sma->sem_perm;
	ipcp->mode = (semflg & 0x01FF);
	ipcp->key = key;
	ipcp->cuid = ipcp->uid = current->euid;
	ipcp->gid = ipcp->cgid = current->egid;
	ipcp->seq = sem_seq;
	sma->eventn = sma->eventz = NULL;
	sma->sem_nsems = nsems;
	sma->sem_ctime = CURRENT_TIME;
        if (id > max_semid)
		max_semid = id;
	used_semids++;
	semary[id] = sma;
	if (sem_lock)
		wake_up (&sem_lock);
	return sem_seq * SEMMNI + id;
}

int sys_semget (key_t key, int nsems, int semflg)
{
	int id;
	struct semid_ds *sma;
	
	if (nsems < 0  || nsems > SEMMSL)
		return -EINVAL;
	if (key == IPC_PRIVATE) 
		return newary(key, nsems, semflg);
	if ((id = findkey (key)) == -1) {  /* key not used */
		if (!(semflg & IPC_CREAT))
			return -ENOENT;
		return newary(key, nsems, semflg);
	}
	if (semflg & IPC_CREAT && semflg & IPC_EXCL)
		return -EEXIST;
	sma = semary[id];
	if (nsems > sma->sem_nsems)
		return -EINVAL;
	if (ipcperms(&sma->sem_perm, semflg))
		return -EACCES;
	return sma->sem_perm.seq*SEMMNI + id;
} 

static void freeary (int id)
{
	struct semid_ds *sma = semary[id];

	sma->sem_perm.seq++;
	if ((int)((++sem_seq + 1) * SEMMNI) < 0)
		sem_seq = 0;
	used_sems -= sma->sem_nsems;
	if (id == max_semid)
		while (max_semid && (semary[--max_semid] == IPC_UNUSED));
	semary[id] = IPC_UNUSED;
	used_semids--;
	if (sma->eventz)
		wake_up (&sma->eventz);
	if (sma->eventn)
		wake_up (&sma->eventn);
	current->counter = 0; 
	schedule(); 
	kfree (sma);
	return;
}

int sys_semctl (int semid, int semnum, int cmd, void *arg)
{
	int id, val = 0;
	struct semid_ds *sma, *buf, tbuf;
	struct ipc_perm *ipcp;
	struct sem *curr;
	struct sem_undo *un;
	ushort i, nsems, *array = NULL;
	ushort sem_io[SEMMSL];
	
	if (semid < 0 || semnum < 0 || cmd < 0)
		return -EINVAL;

	switch (cmd) {
	case IPC_INFO: 
	case SEM_INFO: 
	{
		struct seminfo seminfo, *tmp;
		if (!arg || ! (tmp = (struct seminfo *) get_fs_long (arg)))
			return -EFAULT;
		seminfo.semmni = SEMMNI;
		seminfo.semmns = SEMMNS;
		seminfo.semmsl = SEMMSL;
		seminfo.semopm = SEMOPM;
		seminfo.semvmx = SEMVMX;
		seminfo.semmnu = SEMMNU; 
		seminfo.semmap = SEMMAP; 
		seminfo.semume = SEMUME;
		seminfo.semusz = SEMUSZ;
		seminfo.semaem = SEMAEM;
		if (cmd == SEM_INFO) {
			seminfo.semusz = used_semids;
			seminfo.semaem = used_sems;
		}
		verify_area (VERIFY_WRITE, tmp, sizeof (struct seminfo));
		memcpy_tofs (tmp, &seminfo, sizeof(struct seminfo));
		return max_semid;
	}


	case SEM_STAT:
		if (!arg || ! (buf = (struct semid_ds *) get_fs_long (arg)))
			return -EFAULT;
		verify_area (VERIFY_WRITE, buf, sizeof (*sma));
		if (semid > max_semid)
			return -EINVAL;
		sma = semary[semid];
		if (sma == IPC_UNUSED || sma == IPC_NOID)
			return -EINVAL;
		if (ipcperms (&sma->sem_perm, 0444))
			return -EACCES;
		id = semid + sma->sem_perm.seq * SEMMNI; 
		memcpy_tofs (buf, sma, sizeof(*sma));
		return id;
	}

	id = semid % SEMMNI;
	sma = semary [id];
	if (sma == IPC_UNUSED || sma == IPC_NOID)
		return -EINVAL;
	ipcp = &sma->sem_perm;
	nsems = sma->sem_nsems;
	if (ipcp->seq != semid / SEMMNI)
		return -EIDRM;
	if (semnum >= nsems)
		return -EINVAL;
	curr = &sma->sem_base[semnum];

	switch (cmd) {
	case GETVAL:
	case GETPID:
	case GETNCNT:
	case GETZCNT:
	case GETALL:
		if (ipcperms (ipcp, 0444))
			return -EACCES;
		switch (cmd) {
		case GETVAL : return curr->semval; 
		case GETPID : return curr->sempid;
		case GETNCNT: return curr->semncnt;
		case GETZCNT: return curr->semzcnt;
		case GETALL:
			if (!arg || ! (array = (ushort *) get_fs_long (arg)))
				return -EFAULT;
			verify_area (VERIFY_WRITE, array, nsems * sizeof (short));
		}
		break;
	case SETVAL: 
		if (!arg)
			return -EFAULT;
		if ((val = (int) get_fs_long (arg))  > SEMVMX 
		    || val < 0) 
			return -ERANGE;
		break;
	case IPC_RMID:
		if (suser() || current->euid == ipcp->cuid || 
		    current->euid == ipcp->uid) {
			freeary (id); 
			return 0;
		}
		return -EPERM;
	case SETALL: /* arg is a pointer to an array of ushort */
		if (!arg || ! (array = (ushort *) get_fs_long (arg)) )
			return -EFAULT;
		memcpy_fromfs (sem_io, array, nsems*sizeof(ushort));
		for (i=0; i< nsems; i++)
			if (sem_io[i] > SEMVMX)
				return -ERANGE;
		break;
	case IPC_STAT:
		if (!arg || !(buf = (struct semid_ds *) get_fs_long (arg))) 
			return -EFAULT;
		verify_area (VERIFY_WRITE, arg, sizeof (*sma));
		break;
	case IPC_SET:
		if (!arg || !(buf = (struct semid_ds *) get_fs_long (arg))) 
			return -EFAULT;
		memcpy_fromfs (&tbuf, buf, sizeof tbuf);
		break;
	}
	

	if (semary[id] == IPC_UNUSED || semary[id] == IPC_NOID)
		return -EIDRM;
	if (ipcp->seq != semid / SEMMNI)
		return -EIDRM;

	switch (cmd) {
	case GETALL:
		if (ipcperms (ipcp, 0444))
			return -EACCES;
		for (i=0; i< sma->sem_nsems; i++)
			sem_io[i] = sma->sem_base[i].semval;
		memcpy_tofs (sem_io, array, nsems*sizeof(ushort));
		break;
	case SETVAL:
		if (ipcperms (ipcp, 0222))
			return -EACCES;
		for (un = sma->undo; un; un = un->id_next)
			if (semnum == un->sem_num)
				un->semadj = 0;
		sma->sem_ctime = CURRENT_TIME;
		curr->semval = val;
		if (sma->eventn)
			wake_up (&sma->eventn);
		if (sma->eventz)
			wake_up (&sma->eventz);
		break;
	case IPC_SET:
		if (suser() || current->euid == ipcp->cuid || 
		    current->euid == ipcp->uid) {
			ipcp->uid = tbuf.sem_perm.uid;
			ipcp->gid = tbuf.sem_perm.gid;
			ipcp->mode = (ipcp->mode & ~0777)
				| (tbuf.sem_perm.mode & 0777);
			sma->sem_ctime = CURRENT_TIME;
			return 0;
		}
		return -EPERM;
	case IPC_STAT:
		if (ipcperms (ipcp, 0444))
			return -EACCES;
		memcpy_tofs (buf, sma, sizeof (*sma));
		break;
	case SETALL:
		if (ipcperms (ipcp, 0222))
			return -EACCES;
		for (un = sma->undo; un != NULL; un = un->id_next)
			un->semadj = 0;
		if (sma->eventn)
			wake_up (&sma->eventn);
		if (sma->eventz)
			wake_up (&sma->eventz);
		sma->sem_ctime = CURRENT_TIME;
		break;
	default:
		return -EINVAL;
	}
	return 0;
}

int sys_semop (int semid, struct sembuf *tsops, unsigned nsops)
{
	int i, id;
	struct semid_ds *sma;
	struct sem *curr = NULL;
	struct sembuf sops[SEMOPM], *sop;
	struct sem_undo *un;
	int undos = 0, alter = 0, semncnt = 0, semzcnt = 0;
	
	if (nsops < 1 || semid < 0)
		return -EINVAL;
	if (nsops > SEMOPM)
		return -E2BIG;
	if (!tsops) 
		return -EFAULT;
	memcpy_fromfs (sops, tsops, nsops * sizeof(*tsops));  
	id = semid % SEMMNI;
	if ((sma = semary[id]) == IPC_UNUSED || sma == IPC_NOID)
		return -EINVAL;
	for (i=0; i<nsops; i++) { 
		sop = &sops[i];
		if (sop->sem_num > sma->sem_nsems)
			return -EFBIG;
		if (sop->sem_flg & SEM_UNDO)
			undos++;
		if (sop->sem_op) {
			alter++;
			if (sop->sem_op > 0)
				semncnt ++;
		}
	}
	if (ipcperms(&sma->sem_perm, alter ? 0222 : 0444))
		return -EACCES;
	/* 
	 * ensure every sop with undo gets an undo structure 
	 */
	if (undos) {
		for (i=0; i<nsops; i++) {
			if (!(sops[i].sem_flg & SEM_UNDO))
				continue;
			for (un = current->semun; un; un = un->proc_next) 
				if ((un->semid == semid) && 
				    (un->sem_num == sops[i].sem_num))
					break;
			if (un)
				continue;
			un = (struct sem_undo *) 
				kmalloc (sizeof(*un), GFP_ATOMIC);
			if (!un)
				return -ENOMEM;
			un->semid = semid;
			un->semadj = 0;
			un->sem_num = sops[i].sem_num;
			un->proc_next = current->semun;
			current->semun = un;
			un->id_next = sma->undo;
			if (sma->undo)
				sma->undo->id_prev = un;
			sma->undo = un->id_prev = un;
		}
	}

 slept:
	if (sma->sem_perm.seq != semid / SEMMNI) 
		return -EIDRM;
	for (i=0; i<nsops; i++) {
		sop = &sops[i];
		curr = &sma->sem_base[sop->sem_num];
		if (sop->sem_op + curr->semval > SEMVMX)
			return -ERANGE;
		if (!sop->sem_op && curr->semval) { 
			if (sop->sem_flg & IPC_NOWAIT)
				return -EAGAIN;
			if (current->signal & ~current->blocked) 
				return -EINTR;
			curr->semzcnt++;
			interruptible_sleep_on (&sma->eventz);
			curr->semzcnt--;
			goto slept;
		}
		if ((sop->sem_op + curr->semval < 0) ) { 
			if (sop->sem_flg & IPC_NOWAIT)
				return -EAGAIN;
			if (current->signal & ~current->blocked)
				return -EINTR;
			curr->semncnt++;
			interruptible_sleep_on (&sma->eventn);
			curr->semncnt--;
			goto slept;
		}
	}

	for (i=0; i<nsops; i++) {
		sop = &sops[i];
		curr = &sma->sem_base[sop->sem_num];
		curr->sempid = current->pid;
		if (!(curr->semval += sop->sem_op))
			semzcnt++;
		if (!(sop->sem_flg & SEM_UNDO))
			continue;
		for (un = current->semun; un; un = un->proc_next) 
			if ((un->semid == semid) && 
			    (un->sem_num == sop->sem_num))
					break;
		if (!un) {
			printk ("semop : no undo for op %d\n", i);
			continue;
		}
		un->semadj -= sop->sem_op;
	}
	sma->sem_otime = CURRENT_TIME; 
       	if (semncnt && sma->eventn)
		wake_up(&sma->eventn);
	if (semzcnt && sma->eventz)
		wake_up(&sma->eventz);
	return curr->semval;
}

/*
 * add semadj values to semaphores if alowed. Silently ignore errors!
 */
void sem_exit (void)
{
	struct sem_undo *un, *tmp;
	struct semid_ds *sma;
	struct sem *sem = NULL;
	
	for (un = current->semun; un; kfree(un), un = tmp) {
		un->id_prev->id_next = un->id_next;
		if (un->id_next)
			un->id_next->id_prev = un->id_prev;
		tmp = un->proc_next;
		if (!un->semadj)
			continue;
		sma = semary[un->semid % SEMMNI];
		if (sma == IPC_UNUSED || sma == IPC_NOID 
		    || sma->sem_perm.seq != un->semid / SEMMNI)
			continue;
		sem = &sma->sem_base[un->sem_num];
		if (sem->semval + un->semadj >= 0) {
			sem->semval += un->semadj;
			sem->sempid = current->pid;
			sma->sem_otime = CURRENT_TIME;
			if (un->semadj > 0 && sma->eventn)
				wake_up (&sma->eventn);
			if (!sem->semval && sma->eventz)
				wake_up (&sma->eventz);
		}
	}
	current->semun = NULL;
	return;
}

