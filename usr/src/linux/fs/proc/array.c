/*
 *  linux/fs/proc/array.c
 *
 *  Copyright (C) 1992  by Linus Torvalds
 *  based on ideas by Darren Senn
 *
 *  stat,statm extensions by Michael K. Johnson, johnsonm@stolaf.edu
 */

#include <linux/types.h>
#include <linux/errno.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/tty.h>

#include <asm/segment.h>
#include <asm/io.h>

#define LOAD_INT(x) ((x) >> FSHIFT)
#define LOAD_FRAC(x) LOAD_INT(((x) & (FIXED_1-1)) * 100)

#define	KSTK_EIP(stack)	(((unsigned long *)stack)[1019])
#define	KSTK_ESP(stack)	(((unsigned long *)stack)[1022])

#define	_SSIZE(stack)	(TASK_SIZE - KSTK_ESP(stack))
#define	SSIZE(stack)	(KSTK_ESP(stack) ? _SSIZE(stack) : 0)

#define	VSIZE(task,stack) ((task)->brk + 1023 + SSIZE(stack))


static int get_loadavg(char * buffer)
{
	int a, b, c;

	a = avenrun[0] + (FIXED_1/200);
	b = avenrun[1] + (FIXED_1/200);
	c = avenrun[2] + (FIXED_1/200);
	return sprintf(buffer,"%d.%02d %d.%02d %d.%02d\n",
		LOAD_INT(a), LOAD_FRAC(a),
		LOAD_INT(b), LOAD_FRAC(b),
		LOAD_INT(c), LOAD_FRAC(c));
}

static int get_uptime(char * buffer)
{
	unsigned long uptime;
	unsigned long idle;

	uptime = jiffies + jiffies_offset;
	idle = task[0]->utime + task[0]->stime;
	return sprintf(buffer,"%d.%02d %d.%02d\n",
		uptime / HZ,
		uptime % HZ,
		idle / HZ,
		idle % HZ);
}

static int get_meminfo(char * buffer)
{
	struct sysinfo i;

	si_meminfo(&i);
	si_swapinfo(&i);
	return sprintf(buffer, "        total:   used:    free:   shared:  buffers:\n"
		"Mem:  %8d %8d %8d %8d %8d\n"
		"Swap: %8d %8d %8d\n",
		i.totalram, i.totalram-i.freeram, i.freeram, i.sharedram, i.bufferram,
		i.totalswap, i.totalswap-i.freeswap, i.freeswap);
}

static int get_version(char * buffer)
{
	extern char *linux_banner;

	strcpy(buffer, linux_banner);
	return strlen(buffer);
}

static struct task_struct ** get_task(pid_t pid)
{
	struct task_struct ** p;

	p = task;
	while (++p < task+NR_TASKS) {
		if (*p && (*p)->pid == pid)
			return p;
	}
	return NULL;
}

static unsigned long get_phys_addr(struct task_struct ** p, unsigned long ptr)
{
	unsigned long page;

	if (!p || !*p || ptr >= TASK_SIZE)
		return 0;
	page = (*p)->tss.cr3;
	page += (ptr >> 20) & 0xffc;
	page = *(unsigned long *) page;
	if (!(page & 1))
		return 0;
	page &= 0xfffff000;
	page += (ptr >> 10) & 0xffc;
	page = *(unsigned long *) page;
	if (!(page & 1))
		return 0;
	page &= 0xfffff000;
	page += ptr & 0xfff;
	return page;
}

static int get_array(struct task_struct ** p, unsigned long start, unsigned long end, char * buffer)
{
	unsigned long addr;
	int size = 0, result = 0;
	char c;

	if (start >= end)
		return result;
	for (;;) {
		addr = get_phys_addr(p, start);
		if (!addr)
			return result;
		do {
			c = *(char *) addr;
			if (!c)
				result = size;
			if (size < PAGE_SIZE)
				buffer[size++] = c;
			else
				return result;
			addr++;
			start++;
			if (start >= end)
				return result;
		} while (!(addr & 0xfff));
	}
}

static int get_env(int pid, char * buffer)
{
	struct task_struct ** p = get_task(pid);

	if (!p || !*p)
		return 0;
	return get_array(p, (*p)->env_start, (*p)->env_end, buffer);
}

static int get_arg(int pid, char * buffer)
{
	struct task_struct ** p = get_task(pid);

	if (!p || !*p)
		return 0;
	return get_array(p, (*p)->arg_start, (*p)->arg_end, buffer);
}

static unsigned long get_wchan(struct task_struct *p)
{
	unsigned long ebp, eip;
	unsigned long stack_page;
	int count = 0;

	if (!p || p == current || p->state == TASK_RUNNING)
		return 0;
	ebp = p->tss.ebp;
	stack_page = p->kernel_stack_page;
	do {
		if (ebp < stack_page || ebp >= 4092+stack_page)
			return 0;
		eip = *(unsigned long *) (ebp+4);
		if ((void *)eip != sleep_on &&
		    (void *)eip != interruptible_sleep_on)
			return eip;
		ebp = *(unsigned long *) ebp;
	} while (count++ < 16);
	return 0;
}

static int get_stat(int pid, char * buffer)
{
	struct task_struct ** p = get_task(pid);
	unsigned long sigignore=0, sigcatch=0, bit=1, wchan;
	int i,tty_pgrp;
	char state;

	if (!p || !*p)
		return 0;
	if ((*p)->state < 0 || (*p)->state > 5)
		state = '.';
	else
		state = "RSDZTD"[(*p)->state];
	wchan = get_wchan(*p);
	for(i=0; i<32; ++i) {
		switch((int) (*p)->sigaction[i].sa_handler) {
		case 1: sigignore |= bit; break;
		case 0: break;
		default: sigcatch |= bit;
		} bit <<= 1;
	}
	tty_pgrp = (*p)->tty;
	if (tty_pgrp > 0 && tty_table[tty_pgrp])
		tty_pgrp = tty_table[tty_pgrp]->pgrp;
	else
		tty_pgrp = -1;
	return sprintf(buffer,"%d (%s) %c %d %d %d %d %d %u %u \
%u %u %u %d %d %d %d %d %d %u %u %d %u %u %u %u %u %u %u %u %d \
%d %d %d %u\n",
		pid,
		(*p)->comm,
		state,
		(*p)->p_pptr->pid,
		(*p)->pgrp,
		(*p)->session,
		(*p)->tty,
		tty_pgrp,
		(*p)->flags,
		(*p)->min_flt,
		(*p)->cmin_flt,
		(*p)->maj_flt,
		(*p)->cmaj_flt,
		(*p)->utime,
		(*p)->stime,
		(*p)->cutime,
		(*p)->cstime,
		(*p)->counter,  /* this is the kernel priority ---
				   subtract 30 in your user-level program. */
		(*p)->priority, /* this is the nice value ---
				   subtract 15 in your user-level program. */
		(*p)->timeout,
		(*p)->it_real_value,
		(*p)->start_time,
		VSIZE((*p),(*p)->kernel_stack_page),
		(*p)->rss, /* you might want to shift this left 3 */
		(*p)->rlim[RLIMIT_RSS].rlim_cur,
		(*p)->start_code,
		(*p)->end_code,
		(*p)->start_stack,
		KSTK_ESP((*p)->kernel_stack_page),
		KSTK_EIP((*p)->kernel_stack_page),
		(*p)->signal,
		(*p)->blocked,
		sigignore,
		sigcatch,
		wchan);
}

static int get_statm(int pid, char * buffer)
{
	struct task_struct ** p = get_task(pid);
	int i, tpag;
	int size=0, resident=0, share=0, trs=0, lrs=0, drs=0, dt=0;
	unsigned long ptbl, *buf, *pte, *pagedir, map_nr;

	if (!p || !*p)
		return 0;
	tpag = (*p)->end_code / PAGE_SIZE;
	if ((*p)->state != TASK_ZOMBIE) {
	  pagedir = (void *)((*p)->tss.cr3 + ((*p)->start_code >> 20));
	  for (i = 0; i < 0x300; ++i) {
	    if ((ptbl = pagedir[i]) == 0) {
	      tpag -= 1024;
	      continue;
	    }
	    buf = (void *)(ptbl & 0xfffff000);
	    for (pte = buf; pte < (buf + 1024); ++pte) {
	      if (*pte != 0) {
		++size;
		if (*pte & 1) {
		  ++resident;
		  if (tpag > 0)
		    ++trs;
		  else
		    ++drs;
		  if (i >= 15 && i < 0x2f0) {
		    ++lrs;
		    if (*pte & 0x40)
		      ++dt;
		    else
		      --drs;
		  }
		  map_nr = MAP_NR(*pte);
		  if (map_nr < (high_memory / 4096) && mem_map[map_nr] > 1)
		    ++share;
		}
	      }
	      --tpag;
	    }
	  }
	}
	return sprintf(buffer,"%d %d %d %d %d %d %d\n",
		       size, resident, share, trs, lrs, drs, dt);
}

static int array_read(struct inode * inode, struct file * file,char * buf, int count)
{
	char * page;
	int length;
	int end;
	unsigned int type, pid;

	if (count < 0)
		return -EINVAL;
	page = (char *) get_free_page(GFP_KERNEL);
	if (!page)
		return -ENOMEM;
	type = inode->i_ino;
	pid = type >> 16;
	type &= 0x0000ffff;
	switch (type) {
		case 2:
			length = get_loadavg(page);
			break;
		case 3:
			length = get_uptime(page);
			break;
		case 4:
			length = get_meminfo(page);
			break;
		case 6:
			length = get_version(page);
			break;
		case 9:
			length = get_env(pid, page);
			break;
		case 10:
			length = get_arg(pid, page);
			break;
		case 11:
			length = get_stat(pid, page);
			break;
		case 12:
			length = get_statm(pid, page);
			break;
		default:
			free_page((unsigned long) page);
			return -EBADF;
	}
	if (file->f_pos >= length) {
		free_page((unsigned long) page);
		return 0;
	}
	if (count + file->f_pos > length)
		count = length - file->f_pos;
	end = count + file->f_pos;
	memcpy_tofs(buf, page + file->f_pos, count);
	free_page((unsigned long) page);
	file->f_pos = end;
	return count;
}

static struct file_operations proc_array_operations = {
	NULL,		/* array_lseek */
	array_read,
	NULL,		/* array_write */
	NULL,		/* array_readdir */
	NULL,		/* array_select */
	NULL,		/* array_ioctl */
	NULL,		/* mmap */
	NULL,		/* no special open code */
	NULL,		/* no special release code */
	NULL		/* can't fsync */
};

struct inode_operations proc_array_inode_operations = {
	&proc_array_operations,	/* default base directory file-ops */
	NULL,			/* create */
	NULL,			/* lookup */
	NULL,			/* link */
	NULL,			/* unlink */
	NULL,			/* symlink */
	NULL,			/* mkdir */
	NULL,			/* rmdir */
	NULL,			/* mknod */
	NULL,			/* rename */
	NULL,			/* readlink */
	NULL,			/* follow_link */
	NULL,			/* bmap */
	NULL,			/* truncate */
	NULL			/* permission */
};
