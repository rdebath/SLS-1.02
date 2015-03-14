/*
 *  linux/kernel/fork.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

/*
 *  'fork.c' contains the help-routines for the 'fork' system call
 * (see also system_call.s).
 * Fork is rather simple, once you get the hang of it, but the memory
 * management can be a bitch. See 'mm/mm.c': 'copy_page_tables()'
 */

#include <linux/errno.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/mm.h>
#include <linux/stddef.h>
#include <linux/unistd.h>

#include <asm/segment.h>
#include <asm/system.h>

#define MAX_TASKS_PER_USER (NR_TASKS/2)

long last_pid=0;

static int find_empty_process(void)
{
	int i, task_nr;
	int this_user_tasks;

repeat:
	if ((++last_pid) & 0xffff8000)
		last_pid=1;
	this_user_tasks = 0;
	for(i=0 ; i < NR_TASKS ; i++) {
		if (!task[i])
			continue;
		if (task[i]->uid == current->uid)
			this_user_tasks++;
		if (task[i]->pid == last_pid || task[i]->pgrp == last_pid)
			goto repeat;
	}
	if (this_user_tasks > MAX_TASKS_PER_USER && !suser())
		return -EAGAIN;
/* Only the super-user can fill the last available slot */
	task_nr = 0;
	for(i=1 ; i<NR_TASKS ; i++)
		if (!task[i])
			if (task_nr)
				return task_nr;
			else
				task_nr = i;
	if (task_nr && suser())
		return task_nr;
	return -EAGAIN;
}

#define IS_CLONE (orig_eax == __NR_clone)
#define copy_vm(p) (IS_CLONE?clone_page_tables:copy_page_tables)(p)	

/*
 *  Ok, this is the main fork-routine. It copies the system process
 * information (task[nr]) and sets up the necessary registers. It
 * also copies the data segment in it's entirety.
 */
int sys_fork(long ebx,long ecx,long edx,
		long esi, long edi, long ebp, long eax, long ds,
		long es, long fs, long gs, long orig_eax,
		long eip,long cs,long eflags,long esp,long ss)
{
	struct task_struct *p;
	int i,nr;
	struct file *f;

	p = (struct task_struct *) get_free_page(GFP_KERNEL);
	if (!p)
		return -EAGAIN;
	nr = find_empty_process();
	if (nr < 0) {
		free_page((unsigned long) p);
		return nr;
	}
	task[nr] = p;
	*p = *current;
	p->kernel_stack_page = 0;
	p->state = TASK_UNINTERRUPTIBLE;
	p->flags &= ~(PF_PTRACED|PF_TRACESYS);
	p->pid = last_pid;
	if (p->pid > 1)
		p->swappable = 1;
	p->p_pptr = p->p_opptr = current;
	p->p_cptr = NULL;
	SET_LINKS(p);
	p->signal = 0;
	p->it_real_value = p->it_virt_value = p->it_prof_value = 0;
	p->it_real_incr = p->it_virt_incr = p->it_prof_incr = 0;
	p->leader = 0;		/* process leadership doesn't inherit */
	p->utime = p->stime = 0;
	p->cutime = p->cstime = 0;
	p->min_flt = p->maj_flt = 0;
	p->cmin_flt = p->cmaj_flt = 0;
	p->start_time = jiffies;
	p->tss.back_link = 0;
	p->tss.ss0 = 0x10;
	p->tss.eip = eip;
	p->tss.eflags = eflags & 0xffffcfff;	/* iopl is always 0 for a new process */
	p->tss.eax = 0;
	p->tss.ecx = ecx;
	p->tss.edx = edx;
	p->tss.ebx = ebx;
	p->tss.esp = esp;
	if (IS_CLONE)				/* clone() gets the new stack value */
		p->tss.esp = ebx;
	p->tss.ebp = ebp;
	p->tss.esi = esi;
	p->tss.edi = edi;
	p->tss.es = es & 0xffff;
	p->tss.cs = cs & 0xffff;
	p->tss.ss = ss & 0xffff;
	p->tss.ds = ds & 0xffff;
	p->tss.fs = fs & 0xffff;
	p->tss.gs = gs & 0xffff;
	p->tss.ldt = _LDT(nr);
	p->tss.trace_bitmap = offsetof(struct tss_struct,io_bitmap) << 16;
	for (i = 0; i<IO_BITMAP_SIZE ; i++)
		p->tss.io_bitmap[i] = ~0;
	if (last_task_used_math == current)
		__asm__("clts ; fnsave %0 ; frstor %0"::"m" (p->tss.i387));
	p->kernel_stack_page = get_free_page(GFP_KERNEL);
	if (!p->kernel_stack_page || copy_vm(p)) {
		task[nr] = NULL;
		REMOVE_LINKS(p);
		free_page(p->kernel_stack_page);
		free_page((long) p);
		return -EAGAIN;
	}
	p->tss.esp0 = PAGE_SIZE + p->kernel_stack_page;
	for (i=0; i<NR_OPEN;i++)
		if ((f = p->filp[i]) != NULL)
			f->f_count++;
	if (current->pwd)
		current->pwd->i_count++;
	if (current->root)
		current->root->i_count++;
	if (current->executable)
		current->executable->i_count++;
	for (i=0; i < current->numlibraries ; i++)
		if (current->libraries[i].library)
			current->libraries[i].library->i_count++;
	set_tss_desc(gdt+(nr<<1)+FIRST_TSS_ENTRY,&(p->tss));
	set_ldt_desc(gdt+(nr<<1)+FIRST_LDT_ENTRY,&(p->ldt));
	p->counter = current->counter >> 1;
	p->state = TASK_RUNNING;	/* do this last, just in case */
	return p->pid;
}
