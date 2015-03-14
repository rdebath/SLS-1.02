/*
 *  linux/kernel/signal.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/signal.h>
#include <linux/errno.h>
#include <linux/wait.h>
#include <linux/ptrace.h>
#include <linux/unistd.h>

#include <asm/segment.h>

#define _S(nr) (1<<((nr)-1))

#define _BLOCKABLE (~(_S(SIGKILL) | _S(SIGSTOP)))

extern int core_dump(long signr,struct pt_regs * regs);
int do_signal(unsigned long oldmask, struct pt_regs * regs);

int sys_sgetmask(void)
{
	return current->blocked;
}

int sys_ssetmask(int newmask)
{
	int old=current->blocked;

	current->blocked = newmask & _BLOCKABLE;
	return old;
}

int sys_sigpending(sigset_t *set)
{
	int error;
	/* fill in "set" with signals pending but blocked. */
	error = verify_area(VERIFY_WRITE, set, 4);
	if (!error)
		put_fs_long(current->blocked & current->signal, (unsigned long *)set);
	return error;
}

/*
 * atomically swap in the new signal mask, and wait for a signal.
 */
int sys_sigsuspend(volatile int restart, volatile unsigned long oldmask, unsigned long set)
{
	unsigned long mask;
	struct pt_regs * regs = (struct pt_regs *) &restart;

	mask = current->blocked;
	current->blocked = set & _BLOCKABLE;
	regs->eax = -EINTR;
	while (1) {
		current->state = TASK_INTERRUPTIBLE;
		schedule();
		if (do_signal(mask,regs))
			return -EINTR;
	}
}

/*
 * POSIX 3.3.1.3:
 *  "Setting a signal action to SIG_IGN for a signal that is pending
 *   shall cause the pending signal to be discarded, whether or not
 *   it is blocked" (but SIGCHLD is unspecified: linux leaves it alone).
 *
 *  "Setting a signal action to SIG_DFL for a signal that is pending
 *   and whose default action is to ignore the signal (for example,
 *   SIGCHLD), shall cause the pending signal to be discarded, whether
 *   or not it is blocked"
 *
 * Note the silly behaviour of SIGCHLD: SIG_IGN means that the signal
 * isn't actually ignored, but does automatic child reaping, while
 * SIG_DFL is explicitly said by POSIX to force the signal to be ignored..
 */
static void check_pending(int signum)
{
	struct sigaction *p;

	p = signum - 1 + current->sigaction;
	if (p->sa_handler == SIG_IGN) {
		if (signum == SIGCHLD)
			return;
		current->signal &= ~_S(signum);
		return;
	}
	if (p->sa_handler == SIG_DFL) {
		if (signum != SIGCONT && signum != SIGCHLD && signum != SIGWINCH)
			return;
		current->signal &= ~_S(signum);
		return;
	}	
}

int sys_signal(int signum, long handler)
{
	struct sigaction tmp;

	if (signum<1 || signum>32 || signum==SIGKILL || signum==SIGSTOP)
		return -EINVAL;
	tmp.sa_handler = (void (*)(int)) handler;
	tmp.sa_mask = 0;
	tmp.sa_flags = SA_ONESHOT | SA_NOMASK | SA_INTERRUPT;
	tmp.sa_restorer = NULL;
	handler = (long) current->sigaction[signum-1].sa_handler;
	current->sigaction[signum-1] = tmp;
	check_pending(signum);
	return handler;
}

int sys_sigaction(int signum, const struct sigaction * action,
	struct sigaction * oldaction)
{
	struct sigaction new, *p;

	if (signum<1 || signum>32 || signum==SIGKILL || signum==SIGSTOP)
		return -EINVAL;
	p = signum - 1 + current->sigaction;
	if (action) {
		memcpy_fromfs(&new, action, sizeof(struct sigaction));
		if (new.sa_flags & SA_NOMASK)
			new.sa_mask = 0;
		else {
			new.sa_mask |= _S(signum);
			new.sa_mask &= _BLOCKABLE;
		}
	}
	if (oldaction) {
		if (!verify_area(VERIFY_WRITE,oldaction, sizeof(struct sigaction)))
			memcpy_tofs(oldaction, p, sizeof(struct sigaction));
	}
	if (action) {
		*p = new;
		check_pending(signum);
	}
	return 0;
}

extern int sys_waitpid(pid_t pid,unsigned long * stat_addr, int options);

/*
 * This sets regs->esp even though we don't actually use sigstacks yet..
 */
int sys_sigreturn(int signr, unsigned long oldmask, unsigned long esp)
{
	struct pt_regs * regs;

	regs = (struct pt_regs *) &signr;
	current->blocked = oldmask & _BLOCKABLE;
	regs->esp = esp;
	return 0;
}

/*
 * This routine sets up the return stack for the first signal found
 * (== last delivered). It makes room for the registers we need to save,
 * but the actual saving is left until the very last moment when we
 * know whether we can restart system calls etc.
 */
static unsigned long * setup_first(struct pt_regs * regs,
	int signr, unsigned long sa_handler, unsigned long oldmask)
{
	unsigned long * tmp_esp;

	regs->esp -= 18*4;
	tmp_esp = (unsigned long *) regs->esp;
	verify_area(VERIFY_WRITE,tmp_esp,18*4);
/* set up the "normal" stack seen by the signal handler */
	put_fs_long(regs->esp+15*4,tmp_esp);	/* points to the stack.. */
	put_fs_long(signr,tmp_esp+1);		/* parameter to handler and sigreturn */
	put_fs_long((unsigned long) (tmp_esp+5),tmp_esp+2);
	put_fs_long(oldmask,tmp_esp+3);		/* second .. */
	put_fs_long(__NR_sigreturn,tmp_esp+4);	/* sigreturn number.. */
/* save this frame so that we later can fill in the saved registers */
	return tmp_esp+5;
}

/*
 * This sets up the stack for any stacked signals other than the
 * first one: no need to restore registers etc, as that is done
 * by the very last signal handler return code..
 */
static void setup_other(unsigned long eip, struct pt_regs * regs, int signr,
	unsigned long sa_handler, unsigned long oldmask)
{
	unsigned long * tmp_esp;

	regs->esp -= 9*4;
	tmp_esp = (unsigned long *) regs->esp;
	verify_area(VERIFY_WRITE,tmp_esp,9*4);
/* set up the "normal" stack seen by the signal handler */
	put_fs_long(regs->esp+6*4,tmp_esp);	/* points to the stack.. */
	put_fs_long(signr,tmp_esp+1);		/* parameter to handler and sigreturn */
	put_fs_long((unsigned long) (tmp_esp+5),tmp_esp+2);
	put_fs_long(oldmask,tmp_esp+3);		/* second .. */
	put_fs_long(__NR_sigreturn,tmp_esp+4);	/* sigreturn number.. */
	put_fs_long(eip,tmp_esp+5);		/* return address */
/* set up the return code... */
	put_fs_long(0x58595a5b,tmp_esp+6);	/* pop bx,dx,cx,ax */
	put_fs_long(0x909080cd,tmp_esp+7);	/* int $0x80 + nop + nop */
	put_fs_long(0x000cc290,tmp_esp+8);	/* nop + "ret 12" */
}

/*
 * Note that 'init' is a special process: it doesn't get signals it doesn't
 * want to handle. Thus you cannot kill init even with a SIGKILL even by
 * mistake.
 */
int do_signal(unsigned long oldmask, struct pt_regs * regs)
{
	unsigned long *frame = NULL;
	unsigned long eip = 0;
	unsigned long signr;
	unsigned long sa_handler;
	struct sigaction * sa;

	while ((signr = current->signal & ~current->blocked)) {
		__asm__("bsf %2,%1\n\t"
			"btrl %1,%0"
			:"=m" (current->signal),"=r" (signr)
			:"1" (signr));
		sa = current->sigaction + signr;
		signr++;
		sa_handler = (unsigned long) sa->sa_handler;
		if (sa_handler==1) {
/* check for SIGCHLD: it's special */
			if (signr == SIGCHLD)
				while (sys_waitpid(-1,NULL,WNOHANG) > 0)
					/* nothing */;
			continue;
		}
		if (!sa_handler) {
			if (current->pid == 1)
				continue;
			switch (signr) {
			case SIGCONT: case SIGCHLD: case SIGWINCH:
				continue;

			case SIGSTOP: case SIGTSTP: case SIGTTIN: case SIGTTOU:
				current->state = TASK_STOPPED;
				current->exit_code = signr;
				if (!(current->p_pptr->sigaction[SIGCHLD-1].sa_flags & 
						SA_NOCLDSTOP))
					send_sig(SIGCHLD, current->p_pptr, 1);
				schedule();
				continue;

			case SIGQUIT: case SIGILL: case SIGTRAP:
			case SIGIOT: case SIGFPE: case SIGSEGV:
				if (core_dump(signr,regs))
					signr |= 0x80;
				/* fall through */
			default:
				current->signal |= _S(signr & 0x7f);
				do_exit(signr);
			}
		}
		/*
		 * OK, we're invoking a handler
		 */
		if (regs->orig_eax >= 0) {
			if (regs->eax == -ERESTARTNOHAND ||
			   (regs->eax == -ERESTARTSYS && (sa->sa_flags & SA_INTERRUPT)))
				regs->eax = -EINTR;
		}
		if (sa->sa_flags & SA_ONESHOT)
			sa->sa_handler = NULL;
/* force a supervisor-mode page-in of the signal handler to reduce races */
		__asm__("testb $0,%%fs:%0"::"m" (*(char *) sa_handler));
		if (!frame) {
			frame = setup_first(regs,signr,sa_handler,oldmask);
		} else
			setup_other(eip,regs,signr,sa_handler,oldmask);
		eip = sa_handler;
		current->blocked |= sa->sa_mask;
		oldmask |= sa->sa_mask;
	}
	if (regs->orig_eax >= 0 &&
	    (regs->eax == -ERESTARTNOHAND ||
	     regs->eax == -ERESTARTSYS ||
	     regs->eax == -ERESTARTNOINTR)) {
		regs->eax = regs->orig_eax;
		regs->eip -= 2;
	}
	if (!frame)				/* no handlers installed - return 0 */
		return 0;
/* save registers if one or more handlers are called.. */
	put_fs_long(regs->edi,frame);		/* suitable order for "popad" */
	put_fs_long(regs->esi,frame+1);
	put_fs_long(regs->ebp,frame+2);		/* using 'frame++' instead of the 'frame+x' */
	put_fs_long(regs->esp,frame+3);		/* form used now results in atrocious code */
	put_fs_long(regs->ebx,frame+4);		/* due to gcc not being very good at optimizing */
	put_fs_long(regs->edx,frame+5);		/* things with inline-assembly/functions.. */
	put_fs_long(regs->ecx,frame+6);
	put_fs_long(regs->eax,frame+7);
	put_fs_long(regs->eflags,frame+8);	/* flags */
	put_fs_long(regs->eip,frame+9);		/* original return address */
/* set up the return code... */
	put_fs_long(0x58595a5b,frame+10);	/* pop bx,dx,cx,ax */
	put_fs_long(0x906180cd,frame+11);	/* int $0x80 + popad + nop */
	put_fs_long(0x000cc29d,frame+12);	/* popfl + "ret 12" */
	regs->eip = eip;			/* "return" to the first handler */
	return 1;
}
