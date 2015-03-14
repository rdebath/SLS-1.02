/*
 *  linux/kernel/chr_drv/pty.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 */

/*
 *	pty.c
 *
 * This module exports the following pty function:
 * 
 * 	int  pty_open(struct tty_struct * tty, struct file * filp);
 */

#include <linux/errno.h>
#include <linux/sched.h>
#include <linux/tty.h>
#include <linux/fcntl.h>
#include <linux/interrupt.h>

#include <asm/system.h>
#include <asm/bitops.h>

static void pty_close(struct tty_struct * tty, struct file * filp)
{
	if (!tty || (tty->count > 1))
		return;
	wake_up_interruptible(&tty->read_q.proc_list);
	if (!tty->link)
		return;
	wake_up_interruptible(&tty->link->write_q.proc_list);
	if (IS_A_PTY_MASTER(tty->line)) {
		tty_hangup(tty->link);
		flush_input(tty);
		flush_output(tty);
	}
}

static inline void pty_copy(struct tty_struct * from, struct tty_struct * to)
{
	int c;

	while (!from->stopped && !EMPTY(&from->write_q)) {
		if (FULL(&to->read_q)) {
			if (FULL(&to->secondary))
				break;
			TTY_READ_FLUSH(to);
			continue;
		}
		c = get_tty_queue(&from->write_q);
		put_tty_queue(c, &to->read_q);
		if (current->signal & ~current->blocked)
			break;
	}
	TTY_READ_FLUSH(to);
	wake_up_interruptible(&from->write_q.proc_list);
	if (from->write_data_cnt) {
		set_bit(from->line, &tty_check_write);
		mark_bh(TTY_BH);
	}
}

/*
 * This routine gets called when tty_write has put something into
 * the write_queue. It copies the input to the output-queue of it's
 * slave.
 */
static void pty_write(struct tty_struct * tty)
{
	if (tty->link)
		pty_copy(tty,tty->link);
}

int pty_open(struct tty_struct *tty, struct file * filp)
{
	if (!tty || !tty->link)
		return -ENODEV;
	tty->write = tty->link->write = pty_write;
	tty->close = tty->link->close = pty_close;
	wake_up_interruptible(&tty->read_q.proc_list);
	if (filp->f_flags & O_NDELAY)
		return 0;
	while (!tty->link->count && !(current->signal & ~current->blocked))
		interruptible_sleep_on(&tty->link->read_q.proc_list);
	if (!tty->link->count)
		return -ERESTARTSYS;
	return 0;
}
