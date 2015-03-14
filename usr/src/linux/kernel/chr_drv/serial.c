/*
 *  linux/kernel/serial.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 *
 *  Extensively rewritten by Theodore Ts'o, 8/16/92 -- 9/14/92.  Now
 *  much more extensible to support other serial cards based on the
 *  16450/16550A UART's.  Added support for the AST FourPort and the
 *  Accent Async board.  
 *
 *  set_serial_info fixed to set the flags, custom divisor, and uart
 * 	type fields.  Fix suggested by Michael K. Johnson 12/12/92.
 *
 * This module exports the following rs232 io functions:
 *
 *	long rs_init(long);
 * 	int  rs_open(struct tty_struct * tty, struct file * filp)
 */

#include <linux/errno.h>
#include <linux/signal.h>
#include <linux/sched.h>
#include <linux/timer.h>
#include <linux/tty.h>
#include <linux/serial.h>
#include <linux/interrupt.h>
#include <linux/config.h>
#include <linux/string.h>
#include <linux/fcntl.h>
#include <linux/ptrace.h>

#include <asm/system.h>
#include <asm/io.h>
#include <asm/segment.h>
#include <asm/bitops.h>

/*
 * Serial driver configuration section.  Here are the various options:
 *
 * CONFIG_AUTO_IRQ
 *		Enables automatic IRQ detection.  I've put in some
 * 		fixes to this which should make this work much more
 * 		cleanly than it used to in 0.98pl2-6.  It should be
 * 		much less vulnerable to false IRQ's now.
 * 
 * CONFIG_AST_FOURPORT
 *		Enables support for the AST Fourport serial port.
 * 
 * CONFIG_ACCENT_ASYNC
 *		Enables support for the Accent Async 4 port serial
 * 		port.
 * 
 */

#define WAKEUP_CHARS (3*TTY_BUF_SIZE/4)

/*
 * rs_event		- Bitfield of serial lines that events pending
 * 				to be processed at the next clock tick.
 *
 * We assume here that int's are 32 bits, so an array of two gives us
 * 64 lines, which is the maximum we can support.
 */
static int rs_event[2];

static struct async_struct *IRQ_ports[16];
static int IRQ_active;
static unsigned long IRQ_timer[16];
static int IRQ_timeout[16];
	
/*
 * This assumes you have a 1.8432 MHz clock for your UART.
 *
 * It'd be nice if someone built a serial card with a 24.576 MHz
 * clock, since the 16550A is capable of handling a top speed of 1.5
 * megabits/second; but this requires the faster clock.
 */
#define BASE_BAUD ( 1843200 / 16 ) 

struct async_struct rs_table[] = {
	/* UART CLK   PORT IRQ     FLAGS        */
	{ BASE_BAUD, 0x3F8, 4, ASYNC_SKIP_TEST, },	/* ttyS0 */
	{ BASE_BAUD, 0x2F8, 3, ASYNC_SKIP_TEST, },	/* ttyS1 */
	{ BASE_BAUD, 0x3E8, 4, ASYNC_SKIP_TEST, },	/* ttyS2 */
	{ BASE_BAUD, 0x2E8, 3, ASYNC_SKIP_TEST, },	/* ttyS3 */
#ifdef CONFIG_AST_FOURPORT
	{ BASE_BAUD, 0x1A0, 9, ASYNC_FOURPORT }, 	/* ttyS4 */
	{ BASE_BAUD, 0x1A8, 9, ASYNC_FOURPORT },	/* ttyS5 */
	{ BASE_BAUD, 0x1B0, 9, ASYNC_FOURPORT },	/* ttyS6 */
	{ BASE_BAUD, 0x1B8, 9, ASYNC_FOURPORT },	/* ttyS7 */

	{ BASE_BAUD, 0x2A0, 5, ASYNC_FOURPORT },	/* ttyS8 */
	{ BASE_BAUD, 0x2A8, 5, ASYNC_FOURPORT },	/* ttyS9 */
	{ BASE_BAUD, 0x2B0, 5, ASYNC_FOURPORT },	/* ttyS10 */
	{ BASE_BAUD, 0x2B8, 5, ASYNC_FOURPORT },	/* ttyS11 */
#else /* CONFIG_AST_FOURPORT */
	{ BASE_BAUD, 0x000, 0 },	/* ttyS4 */
	{ BASE_BAUD, 0x000, 0 },	/* ttyS5 */
	{ BASE_BAUD, 0x000, 0 },	/* ttyS6 */
	{ BASE_BAUD, 0x000, 0 },	/* ttyS7 */

	{ BASE_BAUD, 0x000, 0 },	/* ttyS8 */
	{ BASE_BAUD, 0x000, 0 },	/* ttyS9 */
	{ BASE_BAUD, 0x000, 0 },	/* ttyS10 */
	{ BASE_BAUD, 0x000, 0 },	/* ttyS11 */
#endif /* CONFIG_AST_FOURPORT */
	
#ifdef CONFIG_ACCENT_ASYNC
	{ BASE_BAUD, 0x330, 4, 0 },	/* ttyS12 */
	{ BASE_BAUD, 0x338, 4, 0 },	/* ttyS13 */
#else /* CONFIG_ACCENT_ASYNC */
	{ BASE_BAUD, 0x000, 0 },	/* ttyS12 */
	{ BASE_BAUD, 0x000, 0 },	/* ttyS13 */
#endif /* CONFIG_ACCENT_ASYNC */
	{ BASE_BAUD, 0x000, 0 },	/* ttyS14 (spare; user configurable) */
	{ BASE_BAUD, 0x000, 0 },	/* ttyS15 (spare; user configurable) */

	{ BASE_BAUD, 0x100, 12, 0 },	/* ttyS16 */
	{ BASE_BAUD, 0x108, 12, 0 },	/* ttyS17 */
	{ BASE_BAUD, 0x110, 12, 0 },	/* ttyS18 */
	{ BASE_BAUD, 0x118, 12, 0 },	/* ttyS19 */
	{ BASE_BAUD, 0x120, 12, 0 },	/* ttyS20 */
	{ BASE_BAUD, 0x128, 12, 0 },	/* ttyS21 */
	{ BASE_BAUD, 0x130, 12, 0 },	/* ttyS22 */
	{ BASE_BAUD, 0x138, 12, 0 },	/* ttyS23 */
	{ BASE_BAUD, 0x140, 12, 0 },	/* ttyS24 */
	{ BASE_BAUD, 0x148, 12, 0 },	/* ttyS25 */
	{ BASE_BAUD, 0x150, 12, 0 },	/* ttyS26 */
	{ BASE_BAUD, 0x158, 12, 0 },	/* ttyS27 */
	{ BASE_BAUD, 0x160, 12, 0 },	/* ttyS28 */
	{ BASE_BAUD, 0x168, 12, 0 },	/* ttyS29 */
	{ BASE_BAUD, 0x170, 12, 0 },	/* ttyS30 */
	{ BASE_BAUD, 0x178, 12, 0 },	/* ttyS31 */
};

#define NR_PORTS	(sizeof(rs_table)/sizeof(struct async_struct))

/*
 * This is used to figure out the divsor speeds and the timeouts
 */
static int baud_table[] = {
	0, 50, 75, 110, 134, 150, 200, 300, 600, 1200, 1800, 2400, 4800,
	9600, 19200, 38400, 57600, 115200, 0 };

static void rs_throttle(struct tty_struct * tty, int status);

static inline unsigned int serial_in(struct async_struct *info, int offset)
{
	return inb(info->port + offset);
}

static inline unsigned int serial_inp(struct async_struct *info, int offset)
{
	return inb_p(info->port + offset);
}

static inline void serial_out(struct async_struct *info, int offset, int value)
{
	outb(value, info->port+offset);
}

static inline void serial_outp(struct async_struct *info, int offset,
			       int value)
{
	outb_p(value, info->port+offset);
}

/*
 * ----------------------------------------------------------------------
 *
 * Here starts the interrupt handling routines.  All of the following
 * subroutines are declared as inline and are folded into
 * rs_interrupt().  They were separated out for readability's sake.
 *
 * Note: rs_interrupt() is a "fast" interrupt, which means that it
 * runs with interrupts turned off.  People who may want to modify
 * rs_interrupt() should try to keep the interrupt handler as fast as
 * possible.  After you are done making modifications, it is not a bad
 * idea to do:
 * 
 * gcc -S -DKERNEL -Wall -Wstrict-prototypes -O6 -fomit-frame-pointer serial.c
 *
 * and look at the resulting assemble code in serial.s.
 *
 * 				- Ted Ts'o (tytso@mit.edu), 7-Mar-93
 * -----------------------------------------------------------------------
 */

/*
 * This routine is used by the interrupt handler to schedule
 * processing in the software interrupt portion of the driver.
 */
static inline void rs_sched_event(struct async_struct *info,
				  int event)
{
	info->event |= 1 << event;
	set_bit(info->line, rs_event);
	mark_bh(SERIAL_BH);
}

static inline void receive_chars(struct async_struct *info,
				 int *status)
{
	struct tty_queue * queue;
	int head, tail, ch;

/*
 * Just like the LEFT(x) macro, except it uses the loal tail
 * and head variables.
 */
#define VLEFT ((tail-head-1)&(TTY_BUF_SIZE-1))

	queue = &info->tty->read_q;
	head = queue->head;
	tail = queue->tail;
	do {
		ch = serial_in(info, UART_RX);
		/*
		 * There must be at least 2 characters
		 * free in the queue; otherwise we punt.
		 */
		if (VLEFT < 2)
			continue;
		if (*status & info->read_status_mask) {
			set_bit(head, &info->tty->readq_flags);
			if (*status & (UART_LSR_BI)) {
				queue->buf[head++]= TTY_BREAK;
				rs_sched_event(info, RS_EVENT_BREAK);
			} else if (*status & UART_LSR_PE)
				queue->buf[head++]= TTY_PARITY;
			else if (*status & UART_LSR_FE)
				queue->buf[head++]= TTY_FRAME;
			head &= TTY_BUF_SIZE-1;
		}
		queue->buf[head++] = ch;
		head &= TTY_BUF_SIZE-1;
	} while ((*status = serial_inp(info, UART_LSR)) & UART_LSR_DR);
	queue->head = head;
	if ((VLEFT < RQ_THRESHOLD_LW) && !set_bit(TTY_RQ_THROTTLED,
						  &info->tty->flags)) 
		rs_throttle(info->tty, TTY_THROTTLE_RQ_FULL);
	rs_sched_event(info, RS_EVENT_READ_PROCESS);
}

static inline void transmit_chars(struct async_struct *info, int *done_work)
{
	struct tty_queue * queue;
	int head, tail, count;
	
	queue = &info->tty->write_q;
	head = queue->head;
	tail = queue->tail;
	if (head==tail && !info->x_char)
		return;
	count = info->xmit_fifo_size;
	if (info->x_char) {
		serial_outp(info, UART_TX, info->x_char);
		info->x_char = 0;
		count--;
	}
	while (count-- && (tail != head)) {
		serial_outp(info, UART_TX, queue->buf[tail++]);
		tail &= TTY_BUF_SIZE-1;
	}
	queue->tail = tail;
	if (VLEFT > WAKEUP_CHARS) {
		rs_sched_event(info, RS_EVENT_WRITE_WAKEUP);
		if (info->tty->write_data_cnt) {
			set_bit(info->tty->line, &tty_check_write);
			mark_bh(TTY_BH);
		}
	}
#ifdef SERIAL_INT_DEBUG
	printk("THRE...");
#endif
	(*done_work)++;
}

static inline int check_modem_status(struct async_struct *info)
{
	int	status;
	
	status = serial_in(info, UART_MSR);
		
	if ((status & UART_MSR_DDCD) && !C_LOCAL(info->tty)) {
		if (status & UART_MSR_DCD)
			rs_sched_event(info, RS_EVENT_OPEN_WAKEUP);
		else
			rs_sched_event(info, RS_EVENT_HANGUP);
	}
	if (C_RTSCTS(info->tty)) {
		if (info->tty->stopped) {
			if (status & UART_MSR_CTS) {
				info->tty->stopped = 0;
				return 1;
			}
		} else 
			info->tty->stopped = !(status & UART_MSR_CTS);
	}
	return 0;
}

static inline void figure_RS_timer(void)
{
	int	timeout = 6000;	/* 60 seconds; really big :-) */
	int	i, mask;
	
	if (!IRQ_active)
		return;
	for (i=0, mask = 1; mask <= IRQ_active; i++, mask <<= 1) {
		if (!(mask & IRQ_active))
			continue;
		if (IRQ_timer[i] < timeout)
			timeout = IRQ_timer[i];
	}
	timer_table[RS_TIMER].expires = timeout;
	timer_active |= 1 << RS_TIMER;
}


/*
 * This is the serial driver's generic interrupt routine
 */
static void rs_interrupt(int irq)
{
	int status;
	struct async_struct * info;
	int done, done_work, pass_number;

	info = IRQ_ports[irq];
	done = 1;
	done_work = 0;
	pass_number = 0;
	while (info) {
		if (!pass_number ||
		    !(serial_inp(info, UART_IIR) & UART_IIR_NO_INT)) {
			done = 0;
			status = serial_inp(info, UART_LSR);
			if (status & UART_LSR_DR) {
				receive_chars(info, &status);
				done_work++;
			}
		recheck_write:
			if ((status & UART_LSR_THRE) &&
			    !info->tty->stopped) {
				transmit_chars(info, &done_work);
			}
			if (check_modem_status(info))
				goto recheck_write;
		}
		
		info = info->next_port;
		if (!info && !done) {
			info = IRQ_ports[irq];
			done = 1;
			if (pass_number++ > 64)
				break; 		/* Prevent infinite loops */
		}
	}
	if (IRQ_ports[irq]) {
		if (irq && !done_work)
			IRQ_timer[irq] = jiffies + 1500;
		else
			IRQ_timer[irq] = jiffies + IRQ_timeout[irq];
		IRQ_active |= 1 << irq;
	}
	figure_RS_timer();
}

/*
 * This is the serial driver's interrupt routine while we are probing
 * for submarines.
 */
static volatile int rs_irq_triggered;
static volatile int rs_triggered;

static void rs_probe(int irq)
{
	rs_irq_triggered = irq;
	rs_triggered |= 1 << irq;
	return;
}

/*
 * -------------------------------------------------------------------
 * Here ends the serial interrupt routines.
 * -------------------------------------------------------------------
 */

/*
 * This routine is called when we receive a break on a serial line.
 * It is executed out of the software interrupt routine.
 */
static inline void handle_rs_break(struct async_struct *info)
{
	if (info->flags & ASYNC_SAK)
		do_SAK(info->tty);
		
	if (I_BRKINT(info->tty)) {
		flush_input(info->tty);
		flush_output(info->tty);
		if (info->tty->pgrp > 0)
			kill_pg(info->tty->pgrp, SIGINT,1);
	}
}

/*
 * This routine is used to handle the "bottom half" processing for the
 * serial driver, known also the "software interrupt" processing.
 * This processing is done at the kernel interrupt level, after the
 * rs_interrupt() has returned, BUT WITH INTERRUPTS TURNED ON.  This
 * is where time-consuming activities which can not be done in the
 * interrupt driver proper are done; the interrupt driver schedules
 * them using rs_sched_event(), and they get done here.
 */
static void do_softint(void *unused)
{
	int			i;
	struct async_struct	*info;
	
	for (i = 0, info = rs_table; i < NR_PORTS; i++,info++) {
		if (!clear_bit(i, rs_event)) {
			if (!info->tty)	
				continue;
			if (!clear_bit(RS_EVENT_READ_PROCESS, &info->event)) {
				TTY_READ_FLUSH(info->tty);
			}
			if (!clear_bit(RS_EVENT_WRITE_WAKEUP, &info->event)) {
				wake_up_interruptible(&info->tty->write_q.proc_list);
			}
			if (!clear_bit(RS_EVENT_HANGUP, &info->event)) {
				tty_hangup(info->tty);
				wake_up_interruptible(&info->open_wait);
				info->flags &= ~(ASYNC_NORMAL_ACTIVE|
						 ASYNC_CALLOUT_ACTIVE);
			}
			if (!clear_bit(RS_EVENT_BREAK, &info->event))
				handle_rs_break(info);
			if (!clear_bit(RS_EVENT_OPEN_WAKEUP, &info->event)) {
				wake_up_interruptible(&info->open_wait);
			}
		}
	}
}

/*
 * This subroutine is called when the RS_TIMER goes off.  It is used
 * by the serial driver to run the rs_interrupt routine at certain
 * intervals, either because a serial interrupt might have been lost,
 * or because (in the case of IRQ=0) the serial port does not have an
 * interrupt, and is being checked only via the timer interrupts.
 */
static void rs_timer(void)
{
	int	i, mask;
	int	timeout = 0;

	for (i = 0, mask = 1; mask <= IRQ_active; i++, mask <<= 1) {
		if ((mask & IRQ_active) && (IRQ_timer[i] <= jiffies)) {
			IRQ_active &= ~mask;
			if (i) {
				cli();
				rs_interrupt(i);
				sti();
			} else
				rs_interrupt(i);
		}
		if (mask & IRQ_active) {
			if (!timeout || (IRQ_timer[i] < timeout))
				timeout = IRQ_timer[i];
		}
	}
	if (timeout) {
		timer_table[RS_TIMER].expires = timeout;
		timer_active |= 1 << RS_TIMER;
	}
}

/*
 * ---------------------------------------------------------------
 * Low level utility subroutines for the serial driver:  routines to
 * figure out the appropriate timeout for an interrupt chain, routines
 * to initialize and startup a serial port, and routines to shutdown a
 * serial port.  Useful stuff like that.
 * ---------------------------------------------------------------
 */

/*
 * This routine figures out the correct timeout for a particular IRQ.
 * It uses the smallest timeout of all of the serial ports in a
 * particular interrupt chain.
 */
static void figure_IRQ_timeout(int irq)
{
	struct	async_struct	*info;
	int	timeout = 6000;	/* 60 seconds === a long time :-) */

	info = IRQ_ports[irq];
	if (!info) {
		IRQ_timeout[irq] = 0;
		return;
	}
	while (info) {
		if (info->timeout < timeout)
			timeout = info->timeout;
		info = info->next_port;
	}
	if (!irq)
		timeout = timeout / 2;
	IRQ_timeout[irq] = timeout;
}

static void startup(struct async_struct * info)
{
	unsigned short ICP;
	unsigned long flags;

	save_flags(flags); cli();

	/*
	 * First, clear the FIFO buffers and disable them
	 */
	if (info->type == PORT_16550A)
		serial_outp(info, UART_FCR, UART_FCR_CLEAR_CMD);

	/*
	 * Next, clear the interrupt registers.
	 */
	(void)serial_inp(info, UART_LSR);
	(void)serial_inp(info, UART_RX);
	(void)serial_inp(info, UART_IIR);
	(void)serial_inp(info, UART_MSR);

	/*
	 * Now, initialize the UART 
	 */
	serial_outp(info, UART_LCR, UART_LCR_WLEN8);	/* reset DLAB */
	if (info->flags & ASYNC_FOURPORT) 
		serial_outp(info, UART_MCR, UART_MCR_DTR | UART_MCR_RTS);
	else
		serial_outp(info, UART_MCR,
			    UART_MCR_DTR | UART_MCR_RTS | UART_MCR_OUT2);
	
	/*
	 * Enable FIFO's if necessary
	 */
	if (info->type == PORT_16550A) {
		serial_outp(info, UART_FCR, UART_FCR_SETUP_CMD);
		info->xmit_fifo_size = 16;
	} else {
		info->xmit_fifo_size = 1;
	}

	/*
	 * Finally, enable interrupts
	 */
	serial_outp(info, UART_IER, 0x0f);	/* enable all intrs */
	if (info->flags & ASYNC_FOURPORT) {
		/* Enable interrupts on the AST Fourport board */
		ICP = (info->port & 0xFE0) | 0x01F;
		outb_p(0x80, ICP);
		(void) inb_p(ICP);
	}

	/*
	 * And clear the interrupt registers again for luck.
	 */
	(void)serial_inp(info, UART_LSR);
	(void)serial_inp(info, UART_RX);
	(void)serial_inp(info, UART_IIR);
	(void)serial_inp(info, UART_MSR);

	info->flags |= ASYNC_INITIALIZED;
	if (info->tty)
		clear_bit(TTY_IO_ERROR, &info->tty->flags);
	/*
	 * Set up parity check flag
	 */
	if (info->tty && info->tty->termios && I_INPCK(info->tty))
		info->read_status_mask = UART_LSR_BI | UART_LSR_FE |
			UART_LSR_PE;
	else
		info->read_status_mask = UART_LSR_BI | UART_LSR_FE;
	restore_flags(flags);
}

/*
 * This routine shutsdown a serial port; interrupts are disabled, and
 * DTR is dropped if the hangup on close termio flag is on.
 */
static void shutdown(struct async_struct * info)
{
	unsigned long flags;

	save_flags(flags); cli();
	serial_outp(info, UART_IER, 0x00);	/* disable all intrs */
	if (info->tty && !(info->tty->termios->c_cflag & HUPCL))
		serial_outp(info, UART_MCR, UART_MCR_DTR);
	else
		/* reset DTR,RTS,OUT_2 */		
		serial_outp(info, UART_MCR, 0x00);
	serial_outp(info, UART_FCR, UART_FCR_CLEAR_CMD); /* disable FIFO's */
	(void)serial_in(info, UART_RX);    /* read data port to reset things */
	info->flags &= ~ASYNC_INITIALIZED;
	if (info->tty)
		set_bit(TTY_IO_ERROR, &info->tty->flags);
	restore_flags(flags);
}

/*
 * This routine is called to set the UART divisor registers to match
 * the specified baud rate for a serial port.
 */
static void change_speed(unsigned int line)
{
	struct async_struct * info;
	unsigned short port;
	int	quot = 0;
	unsigned cflag,cval,mcr;
	int	i;

	if (line >= NR_PORTS)
		return;
	info = rs_table + line;
	if (!info->tty || !info->tty->termios)
		return;
	cflag = info->tty->termios->c_cflag;
	if (!(port = info->port))
		return;
	i = cflag & CBAUD;
	if (i == 15) {
		if ((info->flags & ASYNC_SPD_MASK) == ASYNC_SPD_HI)
			i += 1;
		if ((info->flags & ASYNC_SPD_MASK) == ASYNC_SPD_VHI)
			i += 2;
		if ((info->flags & ASYNC_SPD_MASK) == ASYNC_SPD_CUST)
			quot = info->custom_divisor;
	}
	if (quot) {
		info->timeout = ((info->xmit_fifo_size*HZ*15*quot) /
				 info->baud_base) + 2;
	} else if (baud_table[i] == 134) {
		quot = (2*info->baud_base / 269);
		info->timeout = (info->xmit_fifo_size*HZ*30/269) + 2;
	} else if (baud_table[i]) {
		quot = info->baud_base / baud_table[i];
		info->timeout = (info->xmit_fifo_size*HZ*15/baud_table[i]) + 2;
	} else {
		quot = 0;
		info->timeout = 0;
	}
	mcr = serial_in(info, UART_MCR);
	if (quot) 
		serial_out(info, UART_MCR, mcr | UART_MCR_DTR);
	else {
		serial_out(info, UART_MCR, mcr & ~UART_MCR_DTR);
		return;
	}
	/* byte size and parity */
	cval = cflag & (CSIZE | CSTOPB);
	cval >>= 4;
	if (cflag & PARENB)
		cval |= UART_LCR_PARITY;
	if (!(cflag & PARODD))
		cval |= UART_LCR_EPAR;
	cli();
	serial_outp(info, UART_LCR, cval | UART_LCR_DLAB);	/* set DLAB */
	serial_outp(info, UART_DLL, quot & 0xff);	/* LS of divisor */
	serial_outp(info, UART_DLM, quot >> 8);		/* MS of divisor */
	serial_outp(info, UART_LCR, cval);		/* reset DLAB */
	sti();
}

/*
 * ------------------------------------------------------------
 * rs_write() and friends
 * ------------------------------------------------------------
 */

/*
 * This routine is used by rs_write to restart transmitter interrupts,
 * which are disabled after we have a transmitter interrupt which went
 * unacknowledged because we had run out of data to transmit.
 * 
 * Note: this subroutine must be called with the interrupts *off*
 */
static void restart_port(struct async_struct *info)
{
	struct tty_queue * queue;
	int head, tail, count;
	
	if (serial_inp(info, UART_LSR) & UART_LSR_THRE) {
		if (info->x_char) {
			serial_outp(info, UART_TX, info->x_char);
			info->x_char = 0;
		} else {
			queue = &info->tty->write_q;
			head = queue->head;
			tail = queue->tail;
			count = info->xmit_fifo_size;
			while (count--) {
				if (tail == head)
					break;
				serial_outp(info, UART_TX, queue->buf[tail++]);
				tail &= TTY_BUF_SIZE-1;
			}
			queue->tail = tail;
		}
	}
}	

/*
 * This routine gets called when tty_write has put something into
 * the write_queue.  
 */
void rs_write(struct tty_struct * tty)
{
	struct async_struct *info;

	if (!tty || tty->stopped)
		return;
	info = rs_table + DEV_TO_SL(tty->line);
	cli();
	restart_port(info);
	sti();
}

/*
 * ------------------------------------------------------------
 * rs_throttle()
 * 
 * This routine is called by the upper-layer tty layer to signal that
 * incoming characters should be throttled (and that the throttled
 * should be released).
 * ------------------------------------------------------------
 */
static void rs_throttle(struct tty_struct * tty, int status)
{
	struct async_struct *info;
	unsigned char mcr;

#ifdef notdef
	printk("throttle tty%d: %d (%d, %d)....\n", DEV_TO_SL(tty->line),
	       status, LEFT(&tty->read_q), LEFT(&tty->secondary));
#endif
	switch (status) {
	case TTY_THROTTLE_RQ_FULL:
		info = rs_table + DEV_TO_SL(tty->line);
		if (tty->termios->c_iflag & IXOFF) {
			info->x_char = STOP_CHAR(tty);
		} else {
			mcr = serial_inp(info, UART_MCR);
			mcr &= ~UART_MCR_RTS;
			serial_out(info, UART_MCR, mcr);
		}
		break;
	case TTY_THROTTLE_RQ_AVAIL:
		info = rs_table + DEV_TO_SL(tty->line);
		if (tty->termios->c_iflag & IXOFF) {
			cli();
			if (info->x_char)
				info->x_char = 0;
			else
				info->x_char = START_CHAR(tty);
			sti();
		} else {
			mcr = serial_in(info, UART_MCR);
			mcr |= UART_MCR_RTS;
			serial_out(info, UART_MCR, mcr);
		}
		break;
	}
}

/*
 * ------------------------------------------------------------
 * rs_ioctl() and friends
 * ------------------------------------------------------------
 */

static int get_serial_info(struct async_struct * info,
			   struct serial_struct * retinfo)
{
	struct serial_struct tmp;
  
	if (!retinfo)
		return -EFAULT;
	memset(&tmp, 0, sizeof(tmp));
	tmp.type = info->type;
	tmp.line = info->line;
	tmp.port = info->port;
	tmp.irq = info->irq;
	tmp.flags = info->flags;
	tmp.baud_base = info->baud_base;
	tmp.close_delay = info->close_delay;
	memcpy_tofs(retinfo,&tmp,sizeof(*retinfo));
	return 0;
}

static int set_serial_info(struct async_struct * info,
			   struct serial_struct * new_info)
{
	struct serial_struct new;
	unsigned int		irq,check_irq;
	int 			retval;
	struct 			sigaction sa;
	struct async_struct	old_info;

	if (!new_info)
		return -EFAULT;
	memcpy_fromfs(&new,new_info,sizeof(new));

	check_irq = 0;
	old_info = *info;
	if (!suser()) {
		info->flags = ((info->flags & ~ASYNC_SPD_MASK) |
			       (new.flags & ASYNC_SPD_MASK));
		info->custom_divisor = new.custom_divisor;
		new.port = 0;	/* Prevent initialization below */
		goto check_and_exit;
	}

	if ((new.irq > 15) || (new.port > 0xffff) ||
	    (new.type < PORT_UNKNOWN) || (new.type > PORT_MAX)) {
		return -EINVAL;
	}
	
	info->baud_base = new.baud_base;
	info->flags = ((info->flags & ~ASYNC_FLAGS) |
			(new.flags & ASYNC_FLAGS));
	info->custom_divisor = new.custom_divisor;
	info->type = new.type;
	info->close_delay = new.close_delay;
	
	if (new.irq == 2)
		new.irq = 9;
	irq = info->irq;
	if (irq == 2)
		irq = 9;
	
	/*
	 * If necessary, first we try to grab the new IRQ for serial
	 * interrupts.  (We have to do this early, since we may get an
	 * error trying to do this.)
	 */
	if (new.port && info->type &&
	    ((irq != new.irq) || !(info->flags & ASYNC_INITIALIZED))) {
		if (new.irq && !IRQ_ports[new.irq]) {
			sa.sa_handler = rs_interrupt;
			sa.sa_flags = (SA_INTERRUPT);
			sa.sa_mask = 0;
			sa.sa_restorer = NULL;
			retval = irqaction(new.irq,&sa);
			if (retval) {
				*info = old_info;
				return retval;
			}
		}
	}

	if ((new.irq != irq) ||
	    (new.port != info->port)) {
		/*
		 * We need to shutdown the serial port at the old
		 * port/irq combination.
		 */
		if (info->flags & ASYNC_INITIALIZED) {
			shutdown(info);
			if (info->next_port)
				info->next_port->prev_port = info->prev_port;
			if (info->prev_port)
				info->prev_port->next_port = info->next_port;
			else
				IRQ_ports[irq] = info->next_port;
			figure_IRQ_timeout(irq);
			check_irq = irq; /* Check later if we need to */
					 /* free the IRQ */
		}
		info->irq = new.irq;
		info->port = new.port;
	}
	
check_and_exit:
	if (new.port && info->type &&
	    !(info->flags & ASYNC_INITIALIZED)) {
		/*
		 * Link the port into the new interrupt chain.
		 */
		info->prev_port = 0;
		info->next_port = IRQ_ports[info->irq];
		if (info->next_port)
			info->next_port->prev_port = info;
		IRQ_ports[info->irq] = info;
		figure_IRQ_timeout(info->irq);
		startup(info);
		change_speed(info->line);
	} else if (((old_info.flags & ASYNC_SPD_MASK) !=
		    (info->flags & ASYNC_SPD_MASK)) ||
		   (old_info.custom_divisor != info->custom_divisor))
		change_speed(info->line);

	if (check_irq && !IRQ_ports[check_irq])
		free_irq(check_irq);
	
	return 0;
}

static int get_modem_info(struct async_struct * info, unsigned int *value)
{
	unsigned char control, status;
	unsigned int result;

	control = serial_in(info, UART_MCR);
	status = serial_in(info, UART_MSR);
	result =  ((control & UART_MCR_RTS) ? TIOCM_RTS : 0)
		| ((control & UART_MCR_DTR) ? TIOCM_DTR : 0)
		| ((status  & UART_MSR_DCD) ? TIOCM_CAR : 0)
		| ((status  & UART_MSR_RI) ? TIOCM_RNG : 0)
		| ((status  & UART_MSR_DSR) ? TIOCM_DSR : 0)
		| ((status  & UART_MSR_CTS) ? TIOCM_CTS : 0);
	put_fs_long(result,(unsigned long *) value);
	return 0;
}

static int set_modem_info(struct async_struct * info, unsigned int cmd,
			  unsigned int *value)
{
	unsigned char control;
	unsigned int arg = get_fs_long((unsigned long *) value);
	
	control = serial_in(info, UART_MCR);

	switch (cmd) {
		case TIOCMBIS:
			if (arg & TIOCM_RTS)
				control |= UART_MCR_RTS;
			if (arg & TIOCM_DTR)
				control |= UART_MCR_DTR;
			break;
		case TIOCMBIC:
			if (arg & TIOCM_RTS)
				control &= ~UART_MCR_RTS;
			if (arg & TIOCM_DTR)
				control &= ~UART_MCR_DTR;
			break;
		case TIOCMSET:
			control = (control & ~0x03)
				| ((arg & TIOCM_RTS) ? UART_MCR_RTS : 0)
				| ((arg & TIOCM_DTR) ? UART_MCR_DTR : 0);
			break;
		default:
			return -EINVAL;
	}
	serial_out(info, UART_MCR, control);
	return 0;
}

/*
 * This routine sends a break character out the serial port.
 */
static void send_break(	struct async_struct * info, int duration)
{
	if (!info->port)
		return;
	current->state = TASK_INTERRUPTIBLE;
	current->timeout = jiffies + duration;
	serial_out(info, UART_LCR, serial_inp(info, UART_LCR) | UART_LCR_SBC);
	schedule();
	serial_out(info, UART_LCR, serial_inp(info, UART_LCR) & ~UART_LCR_SBC);
}

static int rs_ioctl(struct tty_struct *tty, struct file * file,
		    unsigned int cmd, unsigned long arg)
{
	int error, line;
	struct async_struct * info;

	line = DEV_TO_SL(tty->line);
	if (line < 0 || line >= NR_PORTS)
		return -ENODEV;
	info = rs_table + line;
	
	switch (cmd) {
		case TCSBRK:	/* SVID version: non-zero arg --> no break */
			wait_until_sent(tty);
			if (!arg)
				send_break(info, HZ/4);	/* 1/4 second */
			return 0;
		case TCSBRKP:	/* support for POSIX tcsendbreak() */
			wait_until_sent(tty);
			send_break(info, arg ? arg*(HZ/10) : HZ/4);
			return 0;
		case TIOCGSOFTCAR:
			error = verify_area(VERIFY_WRITE, (void *) arg,sizeof(unsigned int *));
			if (error)
				return error;
			put_fs_long(C_LOCAL(tty) ? 1 : 0,
				    (unsigned long *) arg);
			return 0;
		case TIOCSSOFTCAR:
			arg = get_fs_long((unsigned long *) arg);
			tty->termios->c_cflag =
				((tty->termios->c_cflag & ~CLOCAL) |
				 (arg ? CLOCAL : 0));
			return 0;
		case TIOCMGET:
			error = verify_area(VERIFY_WRITE, (void *) arg,sizeof(unsigned int *));
			if (error)
				return error;
			return get_modem_info(info, (unsigned int *) arg);
		case TIOCMBIS:
		case TIOCMBIC:
		case TIOCMSET:
			return set_modem_info(info, cmd, (unsigned int *) arg);
		case TIOCGSERIAL:
			error = verify_area(VERIFY_WRITE, (void *) arg,
						sizeof(struct serial_struct));
			if (error)
				return error;
			return get_serial_info(info,
					       (struct serial_struct *) arg);
		case TIOCSSERIAL:
			return set_serial_info(info,
					       (struct serial_struct *) arg);
		
	default:
		return -EINVAL;
	}
	return 0;
}

static void rs_set_termios(struct tty_struct *tty, struct termios *old_termios)
{
	struct async_struct *info;

	if (tty->termios->c_cflag == old_termios->c_cflag)
		return;

	info = &rs_table[DEV_TO_SL(tty->line)];

	change_speed(DEV_TO_SL(tty->line));
	
	if ((old_termios->c_cflag & CRTSCTS) &&
	    !(tty->termios->c_cflag & CRTSCTS)) {
		tty->stopped = 0;
		rs_write(tty);
	}

	if (!(old_termios->c_cflag & CLOCAL) &&
	    (tty->termios->c_cflag & CLOCAL))
		wake_up_interruptible(&info->open_wait);

	if (I_INPCK(tty))
		info->read_status_mask = UART_LSR_BI | UART_LSR_FE |
			UART_LSR_PE;
	else
		info->read_status_mask = UART_LSR_BI | UART_LSR_FE;
}

/*
 * ------------------------------------------------------------
 * rs_close()
 * 
 * This routine is called when the serial port gets closed.  First, we
 * wait for the last remaining data to be sent.  Then, we unlink its
 * async structure from the interrupt chain if necessary, and we free
 * that IRQ if nothing is left in the chain.
 * ------------------------------------------------------------
 */
static void rs_close(struct tty_struct *tty, struct file * filp)
{
	struct async_struct * info;
	int irq, line;

	line = DEV_TO_SL(tty->line);
	if ((line < 0) || (line >= NR_PORTS))
		return;
	info = rs_table + line;
#ifdef SERIAL_DEBUG_OPEN
	printk("rs_close ttys%d, count = %d\n", info->line, info->count);
#endif
	if (--info->count > 0)
		return;
	tty->stopped = 0;		/* Force flush to succeed */
	wait_until_sent(tty);
	clear_bit(line, rs_event);
	info->event = 0;
	info->count = 0;
	info->flags &= ~(ASYNC_NORMAL_ACTIVE|ASYNC_CALLOUT_ACTIVE);
	if (info->blocked_open) {
		shutdown(info);
		if (info->close_delay) {
			current->state = TASK_INTERRUPTIBLE;
			current->timeout = jiffies + info->close_delay;
			schedule();
		}
		startup(info);
		return;
	}
	if (info->flags & ASYNC_INITIALIZED) {
		shutdown(info);
		irq = info->irq;
		if (irq == 2)
			irq = 9;
		if (info->next_port)
			info->next_port->prev_port = info->prev_port;
		if (info->prev_port)
			info->prev_port->next_port = info->next_port;
		else
			IRQ_ports[irq] = info->next_port;
		if (irq && !IRQ_ports[irq])
			free_irq(irq);
		figure_IRQ_timeout(irq);
	}
	info->tty = 0;
}

/*
 * ------------------------------------------------------------
 * rs_open() and friends
 * ------------------------------------------------------------
 */
static int block_til_ready(struct tty_struct *tty, struct file * filp,
			   struct async_struct *info)
{
	struct wait_queue wait = { current, NULL };
	int	retval;
	
	/*
	 * If this is a callout device, then just make sure the normal
	 * device isn't being used.
	 */
	if (MAJOR(filp->f_rdev) == 5) {
		if (info->flags & ASYNC_NORMAL_ACTIVE)
			return -EBUSY;
		info->flags |= ASYNC_CALLOUT_ACTIVE;
		return 0;
	}
	
	/*
	 * If non-blocking mode is set, then make the check up front
	 * and then exit.
	 */
	if (filp->f_flags & O_NONBLOCK) {
		if (info->flags & ASYNC_CALLOUT_ACTIVE)
			return -EBUSY;
		info->flags |= ASYNC_NORMAL_ACTIVE;
		return 0;
	}

	/*
	 * Block waiting for the carrier detect and the line to become
	 * free (i.e., not in use by the callout).  While we are in
	 * this loop, info->count is dropped by one, so that
	 * rs_close() knows when to free things.  We restore it upon
	 * exit, either normal or abnormal.
	 */
	retval = 0;
	add_wait_queue(&info->open_wait, &wait);
#ifdef SERIAL_DEBUG_OPEN
	printk("block_til_ready before block: ttys%d, count = %d\n",
	       info->line, info->count);
#endif
	info->count--;
	info->blocked_open++;
	while (1) {
		serial_out(info, UART_MCR,
			   serial_inp(info, UART_MCR) | UART_MCR_DTR);
		current->state = TASK_INTERRUPTIBLE;
		if (tty_hung_up_p(filp)) {
			if (info->flags & ASYNC_HUP_NOTIFY)
				retval = -EAGAIN;
			else
				retval = -ERESTARTNOINTR;
			break;
		}
		if (!(info->flags & ASYNC_CALLOUT_ACTIVE) &&
		    (C_LOCAL(tty) ||
		     (serial_in(info, UART_MSR) & UART_MSR_DCD)))
			break;
		if (current->signal & ~current->blocked) {
			retval = -ERESTARTSYS;
			break;
		}
#ifdef SERIAL_DEBUG_OPEN
		printk("block_til_ready blocking: ttys%d, count = %d\n",
		       info->line, info->count);
#endif
		schedule();
	}
	current->state = TASK_RUNNING;
	remove_wait_queue(&info->open_wait, &wait);
	info->count++;
	info->blocked_open--;
#ifdef SERIAL_DEBUG_OPEN
	printk("block_til_ready after blocking: ttys%d, count = %d\n",
	       info->line, info->count);
#endif
	if (retval)
		return retval;
	info->flags |= ASYNC_NORMAL_ACTIVE;
	return 0;
}	

/*
 * This routine is called whenever a serial port is opened.  It
 * enables interrupts for a serial port, linking in its async structure into
 * the IRQ chain.   It also performs the serial-speicific
 * initalization for the tty structure.
 */
int rs_open(struct tty_struct *tty, struct file * filp)
{
	struct async_struct	*info;
	int 			irq, retval, line;
	struct sigaction	sa;

	line = DEV_TO_SL(tty->line);
	if ((line < 0) || (line >= NR_PORTS))
		return -ENODEV;
	info = rs_table + line;
#ifdef SERIAL_DEBUG_OPEN
	printk("rs_open ttys%d, count = %d\n", info->line, info->count);
#endif
	info->count++;
	info->tty = tty;
	
	tty->write = rs_write;
	tty->close = rs_close;
	tty->ioctl = rs_ioctl;
	tty->throttle = rs_throttle;
	tty->set_termios = rs_set_termios;

	if (!(info->flags & ASYNC_INITIALIZED)) {
		if (!info->port || !info->type) {
			set_bit(TTY_IO_ERROR, &tty->flags);
			return 0;
		}
		irq = info->irq;
		if (irq == 2)
			irq = 9;
		if (irq && !IRQ_ports[irq]) {
			sa.sa_handler = rs_interrupt;
			sa.sa_flags = (SA_INTERRUPT);
			sa.sa_mask = 0;
			sa.sa_restorer = NULL;
			retval = irqaction(irq,&sa);
			if (retval)
				return retval;
		}
		/*
		 * Link in port to IRQ chain
		 */
		info->prev_port = 0;
		info->next_port = IRQ_ports[irq];
		if (info->next_port)
			info->next_port->prev_port = info;
		IRQ_ports[irq] = info;
		figure_IRQ_timeout(irq);
		
		startup(info);
		change_speed(info->line);
		if (!irq) {
			cli();
			figure_RS_timer();
			sti();
		}
	}

	retval = block_til_ready(tty, filp, info);
	if (retval)
		return retval;
	
	return 0;
}

/*
 * ---------------------------------------------------------------------
 * rs_init() and friends
 *
 * rs_init() is called at boot-time to initialize the serial driver.
 * ---------------------------------------------------------------------
 */

/*
 * This routine prints out the appropriate serial driver version
 * number, and identifies which options were configured into this
 * driver.
 */
static void show_serial_version(void)
{
	printk("Serial driver version 3.94 with");
#ifdef CONFIG_AST_FOURPORT
	printk(" AST_FOURPORT");
#define SERIAL_OPT
#endif
#ifdef CONFIG_ACCENT_ASYNC
	printk(" ACCENT_ASYNC");
#define SERIAL_OPT
#endif
#ifdef CONFIG_AUTO_IRQ
	printk (" AUTO_IRQ");
#define SERIAL_OPT
#endif
#ifdef SERIAL_OPT
	printk(" enabled\n");
#else
	printk(" no serial options enabled\n");
#endif
#undef SERIAL_OPT
}

/*
 * This routine is called by init(); it attempts to determine which
 * interrupt a serial port is configured to use.  It is not
 * fool-proof, but it works a large part of the time.
 */
static int get_auto_irq(struct async_struct *info)
{
	unsigned char save_MCR, save_IER, save_ICP=0;
	unsigned short ICP=0, port = info->port;
	unsigned long timeout;
	
	/*
	 * Enable interrupts and see who answers
	 */
	rs_irq_triggered = 0;
	save_IER = serial_inp(info, UART_IER);
	save_MCR = serial_inp(info, UART_MCR);
	if (info->flags & ASYNC_FOURPORT)  {
		serial_outp(info, UART_MCR, UART_MCR_DTR | UART_MCR_RTS);
		serial_outp(info, UART_IER, 0x0f);	/* enable all intrs */
		ICP = (port & 0xFE0) | 0x01F;
		save_ICP = inb_p(ICP);
		outb_p(0x80, ICP);
		(void) inb_p(ICP);
	} else {
		serial_outp(info, UART_MCR,
			    UART_MCR_DTR | UART_MCR_RTS | UART_MCR_OUT2);
		serial_outp(info, UART_IER, 0x0f);	/* enable all intrs */
	}
	/*
	 * Next, clear the interrupt registers.
	 */
	(void)serial_inp(info, UART_LSR);
	(void)serial_inp(info, UART_RX);
	(void)serial_inp(info, UART_IIR);
	(void)serial_inp(info, UART_MSR);
	
	timeout = jiffies+2;
	while (timeout >= jiffies) {
		if (rs_irq_triggered)
			break;
	}
	/*
	 * Now check to see if we got any business, and clean up.
	 */
	serial_outp(info, UART_IER, save_IER);
	serial_outp(info, UART_MCR, save_MCR);
	if (info->flags & ASYNC_FOURPORT)
		outb_p(save_ICP, ICP);
	return(rs_irq_triggered);
}

/*
 * This routine is called by rs_init() to initialize a specific serial
 * port.  If CONFIG_AUTO_IRQ is defined, it will attempt to figure out
 * which IRQ the serial port is on by calling get_auto_irq().  (See
 * above).
 *
 * It also determines what type of UART ship this serial port is
 * using: 8250, 16450, 16550, 16550A.  The important question is
 * whether or not this UART is a 16550A or not, since this will
 * determine whether or not we can use its FIFO features or not.
 */
static void init(struct async_struct * info)
{
	unsigned char status1, status2, scratch, scratch2;
	unsigned port = info->port;
	int retries;

	if (!port)
		return;

	/*
	 * Do a simple existence test first; if we fail this, there's
	 * no point trying anything else.
	 */
	scratch = serial_inp(info, UART_IER);
	serial_outp(info, UART_IER, 0);
	scratch2 = serial_inp(info, UART_IER);
	serial_outp(info, UART_IER, scratch);
	if (scratch2)
		return;		/* We failed; there's nothing here */

	/* 
	 * Check to see if a UART is really there.  Certain broken
	 * internal modems based on the Rockwell chipset fail this
	 * test, because they apparently don't implement the loopback
	 * test mode.  So this test is skipped on the COM 1 through
	 * COM 4 ports.  This *should* be safe, since no board
	 * manufactucturer would be stupid enough to design a board
	 * that conflicts with COM 1-4 --- we hope!
	 */
	if (!(info->flags & ASYNC_SKIP_TEST)) {
		scratch = serial_inp(info, UART_MCR);
		serial_outp(info, UART_MCR, UART_MCR_LOOP | scratch);
		scratch2 = serial_inp(info, UART_MSR);
		serial_outp(info, UART_MCR, UART_MCR_LOOP | 0x0A);
		status1 = serial_inp(info, UART_MSR) & 0xF0;
		serial_outp(info, UART_MCR, scratch);
		serial_outp(info, UART_MSR, scratch2);
		if (status1 != 0x90) {
			info->type = PORT_UNKNOWN;
			return;
		}
	}

	/*
	 * Here's where we do the automatic IRQ detection.  We always
	 * try to do this test; CONFIG_AUTO_IRQ merely determins
	 * whether or not we pay attention to the results.  If
	 * CONFIG_AUTO_IRQ is off, then we merely print a warning
	 * message if the default IRQ does not match the results made
	 * by the automatic IRQ detection system.
	 */
	scratch = scratch2 = 0;
	for (retries = 0; retries < 5; retries++) {
		if (!scratch)
			scratch = get_auto_irq(info);
		if (!scratch2)
			scratch2 = get_auto_irq(info);
		if (scratch && scratch2) {
			if (scratch == scratch2)
				break;
			scratch = scratch2 = 0;
		}
	}
	if (scratch && (scratch == scratch2)) {
#ifdef CONFIG_AUTO_IRQ
		info->irq = scratch;
#else
		if (info->irq != scratch)
			printk("Warning: auto IRQ detection for tty%d found IRQ %d, not %d.\n",
			       info->line, scratch, info->irq);
#endif
	} else {
#ifdef CONFIG_AUTO_IRQ
		info->type = PORT_UNKNOWN;
		return;
#else
		printk("Warning: auto IRQ detection for tty%d failed; using default IRQ.\n", info->line);
#endif
	}
	
	outb_p(UART_FCR_ENABLE_FIFO, UART_FCR + port);
	scratch = inb(UART_IIR + port) >> 6;
	info->xmit_fifo_size = 1;
	switch (scratch) {
		case 0:
			info->type = PORT_16450;
			break;
		case 1:
			info->type = PORT_UNKNOWN;
			break;
		case 2:
			info->type = PORT_16550;
			break;
		case 3:
			info->type = PORT_16550A;
			info->xmit_fifo_size = 16;
			break;
	}
	if (info->type == PORT_16450) {
		scratch = inb(UART_SCR + port);
		outb_p(0xa5, UART_SCR + port);
		status1 = inb(UART_SCR + port);
		outb_p(0x5a, UART_SCR + port);
		status2 = inb(UART_SCR + port);
		outb_p(scratch, UART_SCR + port);
		if ((status1 != 0xa5) || (status2 != 0x5a))
			info->type = PORT_8250;
	}
	shutdown(info);
}

/*
 * The serial driver boot-time initialization code!
 */
long rs_init(long kmem_start)
{
	int i;
	struct async_struct * info;
	int irq_lines = 0;
	struct sigaction sa;
	unsigned long timeout;
	
	/*
	 *  We will be auto probing for irq's, so turn on interrupts now!
	 */
	sti();
	
	sa.sa_handler = rs_probe;
	sa.sa_flags = (SA_INTERRUPT);
	sa.sa_mask = 0;
	sa.sa_restorer = NULL;

	memset(&rs_event, 0, sizeof(rs_event));
	bh_base[SERIAL_BH].routine = do_softint;
	timer_table[RS_TIMER].fn = rs_timer;
	timer_table[RS_TIMER].expires = 0;
	IRQ_active = 0;
	
	rs_triggered = 0;
	for (i = 0; i < 16; i++) {
		IRQ_ports[i] = 0;
		IRQ_timeout[i] = 0;
		if (!irqaction(i, &sa))
			irq_lines |= 1 << i;
	}
	
	timeout = jiffies+10;
	while (timeout >= jiffies)
		;
	for (i = 0; i < 16; i++) {
		if ((rs_triggered & (1 << i)) &&
		    (irq_lines & (1 << i))) {
			irq_lines &= ~(1 << i);
			printk("Wild interrupt?  (IRQ %d)\n", i);
			free_irq(i);
		}
	}
	
	show_serial_version();
	for (i = 0, info = rs_table; i < NR_PORTS; i++,info++) {
		info->line = i;
		info->tty = 0;
		info->type = PORT_UNKNOWN;
		info->custom_divisor = 0;
		info->close_delay = 50;
		info->x_char = 0;
		info->event = 0;
		info->count = 0;
		info->blocked_open = 0;
		info->open_wait = 0;
		info->next_port = 0;
		info->prev_port = 0;
		init(info);
		if (info->type == PORT_UNKNOWN)
			continue;
		printk("tty%02d%s at 0x%04x (irq = %d)", info->line, 
		       (info->flags & ASYNC_FOURPORT) ? " FourPort" : "",
		       info->port, info->irq);
		switch (info->type) {
			case PORT_8250:
				printk(" is a 8250\n");
				break;
			case PORT_16450:
				printk(" is a 16450\n");
				break;
			case PORT_16550:
				printk(" is a 16550\n");
				break;
			case PORT_16550A:
				printk(" is a 16550A\n");
				break;
			default:
				printk("\n");
				break;
		}
	}
	/*
	 * Turn interrupts back off, since they were off when we
	 * started this.  See start_kernel() in init/main.c.
	 */
	cli();
	for (i = 0; i < 16; i++) {
		if (irq_lines & (1 << i))
			free_irq(i);
	}
	return kmem_start;
}

