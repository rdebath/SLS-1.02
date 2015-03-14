/*
    linux/kernel/blk_drv/mcd.c - Mitsumi CDROM driver

    Copyright (C) 1992  Martin Harriss

    martin@bdsi.com

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. 

*/

#include <linux/config.h>

#include <linux/errno.h>
#include <linux/signal.h>
#include <linux/sched.h>
#include <linux/timer.h>
#include <linux/fs.h>
#include <linux/kernel.h>

/* #define REALLY_SLOW_IO  */
#include <asm/system.h>
#include <asm/io.h>
#include <asm/segment.h>

#define MAJOR_NR 12
#include "blk.h"

/* *** change this to set the I/O port address */
#define MCDPORT(x)		(mcd_port + (x))

/* *** change this to set the interrupt number */
#define MCD_INTR_NR		mcd_intr_nr

/* status bits */

#define MST_DSK_CHG		0x20			/* disk removed or changed */
#define MST_DSK_IN		0x40			/* disk in the drive */
#define MST_DOOR_OPEN		0x80			/* door is open */

#define SET_TIMER(func, jifs) \
	((timer_table[MCD_TIMER].expires = jiffies + jifs), \
	(timer_table[MCD_TIMER].fn = func), \
	(timer_active |= 1<<MCD_TIMER))

#define CLEAR_TIMER		timer_active &= ~(1<<MCD_TIMER)

#if 0
static int mcd_sizes[] = { 0 };
#endif

static int mcdPresent = 0;

/* buffer for block size conversion */

static int mcd_intr_nr = -1;
static int mcd_port = -1;
static char mcd_buf[2048];
static int mcd_bn = -1;
static int McdTimeout, McdTries;

static void mcd_transfer(void);
static void mcd_start(void);
static void mcd_status(void);
static void mcd_read_cmd(void);
static void mcd_data(void);
static void do_mcd_request(void);
static void hsg2msf(long hsg, char *msf);
static void bin2bcd(char *p);
static int mcdStatus(void);
static void sendMcdCmd(int cmd, char *params);


/*
 * No ioctl support yet
 */

static int mcd_ioctl(struct inode *ip, struct file *fp, unsigned int cmd,
						unsigned int arg)
{
	return -EIO;
}


/*
 * Take care of the different block sizes between cdrom and Linux.
 * When Linux gets variable block sizes this will probably go away.
 */

static void mcd_transfer(void)
{
	long offs;

	while (CURRENT -> nr_sectors > 0 && mcd_bn == CURRENT -> sector / 4)
	{
		offs = (CURRENT -> sector & 3) * 512;
		memcpy(CURRENT -> buffer, mcd_buf + offs, 512);
		CURRENT -> nr_sectors--;
		CURRENT -> sector++;
		CURRENT -> buffer += 512;
	}
}


/*
 * We only seem to get interrupts after an error.
 * Just take the interrupt and clear out the status reg.
 */

static void mcd_interrupt(int unused)
{
	int st;

	st = inb(MCDPORT(1)) & 0xFF;
	if (st != 0xFF)
	{
		st = inb(MCDPORT(0)) & 0xFF;
#if 0
		printk("<int-%02X>", st);
#endif
	}
}


/*
 * I/O request routine called from Linux kernel.
 */

static void do_mcd_request(void)
{
	unsigned int block,dev;
	unsigned int nsect;

repeat:
	INIT_REQUEST;
	dev = MINOR(CURRENT->dev);
	block = CURRENT->sector;
	nsect = CURRENT->nr_sectors;

	if (CURRENT -> cmd != READ)
	{
		printk("bad mcd cmd %d\n", CURRENT -> cmd);
		end_request(0);
		goto repeat;
	}

	mcd_transfer();

	/* if we satisfied the request from the buffer, we're done. */

	if (CURRENT -> nr_sectors == 0)
	{
		end_request(1);
		goto repeat;
	}

	McdTries = 3;
	mcd_start();
}


/*
 * Start the I/O for the cdrom. Handle retry count.
 */

static void mcd_start()
{
	if (McdTries == 0)
	{
		printk("mcd: failed after 3 tries\n");
		end_request(0);
		SET_TIMER(do_mcd_request, 1);	/* wait a bit, try again */
		return;
	}

	McdTries--;
	outb(0x40, MCDPORT(0));		/* get status */
	McdTimeout = 100;
	SET_TIMER(mcd_status, 1);
}


/*
 * Called from the timer to check the results of the get-status cmd.
 * On success, send the set-mode command.
 */

static void mcd_status()
{
	int st;

	McdTimeout--;
	st = mcdStatus();
	if (st == -1)
	{
		if (McdTimeout == 0)
		{
			printk("mcd: status timed out\n");
			SET_TIMER(mcd_start, 1);	/* wait a bit, try again */
			return;
		}

		SET_TIMER(mcd_status, 1);
		return;
	}

	if ((st & MST_DSK_IN) == 0)
	{
		printk("mcd: disk removed\n");
		end_request(0);
		do_mcd_request();
		return;
	}

	outb(0x50, MCDPORT(0));	/* set mode */
	outb(0x01, MCDPORT(0));	/* mode = cooked data */
	McdTimeout = 100;
	SET_TIMER(mcd_read_cmd, 1);
}


/*
 * Check the result of the set-mode command.  On success, send the
 * read-data command.
 */

static void mcd_read_cmd()
{
	int st;
	long block;
 	char mcdcmd[6];

	McdTimeout--;
	st = mcdStatus();
	if (st == -1)
	{
		if (McdTimeout == 0)
		{
			printk("mcd: set mode timed out\n");
			SET_TIMER(mcd_start, 1);	/* wait a bit, try again */
			return;
		}

		SET_TIMER(mcd_read_cmd, 1);
		return;
	}

	mcd_bn = -1;			/* purge our buffer */
	block = CURRENT -> sector / 4;
	hsg2msf(block, &mcdcmd[0]);	/* cvt to msf format */

	bin2bcd(&mcdcmd[0]);		/* convert to BCD */
	bin2bcd(&mcdcmd[1]);
	bin2bcd(&mcdcmd[2]);

	mcdcmd[3] = 0;
	mcdcmd[4] = 0;
	mcdcmd[5] = 1;

	sendMcdCmd(0xC0, mcdcmd);	/* read command */
	McdTimeout = 200;
	SET_TIMER(mcd_data, 1);
}


/*
 * Check the completion of the read-data command.  On success, read
 * the 2048 bytes of data from the disk into our buffer.
 */

static void mcd_data()
{
	int i;
	char *p;

	McdTimeout--;
	cli();
	i = inb(MCDPORT(1));
	if ((i & 0xF) == 0xB)
	{
		printk("got 0xB %02X\n", inb(MCDPORT(0)) & 0xFF);
		SET_TIMER(mcd_data, 1);
		sti();
		return;
	}
	
	if ((i & 0xF) == 0xF)
	{
		if (McdTimeout == 0)
		{
			printk("mcd: data timeout\n");
			SET_TIMER(mcd_start, 1);
		}
		
		else
			SET_TIMER(mcd_data, 1);
		
		sti();
		return;
	}

	CLEAR_TIMER;
	p = &mcd_buf[0];
	for (i = 0; i < 2048; i++)
		*p++ = inb(MCDPORT(0));

	sti();

	mcd_bn = CURRENT -> sector / 4;
	mcd_transfer();
	end_request(1);
	SET_TIMER(do_mcd_request, 1);
}


/*
 * Open the device special file.  Check that a disk is in.
 */

int mcd_open(struct inode *ip, struct file *fp)
{
	int i, st;

	if (mcdPresent == 0)
		return -ENXIO;			/* no hardware */

	outb(0x40, MCDPORT(0));			/* send get-status cmd */
	for (i = 0; i < 30000; i++)
		if ((inb(MCDPORT(1)) & 0xF) == 0xB)
			break;

	if (i >= 30000)
	{
		printk("MCD not responding\n");
		return -EIO;
	}

	st = inb(MCDPORT(0)) & 0xFF;
	if ((st & MST_DSK_IN) == 0)		/* no disk in drive */
		return -EIO;

	return 0;
}


/*
 * On close, we flush all mcd blocks from the buffer cache.
 */

static void mcd_release(struct inode * inode, struct file * file)
{
	mcd_bn = -1;
	sync_dev(inode->i_rdev);
	invalidate_buffers(inode -> i_rdev);
}


static struct file_operations mcd_fops = {
	NULL,			/* lseek - default */
	block_read,		/* read - general block-dev read */
	block_write,		/* write - general block-dev write */
	NULL,			/* readdir - bad */
	NULL,			/* select */
	mcd_ioctl,		/* ioctl */
	NULL,			/* mmap */
	mcd_open,		/* open */
	mcd_release		/* release */
};


/*
 * MCD interrupt descriptor
 */

static struct sigaction mcd_sigaction = {
	mcd_interrupt,
	0,
	SA_INTERRUPT,
	NULL
};


/*
 * Test for presence of drive and initialize it.  Called at boot time.
 */

void mcd_setup(int irq, int ioport)
{
	if (irq>=0)
		mcd_intr_nr = irq;
	if (ioport>=0)
		mcd_port = ioport;
}

void mcd_init()
{
	int count;
	if ((mcd_intr_nr<0) || (mcd_port<0))
		return;

	blk_dev[MAJOR_NR].request_fn = DEVICE_REQUEST;
	if (register_blkdev(MAJOR_NR, "mcd", &mcd_fops) != 0)
	{
		printk("Unable to get major %d for Mitsumi CD-ROM\n", MAJOR_NR);
		return;
	}

	read_ahead[MAJOR_NR] = 4;

	if (irqaction(MCD_INTR_NR,  &mcd_sigaction))
		printk("Unable to get IRQ%d for mcd driver\n", MCD_INTR_NR);

	/* check for card */

	printk("MCD - ");
	outb(0, MCDPORT(1));			/* send reset */
	for (count = 0; count < 1000000; count++)
		(void) inb(MCDPORT(1));		/* delay a bit */

	outb(0x40, MCDPORT(0));			/* send get-stat cmd */
	for (count = 0; count < 1000000; count++)
		if ((inb(MCDPORT(1)) & 0xF) == 0xB)
			break;

	if (count >= 1000000)
	{
		printk("No drive found\n");
		return;
	}

	count = inb(MCDPORT(0));		/* pick up the status */
	mcdPresent = 1;
	printk("Drive present\n");
}


static void hsg2msf(long hsg, char *msf)
{
	hsg += 150;
	msf[0] = hsg / 4500;
	hsg %= 4500;
	msf[1] = hsg / 75;
	msf[2] = hsg % 75;
}


static void bin2bcd(char *p)
{
	int u, t;

	u = *p % 10;
	t = *p / 10;
	*p = u | (t << 4);
}


static int mcdStatus(void)
{
	int i;
	int st;

	st = inb(MCDPORT(1)) & 0xF;
	if (st == 0xD)
	{
		return -1;
	}

	if (st == 0xB)
	{
		i = inb(MCDPORT(0)) & 0xFF;
		return i;
	}

	return -1;
}


static void sendMcdCmd(int cmd, char *params)
{
	int i;

	outb(cmd, MCDPORT(0));
	for (i = 0; i < 6; i++)
	{
		outb(*params, MCDPORT(0));
		params++;
	}
}

