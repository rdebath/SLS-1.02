/*
 *  linux/kernel/printk.c
 *
 *  Copyright (C) 1991, 1992  Linus Torvalds
 *
 * Modified to make sys_syslog() more flexible: added commands to
 * return the last 4k of kernel messages, regardless of whether
 * they've been read or not.  Added option to suppress kernel printk's
 * to the console.  Added hook for sending the console messages
 * elsewhere, in preparation for a serial line console (someday).
 * Ted Ts'o, 2/11/93.
 */

#include <stdarg.h>

#include <asm/segment.h>
#include <asm/system.h>

#include <linux/errno.h>
#include <linux/sched.h>
#include <linux/kernel.h>

static char buf[1024];

extern int vsprintf(char * buf, const char * fmt, va_list args);
extern void console_print(const char *);

static void (*console_print_proc)(const char *) = 0;
static char log_buf[4096];
static unsigned long log_start = 0;
static unsigned long logged_chars = 0;
unsigned long log_size = 0;
int log_to_console = 1;
struct wait_queue * log_wait = NULL;

/*
 * Commands to sys_syslog:
 *
 * 	0 -- Close the log.  Currently a NOP.
 * 	1 -- Open the log. Currently a NOP.
 * 	2 -- Read from the log.
 * 	3 -- Read up to the last 4k of messages in the ring buffer.
 * 	4 -- Read and clear last 4k of messages in the ring buffer
 * 	5 -- Clear ring buffer.
 * 	6 -- Disable printk's to console
 * 	7 -- Enable printk's to console
 */
int sys_syslog(int type, char * buf, int len)
{
	unsigned long i, j, count;
	int do_clear = 0;
	char c;

	if ((type != 3) && !suser())
		return -EPERM;
	switch (type) {
		case 0:		/* Close log */
			return 0;
		case 1:		/* Open log */
			return 0;
		case 2:		/* Read from log */
			if (!buf || len < 0)
				return -EINVAL;
			if (!len)
				return 0;
			verify_area(VERIFY_WRITE,buf,len);
			while (!log_size) {
				if (current->signal & ~current->blocked)
					return -ERESTARTSYS;
				cli();
				if (!log_size)
					interruptible_sleep_on(&log_wait);
				sti();
			}
			i = 0;
			while (log_size && i < len) {
				c = *((char *) log_buf+log_start);
				log_start++;
				log_size--;
				log_start &= 4095;
				put_fs_byte(c,buf);
				buf++;
				i++;
			}
			return i;
		case 4:		/* Read/clear last 4k of kernel messages */
			do_clear = 1; 
		case 3:		/* Read last 4k of kernel messages */
			if (!buf || len < 0)
				return -EINVAL;
			if (!len)
				return 0;
			verify_area(VERIFY_WRITE,buf,len);
			count = len;
			if (count > 4096)
				count = 4096;
			if (count > logged_chars)
				count = logged_chars;
			j = log_start + log_size - count;
			for (i = 0; i < count; i++) {
				c = *((char *) log_buf + (j++ & 4095));
				put_fs_byte(c, buf++);
			}
			if (do_clear)
				logged_chars = 0;
			return i;
		case 5:		/* Clear ring buffer */
			logged_chars = 0;
			return 0;
		case 6:		/* Disable logging to console */
			log_to_console = 0;
			return 0;
		case 7:		/* Enable logging to console */
			log_to_console = 1;
			return 0;
	}
	return -EINVAL;
}
			

int printk(const char *fmt, ...)
{
	va_list args;
	int i,j;

	va_start(args, fmt);
	i=vsprintf(buf,fmt,args);
	va_end(args);
	for (j = 0; j < i ; j++) {
		log_buf[(log_start+log_size) & 4095] = buf[j];
		if (log_size < 4096)
			log_size++;
		else
			log_start++;
		logged_chars++;
	}
	wake_up_interruptible(&log_wait);
	if (log_to_console && console_print_proc)
		(*console_print_proc)(buf);
	return i;
}

/*
 * The console driver calls this routine during kernel initialization
 * to register the console printing procedure with printk() and to
 * print any messages that were printed by the kernel before the
 * console priver was initialized.
 */
void register_console(void (*proc)(const char *))
{
	int	i,j;
	int	p = log_start;
	char	buf[16];

	console_print_proc = proc;

	for (i=0,j=0; i < log_size; i++) {
		buf[j++] = log_buf[p];
		p++; p &= 4095;
		if (j < sizeof(buf)-1)
			continue;
		buf[j] = 0;
		(*proc)(buf);
		j = 0;
	}
	buf[j] = 0;
	(*proc)(buf);
}
