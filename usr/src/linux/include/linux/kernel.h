#ifndef _LINUX_KERNEL_H
#define _LINUX_KERNEL_H

/*
 * 'kernel.h' contains some often-used function prototypes etc
 */

#define VERIFY_READ 0
#define VERIFY_WRITE 1

int verify_area(int type, void * addr, unsigned long count);

volatile void panic(const char * str);
volatile void do_exit(long error_code);
unsigned long simple_strtoul(const char *,char **,unsigned int);
int sprintf(char * buf, const char * fmt, ...);
int printk(const char * fmt, ...);

void * kmalloc(unsigned int size, int priority);
void kfree_s(void * obj, int size);

#define kfree(x) kfree_s((x), 0)

/*
 * This is defined as a macro, but at some point this might become a
 * real subroutine that sets a flag if it returns true (to do
 * BSD-style accounting where the process is flagged if it uses root
 * privs).  The implication of this is that you should do normal
 * permissions checks first, and check suser() last.
 */
#define suser() (current->euid == 0)

#define SI_LOAD_SHIFT	16
struct sysinfo {
	long uptime;			/* Seconds since boot */
	unsigned long loads[3];		/* 1, 5, and 15 minute load averages */
	unsigned long totalram;		/* Total usable main memory size */
	unsigned long freeram;		/* Available memory size */
	unsigned long sharedram;	/* Amount of shared memory */
	unsigned long bufferram;	/* Memory used by buffers */
	unsigned long totalswap;	/* Total swap space size */
	unsigned long freeswap;		/* swap space still available */
	unsigned short procs;		/* Number of current processes */
	char _f[22];			/* Pads structure to 64 bytes */
};

#endif
