/* Configuration file for 16-bit systems */

#ifndef _Iconfig
#define _Iconfig 1

/* Integer size quantities
 * MAXINT Maximum signed integer
 * ISIZ   Number of chars in an int
 * SHFT   LOG2 of ISIZ
 */

#define MAXINT 0x7FFF
#define ISIZ 2
#define SHFT 1

/* Support for segmented systems
 * physical(addr)   Return a linear address given a pointer
 * normalize(addr)  Normalize a pointer so that the offset part is minimized
 */

#define physical(a) ((long)(a))
#define normalize(a) (a)

#define BITS 8			/* Number of bits in a char */
#define MAXLONG 0x7FFFFFFF

/* Uncomment the following line if your compiler has trouble with void */
/* #define void int */

/* NULL should only be used for data-pointers, not function pointers,
 * (because of medium model MSDOS)
 */
#ifndef NULL
#define NULL ((void *)0)
#endif

/* These are for optimizing blocks.c */
/* #define AUTOINC */	/* Define this if CPU can autoincrement faster than
			   it can do [reg+offset] addressing */
/* #define ALIGNED */	/* Define this if CPU can access unaligned ints */
			/* (tries to align ints even if defined) */

/* System calls we use */
char *getenv();
char *getcwd();
long time();
/*
int chdir();
int creat();
int open();
int close();
int read();
int write();
int lseek();
*/

#endif
