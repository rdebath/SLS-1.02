# ifndef yyMemory
# define yyMemory

/* $Id: Memory.h,v 1.5 1992/08/07 14:36:51 grosch rel $ */

/* $Log: Memory.h,v $
 * Revision 1.5  1992/08/07  14:36:51  grosch
 * added comments
 *
 * Revision 1.4  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.3  91/07/17  17:23:14  grosch
 * introduced ARGS trick for ANSI compatibility
 * 
 * Revision 1.2  90/12/14  15:55:53  grosch
 * introduced variable MemoryUsed
 * 
 * Revision 1.1  90/07/04  14:34:00  grosch
 * introduced conditional include
 * 
 * Revision 1.0  88/10/04  11:44:42  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

# ifdef __STDC__
# define ARGS(parameters)	parameters
# else
# define ARGS(parameters)	()
# endif

extern unsigned long MemoryUsed	;
			/* Holds the total amount of memory managed by	*/
			/* this module.					*/

extern void	InitMemory	();
			/* The memory module is initialized.		*/

extern char *	Alloc		ARGS((register unsigned long ByteCount));
			/* Returns a pointer to dynamically allocated	*/
			/* space of size 'ByteCount' bytes.		*/

extern void	Free		ARGS((unsigned long ByteCount, char * a));
			/* The dynamically allocated space starting at	*/
			/* address 'a' of size 'ByteCount' bytes is	*/
			/* released.					*/

# endif
