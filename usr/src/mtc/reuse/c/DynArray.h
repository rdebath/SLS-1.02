# ifndef yyDynArray
# define yyDynArray

/* $Id: DynArray.h,v 1.4 1992/08/07 14:36:51 grosch rel $ */

/* $Log: DynArray.h,v $
 * Revision 1.4  1992/08/07  14:36:51  grosch
 * added comments
 *
 * Revision 1.3  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.2  91/07/17  17:23:02  grosch
 * introduced ARGS trick for ANSI compatibility
 * 
 * Revision 1.1  90/07/04  14:33:52  grosch
 * introduced conditional include
 * 
 * Revision 1.0  88/10/04  11:44:36  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

# ifdef __STDC__
# define ARGS(parameters)	parameters
# else
# define ARGS(parameters)	()
# endif

extern void MakeArray    ARGS((char * * ArrayPtr, unsigned long * ElmtCount, unsigned long ElmtSize));
			/* 'ArrayPtr' is set to the start address of a	*/
			/* memory space to hold an array of 'ElmtCount' */
			/* elements each of size 'ElmtSize' bytes.	*/

extern void ExtendArray  ARGS((char * * ArrayPtr, unsigned long * ElmtCount, unsigned long ElmtSize));
			/* The memory space for the array is increased	*/
			/* by doubling the number of elements.		*/

extern void ReleaseArray ARGS((char * * ArrayPtr, unsigned long * ElmtCount, unsigned long ElmtSize));
			/* The memory space for the array is released.	*/

# endif
