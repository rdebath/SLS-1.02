# ifndef yyStringMem
# define yyStringMem

/* $Id: StringMem.h,v 1.6 1992/08/07 14:36:51 grosch rel $ */

/* $Log: StringMem.h,v $
 * Revision 1.6  1992/08/07  14:36:51  grosch
 * added comments
 *
 * Revision 1.5  1992/02/18  12:52:30  grosch
 * changed tString from unsigned char * to char *
 *
 * Revision 1.4  1992/01/30  13:12:51  grosch
 * complete redesign: pointer instead of array index
 *
 * Revision 1.3  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.2  91/07/17  17:23:48  grosch
 * introduced ARGS trick for ANSI compatibility
 * 
 * Revision 1.1  90/07/04  14:34:08  grosch
 * introduced conditional include
 * 
 * Revision 1.0  88/10/04  11:44:47  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

# include <stdio.h>
# include "ratc.h"

# ifdef __STDC__
# define ARGS(parameters)	parameters
# else
# define ARGS(parameters)	()
# endif

typedef unsigned short * tStringRef;

extern	tStringRef PutString	ARGS((register char * s, register cardinal length));
			/* Stores string 's' in the string memory and	*/
			/* returns a reference to the stored string.	*/

extern	void	StGetString	ARGS((register tStringRef r, register char * s));
			/* Returns the string 's' from the string	*/
			/* memory which is referenced by 'r'.		*/

/* extern cardinal LengthSt	ARGS((register tStringRef r)); */
# define LengthSt(stringref) (* stringref)
			/* Returns the length of the string 's'		*/
			/* which is referenced by 'r'.			*/

extern	bool	IsEqualSt	ARGS((tStringRef r, register char * s));
			/* Compares the string referenced by 'r' and	*/
			/* the string 's'.				*/
			/* Returns true if both are equal.		*/

extern	void	WriteString	ARGS((FILE * f, tStringRef r));
			/* The string referenced by 'r' is printed on	*/
			/* the file 'f'.				*/

extern	void	WriteStringMemory ();
			/* The contents of the string memory is printed	*/
			/* on standard output.				*/

extern	void	InitStringMemory ();
			/* The string memory is initialized.		*/

# endif
