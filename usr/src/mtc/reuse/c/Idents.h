# ifndef yyIdents
# define yyIdents

/* $Id: Idents.h,v 1.8 1992/08/07 14:36:51 grosch rel $ */

/* $Log: Idents.h,v $
 * Revision 1.8  1992/08/07  14:36:51  grosch
 * added comments
 *
 * Revision 1.7  1992/02/18  12:52:30  grosch
 * changed tString from unsigned char * to char *
 *
 * Revision 1.6  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.5  91/09/18  15:18:47  grosch
 * added procedure GetStringRef
 * 
 * Revision 1.4  91/07/17  17:23:08  grosch
 * introduced ARGS trick for ANSI compatibility
 * 
 * Revision 1.3  91/01/21  12:13:21  grosch
 * some performance improvements
 * 
 * Revision 1.2  90/07/04  14:33:56  grosch
 * introduced conditional include
 * 
 * Revision 1.1  89/12/08  17:22:12  grosch
 * added variable NoIdent
 * 
 * Revision 1.0  88/10/04  11:44:39  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

# include <stdio.h>
# include "ratc.h"
# include "StringMem.h"

# ifdef __STDC__
# define ARGS(parameters)	parameters
# else
# define ARGS(parameters)	()
# endif

typedef cardinal	tIdent;

extern	tIdent	NoIdent; /* A default identifer (empty string)		*/

extern	tIdent	MakeIdent	ARGS((register char * string, register cardinal length));
			/* The string (of length) is mapped to a unique	*/
			/* identifier (an integer) which is returned.	*/

extern	void	GetString	ARGS((tIdent ident, char * string));
			/* Returns the string whose identifier is 'ident'.*/

extern	tStringRef GetStringRef ARGS((tIdent ident));
			/* Returns a reference to the string identified	*/
			/* by 'ident'.					*/

extern	tIdent	MaxIdent	();
			/* Returns the currently maximal identifier.	*/

extern	void	WriteIdent	ARGS((FILE * file, tIdent ident));
			/* The string encoded by the identifier 'ident'	*/
			/* is printed on the file.			*/

extern	void	WriteIdents	();
			/* The contents of the identifier table is	*/
			/* printed on the standard output.		*/

extern	void	InitIdents	();
			/* The identifier table is initialized.		*/

extern	void	WriteHashTable	();

# endif
