# ifndef yyratc
# define yyratc

/* $Id: ratc.h,v 1.4 1992/08/07 14:35:05 grosch rel $ */

/* $Log: ratc.h,v $
 * Revision 1.4  1992/08/07  14:35:05  grosch
 * changed tString from # define to typedef
 *
 * Revision 1.3  1992/02/18  12:52:30  grosch
 * changed tString from unsigned char * to char *
 *
 * Revision 1.2  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.1  90/07/04  14:34:13  grosch
 * introduced conditional include
 * 
 * Revision 1.0  88/10/04  11:44:54  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

# ifndef bool
# define bool		char
# endif
# ifndef true
# define true		1
# endif
# ifndef false
# define false		0
# endif
# ifndef cardinal
# define cardinal	/* unsigned */ short
# endif

typedef char * tString;

# endif
