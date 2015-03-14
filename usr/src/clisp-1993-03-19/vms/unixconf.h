/* Bruno Haible 4.3.1993 */

#define UNIXCONF


/* header files */

/* AC_STDC_HEADERS */
/* Define if you have the ANSI C header files
   <stdlib.h>, <stdarg.h>, <string.h>, <limits.h>. */
#define STDC_HEADERS

/* AC_USG */
/* Define if you do not have <strings.h>, index(), bzero(), bcopy() etc.
   and must use strchr(), memset(), memcpy() etc. instead. */
#define USG

/* CL_NETDB */
/* Define if you have <netdb.h>. */
#define HAVE_NETDB_H


/* functions and declarations */

/* CL_ALLOCA */
/* Define if you need to link with an external alloca.o when using alloca(). */
#ifndef __GNUC__
#define NO_ALLOCA
#endif

/* CL_GETHOSTBYNAME */
/* Define if you have gethostbyname() and either <netdb.h> or <sun/netdb.h>. */
#define HAVE_GETHOSTBYNAME


/* compiler characteristics */

/* CL_MACHINE */
/* see machine.h */

