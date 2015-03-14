/* sysdep.h -- common include file for the readline library */
/* Bruno Haible 19.3.1993 */


#if !(defined(__MSDOS__) || defined(__EMX__))

#include "config.h"

#else /* non-Unix systems can't execute the configure script */

#if defined(__EMX__)
#define STDC_HEADERS
#define HAVE_UNISTD_H
#define DIRENT
#define HAVE_TERMIO_H
#define HAVE_SYS_TERMIO_H
#define HAVE_SGTTY_H
#define HAVE_FIONREAD
#define NEED_SYS_IOCTL_H
#define HAVE_ALLOCA_H
#define HAVE_STRPBRK
#define RETSIGTYPE_VOID
#endif

#endif


/* For prototypes:  extern int foo RL((int x, int y)); */
#ifdef __STDC__
#define RL(args) args
#else
#define RL(args) ()
#endif

#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#ifndef alloca
extern void* alloca RL((int size));
#endif
#else
#ifdef _AIX
 #pragma alloca /* AIX requires this to be the first thing in the file. */
#else
extern void* alloca RL((int size)); /* either from libc.a or from alloca.o */
#endif /* _AIX */
#endif /* HAVE_ALLOCA_H */
#endif /* __GNUC__ */

#ifdef STDC_HEADERS
#include <stdlib.h> /* declares malloc(), realloc(), free(), getenv(), abort(), qsort() */
#endif
/* SCO systems may need "#include <malloc.h>" ?? */

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h> /* declares stat(), open(), read(), write(), close(),
                                fileno(), fcntl(), ioctl(),
                                kill (), getpid() */
#endif
#ifdef __EMX__
#include <io.h> /* declares stat(), open(), read(), write(), close(), ioctl() */
#endif

#include <string.h> /* declares strlen(), strcmp(), strncmp(), strcpy(), strncpy(), strcat()
                                and perhaps strchr(), strrchr(), strpbrk() */

#ifdef USG
/* <string.h> declares strchr(), strrchr() */
#define index strchr
#define rindex strrchr
#else
#include <strings.h> /* declares index(), rindex() */
#endif

/* Declaration of dirent, opendir(), readdir(), closedir() */
#if defined(DIRENT) || defined(_POSIX_VERSION)
#include <dirent.h>
typedef struct dirent dirent;
#else
#if defined(SVR3) || defined(__SVR3) || defined(SVR4) || defined(__SVR4) || defined(__svr4__) || defined(USG) || defined(hpux) || defined(__hpux)
#ifdef SYSNDIR
#include <sys/ndir.h>
#else
#include <ndir.h>
#endif
#else
#ifdef SYSDIR
#include <sys/dir.h>
#else
#include <dir.h>
#endif
#endif
typedef struct direct dirent;
#endif

/* storage class of functions:

                                on declaration    on definition

   local to the file                static           static
   globally visible                 extern
   either one, as you like          forward          usable
*/
#ifdef LIBRARY
/* Use this when compiling for a library: many functions globally visible. */
#define forward extern
#define usable
#else
/* Use this when compiling for inclusion into a single program: only the
   functions needed are globally visible, the others are `static' and may
   therefore be optimized away. */
#define forward static
#define usable static
#endif

