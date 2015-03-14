#include <unistd.h>

/* emulib.h by Harry Pulley, IV; 18DEC92.  Written for Coherent 4.0 to emulate
   functions, etc. so that it may compile Linux MGR code. */

#ifndef EMULIB
#define EMULIB 1
/* link <signal.h> to <sys/signal.h> */
/* link <termio.h> <termios.h> */
/* touch <sys/wait.h> */
/* touch <sys/file.h> */
/* /etc/utmp is quite system dependant so it is best just to not define WHO for */
/* now. */
/* Try changing VMIN to 1 and VTIME to 0 in set_mouseio in set_mode.c - if this */
/* works then we can maybe change this permanently? */
/* set_size in put_window.c tries to return -1 from a void - this is a bug. */
/* Set MAXSHELL in defs.h to 127 - this is a bug. */
/* In get_menus.c, HIGH is redifined (originally defined by window.h, which is */
/* brought in by bitblit.h */
/* In bitmaptoc.c, the printf 0%3 should be 0%03 instead so that leading zeroes */
/* are used instead of blanks; otherwise 0 is converted to 0  0 instead of 0000 */
/* In intersect.c, HIGH and WIDE are redifined */
/* In mgr makefile, add extra \ in front of \n characters */
/* In shape.c, FSIZE is redefined */

typedef int pid_t;
typedef int gid_t;

struct winsize { int ws_row, ws_col, ws_xpixel, ws_ypixel; };

#include <sys/param.h>
#include <sys/utsname.h>
#include <path.h>

#define _POSIX_SOURCE /* not true but... */

#define EWOULDBLOCK EAGAIN

#define SIGCONT 999
#define SIGTTIN 998
#define SIGTTOU 997

#define _POSIX_PATH_MAX MAXPATH

#define TIOCSWINSZ -1

#define bzero(x,y) memset(x,0,y)
#define bcmp(x,y,z) memcmp(x,y,z)
#define bcopy(x,y,z) memcpy(y,x,z)

#define random rand
#define srandom srand

#define wait3(x,y,z) wait(x)
#define setreuid(x,y) setuid(x)
#define setregid(x,y) setgid(x)

#define gethostname(x,y) \
{ \
struct utsname sysname; \
uname(&sysname); \
strcpy(x,sysname.sysname); \
}

#define killpg kill

#define vfork fork

#define getdtablesize() NOFILE

/* we need a fchown or */
#define fchown(x,y,z)

/* we need a fchmod or */
#define fchmod(x,y) 

/* we don't have session ids so */
#define setsid()

/* we need a ftruncate or */
#define ftruncate(x,y)

#define poll Poll

#define void char

#define S_ISREG(x) (x&S_IFREG)
#define S_ISDIR(x) (x&S_IFDIR)

#endif
