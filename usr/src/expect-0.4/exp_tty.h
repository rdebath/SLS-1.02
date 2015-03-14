/* exp_tty.h - tty support definitions */
/* Definitions for handling termio inclusion are localized here */

/* Written by: Don Libes, NIST, 2/6/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

#ifdef SYSV3
#define INCLUDE_TERMIO
#if defined(CRAY) && CRAY>=60
#undef INCLUDE_TERMIO
#define INCLUDE_TERMIOS
#endif
#endif

#ifdef POSIX
#include <termios.h>
#endif

#ifdef INCLUDE_TERMIO
#include <sys/termio.h>
#endif

#ifdef INCLUDE_TERMIOS
#include <sys/termios.h>
#endif

#ifndef TERM
#  ifdef POSIX
#    define TERM termios
#  endif
#endif

#ifndef TERM
#  ifdef SYSV3
#    ifdef HPUX
#      define TERM termio
#    else
#      define TERM termios
#    endif
#  else
#      define TERM sgttyb
#  endif
#endif

typedef struct TERM exp_tty;
extern exp_tty exp_tty_original;

extern int exp_dev_tty;
extern int ioctled_devtty;

void exp_init_tty();
void tty_raw();
void tty_set();
void tty_echo();
int tty_raw_noecho();
