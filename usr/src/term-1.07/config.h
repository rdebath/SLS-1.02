/*
 * USE_SETSID use setsid() instead of TCIONOCTTYY
 *
 * USE_TERMIOS uses the termios struct to control terminals.
 *   Comment this out if you want to use sgtty instead.
 * 
 * USE_FCNTL use fcntl() to set non blocking I/O as opposed to ioctl()'s.
 *
 * ERR_BLOCK sets the errno that is returned for an operation
 * that blocked.
 *
 * USE_VHANGUP says to run vhangup() on the new pty's.
 *
 * USE_TCATTR uses tc{get/set}attr() instead of ioctl's to set
 * termios.
 *
 * USE_SIGWINCH tells trsh to watch for SIGWINCH. Only define it if
 * your system supports it.
 *
 * USE_WINCHKILL if you sigwinch is broken, and doesn't send the
 * signal when the window size gets changed.
 *
 */

#ifdef linux
#define USE_SIGWINCH
#define USE_SETSID
#define USE_TERMIOS
#define USE_FCNTL
#define USE_NONBLOCK
#define USE_VHANGUP
#define ERR_BLOCK  EAGAIN
/* #define USE_BSDJOBS */
#endif

#ifdef SVR4
#ifndef SYSV
# define SYSV
#endif
#define USE_SIGWINCH
#define USE_SETSID
#define USE_TERMIOS
#define USE_FCNTL
#define USE_NONBLOCK
#define USE_TCATTR
#define ERR_BLOCK  EAGAIN
#endif

#ifdef ultrix
#define USE_SIGWINCH
#define USE_TIOCNOTTY
#define USE_VHANGUP
#define USE_TERMIOS
#define ERR_BLOCK EWOULDBLOCK
#endif

#ifdef NeXT
#define USE_VHANGUP
#define USE_FCNTL
#define USE_TCATTR
#define ERR_BLOCK EWOULDBLOCK
#define USE_SIGWINCH
#define USE_TIOCNOTTY
#endif

#ifndef SVR4
#if defined(sun) || defined(convex)
#define USE_SIGWINCH
#define USE_VHANGUP
#define USE_TERMIOS
#define USE_FCNTL
#define USE_NONBLOCK
#define USE_TCATTR
#define ERR_BLOCK EAGAIN
#define USE_SETPGRP
#ifdef sun
#define USE_WINCHKILL
#endif
#endif
#endif /* SVR4 was undefined */

#if defined(__hpux) 
#define USE_TERMIOS
#define USE_FCNTL
#define USE_NONBLOCK
#define USE_TCATTR
#define ERR_BLOCK EAGAIN
#define USE_SETSID
#endif

#ifdef _AIX
#define USE_SIGWINCH
#define USE_TERMIOS
#define USE_FCNTL
#define USE_NONBLOCK
#define USE_TCATTR
#define ERR_BLOCK EAGAIN
#define USE_SETPGRP
#endif

#ifdef sgi
#define	USE_SETSID
#define	USE_VHANGUP
#define	USE_TERMIOS
#define	USE_FCNTL
#define	USE_NONBLOCK
#define	USE_TCATTR
#define ERR_BLOCK EAGAIN
#define	USE_BSDJOBS
#define USE_SIGWINCH
#endif

#if defined(BSD) || defined(___386BSD___) || defined(__386BSD__)
#if !defined(___386BSD___) && !defined(__386BSD__)
#define USE_VHANGUP
#endif
#define USE_FCNTL
#define USE_TCATTR
#define ERR_BLOCK EWOULDBLOCK
#define USE_SETPGRP
#define USE_SIGWINCH
#endif
 
#if !defined(ERR_BLOCK) /* if no OS defined */

/* 
#error "Need to define an OS" 
Ultrix MIPS compiler chokes on this, even though ERR_BLOCK is defined!
*/
"Compilation directive: You need to define an OS"

/* If your OS isn't defined you need to work out which of the above defines */
/* you need and build and entry for it. Please send me the diff if you do so */
/* I am oreillym@tartarus.uwa.edu.au */
#endif

