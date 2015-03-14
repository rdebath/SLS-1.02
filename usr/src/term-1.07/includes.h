
/*
**
*/

#include "term.h"

#ifdef I_ERRNO
#include <errno.h>
#endif

#ifdef I_SYS
#include <sys/time.h>
#include <sys/types.h>
#ifdef _AIX
#include <sys/select.h>
#endif
#ifdef SVR4
int gettimeofday(struct timeval *);
int select(int, fd_set *, fd_set *, fd_set *, struct timeval *);
#endif
#endif

#ifdef I_IOCTL
#include <sys/ioctl.h>
#include <fcntl.h>
#endif

#ifdef I_STRING
#include <string.h>
#endif

#ifdef I_SOCKET
#include <sys/types.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/socket.h>
#ifdef SYSV
#include <sys/utsname.h>
#endif
#ifdef SVR4
int accept(int, struct sockaddr *, int *);
int bind(int, struct sockaddr *, int);
int connect(int, struct sockaddr *, int);
int listen(int, int);
int socket(int, int, int);
int socketpair(int, int, int, int[]);
#endif
#endif

#ifdef I_GETOPT
#if defined(linux)
#include <getopt.h>
#elif defined(__hpux) || defined(__386BSD__) || defined(___386BSD___) || defined(SVR4)
#ifdef __hpux__
#include <unistd.h>
extern char *optarg;
extern int optind, opterr;
#endif
/* Do nothing -- declared in stdlib.h */
#else
int getopt(int argc, char *argv[], char *);
extern char *optarg;
extern int optind, opterr;
#endif
#endif

#ifdef I_TTY

# ifdef USE_TERMIOS
#  include <termios.h>
# else
#  include <sgtty.h>
# endif

#endif

#include <stdio.h>
#include <stdlib.h>
#if !defined(NeXT)
#include <unistd.h>
#endif

