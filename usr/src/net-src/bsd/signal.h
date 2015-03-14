#include_next <signal.h>

/* make sure we get BSD style signals (that don't need to be reset) */

#define __USE_BSD_SIGNAL

/* use rough approximation of sigaction to sigvec, not completely safe! */

#define sigvec		sigaction
#define sv_mask		sa_mask
#define sv_handler	sa_handler
#define sv_onstack	sa_mask /* ouch, this one really hurts */

/* BSD uses some non-POSIX signals without ifdefs */

#define SIGSYS		SIGUNUSED
#define SIGBUS		SIGUNUSED

/* BSD wants this typedef for signal handlers */

#define sig_t		__sighandler_t 

