#ifndef _BSD_SIGNAL_H
#define _BSD_SIGNAL_H

/* make sure we get BSD style signals (that don't need to be reset) */

#ifndef __USE_BSD_SIGNAL
#define __USE_BSD_SIGNAL
#endif

#include_next <signal.h>

/* use rough approximation of sigaction to sigvec, not completely safe! */

#define sigvec		sigaction
#define sv_mask		sa_mask
#define sv_handler	sa_handler
#define sv_onstack	sa_flags /* ouch, this one really hurts */

#define SV_ONSTACK	0
#define SV_INTERRUPT	SA_INTERRUPT
#define SV_RESETHAND	SA_ONESHOT

/* BSD uses some non-POSIX signals without ifdefs */

#define SIGSYS		SIGUNUSED
#define SIGBUS		SIGUNUSED

/* BSD wants this typedef for signal handlers */

#define sig_t		__sighandler_t 

#endif /* _BSD_SIGNAL_H */
