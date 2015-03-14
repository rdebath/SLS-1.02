/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)usleep.c	26.8	91/09/14 SMI"

/*
 * usleep() compatibility function
 *
 * Under System V, implements usleep() using the interval timer.  Otherwise,
 * simply calls the library's usleep().  To use this implementation instead of
 * the library's, you must define SYSV.  If you want to this implementation
 * to use BSD-style signals, you must define SYS *and* USE_BSD_SIGNALS, even 
 * if you're not on System V.
 */

#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>

void olwm_do_nothing() { }

int
olwm_usleep(usec)
	unsigned int usec;
{
#ifdef SYSV
	struct itimerval	new, old;
#ifdef USE_BSD_SIGNALS
	struct sigvec		new_vec, old_vec;
#else
	struct sigaction	new_vec, old_vec;
#endif

	if (usec == 0)
	    return 0;

	new.it_interval.tv_sec = 0;
	new.it_interval.tv_usec = 0; /* We only want one tick */
	new.it_value.tv_sec = usec / 1000000;
	new.it_value.tv_usec = usec % 1000000;

#ifdef USE_BSD_SIGNALS
	new_vec.sv_handler = olwm_do_nothing;
	new_vec.sv_mask= 0;
	new_vec.sv_flags = 0;

	sigblock(sigmask(SIGALRM));
	sigvec(SIGALRM, &new_vec, &old_vec);
#else
	new_vec.sa_handler = olwm_do_nothing;
	new_vec.sa_mask.sigbits[0]= 0;
	new_vec.sa_mask.sigbits[1]= 0;
	new_vec.sa_mask.sigbits[2]= 0;
	new_vec.sa_mask.sigbits[3]= 0;
	new_vec.sa_flags = 0;

	sighold(SIGALRM);
	sigaction(SIGALRM, &new_vec, &old_vec);
#endif

	setitimer(ITIMER_REAL, &new, &old);

#ifdef USE_BSD_SIGNALS
	sigpause(0);
	sigvec(SIGALRM, &old_vec, (struct sigvec *)0);
#else
	sigpause(SIGALRM);
	sigaction(SIGALRM, &old_vec, (struct sigaction *)0);
#endif

	setitimer(ITIMER_REAL, &old, (struct itimerval *)0);
	return 0;

#else SYSV

	return usleep(usec);

#endif /* SYSV */
}
