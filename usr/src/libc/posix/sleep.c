/* Copyright (C) 1991, 1992, 1993 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with the GNU C Library; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <ansidecl.h>
#include <signal.h>
#include <time.h>
#include <unistd.h>

/* SIGALRM signal handler for `sleep'.  This does nothing but return,
   but SIG_IGN isn't supposed to break `pause'.  */
static void
DEFUN(sleep_handler, (sig), int sig)
{
  return;
}

/* Make the process sleep for SECONDS seconds, or until a signal arrives
   and is not ignored.  The function returns the number of seconds less
   than SECONDS which it actually slept (zero if it slept the full time).
   If a signal handler does a `longjmp' or modifies the handling of the
   SIGALRM signal while inside `sleep' call, the handling of the SIGALRM
   signal afterwards is undefined.  There is no return value to indicate
   error, but if `sleep' returns SECONDS, it probably didn't work.  */
unsigned int
DEFUN(sleep, (seconds), unsigned int seconds)
{
  unsigned int remaining, slept, realslept;
  time_t before, after;
#ifdef _POSIX_SOURCE
  sigset_t set, oset;
  struct sigaction action, oldaction;
#else /* _POSIX_SOURCE */
#ifdef _BSD_SOURCE
  int oldmask;
#endif /* _BSD_SOURCE */
  __sighandler_t handler;
#endif /* _POSIX_SOURCE */

  if (!seconds) return 0;

  /* We want to block SIGALRM if possible. We
   * unblock it later. A SIGALRM may occur after
   * the singal handler has been installed but before
   * we pause.
   */
#ifdef _POSIX_SOURCE
  __sigemptyset (&set);
  __sigaddset (&set, SIGALRM);
  if (__sigprocmask (SIG_BLOCK, &set, &oset) < 0)
    return seconds;

  action.sa_handler = sleep_handler;
  __sigemptyset (&action.sa_mask);
  action.sa_flags = 0;
  if (__sigaction (SIGALRM, &action, &oldaction) < 0)
    return seconds;
#else /* _POSIX_SOURCE */

#ifdef _BSD_SOURCE
  if ((oldmask = __sigblock (SIGALRM)) < 0)
    return seconds;
#endif /* _BSD_SOURCE */

  handler = signal (SIGALRM, sleep_handler);
  if (handler == SIG_ERR)
    return seconds;
#endif /* _POSIX_SOURCE */

  before = time ((time_t *) NULL);

  /* return value is the time left for alarm if it was set before. if
   * it returns 0, what does that mean? we assume no alarm was set
   * before.
   */
  remaining = alarm (seconds);

  if (remaining && remaining < seconds) {
    /* What if the previous alarm is about to expire. We reinstall
     * the old handler and set previous alarm.
     */
#ifdef _POSIX_SOURCE
    (void) __sigaction (SIGALRM, &oldaction, (struct sigaction *) NULL);
    alarm (remaining);
    /* Unblock SIGALRM with old signal mask and pause. It'd better
     * not block it. */
    sigsuspend (&oset);
#else /* _POSIX_SOURCE */
    (void) signal (SIGALRM, handler);
    alarm (remaining);

#ifdef _BSD_SOURCE
    /* Unblock SIGALRM with old signal mask and pause. It'd better
     * not block it. */
    __sigpause (oldmask);
#else /* _BSD_SOURCE */
    (void) pause ();
#endif /* _BSD_SOURCE */

#endif /* _POSIX_SOURCE */
    after = time ((time_t *) NULL);
  }
  else {
#ifdef _POSIX_SOURCE
    /* Unblock SIGALRM with old signal mask and pause. It'd better
     * not block it. */
    sigsuspend (&oset);
    after = time ((time_t *) NULL);
    (void) __sigaction (SIGALRM, &oldaction, (struct sigaction *) NULL);
#else /* _POSIX_SOURCE */

#ifdef _BSD_SOURCE
    /* Unblock SIGALRM with old signal mask and pause. It'd better
     * not block it. */
    __sigpause (oldmask);
#else /* _BSD_SOURCE */
    (void) pause ();
#endif /* _BSD_SOURCE */

    after = time ((time_t *) NULL);
    (void) signal (SIGALRM, handler);
#endif /* _POSIX_SOURCE */
  }

  realslept = after - before;
  slept = realslept > seconds ? 0 : seconds - realslept;

  /* If we have called alarm () before which is scheduled to go
   * off after sleep (seconds), we should call alarm () again.
   * If sleep () is interrupted by a signal, we should turn off
   * the alarm (call alarm (0)).
   */
  alarm (realslept > remaining ? 0 : remaining - realslept);

  /* We have to restore the old mask since sigsuspend () will
   * restore the previous mask with SIGALRM mask. We have to do it
   * just before we leave. We should get another SIGALRM in sleep.c.
   * Am I wrong? H.J.
   */
#ifdef _POSIX_SOURCE

#ifdef __linux__
  __sigsetmask (oset);
#else /*__linux__ */
  __sigprocmask (SIG_SETMASK, &oset, (sigset_t *) NULL);
#endif /*__linux__ */

#else /* _POSIX_SOURCE */

#ifdef _BSD_SOURCE
  __sigsetmask (oldmask);
#endif /* _BSD_SOURCE */

#endif /* _POSIX_SOURCE */

  return slept;
}
