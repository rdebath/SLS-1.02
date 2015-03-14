#include <signal.h>
#include <errno.h>

int
siginterrupt (int sig, int flag)
{
  struct sigaction sa;

  if (sig < 1 || sig >= NSIG) {
    errno = EINVAL;
    return -1;
  }

  if (__sigaction (sig, (struct sigaction *)0, &sa))
    return -1;
 
  if (flag) {
    if (sa.sa_flags & SA_INTERRUPT) return 0;
    sa.sa_flags |= SA_INTERRUPT;
  }
  else {
    if (!(sa.sa_flags & SA_INTERRUPT)) return 0;
    sa.sa_flags &= ~ SA_INTERRUPT;
  }
  return __sigaction (sig, &sa, (struct sigaction *)0);
}
