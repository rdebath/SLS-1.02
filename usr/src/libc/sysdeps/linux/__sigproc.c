#include <sys/types.h>
#include <signal.h>
#include <errno.h>

int
__sigprocmask(int how, sigset_t * set, sigset_t * oset)
{
    sigset_t old;

    old = __siggetmask();

    if (set)
      switch(how) {
      case SIG_BLOCK:
	old = __sigsetmask((*set) | old);
	break;
      case SIG_UNBLOCK:
	old = __sigsetmask((~(*set)) & old);
	break;
      case SIG_SETMASK:
	old = __sigsetmask(*set);
	break;
      default:
	errno = EINVAL;
	return -1;
      }

    if (oset)
	*oset = old;

    return 0;
}
