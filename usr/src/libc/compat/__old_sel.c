#include <sys/time.h>
#include "warning.h"

int
__old_select (int nd, fd_set * in, fd_set * out, fd_set * ex,
	struct timeval * tv)
{
  __LIBC_WARNING (select);

  if (nd > 32)
	nd = 32;
  return __select (nd, in, out, ex, tv);
}
