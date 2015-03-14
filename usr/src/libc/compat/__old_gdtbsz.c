#include <ansidecl.h>
#include <unistd.h>
#include "warning.h"

/* Return the maximum number of file descriptors
   the current process could possibly have. Very old one. */
int
DEFUN_VOID(____old_getdtablesize)
{
  __LIBC_WARNING (getdtablesize);

  /* Very old value. We have now 256. */
  return 32;
}
