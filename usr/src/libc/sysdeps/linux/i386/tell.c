#include <unistd.h>
#include <syscall.h>

static inline
_syscall3(off_t,lseek,int,fildes,off_t,offset,int,origin)

off_t
tell (int fildes)
{
  return __lseek (fildes, 0, SEEK_CUR);
}
