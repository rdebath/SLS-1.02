#include <dirent.h>
#include <unistd.h>
#include <errno.h>
#include <syscall.h>
#undef lseek

static inline
 _syscall3(off_t,lseek,int,fildes,off_t,offset,int,origin)

void seekdir(DIR * dir, off_t offset)
{
  if (!dir) {
    errno = EBADF;
    return;
  }
  lseek(dir->dd_fd,offset,SEEK_SET);
}
