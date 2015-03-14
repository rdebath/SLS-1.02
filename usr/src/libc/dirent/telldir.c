#include <dirent.h>
#include <unistd.h>
#include <syscall.h>
#include <errno.h>
#undef lseek

static inline
 _syscall3(off_t,lseek,int,fildes,off_t,offset,int,origin)

off_t telldir(DIR * dir)
{
  if (!dir) {
    errno = EBADF;
    return -1;
  }
  return lseek(dir->dd_fd,0,SEEK_CUR);
}
