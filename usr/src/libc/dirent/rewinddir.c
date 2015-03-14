#include <dirent.h>
#include <unistd.h>
#include <errno.h>
#include <syscall.h>
#undef lseek

static inline
 _syscall3(off_t,lseek,int,fildes,off_t,offset,int,origin)

/*
 * rewinddir() just does an lseek(fd,0,0) - see close for comments
 */
void
rewinddir(DIR * dir)
{
  if (!dir) {
    errno = EBADF;
    return;
  }
  lseek(dir->dd_fd,0,SEEK_SET);
}
