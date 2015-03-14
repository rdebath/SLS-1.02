#include <stdlib.h>
#include <dirent.h>
#include <unistd.h>
#include <errno.h>
#include <syscall.h>

static inline
_syscall1(int,close,int,fd)

int closedir(DIR * dir)
{
  int fd;

  if (!dir) {
    errno = EBADF;
    return -1;
  }
  fd = dir->dd_fd;
  free(dir->dd_buf);
  free(dir);
  return close(fd);
}
