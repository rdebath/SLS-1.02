#include <sys/dirent.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>
#include <syscall.h>
#include <errno.h>
#undef close
#undef stat(filename,stat_buf)

static inline
_syscall1(int,close,int,fd)

static inline
_syscall2(int,stat,const char *,filename,struct stat *,stat_buf)

/*
 * opendir just makes an open() call - it return NULL if it fails
 * (open sets errno), otherwise it returns a DIR * pointer.
 */
DIR * opendir(const char * name)
{
  int fd;
  struct stat statbuf;
  struct dirent *buf;
  DIR *ptr;

  if (stat(name,&statbuf)) return NULL;
  if (!S_ISDIR(statbuf.st_mode)) {
    errno = ENOTDIR;
    return NULL;
  }
  if ((fd = __open(name,O_RDONLY)) < 0)
    return NULL;
  /* According to POSIX, directory streams should be closed when
   * exec. From "Anna Pluzhnikov" <besp@midway.uchicago.edu>.
   */
  if (__fcntl(fd, F_SETFD, FD_CLOEXEC) < 0)
    return NULL;
  if (!(ptr=malloc(sizeof(*ptr)))) {
    close(fd);
    errno = ENOMEM;
    return NULL;
  }
  if (!(buf=malloc(NUMENT*sizeof (struct dirent)))) {
    close(fd);
    free(ptr);
    errno = ENOMEM;
    return NULL;
  }
  ptr->dd_fd = fd;
  ptr->dd_loc = ptr->dd_size = 0;
  ptr->dd_buf = buf;
  return ptr;
}
