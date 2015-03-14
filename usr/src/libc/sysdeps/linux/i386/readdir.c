#include <dirent.h>
#include <errno.h>
#include <sys/syscall.h>

/*
 * readdir fills up the buffer with the readdir system call. it also
 * gives a third parameter (currently ignored, but should be 1) that
 * can with a future kernel be enhanced to be the number of entries
 * to be gotten.
 *
 * Right now the readdir system call return the number of characters
 * in the name - in the future it will probably return the number of
 * entries gotten. No matter - right now we just check for positive:
 * that will always work (as we know that it cannot be bigger than 1
 * in the future: we just asked for one entry).
 */
struct dirent *readdir(DIR * dir)
{
  int result;
  int count = NUMENT;

  if (!dir) {
    errno = EBADF;
    return NULL; 
  }

  if (dir->dd_size <= dir->dd_loc) {
    /* read count of directory entries. For now it should be one. */
#if defined(__PIC__) || defined (__pic__)
	__asm__ volatile ("pushl %%ebx\n\t"
			  "movl %%esi,%%ebx\n\t"
			  "int $0x80\n\t"
			  "popl %%ebx"
	:"=a" (result)
	:"0" (SYS_readdir),"S" (dir->dd_fd),
	"c" ((long) dir->dd_buf),"d" (count));
#else
    __asm__("int $0x80"
	:"=a" (result)
	:"0" (SYS_readdir),"b" (dir->dd_fd),
	"c" ((long) dir->dd_buf),"d" (count));
#endif
    if (result <= 0) {
      if (result < 0)
	errno = -result;
      return NULL;
    }

    /*
     * Right now the readdir system call return the number of
     * characters in the name - in the future it will probably return
     * the number of entries gotten. No matter - right now we just
     * check for positive:
     */
#if 0
    dir->dd_size = result;
#else
    dir->dd_size = 1;
#endif

    dir->dd_loc = 0;
  }

  return &(dir->dd_buf [(dir->dd_loc)++]);
}
