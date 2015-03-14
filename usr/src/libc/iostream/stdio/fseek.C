#include <stdioprivate.h>
#include <errno.h>

int fseek(FILE* fp, long int offset, int whence)
{
  if (!__validfp(fp)) {
    errno = EBADF;
    return EOF;
  }
  return (((streambuf*)fp)->seekoff(offset, (_seek_dir)whence) == EOF)
	? EOF : 0;
}
