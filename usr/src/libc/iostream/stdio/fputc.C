#include <stdioprivate.h>

extern "C" int fputc(int c, FILE *fp)
{
    if (!__validfp(fp)) {
	errno = EINVAL;
	return EOF;
    }
    return putc(c, fp);
}
