#include "stdioprivate.h"

extern "C" int fgetc(FILE *fp)
{
    if (!__validfp(fp)) {
	errno = EINVAL;
	return EOF;
    }
    return getc(fp);
}
