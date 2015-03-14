#include "stdioprivate.h"
#include "errno.h"

#undef putw

int putw(int w, FILE *fp)
{
#if 0
    const void *buf = (const void *)&w;
#else
    const char *buf = (const char *)&w;
#endif
    streambuf* sb = FILE_to_streambuf(fp);
    if (!sb) {
	errno = EBADF;
	return 0;
    }
    size_t request = sizeof(w);
    size_t written = sb->sputn(buf, request);
    return (written == request) ? 0 : EOF;
}
