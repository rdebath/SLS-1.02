#include <unistd.h>
#include <sys/time.h>

int utimes(char *path, struct timeval *tvp)
{
	struct utimbuf buf, *times;

	if (tvp) {
		times = &buf;
		times->actime = tvp[0].tv_sec;
		times->modtime = tvp[1].tv_sec;
	}
	else
		times = NULL;
	return utime(path, times);
}
