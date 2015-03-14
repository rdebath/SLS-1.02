/* well, better than nothing */

#include <unistd.h>

int fsync(int fd)
{
	return sync();
}

