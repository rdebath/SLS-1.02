#include <unistd.h>

int setegid(gid_t gid)
{
	return __setregid(-1, gid);
}
