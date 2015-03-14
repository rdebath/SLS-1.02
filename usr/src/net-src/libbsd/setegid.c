#include <unistd.h>

int setegid(gid_t gid)
{
	return setregid(-1, gid);
}
