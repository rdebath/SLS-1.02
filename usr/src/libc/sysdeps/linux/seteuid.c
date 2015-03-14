#include <unistd.h>

int seteuid(uid_t uid)
{
	return __setreuid(-1, uid);
}
