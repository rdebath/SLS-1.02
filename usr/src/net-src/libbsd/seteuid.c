#include <unistd.h>

int seteuid(uid_t uid)
{
	return setreuid(-1, uid);
}
