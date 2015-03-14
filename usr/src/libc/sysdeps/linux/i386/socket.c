#include <sys/socket.h>
#include <syscall.h>
#include <sys/socketcall.h>

static inline
_syscall2(long,socketcall,int,call,unsigned long *,args);

int
socket(int family, int type, int protocol)
{
	unsigned long args[3];

	args[0] = family;
	args[1] = type;
	args[2] = protocol;
	return socketcall(SYS_SOCKET, args);
}
