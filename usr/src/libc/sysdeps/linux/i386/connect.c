#include <syscall.h>
#include <sys/socket.h>
#include <sys/socketcall.h>

static inline
_syscall2(long,socketcall,int,call,unsigned long *,args);

int
connect(int sockfd, struct sockaddr *saddr, int addrlen)
{
	unsigned long args[3];

	args[0] = sockfd;
	args[1] = (unsigned long)saddr;
	args[2] = addrlen;
	return socketcall(SYS_CONNECT, args);
}
