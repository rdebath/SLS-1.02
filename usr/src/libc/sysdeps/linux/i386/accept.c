#include <syscall.h>
#include <sys/socket.h>
#include <sys/socketcall.h>

static inline
_syscall2(long,socketcall,int,call,unsigned long *,args);

int
accept(int sockfd, struct sockaddr *peer, int *paddrlen)
{
	unsigned long args[3];

	args[0] = sockfd;
	args[1] = (unsigned long)peer;
	args[2] = (unsigned long)paddrlen;
	return socketcall(SYS_ACCEPT, args);
}
