#include <syscall.h>
#include <sys/socket.h>
#include <sys/socketcall.h>

static inline
_syscall2(long,socketcall,int,call,unsigned long *,args);

int
getsockopt (int fd, int level, int optname, void *optval, int *optlen)
{
	unsigned long args[5];
	args[0]=fd;
	args[1]=level;
	args[2]=optname;
	args[3]=(unsigned long)optval;
	args[4]=(unsigned long)optlen;
	return (socketcall (SYS_GETSOCKOPT, args));
}
