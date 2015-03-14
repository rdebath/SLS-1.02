#include <syscall.h>
#include <sys/socket.h>
#include <sys/socketcall.h>

static inline
_syscall2(long,socketcall,int,call,unsigned long *,args);

/* [sg]etsockoptions by bir7@leland.stanford.edu */
int
setsockopt (int fd, int level, int optname, const void *optval,
	int optlen)
{
	unsigned long args[5];
	args[0]=fd;
	args[1]=level;
	args[2]=optname;
	args[3]=(unsigned long)optval;
	args[4]=optlen;
	return (socketcall (SYS_SETSOCKOPT, args));
}
