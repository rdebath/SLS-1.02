#include <syscall.h>
#include <sys/socket.h>
#include <sys/socketcall.h>

static inline
_syscall2(long,socketcall,int,call,unsigned long *,args);

/* send, sendto added by bir7@leland.stanford.edu */

int
sendto (int sockfd, const void *buffer, int len, unsigned flags,
	const struct sockaddr *to, int tolen)
{
  unsigned long args[6];
  args[0] = sockfd;
  args[1] = (unsigned long) buffer;
  args[2] = len;
  args[3] = flags;
  args[4] = (unsigned long) to;
  args[5] = tolen;
  return (socketcall (SYS_SENDTO, args));
}
