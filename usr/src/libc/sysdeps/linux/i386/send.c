#include <syscall.h>
#include <sys/socket.h>
#include <sys/socketcall.h>

static inline
_syscall2(long,socketcall,int,call,unsigned long *,args);

/* send, sendto added by bir7@leland.stanford.edu */

int
send (int sockfd, const void *buffer, int len, unsigned flags)
{
  unsigned long args[4];
  args[0] = sockfd;
  args[1] = (unsigned long) buffer;
  args[2] = len;
  args[3] = flags;
  return (socketcall (SYS_SEND, args));
}
