#include <syscall.h>
#include <sys/socket.h>
#include <sys/socketcall.h>

static inline
_syscall2(long,socketcall,int,call,unsigned long *,args);

/* shutdown by bir7@leland.stanford.edu */
int
shutdown (int sockfd, int how)
{
  unsigned long args[2];
  args[0] = sockfd;
  args[1] = how;
  return (socketcall (SYS_SHUTDOWN, args));
}
