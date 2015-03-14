#include <syscall.h>
#include <unistd.h>

static inline
_syscall2(int,setpgid,pid_t,pid,pid_t,pgid)

int
setpgrp(void)
{
	return setpgid(0,0);
}
