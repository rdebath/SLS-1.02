#include <syscall.h>
#include <sys/wait.h>

static inline
_syscall4(pid_t,wait4,pid_t,pid,union wait *,status,int,options,struct rusage *,ru)

pid_t
__wait(__WAIT_STATUS wait_stat)
{
	return wait4(WAIT_ANY, (union wait *) wait_stat, 0, NULL);
}
