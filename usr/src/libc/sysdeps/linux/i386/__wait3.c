#include <syscall.h>
#include <sys/wait.h>

static inline
_syscall4(pid_t,wait4,pid_t,pid,union wait *,status,int,options,struct rusage *,ru)

pid_t
__wait3(union wait *wait_stat, int options, struct rusage *reserved)
{
	return wait4(WAIT_ANY, wait_stat, options, reserved);
}
