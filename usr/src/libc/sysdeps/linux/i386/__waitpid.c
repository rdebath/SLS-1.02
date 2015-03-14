#include <syscall.h>
#include <sys/wait.h>

static inline
_syscall4(pid_t,wait4,pid_t,pid,union wait *,status,int,options,struct rusage *,ru)

pid_t
__waitpid(pid_t pid, int *wait_stat, int options)
{
	return wait4(pid, (union wait *) wait_stat, options, NULL);
}
