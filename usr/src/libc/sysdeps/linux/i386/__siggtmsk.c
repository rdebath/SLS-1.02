#include <signal.h>
#include <sys/syscall.h>

sigset_t
__siggetmask (void)
{
	sigset_t __res;
	__asm__ __volatile__ ("int $0x80"
		: "=a" (__res)
		: "0" (SYS_siggetmask));
	return  __res;
}
