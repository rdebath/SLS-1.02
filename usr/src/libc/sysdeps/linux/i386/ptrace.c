#include <errno.h>
#include <sys/ptrace.h>
#include <sys/syscall.h>

int
ptrace(int request, int pid, int addr, int data)
{
	long ret;
	long res;
	if (request > 0 && request < 4) (long *)data = &ret;

#if defined(__PIC__) || defined (__pic__)
	__asm__ volatile ("pushl %%ebx\n\t"
			  "movl %%edi,%%ebx\n\t"
			  "int $0x80\n\t"
			  "popl %%ebx"
		:"=a" (res)
		:"0" (SYS_ptrace),"D" (request), "c" (pid),
		 "d" (addr), "S" (data));
#else
	__asm__ volatile ("int $0x80"
		:"=a" (res)
		:"0" (SYS_ptrace),"b" (request), "c" (pid),
		 "d" (addr), "S" (data));
#endif

	if (res >= 0) {
		if (request > 0 && request < 4) {
			errno = 0;
			return (ret);
		}
		return (int) res;
	}
	errno = -res;
	return -1;
}
