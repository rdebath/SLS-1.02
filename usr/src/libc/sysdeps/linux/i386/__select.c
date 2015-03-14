#include <errno.h>
#include <sys/syscall.h>
#include <sys/time.h>

int
__select(int nd, fd_set * in, fd_set * out, fd_set * ex, 
	struct timeval * tv)
{
	long __res;
#if defined(__PIC__) || defined (__pic__)
	__asm__ volatile ("pushl %%ebx\n\t"
			  "movl %%ecx,%%ebx\n\t"
			  "int $0x80\n\t"
			  "popl %%ebx"
		: "=a" (__res)
		: "0" (SYS_select),"c" ((long) &nd));
#else
	__asm__ volatile ("int $0x80"
		: "=a" (__res)
		: "0" (SYS_select),"b" ((long) &nd));
#endif
	if (__res >= 0)
		return (int) __res;
	errno = -__res;
	return -1;
}
