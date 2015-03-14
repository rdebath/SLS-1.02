#include <signal.h>
#include <sys/syscall.h>

sigset_t
__sigsetmask (sigset_t mask)
{
	sigset_t __res;
#if defined(__PIC__) || defined (__pic__)
	__asm__ __volatile__ ("pushl %%ebx\n\t"
			  "movl %%ecx,%%ebx\n\t"
			  "int $0x80\n\t"
			  "popl %%ebx"
		: "=a" (__res)
		: "0" (SYS_sigsetmask),"c" ((long) mask));
#else
	__asm__ __volatile__ ("int $0x80"
		: "=a" (__res)
		: "0" (SYS_sigsetmask),"b" ((long) mask));
#endif
	return  __res;
}
