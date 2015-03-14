#include <unistd.h>
#include <sys/syscall.h>
#include <errno.h>

extern void * ___brk_addr;

void *
__sbrk(ptrdiff_t increment)
{
	void * tmp = ___brk_addr+increment;
#if defined(__PIC__) || defined (__pic__)
	__asm__ volatile ("pushl %%ebx\n\t"
			  "movl %%ecx,%%ebx\n\t"
                          "int $0x80\n\t"
                          "popl %%ebx"
		:"=a" (___brk_addr)
		:"0" (SYS_brk),"c" (tmp));
#else
	__asm__ volatile ("int $0x80"
		:"=a" (___brk_addr)
		:"0" (SYS_brk),"b" (tmp));
#endif
	if (___brk_addr == tmp)
		return tmp-increment;
	errno = ENOMEM;
	return ((void *) -1);
}
