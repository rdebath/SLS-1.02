#include <unistd.h>
#include <sys/syscall.h>
#include <errno.h>

extern void * ___brk_addr;

int __brk(void * end_data_seg)
{
#if defined(__PIC__) || defined (__pic__)
	__asm__ volatile ("pushl %%ebx\n\t"
			  "movl %%ecx,%%ebx\n\t"
			  "int $0x80\n\t"
			  "popl %%ebx"
		:"=a" (___brk_addr)
		:"0" (SYS_brk),"c" (end_data_seg));
#else
	__asm__ volatile ("int $0x80"
		:"=a" (___brk_addr)
		:"0" (SYS_brk),"b" (end_data_seg));
#endif
	if (___brk_addr == end_data_seg)
		return 0;
	errno = ENOMEM;
	return -1;
}
