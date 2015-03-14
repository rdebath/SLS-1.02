#include <errno.h>
#include <sys/resource.h>
#include <sys/syscall.h>

#define	PZERO	15

int
getpriority(int which, int who)
{
        long res;

#if defined(__PIC__) || defined (__pic__)
	__asm__ volatile ("pushl %%ebx\n\t"
			  "movl %%edx,%%ebx\n\t"
			  "int $0x80\n\t"
			  "popl %%ebx"
                :"=a" (res)
                :"0" (SYS_getpriority),"d" (which), "c" (who));
#else
        __asm__ volatile ("int $0x80"
                :"=a" (res)
                :"0" (SYS_getpriority),"b" (which), "c" (who));
#endif
        if (res >= 0) {
		errno = 0;
                return (int) PZERO - res;
        }
        errno = -res;
        return -1;
}
