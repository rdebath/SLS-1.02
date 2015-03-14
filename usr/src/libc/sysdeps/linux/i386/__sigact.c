#include <syscall.h>
#include <signal.h>
#include <errno.h>

extern void ___sig_restore();
extern void ___masksig_restore();

int
__sigaction(int sig,struct sigaction * new, struct sigaction * old)
{
	if (new) {
		if (new->sa_flags & SA_NOMASK)
			new->sa_restorer=___sig_restore;
		else
			new->sa_restorer=___masksig_restore;
	}

#if defined(__PIC__) || defined (__pic__)
	__asm__ volatile ("pushl %%ebx\n\t"
			  "movl %%edi,%%ebx\n\t"
			  "int $0x80\n\t"
			  "popl %%ebx"
		:"=a" (sig)
		:"0" (SYS_sigaction),"D" (sig),"c" (new),"d" (old));
#else
	__asm__("int $0x80":"=a" (sig)
		:"0" (SYS_sigaction),"b" (sig),"c" (new),"d" (old));
#endif
	if (sig>=0)
		return 0;
	errno = -sig;
	return -1;
}
