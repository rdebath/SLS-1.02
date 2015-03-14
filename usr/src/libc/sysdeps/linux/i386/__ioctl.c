#include <errno.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <stdarg.h>

int
__ioctl(int fildes, int cmd, ...)
{
	register int res;
	va_list arg;

	va_start(arg,cmd);
	__asm__("int $0x80"
		:"=a" (res)
		:"0" (SYS_ioctl),"b" (fildes),"c" (cmd),
		"d" (va_arg(arg,int)));
	if (res>=0)
		return res;
	errno = -res;
	va_end(arg);
	return -1;
}
