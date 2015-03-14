#include <fcntl.h>
#include <errno.h>
#include <sys/syscall.h>
#include <stdarg.h>

int
__open(const char * filename, int flag, ...)
{
	register int res;
	va_list arg;

	va_start(arg,flag);
	__asm__("int $0x80"
		:"=a" (res)
		:"0" (SYS_open),"b" (filename),"c" (flag),
		"d" (va_arg(arg,int)));
	if (res>=0)
		return res;
	errno = -res;
	va_end(arg);
	return -1;
}
