/* syscall.c - generalized linux system call interface - rick sladkey */

#include <stdarg.h>
#include <syscall.h>
#include <errno.h>

int
syscall(int number, ...)
{
	long res;
	int ebx, ecx, edx, esi, edi;
	va_list args;

	va_start(args, number);
	ebx = va_arg(args, int);
	ecx = va_arg(args, int);
	edx = va_arg(args, int);
	esi = va_arg(args, int);
	edi = va_arg(args, int);
	va_end(args);
	__asm__ volatile ("int $0x80"
		: "=a" (res)
		: "0" (number), "b" (ebx), "c" (ecx), "d" (edx),
			"S" (esi), "D" (edi));
	if (res < 0) {
		errno = -res;
		res = -1;
	}
	return res;
}
