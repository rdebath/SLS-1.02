#include <string.h>

int strncmp(const char * cs,const char * ct,size_t count)
{
register int __res __asm__("ax");
__asm__("cld\n\t"
	"incl %3\n"
	"1:\tdecl %3\n\t"
	"je 2f\n\t"
	"lodsb\n\t"
	"scasb\n\t"
	"jne 3f\n\t"
	"testb %%al,%%al\n\t"
	"jne 1b\n"
	"2:\txorl %%eax,%%eax\n\t"
	"jmp 4f\n"
	"3:\tmovl $1,%%eax\n\t"
	"jb 4f\n\t"
	"negl %%eax\n"
	"4:"
	:"=a" (__res):"D" (cs),"S" (ct),"c" (count):"si","di","cx");
return __res;
}
