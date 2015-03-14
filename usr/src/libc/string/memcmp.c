#include <string.h>

#ifdef isbcmp
#define	memcmp	bcmp
#define size_t	int
#endif

int
memcmp(const void * cs,const void * ct,size_t count)
{
  register int __res __asm__("ax");

#ifdef isbcmp
  if (count <= 0) return 0;
#endif

  __asm__("cld\n\t"
	"repe\n\t"
	"cmpsb\n\t"
	"je 1f\n\t"
	"movl $1,%%eax\n\t"
	"jb 1f\n\t"
	"negl %%eax\n"
	"1:"
	:"=a" (__res):"0" (0),"D" (cs),"S" (ct),"c" (count)
	:"si","di","cx");
  return __res;
}
