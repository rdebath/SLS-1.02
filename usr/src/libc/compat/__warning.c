#include <stdio.h>

#ifdef USE_CONSOLE
#define OUT_FILE_PTR	fp
#else
#define	OUT_FILE_PTR	stderr
#endif	/* USE_CONSOLE */

void
__libc_warning (const char *name)
{
#if 0
#ifdef USE_CONSOLE
  static FILE *fp = NULL;

  if (!fp) fp = fopen ("/dev/console", "w");
  if (!fp) fp = stderr;
  else setbuf (fp, NULL);
#endif	/* USE_CONSOLE */

  fprintf (OUT_FILE_PTR,
	"Old version of C library function is used: `%s'.\n", name);
#endif
}
