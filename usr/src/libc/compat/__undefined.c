#include <stdio.h>
#include <stdlib.h>

#ifdef USE_CONSOLE
#define OUT_FILE_PTR	fp
#else
#define	OUT_FILE_PTR	stderr
#endif	/* USE_CONSOLE */

void
__libc_undefined ()
{
#ifdef USE_CONSOLE
  static FILE *fp = NULL;

  if (!fp) fp = fopen ("/dev/console", "w");
  if (!fp) fp = stderr;
  else setbuf (fp, NULL);
#endif	/* USE_CONSOLE */

  fprintf (OUT_FILE_PTR, "Undefined C library functions:\n");
  fprintf (OUT_FILE_PTR,
	"\t1. light C shared image (Use the real one instead.)\n");
#if 0
\t2. libnet.a (Relink with -lnet.)
\t3. librpc.a (Relink with -lrpc.)
#endif

  exit (-1);
}
