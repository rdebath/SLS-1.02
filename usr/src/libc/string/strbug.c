#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <fcntl.h>

#ifndef HAVE_GNU_LD
#define _sys_nerr	sys_nerr
#define _sys_errlist	sys_errlist
#endif

#define	STREQ(a, b)	(strcmp((a), (b)) == 0)

/* Complain if condition is not true.  */
void
check (int thing, int number)
{
  if (!thing)
    {
      printf("strcmp: flunked test %d with result %d\n", number, thing);
    }
  else
      printf("strcmp: passed test %d with result %d\n", number, thing);
}

int
main ()
{
  /* Test strcmp first because we use it to test other things.  */
  check(strcmp("abcd", "abc"), 5);
  check(strcmp("abcd", "abc") > 0, 5);
}
