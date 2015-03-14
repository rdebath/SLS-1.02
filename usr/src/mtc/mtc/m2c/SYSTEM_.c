/* $Id: SYSTEM_.c,v 1.4 1991/11/21 16:57:59 grosch rel grosch $ */

#include <stdio.h>

#include "SYSTEM_.h"

/*
 *	Implementation of standard functions
 */

LONGINT ABSLI
# ifdef __STDC__
(register LONGINT x)
# else
(x) register LONGINT x;
# endif
{
	return (x < 0 ? -x : x);
}

LONGREAL ABSLR
# ifdef __STDC__
(register LONGREAL x)
# else
(x) register LONGREAL x;
# endif
{
	return (x < 0 ? -x : x);
}

CHAR CAP
# ifdef __STDC__
(register CHAR ch)
# else
(ch) register CHAR ch;
# endif
{
    return (ch >= 'a' && ch <= 'z' ? ch - 'a' + 'A' : ch);
}

/*
 *	Implementation of set operators
 */

unsigned long SET_RANGE1
# ifdef __STDC__
(register CARDINAL lo, register CARDINAL hi)
# else
(lo, hi) register CARDINAL lo, hi;
# endif
{
    return (lo <= hi ? ~0X0L >> lo << lo + SYSTEM_MaxSet - hi >> SYSTEM_MaxSet - hi : 0X0L);
}

/*
 *	Implementation of compiler functions
 */

void CaseError
# ifdef __STDC__
(char file[], int line)
# else
(file, line) char file[]; int line;
# endif
{
    (void) fprintf (stderr, "\"%s\", line %1d: case expression out of range\n", file, line);
    exit(1);
}

void ReturnError
# ifdef __STDC__
(char file[], int line)
# else
(file, line) char file[]; int line;
# endif
{
    (void) fprintf (stderr, "\"%s\", line %1d: missing return from function\n", file, line);
    exit(1);
}

/*
 *	Main program
 */

extern void BEGIN_MODULE();

int    SYSTEM_argc;
char **SYSTEM_argv;
char **SYSTEM_envp;

main(argc, argv, envp)
int argc;
char *argv[], *envp[];
{
  SYSTEM_argc = argc;
  SYSTEM_argv = argv;
  SYSTEM_envp = envp;

  BEGIN_MODULE();

  exit(0);
}
