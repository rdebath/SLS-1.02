/*
Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)

Permission to use, copy, modify, and distribute this material 
for any purpose and without fee is hereby granted, provided 
that the above copyright notice and this permission notice 
appear in all copies, and that the name of Bellcore not be 
used in advertising or publicity pertaining to this 
material without the specific, prior written permission 
of an authorized representative of Bellcore.  BELLCORE 
MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
*/
/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/*
	ulstrcmp.c--compare strings ignoring alphabetic case.
*/

/* $Header: /afs/.andrew.cmu.edu/itc/sm/releases/X.V11R4/andrew/overhead/util/lib/RCS/ulstrcmp.c,v 2.6 89/08/24 12:08:25 cfe Exp $ */
/* $ACIS: $ */
/* $Source: /afs/.andrew.cmu.edu/itc/sm/releases/X.V11R4/andrew/overhead/util/lib/RCS/ulstrcmp.c,v $ */

#ifndef lint
static char *rcsid = "$Header: /afs/.andrew.cmu.edu/itc/sm/releases/X.V11R4/andrew/overhead/util/lib/RCS/ulstrcmp.c,v 2.6 89/08/24 12:08:25 cfe Exp $";
#endif /* lint */

#include <ctype.h>


int ULstrcmp(s1, s2)
register char *s1, *s2;
{
    /* case INSENSITIVE:  Compare strings:  s1>s2: >0  s1==s2: 0  s1<s2: <0
	  */
    register char c1,c2;

    for(;;) {
	c1 = *s1++; if (c1 <= 'Z') if (c1 >= 'A') c1 += 040;
	c2 = *s2++; if (c2 <= 'Z') if (c2 >= 'A') c2 += 040;
	if (c1 != c2) break;
	if (c1 == '\0') return(0);
    }
    return(c1 - c2);
}

#ifdef AMIGA
#ifndef isascii
#define isascii(c)      ((unsigned)(c)<=127)
#endif
#endif

#define DOWNCASE(x) (isascii(x) && isalpha(x) && isupper(x) ? (tolower(x)) : (x) )

int ULstrncmp(s1,s2,n)
char *s1, *s2;
int n;
{
  /* case INSENSITIVE:  Compare strings, up to n chars:  
     s1>s2: >0  s1==s2: 0  s1<s2: <0
   */

  register int i;
  register int result = 0;

  for(i = 0;(s1[i] || s2[i]) && i<n && !result;++i){
    result = DOWNCASE(s2[i]) - DOWNCASE(s1[i]);
  }
  return(result);
}

/* Just like strncpy but shift-case in transit and forces null termination */
/* Copied 8/24/89 from afs/util/casestrcpy.c to allow omission of lib/afs/util.a */

char *lcstring (d, s, n)
  char *d;				/* dest string */
  char *s;				/* source string */
  int   n;				/* max transfer size */
{   char *original_d = d;
    char  c;

    if ((s == 0) || (d == 0)) return 0;	/* just to be safe */
    while (n) {
	c = *s++;
	c = DOWNCASE(c);
	*d++ = c;
	if (c == 0) break;		/* quit after transferring null */
	if (--n == 0) *(d-1) = 0;	/* make sure null terminated */
    }
    return original_d;
}


#undef DOWNCASE

#ifdef TESTINGONLYTESTING
#include <stdio.h>
main(argc,argv)
int argc;
char *argv[];
{
  int result = 0;

  switch(argc) {
  case 3:
    result = ULstrcmp(argv[1], argv[2]);
    break;
  case 4:
    result = ULstrncmp(argv[1], argv[2], atoi(argv[3]));
    break;
  default:
    fprintf(stderr, "usage: ulstrcmp s1 s2 [n].\n");
    exit(1);
  }
  printf("'%s' %s '%s'.\n",
	 argv[1], (result == 0)?"==":(result<0)?"<":">", argv[2]);
}
#endif /* TESTINGONLYTESTING */
