/*

 Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as all of the original files are included
 unmodified, that it is not sold for profit, and that this copyright notice
 is retained.

*/

/*
 *  util.c by Mark Adler.
 */

#include "zip.h"

/* Local functions */
#ifdef PROTO
  local int recmatch(char *, char *);
#endif /* PROTO */


char *isshexp(p)
char *p;                /* candidate sh expression */
/* If p is a sh expression, a pointer to the first special character is
   returned.  Otherwise, NULL is returned. */
{
  int c;

  c = -1;
  for (; *p; p++)
#ifdef VMS
    if (c != '\\' && (*p == '%' || *p == '*'))
#else /* !VMS */
    if (c != '\\' && (*p == '?' || *p == '*' || *p == '['))
#endif /* ?VMS */
      return p;
  return NULL;
}


local int recmatch(p, s)
char *p;                /* sh pattern to match */
char *s;                /* string to match it to */
/* Recursively compare the sh pattern p with the string s and return 1 if
   they match, and 0 or 2 if they don't or if there is a syntax error in the
   pattern.  This routine recurses on itself no deeper than the number of
   characters in the pattern. */
{
  int c;                /* pattern char or start of range in [-] loop */ 

  /* Get first character, the pattern for new recmatch calls follows */
  c = *p++;

  /* If that was the end of the pattern, match if string empty too */
  if (c == 0)
    return *s == 0;

  /* '?' (or '%') matches any character (but not an empty string) */
#ifdef VMS
  if (c == '%')
#else /* !VMS */
  if (c == '?')
#endif /* ?VMS */
    return *s ? recmatch(p, s + 1) : 0;

  /* '*' matches any number of characters, including zero */
  if (c == '*')
  {
    if (*p == 0)
      return 1;
    for (; *s; s++)
      if ((c = recmatch(p, s)) != 0)
        return c;
    return 2;           /* 2 means give up--shmatch will return false */
  }

#ifndef VMS             /* No bracket matching in VMS */
  /* Parse and process the list of characters and ranges in brackets */
  if (c == '[')
  {
    int e;              /* flag true if next char to be taken literally */
    char *q;            /* pointer to end of [-] group */
    int r;              /* flag true to match anything but the range */

    if (*s == 0)                        /* need a character to match */
      return 0;
    p += (r = *p == '!');               /* see if reverse */
    for (q = p, e = 0; *q; q++)         /* find closing bracket */
      if (e)
        e = 0;
      else
        if (*q == '\\')
          e = 1;
        else if (*q == ']')
          break;
    if (*q != ']')                      /* nothing matches if bad syntax */
      return 0;
    for (c = 0, e = *p == '-'; p < q; p++)      /* go through the list */
    {
      if (e == 0 && *p == '\\')         /* set escape flag if \ */
        e = 1;
      else if (e == 0 && *p == '-')     /* set start of range if - */
        c = *(p-1);
      else
      {
        if (*(p+1) != '-')
          for (c = c ? c : *p; c <= *p; c++)    /* compare range */
#ifdef OS2
            if (tolower(c) == tolower(*s))
#else /* !OS2 */
            if (c == *s)
#endif /* ?OS2 */
              return r ? 0 : recmatch(q + 1, s + 1);
        c = e = 0;                      /* clear range, escape flags */
      }
    }
    return r ? recmatch(q + 1, s + 1) : 0;      /* bracket match failed */
  }
#endif /* !VMS */

  /* If escape ('\'), just compare next character */
  if (c == '\\')
    if ((c = *p++) == 0)                /* if \ at end, then syntax error */
      return 0;

  /* Just a character--compare it */
#ifdef OS2
  return tolower(c) == tolower(*s) ? recmatch(p, ++s) : 0;
#else /* !OS2 */
  return c == *s++ ? recmatch(p, s) : 0;        /* compare one character */
#endif /* ?OS2 */
}


int shmatch(p, s)
char *p;                /* sh pattern to match */
char *s;                /* string to match it to */
/* Compare the sh pattern p with the string s and return true if they match,
   false if they don't or if there is a syntax error in the pattern. */
{
  return recmatch(p, s) == 1;
}


#ifdef MSDOS

int dosmatch(p, s)
char *p;                /* dos pattern to match */
char *s;                /* string to match it to */
/* Break the pattern and string into name and extension parts and match
   each separately using shmatch(). */
{
  char *p1, *p2;        /* pattern sections */
  char *s1, *s2;        /* string sections */
  int r;                /* result */

  if ((p1 = malloc(strlen(p) + 1)) == NULL ||
      (s1 = malloc(strlen(s) + 1)) == NULL)
  {
    if (p1 != NULL)
      free((voidp *)p1);
    return 0;
  }
  strcpy(p1, p);
  strcpy(s1, s);
  if ((p2 = strrchr(p1, '.')) != NULL)
    *p2++ = 0;
  else
    p2 = "";
  if ((s2 = strrchr(s1, '.')) != NULL)
    *s2++ = 0;
  else
    s2 = "";
  r = shmatch(p2, s2) && shmatch(p1, s1);
  free((voidp *)p1);
  free((voidp *)s1);
  return r;
}

#endif /* MSDOS */


voidp far **search(b, a, n, cmp)
voidp *b;               /* pointer to value to search for */
voidp far **a;          /* table of pointers to values, sorted */
extent n;               /* number of pointers in a[] */
int (*cmp) OF((voidp *, voidp far *));  /* comparison function for search */
/* Search for b in the pointer list a[0..n-1] using the compare function
   cmp(b, c) where c is an element of a[i] and cmp() returns negative if
   *b < *c, zero if *b == *c, or positive if *b > *c.  If *b is found,
   search returns a pointer to the entry in a[], else search() returns
   NULL.  The nature and size of *b and *c (they can be different) are
   left up to the cmp() function.  A binary search is used, and it is
   assumed that the list is sorted in ascending order. */
{
  voidp far **i;        /* pointer to midpoint of current range */
  voidp far **l;        /* pointer to lower end of current range */
  int r;                /* result of (*cmp)() call */
  voidp far **u;        /* pointer to upper end of current range */

  l = (voidp far **)a;  u = l + (n-1);
  while (u >= l)
    if ((r = (*cmp)(b, *(i = l + ((u - l) >> 1)))) < 0)
      u = i - 1;
    else if (r > 0)
      l = i + 1;
    else
      return (voidp far **)i;
  return NULL;          /* If b were in list, it would belong at l */
}



/* Table of CRC-32's of all single byte values (made by makecrc.c) */
local ulg crctab[] = {
  0x00000000L, 0x77073096L, 0xee0e612cL, 0x990951baL, 0x076dc419L,
  0x706af48fL, 0xe963a535L, 0x9e6495a3L, 0x0edb8832L, 0x79dcb8a4L,
  0xe0d5e91eL, 0x97d2d988L, 0x09b64c2bL, 0x7eb17cbdL, 0xe7b82d07L,
  0x90bf1d91L, 0x1db71064L, 0x6ab020f2L, 0xf3b97148L, 0x84be41deL,
  0x1adad47dL, 0x6ddde4ebL, 0xf4d4b551L, 0x83d385c7L, 0x136c9856L,
  0x646ba8c0L, 0xfd62f97aL, 0x8a65c9ecL, 0x14015c4fL, 0x63066cd9L,
  0xfa0f3d63L, 0x8d080df5L, 0x3b6e20c8L, 0x4c69105eL, 0xd56041e4L,
  0xa2677172L, 0x3c03e4d1L, 0x4b04d447L, 0xd20d85fdL, 0xa50ab56bL,
  0x35b5a8faL, 0x42b2986cL, 0xdbbbc9d6L, 0xacbcf940L, 0x32d86ce3L,
  0x45df5c75L, 0xdcd60dcfL, 0xabd13d59L, 0x26d930acL, 0x51de003aL,
  0xc8d75180L, 0xbfd06116L, 0x21b4f4b5L, 0x56b3c423L, 0xcfba9599L,
  0xb8bda50fL, 0x2802b89eL, 0x5f058808L, 0xc60cd9b2L, 0xb10be924L,
  0x2f6f7c87L, 0x58684c11L, 0xc1611dabL, 0xb6662d3dL, 0x76dc4190L,
  0x01db7106L, 0x98d220bcL, 0xefd5102aL, 0x71b18589L, 0x06b6b51fL,
  0x9fbfe4a5L, 0xe8b8d433L, 0x7807c9a2L, 0x0f00f934L, 0x9609a88eL,
  0xe10e9818L, 0x7f6a0dbbL, 0x086d3d2dL, 0x91646c97L, 0xe6635c01L,
  0x6b6b51f4L, 0x1c6c6162L, 0x856530d8L, 0xf262004eL, 0x6c0695edL,
  0x1b01a57bL, 0x8208f4c1L, 0xf50fc457L, 0x65b0d9c6L, 0x12b7e950L,
  0x8bbeb8eaL, 0xfcb9887cL, 0x62dd1ddfL, 0x15da2d49L, 0x8cd37cf3L,
  0xfbd44c65L, 0x4db26158L, 0x3ab551ceL, 0xa3bc0074L, 0xd4bb30e2L,
  0x4adfa541L, 0x3dd895d7L, 0xa4d1c46dL, 0xd3d6f4fbL, 0x4369e96aL,
  0x346ed9fcL, 0xad678846L, 0xda60b8d0L, 0x44042d73L, 0x33031de5L,
  0xaa0a4c5fL, 0xdd0d7cc9L, 0x5005713cL, 0x270241aaL, 0xbe0b1010L,
  0xc90c2086L, 0x5768b525L, 0x206f85b3L, 0xb966d409L, 0xce61e49fL,
  0x5edef90eL, 0x29d9c998L, 0xb0d09822L, 0xc7d7a8b4L, 0x59b33d17L,
  0x2eb40d81L, 0xb7bd5c3bL, 0xc0ba6cadL, 0xedb88320L, 0x9abfb3b6L,
  0x03b6e20cL, 0x74b1d29aL, 0xead54739L, 0x9dd277afL, 0x04db2615L,
  0x73dc1683L, 0xe3630b12L, 0x94643b84L, 0x0d6d6a3eL, 0x7a6a5aa8L,
  0xe40ecf0bL, 0x9309ff9dL, 0x0a00ae27L, 0x7d079eb1L, 0xf00f9344L,
  0x8708a3d2L, 0x1e01f268L, 0x6906c2feL, 0xf762575dL, 0x806567cbL,
  0x196c3671L, 0x6e6b06e7L, 0xfed41b76L, 0x89d32be0L, 0x10da7a5aL,
  0x67dd4accL, 0xf9b9df6fL, 0x8ebeeff9L, 0x17b7be43L, 0x60b08ed5L,
  0xd6d6a3e8L, 0xa1d1937eL, 0x38d8c2c4L, 0x4fdff252L, 0xd1bb67f1L,
  0xa6bc5767L, 0x3fb506ddL, 0x48b2364bL, 0xd80d2bdaL, 0xaf0a1b4cL,
  0x36034af6L, 0x41047a60L, 0xdf60efc3L, 0xa867df55L, 0x316e8eefL,
  0x4669be79L, 0xcb61b38cL, 0xbc66831aL, 0x256fd2a0L, 0x5268e236L,
  0xcc0c7795L, 0xbb0b4703L, 0x220216b9L, 0x5505262fL, 0xc5ba3bbeL,
  0xb2bd0b28L, 0x2bb45a92L, 0x5cb36a04L, 0xc2d7ffa7L, 0xb5d0cf31L,
  0x2cd99e8bL, 0x5bdeae1dL, 0x9b64c2b0L, 0xec63f226L, 0x756aa39cL,
  0x026d930aL, 0x9c0906a9L, 0xeb0e363fL, 0x72076785L, 0x05005713L,
  0x95bf4a82L, 0xe2b87a14L, 0x7bb12baeL, 0x0cb61b38L, 0x92d28e9bL,
  0xe5d5be0dL, 0x7cdcefb7L, 0x0bdbdf21L, 0x86d3d2d4L, 0xf1d4e242L,
  0x68ddb3f8L, 0x1fda836eL, 0x81be16cdL, 0xf6b9265bL, 0x6fb077e1L,
  0x18b74777L, 0x88085ae6L, 0xff0f6a70L, 0x66063bcaL, 0x11010b5cL,
  0x8f659effL, 0xf862ae69L, 0x616bffd3L, 0x166ccf45L, 0xa00ae278L,
  0xd70dd2eeL, 0x4e048354L, 0x3903b3c2L, 0xa7672661L, 0xd06016f7L,
  0x4969474dL, 0x3e6e77dbL, 0xaed16a4aL, 0xd9d65adcL, 0x40df0b66L,
  0x37d83bf0L, 0xa9bcae53L, 0xdebb9ec5L, 0x47b2cf7fL, 0x30b5ffe9L,
  0xbdbdf21cL, 0xcabac28aL, 0x53b39330L, 0x24b4a3a6L, 0xbad03605L,
  0xcdd70693L, 0x54de5729L, 0x23d967bfL, 0xb3667a2eL, 0xc4614ab8L,
  0x5d681b02L, 0x2a6f2b94L, 0xb40bbe37L, 0xc30c8ea1L, 0x5a05df1bL,
  0x2d02ef8dL
};


ulg crc32(c, b)
ulg c;                  /* current contents of crc shift register */
int b;                  /* byte (eight bits) to run through */
/* Return the CRC-32 c updated with the eight bits in b. */
{
  return crctab[((int)c ^ b) & 0xff] ^ (c >> 8);
}


ulg updcrc(s, n)
char *s;                /* pointer to bytes to pump through */
extent n;               /* number of bytes in s[] */
/* Run a set of bytes through the crc shift register.  If s is a NULL
   pointer, then initialize the crc shift register contents instead.
   Return the current crc in either case. */
{
  register ulg c;       /* temporary variable */

  static ulg crc = 0xffffffffL; /* shift register contents */

  if (s == NULL)
    c = 0xffffffffL;
  else
  {
    c = crc;
    while (n--)
      c = crctab[((int)c ^ (*s++)) & 0xff] ^ (c >> 8);
  }
  crc = c;
  return c ^ 0xffffffffL;       /* (instead of ~c for 64-bit machines) */
}
