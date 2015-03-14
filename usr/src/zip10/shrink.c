/*

 Copyright (C) 1990,1991 Mark Adler, Richard B. Wales, and Jean-loup Gailly.
 Permission is granted to any individual or institution to use, copy, or
 redistribute this software so long as all of the original files are included
 unmodified, that it is not sold for profit, and that this copyright notice
 is retained.

*/

/*
 *  shrink.c by Mark Adler.
 */

#include "zip.h"
#include "tempf.h"


/*
   Shrink.Pas version 1.2 by R. P. Byrne, 1989 (in Pascal and assembler).
   We here heartily acknowledge R. P. Byrne's contribution to this project.
   The existence of this program really triggered our efforts to write a
   portable Unix zip.  What little remains of Byrne's program lies in this
   source file, and those remnants are mostly in the variable and routine
   names, since it has been translated and extensively modified and rewritten.

   Stolen, translated into C, and modified by Mark Adler, 11 October 1990.
   Severely modified again by Mark Adler, 11 July 1991, to remove the
   unnecessary FreeList and ClearTable arrays, and to replace the recursive
   Prune() routine with a non-recursive method.
   As Stravinsky once said: "Mediocre composers plagiarize.
                             Great composers steal."
*/

/*   Compress a set of input files into a Zip file using Lempel-Ziv-Welch */
/*   (LZW) compression techniques (the "shrink" method). */


#define MINBITS         9       /* Starting code size of 9 bits */
#define MAXBITS         13      /* Maximum code size of 13 bits */
#define TABLESIZE       8191    /* We'll need 4K entries in table */
#define SPECIAL         256     /* Special function code */
#define INCSIZE         1       /* Code for a jump in code size */
#define CLEARCODE       2       /* Code for code table has been cleared */
#define FIRSTENTRY      257     /* First available table entry */


/* Define data types needed to implement a code table for LZW compression */

typedef struct CodeRec {
  /* Code Table record format... */
  short Child;          /* Addr of 1st suffix for this prefix */
  short Sibling;        /* Addr of next suffix in chain */
  uch Suffix;           /* Suffix character */
} CodeRec;

typedef CodeRec CodeArray[TABLESIZE + 1];       /* Define the code table */



/* Private globals */
local CodeRec far *CodeTable;   /* Points to code table for LZW compression */

local int NextFree;     /* Next free table entry */

local int CodeSize;     /* Size of codes (in bits) currently being written */
local int MaxCode;      /* Largest code that can be written in CodeSize bits */

local int FirstCh;      /* Flag indicating the START of a shrink operation */

local tFILE *tempf = NULL;      /* Temporary file */
local ulg count;                /* Count of bytes written */


/* Local functions */
#ifdef PROTO
   local void PutCode(int);
   local int Build_Data_Structures(void);
   local void Destroy_Data_Structures(void);
   local void Initialize_Data_Structures(void);
   local void Clear_Table(void);
   local void Table_Add(int, int);
#endif /* PROTO */


/* Macro for PutCode() that writes to tempf and counts bytes in count */
#define PUT(c) {tputc(c, tempf); count++;}

local void PutCode(c)
int c;                  /* code to send */
/* Write out the low CodeSize bits of c using the PUT macro.  If c is -1,
   then flush the bit buffer.  Assumes CodeSize < 16.  By Mark Adler. */
{
  static int b = 0;     /* current bits waiting to go out */
  static int n = 0;     /* number of bits in b */
  /* masks for all bit values */
  static int x[] = {0, 1, 3, 7, 0xf, 0x1f, 0x3f, 0x7f, 0xff, 0x1ff,
                0x3ff, 0x7ff, 0xfff, 0x1fff, 0x3fff, 0x7fff, 0xffff};

  if (c == -1)
  {
    if (n)
    {
      if (n > 8)
      {
        PUT((char)b)
        PUT((char)((b >> 8) & x[n - 8]))
      }
      else
        PUT((char)(b & x[n]))
      b = n = 0;
    }
  }
  else
  {
    b |= (c & x[CodeSize]) << n;
    n += CodeSize;
    if (n >= 16)
    {
      PUT((char)b)
      PUT((char)(b >> 8))
      if (n == 16)
        b = n = 0;
      else
      {
        n -= 16;
        b = (c >> (CodeSize - n)) & x[n];
      }
    }
  }
}



local int Build_Data_Structures()
/* Allocate tables for shrinking.  Return true on failure. */
{
  return (CodeTable = (CodeRec far *)farmalloc(sizeof(CodeArray))) == NULL;
}



local void Destroy_Data_Structures()
/* Deallocate tables for shrinking. */
{
  if (CodeTable != NULL)
  {
    farfree((voidp far *)CodeTable);
    CodeTable = NULL;
  }
}



local void Initialize_Data_Structures()
/* Clear tables for shrinking. */
{
  int i;                /* counter for table entries */
  CodeRec far *t;       /* pointer to current table entry */

  /* Initialize parent symbols */
  for (i = 0, t = CodeTable; i <= 255; i++, t++)
  {
    t->Child = -1;
    t->Suffix = (uch)i;
  }

  /* Build free list */
  NextFree = FIRSTENTRY;
  for (i = FIRSTENTRY, t = CodeTable + FIRSTENTRY; i < TABLESIZE; i++, t++)
    t->Child = i + 1;
  t->Child = -1;
}



local void Clear_Table()
/* Clear the leaves of the tree--assume all entries used (NextFree == -1) */
{
  int n;                /* node counter */
  CodeRec far *p;       /* pointer to next node to look at */
  short far *q;         /* pointer to node child or sibling entry */

  /* Mark leaf nodes */
  p = CodeTable + TABLESIZE;
  n = TABLESIZE + 1 - FIRSTENTRY;
  do {
    if (p->Child == -1)
      p->Child = -2;
    p--;
  } while (--n);

  /* Shake leaves from tree */
  p = CodeTable;
  n = 256;
  do {
    q = &p->Child;
    while (*q != -1 && CodeTable[*q].Child == -2)
      *q = CodeTable[*q].Sibling;
    p++;
  } while (--n);
  p = CodeTable + FIRSTENTRY;
  n = TABLESIZE + 1 - FIRSTENTRY;
  do {
    if (p->Child != -2)
    {
      q = &p->Child;
      while (*q != -1 && CodeTable[*q].Child == -2)
        *q = CodeTable[*q].Sibling;
      q = &p->Sibling;
      while (*q != -1 && CodeTable[*q].Child == -2)
        *q = CodeTable[*q].Sibling;
    }
    p++;
  } while (--n);

  /* Build the list of free table entries */
  NextFree = -1;
  p = CodeTable + TABLESIZE;
  n = TABLESIZE + 1 - FIRSTENTRY;
  do {
    if (p->Child == -2)
    {
      p->Child = NextFree;
      NextFree = n + FIRSTENTRY - 1;
    }
    p--;
  } while (--n);
}



local void Table_Add(p, s)
int p;                  /* prefix to add to */
int s;                  /* suffix to add to it */
/* Add an entry to the table. */
{
  int f;                /* next free node */

  if ((f = NextFree) != -1)
  {
    NextFree = CodeTable[f].Child;
    CodeTable[f].Child = -1;
    CodeTable[f].Sibling = -1;
    CodeTable[f].Suffix = (uch)s;
    if (CodeTable[p].Child == -1)
      CodeTable[p].Child = f;
    else
    {
      p = CodeTable[p].Child;
      while (CodeTable[p].Sibling != -1)
        p = CodeTable[p].Sibling;
      CodeTable[p].Sibling = f;
    }
  }
}


local int lastcode;

int shr_setup()
/* Initialize shrink() routines.  Return an error code in the ZE_ class. */
{
  if (Build_Data_Structures())
    return ZE_MEM;
  Initialize_Data_Structures();
  FirstCh = 1;
  lastcode = -1;
  if ((tempf = topen('S')) == NULL)
    return ZE_MEM;
  count = 0;
  return ZE_OK;
}


int shr_p1(b, n)
uch *b;                 /* buffer with bytes to shrink */
extent n;               /* number of bytes in buffer */
/* Shrink n bytes at *b.  Return an error code in the ZE_ class. */
{
  int f;                /* result of Table_Lookup */
  int s;                /* byte to shrink */

  if (FirstCh && n)
  {                             /* If just getting started ... */
    CodeSize = MINBITS;         /*   Initialize code size to minimum */
    MaxCode = (1 << CodeSize) - 1;
    lastcode = *b++;  n--;      /*   get first character from input, */
    FirstCh = 0;                /*   and reset the first char flag. */
  }
  while (NextFree == -1 && n)
  {
    /* Ok, lets clear the code table (adaptive reset) */
    PutCode(lastcode);
    PutCode(SPECIAL);
    PutCode(CLEARCODE);
    Clear_Table();
    Table_Add(lastcode, s = *b++);  n--;
    lastcode = s;
  }
  while (n)
  {
    s = *b++;  n--;
    f = CodeTable[lastcode].Child;
    while (f != -1 && CodeTable[f].Suffix != (uch)s)
      f = CodeTable[f].Sibling;
    if (f != -1)
      /* If lastcode:s pair is found in the code table, then ... */
      /* ... set lastcode to the entry where the pair is located */
      lastcode = f;
    else
    {
      /* Not in table */
      PutCode(lastcode);        /* Write current lastcode */
      Table_Add(lastcode, s);   /* Attempt to add to code table */
      lastcode = s;             /* Reset lastcode for new char */
      if (NextFree > MaxCode && CodeSize < MAXBITS)
      {
        /* Time to increase the code size and change the max. code */
        PutCode(SPECIAL);
        PutCode(INCSIZE);
        CodeSize++;
        MaxCode = (1 << CodeSize) - 1;
      }
      while (NextFree == -1 && n)
      {
        /* Ok, lets clear the code table (adaptive reset) */
        PutCode(lastcode);
        PutCode(SPECIAL);
        PutCode(CLEARCODE);
        Clear_Table();
        Table_Add(lastcode, s = *b++);  n--;
        lastcode = s;
      }
    }
  }
  return ZE_OK;
}


int shr_size(s)
ulg *s;                 /* return value: size of shrunk data */
/* End shrink and return size of shrunk data in *s.  Return an error code in
   the ZE_ class. */
{
  PutCode(lastcode);            /* Write last prefix code */
  PutCode(-1);                  /* Tell putcode to flush remaining bits */
  Destroy_Data_Structures();
  *s = count;
  return tflush(tempf) || terror(tempf) ? ZE_TEMP : ZE_OK;
}


int shr_p2(f)
FILE *f;                /* file to write shrunk data to */
/* Copy shrunk data from temporary file to zip file *f.  Return an error
   code in the ZE_ class. */
{
  char *b;              /* malloc'ed buffer for copying */
  extent k;             /* holds result of fread */

  if ((b = malloc(BSZ)) == NULL)
    return ZE_MEM;
  trewind(tempf);
  while ((k = tread(b, 1, BSZ, tempf)) > 0)
    if (zfwrite(b, 1, k, f) != k)
    {
      free((voidp *)b);
      return ZE_TEMP;
    }
  free((voidp *)b);
  if (terror(tempf))
    return ZE_TEMP;
  tclose(tempf);
  tempf = NULL;
  return ZE_OK;
}


int shr_clear()
/* Terminate shrink procedure (at any time).  Return an error code in
   the ZE_ class (always ZE_OK). */
{
  Destroy_Data_Structures();
  if (tempf != NULL)
  {
    tclose(tempf);
    tempf =  NULL;
  }
  return ZE_OK;
}
