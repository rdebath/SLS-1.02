/* $Id: StringMem.c,v 1.11 1992/05/05 13:19:05 grosch rel $ */

/* $Log: StringMem.c,v $
 * Revision 1.11  1992/05/05  13:19:05  grosch
 * added rcsid
 *
 * Revision 1.10  1992/02/18  12:52:30  grosch
 * changed tString from unsigned char * to char *
 *
 * Revision 1.9  1992/01/31  16:31:44  grosch
 * adaption to ANSI C
 *
 * Revision 1.8  1992/01/30  13:12:51  grosch
 * complete redesign: pointer instead of array index
 *
 * Revision 1.7  1992/01/14  15:24:35  grosch
 * introduced automatic initialization
 *
 * Revision 1.6  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.5  91/07/17  17:23:46  grosch
 * introduced ARGS trick for ANSI compatibility
 * 
 * Revision 1.4  91/01/21  12:13:25  grosch
 * some performance improvements
 * 
 * Revision 1.3  90/09/20  09:12:28  grosch
 * calmed down lint
 * 
 * Revision 1.2  90/07/04  14:34:07  grosch
 * introduced conditional include
 * 
 * Revision 1.1  89/08/23  16:04:27  grosch
 * bug fix in PutString: stringReg initialized later
 * 
 * Revision 1.0  88/10/04  11:44:47  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

static char rcsid [] = "$Id: StringMem.c,v 1.11 1992/05/05 13:19:05 grosch rel $";

# include "ratc.h"
# include "StringMem.h"
# include "General.h"
# include "Memory.h"

# define MemorySize	1024 * 20

typedef struct sBlock {
   struct sBlock *	Next;
   tStringRef		Last;
   char			Memory [MemorySize];
} tBlock;

struct lBlock {
   struct lBlock *	Next;
   tStringRef		Last;
};

static	tBlock *	BlockList	= NULL;
static	unsigned long	MemorySpaceLeft	= 0;
static	tStringRef	MemoryFreePtr;

tStringRef PutString
# ifdef __STDC__
   (register char * string, register cardinal length)
# else
   (string, length)
   register char *	string;
   register cardinal	length;
# endif
   {
      register char *	stringReg;
      register long	NeededSpace	= (length + 3) & 0xfffffffe;
      register tStringRef StartPtr;

      if (MemorySpaceLeft < NeededSpace) {
	 tBlock * BlockPtr;
	 if (BlockList != NULL) {

	    char * FreePtr = (char *) (((long) MemoryFreePtr + yyMaxAlign - 1) & yyAlignMasks [yyMaxAlign]);
	    unsigned long Rest = MemorySpaceLeft - (FreePtr - (char *) MemoryFreePtr);
	    if (Rest >= yyMaxAlign) Free (Rest, FreePtr);
	    BlockList->Last = MemoryFreePtr;
	 }
	 MemorySpaceLeft = Max (NeededSpace, MemorySize);
	 BlockPtr = (tBlock *) Alloc (MemorySpaceLeft + sizeof (struct lBlock));

	 BlockPtr->Next = BlockList;
	 BlockList = BlockPtr;
	 MemoryFreePtr = (tStringRef) BlockPtr->Memory;
      }
      StartPtr = MemoryFreePtr;
      * StartPtr = length;
      stringReg = (char *) MemoryFreePtr + 2;
      while (length -- > 0) * stringReg ++ = * string ++;
      MemorySpaceLeft -= NeededSpace;
      MemoryFreePtr   += NeededSpace >> 1;
      return StartPtr;
   }

void StGetString (stringref, string)
   register tStringRef	stringref;
   register char *	string;
   {
      register cardinal	length	  = LengthSt (stringref);
      register char *	stringReg = (char *) stringref + 2;

      while (length -- > 0) * string ++ = * stringReg ++;
      * string = '\0';
   }

bool IsEqualSt (stringref, string)
   tStringRef	stringref;
   register char *	string;
   {
      register cardinal	length	  = LengthSt (stringref);
      register char *	stringReg = (char *) stringref + 2;

      while (length -- > 0)
	 if (* stringReg ++ != * string ++)
	    return false;
      return true;
   }

void WriteString (file, stringref)
   FILE *	file;
   tStringRef	stringref;
   {
      register cardinal	length = LengthSt (stringref);

      if (length < 256) {
	 char string [256];
	 StGetString (stringref, string);
	 (void) fputs (string, file);
      } else {
	 char * string = (char *) Alloc ((unsigned long) ++ length);
	 StGetString (stringref, string);
	 (void) fputs ((char *) string, file);
	 Free ((unsigned long) length, (char *) string);
      }
   }

void WriteStringMemory ()
   {
      tBlock *	BlockPtr = BlockList;
      int	Size	 = 0;
   
      if (BlockPtr != NULL) BlockPtr->Last = MemoryFreePtr;
      while (BlockPtr != NULL) {
	 tStringRef StringPtr = (tStringRef) BlockPtr->Memory;
         Size += (char *) BlockPtr->Last - (char *) StringPtr;

	 while (StringPtr < BlockPtr->Last) {
	    int length = LengthSt (StringPtr) + 2;
	    (void) printf ("%8x ", StringPtr);
	    WriteString (stdout, StringPtr);
	    (void) fputc ('\n', stdout);
	    if (length & 1) length ++;
	    StringPtr += length >> 1;
	 }
	 BlockPtr = BlockPtr->Next;
      }
      (void) printf ("\n%5ld Bytes\n", Size);
   }

void InitStringMemory ()
   {
      BlockList		= NULL;
      MemorySpaceLeft	= 0;
   }
