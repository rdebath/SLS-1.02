/* $Id: Memory.c,v 1.13 1992/06/24 12:23:15 grosch rel $ */

/* $Log: Memory.c,v $
 * Revision 1.13  1992/06/24  12:23:15  grosch
 * changed cNoMoreSpace from -1 to 0
 *
 * Revision 1.12  1992/05/05  13:19:05  grosch
 * added rcsid
 *
 * Revision 1.11  1992/01/31  16:31:44  grosch
 * adaption to ANSI C
 *
 * Revision 1.10  1992/01/30  13:16:06  grosch
 * redesign of interface to operating system
 *
 * Revision 1.9  1992/01/14  15:24:35  grosch
 * introduced automatic initialization
 *
 * Revision 1.8  1991/11/21  14:28:16  grosch
 * new version of RCS on SPARC
 *
 * Revision 1.7  91/01/21  12:13:22  grosch
 * some performance improvements
 * 
 * Revision 1.6  90/12/14  15:55:52  grosch
 * introduced variable MemoryUsed
 * 
 * Revision 1.5  90/09/14  11:20:46  grosch
 * removed superfluous declarations for automatic alignment
 * 
 * Revision 1.4  90/09/04  17:32:10  grosch
 * automatic determination of alignment
 * 
 * Revision 1.3  90/07/04  14:33:58  grosch
 * introduced conditional include
 * 
 * Revision 1.2  89/12/08  20:16:43  grosch
 * added alignment for MIPS processor
 * 
 * Revision 1.1  89/06/06  10:28:54  grosch
 * fixed lint problem at call of Free
 * 
 * Revision 1.0  88/10/04  11:44:41  grosch
 * Initial revision
 * 
 */

/* Ich, Doktor Josef Grosch, Informatiker, Sept. 1987 */

static char rcsid [] = "$Id: Memory.c,v 1.13 1992/06/24 12:23:15 grosch rel $";

# include "ratc.h"
# include "Memory.h"
# include "System.h"
# include "General.h"
# include <stdio.h>

# define MinSizeSmallBlock	4
# define MaxSizeSmallBlock	62	/* 64 - 2	*/
# define MinSizeLargeBlockLog	6	/* Log2 64	*/
# define MaxSizeLargeBlockLog	24	/* Log2 2**24	*/
# define PoolSize		10240L
# define NIL			(tBlockPtr) NULL

unsigned long MemoryUsed = 0;

struct tBlock {
   struct tBlock *	Successor;
   unsigned long	Size;
};
typedef struct tBlock *	tBlockPtr;
typedef cardinal	tSmallBlockRange;
typedef cardinal	tLargeBlockRange;

static	tBlockPtr	SmallChain [MaxSizeSmallBlock    + 1] = { 0,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL,
};
static	tBlockPtr	LargeChain [MaxSizeLargeBlockLog + 1] = { 0,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL,
   NIL, NIL, NIL, NIL,
};
static	char *		PoolFreePtr = 0;
static	char *		PoolEndPtr  = 0;

char * Alloc (ByteCount)
   register unsigned long ByteCount;

/* Returns a pointer to dynamically allocated	*/
/* space of size 'ByteCount' bytes.		*/

{
   ByteCount = (ByteCount + yyMaxAlign - 1) & yyAlignMasks [yyMaxAlign];

   if (ByteCount <= MaxSizeSmallBlock) {	/* handle small block */
      if (ByteCount < MinSizeSmallBlock) ByteCount = MinSizeSmallBlock;
      if (SmallChain [ByteCount] != NIL) {	/* obtain block from freelist */
	 register tBlockPtr CurrentBlock = SmallChain [ByteCount];
	 SmallChain [ByteCount] = CurrentBlock->Successor;
	 return (char *) CurrentBlock;
      } else {					/* obtain block from storage pool */
	 register unsigned long FreeBytes;
	 register char *	OldFreePtr;
						/* release old pool */
	 if ((FreeBytes = PoolEndPtr - PoolFreePtr) < ByteCount) {
	    if (FreeBytes >= MinSizeSmallBlock) Free (FreeBytes, PoolFreePtr);
	    PoolFreePtr = Alloc (PoolSize);	/* allocate new pool */
	    PoolEndPtr  = PoolFreePtr + PoolSize;
	 }
	 OldFreePtr = PoolFreePtr;
	 PoolFreePtr += ByteCount;
	 return OldFreePtr;
      }
   } else {					/* handle large block */

      /* 1. search in LargeChain [Log2 (ByteCount)] using BEST FIT */

      register cardinal		ChainNumber	= Log2 (ByteCount);
      register tBlockPtr	CurrentBlock	= LargeChain [ChainNumber];
      register tBlockPtr	PreviousBlock	= (tBlockPtr) & (LargeChain [ChainNumber]);
      register tBlockPtr	BestBlock	= NIL;
      register unsigned long	BestBlockSize	= 1000000000;
      register tBlockPtr	PredecessorBlock;
      register tLargeBlockRange	j		;
      register unsigned long	CurrentBlockSize;

      while (CurrentBlock) {
	 CurrentBlockSize = CurrentBlock->Size;
	 if (CurrentBlockSize >= ByteCount) {
	    if (CurrentBlockSize == ByteCount) { /* exact match */
	       PreviousBlock->Successor = CurrentBlock->Successor;
	       return (char *) CurrentBlock;
	    }
	    if (CurrentBlockSize < BestBlockSize) { /* improve approximation */
	       BestBlock	= CurrentBlock;
	       BestBlockSize	= CurrentBlockSize;
	       PredecessorBlock	= PreviousBlock;
	    }
	 }
	 PreviousBlock	= CurrentBlock;
	 CurrentBlock	= CurrentBlock->Successor;
      }

      if (BestBlock) {
	 PredecessorBlock->Successor = BestBlock->Successor;
	 if (BestBlockSize - ByteCount >= MinSizeSmallBlock) {
	    Free (BestBlockSize - ByteCount, (char *) BestBlock + ByteCount);
	 }
	 return (char *) BestBlock;
      }

      /* 2. search in LargeChain [j], j > Log2 (ByteCount), using FIRST FIT */

      for (j = ChainNumber+1; j <= MaxSizeLargeBlockLog; j ++) {
	 CurrentBlock = LargeChain [j];
	 if (CurrentBlock != NIL) {
	    LargeChain [j] = CurrentBlock->Successor;
	    if (CurrentBlock->Size - ByteCount >= MinSizeSmallBlock) {
	       Free (CurrentBlock->Size - ByteCount, (char *) CurrentBlock + ByteCount);
	    }
	    return (char *) CurrentBlock;
	 }
      }

      if (ByteCount < PoolSize) {		/* 3. obtain block from storage pool */
	 register unsigned long FreeBytes;
						/* release old pool */
	 if ((FreeBytes = PoolEndPtr - PoolFreePtr) < ByteCount) {
	    if (FreeBytes >= MinSizeSmallBlock) Free (FreeBytes, PoolFreePtr);
	    PoolFreePtr = Alloc (PoolSize);	/* allocate new pool */
	    PoolEndPtr  = PoolFreePtr + PoolSize;
	 }
	 PoolFreePtr += ByteCount;
	 return PoolFreePtr - ByteCount;
      } else {					/* 4. allocate individual block */
	 CurrentBlock = (tBlockPtr) SysAlloc ((long) ByteCount);
	 MemoryUsed += ByteCount;
	 return (char *) CurrentBlock;
      }
   }
}

void Free (ByteCount, a)
   unsigned long	ByteCount;
   char *		a;

/* The dynamically allocated space starting at	*/
/* address 'a' of size 'ByteCount' bytes is	*/
/* released.					*/

{
   register tBlockPtr		BlockPtr;
   register tLargeBlockRange	ChainNumber;

   ByteCount = (ByteCount + yyMaxAlign - 1) & yyAlignMasks [yyMaxAlign];

   BlockPtr = (tBlockPtr) a;
   if (ByteCount <= MaxSizeSmallBlock) {
      if (ByteCount < MinSizeSmallBlock) ByteCount = MinSizeSmallBlock;
      BlockPtr->Successor	= SmallChain [ByteCount];
      SmallChain [ByteCount]	= BlockPtr;
   } else {
      ChainNumber		= Log2 (ByteCount);
      BlockPtr->Successor	= LargeChain [ChainNumber];
      BlockPtr->Size		= ByteCount;
      LargeChain [ChainNumber]	= BlockPtr;
   }
}

void InitMemory ()
{
   register int i;

   for (i = MinSizeSmallBlock; i <= MaxSizeSmallBlock; i += 2) {
      SmallChain [i] = NIL;
   }
   for (i = MinSizeLargeBlockLog; i <= MaxSizeLargeBlockLog; i ++) {
      LargeChain [i] = NIL;
   }
   MemoryUsed	= 0;
   PoolFreePtr	= 0;
   PoolEndPtr	= 0;
}
