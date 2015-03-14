#include "SYSTEM_.h"

#ifndef DEFINITION_General
#include "General.h"
#endif

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_Memory
#include "Memory.h"
#endif

LONGCARD Memory_MemoryUsed;

#define MinSizeSmallBlock	4
#define MaxSizeSmallBlock	62
#define MinSizeLargeBlockLog	6
#define MaxSizeLargeBlockLog	24
#define PoolSize	10240
typedef struct S_1 *tBlockPtr;
typedef struct S_1 {
    tBlockPtr Successor;
    LONGINT Size;
} tBlock;
typedef LONGCARD tSmallBlockRange;
typedef LONGCARD tLargeBlockRange;
static struct S_2 {
    ADDRESS A[MaxSizeSmallBlock - MinSizeSmallBlock + 1];
} SmallChain;
static struct S_3 {
    ADDRESS A[MaxSizeLargeBlockLog - MinSizeLargeBlockLog + 1];
} LargeChain;
static ADDRESS PoolFreePtr;
static ADDRESS PoolEndPtr;
static tSmallBlockRange i;
static tLargeBlockRange j;


ADDRESS Memory_Alloc
# ifdef __STDC__
(LONGINT ByteCount)
# else
(ByteCount)
LONGINT ByteCount;
# endif
{
  tBlockPtr BlockPtr, CurrentBlock, PreviousBlock, BestBlock, PredecessorBlock;
  CARDINAL ChainNumber;
  LONGINT CurrentBlockSize, BestBlockSize;
  tLargeBlockRange j;

  ByteCount = (LONGINT)((BITSET)(ByteCount + General_MaxAlign - 1) & General_AlignMasks.A[General_MaxAlign]);
  if (ByteCount <= MaxSizeSmallBlock) {
    if (ByteCount < MinSizeSmallBlock) {
      ByteCount = MinSizeSmallBlock;
    }
    if (SmallChain.A[ByteCount - 4] != NIL) {
      BlockPtr = (tBlockPtr)SmallChain.A[ByteCount - 4];
      SmallChain.A[ByteCount - 4] = (ADDRESS)BlockPtr->Successor;
      return (ADDRESS)BlockPtr;
    } else {
      if ((LONGINT)(PoolEndPtr - (LONGCARD)PoolFreePtr) < ByteCount) {
        if ((LONGCARD)(PoolEndPtr - (LONGCARD)PoolFreePtr) >= MinSizeSmallBlock) {
          Memory_Free((LONGINT)(PoolEndPtr - (LONGCARD)PoolFreePtr), PoolFreePtr);
        }
        PoolFreePtr = Memory_Alloc((LONGINT)PoolSize);
        PoolEndPtr = (ADDRESS)(PoolFreePtr + PoolSize);
      }
      INC1(PoolFreePtr, (LONGCARD)(ADDRESS)ByteCount);
      return PoolFreePtr - (LONGCARD)(ADDRESS)ByteCount;
    }
  } else {
    ChainNumber = General_Log2(ByteCount);
    CurrentBlock = (tBlockPtr)LargeChain.A[ChainNumber - 6];
    PreviousBlock = (tBlockPtr)ADR(LargeChain.A[ChainNumber - 6]);
    BestBlock = NIL;
    BestBlockSize = 1000000000;
    while (CurrentBlock != NIL) {
      CurrentBlockSize = CurrentBlock->Size;
      if (CurrentBlockSize >= ByteCount) {
        if (CurrentBlockSize == ByteCount) {
          PreviousBlock->Successor = CurrentBlock->Successor;
          return (ADDRESS)CurrentBlock;
        }
        if (CurrentBlockSize < BestBlockSize) {
          BestBlock = CurrentBlock;
          BestBlockSize = CurrentBlockSize;
          PredecessorBlock = PreviousBlock;
        }
      }
      PreviousBlock = CurrentBlock;
      CurrentBlock = CurrentBlock->Successor;
    }
    if (BestBlock != NIL) {
      PredecessorBlock->Successor = BestBlock->Successor;
      if (BestBlockSize - ByteCount >= MinSizeSmallBlock) {
        Memory_Free(BestBlockSize - ByteCount, (ADDRESS)BestBlock + (LONGCARD)(ADDRESS)ByteCount);
      }
      return (ADDRESS)BestBlock;
    }
    for (j = ChainNumber + 1; j <= MaxSizeLargeBlockLog; j += 1) {
      CurrentBlock = (tBlockPtr)LargeChain.A[j - 6];
      if (CurrentBlock != NIL) {
        LargeChain.A[j - 6] = (ADDRESS)CurrentBlock->Successor;
        if (CurrentBlock->Size - ByteCount >= MinSizeSmallBlock) {
          Memory_Free(CurrentBlock->Size - ByteCount, (ADDRESS)CurrentBlock + (LONGCARD)(ADDRESS)ByteCount);
        }
        return (ADDRESS)CurrentBlock;
      }
    }
    if (ByteCount < PoolSize) {
      if ((LONGINT)(PoolEndPtr - (LONGCARD)PoolFreePtr) < ByteCount) {
        if ((LONGCARD)(PoolEndPtr - (LONGCARD)PoolFreePtr) >= MinSizeSmallBlock) {
          Memory_Free((LONGINT)(PoolEndPtr - (LONGCARD)PoolFreePtr), PoolFreePtr);
        }
        PoolFreePtr = Memory_Alloc((LONGINT)PoolSize);
        PoolEndPtr = (ADDRESS)(PoolFreePtr + PoolSize);
      }
      INC1(PoolFreePtr, (LONGCARD)(ADDRESS)ByteCount);
      return PoolFreePtr - (LONGCARD)(ADDRESS)ByteCount;
    } else {
      BlockPtr = (tBlockPtr)SysAlloc(ByteCount);
      INC1(Memory_MemoryUsed, ByteCount);
      return (ADDRESS)BlockPtr;
    }
  }
}

void Memory_Free
# ifdef __STDC__
(LONGINT ByteCount, ADDRESS a)
# else
(ByteCount, a)
LONGINT ByteCount;
ADDRESS a;
# endif
{
  tBlockPtr BlockPtr;
  tLargeBlockRange ChainNumber;

  ByteCount = (LONGINT)((BITSET)(ByteCount + General_MaxAlign - 1) & General_AlignMasks.A[General_MaxAlign]);
  BlockPtr = (tBlockPtr)a;
  if (ByteCount <= MaxSizeSmallBlock) {
    if (ByteCount < MinSizeSmallBlock) {
      ByteCount = MinSizeSmallBlock;
    }
    BlockPtr->Successor = (tBlockPtr)SmallChain.A[ByteCount - 4];
    SmallChain.A[ByteCount - 4] = (ADDRESS)BlockPtr;
  } else {
    ChainNumber = General_Log2(ByteCount);
    BlockPtr->Successor = (tBlockPtr)LargeChain.A[ChainNumber - 6];
    BlockPtr->Size = ByteCount;
    LargeChain.A[ChainNumber - 6] = (ADDRESS)BlockPtr;
  }
}

void BEGIN_Memory()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_General();
    BEGIN_System();

    for (i = MinSizeSmallBlock; i <= MaxSizeSmallBlock; i += 2) {
      SmallChain.A[i - 4] = (ADDRESS)NIL;
    }
    for (j = MinSizeLargeBlockLog; j <= MaxSizeLargeBlockLog; j += 1) {
      LargeChain.A[j - 6] = (ADDRESS)NIL;
    }
    PoolFreePtr = (ADDRESS)NIL;
    PoolEndPtr = (ADDRESS)NIL;
    Memory_MemoryUsed = 0;
  }
}
