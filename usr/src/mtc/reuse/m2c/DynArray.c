#include "SYSTEM_.h"

#ifndef DEFINITION_General
#include "General.h"
#endif

#ifndef DEFINITION_Memory
#include "Memory.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_DynArray
#include "DynArray.h"
#endif


static LONGINT AlignedSize ARGS((LONGINT ElmtSize));


void DynArray_MakeArray
# ifdef __STDC__
(ADDRESS *ArrayPtr, LONGINT *ElmtCount, LONGINT ElmtSize)
# else
(ArrayPtr, ElmtCount, ElmtSize)
ADDRESS *ArrayPtr;
LONGINT *ElmtCount;
LONGINT ElmtSize;
# endif
{
  ElmtSize = AlignedSize(ElmtSize);
  switch (ElmtSize % 4) {
  case 0:;
    break;
  case 2:;
    if (ODD(*ElmtCount)) {
      INC(*ElmtCount);
    }
    break;
  case 1:;
  case 3:;
    INC1(*ElmtCount, sizeof(LONGINT) - 1 - (*ElmtCount - 1) % sizeof(LONGINT));
    break;
  }
  *ArrayPtr = Memory_Alloc(*ElmtCount * ElmtSize);
  if (*ArrayPtr == NIL) {
    IO_WriteS((System_tFile)IO_StdError, (STRING)"MakeArray: out of memory", 24L);
    IO_WriteNl((System_tFile)IO_StdError);
  }
}

void DynArray_ExtendArray
# ifdef __STDC__
(ADDRESS *ArrayPtr, LONGINT *ElmtCount, LONGINT ElmtSize)
# else
(ArrayPtr, ElmtCount, ElmtSize)
ADDRESS *ArrayPtr;
LONGINT *ElmtCount;
LONGINT ElmtSize;
# endif
{
  ADDRESS NewPtr;
  LONGINT *Source, *Target;
  LONGINT i;

  ElmtSize = AlignedSize(ElmtSize);
  NewPtr = Memory_Alloc(*ElmtCount * ElmtSize * 2);
  if (NewPtr == NIL) {
    IO_WriteS((System_tFile)IO_StdError, (STRING)"ExtendArray: out of memory", 26L);
    IO_WriteNl((System_tFile)IO_StdError);
  } else {
    Source = (LONGINT *)(*ArrayPtr);
    Target = (LONGINT *)NewPtr;
    {
      LONGINT B_1 = 1, B_2 = *ElmtCount * ElmtSize / sizeof(LONGINT);

      if (B_1 <= B_2)
        for (i = B_1;; i += 1) {
          *Target = *Source;
          Source = (LONGINT *)(ADDRESS)((ADDRESS)Source + sizeof(LONGINT));
          Target = (LONGINT *)(ADDRESS)((ADDRESS)Target + sizeof(LONGINT));
          if (i >= B_2) break;
        }
    }
    Memory_Free(*ElmtCount * ElmtSize, *ArrayPtr);
    INC1(*ElmtCount, *ElmtCount);
  }
  *ArrayPtr = NewPtr;
}

void DynArray_ReleaseArray
# ifdef __STDC__
(ADDRESS *ArrayPtr, LONGINT *ElmtCount, LONGINT ElmtSize)
# else
(ArrayPtr, ElmtCount, ElmtSize)
ADDRESS *ArrayPtr;
LONGINT *ElmtCount;
LONGINT ElmtSize;
# endif
{
  ElmtSize = AlignedSize(ElmtSize);
  Memory_Free(*ElmtCount * ElmtSize, *ArrayPtr);
}

static LONGINT AlignedSize
# ifdef __STDC__
(LONGINT ElmtSize)
# else
(ElmtSize)
LONGINT ElmtSize;
# endif
{
  LONGINT Align;

  if (ElmtSize >= General_MaxAlign) {
    Align = General_MaxAlign;
  } else {
    Align = General_Exp2(General_Log2(ElmtSize + ElmtSize - 2));
  }
  return ElmtSize + Align - 1 - (ElmtSize - 1) % Align;
}

void BEGIN_DynArray()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_General();
    BEGIN_Memory();
    BEGIN_IO();

  }
}
