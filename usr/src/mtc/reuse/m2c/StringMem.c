#include "SYSTEM_.h"

#ifndef DEFINITION_DynArray
#include "DynArray.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_StringMem
#include "StringMem.h"
#endif


#define InitialMemorySize	(1024 * 16)
typedef struct S_1 {
    CHAR A[100000000 + 1];
} Memory;
static Memory *MemoryPtr;
static LONGINT MemorySize;
static LONGINT MemorySpaceLeft;
static LONGINT MemoryFreePtr;


StringMem_tStringRef StringMem_PutString
# ifdef __STDC__
(Strings_tString *s)
# else
(s)
Strings_tString *s;
# endif
{
  LONGINT NeededSpace;
  LONGINT OldMemorySize;
  LONGINT StartPtr;
  Strings_tStringIndex i;

  NeededSpace = s->Length + 2;
  while (MemorySpaceLeft < NeededSpace) {
    OldMemorySize = MemorySize;
    DynArray_ExtendArray((ADDRESS *)&MemoryPtr, &MemorySize, (LONGINT)sizeof(CHAR));
    INC1(MemorySpaceLeft, MemorySize - OldMemorySize);
  }
  StartPtr = MemoryFreePtr;
  MemoryPtr->A[MemoryFreePtr] = CHR(s->Length / 256);
  INC(MemoryFreePtr);
  MemoryPtr->A[MemoryFreePtr] = CHR(s->Length % 256);
  INC(MemoryFreePtr);
  {
    Strings_tStringIndex B_1 = 1, B_2 = s->Length;

    if (B_1 <= B_2)
      for (i = B_1;; i += 1) {
        MemoryPtr->A[MemoryFreePtr] = s->Chars.A[i];
        INC(MemoryFreePtr);
        if (i >= B_2) break;
      }
  }
  DEC1(MemorySpaceLeft, NeededSpace);
  return StartPtr;
}

void StringMem_GetString
# ifdef __STDC__
(StringMem_tStringRef r, Strings_tString *s)
# else
(r, s)
StringMem_tStringRef r;
Strings_tString *s;
# endif
{
  Strings_tStringIndex i;

  s->Length = StringMem_Length(r);
  INC1(r, 2);
  {
    Strings_tStringIndex B_3 = 1, B_4 = s->Length;

    if (B_3 <= B_4)
      for (i = B_3;; i += 1) {
        s->Chars.A[i] = MemoryPtr->A[r];
        INC(r);
        if (i >= B_4) break;
      }
  }
}

CARDINAL StringMem_Length
# ifdef __STDC__
(StringMem_tStringRef r)
# else
(r)
StringMem_tStringRef r;
# endif
{
  return ORD(MemoryPtr->A[r]) * 256 + ORD(MemoryPtr->A[r + 1]);
}

BOOLEAN StringMem_IsEqual
# ifdef __STDC__
(StringMem_tStringRef r, Strings_tString *s)
# else
(r, s)
StringMem_tStringRef r;
Strings_tString *s;
# endif
{
  Strings_tStringIndex i;

  INC1(r, 2);
  {
    Strings_tStringIndex B_5 = 1, B_6 = s->Length;

    if (B_5 <= B_6)
      for (i = B_5;; i += 1) {
        if (MemoryPtr->A[r] != s->Chars.A[i]) {
          return FALSE;
        }
        INC(r);
        if (i >= B_6) break;
      }
  }
  return TRUE;
}

void StringMem_WriteString
# ifdef __STDC__
(IO_tFile f, StringMem_tStringRef r)
# else
(f, r)
IO_tFile f;
StringMem_tStringRef r;
# endif
{
  StringMem_tStringRef i;

  {
    LONGINT B_7 = r + 2, B_8 = r + 1 + (StringMem_tStringRef)StringMem_Length(r);

    if (B_7 <= B_8)
      for (i = B_7;; i += 1) {
        IO_WriteC(f, MemoryPtr->A[i]);
        if (i >= B_8) break;
      }
  }
}

void StringMem_WriteStringMemory
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT StringPtr;
  LONGINT sLength;

  StringPtr = 0;
  while (StringPtr < MemoryFreePtr) {
    IO_WriteI((System_tFile)IO_StdOutput, StringPtr, 5L);
    IO_WriteC((System_tFile)IO_StdOutput, ' ');
    StringMem_WriteString((System_tFile)IO_StdOutput, StringPtr);
    IO_WriteNl((System_tFile)IO_StdOutput);
    sLength = StringMem_Length(StringPtr) + 2;
    INC1(StringPtr, sLength);
  }
  IO_WriteNl((System_tFile)IO_StdOutput);
  IO_WriteI((System_tFile)IO_StdOutput, StringPtr, 5L);
  IO_WriteS((System_tFile)IO_StdOutput, (STRING)" Bytes", 6L);
  IO_WriteNl((System_tFile)IO_StdOutput);
}

void StringMem_InitStringMemory
# ifdef __STDC__
()
# else
()
# endif
{
  MemorySpaceLeft = MemorySize;
  MemoryFreePtr = 0;
}

void BEGIN_StringMem()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_IO();
    BEGIN_Strings();
    BEGIN_DynArray();
    BEGIN_Strings();
    BEGIN_IO();

    MemorySize = InitialMemorySize;
    DynArray_MakeArray((ADDRESS *)&MemoryPtr, &MemorySize, (LONGINT)sizeof(CHAR));
    StringMem_InitStringMemory();
  }
}
