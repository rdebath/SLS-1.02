#include "SYSTEM_.h"

#ifndef DEFINITION_Memory
#include "Memory.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

static ADDRESS p1, p2, p3, p4;
static CARDINAL i;
static LONGINT small, best, notbest, large;
static ADDRESS AllocPrint ARGS((LONGINT n));


static ADDRESS AllocPrint
# ifdef __STDC__
(LONGINT n)
# else
(n)
LONGINT n;
# endif
{
  ADDRESS a;

  a = Memory_Alloc(n);
  IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Alloc:  n = ", 12L);
  IO_WriteLong((System_tFile)IO_StdOutput, n, 10L);
  IO_WriteS((System_tFile)IO_StdOutput, (STRING)", ADR = ", 8L);
  IO_WriteN((System_tFile)IO_StdOutput, (LONGCARD)(INTEGER)a, 8L, 16L);
  IO_WriteNl((System_tFile)IO_StdOutput);
  return a;
}

void BEGIN_MODULE()
{
  BEGIN_Memory();
  BEGIN_IO();

  for (i = 0; i <= 62; i += 1) {
    p1 = AllocPrint((LONGINT)i);
    p2 = AllocPrint((LONGINT)i);
    Memory_Free((LONGINT)i, p1);
    Memory_Free((LONGINT)i, p2);
    p3 = AllocPrint((LONGINT)i);
    p4 = AllocPrint((LONGINT)i);
    if (p3 != p2) {
      IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Alloc/Free small not inverse ", 29L);
      IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)i, 10L);
      IO_WriteNl((System_tFile)IO_StdOutput);
    }
    if (p4 != p1) {
      IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Alloc/Free small not inverse ", 29L);
      IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)i, 10L);
      IO_WriteNl((System_tFile)IO_StdOutput);
    }
  }
  small = 80;
  best = 96;
  notbest = 112;
  large = 128;
  for (;;) {
    for (i = 7; i <= 24; i += 1) {
      IO_WriteS((System_tFile)IO_StdOutput, (STRING)"        i = ", 12L);
      IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)i, 10L);
      IO_WriteNl((System_tFile)IO_StdOutput);
      p1 = AllocPrint(small);
      p2 = AllocPrint(best);
      p3 = AllocPrint(notbest);
      p4 = AllocPrint(large);
      if (p1 == NIL || p2 == NIL || p3 == NIL || p4 == NIL) {
        goto EXIT_1;
      }
      Memory_Free(large, p4);
      Memory_Free(notbest, p3);
      Memory_Free(best, p2);
      Memory_Free(small, p1);
      p1 = AllocPrint(best);
      if (p1 != p2) {
        IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Alloc/Free large not inverse ", 29L);
        IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)i, 10L);
        IO_WriteNl((System_tFile)IO_StdOutput);
      }
      p1 = AllocPrint(best);
      if (p1 != p3) {
        IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Alloc/Free large not inverse ", 29L);
        IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)i, 10L);
        IO_WriteNl((System_tFile)IO_StdOutput);
      }
      p1 = AllocPrint(best);
      if (p1 != p4) {
        IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Alloc/Free large not inverse ", 29L);
        IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)i, 10L);
        IO_WriteNl((System_tFile)IO_StdOutput);
      }
      INC1(small, small);
      INC1(best, best);
      INC1(notbest, notbest);
      INC1(large, large);
    }
  } EXIT_1:;
  IO_WriteNl((System_tFile)IO_StdOutput);
  IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Memory used: ", 13L);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Memory_MemoryUsed, 10L);
  IO_WriteNl((System_tFile)IO_StdOutput);
  IO_CloseIO();
}
