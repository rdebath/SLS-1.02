#include "SYSTEM_.h"

#ifndef DEFINITION_DynArray
#include "DynArray.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

static LONGINT i;
static CARDINAL j;
typedef struct S_1 {
    LONGINT A[100000 - 1 + 1];
} t;
static t *p;
static LONGINT s;


void BEGIN_MODULE()
{
  BEGIN_DynArray();
  BEGIN_IO();

  s = 10;
  DynArray_MakeArray((ADDRESS *)&p, &s, (LONGINT)sizeof(LONGINT));
  {
    LONGINT B_1 = 1, B_2 = s;

    if (B_1 <= B_2)
      for (i = B_1;; i += 1) {
        p->A[i - 1] = i;
        if (i >= B_2) break;
      }
  }
  for (j = 1; j <= 13; j += 1) {
    DynArray_ExtendArray((ADDRESS *)&p, &s, (LONGINT)sizeof(LONGINT));
    if (p == NIL) {
      IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Extend Error", 12L);
      IO_WriteNl((System_tFile)IO_StdOutput);
    }
    {
      LONGINT B_3 = s / 2 + 1, B_4 = s;

      if (B_3 <= B_4)
        for (i = B_3;; i += 1) {
          p->A[i - 1] = i;
          if (i >= B_4) break;
        }
    }
    {
      LONGINT B_5 = 1, B_6 = s;

      if (B_5 <= B_6)
        for (i = B_5;; i += 1) {
          if (p->A[i - 1] != i) {
            IO_WriteS((System_tFile)IO_StdOutput, (STRING)"Error j, i, p^[i] =", 19L);
            IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)j, 5L);
            IO_WriteLong((System_tFile)IO_StdOutput, i, 5L);
            IO_WriteLong((System_tFile)IO_StdOutput, p->A[i - 1], 10L);
            IO_WriteNl((System_tFile)IO_StdOutput);
          }
          if (i >= B_6) break;
        }
    }
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)"j, size = ", 10L);
    IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)j, 5L);
    IO_WriteLong((System_tFile)IO_StdOutput, s, 10L);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)" ok", 3L);
    IO_WriteNl((System_tFile)IO_StdOutput);
  }
  IO_CloseIO();
}
