#include "SYSTEM_.h"

#ifndef DEFINITION_Sets
#include "Sets.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#define max	1000
static Sets_tSet s, t, u;
static CARDINAL i;
static IO_tFile f;


void BEGIN_MODULE()
{
  BEGIN_Sets();
  BEGIN_IO();

  Sets_MakeSet(&s, (LONGCARD)max);
  Sets_MakeSet(&t, (LONGCARD)max);
  Sets_MakeSet(&u, (LONGCARD)max);
  for (i = 2; i <= max; i += 1) {
    Sets_Include(&t, i);
  }
  Sets_AssignEmpty(&s);
  Sets_AssignElmt(&s, 1L);
  Sets_Assign(&u, t);
  Sets_Union(&s, t);
  Sets_AssignEmpty(&t);
  for (i = 0; i <= max; i += 2) {
    Sets_Include(&t, i);
  }
  Sets_Difference(&s, t);
  for (i = 0; i <= max; i += 3) {
    Sets_Exclude(&s, i);
  }
  for (i = 0; i <= max; i += 5) {
    Sets_Exclude(&s, i);
  }
  for (i = 0; i <= max; i += 7) {
    Sets_Exclude(&s, i);
  }
  for (i = 0; i <= max; i += 11) {
    Sets_Exclude(&s, i);
  }
  for (i = 0; i <= max; i += 13) {
    Sets_Exclude(&s, i);
  }
  for (i = 0; i <= max; i += 17) {
    Sets_Exclude(&s, i);
  }
  for (i = 0; i <= max; i += 19) {
    Sets_Exclude(&s, i);
  }
  for (i = 0; i <= max; i += 23) {
    Sets_Exclude(&s, i);
  }
  for (i = 0; i <= max; i += 29) {
    Sets_Exclude(&s, i);
  }
  f = IO_WriteOpen((STRING)"t", 1L);
  Sets_WriteSet(f, s);
  IO_WriteNl(f);
  IO_WriteClose(f);
  f = IO_ReadOpen((STRING)"t", 1L);
  Sets_ReadSet(f, &t);
  IO_ReadClose(f);
  Sets_WriteSet((System_tFile)IO_StdOutput, t);
  IO_WriteNl((System_tFile)IO_StdOutput);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Sets_Size(&t), 5L);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Sets_Card(&t), 5L);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Sets_Minimum(&t), 5L);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Sets_Maximum(&t), 5L);
  IO_WriteNl((System_tFile)IO_StdOutput);
  Sets_AssignEmpty(&u);
  for (i = 7; i <= max; i += 10) {
    Sets_Include(&u, i);
  }
  Sets_WriteSet((System_tFile)IO_StdOutput, u);
  IO_WriteNl((System_tFile)IO_StdOutput);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Sets_Size(&u), 5L);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Sets_Card(&u), 5L);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Sets_Minimum(&u), 5L);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Sets_Maximum(&u), 5L);
  IO_WriteNl((System_tFile)IO_StdOutput);
  Sets_Intersection(&u, t);
  Sets_WriteSet((System_tFile)IO_StdOutput, u);
  IO_WriteNl((System_tFile)IO_StdOutput);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Sets_Size(&u), 5L);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Sets_Card(&u), 5L);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Sets_Minimum(&u), 5L);
  IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Sets_Maximum(&u), 5L);
  IO_WriteNl((System_tFile)IO_StdOutput);
  Sets_ReleaseSet(&s);
  Sets_ReleaseSet(&t);
  Sets_ReleaseSet(&u);
  Sets_MakeSet(&s, 10L);
  Sets_Include(&s, 3L);
  Sets_Include(&s, 7L);
  IO_WriteNl((System_tFile)IO_StdOutput);
  IO_WriteS((System_tFile)IO_StdOutput, (STRING)"enter Size and Set like below! (Size=0 terminates)", 50L);
  IO_WriteNl((System_tFile)IO_StdOutput);
  IO_WriteS((System_tFile)IO_StdOutput, (STRING)"10 ", 3L);
  Sets_WriteSet((System_tFile)IO_StdOutput, s);
  IO_WriteNl((System_tFile)IO_StdOutput);
  Sets_ReleaseSet(&s);
  for (;;) {
    IO_WriteNl((System_tFile)IO_StdOutput);
    IO_WriteFlush((System_tFile)IO_StdOutput);
    i = IO_ReadI((System_tFile)IO_StdInput);
    if (i == 0) {
      goto EXIT_1;
    }
    Sets_MakeSet(&s, i);
    Sets_ReadSet((System_tFile)IO_StdInput, &s);
    Sets_WriteSet((System_tFile)IO_StdOutput, s);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)" Card = ", 8L);
    IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Sets_Card(&s), 0L);
    IO_WriteNl((System_tFile)IO_StdOutput);
    Sets_Complement(&s);
    Sets_WriteSet((System_tFile)IO_StdOutput, s);
    IO_WriteS((System_tFile)IO_StdOutput, (STRING)" Card = ", 8L);
    IO_WriteI((System_tFile)IO_StdOutput, (LONGINT)Sets_Card(&s), 0L);
    IO_WriteNl((System_tFile)IO_StdOutput);
    Sets_ReleaseSet(&s);
  } EXIT_1:;
  IO_CloseIO();
}
