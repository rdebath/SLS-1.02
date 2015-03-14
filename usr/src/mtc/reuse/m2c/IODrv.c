#include "SYSTEM_.h"

#ifndef DEFINITION_IO
#include "IO.h"
#endif

static IO_tFile f, g;
static INTEGER i, j;
static CHAR c;
static struct S_1 {
    CHAR A[200 - 1 + 1];
} b;
static REAL r;


void BEGIN_MODULE()
{
  BEGIN_IO();

  f = IO_ReadOpen((STRING)"Makefile", 8L);
  g = IO_WriteOpen((STRING)"t", 1L);
  for (i = 1; i <= 200; i += 1) {
    c = IO_ReadC(f);
    IO_WriteC((System_tFile)IO_StdOutput, c);
    IO_WriteC(g, c);
  }
  while (!IO_EndOfFile(f)) {
    i = IO_Read(f, ADR(b), 200L);
    j = IO_Write((System_tFile)IO_StdOutput, ADR(b), i);
    j = IO_Write(g, ADR(b), i);
  }
  IO_ReadClose(f);
  IO_WriteClose(g);
  IO_WriteNl((System_tFile)IO_StdOutput);
  IO_WriteS((System_tFile)IO_StdOutput, (STRING)"enter integers, 99 will stop", 28L);
  IO_WriteNl((System_tFile)IO_StdOutput);
  do {
    IO_WriteFlush((System_tFile)IO_StdOutput);
    i = IO_ReadI((System_tFile)IO_StdInput);
    IO_WriteI((System_tFile)IO_StdOutput, i, 10L);
    IO_WriteShort((System_tFile)IO_StdOutput, (SHORTINT)i, 10L);
    IO_WriteLong((System_tFile)IO_StdOutput, i, 10L);
    IO_WriteCard((System_tFile)IO_StdOutput, (LONGCARD)i, 10L);
    c = IO_ReadC((System_tFile)IO_StdInput);
    IO_WriteC((System_tFile)IO_StdOutput, ' ');
    IO_WriteC((System_tFile)IO_StdOutput, c);
    IO_WriteNl((System_tFile)IO_StdOutput);
  } while (!(i == 99));
  IO_WriteS((System_tFile)IO_StdOutput, (STRING)"enter reals, 99 will stop", 25L);
  IO_WriteNl((System_tFile)IO_StdOutput);
  do {
    IO_WriteFlush((System_tFile)IO_StdOutput);
    r = IO_ReadR((System_tFile)IO_StdInput);
    IO_WriteR((System_tFile)IO_StdOutput, r, 2L, 12L, 3L);
    IO_WriteC((System_tFile)IO_StdOutput, ',');
    IO_WriteR((System_tFile)IO_StdOutput, r, 2L, 3L, 2L);
    IO_WriteC((System_tFile)IO_StdOutput, ',');
    IO_WriteR((System_tFile)IO_StdOutput, r, 4L, 3L, 1L);
    IO_WriteC((System_tFile)IO_StdOutput, ',');
    IO_WriteR((System_tFile)IO_StdOutput, r, 8L, 3L, 0L);
    IO_WriteC((System_tFile)IO_StdOutput, ',');
    IO_WriteR((System_tFile)IO_StdOutput, r, 8L, 1L, 0L);
    IO_WriteC((System_tFile)IO_StdOutput, ',');
    IO_WriteR((System_tFile)IO_StdOutput, r, 8L, 0L, 0L);
    IO_WriteC((System_tFile)IO_StdOutput, ',');
    IO_WriteNl((System_tFile)IO_StdOutput);
  } while (!(98.9 <= r && r <= 99.1));
  IO_WriteClose((System_tFile)IO_StdOutput);
}
