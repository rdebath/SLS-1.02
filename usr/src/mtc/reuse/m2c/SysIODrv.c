#include "SYSTEM_.h"

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_Checks
#include "Checks.h"
#endif

static struct S_1 {
    CHAR A[1023 + 1];
} b;
static System_tFile f;
static INTEGER n;


void BEGIN_MODULE()
{
  BEGIN_System();
  BEGIN_Checks();

  f = OpenInput((STRING)"Makefile", 8L);
  Checks_ErrorCheck((STRING)"OpenInput", 9L, f);
  n = Read(f, ADR(b), 1024L);
  Checks_ErrorCheck((STRING)"Read", 4L, n);
  Close(f);
  f = OpenOutput((STRING)"t", 1L);
  Checks_ErrorCheck((STRING)"OpenOutput", 10L, f);
  n = Write(f, ADR(b), 1024L);
  Checks_ErrorCheck((STRING)"Write", 5L, n);
  Close(f);
}
