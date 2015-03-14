#include "SYSTEM_.h"

#ifndef DEFINITION_Arguments
#include "Arguments.h"
#endif

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_General
#include "General.h"
#endif

INTEGER General_MaxAlign;
struct General_1 General_AlignMasks;

static struct S_1 {
    CHAR C_0_char;
    LONGREAL longreal;
} ForAlign;
static SHORTCARD argc;
static Arguments_ArgTable argv;


INTEGER General_Min
# ifdef __STDC__
(INTEGER a, INTEGER b)
# else
(a, b)
INTEGER a, b;
# endif
{
  if (a <= b) {
    return a;
  } else {
    return b;
  }
}

INTEGER General_Max
# ifdef __STDC__
(INTEGER a, INTEGER b)
# else
(a, b)
INTEGER a, b;
# endif
{
  if (a >= b) {
    return a;
  } else {
    return b;
  }
}

CARDINAL General_Log2
# ifdef __STDC__
(LONGINT x)
# else
(x)
LONGINT x;
# endif
{
  CARDINAL y;

  y = 0;
  if (x >= 65536) {
    INC1(y, 16);
    x = x / 65536;
  }
  if (x >= 256) {
    INC1(y, 8);
    x = x / 256;
  }
  if (x >= 16) {
    INC1(y, 4);
    x = x / 16;
  }
  if (x >= 4) {
    INC1(y, 2);
    x = x / 4;
  }
  if (x >= 2) {
    INC1(y, 1);
    x = x / 2;
  }
  return y;
}

CARDINAL General_AntiLog
# ifdef __STDC__
(LONGINT x)
# else
(x)
LONGINT x;
# endif
{
  CARDINAL y;

  y = 0;
  if (x % 65536 == 0) {
    INC1(y, 16);
    x = x / 65536;
  }
  if (x % 256 == 0) {
    INC1(y, 8);
    x = x / 256;
  }
  if (x % 16 == 0) {
    INC1(y, 4);
    x = x / 16;
  }
  if (x % 4 == 0) {
    INC1(y, 2);
    x = x / 4;
  }
  if (x % 2 == 0) {
    INC1(y, 1);
    x = x / 2;
  }
  return y;
}

LONGINT General_Exp2
# ifdef __STDC__
(CARDINAL x)
# else
(x)
CARDINAL x;
# endif
{
  LONGINT y;

  y = 1;
  if (x >= 16) {
    DEC1(x, 16);
    y = y * 65536;
  }
  if (x >= 8) {
    DEC1(x, 8);
    y = y * 256;
  }
  if (x >= 4) {
    DEC1(x, 4);
    y = y * 16;
  }
  if (x >= 2) {
    DEC1(x, 2);
    y = y * 4;
  }
  if (x >= 1) {
    DEC1(x, 1);
    y = y * 2;
  }
  return y;
}

REAL General_Exp10
# ifdef __STDC__
(INTEGER x)
# else
(x)
INTEGER x;
# endif
{
  REAL y;
  BOOLEAN negative;

  negative = x < 0;
  x = ABSLI(x);
  y = 1.0;
  if (x >= 16) {
    DEC1(x, 16);
    y = y * 1.0E16;
  }
  if (x >= 16) {
    DEC1(x, 16);
    y = y * 1.0E16;
  }
  if (x >= 8) {
    DEC1(x, 8);
    y = y * 1.0E8;
  }
  if (x >= 4) {
    DEC1(x, 4);
    y = y * 1.0E4;
  }
  if (x >= 2) {
    DEC1(x, 2);
    y = y * 1.0E2;
  }
  if (x >= 1) {
    DEC1(x, 1);
    y = y * 1.0E1;
  }
  if (negative) {
    return 1.0 / y;
  } else {
    return y;
  }
}

void BEGIN_General()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_Arguments();
    BEGIN_System();

    General_MaxAlign = (CARDINAL)ADR(ForAlign.longreal) - (CARDINAL)ADR(ForAlign.C_0_char);
    General_AlignMasks.A[1] = (BITSET)(LONGCARD)0XFFFFFFFF;
    General_AlignMasks.A[2] = (BITSET)(LONGCARD)0XFFFFFFFE;
    General_AlignMasks.A[3] = (BITSET)(LONGCARD)0XFFFFFFFF;
    General_AlignMasks.A[4] = (BITSET)(LONGCARD)0XFFFFFFFC;
    General_AlignMasks.A[5] = (BITSET)(LONGCARD)0XFFFFFFFF;
    General_AlignMasks.A[6] = (BITSET)(LONGCARD)0XFFFFFFFF;
    General_AlignMasks.A[7] = (BITSET)(LONGCARD)0XFFFFFFFF;
    General_AlignMasks.A[8] = (BITSET)(LONGCARD)0XFFFFFFF8;
    GetArgs(&argc, &argv);
    PutArgs((LONGINT)argc, (ADDRESS)argv);
  }
}
