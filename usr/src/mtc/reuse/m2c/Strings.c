#include "SYSTEM_.h"

#ifndef DEFINITION_General
#include "General.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif


#define EolCh	'\n'
static BOOLEAN reported;
static struct S_1 {
    CHAR A[15 + 1];
} MyCHR;
static void Error ARGS(());
static CARDINAL Rank ARGS((CHAR ch));
#define MaxInt	2147483647
#define MaxIntDiv10	(MaxInt / 10)
struct S_4 {
    CHAR A[10 + 1];
};


static void Error
# ifdef __STDC__
()
# else
()
# endif
{
  IO_WriteS((System_tFile)IO_StdError, (STRING)"string too long, max. 255", 25L);
  IO_WriteNl((System_tFile)IO_StdError);
}

void Strings_Assign
# ifdef __STDC__
(Strings_tString *s1, Strings_tString *s2)
# else
(s1, s2)
Strings_tString *s1, *s2;
# endif
{
  Strings_tStringIndex i;

  {
    register Strings_tString *W_1 = s2;

    {
      Strings_tStringIndex B_1 = 1, B_2 = W_1->Length;

      if (B_1 <= B_2)
        for (i = B_1;; i += 1) {
          s1->Chars.A[i] = W_1->Chars.A[i];
          if (i >= B_2) break;
        }
    }
    s1->Length = W_1->Length;
  }
}

void Strings_AssignEmpty
# ifdef __STDC__
(Strings_tString *s)
# else
(s)
Strings_tString *s;
# endif
{
  s->Length = 0;
  reported = FALSE;
}

void Strings_Concatenate
# ifdef __STDC__
(Strings_tString *s1, Strings_tString *s2)
# else
(s1, s2)
Strings_tString *s1, *s2;
# endif
{
  Strings_tStringIndex i;

  if (s1->Length + s2->Length > Strings_cMaxStrLength) {
    Error();
  } else {
    {
      register Strings_tString *W_2 = s1;

      {
        Strings_tStringIndex B_3 = 1, B_4 = s2->Length;

        if (B_3 <= B_4)
          for (i = B_3;; i += 1) {
            W_2->Chars.A[W_2->Length + i] = s2->Chars.A[i];
            if (i >= B_4) break;
          }
      }
      INC1(W_2->Length, s2->Length);
    }
  }
}

void Strings_Append
# ifdef __STDC__
(Strings_tString *s, CHAR c)
# else
(s, c)
Strings_tString *s;
CHAR c;
# endif
{
  if (s->Length == Strings_cMaxStrLength) {
    if (!reported) {
      Error();
      reported = TRUE;
    }
  } else {
    INC(s->Length);
    s->Chars.A[s->Length] = c;
  }
}

CARDINAL Strings_Length
# ifdef __STDC__
(Strings_tString *s)
# else
(s)
Strings_tString *s;
# endif
{
  return s->Length;
}

BOOLEAN Strings_IsEqual
# ifdef __STDC__
(Strings_tString *s1, Strings_tString *s2)
# else
(s1, s2)
Strings_tString *s1, *s2;
# endif
{
  Strings_tStringIndex i;

  if (s1->Length != s2->Length) {
    return FALSE;
  } else {
    {
      Strings_tStringIndex B_5 = 1, B_6 = s1->Length;

      if (B_5 <= B_6)
        for (i = B_5;; i += 1) {
          if (s1->Chars.A[i] != s2->Chars.A[i]) {
            return FALSE;
          }
          if (i >= B_6) break;
        }
    }
  }
  return TRUE;
}

BOOLEAN Strings_IsInOrder
# ifdef __STDC__
(Strings_tString *s1, Strings_tString *s2)
# else
(s1, s2)
Strings_tString *s1, *s2;
# endif
{
  INTEGER i;
  CARDINAL rank1, rank2;

  {
    LONGINT B_7 = 1, B_8 = General_Min((LONGINT)s1->Length, (LONGINT)s2->Length);

    if (B_7 <= B_8)
      for (i = B_7;; i += 1) {
        rank1 = Rank(s1->Chars.A[i]);
        rank2 = Rank(s2->Chars.A[i]);
        if (rank1 < rank2) {
          return TRUE;
        } else if (rank1 > rank2) {
          return FALSE;
        }
        if (i >= B_8) break;
      }
  }
  return s1->Length <= s2->Length;
}

static CARDINAL Rank
# ifdef __STDC__
(CHAR ch)
# else
(ch)
CHAR ch;
# endif
{
  return ORD(ch);
}

void Strings_Exchange
# ifdef __STDC__
(Strings_tString *s1, Strings_tString *s2)
# else
(s1, s2)
Strings_tString *s1, *s2;
# endif
{
  Strings_tString temp;

  Strings_Assign(&temp, s1);
  Strings_Assign(s1, s2);
  Strings_Assign(s2, &temp);
}

void Strings_SubString
# ifdef __STDC__
(Strings_tString *s1, Strings_tStringIndex from, Strings_tStringIndex to, Strings_tString *s2)
# else
(s1, from, to, s2)
Strings_tString *s1;
Strings_tStringIndex from, to;
Strings_tString *s2;
# endif
{
  Strings_tStringIndex i;

  {
    register Strings_tString *W_3 = s2;

    W_3->Length = 0;
    {
      Strings_tStringIndex B_9 = from, B_10 = to;

      if (B_9 <= B_10)
        for (i = B_9;; i += 1) {
          INC(W_3->Length);
          W_3->Chars.A[W_3->Length] = s1->Chars.A[i];
          if (i >= B_10) break;
        }
    }
  }
}

CHAR Strings_Char
# ifdef __STDC__
(Strings_tString *s, Strings_tStringIndex i)
# else
(s, i)
Strings_tString *s;
Strings_tStringIndex i;
# endif
{
  return s->Chars.A[i];
}

void Strings_ArrayToString
# ifdef __STDC__
(CHAR a[], LONGCARD O_1, Strings_tString *s)
# else
(a, O_1, s)
CHAR a[];
LONGCARD O_1;
Strings_tString *s;
# endif
{
  Strings_tStringIndex i;
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_1 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(a, O_1, CHAR)
  i = 0;
  for (;;) {
    if (a[i] == '\0') {
      goto EXIT_1;
    }
    s->Chars.A[i + 1] = a[i];
    INC(i);
    if (i > (O_1 - 1)) {
      goto EXIT_1;
    }
  } EXIT_1:;
  s->Length = i;
  FREE_OPEN_ARRAYS
}

void Strings_StringToArray
# ifdef __STDC__
(Strings_tString *s, CHAR a[], LONGCARD O_2)
# else
(s, a, O_2)
Strings_tString *s;
CHAR a[];
LONGCARD O_2;
# endif
{
  Strings_tStringIndex i;

  {
    Strings_tStringIndex B_11 = 1, B_12 = s->Length;

    if (B_11 <= B_12)
      for (i = B_11;; i += 1) {
        a[i - 1] = s->Chars.A[i];
        if (i >= B_12) break;
      }
  }
  a[s->Length] = '\0';
}

INTEGER Strings_StringToInt
# ifdef __STDC__
(Strings_tString *s)
# else
(s)
Strings_tString *s;
# endif
{
  Strings_tStringIndex i, start;
  INTEGER n;
  BOOLEAN negative;

  switch (s->Chars.A[1]) {
  case '+':;
    negative = FALSE;
    start = 2;
    break;
  case '-':;
    negative = TRUE;
    start = 2;
    break;
  default :
    negative = FALSE;
    start = 1;
    break;
  }
  n = 0;
  {
    Strings_tStringIndex B_13 = start, B_14 = s->Length;

    if (B_13 <= B_14)
      for (i = B_13;; i += 1) {
        n = n * 10 + (INTEGER)(ORD(s->Chars.A[i]) - ORD('0'));
        if (i >= B_14) break;
      }
  }
  if (negative) {
    return -n;
  } else {
    return n;
  }
}

CARDINAL Strings_StringToNumber
# ifdef __STDC__
(Strings_tString *s, CARDINAL Base)
# else
(s, Base)
Strings_tString *s;
CARDINAL Base;
# endif
{
  Strings_tStringIndex i;
  CARDINAL n;
  CHAR ch;

  n = 0;
  {
    Strings_tStringIndex B_15 = 1, B_16 = s->Length;

    if (B_15 <= B_16)
      for (i = B_15;; i += 1) {
        ch = s->Chars.A[i];
        if ('A' <= ch && ch <= 'F') {
          n = n * Base + ORD(ch) - ORD('A') + 10;
        } else if ('a' <= ch && ch <= 'f') {
          n = n * Base + ORD(ch) - ORD('a') + 10;
        } else {
          n = n * Base + ORD(ch) - ORD('0');
        }
        if (i >= B_16) break;
      }
  }
  return n;
}

REAL Strings_StringToReal
# ifdef __STDC__
(Strings_tString *s)
# else
(s)
Strings_tString *s;
# endif
{
  REAL n;
  LONGCARD Mantissa;
  INTEGER Exponent;
  BOOLEAN MantissaNeg;
  BOOLEAN ExponentNeg;
  CARDINAL FractionDigits;
  CARDINAL TruncatedDigits;
  CHAR ch;
  Strings_tStringIndex i;

  MantissaNeg = FALSE;
  Mantissa = 0;
  Exponent = 0;
  FractionDigits = 0;
  TruncatedDigits = 0;
  i = 0;
  Strings_Append(s, ' ');
  INC(i);
  ch = s->Chars.A[i];
  switch (ch) {
  case '+':;
    INC(i);
    ch = s->Chars.A[i];
    break;
  case '-':;
    INC(i);
    ch = s->Chars.A[i];
    MantissaNeg = TRUE;
    break;
  case 'E':;
  case 'e':;
    Mantissa = 1;
    break;
  default :
    break;
  }
  while ('0' <= ch && ch <= '9') {
    if (Mantissa <= MaxIntDiv10) {
      Mantissa = 10 * Mantissa;
      if (Mantissa <= MaxInt - (ORD(ch) - ORD('0'))) {
        INC1(Mantissa, ORD(ch) - ORD('0'));
      } else {
        INC(TruncatedDigits);
      }
    } else {
      INC(TruncatedDigits);
    }
    INC(i);
    ch = s->Chars.A[i];
  }
  if (ch == '.') {
    INC(i);
    ch = s->Chars.A[i];
  }
  while ('0' <= ch && ch <= '9') {
    if (Mantissa <= MaxIntDiv10) {
      Mantissa = 10 * Mantissa;
      if (Mantissa <= MaxInt - (ORD(ch) - ORD('0'))) {
        INC1(Mantissa, ORD(ch) - ORD('0'));
      } else {
        INC(TruncatedDigits);
      }
    } else {
      INC(TruncatedDigits);
    }
    INC(FractionDigits);
    INC(i);
    ch = s->Chars.A[i];
  }
  if (ch == 'E') {
    INC(i);
    ch = s->Chars.A[i];
    switch (ch) {
    case '+':;
      ExponentNeg = FALSE;
      INC(i);
      ch = s->Chars.A[i];
      break;
    case '-':;
      ExponentNeg = TRUE;
      INC(i);
      ch = s->Chars.A[i];
      break;
    default :
      ExponentNeg = FALSE;
      break;
    }
    while ('0' <= ch && ch <= '9') {
      Exponent = 10 * Exponent + (INTEGER)(ORD(ch) - ORD('0'));
      INC(i);
      ch = s->Chars.A[i];
    }
    if (ExponentNeg) {
      Exponent = -Exponent;
    }
  }
  DEC1(Exponent, FractionDigits - TruncatedDigits);
  n = FLOAT(Mantissa) * General_Exp10(Exponent);
  if (MantissaNeg) {
    return -n;
  } else {
    return n;
  }
}

void Strings_IntToString
# ifdef __STDC__
(INTEGER n, Strings_tString *s)
# else
(n, s)
INTEGER n;
Strings_tString *s;
# endif
{
  INTEGER i, j;
  CARDINAL length;
  struct S_4 digits;

  if (n < 0) {
    s->Chars.A[1] = '-';
    j = 1;
    n = -n;
  } else {
    j = 0;
  }
  length = 0;
  do {
    INC(length);
    digits.A[length] = MyCHR.A[n % 10];
    n = n / 10;
  } while (!(n == 0));
  for (i = (INTEGER)length; i >= 1; i += -1) {
    INC(j);
    s->Chars.A[j] = digits.A[i];
  }
  s->Length = j;
}

void Strings_ReadS
# ifdef __STDC__
(IO_tFile f, Strings_tString *s, Strings_tStringIndex FieldWidth)
# else
(f, s, FieldWidth)
IO_tFile f;
Strings_tString *s;
Strings_tStringIndex FieldWidth;
# endif
{
  Strings_tStringIndex i;

  {
    Strings_tStringIndex B_17 = 1, B_18 = FieldWidth;

    if (B_17 <= B_18)
      for (i = B_17;; i += 1) {
        s->Chars.A[i] = IO_ReadC(f);
        if (i >= B_18) break;
      }
  }
  s->Length = FieldWidth;
}

void Strings_ReadL
# ifdef __STDC__
(IO_tFile f, Strings_tString *s)
# else
(f, s)
IO_tFile f;
Strings_tString *s;
# endif
{
  Strings_tStringIndex i;
  CHAR ch;

  i = 0;
  for (;;) {
    ch = IO_ReadC(f);
    if (ch == EolCh) {
      goto EXIT_2;
    }
    if (i == Strings_cMaxStrLength) {
      do {
      } while (!(IO_ReadC(f) == EolCh));
      goto EXIT_2;
    }
    INC(i);
    s->Chars.A[i] = ch;
  } EXIT_2:;
  s->Length = i;
}

void Strings_WriteS
# ifdef __STDC__
(IO_tFile f, Strings_tString *s)
# else
(f, s)
IO_tFile f;
Strings_tString *s;
# endif
{
  Strings_tStringIndex i;

  {
    Strings_tStringIndex B_19 = 1, B_20 = s->Length;

    if (B_19 <= B_20)
      for (i = B_19;; i += 1) {
        IO_WriteC(f, s->Chars.A[i]);
        if (i >= B_20) break;
      }
  }
}

void Strings_WriteL
# ifdef __STDC__
(IO_tFile f, Strings_tString *s)
# else
(f, s)
IO_tFile f;
Strings_tString *s;
# endif
{
  Strings_WriteS(f, s);
  IO_WriteNl(f);
}

void BEGIN_Strings()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_IO();
    BEGIN_General();
    BEGIN_IO();
    BEGIN_IO();

    MyCHR.A[0] = '0';
    MyCHR.A[1] = '1';
    MyCHR.A[2] = '2';
    MyCHR.A[3] = '3';
    MyCHR.A[4] = '4';
    MyCHR.A[5] = '5';
    MyCHR.A[6] = '6';
    MyCHR.A[7] = '7';
    MyCHR.A[8] = '8';
    MyCHR.A[9] = '9';
    MyCHR.A[10] = 'A';
    MyCHR.A[11] = 'B';
    MyCHR.A[12] = 'C';
    MyCHR.A[13] = 'D';
    MyCHR.A[14] = 'E';
    MyCHR.A[15] = 'F';
  }
}
