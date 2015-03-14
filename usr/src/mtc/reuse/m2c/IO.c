#include "SYSTEM_.h"

#ifndef DEFINITION_General
#include "General.h"
#endif

#ifndef DEFINITION_Memory
#include "Memory.h"
#endif

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif


#define EolCh	'\n'
#define TabCh	'\t'
#define BufferSize	1024
#define MaxInt	2147483647
#define MaxPow10	1000000000
#define MaxIntDiv10	(MaxInt / 10)
typedef struct S_1 {
    struct S_2 {
        CHAR A[BufferSize + 1];
    } *Buffer;
    SHORTINT BufferIndex;
    SHORTINT BytesRead;
    BOOLEAN OpenForOutput;
    BOOLEAN EndOfFile;
    BOOLEAN FlushLine;
} BufferDescriptor;
static struct S_3 {
    BufferDescriptor A[System_cMaxFile - -1 + 1];
} BufferPool;
static IO_tFile i;
static struct S_4 {
    CHAR A[15 + 1];
} MyCHR;
static void FillBuffer ARGS((IO_tFile f));
struct S_9 {
    CHAR A[100000000 + 1];
};
static void CheckFlushLine ARGS((IO_tFile f));
struct S_10 {
    CHAR A[100000000 + 1];
};
struct S_11 {
    CHAR A[10 + 1];
};
#define StartIndex	100
struct S_12 {
    CARDINAL A[200 + 1];
};
struct S_13 {
    CHAR A[32 + 1];
};
struct S_14 {
    CHAR A[10 + 1];
};


static void FillBuffer
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  {
    register BufferDescriptor *W_1 = &BufferPool.A[f - -1];

    if (W_1->FlushLine) {
      IO_WriteFlush((System_tFile)IO_StdOutput);
      IO_WriteFlush((System_tFile)IO_StdError);
    }
    W_1->BufferIndex = 0;
    W_1->BytesRead = Read(f, ADR(W_1->Buffer->A[1]), (LONGINT)BufferSize);
    if (W_1->BytesRead <= 0) {
      W_1->BytesRead = 0;
      W_1->EndOfFile = TRUE;
    }
  }
}

IO_tFile IO_ReadOpen
# ifdef __STDC__
(CHAR FileName[], LONGCARD O_1)
# else
(FileName, O_1)
CHAR FileName[];
LONGCARD O_1;
# endif
{
  IO_tFile f;

  f = OpenInput(FileName, O_1);
  {
    register BufferDescriptor *W_2 = &BufferPool.A[f - -1];

    W_2->Buffer = (struct S_2 *)Memory_Alloc((LONGINT)(BufferSize + 1));
    W_2->BufferIndex = 0;
    W_2->BytesRead = 0;
    W_2->OpenForOutput = FALSE;
    W_2->EndOfFile = FALSE;
  }
  CheckFlushLine(f);
  return f;
}

void IO_ReadClose
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  Close(f);
  {
    register BufferDescriptor *W_3 = &BufferPool.A[f - -1];

    Memory_Free((LONGINT)(BufferSize + 1), (ADDRESS)W_3->Buffer);
    W_3->Buffer = NIL;
  }
}

INTEGER IO_Read
# ifdef __STDC__
(IO_tFile f, ADDRESS Buffer, CARDINAL Size)
# else
(f, Buffer, Size)
IO_tFile f;
ADDRESS Buffer;
CARDINAL Size;
# endif
{
  struct S_9 *BufferPtr;
  CARDINAL i;

  BufferPtr = (struct S_9 *)Buffer;
  {
    register BufferDescriptor *W_4 = &BufferPool.A[f - -1];

    i = 0;
    for (;;) {
      if (i == Size) {
        return i;
      }
      if (W_4->BufferIndex == W_4->BytesRead) {
        FillBuffer(f);
        if (W_4->EndOfFile) {
          W_4->Buffer->A[1] = '\0';
        }
      }
      INC(W_4->BufferIndex);
      BufferPtr->A[i] = W_4->Buffer->A[W_4->BufferIndex];
      if (W_4->EndOfFile) {
        return i;
      }
      INC(i);
    } EXIT_1:;
  }
}

CHAR IO_ReadC
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  {
    register BufferDescriptor *W_5 = &BufferPool.A[f - -1];

    if (W_5->BufferIndex == W_5->BytesRead) {
      FillBuffer(f);
      if (W_5->EndOfFile) {
        W_5->Buffer->A[1] = '\0';
      }
    }
    INC(W_5->BufferIndex);
    return W_5->Buffer->A[W_5->BufferIndex];
  }
}

INTEGER IO_ReadI
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  INTEGER n;
  CHAR ch;
  BOOLEAN negative;

  do {
    ch = IO_ReadC(f);
  } while (!(ch != ' ' && ch != TabCh && ch != EolCh));
  switch (ch) {
  case '+':;
    negative = FALSE;
    ch = IO_ReadC(f);
    break;
  case '-':;
    negative = TRUE;
    ch = IO_ReadC(f);
    break;
  default :
    negative = FALSE;
    break;
  }
  n = 0;
  while ('0' <= ch && ch <= '9') {
    n = 10 * n + (INTEGER)(ORD(ch) - ORD('0'));
    ch = IO_ReadC(f);
  }
  DEC(BufferPool.A[f - -1].BufferIndex);
  if (negative) {
    return -n;
  } else {
    return n;
  }
}

REAL IO_ReadR
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
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

  MantissaNeg = FALSE;
  Mantissa = 0;
  Exponent = 0;
  FractionDigits = 0;
  TruncatedDigits = 0;
  do {
    ch = IO_ReadC(f);
  } while (!(ch != ' ' && ch != TabCh && ch != EolCh));
  switch (ch) {
  case '+':;
    ch = IO_ReadC(f);
    break;
  case '-':;
    ch = IO_ReadC(f);
    MantissaNeg = TRUE;
    break;
  case 'E':;
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
    ch = IO_ReadC(f);
  }
  if (ch == '.') {
    ch = IO_ReadC(f);
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
    ch = IO_ReadC(f);
  }
  if (ch == 'E') {
    ch = IO_ReadC(f);
    switch (ch) {
    case '+':;
      ExponentNeg = FALSE;
      ch = IO_ReadC(f);
      break;
    case '-':;
      ExponentNeg = TRUE;
      ch = IO_ReadC(f);
      break;
    default :
      ExponentNeg = FALSE;
      break;
    }
    while ('0' <= ch && ch <= '9') {
      Exponent = 10 * Exponent + (INTEGER)(ORD(ch) - ORD('0'));
      ch = IO_ReadC(f);
    }
    if (ExponentNeg) {
      Exponent = -Exponent;
    }
  }
  DEC(BufferPool.A[f - -1].BufferIndex);
  DEC1(Exponent, FractionDigits - TruncatedDigits);
  n = FLOAT(Mantissa) * General_Exp10(Exponent);
  if (MantissaNeg) {
    return -n;
  } else {
    return n;
  }
}

BOOLEAN IO_ReadB
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  return IO_ReadC(f) == 'T';
}

INTEGER IO_ReadN
# ifdef __STDC__
(IO_tFile f, INTEGER Base)
# else
(f, Base)
IO_tFile f;
INTEGER Base;
# endif
{
  INTEGER n;
  CHAR ch;
  INTEGER digit;

  do {
    ch = IO_ReadC(f);
  } while (!(ch != ' ' && ch != TabCh && ch != EolCh));
  n = 0;
  for (;;) {
    if ('0' <= ch && ch <= '9') {
      digit = ORD(ch) - ORD('0');
    } else if ('A' <= ch && ch <= 'F') {
      digit = ORD(ch) - ORD('A') + 10;
    } else {
      digit = 99;
    }
    if (digit >= Base) {
      goto EXIT_2;
    }
    n = Base * n + digit;
    ch = IO_ReadC(f);
  } EXIT_2:;
  DEC(BufferPool.A[f - -1].BufferIndex);
  return n;
}

void IO_ReadS
# ifdef __STDC__
(IO_tFile f, CHAR s[], LONGCARD O_2)
# else
(f, s, O_2)
IO_tFile f;
CHAR s[];
LONGCARD O_2;
# endif
{
  CARDINAL i;

  {
    register BufferDescriptor *W_6 = &BufferPool.A[f - -1];

    {
      LONGCARD B_1 = 0, B_2 = (O_2 - 1);

      if (B_1 <= B_2)
        for (i = B_1;; i += 1) {
          if (W_6->BufferIndex == W_6->BytesRead) {
            FillBuffer(f);
            if (W_6->EndOfFile) {
              W_6->Buffer->A[1] = '\0';
            }
          }
          INC(W_6->BufferIndex);
          s[i] = W_6->Buffer->A[W_6->BufferIndex];
          if (i >= B_2) break;
        }
    }
  }
}

SHORTINT IO_ReadShort
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  return IO_ReadI(f);
}

LONGINT IO_ReadLong
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  return IO_ReadI(f);
}

CARDINAL IO_ReadCard
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  return IO_ReadI(f);
}

void IO_ReadNl
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  do {
  } while (!(IO_ReadC(f) == EolCh));
}

void IO_UnRead
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  DEC(BufferPool.A[f - -1].BufferIndex);
}

BOOLEAN IO_EndOfLine
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  CHAR ch;

  {
    register BufferDescriptor *W_7 = &BufferPool.A[f - -1];

    if (W_7->BufferIndex == W_7->BytesRead) {
      FillBuffer(f);
      if (W_7->EndOfFile) {
        W_7->Buffer->A[1] = '\0';
      }
    }
    return W_7->Buffer->A[W_7->BufferIndex + 1] == EolCh;
  }
}

BOOLEAN IO_EndOfFile
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  CHAR ch;

  ch = IO_ReadC(f);
  DEC(BufferPool.A[f - -1].BufferIndex);
  return BufferPool.A[f - -1].EndOfFile;
}

static void CheckFlushLine
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  BufferPool.A[f - -1].FlushLine = IsCharacterSpecial(f);
}

IO_tFile IO_WriteOpen
# ifdef __STDC__
(CHAR FileName[], LONGCARD O_3)
# else
(FileName, O_3)
CHAR FileName[];
LONGCARD O_3;
# endif
{
  IO_tFile f;

  f = OpenOutput(FileName, O_3);
  {
    register BufferDescriptor *W_8 = &BufferPool.A[f - -1];

    W_8->Buffer = (struct S_2 *)Memory_Alloc((LONGINT)(BufferSize + 1));
    W_8->BufferIndex = 0;
    W_8->OpenForOutput = TRUE;
  }
  CheckFlushLine(f);
  return f;
}

void IO_WriteClose
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  IO_WriteFlush(f);
  Close(f);
  {
    register BufferDescriptor *W_9 = &BufferPool.A[f - -1];

    Memory_Free((LONGINT)(BufferSize + 1), (ADDRESS)W_9->Buffer);
    W_9->Buffer = NIL;
  }
}

void IO_WriteFlush
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  {
    register BufferDescriptor *W_10 = &BufferPool.A[f - -1];

    W_10->BytesRead = Write(f, ADR(W_10->Buffer->A[1]), (LONGINT)W_10->BufferIndex);
    W_10->BufferIndex = 0;
  }
}

INTEGER IO_Write
# ifdef __STDC__
(IO_tFile f, ADDRESS Buffer, INTEGER Size)
# else
(f, Buffer, Size)
IO_tFile f;
ADDRESS Buffer;
INTEGER Size;
# endif
{
  struct S_10 *BufferPtr;
  INTEGER i;

  BufferPtr = (struct S_10 *)Buffer;
  {
    register BufferDescriptor *W_11 = &BufferPool.A[f - -1];

    {
      LONGINT B_3 = 0, B_4 = Size - 1;

      if (B_3 <= B_4)
        for (i = B_3;; i += 1) {
          INC(W_11->BufferIndex);
          W_11->Buffer->A[W_11->BufferIndex] = BufferPtr->A[i];
          if (W_11->BufferIndex == BufferSize) {
            IO_WriteFlush(f);
          }
          if (i >= B_4) break;
        }
    }
  }
  return Size;
}

void IO_WriteC
# ifdef __STDC__
(IO_tFile f, CHAR c)
# else
(f, c)
IO_tFile f;
CHAR c;
# endif
{
  {
    register BufferDescriptor *W_12 = &BufferPool.A[f - -1];

    INC(W_12->BufferIndex);
    W_12->Buffer->A[W_12->BufferIndex] = c;
    if (W_12->BufferIndex == BufferSize || W_12->FlushLine && c == EolCh) {
      IO_WriteFlush(f);
    }
  }
}

void IO_WriteI
# ifdef __STDC__
(IO_tFile f, INTEGER n, CARDINAL FieldWidth)
# else
(f, n, FieldWidth)
IO_tFile f;
INTEGER n;
CARDINAL FieldWidth;
# endif
{
  INTEGER i;
  CARDINAL length;
  CARDINAL negative;
  struct S_11 digits;

  if (n < 0) {
    negative = 1;
    n = -n;
  } else {
    negative = 0;
  }
  length = 0;
  do {
    INC(length);
    digits.A[length] = MyCHR.A[n % 10];
    n = n / 10;
  } while (!(n == 0));
  {
    LONGINT B_5 = 1, B_6 = (INTEGER)(FieldWidth - length - negative);

    if (B_5 <= B_6)
      for (i = B_5;; i += 1) {
        IO_WriteC(f, ' ');
        if (i >= B_6) break;
      }
  }
  if (negative == 1) {
    IO_WriteC(f, '-');
  }
  for (i = (INTEGER)length; i >= 1; i += -1) {
    IO_WriteC(f, digits.A[i]);
  }
}

void IO_WriteR
# ifdef __STDC__
(IO_tFile f, REAL n, CARDINAL Before, CARDINAL After, CARDINAL Exp)
# else
(f, n, Before, After, Exp)
IO_tFile f;
REAL n;
CARDINAL Before, After, Exp;
# endif
{
  CARDINAL i;
  INTEGER j;
  CARDINAL FirstDigit;
  CARDINAL IntegerDigits;
  CARDINAL TotalDigits;
  CARDINAL IsNegative;
  struct S_12 Digits;
  REAL MaxCard;
  REAL MaxCardDiv10;
  LONGCARD Mantissa;
  INTEGER Exponent;

  MaxCard = FLOAT(MaxInt);
  MaxCardDiv10 = FLOAT(MaxIntDiv10);
  if (n < 0.0) {
    IsNegative = 1;
    n = -n;
  } else {
    IsNegative = 0;
  }
  if (n == 0.0) {
    Mantissa = 0;
    Exponent = 1;
  } else {
    Exponent = 10;
    while (n > MaxCard) {
      n = n / 10.0;
      INC(Exponent);
    }
    while (n <= MaxCardDiv10) {
      n = n * 10.0;
      DEC(Exponent);
    }
    Mantissa = TRUNC(n);
    if (Mantissa < MaxPow10) {
      DEC(Exponent);
    }
  }
  if (Exp > 0 || Exponent <= 0) {
    IntegerDigits = 1;
  } else {
    IntegerDigits = Exponent;
  }
  if (After == 0) {
    After = 1;
  }
  TotalDigits = IntegerDigits + After;
  FirstDigit = StartIndex;
  do {
    DEC(FirstDigit);
    Digits.A[FirstDigit] = Mantissa % 10;
    Mantissa = Mantissa / 10;
  } while (!(Mantissa == 0));
  if (Exp == 0) {
    {
      LONGINT B_7 = 1, B_8 = 1 - Exponent;

      if (B_7 <= B_8)
        for (j = B_7;; j += 1) {
          DEC(FirstDigit);
          Digits.A[FirstDigit] = 0;
          if (j >= B_8) break;
        }
    }
  }
  {
    LONGCARD B_9 = StartIndex, B_10 = FirstDigit + TotalDigits;

    if (B_9 <= B_10)
      for (i = B_9;; i += 1) {
        Digits.A[i] = 0;
        if (i >= B_10) break;
      }
  }
  Digits.A[FirstDigit - 1] = 0;
  if (Digits.A[FirstDigit + TotalDigits] >= 5) {
    i = FirstDigit + TotalDigits - 1;
    while (Digits.A[i] == 9) {
      Digits.A[i] = 0;
      DEC(i);
    }
    INC(Digits.A[i]);
    if (i == FirstDigit - 1) {
      FirstDigit = i;
      if (Exp > 0) {
        INC(Exponent);
      } else if (Exponent > 0) {
        INC(IntegerDigits);
      }
    }
  }
  {
    LONGINT B_11 = 1, B_12 = (INTEGER)(Before - IsNegative - IntegerDigits);

    if (B_11 <= B_12)
      for (j = B_11;; j += 1) {
        IO_WriteC(f, ' ');
        if (j >= B_12) break;
      }
  }
  if (IsNegative == 1) {
    IO_WriteC(f, '-');
  }
  {
    LONGCARD B_13 = 1, B_14 = IntegerDigits;

    if (B_13 <= B_14)
      for (i = B_13;; i += 1) {
        IO_WriteC(f, MyCHR.A[Digits.A[FirstDigit]]);
        INC(FirstDigit);
        if (i >= B_14) break;
      }
  }
  IO_WriteC(f, '.');
  {
    LONGCARD B_15 = 1, B_16 = After;

    if (B_15 <= B_16)
      for (i = B_15;; i += 1) {
        IO_WriteC(f, MyCHR.A[Digits.A[FirstDigit]]);
        INC(FirstDigit);
        if (i >= B_16) break;
      }
  }
  if (Exp > 0) {
    DEC(Exponent);
    IO_WriteC(f, 'E');
    if (Exponent < 0) {
      IO_WriteC(f, '-');
      Exponent = -Exponent;
    } else {
      IO_WriteC(f, '+');
    }
    IO_WriteN(f, (LONGCARD)Exponent, Exp - 1, 10L);
  }
}

void IO_WriteB
# ifdef __STDC__
(IO_tFile f, BOOLEAN b)
# else
(f, b)
IO_tFile f;
BOOLEAN b;
# endif
{
  if (b) {
    IO_WriteC(f, 'T');
  } else {
    IO_WriteC(f, 'F');
  }
}

void IO_WriteN
# ifdef __STDC__
(IO_tFile f, LONGCARD n, CARDINAL FieldWidth, CARDINAL Base)
# else
(f, n, FieldWidth, Base)
IO_tFile f;
LONGCARD n;
CARDINAL FieldWidth, Base;
# endif
{
  INTEGER i;
  CARDINAL length;
  struct S_13 digits;

  length = 0;
  do {
    INC(length);
    digits.A[length] = MyCHR.A[n % Base];
    n = n / Base;
  } while (!(n == 0));
  {
    LONGINT B_17 = 1, B_18 = (INTEGER)(FieldWidth - length);

    if (B_17 <= B_18)
      for (i = B_17;; i += 1) {
        IO_WriteC(f, '0');
        if (i >= B_18) break;
      }
  }
  for (i = (INTEGER)length; i >= 1; i += -1) {
    IO_WriteC(f, digits.A[i]);
  }
}

void IO_WriteS
# ifdef __STDC__
(IO_tFile f, CHAR s[], LONGCARD O_4)
# else
(f, s, O_4)
IO_tFile f;
CHAR s[];
LONGCARD O_4;
# endif
{
  CARDINAL i;
  CHAR c;

  {
    register BufferDescriptor *W_13 = &BufferPool.A[f - -1];

    {
      LONGCARD B_19 = 0, B_20 = (O_4 - 1);

      if (B_19 <= B_20)
        for (i = B_19;; i += 1) {
          c = s[i];
          if (c == '\0') {
            return;
          }
          INC(W_13->BufferIndex);
          W_13->Buffer->A[W_13->BufferIndex] = c;
          if (W_13->BufferIndex == BufferSize || W_13->FlushLine && c == EolCh) {
            IO_WriteFlush(f);
          }
          if (i >= B_20) break;
        }
    }
  }
}

void IO_WriteShort
# ifdef __STDC__
(IO_tFile f, SHORTINT n, CARDINAL FieldWidth)
# else
(f, n, FieldWidth)
IO_tFile f;
SHORTINT n;
CARDINAL FieldWidth;
# endif
{
  IO_WriteI(f, (LONGINT)n, FieldWidth);
}

void IO_WriteLong
# ifdef __STDC__
(IO_tFile f, LONGINT n, CARDINAL FieldWidth)
# else
(f, n, FieldWidth)
IO_tFile f;
LONGINT n;
CARDINAL FieldWidth;
# endif
{
  IO_WriteI(f, n, FieldWidth);
}

void IO_WriteCard
# ifdef __STDC__
(IO_tFile f, CARDINAL n, CARDINAL FieldWidth)
# else
(f, n, FieldWidth)
IO_tFile f;
CARDINAL n;
CARDINAL FieldWidth;
# endif
{
  INTEGER i;
  CARDINAL length;
  struct S_14 digits;

  length = 0;
  do {
    INC(length);
    digits.A[length] = MyCHR.A[n % 10];
    n = n / 10;
  } while (!(n == 0));
  {
    LONGINT B_21 = 1, B_22 = (INTEGER)(FieldWidth - length);

    if (B_21 <= B_22)
      for (i = B_21;; i += 1) {
        IO_WriteC(f, ' ');
        if (i >= B_22) break;
      }
  }
  for (i = (INTEGER)length; i >= 1; i += -1) {
    IO_WriteC(f, digits.A[i]);
  }
}

void IO_WriteNl
# ifdef __STDC__
(IO_tFile f)
# else
(f)
IO_tFile f;
# endif
{
  IO_WriteC(f, EolCh);
}

void IO_CloseIO
# ifdef __STDC__
()
# else
()
# endif
{
  IO_tFile i;

  for (i = 0; i <= System_cMaxFile; i += 1) {
    {
      register BufferDescriptor *W_14 = &BufferPool.A[i - -1];

      if (W_14->Buffer != NIL) {
        if (W_14->OpenForOutput) {
          IO_WriteClose(i);
        } else {
          IO_ReadClose(i);
        }
      }
    }
  }
}

void BEGIN_IO()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_System();
    BEGIN_General();
    BEGIN_Memory();
    BEGIN_System();

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
    for (i = 0; i <= System_cMaxFile; i += 1) {
      {
        register BufferDescriptor *W_15 = &BufferPool.A[i - -1];

        W_15->Buffer = NIL;
        W_15->BufferIndex = 0;
        W_15->BytesRead = 0;
        W_15->OpenForOutput = FALSE;
        W_15->EndOfFile = FALSE;
        W_15->FlushLine = FALSE;
      }
    }
    BufferPool.A[IO_StdInput - -1].Buffer = (struct S_2 *)Memory_Alloc((LONGINT)(BufferSize + 1));
    BufferPool.A[IO_StdOutput - -1].Buffer = (struct S_2 *)Memory_Alloc((LONGINT)(BufferSize + 1));
    BufferPool.A[IO_StdError - -1].Buffer = (struct S_2 *)Memory_Alloc((LONGINT)(BufferSize + 1));
    BufferPool.A[IO_StdInput - -1].OpenForOutput = FALSE;
    BufferPool.A[IO_StdOutput - -1].OpenForOutput = TRUE;
    BufferPool.A[IO_StdError - -1].OpenForOutput = TRUE;
    CheckFlushLine((System_tFile)IO_StdInput);
    CheckFlushLine((System_tFile)IO_StdOutput);
    CheckFlushLine((System_tFile)IO_StdError);
  }
}
