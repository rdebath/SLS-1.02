#include "SYSTEM_.h"

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_Memory
#include "Memory.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_General
#include "General.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Scanner
#include "Scanner.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Positions
#include "Positions.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Errors
#include "Errors.h"
#endif


#define cMaxError	64
#define cNoWarnings	TRUE
typedef struct S_1 {
    CHAR A[255 + 1];
} tArray;
typedef struct S_2 {
    union {
        struct {
            char dummy;
        } V_1;
        struct {
            INTEGER vInteger;
        } V_2;
        struct {
            Strings_tString *vString;
        } V_3;
        struct {
            tArray *vArray;
        } V_4;
        struct {
            Idents_tIdent vIdent;
        } V_5;
    } U_1;
    SHORTCARD InfoClass;
    SHORTCARD ErrorNumber;
    SHORTCARD ErrorCode;
    SHORTCARD ErrorClass;
    Idents_tIdent File;
    SHORTCARD Line;
    SHORTCARD Column;
} ErrorEntry;
static struct S_3 {
    ErrorEntry A[cMaxError - 1 + 1];
} ErrorTable;
static CARDINAL ErrorCount;
static struct S_4 {
    CARDINAL A[Errors_Information - Errors_Fatal + 1];
} ClassCount;
static CARDINAL ErrorClass;
static void StoreMessage ARGS((CARDINAL pErrorCode, CARDINAL pErrorClass, Idents_tIdent pFile, CARDINAL pLine, CARDINAL pColumn, CARDINAL pInfoClass, ADDRESS pInfo));
static void SortMessages ARGS((INTEGER lwb, INTEGER upb));
static BOOLEAN IsLess ARGS((INTEGER i, INTEGER j));
static void Swap ARGS((INTEGER i, INTEGER j));
static void WriteMessage ARGS((IO_tFile f, CARDINAL ErrorCode, CARDINAL ErrorClass, CARDINAL Line, CARDINAL Column));
static void WriteInfo ARGS((IO_tFile f, CARDINAL InfoClass, ADDRESS Info));


void Errors_ErrorMessageP
# ifdef __STDC__
(CARDINAL ErrorCode, CARDINAL ErrorClass, Positions_tPosition Pos)
# else
(ErrorCode, ErrorClass, Pos)
CARDINAL ErrorCode, ErrorClass;
Positions_tPosition Pos;
# endif
{
  Errors_ErrorMessagePI(ErrorCode, ErrorClass, Pos, (LONGCARD)Errors_None, (ADDRESS)NIL);
}

void Errors_ErrorMessagePI
# ifdef __STDC__
(CARDINAL ErrorCode, CARDINAL ErrorClass, Positions_tPosition Pos, CARDINAL InfoClass, ADDRESS Info)
# else
(ErrorCode, ErrorClass, Pos, InfoClass, Info)
CARDINAL ErrorCode, ErrorClass;
Positions_tPosition Pos;
CARDINAL InfoClass;
ADDRESS Info;
# endif
{
  StoreMessage(ErrorCode, ErrorClass, Pos.File, (LONGCARD)Pos.Line, (LONGCARD)Pos.Column, InfoClass, Info);
  if (ErrorClass == Errors_Fatal) {
    Errors_PrintMessages(cNoWarnings);
    IO_CloseIO();
    Exit(1L);
  }
}

void Errors_ErrorMessage
# ifdef __STDC__
(CARDINAL ErrorCode, CARDINAL ErrorClass, Positions_tPosition Pos)
# else
(ErrorCode, ErrorClass, Pos)
CARDINAL ErrorCode, ErrorClass;
Positions_tPosition Pos;
# endif
{
  Errors_ErrorMessageI(ErrorCode, ErrorClass, Pos, (LONGCARD)Errors_None, (ADDRESS)NIL);
}

void Errors_ErrorMessageI
# ifdef __STDC__
(CARDINAL ErrorCode, CARDINAL ErrorClass, Positions_tPosition Pos, CARDINAL InfoClass, ADDRESS Info)
# else
(ErrorCode, ErrorClass, Pos, InfoClass, Info)
CARDINAL ErrorCode, ErrorClass;
Positions_tPosition Pos;
CARDINAL InfoClass;
ADDRESS Info;
# endif
{
  StoreMessage(ErrorCode, ErrorClass, Pos.File, (LONGCARD)Pos.Line, (LONGCARD)Pos.Column, InfoClass, Info);
  if (ErrorClass == Errors_Fatal) {
    Errors_PrintMessages(cNoWarnings);
    IO_CloseIO();
    Exit(1L);
  }
}

void Errors_CompilerError
# ifdef __STDC__
(CHAR Proc[], LONGCARD O_1)
# else
(Proc, O_1)
CHAR Proc[];
LONGCARD O_1;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_1 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(Proc, O_1, CHAR)
  IO_WriteS((System_tFile)IO_StdError, Proc, O_1);
  IO_WriteS((System_tFile)IO_StdError, (STRING)": assertion violation", 21L);
  IO_WriteNl((System_tFile)IO_StdError);
  IO_CloseIO();
  Exit(1L);
  FREE_OPEN_ARRAYS
}

CARDINAL Errors_NumberOfErrors
# ifdef __STDC__
()
# else
()
# endif
{
  return ClassCount.A[Errors_Fatal - 1] + ClassCount.A[Errors_Restriction - 1] + ClassCount.A[Errors_Error - 1];
}

void Errors_PrintMessages
# ifdef __STDC__
(BOOLEAN NoWarnings)
# else
(NoWarnings)
BOOLEAN NoWarnings;
# endif
{
  CARDINAL MaxError, CurError, CurClass;
  Idents_tIdent PrevFile;

  if (ErrorCount == 0) {
    return;
  }
  if (NoWarnings && ErrorCount == ClassCount.A[Errors_Warning - 1]) {
    return;
  }
  MaxError = General_Min((LONGINT)cMaxError, (LONGINT)ErrorCount);
  SortMessages(1L, (LONGINT)MaxError);
  PrevFile = Idents_NoIdent;
  {
    LONGCARD B_1 = 1, B_2 = MaxError;

    if (B_1 <= B_2)
      for (CurError = B_1;; CurError += 1) {
        {
          register ErrorEntry *W_1 = &ErrorTable.A[CurError - 1];

          if (NoWarnings && W_1->ErrorClass == Errors_Warning) {
          } else {
            if (W_1->File != PrevFile) {
              Idents_WriteIdent((System_tFile)IO_StdError, W_1->File);
              IO_WriteC((System_tFile)IO_StdError, ':');
              IO_WriteNl((System_tFile)IO_StdError);
              PrevFile = W_1->File;
            }
            WriteMessage((System_tFile)IO_StdError, (LONGCARD)W_1->ErrorCode, (LONGCARD)W_1->ErrorClass, (LONGCARD)W_1->Line, (LONGCARD)W_1->Column);
            WriteInfo((System_tFile)IO_StdError, (LONGCARD)W_1->InfoClass, ADR(W_1->U_1.V_2.vInteger));
            IO_WriteNl((System_tFile)IO_StdError);
          }
        }
        if (CurError >= B_2) break;
      }
  }
  for (CurClass = Errors_Fatal; CurClass <= Errors_Information; CurClass += 1) {
    if (NoWarnings && CurClass == Errors_Warning) {
    } else {
      if (ClassCount.A[CurClass - 1] > 0) {
        IO_WriteS((System_tFile)IO_StdError, (STRING)"  ", 2L);
        IO_WriteI((System_tFile)IO_StdError, (LONGINT)ClassCount.A[CurClass - 1], 1L);
        IO_WriteC((System_tFile)IO_StdError, ' ');
        switch (CurClass) {
        case Errors_Fatal:;
          IO_WriteS((System_tFile)IO_StdError, (STRING)"fatal error(s)", 14L);
          break;
        case Errors_Restriction:;
          IO_WriteS((System_tFile)IO_StdError, (STRING)"restriction(s)", 14L);
          break;
        case Errors_Error:;
          IO_WriteS((System_tFile)IO_StdError, (STRING)"error(s)", 8L);
          break;
        case Errors_Warning:;
          IO_WriteS((System_tFile)IO_StdError, (STRING)"warning(s)", 10L);
          break;
        case Errors_Repair:;
          IO_WriteS((System_tFile)IO_StdError, (STRING)"repair(s)", 9L);
          break;
        case Errors_Note:;
          IO_WriteS((System_tFile)IO_StdError, (STRING)"note(s)", 7L);
          break;
        case Errors_Information:;
          IO_WriteS((System_tFile)IO_StdError, (STRING)"information(s)", 14L);
          break;
        }
      }
    }
  }
  IO_WriteNl((System_tFile)IO_StdError);
}

static void StoreMessage
# ifdef __STDC__
(CARDINAL pErrorCode, CARDINAL pErrorClass, Idents_tIdent pFile, CARDINAL pLine, CARDINAL pColumn, CARDINAL pInfoClass, ADDRESS pInfo)
# else
(pErrorCode, pErrorClass, pFile, pLine, pColumn, pInfoClass, pInfo)
CARDINAL pErrorCode, pErrorClass;
Idents_tIdent pFile;
CARDINAL pLine, pColumn;
CARDINAL pInfoClass;
ADDRESS pInfo;
# endif
{
  INTEGER *PtrToInteger;
  Strings_tString *PtrToString;
  tArray *PtrToArray;
  Idents_tIdent *PtrToIdent;

  INC(ClassCount.A[pErrorClass - 1]);
  if (ErrorCount < cMaxError) {
    INC(ErrorCount);
    {
      register ErrorEntry *W_2 = &ErrorTable.A[ErrorCount - 1];

      W_2->ErrorNumber = ErrorCount;
      W_2->ErrorCode = pErrorCode;
      W_2->ErrorClass = pErrorClass;
      W_2->File = pFile;
      W_2->Line = pLine;
      W_2->Column = pColumn;
      W_2->InfoClass = pInfoClass;
      switch (W_2->InfoClass) {
      case Errors_Integer:;
        PtrToInteger = (LONGINT *)pInfo;
        W_2->U_1.V_2.vInteger = *PtrToInteger;
        break;
      case Errors_String:;
        PtrToString = (Strings_tString *)pInfo;
        W_2->U_1.V_3.vString = (Strings_tString *)Memory_Alloc((LONGINT)sizeof(Strings_tString));
        *W_2->U_1.V_3.vString = *PtrToString;
        break;
      case Errors_Array:;
        PtrToArray = (tArray *)pInfo;
        W_2->U_1.V_4.vArray = (tArray *)Memory_Alloc((LONGINT)sizeof(tArray));
        *W_2->U_1.V_4.vArray = *PtrToArray;
        break;
      case Errors_Ident:;
        PtrToIdent = (SHORTCARD *)pInfo;
        W_2->U_1.V_5.vIdent = *PtrToIdent;
        break;
      default :
        break;
      }
      if (ErrorCount == cMaxError) {
        W_2->ErrorCode = Errors_TooManyErrors;
        W_2->ErrorClass = Errors_Restriction;
        W_2->InfoClass = Errors_None;
      }
    }
  } else {
    INC(ErrorCount);
  }
}

static void SortMessages
# ifdef __STDC__
(INTEGER lwb, INTEGER upb)
# else
(lwb, upb)
INTEGER lwb, upb;
# endif
{
  INTEGER i, j;

  if (lwb < upb) {
    i = lwb + 1;
    j = upb;
    do {
      while (i < upb && IsLess(i, lwb)) {
        INC(i);
      }
      while (lwb < j && IsLess(lwb, j)) {
        DEC(j);
      }
      if (i < j) {
        Swap(i, j);
      }
    } while (!(i >= j));
    Swap(lwb, j);
    SortMessages(lwb, j - 1);
    SortMessages(j + 1, upb);
  }
}

static BOOLEAN IsLess
# ifdef __STDC__
(INTEGER i, INTEGER j)
# else
(i, j)
INTEGER i, j;
# endif
{
  {
    register ErrorEntry *W_3 = &ErrorTable.A[i - 1];

    if (W_3->File < ErrorTable.A[j - 1].File) {
      return TRUE;
    }
    if (W_3->File > ErrorTable.A[j - 1].File) {
      return FALSE;
    }
    if (W_3->Line < ErrorTable.A[j - 1].Line) {
      return TRUE;
    }
    if (W_3->Line > ErrorTable.A[j - 1].Line) {
      return FALSE;
    }
    if (W_3->Column < ErrorTable.A[j - 1].Column) {
      return TRUE;
    }
    if (W_3->Column > ErrorTable.A[j - 1].Column) {
      return FALSE;
    }
    return W_3->ErrorNumber < ErrorTable.A[j - 1].ErrorNumber;
  }
}

static void Swap
# ifdef __STDC__
(INTEGER i, INTEGER j)
# else
(i, j)
INTEGER i, j;
# endif
{
  ErrorEntry t;

  t = ErrorTable.A[i - 1];
  ErrorTable.A[i - 1] = ErrorTable.A[j - 1];
  ErrorTable.A[j - 1] = t;
}

static void WriteMessage
# ifdef __STDC__
(IO_tFile f, CARDINAL ErrorCode, CARDINAL ErrorClass, CARDINAL Line, CARDINAL Column)
# else
(f, ErrorCode, ErrorClass, Line, Column)
IO_tFile f;
CARDINAL ErrorCode, ErrorClass;
CARDINAL Line, Column;
# endif
{
  IO_WriteI(f, (LONGINT)Line, 3L);
  IO_WriteS(f, (STRING)", ", 2L);
  IO_WriteI(f, (LONGINT)Column, 2L);
  IO_WriteS(f, (STRING)": ", 2L);
  switch (ErrorClass) {
  case Errors_Fatal:;
    IO_WriteS(f, (STRING)"Fatal        ", 13L);
    break;
  case Errors_Restriction:;
    IO_WriteS(f, (STRING)"Restriction  ", 13L);
    break;
  case Errors_Error:;
    IO_WriteS(f, (STRING)"Error        ", 13L);
    break;
  case Errors_Warning:;
    IO_WriteS(f, (STRING)"Warning      ", 13L);
    break;
  case Errors_Repair:;
    IO_WriteS(f, (STRING)"Repair       ", 13L);
    break;
  case Errors_Note:;
    IO_WriteS(f, (STRING)"Note         ", 13L);
    break;
  case Errors_Information:;
    IO_WriteS(f, (STRING)"Information  ", 13L);
    break;
  }
  switch (ErrorCode) {
  case Errors_TooManyErrors:;
    IO_WriteS(f, (STRING)"too many errors", 15L);
    break;
  case Errors_SyntaxError:;
    IO_WriteS(f, (STRING)"syntax error", 12L);
    break;
  case Errors_ExpectedTokens:;
    IO_WriteS(f, (STRING)"expected tokens:", 16L);
    break;
  case Errors_RestartPoint:;
    IO_WriteS(f, (STRING)"restart point", 13L);
    break;
  case Errors_TokenInserted:;
    IO_WriteS(f, (STRING)"token inserted :", 16L);
    break;
  case Errors_ReadParseTable:;
    IO_WriteS(f, (STRING)"error reading parse table", 25L);
    break;
  case Errors_IllegalChar:;
    IO_WriteS(f, (STRING)"illegal character", 17L);
    break;
  case Errors_UnclosedComment:;
    IO_WriteS(f, (STRING)"unclosed comment", 16L);
    break;
  case Errors_UnclosedString:;
    IO_WriteS(f, (STRING)"unclosed string", 15L);
    break;
  case Errors_CyclicDefMods:;
    IO_WriteS(f, (STRING)"cyclic module dependency", 24L);
    break;
  case Errors_ModNotFound:;
    IO_WriteS(f, (STRING)"cannot find definition module", 29L);
    break;
  case Errors_NoNEWPROCESS:;
    IO_WriteS(f, (STRING)"cannot translate standard procedure NEWPROCESS", 46L);
    break;
  case Errors_NoTRANSFER:;
    IO_WriteS(f, (STRING)"cannot translate standard procedure TRANSFER", 44L);
    break;
  case Errors_NoIOTRANSFER:;
    IO_WriteS(f, (STRING)"cannot translate standard procedure IOTRANSFER", 46L);
    break;
  case Errors_StructTypeReq:;
    IO_WriteS(f, (STRING)"cannot translate forward reference to non structured type", 57L);
    break;
  case Errors_OpaqueConflict:;
    IO_WriteS(f, (STRING)"cannot resolve name conflict with opaque type", 45L);
    break;
  case Errors_ForeignConflict:;
    IO_WriteS(f, (STRING)"name conflict with FOREIGN procedure", 36L);
    break;
  case Errors_Underscores:;
    IO_WriteS(f, (STRING)"use of '_' in identifiers may lead to name conflicts", 52L);
    break;
  case Errors_OutOfLongRange:;
    IO_WriteS(f, (STRING)"constant out of long range", 26L);
    break;
  default :
    IO_WriteI(f, (LONGINT)ErrorCode, 1L);
    break;
  }
}

static void WriteInfo
# ifdef __STDC__
(IO_tFile f, CARDINAL InfoClass, ADDRESS Info)
# else
(f, InfoClass, Info)
IO_tFile f;
CARDINAL InfoClass;
ADDRESS Info;
# endif
{
  INTEGER *PtrToInteger;
  Strings_tString *PtrToString;
  tArray *PtrToArray;
  Idents_tIdent *PtrToIdent;

  if (InfoClass == Errors_None) {
    return;
  }
  IO_WriteC(f, ' ');
  switch (InfoClass) {
  case Errors_Integer:;
    PtrToInteger = (LONGINT *)Info;
    IO_WriteI(f, *PtrToInteger, 1L);
    break;
  case Errors_String:;
    PtrToString = (Strings_tString *)Info;
    Strings_WriteS(f, PtrToString);
    break;
  case Errors_Array:;
    PtrToArray = (tArray *)Info;
    IO_WriteS(f, (*PtrToArray).A, 256L);
    break;
  case Errors_Ident:;
    PtrToIdent = (SHORTCARD *)Info;
    Idents_WriteIdent(f, *PtrToIdent);
    break;
  default :
    break;
  }
}

void BEGIN_Errors()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_Idents();
    BEGIN_IO();
    BEGIN_Positions();
    BEGIN_System();
    BEGIN_Memory();
    BEGIN_IO();
    BEGIN_General();
    BEGIN_Strings();
    BEGIN_Scanner();
    BEGIN_Idents();
    BEGIN_Positions();
    BEGIN_Strings();

    ErrorCount = 0;
    for (ErrorClass = Errors_Fatal; ErrorClass <= Errors_Information; ErrorClass += 1) {
      ClassCount.A[ErrorClass - 1] = 0;
    }
  }
}
