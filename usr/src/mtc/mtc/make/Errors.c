#include "SYSTEM_.h"

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Sets
#include "Sets.h"
#endif

#ifndef DEFINITION_Positions
#include "Positions.h"
#endif

#ifndef DEFINITION_Errors
#include "Errors.h"
#endif


typedef struct S_1 {
    CHAR A[256 + 1];
} tArray;
static CARDINAL ErrorCount;
static void WriteErrorMessage ARGS((CARDINAL ErrorCode, CARDINAL ErrorClass, Positions_tPosition Position));


CARDINAL Errors_NumberOfErrors
# ifdef __STDC__
()
# else
()
# endif
{
  return ErrorCount;
}

static void WriteErrorMessage
# ifdef __STDC__
(CARDINAL ErrorCode, CARDINAL ErrorClass, Positions_tPosition Position)
# else
(ErrorCode, ErrorClass, Position)
CARDINAL ErrorCode, ErrorClass;
Positions_tPosition Position;
# endif
{
  INC(ErrorCount);
  Positions_WritePosition(IO_StdError, Position);
  IO_WriteS(IO_StdError, ": ", 2L);
  switch (ErrorClass) {
  case Errors_Fatal:;
    IO_WriteS(IO_StdError, "Fatal      ", 11L);
    break;
  case Errors_Restriction:;
    IO_WriteS(IO_StdError, "Restriction", 11L);
    break;
  case Errors_Error:;
    IO_WriteS(IO_StdError, "Error      ", 11L);
    break;
  case Errors_Warning:;
    IO_WriteS(IO_StdError, "Warning    ", 11L);
    break;
  case Errors_Repair:;
    IO_WriteS(IO_StdError, "Repair     ", 11L);
    break;
  case Errors_Note:;
    IO_WriteS(IO_StdError, "Note       ", 11L);
    break;
  case Errors_Information:;
    IO_WriteS(IO_StdError, "Information", 11L);
    break;
  default :
    IO_WriteS(IO_StdError, "Error class: ", 13L);
    IO_WriteI(IO_StdError, ErrorClass, 0);
    break;
  }
  switch (ErrorCode) {
  case Errors_NoText:;
    break;
  case Errors_SyntaxError:;
    IO_WriteS(IO_StdError, " syntax error", 13L);
    break;
  case Errors_ExpectedTokens:;
    IO_WriteS(IO_StdError, " expected tokens:", 17L);
    break;
  case Errors_RestartPoint:;
    IO_WriteS(IO_StdError, " restart point", 14L);
    break;
  case Errors_TokenInserted:;
    IO_WriteS(IO_StdError, " token inserted :", 17L);
    break;
  case Errors_ReadParseTable:;
    IO_WriteS(IO_StdError, " error reading Pars.Tab", 23L);
    break;
  case Errors_IllegalChar:;
    IO_WriteS(IO_StdError, " illegal character", 18L);
    break;
  case Errors_UnclosedComment:;
    IO_WriteS(IO_StdError, " unclosed comment", 17L);
    break;
  case Errors_UnclosedString:;
    IO_WriteS(IO_StdError, " unclosed string", 16L);
    break;
  default :
    IO_WriteS(IO_StdError, " error code: ", 13L);
    IO_WriteI(IO_StdError, ErrorCode, 0);
    break;
  }
}

void Errors_ErrorMessage
# ifdef __STDC__
(CARDINAL ErrorCode, CARDINAL ErrorClass, Positions_tPosition Position)
# else
(ErrorCode, ErrorClass, Position)
CARDINAL ErrorCode, ErrorClass;
Positions_tPosition Position;
# endif
{
  WriteErrorMessage(ErrorCode, ErrorClass, Position);
  IO_WriteNl(IO_StdError);
  if (ErrorClass == Errors_Fatal) {
    IO_CloseIO();
    exit(1);
  }
}

void Errors_ErrorMessageI
# ifdef __STDC__
(CARDINAL ErrorCode, CARDINAL ErrorClass, Positions_tPosition Position, CARDINAL InfoClass, ADDRESS Info)
# else
(ErrorCode, ErrorClass, Position, InfoClass, Info)
CARDINAL ErrorCode, ErrorClass;
Positions_tPosition Position;
CARDINAL InfoClass;
ADDRESS Info;
# endif
{
  INTEGER *PtrToInteger;
  SHORTCARD *PtrToShort;
  LONGINT *PtrToLong;
  REAL *PtrToReal;
  BOOLEAN *PtrToBoolean;
  CHAR *PtrToCharacter;
  tArray *PtrToArray;

  WriteErrorMessage(ErrorCode, ErrorClass, Position);
  IO_WriteS(IO_StdError, " ", 1L);
  switch (InfoClass) {
  case Errors_Integer:;
    PtrToInteger = (LONGINT *)Info;
    IO_WriteI(IO_StdError, *PtrToInteger, 0);
    break;
  case Errors_Short:;
    PtrToShort = (SHORTCARD *)Info;
    IO_WriteI(IO_StdError, *PtrToShort, 0);
    break;
  case Errors_Long:;
    PtrToLong = (LONGINT *)Info;
    IO_WriteI(IO_StdError, *PtrToLong, 0);
    break;
  case Errors_Real:;
    PtrToReal = (REAL *)Info;
    IO_WriteR(IO_StdError, *PtrToReal, 1, 10, 1);
    break;
  case Errors_Boolean:;
    PtrToBoolean = (BOOLEAN *)Info;
    IO_WriteB(IO_StdError, *PtrToBoolean);
    break;
  case Errors_Character:;
    PtrToCharacter = (CHAR *)Info;
    IO_WriteC(IO_StdError, *PtrToCharacter);
    break;
  case Errors_Array:;
    PtrToArray = (tArray *)Info;
    IO_WriteS(IO_StdError, (*PtrToArray).A, 257L);
    break;
  default :
    break;
  }
  IO_WriteNl(IO_StdError);
  if (ErrorClass == Errors_Fatal) {
    IO_CloseIO();
    exit(1);
  }
}

void BEGIN_Errors()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_Positions();
    BEGIN_IO();
    BEGIN_Sets();
    BEGIN_Positions();

    ErrorCount = 0;
  }
}
