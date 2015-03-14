#include "SYSTEM_.h"

#ifndef DEFINITION_Memory
#include "Memory.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Positions
#include "Positions.h"
#endif

#ifndef DEFINITION_StringMem
#include "StringMem.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Sets
#include "Sets.h"
#endif

#ifndef DEFINITION_Sort
#include "Sort.h"
#endif

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Errors
#include "Errors.h"
#endif

PROC Errors_Exit;

#define MaxError	100
typedef struct S_1 {
    CHAR A[255 + 1];
} tArray;
typedef struct S_2 {
    Positions_tPosition Position;
    BOOLEAN IsErrorCode;
    SHORTCARD ErrorNumber;
    SHORTCARD ErrorCode;
    SHORTCARD ErrorClass;
    SHORTCARD InfoClass;
    union {
        struct {
            char dummy;
        } V_1;
        struct {
            INTEGER vInteger;
        } V_2;
        struct {
            INTEGER vShort;
        } V_3;
        struct {
            INTEGER vLong;
        } V_4;
        struct {
            REAL vReal;
        } V_5;
        struct {
            BOOLEAN vBoolean;
        } V_6;
        struct {
            CHAR vCharacter;
        } V_7;
        struct {
            StringMem_tStringRef vString;
        } V_8;
        struct {
            StringMem_tStringRef vArray;
        } V_9;
        struct {
            Sets_tSet *vSet;
        } V_10;
        struct {
            Idents_tIdent vIdent;
        } V_11;
    } U_1;
} tError;
static struct S_3 {
    tError A[MaxError + 1];
} ErrorTable;
static INTEGER MessageCount;
static BOOLEAN IsStore;
static void (*HandleMessage) ARGS((BOOLEAN, CARDINAL, CARDINAL, Positions_tPosition, CARDINAL, ADDRESS));
static IO_tFile Out;
static void WriteHead ARGS((Positions_tPosition Position, CARDINAL ErrorClass));
static void WriteCode ARGS((CARDINAL ErrorCode));
static void WriteInfo ARGS((CARDINAL InfoClass, ADDRESS Info));
static void WriteMessage ARGS((BOOLEAN IsErrorCode, CARDINAL ErrorCode, CARDINAL ErrorClass, Positions_tPosition Position, CARDINAL InfoClass, ADDRESS Info));
static void StoreMessage ARGS((BOOLEAN pIsErrorCode, CARDINAL pErrorCode, CARDINAL pErrorClass, Positions_tPosition pPosition, CARDINAL pInfoClass, ADDRESS pInfo));
static BOOLEAN IsLess ARGS((INTEGER i, INTEGER j));
static void Swap ARGS((INTEGER i, INTEGER j));
static void yyExit ARGS(());


void Errors_ErrorMessage
# ifdef __STDC__
(CARDINAL ErrorCode, CARDINAL ErrorClass, Positions_tPosition Position)
# else
(ErrorCode, ErrorClass, Position)
CARDINAL ErrorCode, ErrorClass;
Positions_tPosition Position;
# endif
{
  (*HandleMessage)(TRUE, ErrorCode, ErrorClass, Position, (LONGCARD)Errors_None, (ADDRESS)NIL);
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
  (*HandleMessage)(TRUE, ErrorCode, ErrorClass, Position, InfoClass, Info);
}

void Errors_Message
# ifdef __STDC__
(CHAR ErrorText[], LONGCARD O_1, CARDINAL ErrorClass, Positions_tPosition Position)
# else
(ErrorText, O_1, ErrorClass, Position)
CHAR ErrorText[];
LONGCARD O_1;
CARDINAL ErrorClass;
Positions_tPosition Position;
# endif
{
  Strings_tString String;
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_1 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(ErrorText, O_1, CHAR)
  Strings_ArrayToString(ErrorText, O_1, &String);
  (*HandleMessage)(FALSE, (LONGCARD)Idents_MakeIdent(&String), ErrorClass, Position, (LONGCARD)Errors_None, (ADDRESS)NIL);
  FREE_OPEN_ARRAYS
}

void Errors_MessageI
# ifdef __STDC__
(CHAR ErrorText[], LONGCARD O_2, CARDINAL ErrorClass, Positions_tPosition Position, CARDINAL InfoClass, ADDRESS Info)
# else
(ErrorText, O_2, ErrorClass, Position, InfoClass, Info)
CHAR ErrorText[];
LONGCARD O_2;
CARDINAL ErrorClass;
Positions_tPosition Position;
CARDINAL InfoClass;
ADDRESS Info;
# endif
{
  Strings_tString String;
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_2 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(ErrorText, O_2, CHAR)
  Strings_ArrayToString(ErrorText, O_2, &String);
  (*HandleMessage)(FALSE, (LONGCARD)Idents_MakeIdent(&String), ErrorClass, Position, InfoClass, Info);
  FREE_OPEN_ARRAYS
}

static void WriteHead
# ifdef __STDC__
(Positions_tPosition Position, CARDINAL ErrorClass)
# else
(Position, ErrorClass)
Positions_tPosition Position;
CARDINAL ErrorClass;
# endif
{
  Positions_WritePosition(Out, Position);
  IO_WriteS(Out, (STRING)": ", 2L);
  switch (ErrorClass) {
  case Errors_Fatal:;
    IO_WriteS(Out, (STRING)"Fatal       ", 12L);
    break;
  case Errors_Restriction:;
    IO_WriteS(Out, (STRING)"Restriction ", 12L);
    break;
  case Errors_Error:;
    IO_WriteS(Out, (STRING)"Error       ", 12L);
    break;
  case Errors_Warning:;
    IO_WriteS(Out, (STRING)"Warning     ", 12L);
    break;
  case Errors_Repair:;
    IO_WriteS(Out, (STRING)"Repair      ", 12L);
    break;
  case Errors_Note:;
    IO_WriteS(Out, (STRING)"Note        ", 12L);
    break;
  case Errors_Information:;
    IO_WriteS(Out, (STRING)"Information ", 12L);
    break;
  default :
    IO_WriteS(Out, (STRING)"Error class: ", 13L);
    IO_WriteI(Out, (LONGINT)ErrorClass, 0L);
    break;
  }
}

static void WriteCode
# ifdef __STDC__
(CARDINAL ErrorCode)
# else
(ErrorCode)
CARDINAL ErrorCode;
# endif
{
  switch (ErrorCode) {
  case Errors_NoText:;
    break;
  case Errors_SyntaxError:;
    IO_WriteS(Out, (STRING)"syntax error", 12L);
    break;
  case Errors_ExpectedTokens:;
    IO_WriteS(Out, (STRING)"expected tokens", 15L);
    break;
  case Errors_RestartPoint:;
    IO_WriteS(Out, (STRING)"restart point", 13L);
    break;
  case Errors_TokenInserted:;
    IO_WriteS(Out, (STRING)"token inserted ", 15L);
    break;
  case Errors_WrongParseTable:;
    IO_WriteS(Out, (STRING)"parse table mismatch", 20L);
    break;
  case Errors_OpenParseTable:;
    IO_WriteS(Out, (STRING)"cannot open parse table", 23L);
    break;
  case Errors_ReadParseTable:;
    IO_WriteS(Out, (STRING)"cannot read parse table", 23L);
    break;
  case Errors_TooManyErrors:;
    IO_WriteS(Out, (STRING)"too many errors", 15L);
    break;
  default :
    IO_WriteS(Out, (STRING)" error code: ", 13L);
    IO_WriteI(Out, (LONGINT)ErrorCode, 0L);
    break;
  }
}

static void WriteInfo
# ifdef __STDC__
(CARDINAL InfoClass, ADDRESS Info)
# else
(InfoClass, Info)
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
  Strings_tString *PtrToString;
  tArray *PtrToArray;
  Idents_tIdent *PtrToIdent;

  if (InfoClass == Errors_None) {
    return;
  }
  IO_WriteS(Out, (STRING)": ", 2L);
  switch (InfoClass) {
  case Errors_Integer:;
    PtrToInteger = (LONGINT *)Info;
    IO_WriteI(Out, *PtrToInteger, 0L);
    break;
  case Errors_Short:;
    PtrToShort = (SHORTCARD *)Info;
    IO_WriteI(Out, (LONGINT)(*PtrToShort), 0L);
    break;
  case Errors_Long:;
    PtrToLong = (LONGINT *)Info;
    IO_WriteI(Out, *PtrToLong, 0L);
    break;
  case Errors_Real:;
    PtrToReal = (REAL *)Info;
    IO_WriteR(Out, *PtrToReal, 1L, 10L, 1L);
    break;
  case Errors_Boolean:;
    PtrToBoolean = (BOOLEAN *)Info;
    IO_WriteB(Out, *PtrToBoolean);
    break;
  case Errors_Character:;
    PtrToCharacter = (CHAR *)Info;
    IO_WriteC(Out, *PtrToCharacter);
    break;
  case Errors_String:;
    PtrToString = (Strings_tString *)Info;
    Strings_WriteS(Out, PtrToString);
    break;
  case Errors_Array:;
    PtrToArray = (tArray *)Info;
    IO_WriteS(Out, (*PtrToArray).A, 256L);
    break;
  case Errors_Ident:;
    PtrToIdent = (SHORTCARD *)Info;
    Idents_WriteIdent(Out, *PtrToIdent);
    break;
  default :
    break;
  }
}

static void WriteMessage
# ifdef __STDC__
(BOOLEAN IsErrorCode, CARDINAL ErrorCode, CARDINAL ErrorClass, Positions_tPosition Position, CARDINAL InfoClass, ADDRESS Info)
# else
(IsErrorCode, ErrorCode, ErrorClass, Position, InfoClass, Info)
BOOLEAN IsErrorCode;
CARDINAL ErrorCode, ErrorClass;
Positions_tPosition Position;
CARDINAL InfoClass;
ADDRESS Info;
# endif
{
  WriteHead(Position, ErrorClass);
  if (IsErrorCode) {
    WriteCode(ErrorCode);
  } else {
    Idents_WriteIdent(Out, (SHORTCARD)ErrorCode);
  }
  WriteInfo(InfoClass, Info);
  IO_WriteNl(Out);
  if (ErrorClass == Errors_Fatal && !IsStore) {
    (*Errors_Exit)();
  }
}

void Errors_WriteMessages
# ifdef __STDC__
(IO_tFile File)
# else
(File)
IO_tFile File;
# endif
{
  INTEGER i;
  ADDRESS Info;
  Strings_tString s;

  Sort_Sort(1L, MessageCount, (Sort_tProcIntIntBool)IsLess, (Sort_tProcIntInt)Swap);
  Out = File;
  {
    LONGINT B_1 = 1, B_2 = MessageCount;

    if (B_1 <= B_2)
      for (i = B_1;; i += 1) {
        {
          register tError *W_1 = &ErrorTable.A[i];

          switch (W_1->InfoClass) {
          case Errors_Integer:;
            Info = ADR(W_1->U_1.V_2.vInteger);
            break;
          case Errors_Short:;
            Info = ADR(W_1->U_1.V_3.vShort);
            break;
          case Errors_Long:;
            Info = ADR(W_1->U_1.V_4.vLong);
            break;
          case Errors_Real:;
            Info = ADR(W_1->U_1.V_5.vReal);
            break;
          case Errors_Boolean:;
            Info = ADR(W_1->U_1.V_6.vBoolean);
            break;
          case Errors_Character:;
            Info = ADR(W_1->U_1.V_7.vCharacter);
            break;
          case Errors_String:;
            StringMem_GetString(W_1->U_1.V_8.vString, &s);
            Info = ADR(s);
            break;
          case Errors_Set:;
            Info = (ADDRESS)W_1->U_1.V_10.vSet;
            break;
          case Errors_Ident:;
            Info = ADR(W_1->U_1.V_11.vIdent);
            break;
          default :
            break;
          }
          WriteMessage(W_1->IsErrorCode, (LONGCARD)W_1->ErrorCode, (LONGCARD)W_1->ErrorClass, W_1->Position, (LONGCARD)W_1->InfoClass, Info);
        }
        if (i >= B_2) break;
      }
  }
  Out = IO_StdError;
}

static void StoreMessage
# ifdef __STDC__
(BOOLEAN pIsErrorCode, CARDINAL pErrorCode, CARDINAL pErrorClass, Positions_tPosition pPosition, CARDINAL pInfoClass, ADDRESS pInfo)
# else
(pIsErrorCode, pErrorCode, pErrorClass, pPosition, pInfoClass, pInfo)
BOOLEAN pIsErrorCode;
CARDINAL pErrorCode, pErrorClass;
Positions_tPosition pPosition;
CARDINAL pInfoClass;
ADDRESS pInfo;
# endif
{
  INTEGER *PtrToInteger;
  SHORTCARD *PtrToShort;
  LONGINT *PtrToLong;
  REAL *PtrToReal;
  BOOLEAN *PtrToBoolean;
  CHAR *PtrToCharacter;
  Strings_tString *PtrToString;
  tArray *PtrToArray;
  Sets_tSet *PtrToSet;
  Idents_tIdent *PtrToIdent;
  Strings_tString s;

  if (MessageCount < MaxError) {
    INC(MessageCount);
    {
      register tError *W_2 = &ErrorTable.A[MessageCount];

      W_2->Position = pPosition;
      W_2->IsErrorCode = pIsErrorCode;
      W_2->ErrorNumber = MessageCount;
      W_2->ErrorCode = pErrorCode;
      W_2->ErrorClass = pErrorClass;
      W_2->InfoClass = pInfoClass;
      switch (W_2->InfoClass) {
      case Errors_Integer:;
        PtrToInteger = (LONGINT *)pInfo;
        W_2->U_1.V_2.vInteger = *PtrToInteger;
        break;
      case Errors_Short:;
        PtrToShort = (SHORTCARD *)pInfo;
        W_2->U_1.V_3.vShort = *PtrToShort;
        break;
      case Errors_Long:;
        PtrToLong = (LONGINT *)pInfo;
        W_2->U_1.V_4.vLong = *PtrToLong;
        break;
      case Errors_Real:;
        PtrToReal = (REAL *)pInfo;
        W_2->U_1.V_5.vReal = *PtrToReal;
        break;
      case Errors_Boolean:;
        PtrToBoolean = (BOOLEAN *)pInfo;
        W_2->U_1.V_6.vBoolean = *PtrToBoolean;
        break;
      case Errors_Character:;
        PtrToCharacter = (CHAR *)pInfo;
        W_2->U_1.V_7.vCharacter = *PtrToCharacter;
        break;
      case Errors_String:;
        PtrToString = (Strings_tString *)pInfo;
        W_2->U_1.V_8.vString = StringMem_PutString(PtrToString);
        break;
      case Errors_Array:;
        PtrToArray = (tArray *)pInfo;
        Strings_ArrayToString((*PtrToArray).A, 256L, &s);
        W_2->InfoClass = Errors_String;
        W_2->U_1.V_9.vArray = StringMem_PutString(&s);
        break;
      case Errors_Set:;
        PtrToSet = (Sets_tSet *)pInfo;
        W_2->U_1.V_10.vSet = (Sets_tSet *)Memory_Alloc((LONGINT)sizeof(Sets_tSet));
        Sets_MakeSet(W_2->U_1.V_10.vSet, Sets_Size(PtrToSet));
        Sets_Assign(W_2->U_1.V_10.vSet, *PtrToSet);
        break;
      case Errors_Ident:;
        PtrToIdent = (SHORTCARD *)pInfo;
        W_2->U_1.V_11.vIdent = *PtrToIdent;
        break;
      default :
        break;
      }
    }
  } else {
    {
      register tError *W_3 = &ErrorTable.A[MessageCount];

      W_3->IsErrorCode = TRUE;
      W_3->ErrorCode = Errors_TooManyErrors;
      W_3->ErrorClass = Errors_Restriction;
      W_3->InfoClass = Errors_None;
    }
  }
  if (pErrorClass == Errors_Fatal) {
    Errors_WriteMessages((System_tFile)IO_StdError);
    (*Errors_Exit)();
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
  INTEGER r;

  r = Positions_Compare(ErrorTable.A[i].Position, ErrorTable.A[j].Position);
  if (r == -1) {
    return TRUE;
  }
  if (r == 1) {
    return FALSE;
  }
  return ErrorTable.A[i].ErrorNumber < ErrorTable.A[j].ErrorNumber;
}

static void Swap
# ifdef __STDC__
(INTEGER i, INTEGER j)
# else
(i, j)
INTEGER i, j;
# endif
{
  tError t;

  t = ErrorTable.A[i];
  ErrorTable.A[i] = ErrorTable.A[j];
  ErrorTable.A[j] = t;
}

void Errors_StoreMessages
# ifdef __STDC__
(BOOLEAN Store)
# else
(Store)
BOOLEAN Store;
# endif
{
  if (Store) {
    HandleMessage = StoreMessage;
    MessageCount = 0;
  } else {
    HandleMessage = WriteMessage;
  }
  IsStore = Store;
}

static void yyExit
# ifdef __STDC__
()
# else
()
# endif
{
  IO_CloseIO();
  Exit(1L);
}

void BEGIN_Errors()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_Positions();
    BEGIN_IO();
    BEGIN_Memory();
    BEGIN_IO();
    BEGIN_Positions();
    BEGIN_StringMem();
    BEGIN_Strings();
    BEGIN_Idents();
    BEGIN_Sets();
    BEGIN_Sort();
    BEGIN_System();
    BEGIN_Strings();

    Errors_Exit = yyExit;
    IsStore = FALSE;
    Out = IO_StdError;
    HandleMessage = WriteMessage;
  }
}
