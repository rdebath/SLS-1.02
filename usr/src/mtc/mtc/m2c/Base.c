#include "SYSTEM_.h"

#ifndef DEFINITION_System
#include "System.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_DynArray
#include "DynArray.h"
#endif

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_Strings
#include "Strings.h"
#endif

#ifndef DEFINITION_Sets
#include "Sets.h"
#endif

#ifndef DEFINITION_Base
#include "Base.h"
#endif

struct Base_1 Base_SourceFile;
Strings_tString Base_MtcLibrary;

#define Infinite	65535
#define InitialSize	4
static LONGINT TablePtr, TableSize;
static struct S_1 {
    Strings_tString A[Infinite + 1];
} *LibraryTable;
static Sets_tSet Options;
struct S_5 {
    CHAR A[127 + 1];
};
static void DefineLibrary ARGS((CHAR Directory[], LONGCARD ));
static void CheckFile ARGS((CHAR FileName[], LONGCARD , BOOLEAN *Success));
struct S_7 {
    CHAR A[127 + 1];
};
struct S_8 {
    CHAR A[127 + 1];
};
static void CheckOpen ARGS((IO_tFile File, CHAR FileName[], LONGCARD ));


void Base_CheckArguments
# ifdef __STDC__
()
# else
()
# endif
{
  CARDINAL i, j;
  struct S_5 Argument;
  CHAR ch;
  IO_tFile f;

  {
    LONGCARD B_1 = 1, B_2 = GetArgCount() - 1;

    if (B_1 <= B_2)
      for (i = B_1;; i += 1) {
        GetArgument((LONGINT)i, Argument.A, 128L);
        if (Argument.A[0] == '-') {
          if (Argument.A[1] == 'd') {
            j = 2;
            for (;;) {
              ch = Argument.A[j];
              Argument.A[j - 2] = ch;
              if (ch == '\0') {
                goto EXIT_1;
              }
              INC(j);
            } EXIT_1:;
            DefineLibrary(Argument.A, 128L);
          } else if (Argument.A[1] == 'l') {
            j = 2;
            for (;;) {
              ch = Argument.A[j];
              if (ch == '\0') {
                goto EXIT_2;
              }
              Strings_Append(&Base_MtcLibrary, ch);
              INC(j);
            } EXIT_2:;
            Strings_Append(&Base_MtcLibrary, '/');
          } else {
            j = 0;
            for (;;) {
              INC(j);
              ch = Argument.A[j];
              switch (ch) {
              case '\0':;
                goto EXIT_3;
                break;
              case 'c':;
              case 'h':;
              case 'i':;
              case 'm':;
              case 'r':;
              case 't':;
              case 'w':;
                Sets_Include(&Options, ORD(ch));
                break;
              default :
                IO_WriteS((System_tFile)IO_StdError, (STRING)"mtc: illegal option '", 21L);
                IO_WriteC((System_tFile)IO_StdError, ch);
                IO_WriteS((System_tFile)IO_StdError, (STRING)"'", 1L);
                IO_WriteNl((System_tFile)IO_StdError);
                IO_CloseIO();
                Exit(1L);
                break;
              }
            } EXIT_3:;
          }
        } else {
          j = 0;
          do {
            ch = Argument.A[j];
            Base_SourceFile.A[j] = ch;
            INC(j);
          } while (!(ch == '\0'));
          f = IO_ReadOpen(Base_SourceFile.A, 128L);
          CheckOpen(f, Base_SourceFile.A, 128L);
          IO_ReadClose(f);
        }
        if (i >= B_2) break;
      }
  }
}

BOOLEAN Base_OptionIsSet
# ifdef __STDC__
(CHAR Option)
# else
(Option)
CHAR Option;
# endif
{
  return Sets_IsElement(ORD(Option), &Options);
}

static void DefineLibrary
# ifdef __STDC__
(CHAR Directory[], LONGCARD O_1)
# else
(Directory, O_1)
CHAR Directory[];
LONGCARD O_1;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_1 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(Directory, O_1, CHAR)
  if (TablePtr >= TableSize) {
    DynArray_ExtendArray((ADDRESS *)&LibraryTable, &TableSize, (LONGINT)sizeof(Strings_tString));
  }
  Strings_ArrayToString(Directory, O_1, &LibraryTable->A[TablePtr]);
  Strings_Append(&LibraryTable->A[TablePtr], '/');
  INC(TablePtr);
  FREE_OPEN_ARRAYS
}

static void CheckFile
# ifdef __STDC__
(CHAR FileName[], LONGCARD O_2, BOOLEAN *Success)
# else
(FileName, O_2, Success)
CHAR FileName[];
LONGCARD O_2;
BOOLEAN *Success;
# endif
{
  IO_tFile File;
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_2 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(FileName, O_2, CHAR)
  File = IO_ReadOpen(FileName, O_2);
  if (File < 0) {
    *Success = FALSE;
  } else {
    *Success = TRUE;
    IO_ReadClose(File);
  }
  FREE_OPEN_ARRAYS
}

void Base_CheckDefFile
# ifdef __STDC__
(Idents_tIdent ModuleName, CHAR FileName[], LONGCARD O_3, BOOLEAN *Success)
# else
(ModuleName, FileName, O_3, Success)
Idents_tIdent ModuleName;
CHAR FileName[];
LONGCARD O_3;
BOOLEAN *Success;
# endif
{
  Strings_tString BaseName, PathName;
  LONGINT TableIndex;

  Idents_GetString(ModuleName, &BaseName);
  Strings_Append(&BaseName, '.');
  Strings_Append(&BaseName, 'm');
  Strings_Append(&BaseName, 'd');
  Strings_Append(&BaseName, '\0');
  Strings_StringToArray(&BaseName, FileName, O_3);
  CheckFile(FileName, O_3, Success);
  if (*Success) {
    return;
  }
  TableIndex = 0;
  while (!*Success && TableIndex < TablePtr) {
    Strings_Assign(&PathName, &LibraryTable->A[TableIndex]);
    Strings_Concatenate(&PathName, &BaseName);
    Strings_StringToArray(&PathName, FileName, O_3);
    CheckFile(FileName, O_3, Success);
    INC(TableIndex);
  }
}

IO_tFile Base_OpenHeader
# ifdef __STDC__
(Idents_tIdent Ident)
# else
(Ident)
Idents_tIdent Ident;
# endif
{
  struct S_7 FileName;
  IO_tFile File;
  Strings_tString String;

  Idents_GetString(Ident, &String);
  Strings_Append(&String, '.');
  Strings_Append(&String, 'h');
  Strings_Append(&String, '\0');
  Strings_StringToArray(&String, FileName.A, 128L);
  File = IO_WriteOpen(FileName.A, 128L);
  CheckOpen(File, FileName.A, 128L);
  return File;
}

IO_tFile Base_OpenProgram
# ifdef __STDC__
(Idents_tIdent Ident)
# else
(Ident)
Idents_tIdent Ident;
# endif
{
  struct S_8 FileName;
  IO_tFile File;
  Strings_tString String;

  Idents_GetString(Ident, &String);
  Strings_Append(&String, '.');
  Strings_Append(&String, 'c');
  Strings_Append(&String, '\0');
  Strings_StringToArray(&String, FileName.A, 128L);
  File = IO_WriteOpen(FileName.A, 128L);
  CheckOpen(File, FileName.A, 128L);
  return File;
}

static void CheckOpen
# ifdef __STDC__
(IO_tFile File, CHAR FileName[], LONGCARD O_4)
# else
(File, FileName, O_4)
IO_tFile File;
CHAR FileName[];
LONGCARD O_4;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_4 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(FileName, O_4, CHAR)
  if (File < 0) {
    IO_WriteS((System_tFile)IO_StdError, (STRING)"mtc: cannot open file ", 22L);
    IO_WriteS((System_tFile)IO_StdError, FileName, O_4);
    IO_WriteNl((System_tFile)IO_StdError);
    IO_CloseIO();
    Exit(1L);
  }
  FREE_OPEN_ARRAYS
}

void BEGIN_Base()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_IO();
    BEGIN_Idents();
    BEGIN_Strings();
    BEGIN_System();
    BEGIN_IO();
    BEGIN_DynArray();
    BEGIN_Idents();
    BEGIN_Strings();
    BEGIN_Sets();

    Sets_MakeSet(&Options, ORD(MAX_CHAR));
    TablePtr = 0;
    TableSize = InitialSize;
    DynArray_MakeArray((ADDRESS *)&LibraryTable, &TableSize, (LONGINT)sizeof(Strings_tString));
  }
}
