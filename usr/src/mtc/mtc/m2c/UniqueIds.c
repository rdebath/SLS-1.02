#include "SYSTEM_.h"

#ifndef DEFINITION_Errors
#include "Errors.h"
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

#ifndef DEFINITION_DynArray
#include "DynArray.h"
#endif

#ifndef DEFINITION_UniqueIds
#include "UniqueIds.h"
#endif


#define Infinite	1048576
#define InitialSize	4
#define MaxKeyword	90
static struct S_1 {
    Idents_tIdent A[MaxKeyword - 1 + 1];
} KeywordTable;
static Sets_tSet ModulaIds, PervasiveIds;
static LONGINT StackPtr, StackSize;
static struct S_2 {
    Sets_tSet A[Infinite + 1];
} *IdStack;
static LONGINT KeywordCount;
static CARDINAL SetSize;
static void DefineKeyword ARGS((CHAR Name[], LONGCARD ));


BOOLEAN UniqueIds_NameConflict
# ifdef __STDC__
(UniqueIds_tIdents Idents, UniqueIds_tIdentClass IdentClass, Idents_tIdent Ident)
# else
(Idents, IdentClass, Ident)
UniqueIds_tIdents Idents;
UniqueIds_tIdentClass IdentClass;
Idents_tIdent Ident;
# endif
{
  if (Ident > SetSize) {
    Errors_CompilerError((STRING)"UniqueIds.NameConflict", 22L);
  }
  switch (IdentClass) {
  case UniqueIds_eField:;
    return Sets_IsElement((LONGCARD)Ident, &PervasiveIds);
    break;
  case UniqueIds_eConst:;
  case UniqueIds_eType:;
    return Sets_IsElement((LONGCARD)Ident, &ModulaIds);
    break;
  case UniqueIds_eProc:;
    return Sets_IsElement((LONGCARD)Ident, &PervasiveIds) || Sets_IsElement((LONGCARD)Ident, &IdStack->A[0]);
    break;
  case UniqueIds_eVar:;
    return Sets_IsElement((LONGCARD)Ident, &PervasiveIds) || Sets_IsElement((LONGCARD)Ident, &IdStack->A[StackPtr]);
    break;
  case UniqueIds_eModuleVar:;
    return Sets_IsElement((LONGCARD)Ident, &PervasiveIds) || Sets_IsElement((LONGCARD)Ident, &IdStack->A[StackPtr]) || Sets_IsElement((LONGCARD)Ident, &IdStack->A[0]);
    break;
  case UniqueIds_eKeyword:;
    return TRUE;
    break;
  }
}

UniqueIds_tIdents UniqueIds_DeclareIdent
# ifdef __STDC__
(UniqueIds_tIdents Idents, UniqueIds_tIdentClass IdentClass, Idents_tIdent Ident)
# else
(Idents, IdentClass, Ident)
UniqueIds_tIdents Idents;
UniqueIds_tIdentClass IdentClass;
Idents_tIdent Ident;
# endif
{
  if (Ident > SetSize) {
    Errors_CompilerError((STRING)"UniqueIds.DeclareIdent", 22L);
  }
  Sets_Include(&ModulaIds, (LONGCARD)Ident);
  switch (IdentClass) {
  case UniqueIds_eKeyword:;
  case UniqueIds_eConst:;
  case UniqueIds_eType:;
    Sets_Include(&PervasiveIds, (LONGCARD)Ident);
    break;
  case UniqueIds_eProc:;
    Sets_Include(&IdStack->A[0], (LONGCARD)Ident);
    break;
  case UniqueIds_eVar:;
    Sets_Include(&IdStack->A[StackPtr], (LONGCARD)Ident);
    break;
  case UniqueIds_eModuleVar:;
    Sets_Include(&IdStack->A[StackPtr], (LONGCARD)Ident);
    Sets_Include(&IdStack->A[0], (LONGCARD)Ident);
    break;
  default :
    break;
  }
  return Idents;
}

void UniqueIds_BeginUniqueIds
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT i;
  UniqueIds_tIdents Idents;

  SetSize = Idents_MaxIdent();
  Sets_MakeSet(&ModulaIds, SetSize);
  Sets_MakeSet(&PervasiveIds, SetSize);
  StackPtr = 0;
  StackSize = InitialSize;
  DynArray_MakeArray((ADDRESS *)&IdStack, &StackSize, (LONGINT)sizeof(Sets_tSet));
  {
    LONGINT B_1 = 0, B_2 = StackSize - 1;

    if (B_1 <= B_2)
      for (i = B_1;; i += 1) {
        Sets_MakeSet(&IdStack->A[i], SetSize);
        if (i >= B_2) break;
      }
  }
  Sets_AssignEmpty(&ModulaIds);
  Sets_AssignEmpty(&PervasiveIds);
  Sets_AssignEmpty(&IdStack->A[StackPtr]);
  {
    LONGINT B_3 = 1, B_4 = KeywordCount;

    if (B_3 <= B_4)
      for (i = B_3;; i += 1) {
        Idents = UniqueIds_DeclareIdent(Idents, UniqueIds_eKeyword, KeywordTable.A[i - 1]);
        if (i >= B_4) break;
      }
  }
}

void UniqueIds_CloseUniqueIds
# ifdef __STDC__
()
# else
()
# endif
{
  LONGINT i;

  Sets_ReleaseSet(&ModulaIds);
  Sets_ReleaseSet(&PervasiveIds);
  {
    LONGINT B_5 = 0, B_6 = StackSize - 1;

    if (B_5 <= B_6)
      for (i = B_5;; i += 1) {
        Sets_ReleaseSet(&IdStack->A[i]);
        if (i >= B_6) break;
      }
  }
  DynArray_ReleaseArray((ADDRESS *)&IdStack, &StackSize, (LONGINT)sizeof(Sets_tSet));
}

UniqueIds_tIdents UniqueIds_EnterProc
# ifdef __STDC__
(UniqueIds_tIdents Idents)
# else
(Idents)
UniqueIds_tIdents Idents;
# endif
{
  LONGINT OldSize, i;

  INC(StackPtr);
  if (StackPtr >= StackSize) {
    OldSize = StackSize;
    DynArray_ExtendArray((ADDRESS *)&IdStack, &StackSize, (LONGINT)sizeof(Sets_tSet));
    {
      LONGINT B_7 = OldSize, B_8 = StackSize - 1;

      if (B_7 <= B_8)
        for (i = B_7;; i += 1) {
          Sets_MakeSet(&IdStack->A[i], SetSize);
          if (i >= B_8) break;
        }
    }
  }
  Sets_AssignEmpty(&IdStack->A[StackPtr]);
  return Idents;
}

UniqueIds_tIdents UniqueIds_LeaveProc
# ifdef __STDC__
(UniqueIds_tIdents Idents)
# else
(Idents)
UniqueIds_tIdents Idents;
# endif
{
  DEC(StackPtr);
  return Idents;
}

static void DefineKeyword
# ifdef __STDC__
(CHAR Name[], LONGCARD O_1)
# else
(Name, O_1)
CHAR Name[];
LONGCARD O_1;
# endif
{
  Strings_tString String;
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_1 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(Name, O_1, CHAR)
  INC(KeywordCount);
  if (KeywordCount > MaxKeyword) {
    Errors_CompilerError((STRING)"UniqueIds.DefineKeyword", 23L);
  }
  Strings_ArrayToString(Name, O_1, &String);
  KeywordTable.A[KeywordCount - 1] = Idents_MakeIdent(&String);
  FREE_OPEN_ARRAYS
}

void BEGIN_UniqueIds()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_Idents();
    BEGIN_Errors();
    BEGIN_Idents();
    BEGIN_Strings();
    BEGIN_Sets();
    BEGIN_DynArray();

    KeywordCount = 0;
    DefineKeyword((STRING)"auto", 4L);
    DefineKeyword((STRING)"break", 5L);
    DefineKeyword((STRING)"case", 4L);
    DefineKeyword((STRING)"char", 4L);
    DefineKeyword((STRING)"continue", 8L);
    DefineKeyword((STRING)"default", 7L);
    DefineKeyword((STRING)"define", 6L);
    DefineKeyword((STRING)"do", 2L);
    DefineKeyword((STRING)"double", 6L);
    DefineKeyword((STRING)"else", 4L);
    DefineKeyword((STRING)"endif", 5L);
    DefineKeyword((STRING)"entry", 5L);
    DefineKeyword((STRING)"enum", 4L);
    DefineKeyword((STRING)"extern", 6L);
    DefineKeyword((STRING)"float", 5L);
    DefineKeyword((STRING)"for", 3L);
    DefineKeyword((STRING)"goto", 4L);
    DefineKeyword((STRING)"if", 2L);
    DefineKeyword((STRING)"ifdef", 5L);
    DefineKeyword((STRING)"ifndef", 6L);
    DefineKeyword((STRING)"include", 7L);
    DefineKeyword((STRING)"int", 3L);
    DefineKeyword((STRING)"line", 4L);
    DefineKeyword((STRING)"long", 4L);
    DefineKeyword((STRING)"register", 8L);
    DefineKeyword((STRING)"return", 6L);
    DefineKeyword((STRING)"short", 5L);
    DefineKeyword((STRING)"sizeof", 6L);
    DefineKeyword((STRING)"static", 6L);
    DefineKeyword((STRING)"struct", 6L);
    DefineKeyword((STRING)"switch", 6L);
    DefineKeyword((STRING)"typedef", 7L);
    DefineKeyword((STRING)"undef", 5L);
    DefineKeyword((STRING)"union", 5L);
    DefineKeyword((STRING)"unsigned", 8L);
    DefineKeyword((STRING)"void", 4L);
    DefineKeyword((STRING)"while", 5L);
    DefineKeyword((STRING)"TRUE", 4L);
    DefineKeyword((STRING)"FALSE", 5L);
    DefineKeyword((STRING)"NIL", 3L);
    DefineKeyword((STRING)"SHORTINT", 8L);
    DefineKeyword((STRING)"INTEGER", 7L);
    DefineKeyword((STRING)"LONGINT", 7L);
    DefineKeyword((STRING)"SHORTCARD", 9L);
    DefineKeyword((STRING)"CARDINAL", 8L);
    DefineKeyword((STRING)"LONGCARD", 8L);
    DefineKeyword((STRING)"BOOLEAN", 7L);
    DefineKeyword((STRING)"CHAR", 4L);
    DefineKeyword((STRING)"REAL", 4L);
    DefineKeyword((STRING)"LONGREAL", 8L);
    DefineKeyword((STRING)"BITSET", 6L);
    DefineKeyword((STRING)"PROC", 4L);
    DefineKeyword((STRING)"BYTE", 4L);
    DefineKeyword((STRING)"WORD", 4L);
    DefineKeyword((STRING)"ADDRESS", 7L);
    DefineKeyword((STRING)"ABS", 3L);
    DefineKeyword((STRING)"ABSSI", 5L);
    DefineKeyword((STRING)"ABSLI", 5L);
    DefineKeyword((STRING)"ABSSC", 5L);
    DefineKeyword((STRING)"ABSLC", 5L);
    DefineKeyword((STRING)"ABSR", 4L);
    DefineKeyword((STRING)"ABSLR", 5L);
    DefineKeyword((STRING)"CAP", 3L);
    DefineKeyword((STRING)"CHR", 3L);
    DefineKeyword((STRING)"FLOAT", 5L);
    DefineKeyword((STRING)"ORD", 3L);
    DefineKeyword((STRING)"TRUNC", 5L);
    DefineKeyword((STRING)"VAL", 3L);
    DefineKeyword((STRING)"ODD", 3L);
    DefineKeyword((STRING)"INC", 3L);
    DefineKeyword((STRING)"INC1", 4L);
    DefineKeyword((STRING)"DEC", 3L);
    DefineKeyword((STRING)"DEC1", 4L);
    DefineKeyword((STRING)"EXCL", 4L);
    DefineKeyword((STRING)"INCL", 4L);
    DefineKeyword((STRING)"ADR", 3L);
    DefineKeyword((STRING)"ADR1", 4L);
    DefineKeyword((STRING)"OPAQUE", 6L);
    DefineKeyword((STRING)"STRING", 6L);
    DefineKeyword((STRING)"CaseError", 9L);
    DefineKeyword((STRING)"ReturnError", 11L);
    DefineKeyword((STRING)"StackAlloc", 10L);
    DefineKeyword((STRING)"A", 1L);
    DefineKeyword((STRING)"dummy", 5L);
  }
}
