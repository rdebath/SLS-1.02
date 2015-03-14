#include "SYSTEM_.h"

#ifndef DEFINITION_Sets
#include "Sets.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_SetsC
#include "SetsC.h"
#endif


static void CheckSetSet ARGS((SetsC_tSet *Set1, SetsC_tSet *Set2, CHAR Name[], LONGCARD ));
static void CheckSetElmt ARGS((SetsC_tSet *Set, INTEGER Elmt, CHAR Name[], LONGCARD ));
static void CheckSet ARGS((SetsC_tSet *Set, CHAR Name[], LONGCARD ));
static void CheckNotEmpty ARGS((SetsC_tSet *Set, CHAR Name[], LONGCARD ));
static void PrintSet ARGS((SetsC_tSet Set));


static void CheckSetSet
# ifdef __STDC__
(SetsC_tSet *Set1, SetsC_tSet *Set2, CHAR Name[], LONGCARD O_1)
# else
(Set1, Set2, Name, O_1)
SetsC_tSet *Set1, *Set2;
CHAR Name[];
LONGCARD O_1;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_1 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(Name, O_1, CHAR)
  CheckSet(Set1, Name, O_1);
  CheckSet(Set2, Name, O_1);
  if (Set1->MaxElmt != Set2->MaxElmt) {
    IO_WriteS((System_tFile)IO_StdError, (STRING)"Sets.", 5L);
    IO_WriteS((System_tFile)IO_StdError, Name, O_1);
    IO_WriteS((System_tFile)IO_StdError, (STRING)": incompatible sets", 19L);
    IO_WriteNl((System_tFile)IO_StdError);
    PrintSet(*Set1);
    PrintSet(*Set2);
  }
  FREE_OPEN_ARRAYS
}

static void CheckSetElmt
# ifdef __STDC__
(SetsC_tSet *Set, INTEGER Elmt, CHAR Name[], LONGCARD O_2)
# else
(Set, Elmt, Name, O_2)
SetsC_tSet *Set;
INTEGER Elmt;
CHAR Name[];
LONGCARD O_2;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_2 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(Name, O_2, CHAR)
  CheckSet(Set, Name, O_2);
  if (Elmt < 0) {
    IO_WriteS((System_tFile)IO_StdError, (STRING)"Sets.", 5L);
    IO_WriteS((System_tFile)IO_StdError, Name, O_2);
    IO_WriteS((System_tFile)IO_StdError, (STRING)": negative element: ", 20L);
    IO_WriteI((System_tFile)IO_StdError, Elmt, 0L);
    IO_WriteNl((System_tFile)IO_StdError);
    PrintSet(*Set);
  }
  if (Elmt > (SHORTINT)Set->MaxElmt) {
    IO_WriteS((System_tFile)IO_StdError, (STRING)"Sets.", 5L);
    IO_WriteS((System_tFile)IO_StdError, Name, O_2);
    IO_WriteS((System_tFile)IO_StdError, (STRING)": element out of range: ", 24L);
    IO_WriteI((System_tFile)IO_StdError, Elmt, 0L);
    IO_WriteNl((System_tFile)IO_StdError);
    PrintSet(*Set);
  }
  FREE_OPEN_ARRAYS
}

static void CheckSet
# ifdef __STDC__
(SetsC_tSet *Set, CHAR Name[], LONGCARD O_3)
# else
(Set, Name, O_3)
SetsC_tSet *Set;
CHAR Name[];
LONGCARD O_3;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_3 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(Name, O_3, CHAR)
  if (Set->BitsetPtr == NIL) {
    IO_WriteS((System_tFile)IO_StdError, (STRING)"Sets.", 5L);
    IO_WriteS((System_tFile)IO_StdError, Name, O_3);
    IO_WriteS((System_tFile)IO_StdError, (STRING)": set probably not initialized", 30L);
    IO_WriteNl((System_tFile)IO_StdError);
    PrintSet(*Set);
  }
  FREE_OPEN_ARRAYS
}

static void CheckNotEmpty
# ifdef __STDC__
(SetsC_tSet *Set, CHAR Name[], LONGCARD O_4)
# else
(Set, Name, O_4)
SetsC_tSet *Set;
CHAR Name[];
LONGCARD O_4;
# endif
{
  OPEN_ARRAY_LOCALS

  ALLOC_OPEN_ARRAYS(O_4 * sizeof(CHAR), 1)
  COPY_OPEN_ARRAY(Name, O_4, CHAR)
  CheckSet(Set, Name, O_4);
  if (Sets_IsEmpty(*Set)) {
    IO_WriteS((System_tFile)IO_StdError, (STRING)"Sets.", 5L);
    IO_WriteS((System_tFile)IO_StdError, Name, O_4);
    IO_WriteS((System_tFile)IO_StdError, (STRING)": applied to empty set", 22L);
    IO_WriteNl((System_tFile)IO_StdError);
    PrintSet(*Set);
  }
  FREE_OPEN_ARRAYS
}

static void PrintSet
# ifdef __STDC__
(SetsC_tSet Set)
# else
(Set)
SetsC_tSet Set;
# endif
{
  {
    register Sets_tSet *W_1 = &Set;

    IO_WriteS((System_tFile)IO_StdError, (STRING)"BitsetPtr = ", 12L);
    IO_WriteN((System_tFile)IO_StdError, (LONGCARD)(INTEGER)W_1->BitsetPtr, 0L, 16L);
    IO_WriteNl((System_tFile)IO_StdError);
    IO_WriteS((System_tFile)IO_StdError, (STRING)"MaxElmt   = ", 12L);
    IO_WriteI((System_tFile)IO_StdError, (LONGINT)W_1->MaxElmt, 0L);
    IO_WriteNl((System_tFile)IO_StdError);
    IO_WriteS((System_tFile)IO_StdError, (STRING)"LastBitset= ", 12L);
    IO_WriteI((System_tFile)IO_StdError, (LONGINT)W_1->LastBitset, 0L);
    IO_WriteNl((System_tFile)IO_StdError);
    IO_WriteS((System_tFile)IO_StdError, (STRING)"Card      = ", 12L);
    IO_WriteI((System_tFile)IO_StdError, (LONGINT)W_1->Card, 0L);
    IO_WriteNl((System_tFile)IO_StdError);
    IO_WriteS((System_tFile)IO_StdError, (STRING)"FirstElmt = ", 12L);
    IO_WriteI((System_tFile)IO_StdError, (LONGINT)W_1->FirstElmt, 0L);
    IO_WriteNl((System_tFile)IO_StdError);
    IO_WriteS((System_tFile)IO_StdError, (STRING)"LastElmt  = ", 12L);
    IO_WriteI((System_tFile)IO_StdError, (LONGINT)W_1->LastElmt, 0L);
    IO_WriteNl((System_tFile)IO_StdError);
  }
}

void SetsC_MakeSet
# ifdef __STDC__
(SetsC_tSet *Set, CARDINAL MaxSize)
# else
(Set, MaxSize)
SetsC_tSet *Set;
CARDINAL MaxSize;
# endif
{
  Sets_MakeSet(Set, MaxSize);
  CheckSet(Set, (STRING)"MakeSet", 7L);
}

void SetsC_ReleaseSet
# ifdef __STDC__
(SetsC_tSet *Set)
# else
(Set)
SetsC_tSet *Set;
# endif
{
  CheckSet(Set, (STRING)"ReleaseSet", 10L);
  Sets_ReleaseSet(Set);
}

void SetsC_Union
# ifdef __STDC__
(SetsC_tSet *Set1, SetsC_tSet Set2)
# else
(Set1, Set2)
SetsC_tSet *Set1;
SetsC_tSet Set2;
# endif
{
  CheckSetSet(Set1, &Set2, (STRING)"Union", 5L);
  Sets_Union(Set1, Set2);
}

void SetsC_Difference
# ifdef __STDC__
(SetsC_tSet *Set1, SetsC_tSet Set2)
# else
(Set1, Set2)
SetsC_tSet *Set1;
SetsC_tSet Set2;
# endif
{
  CheckSetSet(Set1, &Set2, (STRING)"Difference", 10L);
  Sets_Difference(Set1, Set2);
}

void SetsC_Intersection
# ifdef __STDC__
(SetsC_tSet *Set1, SetsC_tSet Set2)
# else
(Set1, Set2)
SetsC_tSet *Set1;
SetsC_tSet Set2;
# endif
{
  CheckSetSet(Set1, &Set2, (STRING)"Intersection", 12L);
  Sets_Intersection(Set1, Set2);
}

void SetsC_SymDiff
# ifdef __STDC__
(SetsC_tSet *Set1, SetsC_tSet Set2)
# else
(Set1, Set2)
SetsC_tSet *Set1;
SetsC_tSet Set2;
# endif
{
  CheckSetSet(Set1, &Set2, (STRING)"SymDiff", 7L);
  Sets_SymDiff(Set1, Set2);
}

void SetsC_Complement
# ifdef __STDC__
(SetsC_tSet *Set)
# else
(Set)
SetsC_tSet *Set;
# endif
{
  CheckSet(Set, (STRING)"Complement", 10L);
  Sets_Complement(Set);
}

void SetsC_Include
# ifdef __STDC__
(SetsC_tSet *Set, CARDINAL Elmt)
# else
(Set, Elmt)
SetsC_tSet *Set;
CARDINAL Elmt;
# endif
{
  CheckSetElmt(Set, (LONGINT)Elmt, (STRING)"Include", 7L);
  Sets_Include(Set, Elmt);
}

void SetsC_Exclude
# ifdef __STDC__
(SetsC_tSet *Set, CARDINAL Elmt)
# else
(Set, Elmt)
SetsC_tSet *Set;
CARDINAL Elmt;
# endif
{
  CheckSetElmt(Set, (LONGINT)Elmt, (STRING)"Exclude", 7L);
  Sets_Exclude(Set, Elmt);
}

CARDINAL SetsC_Card
# ifdef __STDC__
(SetsC_tSet *Set)
# else
(Set)
SetsC_tSet *Set;
# endif
{
  CheckSet(Set, (STRING)"Card", 4L);
  return Sets_Card(Set);
}

CARDINAL SetsC_Size
# ifdef __STDC__
(SetsC_tSet *Set)
# else
(Set)
SetsC_tSet *Set;
# endif
{
  CheckSet(Set, (STRING)"Size", 4L);
  return Sets_Size(Set);
}

CARDINAL SetsC_Minimum
# ifdef __STDC__
(SetsC_tSet *Set)
# else
(Set)
SetsC_tSet *Set;
# endif
{
  CheckNotEmpty(Set, (STRING)"Minimum", 7L);
  return Sets_Minimum(Set);
}

CARDINAL SetsC_Maximum
# ifdef __STDC__
(SetsC_tSet *Set)
# else
(Set)
SetsC_tSet *Set;
# endif
{
  CheckNotEmpty(Set, (STRING)"Maximum", 7L);
  return Sets_Maximum(Set);
}

CARDINAL SetsC_Select
# ifdef __STDC__
(SetsC_tSet *Set)
# else
(Set)
SetsC_tSet *Set;
# endif
{
  CheckNotEmpty(Set, (STRING)"Select", 6L);
  return Sets_Select(Set);
}

CARDINAL SetsC_Extract
# ifdef __STDC__
(SetsC_tSet *Set)
# else
(Set)
SetsC_tSet *Set;
# endif
{
  CheckNotEmpty(Set, (STRING)"Extract", 7L);
  return Sets_Extract(Set);
}

BOOLEAN SetsC_IsSubset
# ifdef __STDC__
(SetsC_tSet Set1, SetsC_tSet Set2)
# else
(Set1, Set2)
SetsC_tSet Set1, Set2;
# endif
{
  CheckSetSet(&Set1, &Set2, (STRING)"IsSubset", 8L);
  return Sets_IsSubset(Set1, Set2);
}

BOOLEAN SetsC_IsStrictSubset
# ifdef __STDC__
(SetsC_tSet Set1, SetsC_tSet Set2)
# else
(Set1, Set2)
SetsC_tSet Set1, Set2;
# endif
{
  CheckSetSet(&Set1, &Set2, (STRING)"IsStrictSubset", 14L);
  return Sets_IsStrictSubset(Set1, Set2);
}

BOOLEAN SetsC_IsEqual
# ifdef __STDC__
(SetsC_tSet *Set1, SetsC_tSet *Set2)
# else
(Set1, Set2)
SetsC_tSet *Set1, *Set2;
# endif
{
  CheckSetSet(Set1, Set2, (STRING)"IsEqual", 7L);
  return Sets_IsEqual(Set1, Set2);
}

BOOLEAN SetsC_IsNotEqual
# ifdef __STDC__
(SetsC_tSet Set1, SetsC_tSet Set2)
# else
(Set1, Set2)
SetsC_tSet Set1, Set2;
# endif
{
  CheckSetSet(&Set1, &Set2, (STRING)"IsNotEqual", 10L);
  return Sets_IsNotEqual(Set1, Set2);
}

BOOLEAN SetsC_IsElement
# ifdef __STDC__
(CARDINAL Elmt, SetsC_tSet *Set)
# else
(Elmt, Set)
CARDINAL Elmt;
SetsC_tSet *Set;
# endif
{
  CheckSetElmt(Set, (LONGINT)Elmt, (STRING)"IsElement", 9L);
  return Sets_IsElement(Elmt, Set);
}

BOOLEAN SetsC_IsEmpty
# ifdef __STDC__
(SetsC_tSet Set)
# else
(Set)
SetsC_tSet Set;
# endif
{
  CheckSet(&Set, (STRING)"IsEmpty", 7L);
  return Sets_IsEmpty(Set);
}

BOOLEAN SetsC_Forall
# ifdef __STDC__
(SetsC_tSet Set, SetsC_ProcOfCardToBool Proc)
# else
(Set, Proc)
SetsC_tSet Set;
SetsC_ProcOfCardToBool Proc;
# endif
{
  CheckSet(&Set, (STRING)"Forall", 6L);
  return Sets_Forall(Set, Proc);
}

BOOLEAN SetsC_Exists
# ifdef __STDC__
(SetsC_tSet Set, SetsC_ProcOfCardToBool Proc)
# else
(Set, Proc)
SetsC_tSet Set;
SetsC_ProcOfCardToBool Proc;
# endif
{
  CheckSet(&Set, (STRING)"Exists", 6L);
  return Sets_Exists(Set, Proc);
}

BOOLEAN SetsC_Exists1
# ifdef __STDC__
(SetsC_tSet Set, SetsC_ProcOfCardToBool Proc)
# else
(Set, Proc)
SetsC_tSet Set;
SetsC_ProcOfCardToBool Proc;
# endif
{
  CheckSet(&Set, (STRING)"Exists1", 7L);
  return Sets_Exists1(Set, Proc);
}

void SetsC_Assign
# ifdef __STDC__
(SetsC_tSet *Set1, SetsC_tSet Set2)
# else
(Set1, Set2)
SetsC_tSet *Set1;
SetsC_tSet Set2;
# endif
{
  CheckSetSet(Set1, &Set2, (STRING)"Assign", 6L);
  Sets_Assign(Set1, Set2);
}

void SetsC_AssignElmt
# ifdef __STDC__
(SetsC_tSet *Set, CARDINAL Elmt)
# else
(Set, Elmt)
SetsC_tSet *Set;
CARDINAL Elmt;
# endif
{
  CheckSetElmt(Set, (LONGINT)Elmt, (STRING)"AssignElmt", 10L);
  Sets_AssignElmt(Set, Elmt);
}

void SetsC_AssignEmpty
# ifdef __STDC__
(SetsC_tSet *Set)
# else
(Set)
SetsC_tSet *Set;
# endif
{
  CheckSet(Set, (STRING)"AssignEmpty", 11L);
  Sets_AssignEmpty(Set);
}

void SetsC_ForallDo
# ifdef __STDC__
(SetsC_tSet Set, SetsC_ProcOfCard Proc)
# else
(Set, Proc)
SetsC_tSet Set;
SetsC_ProcOfCard Proc;
# endif
{
  CheckSet(&Set, (STRING)"ForallDo", 8L);
  Sets_ForallDo(Set, Proc);
}

void SetsC_ReadSet
# ifdef __STDC__
(IO_tFile f, SetsC_tSet *Set)
# else
(f, Set)
IO_tFile f;
SetsC_tSet *Set;
# endif
{
  CheckSet(Set, (STRING)"ReadSet", 7L);
  Sets_ReadSet(f, Set);
}

void SetsC_WriteSet
# ifdef __STDC__
(IO_tFile f, SetsC_tSet Set)
# else
(f, Set)
IO_tFile f;
SetsC_tSet Set;
# endif
{
  CheckSet(&Set, (STRING)"WriteSet", 8L);
  Sets_WriteSet(f, Set);
}

void BEGIN_SetsC()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_IO();
    BEGIN_Sets();
    BEGIN_Sets();
    BEGIN_IO();

  }
}
