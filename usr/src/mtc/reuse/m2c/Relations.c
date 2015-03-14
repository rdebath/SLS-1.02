#include "SYSTEM_.h"

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_DynArray
#include "DynArray.h"
#endif

#ifndef DEFINITION_Sets
#include "Sets.h"
#endif

#ifndef DEFINITION_Sets
#include "Sets.h"
#endif

#ifndef DEFINITION_Relations
#include "Relations.h"
#endif


static SHORTCARD i, j;
static Relations_tRelation gRel;
static BOOLEAN gSymmetric ARGS((CARDINAL e));
typedef struct S_1 {
    SHORTCARD A[100000000 + 1];
} PredCount;
static PredCount *PredCountPtr;
static Sets_tSet WithoutPred;
static void gPredCount ARGS((CARDINAL e));
static void gPredCount2 ARGS((CARDINAL e));
static Relations_ProcOfIntIntToBool gProc2b;
static BOOLEAN gProc1b ARGS((CARDINAL e));
static Relations_ProcOfIntInt gProc2;
static void gProc1 ARGS((CARDINAL e));
static IO_tFile g;
static void WritePair ARGS((INTEGER e1, INTEGER e2));


void Relations_MakeRelation
# ifdef __STDC__
(Relations_tRelation *Rel, INTEGER Size1, INTEGER Size2)
# else
(Rel, Size1, Size2)
Relations_tRelation *Rel;
INTEGER Size1, Size2;
# endif
{
  LONGINT ElmtCount;

  Rel->Size1 = Size1;
  Rel->Size2 = Size2;
  ElmtCount = Size1 + 1;
  DynArray_MakeArray((ADDRESS *)&Rel->ArrayPtr, &ElmtCount, (LONGINT)sizeof(Sets_tSet));
  {
    SHORTCARD B_1 = 0, B_2 = Rel->Size1;

    if (B_1 <= B_2)
      for (i = B_1;; i += 1) {
        Sets_MakeSet(&Rel->ArrayPtr->A[i], (LONGCARD)Size2);
        if (i >= B_2) break;
      }
  }
}

void Relations_ReleaseRelation
# ifdef __STDC__
(Relations_tRelation *Rel)
# else
(Rel)
Relations_tRelation *Rel;
# endif
{
  LONGINT ElmtCount;

  {
    SHORTCARD B_3 = 0, B_4 = Rel->Size1;

    if (B_3 <= B_4)
      for (i = B_3;; i += 1) {
        Sets_ReleaseSet(&Rel->ArrayPtr->A[i]);
        if (i >= B_4) break;
      }
  }
  ElmtCount = Rel->Size1 + 1;
  DynArray_ReleaseArray((ADDRESS *)&Rel->ArrayPtr, &ElmtCount, (LONGINT)sizeof(Sets_tSet));
}

void Relations_Include
# ifdef __STDC__
(Relations_tRelation *Rel, INTEGER e1, INTEGER e2)
# else
(Rel, e1, e2)
Relations_tRelation *Rel;
INTEGER e1, e2;
# endif
{
  Sets_Include(&Rel->ArrayPtr->A[e1], (LONGCARD)e2);
}

void Relations_Exclude
# ifdef __STDC__
(Relations_tRelation *Rel, INTEGER e1, INTEGER e2)
# else
(Rel, e1, e2)
Relations_tRelation *Rel;
INTEGER e1, e2;
# endif
{
  Sets_Exclude(&Rel->ArrayPtr->A[e1], (LONGCARD)e2);
}

BOOLEAN Relations_IsElement
# ifdef __STDC__
(INTEGER e1, INTEGER e2, Relations_tRelation Rel)
# else
(e1, e2, Rel)
INTEGER e1, e2;
Relations_tRelation Rel;
# endif
{
  return Sets_IsElement((LONGCARD)e2, &Rel.ArrayPtr->A[e1]);
}

BOOLEAN Relations_IsRelated
# ifdef __STDC__
(INTEGER e1, INTEGER e2, Relations_tRelation Rel)
# else
(e1, e2, Rel)
INTEGER e1, e2;
Relations_tRelation Rel;
# endif
{
  return Sets_IsElement((LONGCARD)e2, &Rel.ArrayPtr->A[e1]);
}

BOOLEAN Relations_IsReflexive1
# ifdef __STDC__
(INTEGER e1, Relations_tRelation Rel)
# else
(e1, Rel)
INTEGER e1;
Relations_tRelation Rel;
# endif
{
  return Sets_IsElement((LONGCARD)e1, &Rel.ArrayPtr->A[e1]);
}

BOOLEAN Relations_IsSymmetric1
# ifdef __STDC__
(INTEGER e1, INTEGER e2, Relations_tRelation Rel)
# else
(e1, e2, Rel)
INTEGER e1, e2;
Relations_tRelation Rel;
# endif
{
  return !Sets_IsElement((LONGCARD)e2, &Rel.ArrayPtr->A[e1]) || Sets_IsElement((LONGCARD)e1, &Rel.ArrayPtr->A[e2]);
}

BOOLEAN Relations_IsTransitive1
# ifdef __STDC__
(INTEGER e1, INTEGER e2, INTEGER e3, Relations_tRelation Rel)
# else
(e1, e2, e3, Rel)
INTEGER e1, e2, e3;
Relations_tRelation Rel;
# endif
{
  return !(Sets_IsElement((LONGCARD)e2, &Rel.ArrayPtr->A[e1]) && Sets_IsElement((LONGCARD)e3, &Rel.ArrayPtr->A[e2])) || Sets_IsElement((LONGCARD)e3, &Rel.ArrayPtr->A[e1]);
}

BOOLEAN Relations_IsReflexive
# ifdef __STDC__
(Relations_tRelation Rel)
# else
(Rel)
Relations_tRelation Rel;
# endif
{
  {
    SHORTCARD B_5 = 0, B_6 = Rel.Size1;

    if (B_5 <= B_6)
      for (i = B_5;; i += 1) {
        if (!Sets_IsElement((LONGCARD)i, &Rel.ArrayPtr->A[i])) {
          return FALSE;
        }
        if (i >= B_6) break;
      }
  }
  return TRUE;
}

static BOOLEAN gSymmetric
# ifdef __STDC__
(CARDINAL e)
# else
(e)
CARDINAL e;
# endif
{
  return Sets_IsElement((LONGCARD)i, &gRel.ArrayPtr->A[e]);
}

BOOLEAN Relations_IsSymmetric
# ifdef __STDC__
(Relations_tRelation Rel)
# else
(Rel)
Relations_tRelation Rel;
# endif
{
  gRel = Rel;
  {
    SHORTCARD B_7 = 0, B_8 = Rel.Size1;

    if (B_7 <= B_8)
      for (i = B_7;; i += 1) {
        if (!Sets_Forall(Rel.ArrayPtr->A[i], (Sets_ProcOfCardToBool)gSymmetric)) {
          return FALSE;
        }
        if (i >= B_8) break;
      }
  }
  return TRUE;
}

BOOLEAN Relations_IsTransitive
# ifdef __STDC__
(Relations_tRelation Rel)
# else
(Rel)
Relations_tRelation Rel;
# endif
{
  Relations_tRelation r;
  BOOLEAN Result;

  Relations_MakeRelation(&r, (LONGINT)Rel.Size1, (LONGINT)Rel.Size2);
  Relations_Assign(&r, Rel);
  Relations_Closure(&r);
  Result = Relations_IsEqual(&r, &Rel);
  Relations_ReleaseRelation(&r);
  return Result;
}

BOOLEAN Relations_IsEquivalence
# ifdef __STDC__
(Relations_tRelation Rel)
# else
(Rel)
Relations_tRelation Rel;
# endif
{
  return Relations_IsReflexive(Rel) && Relations_IsSymmetric(Rel) && Relations_IsTransitive(Rel);
}

BOOLEAN Relations_HasReflexive
# ifdef __STDC__
(Relations_tRelation Rel)
# else
(Rel)
Relations_tRelation Rel;
# endif
{
  {
    SHORTCARD B_9 = 0, B_10 = Rel.Size1;

    if (B_9 <= B_10)
      for (i = B_9;; i += 1) {
        if (Sets_IsElement((LONGCARD)i, &Rel.ArrayPtr->A[i])) {
          return TRUE;
        }
        if (i >= B_10) break;
      }
  }
  return FALSE;
}

BOOLEAN Relations_IsCyclic
# ifdef __STDC__
(Relations_tRelation Rel)
# else
(Rel)
Relations_tRelation Rel;
# endif
{
  LONGINT PredCountSize;
  Sets_tSet WithPred;
  BOOLEAN Result;

  PredCountSize = Rel.Size1 + 1;
  DynArray_MakeArray((ADDRESS *)&PredCountPtr, &PredCountSize, (LONGINT)sizeof(SHORTCARD));
  Sets_MakeSet(&WithoutPred, (LONGCARD)Rel.Size1);
  Sets_MakeSet(&WithPred, (LONGCARD)Rel.Size1);
  {
    SHORTCARD B_11 = 0, B_12 = Rel.Size1;

    if (B_11 <= B_12)
      for (i = B_11;; i += 1) {
        PredCountPtr->A[i] = 0;
        if (i >= B_12) break;
      }
  }
  {
    SHORTCARD B_13 = 0, B_14 = Rel.Size1;

    if (B_13 <= B_14)
      for (i = B_13;; i += 1) {
        Sets_ForallDo(Rel.ArrayPtr->A[i], (Sets_ProcOfCard)gPredCount);
        if (i >= B_14) break;
      }
  }
  {
    SHORTCARD B_15 = 0, B_16 = Rel.Size1;

    if (B_15 <= B_16)
      for (i = B_15;; i += 1) {
        if (PredCountPtr->A[i] == 0) {
          Sets_Include(&WithoutPred, (LONGCARD)i);
        }
        if (i >= B_16) break;
      }
  }
  Sets_Complement(&WithPred);
  while (!Sets_IsEmpty(WithoutPred)) {
    i = Sets_Extract(&WithoutPred);
    Sets_Exclude(&WithPred, (LONGCARD)i);
    Sets_ForallDo(Rel.ArrayPtr->A[i], (Sets_ProcOfCard)gPredCount2);
  }
  Result = !Sets_IsEmpty(WithPred);
  Sets_ReleaseSet(&WithoutPred);
  Sets_ReleaseSet(&WithPred);
  DynArray_ReleaseArray((ADDRESS *)&PredCountPtr, &PredCountSize, (LONGINT)sizeof(SHORTCARD));
  return Result;
}

static void gPredCount
# ifdef __STDC__
(CARDINAL e)
# else
(e)
CARDINAL e;
# endif
{
  INC(PredCountPtr->A[e]);
}

static void gPredCount2
# ifdef __STDC__
(CARDINAL e)
# else
(e)
CARDINAL e;
# endif
{
  DEC(PredCountPtr->A[e]);
  if (PredCountPtr->A[e] == 0) {
    Sets_Include(&WithoutPred, e);
  }
}

void Relations_GetCyclics
# ifdef __STDC__
(Relations_tRelation Rel, Sets_tSet *Set)
# else
(Rel, Set)
Relations_tRelation Rel;
Sets_tSet *Set;
# endif
{
  Relations_tRelation r;

  Relations_MakeRelation(&r, (LONGINT)Rel.Size1, (LONGINT)Rel.Size2);
  Relations_Assign(&r, Rel);
  Relations_Closure(&r);
  Sets_AssignEmpty(Set);
  {
    SHORTCARD B_17 = 0, B_18 = r.Size1;

    if (B_17 <= B_18)
      for (i = B_17;; i += 1) {
        if (Sets_IsElement((LONGCARD)i, &r.ArrayPtr->A[i])) {
          Sets_Include(Set, (LONGCARD)i);
        }
        if (i >= B_18) break;
      }
  }
  Relations_ReleaseRelation(&r);
}

void Relations_AssignEmpty
# ifdef __STDC__
(Relations_tRelation *Rel)
# else
(Rel)
Relations_tRelation *Rel;
# endif
{
  {
    SHORTCARD B_19 = 0, B_20 = Rel->Size1;

    if (B_19 <= B_20)
      for (i = B_19;; i += 1) {
        Sets_AssignEmpty(&Rel->ArrayPtr->A[i]);
        if (i >= B_20) break;
      }
  }
}

void Relations_AssignElmt
# ifdef __STDC__
(Relations_tRelation *Rel, INTEGER e1, INTEGER e2)
# else
(Rel, e1, e2)
Relations_tRelation *Rel;
INTEGER e1, e2;
# endif
{
  Relations_AssignEmpty(Rel);
  Relations_Include(Rel, e1, e2);
}

void Relations_Assign
# ifdef __STDC__
(Relations_tRelation *Rel1, Relations_tRelation Rel2)
# else
(Rel1, Rel2)
Relations_tRelation *Rel1;
Relations_tRelation Rel2;
# endif
{
  {
    SHORTCARD B_21 = 0, B_22 = Rel1->Size1;

    if (B_21 <= B_22)
      for (i = B_21;; i += 1) {
        Sets_Assign(&Rel1->ArrayPtr->A[i], Rel2.ArrayPtr->A[i]);
        if (i >= B_22) break;
      }
  }
}

void Relations_Closure
# ifdef __STDC__
(Relations_tRelation *Rel)
# else
(Rel)
Relations_tRelation *Rel;
# endif
{
  Sets_tSet aj;

  {
    register Relations_tRelation *W_1 = Rel;

    {
      SHORTCARD B_23 = 0, B_24 = W_1->Size1;

      if (B_23 <= B_24)
        for (j = B_23;; j += 1) {
          if (!Sets_IsEmpty(W_1->ArrayPtr->A[j])) {
            aj = W_1->ArrayPtr->A[j];
            {
              SHORTCARD B_25 = 0, B_26 = W_1->Size1;

              if (B_25 <= B_26)
                for (i = B_25;; i += 1) {
                  if (Sets_IsElement((LONGCARD)j, &W_1->ArrayPtr->A[i])) {
                    Sets_Union(&W_1->ArrayPtr->A[i], aj);
                  }
                  if (i >= B_26) break;
                }
            }
          }
          if (j >= B_24) break;
        }
    }
  }
}

void Relations_Union
# ifdef __STDC__
(Relations_tRelation *Rel1, Relations_tRelation Rel2)
# else
(Rel1, Rel2)
Relations_tRelation *Rel1;
Relations_tRelation Rel2;
# endif
{
  {
    SHORTCARD B_27 = 0, B_28 = Rel1->Size1;

    if (B_27 <= B_28)
      for (i = B_27;; i += 1) {
        Sets_Union(&Rel1->ArrayPtr->A[i], Rel2.ArrayPtr->A[i]);
        if (i >= B_28) break;
      }
  }
}

void Relations_Difference
# ifdef __STDC__
(Relations_tRelation *Rel1, Relations_tRelation Rel2)
# else
(Rel1, Rel2)
Relations_tRelation *Rel1;
Relations_tRelation Rel2;
# endif
{
  {
    SHORTCARD B_29 = 0, B_30 = Rel1->Size1;

    if (B_29 <= B_30)
      for (i = B_29;; i += 1) {
        Sets_Difference(&Rel1->ArrayPtr->A[i], Rel2.ArrayPtr->A[i]);
        if (i >= B_30) break;
      }
  }
}

void Relations_Intersection
# ifdef __STDC__
(Relations_tRelation *Rel1, Relations_tRelation Rel2)
# else
(Rel1, Rel2)
Relations_tRelation *Rel1;
Relations_tRelation Rel2;
# endif
{
  {
    SHORTCARD B_31 = 0, B_32 = Rel1->Size1;

    if (B_31 <= B_32)
      for (i = B_31;; i += 1) {
        Sets_Intersection(&Rel1->ArrayPtr->A[i], Rel2.ArrayPtr->A[i]);
        if (i >= B_32) break;
      }
  }
}

void Relations_SymDiff
# ifdef __STDC__
(Relations_tRelation *Rel1, Relations_tRelation Rel2)
# else
(Rel1, Rel2)
Relations_tRelation *Rel1;
Relations_tRelation Rel2;
# endif
{
  {
    SHORTCARD B_33 = 0, B_34 = Rel1->Size1;

    if (B_33 <= B_34)
      for (i = B_33;; i += 1) {
        Sets_SymDiff(&Rel1->ArrayPtr->A[i], Rel2.ArrayPtr->A[i]);
        if (i >= B_34) break;
      }
  }
}

void Relations_Complement
# ifdef __STDC__
(Relations_tRelation *Rel)
# else
(Rel)
Relations_tRelation *Rel;
# endif
{
  {
    SHORTCARD B_35 = 0, B_36 = Rel->Size1;

    if (B_35 <= B_36)
      for (i = B_35;; i += 1) {
        Sets_Complement(&Rel->ArrayPtr->A[i]);
        if (i >= B_36) break;
      }
  }
}

BOOLEAN Relations_IsSubset
# ifdef __STDC__
(Relations_tRelation Rel1, Relations_tRelation Rel2)
# else
(Rel1, Rel2)
Relations_tRelation Rel1, Rel2;
# endif
{
  {
    SHORTCARD B_37 = 0, B_38 = Rel1.Size1;

    if (B_37 <= B_38)
      for (i = B_37;; i += 1) {
        if (!Sets_IsSubset(Rel1.ArrayPtr->A[i], Rel2.ArrayPtr->A[i])) {
          return FALSE;
        }
        if (i >= B_38) break;
      }
  }
  return TRUE;
}

BOOLEAN Relations_IsStrictSubset
# ifdef __STDC__
(Relations_tRelation Rel1, Relations_tRelation Rel2)
# else
(Rel1, Rel2)
Relations_tRelation Rel1, Rel2;
# endif
{
  return Relations_IsSubset(Rel1, Rel2) && Relations_IsNotEqual(Rel1, Rel2);
}

BOOLEAN Relations_IsEqual
# ifdef __STDC__
(Relations_tRelation *Rel1, Relations_tRelation *Rel2)
# else
(Rel1, Rel2)
Relations_tRelation *Rel1, *Rel2;
# endif
{
  {
    SHORTCARD B_39 = 0, B_40 = Rel1->Size1;

    if (B_39 <= B_40)
      for (i = B_39;; i += 1) {
        if (!Sets_IsEqual(&Rel1->ArrayPtr->A[i], &Rel2->ArrayPtr->A[i])) {
          return FALSE;
        }
        if (i >= B_40) break;
      }
  }
  return TRUE;
}

BOOLEAN Relations_IsNotEqual
# ifdef __STDC__
(Relations_tRelation Rel1, Relations_tRelation Rel2)
# else
(Rel1, Rel2)
Relations_tRelation Rel1, Rel2;
# endif
{
  return !Relations_IsEqual(&Rel1, &Rel2);
}

BOOLEAN Relations_IsEmpty
# ifdef __STDC__
(Relations_tRelation Rel)
# else
(Rel)
Relations_tRelation Rel;
# endif
{
  {
    SHORTCARD B_41 = 0, B_42 = Rel.Size1;

    if (B_41 <= B_42)
      for (i = B_41;; i += 1) {
        if (!Sets_IsEmpty(Rel.ArrayPtr->A[i])) {
          return FALSE;
        }
        if (i >= B_42) break;
      }
  }
  return TRUE;
}

INTEGER Relations_Card
# ifdef __STDC__
(Relations_tRelation *Rel)
# else
(Rel)
Relations_tRelation *Rel;
# endif
{
  INTEGER n;

  n = 0;
  {
    SHORTCARD B_43 = 0, B_44 = Rel->Size1;

    if (B_43 <= B_44)
      for (i = B_43;; i += 1) {
        INC1(n, Sets_Card(&Rel->ArrayPtr->A[i]));
        if (i >= B_44) break;
      }
  }
  return n;
}

void Relations_Select
# ifdef __STDC__
(Relations_tRelation *Rel, INTEGER *e1, INTEGER *e2)
# else
(Rel, e1, e2)
Relations_tRelation *Rel;
INTEGER *e1, *e2;
# endif
{
  {
    SHORTCARD B_45 = 0, B_46 = Rel->Size1;

    if (B_45 <= B_46)
      for (i = B_45;; i += 1) {
        if (!Sets_IsEmpty(Rel->ArrayPtr->A[i])) {
          *e1 = i;
          *e2 = Sets_Select(&Rel->ArrayPtr->A[i]);
          return;
        }
        if (i >= B_46) break;
      }
  }
  *e1 = 0;
  *e2 = 0;
}

void Relations_Extract
# ifdef __STDC__
(Relations_tRelation *Rel, INTEGER *e1, INTEGER *e2)
# else
(Rel, e1, e2)
Relations_tRelation *Rel;
INTEGER *e1, *e2;
# endif
{
  Relations_Select(Rel, e1, e2);
  Relations_Exclude(Rel, *e1, *e2);
}

static BOOLEAN gProc1b
# ifdef __STDC__
(CARDINAL e)
# else
(e)
CARDINAL e;
# endif
{
  return (*gProc2b)((LONGINT)i, (LONGINT)e);
}

BOOLEAN Relations_Forall
# ifdef __STDC__
(Relations_tRelation Rel, Relations_ProcOfIntIntToBool Proc)
# else
(Rel, Proc)
Relations_tRelation Rel;
Relations_ProcOfIntIntToBool Proc;
# endif
{
  gProc2b = Proc;
  {
    SHORTCARD B_47 = 0, B_48 = Rel.Size1;

    if (B_47 <= B_48)
      for (i = B_47;; i += 1) {
        if (!Sets_Forall(Rel.ArrayPtr->A[i], (Sets_ProcOfCardToBool)gProc1b)) {
          return FALSE;
        }
        if (i >= B_48) break;
      }
  }
  return TRUE;
}

BOOLEAN Relations_Exists
# ifdef __STDC__
(Relations_tRelation Rel, Relations_ProcOfIntIntToBool Proc)
# else
(Rel, Proc)
Relations_tRelation Rel;
Relations_ProcOfIntIntToBool Proc;
# endif
{
  gProc2b = Proc;
  {
    SHORTCARD B_49 = 0, B_50 = Rel.Size1;

    if (B_49 <= B_50)
      for (i = B_49;; i += 1) {
        if (Sets_Exists(Rel.ArrayPtr->A[i], (Sets_ProcOfCardToBool)gProc1b)) {
          return TRUE;
        }
        if (i >= B_50) break;
      }
  }
  return FALSE;
}

BOOLEAN Relations_Exists1
# ifdef __STDC__
(Relations_tRelation Rel, Relations_ProcOfIntIntToBool Proc)
# else
(Rel, Proc)
Relations_tRelation Rel;
Relations_ProcOfIntIntToBool Proc;
# endif
{
  INTEGER n;

  n = 0;
  gProc2b = Proc;
  {
    SHORTCARD B_51 = 0, B_52 = Rel.Size1;

    if (B_51 <= B_52)
      for (i = B_51;; i += 1) {
        if (Sets_Exists(Rel.ArrayPtr->A[i], (Sets_ProcOfCardToBool)gProc1b)) {
          INC(n);
        }
        if (i >= B_52) break;
      }
  }
  return n == 1;
}

static void gProc1
# ifdef __STDC__
(CARDINAL e)
# else
(e)
CARDINAL e;
# endif
{
  (*gProc2)((LONGINT)i, (LONGINT)e);
}

void Relations_ForallDo
# ifdef __STDC__
(Relations_tRelation Rel, Relations_ProcOfIntInt Proc)
# else
(Rel, Proc)
Relations_tRelation Rel;
Relations_ProcOfIntInt Proc;
# endif
{
  gProc2 = Proc;
  {
    SHORTCARD B_53 = 0, B_54 = Rel.Size1;

    if (B_53 <= B_54)
      for (i = B_53;; i += 1) {
        Sets_ForallDo(Rel.ArrayPtr->A[i], (Sets_ProcOfCard)gProc1);
        if (i >= B_54) break;
      }
  }
}

void Relations_ReadRelation
# ifdef __STDC__
(IO_tFile f, Relations_tRelation *Rel)
# else
(f, Rel)
IO_tFile f;
Relations_tRelation *Rel;
# endif
{
  CHAR ch;

  do {
  } while (!(IO_ReadC(f) == '{'));
  Relations_AssignEmpty(Rel);
  for (;;) {
    if (IO_ReadC(f) == '}') {
      goto EXIT_1;
    }
    i = IO_ReadI(f);
    Relations_Include(Rel, (LONGINT)i, IO_ReadI(f));
    ch = IO_ReadC(f);
  } EXIT_1:;
}

void Relations_WriteRelation
# ifdef __STDC__
(IO_tFile f, Relations_tRelation Rel)
# else
(f, Rel)
IO_tFile f;
Relations_tRelation Rel;
# endif
{
  g = f;
  IO_WriteC(f, '{');
  Relations_ForallDo(Rel, (Relations_ProcOfIntInt)WritePair);
  IO_WriteC(f, '}');
}

static void WritePair
# ifdef __STDC__
(INTEGER e1, INTEGER e2)
# else
(e1, e2)
INTEGER e1, e2;
# endif
{
  IO_WriteC(g, ' ');
  IO_WriteI(g, e1, 1L);
  IO_WriteC(g, ' ');
  IO_WriteI(g, e2, 1L);
  IO_WriteC(g, ',');
}

void BEGIN_Relations()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_IO();
    BEGIN_Sets();
    BEGIN_IO();
    BEGIN_DynArray();
    BEGIN_Sets();
    BEGIN_Sets();

  }
}
