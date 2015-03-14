#include "SYSTEM_.h"

#ifndef DEFINITION_General
#include "General.h"
#endif

#ifndef DEFINITION_DynArray
#include "DynArray.h"
#endif

#ifndef DEFINITION_IO
#include "IO.h"
#endif

#ifndef DEFINITION_Sets
#include "Sets.h"
#endif


#define BitsPerBitset	32
#define BitsPerByte	8
#define BytesPerBitset	(BitsPerBitset / BitsPerByte)
#define NoCard	-1
static BITSET AllBits;
static IO_tFile g;
static void WriteElmt ARGS((CARDINAL Elmt));


void Sets_MakeSet
# ifdef __STDC__
(Sets_tSet *Set, CARDINAL MaxSize)
# else
(Set, MaxSize)
Sets_tSet *Set;
CARDINAL MaxSize;
# endif
{
  LONGINT ElmtCount;

  {
    register Sets_tSet *W_1 = Set;

    ElmtCount = (MaxSize + BitsPerBitset - MaxSize % BitsPerBitset) / BitsPerBitset;
    DynArray_MakeArray((ADDRESS *)&W_1->BitsetPtr, &ElmtCount, (LONGINT)BytesPerBitset);
    W_1->MaxElmt = MaxSize;
    W_1->LastBitset = ElmtCount - 1;
    Sets_AssignEmpty(Set);
  }
}

void Sets_ReleaseSet
# ifdef __STDC__
(Sets_tSet *Set)
# else
(Set)
Sets_tSet *Set;
# endif
{
  LONGINT ElmtCount;

  ElmtCount = Set->LastBitset + 1;
  DynArray_ReleaseArray((ADDRESS *)&Set->BitsetPtr, &ElmtCount, (LONGINT)BytesPerBitset);
}

void Sets_Union
# ifdef __STDC__
(Sets_tSet *Set1, Sets_tSet Set2)
# else
(Set1, Set2)
Sets_tSet *Set1;
Sets_tSet Set2;
# endif
{
  CARDINAL i;

  {
    register Sets_tSet *W_2 = Set1;

    i = 0;
    while (i <= W_2->LastBitset) {
      W_2->BitsetPtr->A[i] = W_2->BitsetPtr->A[i] | Set2.BitsetPtr->A[i];
      INC(i);
    }
    W_2->Card = NoCard;
    W_2->FirstElmt = General_Min((LONGINT)W_2->FirstElmt, (LONGINT)Set2.FirstElmt);
    W_2->LastElmt = General_Max((LONGINT)W_2->LastElmt, (LONGINT)Set2.LastElmt);
  }
}

void Sets_Difference
# ifdef __STDC__
(Sets_tSet *Set1, Sets_tSet Set2)
# else
(Set1, Set2)
Sets_tSet *Set1;
Sets_tSet Set2;
# endif
{
  CARDINAL i;

  {
    register Sets_tSet *W_3 = Set1;

    i = 0;
    while (i <= W_3->LastBitset) {
      W_3->BitsetPtr->A[i] = SET_DIFF(W_3->BitsetPtr->A[i], Set2.BitsetPtr->A[i]);
      INC(i);
    }
    W_3->Card = NoCard;
  }
}

void Sets_Intersection
# ifdef __STDC__
(Sets_tSet *Set1, Sets_tSet Set2)
# else
(Set1, Set2)
Sets_tSet *Set1;
Sets_tSet Set2;
# endif
{
  CARDINAL i;

  {
    register Sets_tSet *W_4 = Set1;

    i = 0;
    while (i <= W_4->LastBitset) {
      W_4->BitsetPtr->A[i] = W_4->BitsetPtr->A[i] & Set2.BitsetPtr->A[i];
      INC(i);
    }
    W_4->Card = NoCard;
    W_4->FirstElmt = General_Max((LONGINT)W_4->FirstElmt, (LONGINT)Set2.FirstElmt);
    W_4->LastElmt = General_Min((LONGINT)W_4->LastElmt, (LONGINT)Set2.LastElmt);
  }
}

void Sets_SymDiff
# ifdef __STDC__
(Sets_tSet *Set1, Sets_tSet Set2)
# else
(Set1, Set2)
Sets_tSet *Set1;
Sets_tSet Set2;
# endif
{
  CARDINAL i;

  {
    register Sets_tSet *W_5 = Set1;

    i = 0;
    while (i <= W_5->LastBitset) {
      W_5->BitsetPtr->A[i] = W_5->BitsetPtr->A[i] ^ Set2.BitsetPtr->A[i];
      INC(i);
    }
    W_5->Card = NoCard;
    W_5->FirstElmt = General_Min((LONGINT)W_5->FirstElmt, (LONGINT)Set2.FirstElmt);
    W_5->LastElmt = General_Max((LONGINT)W_5->LastElmt, (LONGINT)Set2.LastElmt);
  }
}

void Sets_Complement
# ifdef __STDC__
(Sets_tSet *Set)
# else
(Set)
Sets_tSet *Set;
# endif
{
  SHORTINT i;
  BITSET s;

  {
    register Sets_tSet *W_6 = Set;

    i = 0;
    while (i <= (SHORTINT)W_6->LastBitset - 1) {
      W_6->BitsetPtr->A[i] = SET_DIFF(AllBits, W_6->BitsetPtr->A[i]);
      INC(i);
    }
    s = 0X0L;
    {
      SHORTINT B_1 = 0, B_2 = (SHORTINT)W_6->MaxElmt % BitsPerBitset;

      if (B_1 <= B_2)
        for (i = B_1;; i += 1) {
          INCL(s, (SHORTCARD)i);
          if (i >= B_2) break;
        }
    }
    W_6->BitsetPtr->A[W_6->LastBitset] = SET_DIFF(s, W_6->BitsetPtr->A[W_6->LastBitset]);
    if (W_6->Card != NoCard) {
      W_6->Card = (SHORTINT)W_6->MaxElmt + 1 - W_6->Card;
    }
    W_6->FirstElmt = 0;
    W_6->LastElmt = W_6->MaxElmt;
  }
}

void Sets_Include
# ifdef __STDC__
(Sets_tSet *Set, CARDINAL Elmt)
# else
(Set, Elmt)
Sets_tSet *Set;
CARDINAL Elmt;
# endif
{
  {
    register Sets_tSet *W_7 = Set;

    INCL(W_7->BitsetPtr->A[Elmt / BitsPerBitset], Elmt % BitsPerBitset);
    W_7->Card = NoCard;
    if (W_7->FirstElmt > Elmt) {
      W_7->FirstElmt = Elmt;
    }
    if (W_7->LastElmt < Elmt) {
      W_7->LastElmt = Elmt;
    }
  }
}

void Sets_Exclude
# ifdef __STDC__
(Sets_tSet *Set, CARDINAL Elmt)
# else
(Set, Elmt)
Sets_tSet *Set;
CARDINAL Elmt;
# endif
{
  {
    register Sets_tSet *W_8 = Set;

    EXCL(W_8->BitsetPtr->A[Elmt / BitsPerBitset], Elmt % BitsPerBitset);
    W_8->Card = NoCard;
    if (Elmt == W_8->FirstElmt && Elmt < W_8->MaxElmt) {
      INC(W_8->FirstElmt);
    }
    if (Elmt == W_8->LastElmt && Elmt > 0) {
      DEC(W_8->LastElmt);
    }
  }
}

CARDINAL Sets_Card
# ifdef __STDC__
(Sets_tSet *Set)
# else
(Set)
Sets_tSet *Set;
# endif
{
  CARDINAL i;

  {
    register Sets_tSet *W_9 = Set;

    if (W_9->Card == NoCard) {
      W_9->Card = 0;
      {
        LONGCARD B_3 = W_9->FirstElmt, B_4 = W_9->LastElmt;

        if (B_3 <= B_4)
          for (i = B_3;; i += 1) {
            if (Sets_IsElement(i, Set)) {
              INC(W_9->Card);
            }
            if (i >= B_4) break;
          }
      }
    }
    return W_9->Card;
  }
}

CARDINAL Sets_Size
# ifdef __STDC__
(Sets_tSet *Set)
# else
(Set)
Sets_tSet *Set;
# endif
{
  return Set->MaxElmt;
}

CARDINAL Sets_Minimum
# ifdef __STDC__
(Sets_tSet *Set)
# else
(Set)
Sets_tSet *Set;
# endif
{
  CARDINAL i;

  {
    register Sets_tSet *W_10 = Set;

    i = W_10->FirstElmt;
    while (i <= W_10->LastElmt) {
      if (Sets_IsElement(i, Set)) {
        W_10->FirstElmt = i;
        return i;
      }
      INC(i);
    }
    return 0;
  }
}

CARDINAL Sets_Maximum
# ifdef __STDC__
(Sets_tSet *Set)
# else
(Set)
Sets_tSet *Set;
# endif
{
  CARDINAL i;

  {
    register Sets_tSet *W_11 = Set;

    i = W_11->LastElmt;
    while (i >= W_11->FirstElmt) {
      if (Sets_IsElement(i, Set)) {
        W_11->LastElmt = i;
        return i;
      }
      DEC(i);
    }
    return 0;
  }
}

CARDINAL Sets_Select
# ifdef __STDC__
(Sets_tSet *Set)
# else
(Set)
Sets_tSet *Set;
# endif
{
  return Sets_Minimum(Set);
}

CARDINAL Sets_Extract
# ifdef __STDC__
(Sets_tSet *Set)
# else
(Set)
Sets_tSet *Set;
# endif
{
  CARDINAL i;

  i = Sets_Minimum(Set);
  Sets_Exclude(Set, i);
  return i;
}

BOOLEAN Sets_IsSubset
# ifdef __STDC__
(Sets_tSet Set1, Sets_tSet Set2)
# else
(Set1, Set2)
Sets_tSet Set1, Set2;
# endif
{
  CARDINAL i;

  {
    register Sets_tSet *W_12 = &Set1;

    i = 0;
    while (i <= W_12->LastBitset) {
      if (!SET_IS_SUBSET1(W_12->BitsetPtr->A[i], Set2.BitsetPtr->A[i])) {
        return FALSE;
      }
      INC(i);
    }
    return TRUE;
  }
}

BOOLEAN Sets_IsStrictSubset
# ifdef __STDC__
(Sets_tSet Set1, Sets_tSet Set2)
# else
(Set1, Set2)
Sets_tSet Set1, Set2;
# endif
{
  return Sets_IsSubset(Set1, Set2) && Sets_IsNotEqual(Set1, Set2);
}

BOOLEAN Sets_IsEqual
# ifdef __STDC__
(Sets_tSet *Set1, Sets_tSet *Set2)
# else
(Set1, Set2)
Sets_tSet *Set1, *Set2;
# endif
{
  CARDINAL i;

  {
    register Sets_tSet *W_13 = Set1;

    i = 0;
    while (i <= W_13->LastBitset) {
      if (W_13->BitsetPtr->A[i] != Set2->BitsetPtr->A[i]) {
        return FALSE;
      }
      INC(i);
    }
    return TRUE;
  }
}

BOOLEAN Sets_IsNotEqual
# ifdef __STDC__
(Sets_tSet Set1, Sets_tSet Set2)
# else
(Set1, Set2)
Sets_tSet Set1, Set2;
# endif
{
  return !Sets_IsEqual(&Set1, &Set2);
}

BOOLEAN Sets_IsElement
# ifdef __STDC__
(CARDINAL Elmt, Sets_tSet *Set)
# else
(Elmt, Set)
CARDINAL Elmt;
Sets_tSet *Set;
# endif
{
  return IN(Elmt % BitsPerBitset, Set->BitsetPtr->A[Elmt / BitsPerBitset]);
}

BOOLEAN Sets_IsEmpty
# ifdef __STDC__
(Sets_tSet Set)
# else
(Set)
Sets_tSet Set;
# endif
{
  CARDINAL i;

  {
    register Sets_tSet *W_14 = &Set;

    if (W_14->FirstElmt <= W_14->LastElmt) {
      i = 0;
      while (i <= W_14->LastBitset) {
        if (W_14->BitsetPtr->A[i] != 0X0L) {
          return FALSE;
        }
        INC(i);
      }
    }
    return TRUE;
  }
}

BOOLEAN Sets_Forall
# ifdef __STDC__
(Sets_tSet Set, Sets_ProcOfCardToBool Proc)
# else
(Set, Proc)
Sets_tSet Set;
Sets_ProcOfCardToBool Proc;
# endif
{
  CARDINAL i;

  {
    register Sets_tSet *W_15 = &Set;

    {
      LONGCARD B_5 = W_15->FirstElmt, B_6 = W_15->LastElmt;

      if (B_5 <= B_6)
        for (i = B_5;; i += 1) {
          if (Sets_IsElement(i, &Set) && !(*Proc)(i)) {
            return FALSE;
          }
          if (i >= B_6) break;
        }
    }
    return TRUE;
  }
}

BOOLEAN Sets_Exists
# ifdef __STDC__
(Sets_tSet Set, Sets_ProcOfCardToBool Proc)
# else
(Set, Proc)
Sets_tSet Set;
Sets_ProcOfCardToBool Proc;
# endif
{
  CARDINAL i;

  {
    register Sets_tSet *W_16 = &Set;

    {
      LONGCARD B_7 = W_16->FirstElmt, B_8 = W_16->LastElmt;

      if (B_7 <= B_8)
        for (i = B_7;; i += 1) {
          if (Sets_IsElement(i, &Set) && (*Proc)(i)) {
            return TRUE;
          }
          if (i >= B_8) break;
        }
    }
    return FALSE;
  }
}

BOOLEAN Sets_Exists1
# ifdef __STDC__
(Sets_tSet Set, Sets_ProcOfCardToBool Proc)
# else
(Set, Proc)
Sets_tSet Set;
Sets_ProcOfCardToBool Proc;
# endif
{
  CARDINAL i, n;

  {
    register Sets_tSet *W_17 = &Set;

    n = 0;
    {
      LONGCARD B_9 = W_17->FirstElmt, B_10 = W_17->LastElmt;

      if (B_9 <= B_10)
        for (i = B_9;; i += 1) {
          if (Sets_IsElement(i, &Set) && (*Proc)(i)) {
            INC(n);
          }
          if (i >= B_10) break;
        }
    }
    return n == 1;
  }
}

void Sets_Assign
# ifdef __STDC__
(Sets_tSet *Set1, Sets_tSet Set2)
# else
(Set1, Set2)
Sets_tSet *Set1;
Sets_tSet Set2;
# endif
{
  CARDINAL i;

  {
    register Sets_tSet *W_18 = Set1;

    i = 0;
    while (i <= W_18->LastBitset) {
      W_18->BitsetPtr->A[i] = Set2.BitsetPtr->A[i];
      INC(i);
    }
    W_18->Card = Set2.Card;
    W_18->FirstElmt = Set2.FirstElmt;
    W_18->LastElmt = Set2.LastElmt;
  }
}

void Sets_AssignElmt
# ifdef __STDC__
(Sets_tSet *Set, CARDINAL Elmt)
# else
(Set, Elmt)
Sets_tSet *Set;
CARDINAL Elmt;
# endif
{
  {
    register Sets_tSet *W_19 = Set;

    Sets_AssignEmpty(Set);
    Sets_Include(Set, Elmt);
    W_19->Card = 1;
    W_19->FirstElmt = Elmt;
    W_19->LastElmt = Elmt;
  }
}

void Sets_AssignEmpty
# ifdef __STDC__
(Sets_tSet *Set)
# else
(Set)
Sets_tSet *Set;
# endif
{
  CARDINAL i;

  {
    register Sets_tSet *W_20 = Set;

    i = 0;
    while (i <= W_20->LastBitset) {
      W_20->BitsetPtr->A[i] = 0X0L;
      INC(i);
    }
    W_20->Card = 0;
    W_20->FirstElmt = W_20->MaxElmt;
    W_20->LastElmt = 0;
  }
}

void Sets_ForallDo
# ifdef __STDC__
(Sets_tSet Set, Sets_ProcOfCard Proc)
# else
(Set, Proc)
Sets_tSet Set;
Sets_ProcOfCard Proc;
# endif
{
  CARDINAL i;

  {
    register Sets_tSet *W_21 = &Set;

    {
      LONGCARD B_11 = W_21->FirstElmt, B_12 = W_21->LastElmt;

      if (B_11 <= B_12)
        for (i = B_11;; i += 1) {
          if (Sets_IsElement(i, &Set)) {
            (*Proc)(i);
          }
          if (i >= B_12) break;
        }
    }
  }
}

void Sets_ReadSet
# ifdef __STDC__
(IO_tFile f, Sets_tSet *Set)
# else
(f, Set)
IO_tFile f;
Sets_tSet *Set;
# endif
{
  CARDINAL card;

  do {
  } while (!(IO_ReadC(f) == '{'));
  Sets_AssignEmpty(Set);
  card = 0;
  for (;;) {
    if (IO_ReadC(f) == '}') {
      goto EXIT_1;
    }
    Sets_Include(Set, (LONGCARD)IO_ReadI(f));
    INC(card);
  } EXIT_1:;
  Set->Card = card;
}

void Sets_WriteSet
# ifdef __STDC__
(IO_tFile f, Sets_tSet Set)
# else
(f, Set)
IO_tFile f;
Sets_tSet Set;
# endif
{
  {
    register Sets_tSet *W_22 = &Set;

    g = f;
    IO_WriteC(f, '{');
    Sets_ForallDo(Set, (Sets_ProcOfCard)WriteElmt);
    IO_WriteC(f, '}');
  }
}

static void WriteElmt
# ifdef __STDC__
(CARDINAL Elmt)
# else
(Elmt)
CARDINAL Elmt;
# endif
{
  IO_WriteC(g, ' ');
  IO_WriteI(g, (LONGINT)Elmt, 0L);
}

void BEGIN_Sets()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_IO();
    BEGIN_General();
    BEGIN_DynArray();
    BEGIN_IO();

    AllBits = SET_cRNG(0, BitsPerBitset - 1);
    if (sizeof(BITSET) != BytesPerBitset) {
      IO_WriteS((System_tFile)IO_StdError, (STRING)"TSIZE (BITSET) = ", 17L);
      IO_WriteI((System_tFile)IO_StdError, (LONGINT)sizeof(BITSET), 0L);
      IO_WriteNl((System_tFile)IO_StdError);
    }
  }
}
