#include "SYSTEM_.h"

#ifndef DEFINITION_Idents
#include "Idents.h"
#endif

#ifndef DEFINITION_DynArray
#include "DynArray.h"
#endif

#ifndef DEFINITION_AssocTab
#include "AssocTab.h"
#endif


#define Infinite	1048576
typedef struct S_1 {
    ADDRESS A[Infinite + 1];
} tAssocTab;
static LONGINT ActualSize;
static CARDINAL ActMaxAssoc;
static tAssocTab *AssocTab;


void AssocTab_BeginAssocTab
# ifdef __STDC__
()
# else
()
# endif
{
  CARDINAL Index;
  LONGINT ActMaxIdent;

  ActualSize = 1;
  ActMaxIdent = Idents_MaxIdent();
  while (ActualSize <= ActMaxIdent) {
    ActualSize = ActualSize * 2;
  }
  DynArray_MakeArray((ADDRESS *)&AssocTab, &ActualSize, (LONGINT)sizeof(ADDRESS));
  ActMaxAssoc = ActualSize - 1;
  {
    LONGCARD B_1 = 0, B_2 = ActMaxAssoc;

    if (B_1 <= B_2)
      for (Index = B_1;; Index += 1) {
        AssocTab->A[Index] = (ADDRESS)NIL;
        if (Index >= B_2) break;
      }
  }
}

void AssocTab_PutAssoc
# ifdef __STDC__
(Idents_tIdent Ident, ADDRESS Object)
# else
(Ident, Object)
Idents_tIdent Ident;
ADDRESS Object;
# endif
{
  CARDINAL Index, OldMaxAssoc;
  LONGINT ActMaxIdent;

  if (Ident > ActMaxAssoc) {
    ActMaxIdent = Idents_MaxIdent();
    while (ActualSize <= ActMaxIdent) {
      DynArray_ExtendArray((ADDRESS *)&AssocTab, &ActualSize, (LONGINT)sizeof(ADDRESS));
    }
    OldMaxAssoc = ActMaxAssoc;
    ActMaxAssoc = ActualSize - 1;
    {
      LONGCARD B_3 = OldMaxAssoc + 1, B_4 = ActMaxAssoc;

      if (B_3 <= B_4)
        for (Index = B_3;; Index += 1) {
          AssocTab->A[Index] = (ADDRESS)NIL;
          if (Index >= B_4) break;
        }
    }
  }
  AssocTab->A[Ident] = Object;
}

void AssocTab_GetAssoc
# ifdef __STDC__
(Idents_tIdent Ident, ADDRESS *Object)
# else
(Ident, Object)
Idents_tIdent Ident;
ADDRESS *Object;
# endif
{
  if (Ident > ActMaxAssoc) {
    *Object = (ADDRESS)NIL;
  } else {
    *Object = AssocTab->A[Ident];
  }
}

void AssocTab_CloseAssocTab
# ifdef __STDC__
()
# else
()
# endif
{
  DynArray_ReleaseArray((ADDRESS *)&AssocTab, &ActualSize, (LONGINT)sizeof(ADDRESS));
}

void BEGIN_AssocTab()
{
  static BOOLEAN has_been_called = FALSE;

  if (!has_been_called) {
    has_been_called = TRUE;

    BEGIN_Idents();
    BEGIN_Idents();
    BEGIN_DynArray();

  }
}
