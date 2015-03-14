#define DEFINITION_Sets

#ifndef DEFINITION_IO
#include "IO.h"
#endif

typedef struct Sets_1 {
    BITSET A[10000000 + 1];
} Sets_ArrayOfBitset;
typedef void (*Sets_ProcOfCard) ARGS((CARDINAL));
typedef BOOLEAN (*Sets_ProcOfCardToBool) ARGS((CARDINAL));
typedef struct Sets_2 {
    Sets_ArrayOfBitset *BitsetPtr;
    SHORTCARD MaxElmt, LastBitset;
    SHORTINT Card;
    SHORTCARD FirstElmt, LastElmt;
} Sets_tSet;
extern void Sets_MakeSet ARGS((Sets_tSet *Set, CARDINAL MaxSize));
extern void Sets_ReleaseSet ARGS((Sets_tSet *Set));
extern void Sets_Union ARGS((Sets_tSet *Set1, Sets_tSet Set2));
extern void Sets_Difference ARGS((Sets_tSet *Set1, Sets_tSet Set2));
extern void Sets_Intersection ARGS((Sets_tSet *Set1, Sets_tSet Set2));
extern void Sets_SymDiff ARGS((Sets_tSet *Set1, Sets_tSet Set2));
extern void Sets_Complement ARGS((Sets_tSet *Set));
extern void Sets_Include ARGS((Sets_tSet *Set, CARDINAL Elmt));
extern void Sets_Exclude ARGS((Sets_tSet *Set, CARDINAL Elmt));
extern CARDINAL Sets_Card ARGS((Sets_tSet *Set));
extern CARDINAL Sets_Size ARGS((Sets_tSet *Set));
extern CARDINAL Sets_Minimum ARGS((Sets_tSet *Set));
extern CARDINAL Sets_Maximum ARGS((Sets_tSet *Set));
extern CARDINAL Sets_Select ARGS((Sets_tSet *Set));
extern CARDINAL Sets_Extract ARGS((Sets_tSet *Set));
extern BOOLEAN Sets_IsSubset ARGS((Sets_tSet Set1, Sets_tSet Set2));
extern BOOLEAN Sets_IsStrictSubset ARGS((Sets_tSet Set1, Sets_tSet Set2));
extern BOOLEAN Sets_IsEqual ARGS((Sets_tSet *Set1, Sets_tSet *Set2));
extern BOOLEAN Sets_IsNotEqual ARGS((Sets_tSet Set1, Sets_tSet Set2));
extern BOOLEAN Sets_IsElement ARGS((CARDINAL Elmt, Sets_tSet *Set));
extern BOOLEAN Sets_IsEmpty ARGS((Sets_tSet Set));
extern BOOLEAN Sets_Forall ARGS((Sets_tSet Set, Sets_ProcOfCardToBool Proc));
extern BOOLEAN Sets_Exists ARGS((Sets_tSet Set, Sets_ProcOfCardToBool Proc));
extern BOOLEAN Sets_Exists1 ARGS((Sets_tSet Set, Sets_ProcOfCardToBool Proc));
extern void Sets_Assign ARGS((Sets_tSet *Set1, Sets_tSet Set2));
extern void Sets_AssignElmt ARGS((Sets_tSet *Set, CARDINAL Elmt));
extern void Sets_AssignEmpty ARGS((Sets_tSet *Set));
extern void Sets_ForallDo ARGS((Sets_tSet Set, Sets_ProcOfCard Proc));
extern void Sets_ReadSet ARGS((IO_tFile f, Sets_tSet *Set));
extern void Sets_WriteSet ARGS((IO_tFile f, Sets_tSet Set));
extern void BEGIN_Sets();
